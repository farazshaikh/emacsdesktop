/*
 * fileSplitter.c --
 *
 *      Split file into fixed size blocks.
 *      Bugs: fshaikh@cs.cmu.edu
 */

/*  This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/> */


#ifndef  _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread.h>
#include <getopt.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <getopt.h>

#include "fileSplitter.h"

/*
 * Notes:
 * File splitter splits files using multiple threads.
 * Each thread reads a contigous chunk of file data and then splits it into mulitple files
 * Threads then move on to next regions of the file
 *
 * File Shards + Reconciliation info is store in a folder with the same name as the file
 *
 * This is a stand alone program and isn't long running. Depends on the OS to close file handles
 * and free up memory upon exit.
 *
 */

typedef struct FileSplitter FileSplitter;
typedef struct ThreadTask {
    int noMoreTask;                  // No more task to dole out
    off64_t offset;                  // file offset this thread is working with
} ThreadTask;

/* thread info */
typedef struct ThreadInfo {
    int fileFD;                     // file descriptor used by this threadNR
    // each thread uses it own fd, because pread() and pwrite()
    // thready safety is not guaranteed/documented
    pthread_t pthread;              // pthread id
    int       threadNR;             // Each thread is numbered, used for printing
    uint8_t  *ioBuffer;             // IO buffer used by this thread
    unsigned int validDataLen;      // How much of buffer is valid
    ThreadTask task;                // current IO task for this thread

    FileSplitter *fileSplitter;      // back ptr original task
} ThreadInfo;


/* Program info */
struct FileSplitter {
    /* Test Configuration */
    unsigned int numThreads;        // number of threads in this test
    unsigned int shardSize;         // size of resulting shards
    unsigned int reconcileMode;     // we are is reconciling shards, instead of splitting
    unsigned int reconcileStat;    // print stats about reconciliation
    unsigned int readChunkSize;     // granularity at which a single thread work
    char fileName[SECTOR_SIZE];     // filename test is operating on
    char outputDir[SECTOR_SIZE];    // directory in which file shards will be saved

    /* RunTime */
    int operationFailed;
    off64_t curr_offset;            // current file offset
    off64_t fileLength;             // file length

    pthread_mutex_t taskMutex;      // mutexes to protect global task state

    FileMeta fileMeta;              // in memory copy of filemeta <see fileSplitterLayout.h>
    ReconcileMeta reconcileMeta;    // in memory copy of reconcile meta <see fileSplitterLayout.h>
    ThreadInfo *threadInfo;         // Array of thread local info
};

FileSplitter fileSplitter;
unsigned int verbose;               // verbose log of program

/*
 *-----------------------------------------------------------------------------
 *
 * help --
 *      ??
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static void
help(int argc, char **argv) {
    printf("\n %s split/reconcile files into/from shards", argv[0]);
    printf("\n --filename filename  : Filename to be split");
    printf("\n --outputdirname dir  : Directory under which the shards will"
	   " be collected");
    printf("\n [--shardsize num]    : shard size");
    printf("\n [--numthreads num]   : number of threads sharding parts of the"
	   " file in parallel");
    printf("\n [--readchunksize num]: task size for each thread");
    printf("\n --reconcile chunkList: task size for each thread");
    printf("\n --reconcilestat      : Print reconcile stat");
    printf("\n [--verbose]          : print logging info");
}

/*
 *-----------------------------------------------------------------------------
 *
 * WriteFileMeta --
 *      Upon successfully sharding the file, write out file meta encapsulating
 *      info about the original file
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
WriteFileMeta(FileSplitter *s)
{
    int fd;

    fd = open(SHARD_META_FILE, O_CREAT | O_RDWR, 0777);
    if (fd == -1) {
	printf("\n shard file creation failed errno %d", errno);
	return -1;
    }

    strncpy((char *)s->fileMeta.fileName, s->fileName, sizeof(s->fileMeta.fileName));
    s->fileMeta.magic = SHARD_MAGIC;
    s->fileMeta.version = MAX_SHARDINFO_VER;
    s->fileMeta.shardSize= s->shardSize;
    s->fileMeta.length = (uint64_t) s->fileLength;

    if (sizeof(s->fileMeta) != write(fd, &s->fileMeta, sizeof(s->fileMeta))) {
	printf("Short write on shard meta file Bailing out %d", errno);
	return -1;
    }

    VLOG("\n Writing out filemeta %s", SHARD_META_FILE);
    VLOG("\n fileName %s", s->fileMeta.fileName);
    VLOG("\n filelength %lu", s->fileMeta.length);
    VLOG("\n ShardSize %lu", s->fileMeta.shardSize);

    return 0;
}


/*
 *-----------------------------------------------------------------------------
 *
 * IOThreadStoreShards --
 *      Thread routine for storing a threads task as shards
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
IOThreadStoreShards(ThreadInfo *threadInfo)
{
    char fileName[MAX_PATH];
    shardHeader header;
    unsigned int dataOffset = 0;
    int printSize;


    while (dataOffset < threadInfo->validDataLen) {
	uint64_t thisShardLength;
	int fd;
	ssize_t wsize;

	thisShardLength = MIN(threadInfo->fileSplitter->shardSize,
			      threadInfo->validDataLen - dataOffset);

	/* build shard header for this file */
	getShardHeader(threadInfo->ioBuffer + dataOffset,
		       MAX_SHARDINFO_VER,
		       threadInfo->task.offset + dataOffset,
		       thisShardLength,
		       &header);

	/* build shard file name */
	printSize = snprintf(fileName, MAX_PATH, "%llu%s",
			     (unsigned long long) (threadInfo->task.offset + dataOffset),
			     SHARD_SUFFIX);
	if (printSize >= MAX_PATH) {
	    printf("\n Shard File name cannot be accomodated in %d len buffer",
		   MAX_PATH);
	    return -1;
	}

	/* Create the shard file */
	fd = open(fileName, O_CREAT | O_RDWR, 0777);
	if (fd == -1) {
	    printf("\n shard file creation failed errno %d", errno);
	    return -1;
	}

	/* Write the shard header out */
	wsize = write(fd, &header, sizeof(header));
	if (wsize != sizeof(header)) {
	    printf("\n short write/faile write for shard errno %d", errno);
	    return -1;
	}

	/* Write the shard data out */
	wsize = write(fd, threadInfo->ioBuffer + dataOffset, thisShardLength);
	if (wsize != thisShardLength) {
	    printf("\n short write/faile write for shard errno %d", errno);
	    return -1;
	}

	/* close the file */
	if (close(fd) != 0) {
	    printf("\n close failed for shard errno %d", errno);
	    return -1;
	}

	VLOG("\n[THR%d]shard created %s Offset %lu len %lu CRC %lu ",
	     threadInfo->threadNR,
	     fileName,
	     threadInfo->task.offset + dataOffset,
	     thisShardLength,
	     header.checkSum
	     );

	/* Move on to next shard */
	dataOffset += thisShardLength;
    }
    return 0;
}


/*
 *-----------------------------------------------------------------------------
 *
 * IOThreadGetNextTask --
 *      Compute the next task that a thread needs to perform
 *
 * Results:
 *      none
 * -----------------------------------------------------------------------------
 */
static void
IOThreadGetNextTask(ThreadInfo *threadInfo)
{

    /* generate the next sector number under task mutex */
    if (pthread_mutex_lock(&threadInfo->fileSplitter->taskMutex)) {
	printf("fatal mutex locking error bailing out %d", errno);
	exit(255);
    }

    threadInfo->task.offset = threadInfo->fileSplitter->curr_offset;
    threadInfo->fileSplitter->curr_offset += threadInfo->fileSplitter->readChunkSize;

    if (pthread_mutex_unlock(&threadInfo->fileSplitter->taskMutex)) {
	printf("fatal mutex unlocking error bailing out %d", errno);
	exit(255);
    }

    VLOG("\nTHR%d: Task at offset %lu length %d ",
	 threadInfo->threadNR, threadInfo->task.offset,
	 threadInfo->fileSplitter->readChunkSize);
}

/*
 *-----------------------------------------------------------------------------
 *
 * IOThread --
 *      Thread routine for performing IO
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static void *
IOThread(void *args)
{
    ThreadInfo *threadInfo =(ThreadInfo *) args;
    int ret;


    IOThreadGetNextTask(threadInfo);
    while(threadInfo->task.noMoreTask == 0) {
	ssize_t ioSize;
	ioSize = pread64(threadInfo->fileFD,
			 threadInfo->ioBuffer,
			 threadInfo->fileSplitter->readChunkSize,
			 threadInfo->task.offset);
	if (ioSize == -1) {
	    /* read error */
	    printf("\n THR%d read failed, bailing out %lld errno %d",
		   threadInfo->threadNR, (long long int)ioSize, errno);
	    threadInfo->fileSplitter->operationFailed = 1;
	    break;
	}

	if (ioSize == 0) {
	    /* end of file */
	    break;
	}

	threadInfo->validDataLen = ioSize;
	ret = IOThreadStoreShards(threadInfo);
	if (ret != 0) {
	    threadInfo->fileSplitter->operationFailed = 1;
	    break;
	}

	/* Get next task for this thread */
	IOThreadGetNextTask(threadInfo);
    }

    return NULL;
}

/*
 *-----------------------------------------------------------------------------
 *
 * run --
 *      fork of parallel worker threads and wait for their completion
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
run()
{
    unsigned int i;
    int ret;

    /* fire up threads */
    for (i = 0; i < fileSplitter.numThreads; i++) {
	ret = pthread_create(&fileSplitter.threadInfo[i].pthread,
			     NULL,
			     IOThread,
			     (void *) &fileSplitter.threadInfo[i]);
	if (ret) {
	    printf("\nfailed to spawn thread %d %d", ret, errno);
	    return ret;
	}
    }

    for (i = 0; i < fileSplitter.numThreads; i++) {
	ret = pthread_join(fileSplitter.threadInfo[i].pthread,
			   NULL);
	if (ret) {
	    printf("\nfailed to join thread %d %d", ret, errno);
	    return ret;
	}
    }

    if (fileSplitter.operationFailed) {
	printf("\nSplit operation failed see verbose logs for errors: %d", errno);
	return -1;
    }

    ret = WriteFileMeta(&fileSplitter);
    if (ret) {
	return ret;
    }

    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 *  setup --
 *  Setup the file splitter environment
 *
 *  Results:
 *  0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
setup()
{
    unsigned int i;
    int ret;

    /* alloc thread info structures */
    fileSplitter.threadInfo = malloc(sizeof(fileSplitter.threadInfo[0]) *
				     fileSplitter.numThreads);
    if (fileSplitter.threadInfo == NULL) {
	printf("\nfailed to allocate info");
	return -1;
    }

    for (i = 0; i < fileSplitter.numThreads; i++) {
	fileSplitter.threadInfo[i].ioBuffer = malloc(fileSplitter.readChunkSize);
	if (fileSplitter.threadInfo[i].ioBuffer == NULL) {
	    printf("\nfailed to alloc io memory ");
	    return -1;
	}
	memset(fileSplitter.threadInfo[i].ioBuffer, 0,
	       fileSplitter.readChunkSize);

	/*
	 * some versions of pread/pwrite are not thread safe
	 * To protect against this each thread has its own fd
	 */
	fileSplitter.threadInfo[i].fileFD = open(fileSplitter.fileName,
						 O_RDWR);
	if (fileSplitter.threadInfo[i].fileFD == -1) {
	    printf("\nFailed to open file %d", errno);
	    return -1;
	}

	fileSplitter.threadInfo[i].threadNR = i;
	fileSplitter.threadInfo[i].fileSplitter = &fileSplitter;
    }

    ret = pthread_mutex_init(&fileSplitter.taskMutex, NULL);
    if (ret) {
	printf("\nMutex Initialization failed:%d", errno);
	return -1;
    }

    /* create the output directory */
    ret = mkdir(fileSplitter.outputDir, 0777);
    if (ret) {
	printf("\nFailed to create output directory: %d", errno);
	return -1;
    }

    ret = chdir(fileSplitter.outputDir);
    if (ret) {
	printf("\nFailed to cd into output directory: %d", errno);
	return -1;
    }

    /* get the length of the file upfront */
    fileSplitter.fileLength = lseek64(fileSplitter.threadInfo[0].fileFD,
				      0,
				      SEEK_END);
    if (fileSplitter.fileLength == (off64_t) -1) {
	printf("\nFailed to get file length: %d", errno);
	return -1;
    }

    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * parseOptions --
 *      Parse command line options
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
#define DEFAULT_THREADS    (10)
#define DEFAULT_SHARD_SIZE (1024 * 1024)
#define DEFAULT_CHUNK_SIZE (5 * DEFAULT_SHARD_SIZE)
int
parseOptions(int argc, char **argv)
{
    int option_index = 0;
    int c;

    struct option options[] = {
	{"filename", 1 , NULL, 'f' },
	{"outputdirname", 1, NULL, 'd' },
	{"shardsize", 1, NULL, 's' },
	{"numthreads", 1, NULL, 'n'},
	{"readchunksize", 1, NULL, 'r'},
	{"reconcile", 0, NULL, 'R' },
	{"reconcilestat", 0, NULL, 'S' },
	{"verbose", 0, NULL, 'V' },
	{0 , 0, 0 ,0}
    };

    /* set up defaults */
    memset(&fileSplitter, 0, sizeof(fileSplitter));
    fileSplitter.numThreads = DEFAULT_THREADS;
    fileSplitter.shardSize = DEFAULT_SHARD_SIZE;
    fileSplitter.readChunkSize  = DEFAULT_CHUNK_SIZE;
    verbose    = 0;
    fileSplitter.operationFailed = 0;
    fileSplitter.fileName[0] = 0;
    fileSplitter.outputDir[0] = 0;
    fileSplitter.reconcileMode = 0;
    fileSplitter.reconcileStat = 0;

    while (1) {
	c = getopt_long (argc, argv, "f:d:s:n:r:RSV",
			 options, &option_index);
	if (c == -1) {
	    break;
	}

	switch (c) {
	case 'f':
	    strncpy(fileSplitter.fileName, optarg, SECTOR_SIZE);
	    continue;
	case 'd':
	    strncpy(fileSplitter.outputDir, optarg, SECTOR_SIZE);
	    continue;
	case 's':
	    fileSplitter.shardSize = atoi(optarg);
	    continue;
	case 'n':
	    fileSplitter.numThreads = atoi(optarg);
	    continue;
	case 'r':
	    fileSplitter.readChunkSize = atoi(optarg);
	    continue;
	case 'V':
	    verbose = 1;
	    continue;
	case '?':
	    return -1;
	case 'R':
	    fileSplitter.reconcileMode = 1;
	    continue;
	case 'S':
	    fileSplitter.reconcileStat = 1;
	    continue;
	default:
	    return -1;
	}
    }

    if (fileSplitter.reconcileMode) {
	return 0;
    }

    if (fileSplitter.reconcileStat) {
	return 0;
    }

    /* Sanitize inputs */
    if (fileSplitter.fileName[0] == 0 || fileSplitter.outputDir[0] == 0) {
	return -1;
    }

    if (fileSplitter.shardSize % SECTOR_SIZE) {
	printf("\n Shard size must be a multiple of %d", SECTOR_SIZE);
	return -1;
    }

    if (fileSplitter.shardSize <= sizeof(shardHeader)) {
	printf("\n Shard size must be minimum of %lu", sizeof(shardHeader));
	return -1;
    }

    fileSplitter.shardSize -= sizeof(shardHeader);

    fileSplitter.readChunkSize = MAX(fileSplitter.readChunkSize, fileSplitter.shardSize);
    fileSplitter.readChunkSize /= fileSplitter.shardSize;
    fileSplitter.readChunkSize *= fileSplitter.shardSize;

    VLOG("\n File Splitter:"
	 "\n\t fileName:          %s"
	 "\n\t dirName:           %s"
	 "\n\t shardSize:         %u"
	 "\n\t numThreads:        %u"
	 "\n\t readChunkSize:     %u"
	 "\n\t reconcileMode:     %s"
	 "\n\t reconcileStat:     %s"
	 "\n\t verbose:           %s",
	 fileSplitter.fileName,
	 fileSplitter.outputDir,
	 fileSplitter.shardSize,
	 fileSplitter.numThreads,
	 fileSplitter.readChunkSize,
	 fileSplitter.reconcileMode ? "TRUE" : "FALSE",
	 fileSplitter.reconcileStat ? "TRUE" : "FALSE",
	 verbose ? "TRUE" : "FALSE"
	 );
    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * fileSplitter --
 *
 *      Split file into fixed sized shards.
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
int
main(int argc, char **argv)
{
    int results;

    do {

	results = parseOptions(argc, argv);
	if (results) {
	    help(argc, argv);
	    break;
	}

	if (fileSplitter.reconcileMode) {
	    results = reconcile_shards(argc, argv);
	    break;
	}

	if (fileSplitter.reconcileStat) {
	    results = reconcile_stat(argc, argv);
	    break;
	}

	results = setup();
	if(results) {
	    break;
	}

	results = run();
	if(results) {
	    break;
	}

    } while (0);

    if (results == 0) {
	VLOG("\nSUCCESS");
    } else {
	printf("\nFAIL");
    }
    VLOG("\n");
    return results;
}
