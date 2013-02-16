/*
 * fileReconcile.c --
 *
 *      Reconcile file shards created by the fileSplitter program
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
#define FMT64 "%lu"

uint8_t mask[] =  { 0x1 , 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80 };

typedef struct FileReconcile {
    int originalFileFD;
    int reconcileFileFD;
    FileMeta fileMeta;
    ReconcileMeta reconcileMeta;
} FileReconcile;

FileReconcile fileReconcile;

static
uint64_t getReconcileBitMapSize(FileReconcile *fr)
{
    uint64_t bmSize;
    /* Total number of shards */
    bmSize = ROUND_UP(fr->fileMeta.length, fr->fileMeta.shardSize);

    /* one bit per shard */
    bmSize = ROUND_UP(bmSize, 8);

    VLOG("BM Size %lu", bmSize);
    return bmSize;
}

/*
 *-----------------------------------------------------------------------------
 *
 * CreateReconcileFile --
 *      Creates a reconcile meta file, to record info/progress about file
 *      reconcilation
 *
 * NOTE:
 *      fr->fileMeta must be initialized before calling into this function
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
CreateReconcileFile(FileReconcile *fr)
{
    off64_t fileLength;
    uint64_t bmSize, bytesDone, zero;

    fr->reconcileFileFD = open(SHARD_RECONCILE_FILE,
			       O_CREAT | O_RDWR | O_TRUNC,
			       0777);
    if (fr->reconcileFileFD == -1) {
	printf("\n Failed to create reconcile file %d", errno);
	return -1;
    }

    bmSize = getReconcileBitMapSize(fr);

    /* write out the bitmap first */
    fileLength = lseek64(fr->reconcileFileFD, sizeof(fr->reconcileMeta) , SEEK_SET);
    if (fileLength == (off64_t) -1) {
	printf("\n lseek failed on reconcile meta file %d", errno);
	close(fr->reconcileFileFD);
	return -1;
    }

    /*
     *  not the most efficient form of writing zeros to a file
     *  Note: We could have used the sparseness property of the underlying fs to
     *  zero out the file contents. We choose not to depend on this property as its
     *  not a standard. Instead just write out the zero bitmap
     */
    bytesDone = 0;
    while (bytesDone < bmSize) {
	uint64_t bytesWritten;
	zero = 0;

	bytesWritten = write(fr->reconcileFileFD, &zero,
			     MIN(sizeof(uint64_t), bmSize - bytesDone));

	if (bytesWritten != MIN(sizeof(uint64_t), bmSize - bytesDone)) {
	    printf("\n failed to write out bitmap %d", errno);
	    close(fr->reconcileFileFD);
	    return -1;
	}

	bytesDone += bytesWritten;
    }

    /* write out the header */
    memset(&fr->reconcileMeta, 0, sizeof(fr->reconcileMeta));
    fr->reconcileMeta.meta.magic = SHARD_MAGIC;
    fr->reconcileMeta.meta.version = MAX_SHARDINFO_VER;
    fr->reconcileMeta.meta.totalShards =
	ROUND_UP(fr->fileMeta.length, fr->fileMeta.shardSize);

    bytesDone = pwrite(fr->reconcileFileFD,
		       &fr->reconcileMeta,
		       sizeof(fr->reconcileMeta),
		       0);
    if (bytesDone != sizeof(fr->reconcileMeta)) {
	printf("\n failed to write out reconcile meta %d", errno);
	close(fr->reconcileFileFD);
	return -1;
    }

    if (fsync(fr->reconcileFileFD) != 0) {
	printf("\n failed to sync out reconcile file meta %d", errno);
	close(fr->reconcileFileFD);
	return -1;
    }

    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * OpenReconcileFile --
 *      Open/Creates and reconciliation  session
 *
 * NOTE:
 *      fr->fileMeta must be initialized before calling into this function
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
OpenReconcileFile(FileReconcile *fr)
{
    int ret;
    off64_t fileLength;
    uint64_t bmSize;
    int createReconcileFile;

    bmSize = getReconcileBitMapSize(fr);

    /* Do we have a valid reconcile file present */
    createReconcileFile = 0;
    do {
	fr->reconcileFileFD = open(SHARD_RECONCILE_FILE, O_RDWR);
	/* file not found */
	if (fr->reconcileFileFD == -1) {
	    if (errno != ENOENT) {
		printf("\nOpening on reconcile failed with %d", errno);
	    }
	    createReconcileFile = 1;
	    break;
	}

	/* read the reconcile file meta */
	if (sizeof(fr->reconcileMeta) != read(fr->reconcileFileFD,
					      &fr->reconcileMeta,
					      sizeof(fr->reconcileMeta))) {
	    printf("\nshort read/failed read on reconcile file meta, WILL RECREATE !!!!");
	    close(fr->reconcileFileFD);
	    createReconcileFile = 1;
	    break;
	}

	/* do we have a valid header */
	if (fr->reconcileMeta.meta.magic != SHARD_MAGIC ||
	    fr->reconcileMeta.meta.version != MAX_SHARDINFO_VER ||
	    fr->reconcileMeta.meta.reconciledShards > fr->reconcileMeta.meta.totalShards) {
	    printf("\n reconcile meta corrupted WILL RECREATE !!!!");
	    close(fr->reconcileFileFD);
	    createReconcileFile = 1;
	    break;
	}

	/* do we have a valid reconcile bitmap */
	fileLength = lseek64(fr->reconcileFileFD, 0, SEEK_END);
	if (fileLength == (off64_t) -1) {
	    printf("\n lseek failed on reconcile meta file %d", errno);
	    close(fr->reconcileFileFD);
	    return -1;
	}

	if (fileLength != sizeof(fr->reconcileMeta) + bmSize) {
	    printf("\n corrupt reconcile bitmap expected  fileLen "FMT64" got "FMT64
		   "WILL RECREATE !!!",
		   sizeof(fr->reconcileMeta) + bmSize, fileLength);
	    close(fr->reconcileFileFD);
	    createReconcileFile = 1;
	    break;
	}
    } while (0);

    /* If the reconcile file is absent then create one */
    if (createReconcileFile) {
	ret = CreateReconcileFile(fr);
	if (ret) {
	    return ret;
	}
    }

    /* at this point we should have a valid reconcile file opened */
    VLOG("\n Reconcile file %s %s", SHARD_RECONCILE_FILE, createReconcileFile ?
	 "Created" : "Opened");
    VLOG("\n Reconciled Shards "FMT64, fr->reconcileMeta.meta.reconciledShards);
    VLOG("\n Total Shards "FMT64, fr->reconcileMeta.meta.totalShards);
    VLOG("\n Awaiting Shards "FMT64, fr->reconcileMeta.meta.totalShards -
	 fr->reconcileMeta.meta.reconciledShards);
    VLOG("\n Bitmap size "FMT64, bmSize);

    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * ReadFileMeta --
 *      Read in info about the original file
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
ReadFileMeta(FileMeta *fileMeta)
{
    int fd;

    fd = open(SHARD_META_FILE, O_RDONLY);
    if (fd == -1) {
	printf("\n failed to open filemeta  %s %d", SHARD_META_FILE,  errno);
	return -1;
    }


    if (sizeof(*fileMeta) != read(fd, fileMeta, sizeof(*fileMeta))) {
	printf("\nShort read on shard meta file Bailing out %d", errno);
	return -1;
    }

    if(fileMeta->magic != SHARD_MAGIC) {
	printf("\nfile meta file magic mismatch");
	return -1;
    }

    if(fileMeta->version != MAX_SHARDINFO_VER) {
	printf("\nfile meta file version mismatch");
	return -1;
    }

    VLOG("\n Read filemeta %s", SHARD_META_FILE);
    VLOG("\n fileName %s", fileMeta->fileName);
    VLOG("\n filelength "FMT64, fileMeta->length);
    VLOG("\n ShardSize "FMT64, fileMeta->shardSize);

    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * setup --
 *
 *      setup environment required for file reconciliation
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
static int
setup(FileReconcile *fr)
{
    int ret;

    /* Slurp in meta data about the original file */
    ret = ReadFileMeta(&fileReconcile.fileMeta);
    if (ret) {
	return ret;
    }

    /*
     * Read in the reconcile info file
     * This keeps track of how many shards have been reconciled and thier
     * locations. Using this we can continue previous reconciliation session.
     */
    ret = OpenReconcileFile(fr);
    if (ret) {
	return ret;
    }

    fr->originalFileFD = open((char *) fr->fileMeta.fileName,
			      O_CREAT | O_RDWR, 0777);
    if (fr->originalFileFD == -1) {
	printf("\nfailed to open original file %s", fr->fileMeta.fileName);
	return -1;
    }
    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * recordShardReconciliation --
 *
 *      record the fact that a particular shard has been reconciled into the
 *      original file.
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
int
recordShardReconciliation(FileReconcile *fr, uint64_t shardNumber)
{
    int ret;
    uint64_t byteNumber;
    unsigned int bitNumber;
    uint8_t bitMapByte;

    byteNumber = shardNumber / 8;
    bitNumber = shardNumber % 8;

    /* Read in part of the reconcile bitmap */
    ret = pread(fr->reconcileFileFD,
		&bitMapByte,
		sizeof(bitMapByte),
		sizeof(ReconcileMeta) + byteNumber);
    if (ret != sizeof(bitMapByte)) {
	printf("\npread failed on reconcile bitmap %d", errno);
	return -1;
    }

    /* Nothing to do if  the byte is already set */
    if (bitMapByte & mask[bitNumber]) {
	printf("\nshard number "FMT64" has already been recorded as reconciled",
	       shardNumber);

	if (fr->reconcileMeta.meta.reconciledShards >=  fr->reconcileMeta.meta.totalShards) {
	    printf("\n Reconcilation done !!");
	}

	return 0;
    }

    /*
     *  Note on ordering writes to bitmap and reconcile header
     *  a-ReconcileHeader fits in a sector an so writes to it are atomic
     *  b-Header is authoritative in case we have a divergence in header.reconciledShards
     *   and bitmap bits.
     *  c-Given a & b we write the bitmap first and then update the header bits
     *  d-We can crash after the fact the write out the bitmap.
     *    If we do crash, what will happen ?
     *    Number of counted shards in the header will always be less than number
     *    of bits in the bitmap. This state will never let the file to be fully assembled
     *    and this situation is DETECTABLE, user can then attempt a reconcile all over
     *    again.
     *
     *    This is not an ideal solution, the ideal solution would be to do multisector
     *    atomic updates using journaling or have a format where the bitCount and bitmap
     *    are part of the same sector.
     */
    bitMapByte |= mask[bitNumber];

    /* write out part of the reconcile bitmap */
    ret = pwrite(fr->reconcileFileFD,
		 &bitMapByte,
		 sizeof(bitMapByte),
		 sizeof(ReconcileMeta) + byteNumber);
    if (ret != sizeof(bitMapByte)) {
	printf("\npwrite failed on reconcile bitmap %d", errno);
	return -1;
    }

    /* update the shard count in the reconcile header */
    fr->reconcileMeta.meta.reconciledShards++;
    ret = pwrite(fr->reconcileFileFD,
		 &fr->reconcileMeta,
		 sizeof(fr->reconcileMeta),
		 0);
    if (ret != sizeof(fr->reconcileMeta)) {
	printf("\npwrite failed on reconcile header %d", errno);
	return -1;
    }

    VLOG("\nRecorded Shard reconcilation for shard "FMT64, shardNumber);
    if (fr->reconcileMeta.meta.reconciledShards <  fr->reconcileMeta.meta.totalShards) {
	VLOG("\n Expecting "FMT64" more shards",
	     fr->reconcileMeta.meta.totalShards - fr->reconcileMeta.meta.reconciledShards );
    } else {
	VLOG("\n Reconcilation done !!");
    }

    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * reconcile_shards --
 *
 *      reconcile shards specifed on the command line
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
int
reconcileShards(FileReconcile *fr, char *shardFileName)
{
    int fd;
    int ret;
    char *dataBuffer;
    shardHeader hdr, expectedHdr;
    uint64_t shardNumber;

    fd = open(shardFileName, O_RDONLY);
    if (fd == -1) {
	printf("\nfailed to open shard %s %d", shardFileName, errno);
	return -1;
    }

    ret = read(fd, &hdr, sizeof(hdr));
    if (ret != sizeof(hdr)) {
	printf("\nShort read on shard header %d", errno);
	close(fd);
	return -1;
    }

    /* is the basic shard header correct */
    if (hdr.magic != SHARD_MAGIC ||
	hdr.version != MAX_SHARDINFO_VER) {
	printf("Invalid shard %s %d", shardFileName, errno);
	close(fd);
	return -1;
    }

    dataBuffer = malloc(hdr.length);
    if (dataBuffer == NULL) {
	printf("\nCannot allocate memory for shard %s lenght "FMT64, shardFileName, hdr.length);
	close(fd);
	return -1;
    }

    /* read in the shard data */
    ret = read(fd, dataBuffer, hdr.length);
    if (ret != hdr.length) {
	printf("\nshort read shard %s lenght "FMT64, shardFileName, hdr.length);
	close(fd);
	free(dataBuffer);
	return -1;
    }

    if (hdr.length > fr->fileMeta.shardSize) {
	printf("\n Shard size is greater that expected %s lenght "FMT64" max "FMT64,
	       shardFileName, hdr.length , fr->fileMeta.shardSize);
	close(fd);
	free(dataBuffer);
	return -1;
    }

    /* compute the expected header */
    getShardHeader((uint8_t *) dataBuffer,
		   hdr.version,
		   hdr.offset,
		   hdr.length,
		   &expectedHdr);
    if (expectedHdr.checkSum != hdr.checkSum) {
	printf("\nchecksum mismatch shard %s lenght expected "FMT64" got "FMT64,
	       shardFileName, expectedHdr.checkSum, hdr.checkSum);
	close(fd);
	free(dataBuffer);
	return -1;
    }

    /* Write out the data to the original file */
    ret = pwrite(fr->originalFileFD,  dataBuffer, hdr.length, hdr.offset);
    if (ret != hdr.length) {
	printf("\nshort write on original file");
	close(fd);
	free(dataBuffer);
	return -1;
    }

    /* record the the fact that we have reconciled this particular shard */
    shardNumber = hdr.offset / fr->fileMeta.shardSize;
    ret = recordShardReconciliation(fr, shardNumber);
    if (ret) {
	return ret;
    }

    close(fd);
    free(dataBuffer);
    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * reconcile_shards --
 *
 *      Print information about shards that are yet to be reconciled
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
int
reconcile_stat(int argc, char **argv) {
    uint64_t totalShards, i, j, awaitingAny;
    int ret;
    uint8_t bitMapByte;
    FileReconcile *fr = &fileReconcile;

    ret = setup(fr);
    if (ret) {
	return ret;
    }

    printf("\nStill Awaiting ..");
    awaitingAny = 0;
    totalShards = ROUND_UP(fr->fileMeta.length, fr->fileMeta.shardSize);
    for (i = 0; i < totalShards;) {
	uint64_t byteNumber = i / 8;
	/* Read in part of the reconcile bitmap */
	ret = pread(fr->reconcileFileFD,
		    &bitMapByte,
		    sizeof(bitMapByte),
		    sizeof(ReconcileMeta) + byteNumber);
	if (ret != sizeof(bitMapByte)) {
	    printf("\npread failed on reconcile bitmap %d", errno);
	    return -1;
	}

	for (j = 0; j < 8 && i < totalShards; i++, j++) {
	    if ((bitMapByte & mask[j]) == 0) {
		printf("\n "FMT64".p", i * fr->fileMeta.shardSize);
		awaitingAny++;
	    }
	}
    }

    if (!awaitingAny) {
	printf("\nReconciliation done");
    } else {
	printf("\n "FMT64" Shards", awaitingAny);
    }
    printf("\n");
    return 0;
}

/*
 *-----------------------------------------------------------------------------
 *
 * reconcile_shards --
 *
 *      reconcile shards specifed on the command line
 *
 * Results:
 *      0 on success, error code otherwise
 * -----------------------------------------------------------------------------
 */
int
reconcile_shards(int argc, char **argv)
{
    int ret,i;

    if (argc < 2) {
	printf("specify shards in command line");
	return -1;
    }

    ret = setup(&fileReconcile);
    if (ret) {
	return ret;
    }


    for (i = 1; i < argc; i++) {
	/*
	 * skip args not ending in .p
	 * file shards end with .p
	 */
	if (argv[i][strlen(argv[i]) - 1] != 'p') {
	    VLOG("skipping arg %s", argv[i]);
	    continue;
	}

	ret = reconcileShards(&fileReconcile, argv[i]);

	if (ret) {
	    printf("\n WARN Reconciling shard %s has failed",
		   argv[i]);
	}

	/*
	 * This slows down the system reconcile procedure by a
	 * considerable amount. Commented for now
	 */
	// fsync(fileReconcile.originalFileFD) ?
	//   printf("\n WARN sync failed on original file %d", errno) : {};
    }

    close(fileReconcile.originalFileFD);
    close(fileReconcile.reconcileFileFD);
    return 0;
}
