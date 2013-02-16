#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#define MEM_BUFF_SIZE (512)

#define MIN(A,B) ((A)<(B)?(A):(B))

int main(int argc, char **argv)  {
    char pattern;
    char *buffer;
    unsigned int size;
    unsigned int byteswritten;
    int fd;

    if (argc != 3) {
	printf ("\nUsage %s ch,size filename\n",argv[0]);
	exit(0);
    }

    sscanf(argv[1],"%c,%u",&pattern,&size);
    buffer = malloc (MEM_BUFF_SIZE);
    if (NULL == buffer) {
	printf ("\nFailed to allocate buffers");
	exit(0);
    }

    fd = open(argv[2], O_WRONLY | O_CREAT, 0777);
    if (fd < 0) {
	printf("\ncannot open file errno %d\n",errno);
	exit(0);
    }


    byteswritten = 0;
    while (byteswritten < size)  {
	int written;
	memset(buffer, pattern++, MEM_BUFF_SIZE);
	written = write(fd,buffer,MIN(size-byteswritten,MEM_BUFF_SIZE));
	if (written != MIN(size-byteswritten,MEM_BUFF_SIZE)) {
	    printf("Cannot write to file %d",errno);
	    exit(0);
	}
	byteswritten +=  written;
    }

    printf("\nDone Writing pattern to file\n");
    close(fd);
    return 0;
}
