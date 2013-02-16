#ifndef _SPLITTER_
#define _SPLITTER_

#include "fileSplitterLayout.h"

extern unsigned int verbose;
#define VLOG(fmt, ...) do {                     \
	if (verbose) {				\
	    printf(fmt, ##__VA_ARGS__);		\
	}					\
    } while (0);


int reconcile_shards(int argc, char **argv);
int reconcile_stat(int argc, char **argv);

#endif
