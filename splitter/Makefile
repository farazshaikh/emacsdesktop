all	: patternWriter fileSplitter

fileSplitter	: fileSplitter.c fileReconcile.c  fileSplitterLayout.h
	gcc -g -Wall  fileSplitter.c fileReconcile.c -o fileSplitter -lpthread

patternWriter	: patternwriter.c
	gcc -g -Wall  patternwriter.c -o patternwriter

clean :
	rm fileSplitter patternwriter


