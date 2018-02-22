#include<stdio.h>
#include<iostream>

using namespace std;

static inline void swap(char *s, char *x) {
    char t;
    t = *s;
    *s = *x;
    *x = t;
}

int strlen(char * str) {
    int i = 0;
    while (str[i]) {
        i++;
    }
    return i;
}

void printchar(char *c, int len) {
    int i;
    for (i = 0; i < len; i++) {
        cout << c[i];
    }
}

void
permutate(char *s, int len, char *t, int len2) {
    int x;
    if (len2 == 1) {
        printchar(s,len);
        printchar(t, len2);
        cout << endl;
    }

    for (x = 0; x  < len2; x++) {
        swap(&t[0], &t[x]);
        permutate(s, len+1, t+1, len2 -1);
        swap(&t[x], &t[0]);
    }
}

int
main(int argc, char **argv) {
    int len, i;
    char *str = argv[1];
    len = strlen(str);
    std::cout << "strlen" << len;
    cout << endl;
    permutate(str, 0, str, len);
}
