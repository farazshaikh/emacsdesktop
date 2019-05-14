#include <malloc.h>
#include <string.h>

/* all elements are valid */
typedef struct countedArray {
    int idx;     /* current idx */
    int len;     /* max len of buffer */
    int elem[0]; /* elements */
} countedArray;


void dumpCountedArray(countedArray *a) {
    int i;
    for (i = 0; i < a->len; i++) {
        printf("%d \n", a->elem[i]);
    }
}
void
copyOver(countedArray *src,
         int len,
         countedArray *dst) {
    while (len) {
        dst->elem[dst->idx] = src->elem[src->idx];
        src->idx++;
        dst->idx++;
        len--;
    }
}

countedArray *
newCountedArray(int len) {
    countedArray *ret;
    ret = (countedArray *)malloc(sizeof *ret + (len * sizeof(int)));
    memset(ret, 0, sizeof *ret + (len * sizeof(int)));
    ret->len = len;
    return ret;
}

void
mergetwosortedarrays(countedArray *a,
                     countedArray *b,
                     countedArray *c)
{
    a->idx = b->idx = c->idx = 0;
    while(a->idx < a->len && b->idx < b->len) {
        if (a->elem[a->idx] < b->elem[b->idx]) {
            c->elem[c->idx] = a->elem[a->idx];
            a->idx++;
        } else {
            c->elem[c->idx] = b->elem[b->idx];
            b->idx++;
        }
        c->idx++;
    }

    /* copy over remaining array */
    if (a->idx < a->len) {
        copyOver(a,a->len  - a->idx, c);
    } else {
        copyOver(b,b->len - b->idx, c);
    }
}

void
mergesort(countedArray *a) {
    int len1, len2;
    countedArray *part1, *part2;
    if (a->len == 1) {
        return;
    }

    /* split */
    len1 = a->len / 2;
    len2 = a->len - len1;

    part1 = newCountedArray(len1);
    part2 = newCountedArray(len2);

    a->idx = 0;
    copyOver(a, len1, part1);
    a->idx = len1;
    copyOver(a, len2, part2);

    mergesort(part1);
    mergesort(part2);
    mergetwosortedarrays(part1, part2, a);
    free(part1);
    free(part2);
}



main() {
    int i;
    countedArray *a, *b, *c, *d;


    d = newCountedArray(50000000);
    for (i = 0; i < 50000000; i++) {
        d->elem[i] = rand() % 50000000;
    }

    d->idx = 0;
    mergesort(d);

    dumpCountedArray(d);

    return 0;

    a = newCountedArray(10);
    b = newCountedArray(10);
    c = newCountedArray(a->len + b->len);

    for (i = 0; i < 10; i++) {
        a->elem[i] = i * 2;
        b->elem[i] = i * 3;
    }

    mergetwosortedarrays(a,b,c);

    dumpCountedArray(a);
    dumpCountedArray(b);
    dumpCountedArray(c);
    free(a);
    free(b);
    free(c);
}
