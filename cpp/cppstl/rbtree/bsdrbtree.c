// apt-get install libbsd-dev
#include <bsd/sys/tree.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define NUM_ELTS (5000)

#define NDEBUG
typedef struct IntNode  {
    uint32_t val;
    RB_ENTRY(IntNode) entry;
} IntNode;

int
IntNodeCompare(IntNode *a, IntNode *b) {
    return a->val - b->val;
}


#define __unused __attribute__((unused))
RB_HEAD(IntTree, IntNode);
typedef struct IntTree InTree;
RB_PROTOTYPE_STATIC(IntTree, IntNode, entry, IntNodeCompare);
RB_GENERATE(IntTree, IntNode, entry, IntNodeCompare);


int main(int argc, char **argv) {
    uint32_t i;
    struct IntTree intTree;
    IntNode *trav;
    RB_INIT(&intTree);



    srandom(NUM_ELTS);
    for (i = 0; i < NUM_ELTS; i++) {
        IntNode *x = malloc(sizeof *x);
        assert(x != NULL);
        x->val = random() % NUM_ELTS;
        RB_INSERT(IntTree, &intTree, x);
    }


    srandom(NUM_ELTS);
    for (i = 0; i < NUM_ELTS; i++) {
        IntNode x;
        x.val = random() % NUM_ELTS;
        trav = RB_FIND(IntTree, &intTree, &x);
        assert(trav != NULL && trav->val == x.val);
    }

    RB_FOREACH(trav, IntTree, &intTree) {
        printf("%d ", trav->val);
    }

    return 0;
}
