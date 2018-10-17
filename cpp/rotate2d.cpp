/*
 * Rotate a N*N 2D Matrix.
 * NO MATRIX IS STORED IN MEMORY.
 *
 * Only the alogrithm/equation to derive row and column number after
 * and before the rotation is described.

 * For printing purpose Numbers used are derived from their
 * row and column location as numRow * MAX_ROWS + column.
 *
 * fshaikh@cs.cmu.edu
 *
 * Sample OP
 *./a.out 4
 *   0   1   2   3
 *   4   5   6   7
 *   8   9  10  11
 *  12  13  14  15
 *left rotate
 *   3   7  11  15
 *   2   6  10  14
 *   1   5   9  13
 *   0   4   8  12
 *right rotate
 *  12   8   4   0
 *  13   9   5   1
 *  14  10   6   2
 *  15  11   7   3
 */
#include <iostream>
#include <cstdlib>
#include <iomanip>
using namespace std;

int main(int argc, char **argv) {
    int foo, *p;
    int num, col, row, numDir;

    foo = 4;
    p = &foo;
    *p++;

    num = 4;
    if (argc >= 2) {
        num = atoi(argv[1]);
    }

    /* original array */
    for (int i = 0; i < num; i++) {
        for (int j = 0; j < num; j++) {
            //   cout << setw(4);
            cout << setw (4) << i * num + j;
        }
        cout << endl;
    }

    /* left rotate */
    cout << "left rotate" << endl;
    for (int i = 0; i < num; i++) {
        for (int j = 0; j < num; j++) {
            //   cout << setw(4);
            int lri, lrj;
            lri = j;
            lrj = num - i - 1;
            cout << setw (4) << lri * num + lrj;
        }
        cout << endl;
    }

    /* left rotate */
    cout << "right rotate" << endl;
    for (int i = 0; i < num; i++) {
        for (int j = 0; j < num; j++) {
            //   cout << setw(4);
            int lri, lrj;
            lri = num - j -1;
            lrj = i;
            cout << setw (4) << lri * num + lrj;
        }
        cout << endl;
    }
}
