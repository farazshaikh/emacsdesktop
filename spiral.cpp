/* print a N x N array in spiral fashion */
#include <iostream>
#include <cstdlib>
#include <iomanip>
using namespace std;

void
printnum(int row, int col, int nMax) {
        if (row < nMax && col < nMax) {
            cout << setw(4) << (row * nMax) + col;
        }
}
void
printspiral(int nMax,
            int eachDirection,
            int *col,
            int *row)
{
    /* print in up dir */
    for (int k = 0; k < eachDirection; k++) {
        if (*row) {
            (*row)--;
        } else {
            return;
        }
        printnum(*row,*col,nMax);
    }
    /* print in right */
    for (int k = 0; k < eachDirection; k++) {
        if (*col + 1 < nMax) {
            (*col)++;
        } else {
            return;
        }
     printnum(*row,*col,nMax);

    }

    /* print in down dir */
    for (int k = 0; k < eachDirection; k++) {
        if (*row + 1 < nMax) {
            (*row)++;
        } else {
            return;
        }
     printnum(*row,*col,nMax);

    }

    /* print in right direction */
    for (int k = 0; k < eachDirection; k++) {
        if (*col) {
            (*col)--;
        } else {
            return;
        }
     printnum(*row,*col,nMax);

    }
}

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


    for (int i = 0; i < num; i++) {
        for (int j = 0; j < num; j++) {
            //   cout << setw(4);
            cout << setw (4) << i * num + j;
        }
        cout << endl;
    }


    /* print each border */
    for (int spiral = 0; spiral < (num + 1) / 2; spiral++) {
        if (spiral == 0) {
            int loc = (num - 1) / 2;
            col = loc;
            row = loc + 1;
        } else {
            col = col - 1;
            row = row + 1;
        }

        /* each spiral grows in form 1,3,5,7 */
        numDir = spiral * 2 + 1;
        cout << "\nsprial"<< spiral << " is:";
        printspiral(num, numDir, &col, &row);
    }

}
