#include <stdio.h>

void
swap(char *str, int start, int end) {
    char temp;
    temp = str[start];
    str[start] = str[end];
    str[end] = temp;
}


void
permutate(char *str, int start, int end) {
    if (start == end) {
        printf("\n%s", str);
        return;
    }

    for (int j = start; j <= end; j++) {
        swap(str, start, j);
        permutate(str, start+1, end);
        swap(str, start, j);
    }
}

int main(int argc, char **argv)
{
   char str[5] = {'f','a','r','a','z'};
    permutate(str, 0, 2);
}












