#include <iostream>
#include <string>

using namespace std;

static inline
void stringCharSwapChar(string &str, uint32_t i, uint32_t j)
{
    char t;
    t = str[i];
    str[i] = str[j];
    str[j] = t;
}

void permutate(string& str,
               uint32_t prefix_start,
               uint32_t prefix_len,
               uint32_t suffix_start,
               uint32_t suffix_len) {
    if (suffix_len == 1) {
        cout << str << endl;
    }
    for (uint32_t i = 0; i < suffix_len; i++) {
        stringCharSwapChar(str, suffix_start, suffix_start + i);
        permutate(str,
                  prefix_start,
                  prefix_len + 1,
                  suffix_start + 1,
                  suffix_len - 1);
        stringCharSwapChar(str, suffix_start + i, suffix_start);
    }
}
int main(int argc, char **argv) {
    string s;
    s = argv[1];
    //cout << s  << s.length() << endl;
    cout << endl;
    permutate(s, 0, 0, 0, s.length());
}



















