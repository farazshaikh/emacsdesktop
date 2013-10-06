// clan/g++ -g -std=c++11 /tmp/burncores.cpp -lpthread
// Burn cores by enumerating bit patterns for all 128 bit number.
// author: Faraz Shaikh
// Test program to burn cores on a VM/physical machine
// No licencse

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <thread>
#include <list>
#include <stdint.h>
#include <atomic>
#include <vector>

using namespace std;
namespace {
    const uint32_t numBits = 128;
};


class odoMeter {
    uint32_t thrId;
    void printPatternInt(uint32_t  numBits,
                         uint32_t curBit,
                         char *str)
    {
        if (curBit == 0) {
            cout << thrId << " " <<  str << "\n";
            cout.flush();
            return;
        }

        str[numBits - curBit] = '0';
        printPatternInt(numBits, curBit-1, str);
        str[numBits - curBit] = '1';
        printPatternInt(numBits, curBit-1,  str);
    }

public:
    odoMeter(uint32_t thrId) {
        this->thrId = thrId;
    }

    void printPattern(uint32_t numBits)  {
        char *str;
        str = new char[numBits + 1];
        memset(str, 0, numBits +1);
        printPatternInt(numBits, numBits, str);
    }
};

void RunOdoMeter(uint32_t thrId, uint32_t y) {
    odoMeter odoMeter(thrId);
    odoMeter.printPattern(128); // calulate all bit patterns from 0 to 2^32
}

int
main(int argc, char **argv)
{
    vector<thread> threads;
    uint32_t y;
    uint32_t numCoresToBurn;

    cout << argv[0] << " numCoresToBurn";

    numCoresToBurn = atoi(argv[1]);

    for (uint32_t i = 0; i < numCoresToBurn; i++) {
        threads.push_back(std::thread(RunOdoMeter, i, y));
    }

    for (auto& th: threads) {
        th.join();
    }
}
