#include <vector>
#include <iostream>
using namespace std;
vector<int>
mergesort(vector<int> arr1, vector<int> arr2) {
    int minArrayIdx;
    vector<int> sortedArray;
    vector<vector<int>> sortedArrays(2);
    sortedArrays[0] = arr1;
    sortedArrays[1] = arr2;

    minArrayIdx = 0;

    while (sortedArrays[0].size() && sortedArrays[1].size()) {
        while(sortedArrays[minArrayIdx].back() >
              sortedArrays[(minArrayIdx + 1) % 2].back()) {
            sortedArray.push_back(sortedArrays[minArrayIdx].back());
            sortedArrays[minArrayIdx].pop_back();
        }
        minArrayIdx = (minArrayIdx + 1) % 2;
    }

    if (arr1.size() == 0 && arr2.size() == 0) {
        return sortedArray;
    }

    /* copy over left of the array */
    minArrayIdx = 0;
    if (sortedArrays[1].size()) {
        minArrayIdx = 1;
    }

    while (sortedArrays[minArrayIdx].size()) {
        sortedArray.push_back(sortedArrays[minArrayIdx].back());
        sortedArrays[minArrayIdx].pop_back();
    }
    return sortedArray;
}

int main() {
    vector<int> ans;
    //ans = mergesort({1,2,3,6,7,8}, {4,5,9,10,11});
    ans = mergesort({}, {4,5,9,10,11});

    for (auto x = ans.begin(); x != ans.end(); x++)
        cout << " " << *x;
}
