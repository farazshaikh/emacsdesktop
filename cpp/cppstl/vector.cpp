#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
using namespace std;

int SortArraysAtIndex(vector<vector<int>> sortedArrays, vector<int> indices) {
    unsigned int minIdx = 0;
    for (auto i = 1U; i < indices.size(); i++) {
        // check if value is less than the currently selected minimum
        if (sortedArrays[i][indices[i]] <
            sortedArrays[minIdx][indices[minIdx]] /* currently selected minimum */) {
            minIdx = i;
        }
    }
    return minIdx;
}

vector<int> mergesort(vector<vector<int>> sortedArrays) {
    vector<int> sortedArray;
    vector<int> indices(sortedArrays.size(), 0);
    cout << "Faraz" << SortArraysAtIndex(sortedArrays, indices);
    return sortedArray;
}





template<class ForwardIT, class T>
ForwardIT RemoveIF(ForwardIT first,ForwardIT last, const T& p) {
   first = std::find(first ,last ,p);
   if (first != last) {
      for(ForwardIT i = first; ++i != last;) {
         if (!(*i == p)) {
            *first++ = std::move(*i);
         }
      }
   }
   return first;
}







int main2() {
    //vector<string> mySet = {"one", "two", "three"};
    vector<int> mySet = {0 ,1 , 5, 7, 6 };
    vector<int> mySet2 = { 17, 23, 50, 54 };
    vector<int> mySet3 = { 33, 42, 98, 56, 70 };
    vector<vector <int>> merge(3);
    merge[0] = mySet;
    merge[1] = mySet2;
    merge[2] = mySet3;

    mergesort(merge);

    mySet.reserve(mySet.size() + mySet2.size() + mySet3.size());
    mySet.insert(mySet.end(), mySet2.begin(), mySet2.end());
    mySet.insert(mySet.end(), mySet3.begin(), mySet3.end());

    std::sort(mySet.begin(), mySet.end());


    for (auto x = mySet.begin(); x != mySet.end(); x++) {
        cout << " " << *x;
    }
    return 0;
}


vector<int> vec = {1, 5, 2, 4 ,3 ,3 ,4 ,42, 2, 3 ,2 ,23,4 ,3 ,3 ,2 ,25, 5};
int main() {


   for (auto &x: vec)
      std::cout << " "  << x;
   cout << endl;
   
   vec.erase(RemoveIF(vec.begin(), vec.end(), 2), vec.end());
   for (auto &x: vec)
      std::cout << " "  << x;
   return 1;
}
