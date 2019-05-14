#include <iostream>
#include <vector>
using namespace std;
int vals[] = {0,1,2,3};

int& setVal(int idx) {
    return vals[idx];
}

void printMat(vector<vector<int>> ans) {
    for (int j = 0 ; j < ans.size() ; j++) {
        vector<int> &subSet = ans[j];
        cout << "\n {";
        if (subSet.size() == 0) {
            cout << "{}";
        }
        for (std::vector<int>::iterator it = subSet.begin(); it != subSet.end(); it++) {
            cout << " "<< *it;
        }
        cout << "}";
    }
    cout << "\n";
}


vector<vector<int>> getsubsets(vector<int> set)
{
    vector<vector<int>> retAns;
    vector<int> empty;
    retAns.push_back(empty);

    for(int i = 0 ; i  < set.size(); i++) {
        vector<vector<int>> hasElt = retAns;

        for (int j = 0 ; j < retAns.size(); j++)
            hasElt[j].push_back(set[i]);

        for (int j = 0 ; j < hasElt.size(); j++)
            retAns.push_back(hasElt[j]);


    }
    return retAns;
}


int main() {
    vector<vector<int>> ans;
    vector<int> set = {1,2,3,4};
    std::cout << "Hello";
    setVal(0) = 500;
    std::cout << vals[0] << vals[1] << vals[2];

    ans = getsubsets(set);
    printMat(ans);
}







