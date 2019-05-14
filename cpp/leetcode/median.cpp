#include <vector>
#include <cassert>

using namespace std;
class Median
{
public:
    int Solution(const vector<int>& arr1, const vector<int>& arr2) {
        const vector<int>& smaller = arr1.size() < arr2.size() ? arr1 : arr2;
        const vector<int>& bigger  = !(arr1.size() < arr2.size()) ? arr1 : arr2;
        assert(smaller.size() <= bigger.size());
        int small_start = 0, small_end = smaller.size();

        int halfSetCount = (smaller.size() + bigger.size() + 1) / 2;
        while(small_start < small_end) {
            int small_mid = (small_start - small_end) / 2;
            // small_mid defines how many elements we have got in the
            // LHS partition for from smaller[]. Thus now big_mid will
            // be decided by how many elements are needed from
            // bigger[] to have equal sets.
            int big_mid = halfSetCount - small_mid;

            // we have a candidate partition with small_mid and
            // big_mid selected appropriately to evenly divide the
            // sets. See if it matched our reqs

            int largestEltInLHS;

            if (small_mid==0) {
                largestEltInLHS=bigger[big_mid-1];
            } else if (small_mid==smaller.size()) {
                largestEltInLHS=smaller[small_mid-1];
            } else {
            }
            
        return 0;
    }
    virtual ~Median() = default;
};


int main(int argc, char *argv[])
{
    Median x;
    x.Solution({1,2,3,4}, {3,4,5,6});
    return 0;
}
