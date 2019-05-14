#include <vector>
namespace sortutil {
   namespace detail {
      typedef enum Rel {
         EQL,
         ASC,
         DSC,
      }Rel;
      Rel getRel(const int& first, const int &second) {
         Rel rel;
         if (first < second) {
            rel = ASC;
         } else if (first > second) {
            rel = DSC;
         } else {
            rel = EQL;
         }
         return rel;
      }
   }

   bool isSorted(std::vector<int>& input);
   std::vector<int>& bubble(std::vector<int>& input);
   std::vector<int>& selection(std::vector<int>& input);
   std::vector<int>& insertion(std::vector<int>& input);


   std::vector<int>& quicksort(std::vector<int>& input);
   std::vector<int>& mergesort(std::vector<int>& input);
   std::vector<int>& heapsort(std::vector<int>& input);

}
