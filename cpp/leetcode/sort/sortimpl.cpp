//g++ -g -W -Wall --std=c++17 ./sortimpl.cpp; ./a.out
#include <functional>
#include <random>
#include <chrono>
#include <algorithm>
#include <iostream>
#include <cassert>
#include "sort.h"
using namespace std;


std::vector<int>& sortutil::insertion(std::vector<int>& input)
{
   for (auto i=1U; i < input.size(); i++) {
      for (signed j=i; j >=0 ; j--) {
         if (input[j] < input[j-1]) {
            std::swap(input[j],input[j-1]);
         } else {
            break;
         }
      }
   }
   return input;
}



bool sortutil::isSorted(std::vector<int> &input)
{
   auto knowRel = sortutil::detail::Rel::EQL;
   auto size = input.size();
   if (size == 0 || size == 1) {
      return true;
   }

   knowRel = sortutil::detail::getRel(input[0], input[1]);

   for (auto j=1U; j < input.size(); j++) {
      auto thisRel = sortutil::detail::getRel(input[j-1], input[j]);
      if (thisRel == sortutil::detail::Rel::EQL)
         continue;

      /* make note of the relationship */
      if (knowRel == sortutil::detail::Rel::EQL &&
          thisRel != sortutil::detail::Rel::EQL) {
         knowRel = thisRel;
      }

      if (thisRel != knowRel) {
         cout << "Exception" << j << " " << input[j - 1] << " " << input[j] << endl;
         return false;
      }
    }
   return true;
}


std::vector<int>& sortutil::selection(std::vector<int>& input) {
   for (auto i = 0U; i < input.size(); i++)
      for (auto j = i + 1U; j < input.size(); j++) {
         if (input[j] < input[i]) {
            std::swap(input[j], input[i]);
         }
      }
   return input;
}


std::vector<int>& sortutil::bubble(std::vector<int>& input)
{
   bool swapped;
   for (auto i=0U; i < input.size(); i++) {
      swapped = false;
      for (auto j = 1U; j < input.size() - i; j++) {
         if (input[j-1] > input[j]) {
            std::swap(input[j-1], input[j]);
            swapped = true;
         }
      }
      if (!swapped) {
         break;
      }
   }
   return input;
}


void dumpHisto(vector<int>& vec) {
   vector<int> hist(vec.size(),0);
   for (auto x: vec) {
      if (x > (signed)vec.size()) assert(false);
      ++hist[x];
   }

   for(auto j=0U; j < vec.size(); j++) {
      std::cout << endl << j << ":\t";
      for (int i = 0; i < hist[j]; i++)
         std::cout << "*";
   }
}



void dumpVector(vector<int> &vec) {
   cout << endl;
   std::for_each(vec.begin(), vec.end(),[](int &x) {cout << " "  << x;});
}

int main(int argc, char **argv)
{
   (void)argc;
   (void)argv;
   const unsigned NUM_ELTS=100;
   std::default_random_engine
      eng(std::chrono::steady_clock::now().time_since_epoch().count());
   std::uniform_int_distribution<int> dist(0,NUM_ELTS-1);
   vector<int> x(NUM_ELTS);

   auto genNew = std::bind(dist, eng);
   std::generate(x.begin(), x.end(), [&genNew]() {return genNew();});
   dumpHisto(x);
   dumpVector(x);

   cout << endl << "Sorted";
   dumpVector(sortutil::bubble(x));
   assert(sortutil::isSorted(x));

   std::shuffle(x.begin(), x.end(), eng);
   dumpVector(sortutil::selection(x));
   assert(sortutil::isSorted(x));

   std::shuffle(x.begin(), x.end(), eng);
   dumpVector(sortutil::insertion(x));
   assert(sortutil::isSorted(x));

   return 0;
}
