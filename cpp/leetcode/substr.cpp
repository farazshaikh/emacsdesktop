// Substring algorithms
// 1. Naive
// 2. Knuth Morrison Pratt
// 3. Rabin Karp Rolling hash

#include <cassert>
#include <cstring>
#include <iostream>
#include <vector>

using namespace std;

// Naive
bool is_strict_prefix(const char *base, int base_len, const char *prefix,
                      int prefix_len, int &failed_offset) {
  if (prefix_len > base_len) {
    return false;
  }
  for (auto i = 0; i < prefix_len; i++) {
    if (base[i] != prefix[i]) {
      failed_offset = i;
      return false;
    }
  }
  return true;
}

int substr_naive(const char *base, const char *search) {
  int base_len = strlen(base);
  int search_len = strlen(search);
  auto offset = 0;
  for (auto i = 0; i < base_len; i++) {
    if (base_len - i < search_len) {
      break;
    }
    if (is_strict_prefix(base + i, base_len - i, search, search_len, offset)) {
      return i;
    }
  }
  return -1;
}

// Knuth Morrison Pratt (KMP)
//
// Through out example the interesting case in question to be handled is
// Needle:   almost_almost_done
// Hay:      almost_almost_almost_done

//  Derivation of the interesting case
//  << Derive a needle string in way where the suffix is
//     the prefix   almost_[almost_]done
//     Place the needle at the end of the hay. almost_[almost_almost_done]
//
//  Another example
//  Needle: sunny[sunny]day
//  Hay: sunnysunny[sunnysunnyday]

//
//
//
// We want to skip searching parts of the hay string that we have alread
// searched.  Problem is that if we skip naively we will miss parts of
// the hay-string that matched as a suffix of the last failure.
//
//  Example of Missing Suffix for the last search problem:
//  hay:      almost_almost_almost_a_match
//  needle:   almost_almost_a_match

//  hay:      almost_almost_almost_a_match
//  needle:   almost_almost_a_match
//                           ^ Our search would fail here
//  If we skip naively, the next search gets setup incorrectly
//  BUG!
//  hay:      almost_almost_almost_a_match
//  needle:                  almost_almost_a_match
//                           ^ We will not find a match because we skipped the
//                           second occurrence of almost.
//
//
//  Back Track/SuffixIsPrefix() calculation
//   -------------------------------------
//  Fix is to find the right amount of back track for search such that
//  we don't mist the second " almost" in the hay.
//
//  hay:      almost_almost_almost_a_match
//  needle:      <<< almost_almost_a_match
//               <<< Back track

//  How to calculate the backtrack for a given needle:
//  The problem conceptually:
//      One one can make all suffixes and see if the are prefixes and choose
//      them max.
//
//      Example:
//
//      needle:
//    almost_almost_a_ ;; we use this string as a example as
//                             ? ;; we failed at this state in the example we
//                               ;; are dealing with
//
//      All suffixes are         (Is suffix also prefix of needle)       Length
///     of needle are            needle: almost_almost_a_
//      //////////////////////////////////////////////////////////////////////
//                    _          No
//                   a_          No
//                  _a_          No
//                 t_a_          No
//                st_a_          No
//               ost_a_          No
//              most_a_          No
//             lmost_a_          No
//            almost_a_          YES (this suffix is also prefix)       9
//           _almost_a_          No
//          t_almost_a_          No
//         st_almost_a_          No
//        ost_almost_a_          No
//      lmost_almost_a_          No
//     almost_almost_a_          No
//
//  We need to thus calculate the suffix lengths of the needle (as in the
//  matching attempt we can fails at any offset)
//
//  Example
//  Input Needle:
//  Output PrefixEqualsSuffixLen(needle)->Vector 00000000123456789
//   almost_almost_alm  o  s  t  _  a  _  m  a  t  c  h
//   01000000123456789 10 11
//
//                         ^ Means that last 11 letter of the prior string
//                           "almost_almo"are also a prefix.
//
//  Dynamic Programming for calculating the SuffixIsPrefix(Needle)
//  vector.  We start of by observing that if we have a solution to a
//  Needle instance 'X' , we can derive the vector for Needle instance 'X+n'
//  where n is
//
//  arbitrary character.  i.e
//                           Given the solution  almal      -> 01012
//                           We can calculate    almal+m    -> 010120
//                           and calculate       almal+m+l  -> 0101203
//                           ...
//
//                           almalm
//                          0000123
// Given this bottom up formulation we can calculate increasing longer
// solution vectors.  With the base case  'x''y' ....
//                                         0  0
// Where 'x'  and 'y' are any arbitrary first two characters of *ANY*
// needle.
//
//
//
// Implementation:
//       There are two ways to formulate the problem.
//       1>
//       suffix_table[i] encodes the max suffix  of the needle prior to i.
//
//        allallp
//        00001   << 'a' is matching at the index of l.
//       2>
//       suffix_table[i] encode the max suffix including this i offset
vector<int> buildSuffixTable(const char *needle, int size) {
  vector<int> suffix_table;
  suffix_table.resize(size + 1);
  suffix_table[0] = 0;
  suffix_table[1] = 0;

  //   allallPa
  //   010012301
  for (auto i = 2; i <= size; i++) {
    // we didn't start finding a prefix yet
    if (suffix_table[i - 1] == 0) {
      suffix_table[i] = (needle[i - 1] == needle[0]) ? 1 : 0;
      continue;
    }
    if (needle[i - 1] == needle[suffix_table[i - 1]]) {
      suffix_table[i] = suffix_table[i - 1] + 1;
    } else {
      suffix_table[i] = 0;
    }
  }
  return suffix_table;
}

vector<int> buildSuffixTable_strategy2(const char *needle, int size) {
  // allallp
  // 0001230
  vector<int> suffix_table;
  suffix_table.resize(size);
  suffix_table[0] = 0;
  for (auto i = 1; i < size; i++) {
    if (suffix_table[i - 1] == 0) {
      suffix_table[i] = needle[i] == needle[0] ? 1 : 0;
      continue;
    }

    if (needle[i] == needle[suffix_table[i - 1]]) {
      suffix_table[i] = suffix_table[i - 1] + 1;
    } else {
      suffix_table[i] = 0;
    }
  }
  return suffix_table;
}

int substr_kmp(const char *hay, const char *needle) {
  auto suffix_table = buildSuffixTable(needle, strlen(needle));
  auto hay_len = strlen(hay);
  auto needle_len = strlen(needle);
  int failed_offset = 0;
  auto i = 0;
  while (i < hay_len) {
    if (hay_len - i < needle_len) {
      break;
    }
    // all_all_all_done
    // all_all_done
    //         4
    //     all_all_done
    //         4

    //
    if (is_strict_prefix(hay + i, hay_len - i, needle, needle_len,
                         failed_offset)) {
      return i;
    }

    if (failed_offset == 0) {
      i++;
      continue;
    }
    i = i + failed_offset;
    // Safe back track
    i = i - suffix_table[failed_offset];
  }
  return -1;
}

// Rabin Karp

//////////////
// // Tests //
//////////////
int test_naive(const char *hay, const char *needle, int expectation) {
  int offset = substr_naive(hay, needle);
  cout << endl
       << hay << " " << needle << " "
       << ((offset == -1) ? "NotFound" : &hay[offset]);
  if (offset != expectation) {
    cout << " " << offset << " != " << expectation << endl;
    assert(false);
  }
}

int test_kmp(const char *hay, const char *needle, int expectation) {
  int offset = substr_kmp(hay, needle);
  cout << endl
       << hay << " " << needle << " "
       << ((offset == -1) ? "NotFound" : &hay[offset]);
  if (offset != expectation) {
    cout << " " << offset << " != " << expectation << endl;
    assert(false);
  }
}

int test_suffix(const char *needle, vector<int> expected) {
  auto ret = buildSuffixTable(needle, strlen(needle));
  assert(std::equal(expected.begin(), expected.end(), ret.begin()));
}

int test_suffix_strategy2(const char *needle, vector<int> expected) {
  auto ret = buildSuffixTable_strategy2(needle, strlen(needle));
  assert(std::equal(expected.begin(), expected.end(), ret.begin()));
}

// FARAFARAZsdfsdfsadfs
// FARAZ
int main(int argc, char *argv[]) {
  test_naive("FARAFARAZsdfsdfsadfs", "FARAZ", 4);
  test_naive("FARAKFARAZsdfsdfsadfs", "FARAZ", 5);
  test_naive("FARAKFARAZsdfsdfsadfs", "FARAZUK", -1);
  // overlapping cases
  test_naive("all_all_all_done", "all_done", 8);
  test_kmp("banabanbanabananabanana", "banana", 11);

  // KMP suffix building test
  test_suffix("almalm", {0, 0, 0, 0, 1, 2, 3});
  test_suffix("banbananaaban", {0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 1, 2, 3});

  test_suffix_strategy2("almalm", {0, 0, 0, 1, 2, 3});
  test_suffix_strategy2("banbananaaban",
                        {0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 1, 2, 3});

  // KMP test
  test_kmp("FARAFARAZsdfsdfsadfs", "FARAZ", 4);
  test_kmp("FARAKFARAZsdfsdfsadfs", "FARAZ", 5);
  test_kmp("FARAKFARAZsdfsdfsadfs", "FARAZUK", -1);
  // overlapping cases
  test_kmp("all_all_all_done", "all_done", 8);
  test_kmp("banabanbanabananabanana", "banana", 11);
  test_kmp("sunsunnysunnysunsunnysunnysunnyday", "sunnysunnyday", 21);
  return 0;
}
