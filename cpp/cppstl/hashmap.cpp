#include <map>
#include <unordered_map>
#include <iostream>
#include <memory>
using namespace std;
int main2() {
    std::map<int, std::string> myMap;
    myMap[0] = "zero";
    myMap[1] = "one";
    myMap.insert(pair<int,string>(2, "two"));

    for (auto it = myMap.begin() ;
         it != myMap.end();
         it++) {
        cout << it->first << it->second ;
        it->second = it->second + "foo";
    }

    auto x = myMap.find(0);
    if (x != myMap.end()) {
        cout << "Faraz " << x->first;
        x->second = "Zorro";
    }

    myMap.erase(0);
    cout << "erased 0";


    for (auto it = myMap.begin(); it != myMap.end() ; it++) {
        cout << it->first << it->second << " ";
    }


    //cout << myMap.size();
    return 0;
}


class Student {
private:
   int rollNumber;
   string address;
public:
   Student(): rollNumber(0), address("Unknown") {};
   Student(const Student &x): rollNumber(x.rollNumber), address(x.address) {
   }
   Student(int x, string address): rollNumber(x), address(address) {
   }
   friend ostream& operator <<(ostream& os, const Student &x) {
      os << x.rollNumber << " " << x.address;
      return os;
   }
};

int main() {
   map<string, Student> students;
   students.insert(make_pair("Faraz", Student(1, "Sun")));
   students.insert(make_pair("Sana", Student(1, "Boss")));
   students.insert(make_pair("Faiz", Student(1, "dd")));
   for (auto x: students) {
      cout << endl << x.first << " " << x.second;
   }

   students.erase("Zarroo");
   cout << "\nPrinting";
   for (auto x: students) {
      cout << endl << x.first << " " << x.second;
   }

   students.erase("Faraz");
   cout << "\nPrinting";
   for (auto x: students) {
      cout << endl << x.first << " " << x.second;
   }

   Student x = students["Faiz"];
   cout << x;
}
