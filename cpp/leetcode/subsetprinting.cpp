#include <vector>
#include <string>
#include <iostream>
using namespace std;
static int x;
void printAllSubSets(string& setMembers,
                     vector<bool>& setMemberShip,
                     unsigned int recursionLlevel)
{
   if (recursionLlevel == setMembers.length()) {
      cout << endl << x++ << "  ";
         auto i=0U;
      for (i = 0U ; i < setMembers.length(); i++) {
         if (setMemberShip[i] == false) {
            continue;
         }
         cout << setMembers[i];
      }
      return;
   }

   setMemberShip[recursionLlevel] = false;
   printAllSubSets(setMembers, setMemberShip  ,recursionLlevel+1);
   setMemberShip[recursionLlevel] = true;
   printAllSubSets(setMembers, setMemberShip  ,recursionLlevel+1);
}


int main() {
   string x="jlkjljl/jl/jlkjlkjjk";
   vector<bool> setMemberShip(x.length(), false);
   printAllSubSets(x, setMemberShip, 0);
}
