#include <vector>
#include <memory>
#include <algorithm>
#include <queue>
#include <iostream>

using namespace std;
typedef struct Point {
   int x, y;
   bool start;
} Point;


class extendedPriortityQ : public priority_queue<int,std::vector<int>,std::greater<int>> {
public:
   bool remove(const int& x) {
      auto elt =  std::find(this->c.begin(), this->c.end(), x);
      if (elt != this->c.end()) {
         this->c.erase(elt);
         std::make_heap(this->c.begin(), this->c.end());
         return true;
      }
      return  false;
   }
};


std::unique_ptr<vector<Point>> GetHorizon(vector<Point> &input)
{
   unique_ptr<vector<Point>> out(new std::vector<Point>);

   extendedPriortityQ maxHeightSoFar;
   maxHeightSoFar.push(0);
   for (auto &point : input) {
      // Start
      if (point.start) {
         if (point.y >= maxHeightSoFar.top()) {
            out->push_back({maxHeightSoFar.top(), point.y, false});
            out->push_back(point);
         }
         maxHeightSoFar.push(point.y);
         continue;
      }
      if (point.y <= maxHeightSoFar.top()) {
         // silently remove
         maxHeightSoFar.remove(point.y);
      } else {
         int y;
         out->push_back(point);
         maxHeightSoFar.pop();
         y = maxHeightSoFar.top();
         out->push_back({point.x,y,false});
      }
   }

   return out;
}



int main() {
   vector<Point> input = {
      {3,3,true},
      {4,3,false},
      {3,5,true},
      {5,5,false}
   };

   struct comparator {
      bool operator()(const Point& b, const Point &a) {
           if (a.x != b.x) {
            return a.x > b.x;
         }
         if (a.y != b.y) {
            return a.y > b.y;
         }
         if (a.start != b.start) {
            return (a.start);
         }
         return false;
      }
   };

   std::sort(input.begin(), input.end(), comparator());

     auto horizon = GetHorizon(input);
     for (auto &x: *horizon) {
        cout << "X " << x.x << " Y " << x.y << std::endl; 
     }
}
