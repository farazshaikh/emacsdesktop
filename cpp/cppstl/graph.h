#include <vector>
#include <iostream>
#include <set>
#include <cassert>
#include <algorithm>

using namespace std;
class Node {
 public:
   int num;
   std::vector< int> adjacents;
};

class Graph {
   std::vector<Node> adjacencyList;
   int numNodes;
 public:
 Graph(int numNodes): adjacencyList(numNodes), numNodes(numNodes) {
      int num = 0;
      for (auto &x: adjacencyList) {
         x.num = num;
         num++;
      }
   };

   int insertEdge(int start, int end) {
      if (start >= numNodes || end >= numNodes) {
         assert(false);
         return -1;
      }
      adjacencyList[start].num = start;
      adjacencyList[start].adjacents.push_back(end);
      return 0;
   }


   void printEdges() {
      for (auto &x: adjacencyList) {
         cout << x.num << "->";
         for (auto y: x.adjacents) {
            cout << y << "  ";
         }
         cout << endl;
      }
   }

   int expandNodes(int numNodes) {
      if (numNodes <= this->numNodes) {
         assert(false);
         return -1;
      }
      this->adjacencyList.resize(numNodes);
      for (auto x=this->numNodes; x < numNodes; x++) {
         this->adjacencyList[x].num = x;
      }
      this->numNodes = numNodes;
      return 0;
   }

   void bfs() {
      /*
        frontier = root;
        get all nodes from frontier into next;
        while (frontier)
        for x
           (for x adjacents ) { !visited add to next}
           print frontier if not visited
        next = frontier
       */
      set<int> frontier;
      set<int> next;
      set<int> visited;
      frontier.insert(adjacencyList[0].num);

      while (!frontier.empty()) {
         for (auto &x: frontier) {
            if (visited.find(x) == visited.end()) {
               cout << x;
               visited.insert(x);
            }

            for (auto &y: adjacencyList[x].adjacents) {
               if (visited.find(y) == visited.end()) {
                  next.insert(y);
               }
            }
         }

         frontier = next;
         next.clear();
         cout << endl;
      }
   }



   typedef enum EDGE_TYPE {
      EDGE_TREE,
      EDGE_FORWARD,
      EDGE_BACKWARD,
      EDGE_ACROSS,
   } EDGE_TYPE;

   typedef struct Edge {
      int start;
      int end;
      EDGE_TYPE type;
   } Edge;
   typedef vector<Edge> EdgeSummary;


   void dfs_recurse(int start,
                    vector<bool> &visited,
                    EdgeSummary &edgeSummary) {
         Node& startNode= this->adjacencyList[start];
         for (auto y: startNode.adjacents) {
            if (visited[y]) {
               edgeSummary.push_back({start,y,EDGE_BACKWARD});
               continue;
            }
            edgeSummary.push_back({start, y, EDGE_TREE});
            visited[y] = true;
            dfs_recurse(y, visited, edgeSummary);
         }
         cout << " " << start;
   }

   void dfs(int start) {
      EdgeSummary edgeSummary;
      vector<bool> visited(this->numNodes, false);
      dfs_recurse(start, visited, edgeSummary);
   }


   void dfs_exhaust() {
      vector<bool> visited(this->numNodes, false);
      EdgeSummary edgeSummary;
      for (auto &x: adjacencyList)
         if(!visited[x.num])
            dfs_recurse(x.num, visited, edgeSummary);
      /* Tree Edges */
      for_each (edgeSummary.begin(), edgeSummary.end(),
                [](const  Edge &edge) {
                   if (edge.type == EDGE_TREE) {
                      cout << endl << "TREE_EDGE " << edge.start << " " << edge.end;
                   }
                   if (edge.type == EDGE_BACKWARD) {
                      cout << endl << "TREE_BACK " << edge.start << " " << edge.end;
                   }
                });
   }

 };
