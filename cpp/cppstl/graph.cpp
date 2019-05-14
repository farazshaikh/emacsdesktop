#include "graph.h"

int main() {
   Graph g(8);
   g.insertEdge(0,1);
   g.insertEdge(0,4);
   g.insertEdge(0,5);
   g.insertEdge(1,2);
   g.insertEdge(1,6);
   g.insertEdge(2,3);
   g.insertEdge(3,7);
   g.insertEdge(4,5);
   g.insertEdge(5,6);
   g.insertEdge(6,7);

   cout << "Print Edges" << endl;
   g.printEdges();

   cout << "BFS" << endl;
   g.bfs();

   cout << "DFS" << endl;
   g.dfs(0);

   // Add a disjoint graph
   Graph h = g;
   g.expandNodes(11);
   g.insertEdge(9, 7);
   g.insertEdge(8, 9);
   g.insertEdge(9, 10);
   cout << endl << "Print Edges";
   g.printEdges();

   cout << endl << "DFS";
   g.dfs(0);

   cout << endl << "DFS Exhaustive";
   g.dfs_exhaust();
}



/*

0 - 1 - 2 - 3        8
| \   \     |        |
4 - 5 - 6 - 7  <-    9 - 10

*/
