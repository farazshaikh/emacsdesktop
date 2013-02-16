Splitter
========
   Multi-thread file splitting and reconcile tool. Similar to UNIX split, but configurable in terms of
   1. number of threads
   2. IO size used by thread for doing the split
   3. Reconcile is restart-able i.e the splitted shards can arrive at destination and any order/time
      reconciliation can continue as and when shards arrive.