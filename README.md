Desktop Setup for Emacs Window Manager
====
wget -qO- https://raw.githubusercontent.com/farazshaikh/Misc/master/emacs/installexwm.sh | sudo bash -x
Logout and login forever into emacs :)

Misc
====

Miscellaneous Code Written Over time

Splitter: splitter/README.md
========
   Multi-thread file splitting and reconcile tool. Similar to UNIX split, but configurable in terms of
   1. number of threads
   2. IO size used by thread for doing the split
   3. Reconcile is restart-able i.e the splitted shards can arrive at destination in any order/time,
      reconciliation can continue as and when shards arrive.
