#!/bin/bash
killall emacs
emacs25 -q -l /usr/share/xsessions/emacswm.el -rv --daemon
emacsclient.emacs25 -a '' -c
