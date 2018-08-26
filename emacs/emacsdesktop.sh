#!/bin/bash
killall emacs
emacs -rv --daemon
emacsclient -a '' -c
