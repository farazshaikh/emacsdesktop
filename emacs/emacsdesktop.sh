#!/bin/bash
killall emacs25
emacs25 -rv --daemon -f exwm-enable
emacsclient.emacs25 -a '' -c
