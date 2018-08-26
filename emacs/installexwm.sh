#!/bin/bash

sudo apt-get install emacs25 -y
cd /tmp
git clone http://github.com/farazshaikh/Misc
cd ./Misc/
# make xsession entry
sudo cp ./emacs/usr_share_xsessions_emacs.desktop /usr/share/xsessions/emacs.desktop
# make emacs wrapper for running as daemon
sudo cp ./emacs/emacsdesktop.sh /usr/share/xsessions/emacsdesktop.sh
sudo cp ./emacs/.emacs /usr/share/xsessions/emacswm.el

