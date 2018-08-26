#!/bin/bash


if [ "$EUID" -ne 0 ]
  then echo "Please run as root or sudo"
  exit
fi

installTime=`date | sed -e "s/ /_/g"`
echo ${installTime}

sudo apt-get install emacs25 -y


mkdir -p /usr/share/faraz/
cd /usr/share/faraz/
git clone http://github.com/farazshaikh/Misc
cd ./Misc/


# make xsession entry
mv /usr/share/xsessions/emacs.desktop /usr/share/xsessions/emacs.desktop.backup.${installTime}
ln -s `pwd`/emacs/usr_share_xsessions_emacs.desktop /usr/share/xsessions/emacs.desktop

# make emacs wrapper for running as daemon
mv /usr/share/xsessions/emacsdesktop.sh /usr/share/xsessions/emacsdesktop.sh.backup.${installTime}
ln -s `pwd`/emacs/emacsdesktop.sh /usr/share/xsessions/emacsdesktop.sh

# copy over our emacs file
mv ~/.emacs ~/.emacs.exwm.backup.${installTime}
ln -s `pwd`/emacs/.emacs ~/.emacs

