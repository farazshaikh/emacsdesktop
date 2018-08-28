#!/bin/bash




if [ "$EUID" -ne 0 ]
  then echo "Please run as root or sudo"
  exit
fi

###################
#EXWM Installation#
###################
installTime=`date | sed -e "s/ /_/g"`
echo ${installTime}

sudo apt-get install emacs25 -y
sudo apt-get install suckless-tools -y

sudo mkdir -p /usr/share/faraz/
cd /usr/share/faraz/
git clone http://github.com/farazshaikh/Misc
sudo chmod -R 0777 ./Misc
cd ./Misc/
git config core.fileMode false


# make xsession entry
mv /usr/share/xsessions/emacs.desktop /usr/share/xsessions/emacs.desktop.backup.${installTime}
ln -s `pwd`/emacs/usr_share_xsessions_emacs.desktop /usr/share/xsessions/emacs.desktop

# make emacs wrapper for running as daemon
mv /usr/share/xsessions/emacsdesktop.sh /usr/share/xsessions/emacsdesktop.sh.backup.${installTime}
ln -s `pwd`/emacs/emacsdesktop.sh /usr/share/xsessions/emacsdesktop.sh

# copy over our emacs file
mv ~/.emacs ~/.emacs.exwm.backup.${installTime}
ln -s `pwd`/emacs/.emacs ~/.emacs


###################
#Other setup      #
###################
mv ~/.bashrc ~/.bashrc.backup.${installTime}
ln -s `pwd`/.bashrc ~/.bashrc

mv ~/.screenrc ~/.screenrc.backup.${installTime}
ln -s `pwd`/.screenrc ~/.screenrc

mkdir ~/.i3
mv ~/.i3/config ~/.i3/config.backup.${installTime}
ln -s `pwd`/.i3/config ~/.i3/config
