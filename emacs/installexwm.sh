#!/bin/bash

linkup() {
    local ts=${1}
    local src=${2}
    local dst=${3}
    [ -d `dirname ${src}` ] || mkdir `dirname ${src}`
    if ! [ -e "${src}" ]; then
        ## source doesn't exists
        echo -n "Link "
        ln -s ${dst} ${src}
    elif ! [ "${src}" -ef "${dst}" ]; then
        ## source exists and differs from destinaton
        mv ${src} ${src}.${ts}.exwm.bkup
        echo -n "Backup and Link "
        ln -s ${dst} ${src}
    else
        ## source exists and is same as destination
        echo -n "Skipping Link "
    fi
    echo ${src} "->" `readlink -f ${src}`
}


###################
#EXWM Installation#
###################
packageInstall() {
    sudo apt-get install emacs25 -y
    sudo apt-get install emacs26 -y
    sudo apt-get install suckless-tools -y
    sudo apt-get install git -y
    sudo apt-get install chromium-browser -y
    set +e
    sudo apt-get install chromium-ublock-origin -y
    set -e
    sudo apt-get install screen -y
    sudo apt-get install xsel -y
    sudo apt-get install vlc -y
    sudo apt-get install feh -y
    sudo apt-get install xterm -y
    sudo apt-get install vim -y
    sudo apt-get install mame -y
    sudo apt-get install blueman -y
    sudo apt-get install cheese -y
    sudo apt-get install redshift-gtk -y
}

checkoutCode() {
    pushd `pwd`
    local installLoc=${1}
    if [ -d ${installLoc} ]; then
        cd ${installLoc}/Misc
        git fetch
        git rebase
        cd ..
    else
        mkdir -p ${installLoc}
        cd ${installLoc}
        git clone http://github.com/farazshaikh/Misc
    fi
    chmod -R 0777 ./Misc
    cd ./Misc
    git config core.fileMode false
    popd
}


linkupFiles() {
    pushd `pwd`
    local installLoc=${1}
    local ts=${2}

    cd ${installLoc}/Misc

    linkup ${ts} ~/.bashrc `pwd`/.bashrc
    linkup ${ts} ~/.XtermModifiedITERM.json `pwd`/.XtermModifiedITERM.json
    linkup ${ts} ~/.screenrc `pwd`/.screenrc
    linkup ${ts} ~/.i3/config `pwd`/.i3/config
    linkup ${ts} ~/.i3/.inputrc `pwd`/.inputrc


    linkup ${ts} ~/.gitconfig `pwd`/emacs/.gitconfig
    linkup ${ts} ~/.emacs `pwd`/emacs/.emacs
    linkup ${ts} ~/.xinitrc `pwd`/emacs/.xinitrc
    linkup ${ts} ~/.Xresources `pwd`/emacs/.Xresources
    linkup ${ts} ~/ediff.sh `pwd`/emacs/ediff.sh
    linkup ${ts} ~/wallpaper.jpg `pwd`/emacs/wallpaper.jpg

    linkup ${ts} /etc/X11/Xsession.d/10-retina-display `pwd`/emacs/etc_X11_Xsession.d_10-retina-display
    linkup ${ts} /etc/X11/Xresources/retina-display `pwd`/emacs/etc_X11_Xresources_retina-display
    linkup ${ts} /etc/apt/apt.conf.d/90aptforceyes `pwd`/emacs/90aptforceyes

    ## Optional integrate with DM
    linkup ${ts} /usr/share/xsessions/emacsdesktop.sh `pwd`/emacs/emacsdesktop.sh
    linkup ${ts} /usr/share/xsessions/emacs.desktop `pwd`/emacs/usr_share_xsessions_emacs.desktop
    popd
}


main() {
    installTime=`date | sed -e "s/ /_/g"`
    installLoc=/usr/share/faraz

    if [ "$EUID" -ne 0 ]
    then echo "Please run as root or sudo"
         exit
    fi

    echo InstallID ${installTime}
    packageInstall
    checkoutCode ${installLoc}
    linkupFiles ${installLoc} ${installTime}
}

set -e
main
