#!/bin/bash

linkup() {
    local ts=${1}
    local src=${2}
    local dst=${3}
    [ -d `dirname ${src}` ] || mkdir `dirname ${src}`
    if ! [ -e "${src}" ]; then
        ## source doesn't exists
        echo -n "Link "
        ln -snf ${dst} ${src}
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
    sudo apt-get install wget -y
    ## ppa's
    echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' | sudo tee -a /etc/apt/sources.list.d/google-chrome.list > /dev/null
    wget https://dl.google.com/linux/linux_signing_key.pub
    sudo apt-key add linux_signing_key.pub
    sudo add-apt-repository ppa:kelleyk/emacs
    sudo apt-get update

    sudo apt install emacs26 -y
    sudo apt-get install emacs25 -y

    sudo apt-get install git -y
    sudo apt-get install curl -y
    sudo apt-get install openssh-server -y

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
    sudo apt-get install tmux -y
    sudo apt-get install gnome-flashback -y
    sudo apt-get install fonts-noto -y
    sudo apt-get install gnome-screensaver -y
    sudo apt-get install fonts-powerline -y
}

checkoutCode() {
    pushd `pwd`
    local installLoc=${1}
    local repo_url=${2}
    local dirname=`basename $repo_url`
    local extension="${dirname##*.}"
    local dirname="${dirname%.*}"

    echo "Install/fetch ${repo_url} into folder ${installLoc}/${dirname}"
    if [ -d ${installLoc}/${dirname} ]; then
        cd ${installLoc}/${dirname}
        git fetch
        git rebase
        git config core.fileMode false
        cd ..
    else
        mkdir -p ${installLoc}
        cd ${installLoc}
        git clone ${repo_url} ${dirname}
     fi
    popd
}


linkupFiles() {
    pushd `pwd`
    local installLoc=${1}
    local ts=${2}

    cd ${installLoc}/Misc

    linkup ${ts} ~/.bashrc `pwd`/.bashrc
    linkup ${ts} ~/.gdbinit `pwd`/.gdbinit
    linkup ${ts} ~/.XtermModifiedITERM.json `pwd`/.XtermModifiedITERM.json
    linkup ${ts} ~/.screenrc `pwd`/.screenrc
    linkup ${ts} ~/.tmux.conf `pwd`/.tmux.conf
    linkup ${ts} ~/.i3/config `pwd`/.i3/config
    linkup ${ts} ~/.i3/.inputrc `pwd`/.inputrc

    mkdir -p  ~/.config/powerline-shell
    linkup ${ts} ~/.config/powerline-shell/config.json `pwd`/emacs/config_powerline-shell_config.json

    linkup ${ts} ~/.gitconfig `pwd`/emacs/.gitconfig
    linkup ${ts} ~/.gitignore `pwd`/emacs/.gitignore
    linkup ${ts} ~/.emacs `pwd`/emacs/.emacs
    linkup ${ts} ~/acme.png `pwd`/emacs/acme.png
    linkup ${ts} ~/.xinitrc `pwd`/emacs/.xinitrc
    linkup ${ts} ~/.Xresources `pwd`/emacs/.Xresources
    linkup ${ts} ~/ediff.sh `pwd`/emacs/ediff.sh
    linkup ${ts} ~/wallpaper.jpg `pwd`/emacs/wallpaper.jpg

    linkup ${ts} /etc/X11/Xsession.d/10-retina-display `pwd`/emacs/etc_X11_Xsession.d_10-retina-display
    linkup ${ts} /etc/X11/Xresources/retina-display `pwd`/emacs/etc_X11_Xresources_retina-display
    linkup ${ts} /etc/apt/apt.conf.d/90aptforceyes `pwd`/emacs/90aptforceyes
    touch ~/.gitmessage

    ## Optional integrate with DM
    linkup ${ts} /usr/share/xsessions/emacsdesktop.sh `pwd`/emacs/emacsdesktop.sh
    linkup ${ts} /usr/share/xsessions/emacs.desktop `pwd`/emacs/usr_share_xsessions_emacs.desktop
    popd
}

disable_greeter() {
    sudo systemctl set-default multi-user.target --force
    sudo systemctl set-default multi-user.target
    sudo systemctl set-default graphical.target
}

main() {
    installTime=`date | sed -e "s/ /_/g"`
    installLoc=${HOME}/.eos
    gitRepoInstallLoc=${installLoc}/third_party_git_repos

    if [ "$EUID" -ne 0 ]
    then echo "During the installation you might be prompted for credentials required to install packages"
#         exit
    fi

    # Get an install ID for this install
    echo InstallID ${installTime}

    # Checkout code that comes out of git-repos
    # /Misc
    checkoutCode ${installLoc} https://github.com/farazshaikh/Misc

    # /site_eos
    checkoutCode ${gitRepoInstallLoc} http://github.com/farazshaikh/fzf.git
    ${gitRepoInstallLoc}/fzf/install

    checkoutCode ${gitRepoInstallLoc}  https://github.com/farazshaikh/powerline-shell



    linkupFiles ${installLoc} ${installTime}
    packageInstall

    disable_greeter
}

set -e
main
