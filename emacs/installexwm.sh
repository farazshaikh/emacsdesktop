#!/bin/bash

linkup() {
    local ts=${1}
    local src=${2}
    local dst=${3}

    ln_cmd="ln"
    mv_cm="mv"
    if [ -n ${4} ]; then
        ln_cmd="sudo ln"
        mv_cmd="sudo mv"
    fi

    [ -d `dirname ${src}` ] || mkdir `dirname ${src}`
    if ! [ -e "${src}" ]; then
        ## source doesn't exists
        echo -n "Link "
        ${ln_cmd} -snf ${dst} ${src}
    elif ! [ "${src}" -ef "${dst}" ]; then
        ## source exists and differs from destinaton
        ${mv_cmd} ${src} ${src}.${ts}.exwm.bkup
        echo -n "Backup and Link "
        ${ln_cmd} -s ${dst} ${src}
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

    sudo apt-get install pasystray
    sudo apt install emacs26 emacs25 vim -y
    sudo apt-get install chromium-browser -y
    sudo apt-get install git curl openssh-server -y
    sudo apt-get install suckless-tools -y
    sudo apt-get install xsel xterm -y
    sudo apt-get install vlc feh -y
    sudo apt-get install tmux screen -y
    sudo apt-get install mame -y
    sudo apt-get install blueman cheese -y
    sudo apt-get install gnome-flashback gnome-screensaver -y
    sudo apt-get install fonts-noto fonts-powerline -y
    sudo snap install ripgrep --classic

    curl -L https://github.com/farazshaikh/top-programming-fonts/raw/master/install.sh | bash
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
    local gitRepoInstallLoc=${2}
    local ts=${3}

    cd ${installLoc}/emacsdesktop

    linkup ${ts} ~/.bashrc `pwd`/.bashrc
    linkup ${ts} ~/.gdbinit `pwd`/.gdbinit
    linkup ${ts} ~/.XtermModifiedITERM.json `pwd`/.XtermModifiedITERM.json
    linkup ${ts} ~/.screenrc `pwd`/.screenrc
    linkup ${ts} ~/.tmux.conf `pwd`/.tmux.conf
    linkup ${ts} ~/.i3/config `pwd`/.i3/config
    linkup ${ts} ~/.i3/i3-wm-scripts ${gitRepoInstallLoc}/i3-wm-scripts
    linkup ${ts} ~/.i3/.inputrc `pwd`/.inputrc
    linkup ${ts} ~/.config/rofi/config  `pwd`/.config/rofi/config

    mkdir -p  ~/.config/powerline-shell
    linkup ${ts} ~/.config/powerline-shell/config.json `pwd`/emacs/config_powerline-shell_config.json

    linkup ${ts} ~/.gitconfig `pwd`/emacs/.gitconfig
    linkup ${ts} ~/.gitignore `pwd`/.gitignore
    linkup ${ts} ~/.emacs `pwd`/emacs/.emacs
    linkup ${ts} ~/acme.png `pwd`/emacs/acme.png
    linkup ${ts} ~/.xinitrc `pwd`/emacs/.xinitrc
    linkup ${ts} ~/.Xresources `pwd`/emacs/.Xresources
    linkup ${ts} ~/ediff.sh `pwd`/emacs/ediff.sh
    linkup ${ts} ~/wallpaper.jpg `pwd`/wallpapers/wallpaper.jpg

    linkup ${ts} /etc/X11/Xsession.d/10-retina-display `pwd`/emacs/etc_X11_Xsession.d_10-retina-display +
    linkup ${ts} /etc/X11/Xresources/retina-display `pwd`/emacs/etc_X11_Xresources_retina-display +
    #linkup ${ts} /etc/apt/apt.conf.d/90aptforceyes `pwd`/emacs/90aptforceyes +
    touch ~/.gitmessage

    ## Optional integrate with DM
    linkup ${ts} /usr/share/xsessions/emacsdesktop.sh `pwd`/emacs/emacsdesktop.sh +
    linkup ${ts} /usr/share/xsessions/emacs.desktop `pwd`/emacs/usr_share_xsessions_emacs.desktop +
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

    #1. install packages first as we need git for the next step
    packageInstall

    set +e
    # 2. Checkout code that comes out of git-repos
    # /emacsdesktop
    checkoutCode ${installLoc} git@github.com:farazshaikh/emacsdesktop.git
    # /third_party_git_repos
    checkoutCode ${gitRepoInstallLoc} git@github.com:farazshaikh/fzf.git
    ${gitRepoInstallLoc}/fzf/install
    checkoutCode ${gitRepoInstallLoc}  git@github.com:farazshaikh/powerline-shell.git
    ${gitRepoInstallLoc}/powerline-shell/setup.py install
    checkoutCode ${gitRepoInstallLoc}  git@github.com:farazshaikh/Gogh.git
    checkoutCode ${gitRepoInstallLoc} git@github.com:farazshaikh/i3-wm-scripts.git
    checkoutCode ${gitRepoInstallLoc} git@github.com:farazshaikh/i3-gnome
    cd i3-gnome
    make
    sudo make install
    set -e

    # 3. Linkup the files from the git checkout
    linkupFiles ${installLoc} ${gitRepoInstallLoc} ${installTime}

    # 4. Nop
    disable_greeter
}

set -e
main
