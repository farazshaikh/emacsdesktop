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
    sudo apt-get update
    sudo apt-get install wget -y

    sudo apt install nvim vim -y
    sudo apt-get install chromium-browser -y
    sudo apt-get install git curl openssh-server -y
    sudo apt-get install suckless-tools -y
    sudo apt-get install xsel xterm -y
    sudo apt-get install vlc feh -y
    sudo apt-get install tmux -y
    sudo apt-get install blueman cheese -y
    sudo apt-get install fonts-noto fonts-powerline -y
    sudo apt-get install fzf -y

    sudo apt-get install pip -y
    sudo apt-get install python3-pip -y

    sudo snap install ripgrep --classic
    sudo snap install emacs --classic
    sudo snap install retroarch --classic
    sudo snap install ghostty --classic
    sudo snap install alacritty --classic

    curl -sS https://starship.rs/install.sh | sh
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
    linkup ${ts} ~/i3/config `pwd`/i3/config
    linkup ${ts} ~/i3/i3-wm-scripts ${gitRepoInstallLoc}/i3-wm-scripts
    linkup ${ts} ~/i3/.inputrc `pwd`/.inputrc
    linkup ${ts} ~/.config/rofi/config  `pwd`/.config/rofi/config
    linkup ${ts} ~/.config/ghostty/config  `pwd`/.config/ghostty/config

    mkdir -p  ~/.config/powerline-shell
    linkup ${ts} ~/.config/powerline-shell/config.json `pwd`/emacs/config_powerline-shell_config.json

    linkup ${ts} ~/.gitconfig `pwd`/emacs/.gitconfig
    linkup ${ts} ~/.gitignore `pwd`/.gitignore
    linkup ${ts} ~/.emacs `pwd`/emacs/.emacs
    linkup ${ts} ~/.emacs_custom.el `pwd`/emacs/.emacs_custom.el
    linkup ${ts} ~/acme.png `pwd`/emacs/acme.png
    linkup ${ts} ~/.xinitrc `pwd`/emacs/.xinitrc
    linkup ${ts} ~/.Xresources `pwd`/emacs/.Xresources
    linkup ${ts} ~/ediff.sh `pwd`/emacs/ediff.sh
    linkup ${ts} ~/wallpaper.jpg `pwd`/wallpapers/wallpaper.jpg
    linkup ${ts} ~/24bit.sh `pwd`/24bit.sh

    linkup ${ts} /etc/X11/Xsession.d/10-retina-display `pwd`/emacs/etc_X11_Xsession.d_10-retina-display +
    linkup ${ts} /etc/X11/Xresources/retina-display `pwd`/emacs/etc_X11_Xresources_retina-display +
    #linkup ${ts} /etc/apt/apt.conf.d/90aptforceyes `pwd`/emacs/90aptforceyes +
    touch ~/.gitmessage

    ## Optional integrate with DM(obsolete)
    linkup ${ts} /usr/share/xsessions/emacsdesktop.sh `pwd`/emacs/emacsdesktop.sh +
    linkup ${ts} /usr/share/xsessions/emacs.desktop `pwd`/emacs/usr_share_xsessions_emacs.desktop +

    ## gdm3login/
    linkup ${ts} /usr/share/xsessions/emacs.desktop `pwd`/emacs/gdm3login/usr_share_xsessions_emacs.desktop +
    linkup ${ts} /usr/bin/emacs-gnome-flashback-session `pwd`/emacs/gdm3login/usr_bin_emacs-gnome-flashback-session +
    linkup ${ts} /usr/share/gnome-session/sessions/emacs-gnome-flashback.session `pwd`/emacs/gdm3login/usr_share_gnome-session_sessions_emacs-gnome-flashback.session +
    linkup ${ts} /usr/bin/emacs-gnome-flashback  `pwd`/emacs/gdm3login/usr_bin_emacs-gnome-flashback +
    popd
}

linkOnlyMode() {
    installTime=`date | sed -e "s/ /_/g"`
    installLoc=${HOME}/.eos
    gitRepoInstallLoc=${installLoc}/third_party_git_repos

    # Check if repo exists
    if [ ! -d "${installLoc}/emacsdesktop" ]; then
        echo "Error: ${installLoc}/emacsdesktop does not exist. Run full install first."
        exit 1
    fi

    echo "Link-only mode: InstallID ${installTime}"
    
    # Only run the linkup step
    linkupFiles ${installLoc} ${gitRepoInstallLoc} ${installTime}
}

main() {
    # Check for --link-only option
    if [ "$1" = "--link-only" ]; then
        linkOnlyMode
        return
    fi

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
    checkoutCode ${gitRepoInstallLoc}  git@github.com:farazshaikh/Gogh.git

    # 3. Linkup the files from the git checkout
    linkupFiles ${installLoc} ${gitRepoInstallLoc} ${installTime}
}

set -e
main "$@"
