#!/bin/bash

cat <<END > /tmp/sshrc
#!/bin/bash

# Fix SSH auth socket location so agent forwarding works with tmux
if test "\$SSH_AUTH_SOCK" ; then
    ln -sf \$SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock
fi
END

linkupFiles() {
    pushd `pwd`
    local installLoc=${1}
    local gitRepoInstallLoc=${2}
    local ts=${3}
    local host=${4}

    cd ${installLoc}/emacsdesktop

    linkup ${ts} '~/.bashrc'  ${host}
    linkup ${ts} '~/.gdbinit'  ${host}
    linkup ${ts} '~/.XtermModifiedITERM.json'  ${host}
    linkup ${ts} '~/.screenrc'  ${host}
    linkup ${ts} '~/.tmux.conf'  ${host}
    echo "uncomment AUTH_SOCK line in remote tmux.conf"
    linkup ${ts} '~/.config/powerline-shell/config.json'  ${host}
    linkup ${ts} '~/.gitconfig'  ${host}
    linkup ${ts} '~/.gitignore'  ${host}
    linkup ${ts} '~/.emacs'  ${host}
    linkup ${ts} '~/.emacs_custom.el' ${host}
    linkup ${ts} '~/ediff.sh'  ${host}
    linkup ${ts} '~/24bit.sh'  ${host}

    # Copy remote specific files
    echo scp /tmp/sshrc ${host}:"~/.ssh/rc"
    scp /tmp/sshrc ${host}:"~/.ssh/rc"
    popd
}



# override the defination of linkup() to do a scp instead of linking
# files
linkup() {
    local ts=${1}
    local src=${2}
    local host=${3}
    ln_cmd="ln"
    mv_cmd="mv"
    scp_cmd="scp"
    eval echo ${scp_cmd} ${src} ${host}:$src
    eval ${scp_cmd} ${src} ${host}:$src
}


installLoc=${HOME}/.eos
gitRepoInstallLoc=${installLoc}/third_party_git_repos
installTime=`date | sed -e "s/ /_/g"`
scphost="farazshaikh@zh1-spm34.dc1.dfinity.network"
echo $scphost

# 3. Linkup the files from the git checkout
linkupFiles ${installLoc} ${gitRepoInstallLoc} ${installTime} ${scphost}
