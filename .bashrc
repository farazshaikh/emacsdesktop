alias ssho='ssh -o "StrictHostKeyChecking no"'
alias ff="find . -name"
alias ffg="find . -type f | xargs grep -nH "
alias pssh='parallel-ssh -i -t0 -h ~/.vmcluster $@'
alias ee='emacsclient -n '
alias magit='emacsclient -n --eval "(magit)"'
alias toff="tmux set-window-option synchronize-panes off"
alias ton="tmux set-window-option synchronize-panes on"
export EDITOR='emacsclient -n'
export HISTCONTROL=ignoredups
export EOS=~/.eos


# Fallback command prompt if powerline-shell isn't available
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWSTASHSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWUPSTREAM=true
export PS1='[\033[1m\033[34m \u@\h:`pwd` \[\033[00m\]][$(__git_ps1)]\n'
__git_ps1 ()
{
    local b="$(git symbolic-ref HEAD 2>/dev/null)";
    if [ -n "$b" ]; then
        printf " (%s)" "${b##refs/heads/}";
    fi
}

# powerline-shell integration
# https://github.com/b-ryan/powerline-shell
function _update_ps1() {
    PS1=$(powerline-shell $?)
}

if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
fi

untarall ()
{
    for filegz in `ls *.tar.gz`
    do
    echo Untaring $filegz
    mkdir `echo $filegz | cut -d "." -f1` > /dev/null
    tar -zxvf $filegz -C `echo $filegz | cut -d "." -f1` > /dev/null
    done
}

cdbug () {
      bugNum=$1
      baseBugDir="/qa/bugs"
      tmpBugDir="/tmp/bugs"
      bugNumDir=""
      zeroDir=""
      targDir=""

      numDigit=${#bugNum}
      for i in $(seq 0 1 $numDigit)
      do
        bugNumDir=$bugNumDir"/"${bugNum:$i:1}
      done

      echo $bugNumdir

      for i in $(seq 0 1 10)
      do
        targDir=$baseBugDir$zeroDir$bugNumDir
        echo $targDir
        if [ -d $targDir ]; then
           mkdir -p $tmpBugDir/$bugNum
           echo "Copy bugs from $targDir to $tmpBugDir/$bugNum"
           cp ${targDir}* $tmpBugDir/$bugNum/
           cd $tmpBugDir/$bugNum
           untarall
           break
        fi
        zeroDir=$zeroDir"/0"
      done
}

prune() {
        if [ $# -eq 0 ]
          then
           echo "No arguments supplied, Example prune start end file"
          else
           sed -n '/$1/,/$2/p' $3
        fi
}

runtillfail () {
     command=$1
     while $command; do :; done
}

export PYTHONSTARTUP=~/.pythonrc
# Source rust and rust/cargo/nix
[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && source $HOME/.nix-profile/etc/profile.d/nix.sh
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env
[ -f /usr/share/bash-completion/completions/git ] && source /usr/share/bash-completion/completions/git

# dynamic title for screen
case "$TERM" in
    screen*)
         export PS1=$PS1'\[\033k\033\\\]\$ '
         ;;
    *)
         export PS1=$PS1'\$ '
esac

[ -f ~/.fzf.bash ] && source ~/.fzf.bash



# sync bash commands to file always
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
export WRK="/home/faraz/dfn/dfinity/./"

