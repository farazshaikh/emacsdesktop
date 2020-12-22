alias ssho='ssh -o "StrictHostKeyChecking no"'
alias ff="find . -name"
alias ffg="find . -type f | xargs grep -nH "
alias pssh='parallel-ssh -i -t0 -h ~/.vmcluster $@'
alias ec='emacsclient -n '
alias magit='emacsclient -n --eval "(magit)"'
alias toff="tmux set-window-option synchronize-panes off"
alias ton="tmux set-window-option synchronize-panes on"
alias tmuxa="tmux attach"
export EDITOR=ec
export HISTCONTROL=ignoredups
export EOS=~/.eos
alias scr="screen -DAR -h 10000"

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

g() {
    git log --max-count=5 --format=oneline --abbrev-commit; git status ; git rev-parse --abbrev-ref HEAD
}


rustproj() {
	mkdir ~/rustplay
	cd ~/rustplay
	cargo init $1
	cd $1
	$EDITOR ./src/main.rs
}

runtillfail () {
     command=$1
     while $command; do :; done
}


function git-grepblame {
  local script
  script="$(cat <<'EOF'
  my $input = do { local $/=undef; <> };
  while ($input =~ m!\A(([^\0]+)\0([1-9][0-9]*)\0([^\n]+)\n)!xmsg) {
    my ($orig, $filename, $lineno, $line) = ($1, $2, $3, $4);
    $input = substr $input, length $orig;
    # Escape each single quote "'" in a singly quoted string as: "'\''"
    $filename =~ s!'!'\\''!gxms;
    # Run "git blame" on the file/line, and show the output.
    print qx<
      git blame --show-name --show-number -L $lineno,$lineno -- '$filename'
    >;
  }
EOF
  )"
  git grep  --null --line-number "$@" | perl -e "$script"
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
export WRK="$HOME/dfn/dfinity/rs/"

eval "$(direnv hook bash)"
