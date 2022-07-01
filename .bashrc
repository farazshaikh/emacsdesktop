# If not running interactively, don't do anything
[ -z "$PS1" ] && return
# If not running interactively, don't do anything
[[ $- != *i* ]] && return
# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac


alias ssh='ssh -o "StrictHostKeyChecking no" -A'
alias ff="find . -name"
alias gg="git grep -n"
alias ffg="find . -type f | xargs grep -nH "
alias pssh='parallel-ssh -i -t0 -h ~/.vmcluster $@'
alias ecx='emacsclient -n -c -s  ~/.emacs.d/server'
alias ec='emacsclient -t -s  ~/.emacs.d/server'
alias ef='emacsclient -n -s  ~/.emacs.d/server $(fzf)'
alias magit='ec --eval "(magit)"'
alias toff="tmux set-window-option synchronize-panes off"
alias ton="tmux set-window-option synchronize-panes on"
alias tmuxa="tmux attach || tmux new"
alias tmuxfixssh='eval $(tmux showenv -s SSH_AUTH_SOCK)'
export EDITOR=ec
export HISTCONTROL=ignoredups
export EOS=~/.eos
alias scr="screen -DAR -h 10000"
alias prodssh="source $EOS/emacsdesktop/.prodssh.rc"
alias rcd='cd $(git rev-parse --show-cdup)'

untarall ()
{
    for filegz in `ls *.tar.gz`
    do
    echo Untaring $filegz
    mkdir `echo $filegz | cut -d "." -f1` > /dev/null
    tar -zxvf $filegz -C `echo $filegz | cut -d "." -f1` > /dev/null
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

#fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# nvim
if command -v nvim &> /dev/null
then
alias vimdiff='nvim -d'
alias vim='nvim'
alias vi='nvim'
fi



# sync bash commands to file always
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
export WRK="/home/faraz/dfn/release"
export WRK2="/wrk/sfn"
eval "$(direnv hook bash)"
. "$HOME/.cargo/env"

# >>>> Vagrant command completion (start)
GEM_COMPLETION=/opt/vagrant/embedded/gems/2.2.16/gems/vagrant-2.2.16/contrib/bash/completion.sh
[ -s ${GEM_COMPLETION} ] && \. ${GEM_COMPLETION}
# <<<<  Vagrant command completion (end)

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#~/24bit.sh
if [ -f ~/24bit.sh ]; then
    echo -n  "Color test: "
   ~/24bit.sh
fi



#sh -c "$(curl -fsSL https://starship.rs/install.sh)"
eval "$(starship init bash)"
