# If not running interactively, don't do anything
[ -z "$PS1" ] && return
# If not running interactively, don't do anything
[[ $- != *i* ]] && return
# If not running interactively, don't do anything
case $- in
*i*) ;;
*) return ;;
esac

set -o emacs
export EDITOR=ec
export HISTCONTROL=ignoredups
export EOS=~/.eos
export WRK="$HOME/wrk/lyn"
alias ssh='ssh -o "StrictHostKeyChecking no" -A'
alias ff="find . -name"
alias gg="git grep -n"
alias ffg="find . -type f | xargs grep -nH "
alias pssh='parallel-ssh -i -t0 -h ~/.vmcluster $@'
alias ecx='emacsclient -n -c -s  ~/.emacs.d/server'
alias ec='emacsclient -t -s  ~/.emacs.d/server'
alias ef='emacsclient -n -s  ~/.emacs.d/server $(fzf)'
alias eo='emacsclient -n -s ~/.emacs.d/server'
alias magit='ec --eval "(magit)"'
alias tmuxa="tmux new -s MAIN -c $WRK || tmux attach"
alias tmuxs="tmux new-session -c $WRK -d -s "
alias tmuxfixssh='eval $(tmux showenv -s SSH_AUTH_SOCK)'
alias tmuxswap="tmux swap-window -t"
alias prodssh="source $EOS/emacsdesktop/.prodssh.rc"
alias rcd='cd $(git rev-parse --show-cdup)'
alias ls='ls --color'
alias cpue='sudo chcpu -e 1,2,3,4,6,7,8,9,10,11,12'
alias cpud='sudo chcpu -d 1,2,3,4,6,7,8,9,10,11,12'
alias ipython="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"

# Define the alias for code
alias co="code -r"
alias cw="code -w"
alias cu="cursor -r"
alias cuw="cursor -w"

# Autocompletion function for the alias
_co_autocomplete() {
  local cur
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=($(compgen -f -- "${cur}"))
  return 0
}

# Register the autocompletion function for the alias
complete -F _co_autocomplete co ec

# Define the alias tmux
alias ts="tmux split"

# Autocompletion function for the alias
_ts_autocomplete() {
  local cur
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=($(compgen -W "-h" -- "${cur}"))
  return 0
}

# Register the autocompletion function for the ts
complete -F _ts_autocomplete ts

# Tmux synchronization
alias tsync="tmux set-window-option synchronize-panes "
_tsync_autocomplete() {
  local cur
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=($(compgen -W "on off" -- "${cur}"))
  return 0
}
complete -F _tsync_autocomplete tsync

# watch file and perform action
fwatch() {
  if [ $# -lt 2 ]; then
    echo "Usage: fwatch <file> <command>"
    echo "Example: fwatch myfile.txt 'clear && cat myfile.txt'"
    return 1
  fi
  local file="$1"
  shift
  local cmd="$@"
  while inotifywait -q -e close_write "$file"; do
    eval "$cmd"
  done
}

_fwatch_autocomplete() {
  local cur
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=($(compgen -f -- "${cur}"))
  return 0
}
complete -F _fwatch_autocomplete fwatch

alias fixsound="pactl list short sinks | pactl set-default-sink alsa_output.pci-0000_06_00.1.hdmi-stereo;\
       pactl list short sources | pactl set-default-source alsa_output.usb-Blue_Microphones_Yeti_X_2046SG003K88_888-000313110306-00.iec958-stereo.monitor"
alias fixsound_z13="pactl list short sinks | pactl set-default-sink alsa_output.pci-0000_64_00.1.HiFi__hw_Generic_9__sink; \
       pactl list short sources | pactl set-default-source alsa_output.usb-Blue_Microphones_Yeti_X_2046SG003K88_888-000313110306-00.iec958-stereo.monitor"

alias cargowatchlib='cargo watch -c -x  "test --release --message-format=human -- --nocapture"'
alias cargowatch='cargo watch -c -x  "run --release --message-format=human -- --nocapture"'

untarall() {
  for filegz in $(ls *.tar.gz); do
    echo Untaring $filegz
    mkdir $(echo $filegz | cut -d "." -f1) >/dev/null
    tar -zxvf $filegz -C $(echo $filegz | cut -d "." -f1) >/dev/null
  done
}

prune() {
  if [ $# -eq 0 ]; then
    echo "No arguments supplied, Example prune start end file"
  else
    sed -n '/$1/,/$2/p' $3
  fi
}

g() {
  git log --max-count=5 --format=oneline --abbrev-commit
  git status
  git rev-parse --abbrev-ref HEAD
}

rustproj() {
  mkdir ~/rustplay
  cd ~/rustplay
  cargo init $1
  cd $1
  $EDITOR ./src/main.rs
}

rust_jupyter_repl() {
  #evcxr
  jupyter-lab --notebook-dir=~/rustplay --no-browser
}

rustrepl_install() {
  cargo install evcxr_jupyter
  evcxr_jupyter --install
}

runtillfail() {
  command=$1
  while $command; do :; done
}

function git-grepblame {
  local script
  script="$(
    cat <<'EOF'
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
  git grep --null --line-number "$@" | perl -e "$script"
}

laptop_power_debug() {
  # For new latops debugs power consuption
  sudo apt install powertop
  # install lmt tools
  sudo apt install laptop-mode-tools
  sudo laptop-detect
  status=$?
  if [ $status -ne 0 ]; then
    echo "Laptop not detected"
    return 1
  else
    sudo laptop-detect -V
    echo "Most likely running a laptop"
  fi
  sudo laptop_mode
  sudo powertop --auto-tune
  sudo powertop
}

# Disable built-in Trackpad and TrackPoint (psmouse)
disable_psmouse() {
  echo "Disabling internal mouse (Trackpad + TrackPoint)..."
  if lsmod | grep -q psmouse; then
    sudo modprobe -r psmouse && echo "psmouse module unloaded."
  else
    echo "psmouse module already unloaded."
  fi
}

# Enable built-in Trackpad and TrackPoint (psmouse)
enable_psmouse() {
  echo "Enabling internal mouse (Trackpad + TrackPoint)..."
  if ! lsmod | grep -q psmouse; then
    sudo modprobe psmouse && echo "psmouse module loaded."
  else
    echo "psmouse module already loaded."
  fi
}

[ -f ~/.python.rc ] && export PYTHONSTARTUP=~/.python.rc
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
export PATH="/home/faraz/.local/bin:$PATH"

# Source rust and rust/cargo/nix
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env
[ -f /usr/share/bash-completion/completions/git ] && source /usr/share/bash-completion/completions/git

#fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# nvim
if command -v nvim &>/dev/null; then
  alias vimdiff='nvim -d'
  alias vim='nvim'
  alias vi='nvim'
fi

# sync bash commands to file always
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
eval "$(direnv hook bash)"

# >>>> Vagrant command completion (start)
GEM_COMPLETION=/opt/vagrant/embedded/gems/2.2.16/gems/vagrant-2.2.16/contrib/bash/completion.sh
[ -s ${GEM_COMPLETION} ] && \. ${GEM_COMPLETION}
# <<<<  Vagrant command completion (end)

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"                   # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

#~/24bit.sh
if [ -f ~/24bit.sh ]; then
  echo "Color test: "
  ~/24bit.sh
fi

[ -f ~/.bash_completion/alacritty ] && source ~/.bash_completion/alacritty

#sh -c "$(curl -fsSL https://starship.rs/install.sh)"
eval "$(starship init bash)"

[ -d "/home/faraz/.foundry/" ] && export PATH="$PATH:/home/faraz/.foundry/bin"
[ -d "/home/faraz/.ghcup/env" ] && source "/home/faraz/.ghcup/env" # ghcup-env

#source credentials
source ~/.bashrc.credentials
[ -f $HOME/.deno/env ] && source "$HOME/.deno/env"
. "$HOME/.cargo/env"

# This alias runs the Cursor Setup Wizard, simplifying installation and configuration.
# For more details, visit: https://github.com/jorcelinojunior/cursor-setup-wizard
alias cursor-setup="/home/faraz/cursor-setup-wizard/cursor_setup.sh"
