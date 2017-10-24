#!/bin/bash
# parallel ssh [par-ssh]
# tmux based multi-ssh. This allows you to get a mssh session over a terminal rather than X.

# Please see: www.linuxpixies.blogspot.jp/2011/06/tmux-copy-mode-and-how-to-control.html

parallel_ssh() {
    echo faraz $1
    local SESSION=`date +"%d%b%Y%k%M%S"`i
    local hosts=( ${HOSTS:=$*} )
    echo $hosts $SESSION


    #start a new tmux session to host the mssh
    tmux new-session -d -s $SESSION
    tmux ls

  
    #create the base window as first session
    tmux new-window -t ${SESSION} "ssh ${hosts[0]}"
    unset hosts[0];

    #split remaining sessions in the same window
    for i in "${hosts[@]}"; do
        tmux split-window -t ${SESSION} -h  "ssh $i"
        tmux select-layout -t ${SESSION} tiled > /dev/null
    done
    tmux select-pane -t 0 -t ${SESSION}
    tmux set-window-option synchronize-panes on > /dev/null

    #kill the additional bash that gets spawned
    #XXX figure out how to kill a window in a named session.
    #For now the last created session is selected. 
    #This is fortunately the desired behavior
    tmux kill-window -t 0 

    #attach to created session
    tmux attach -t ${SESSION}
}


install_tmux() {
   if ! which tmux > /dev/null; then
      echo -e "tmux not found attempting install. sudo apt-get install tmux "
      sudo apt-get install -y tmux
   fi
}


HOSTS=${HOSTS:=$*}
if [ -z "$HOSTS" ]; then
   echo "Usage: Exmaple1: $0 usr1@host1 usr1@host2 ... "
   echo "       Example2: $0 usr1@host{1,2,3,4}"
else
   install_tmux
   parallel_ssh $*
fi
