#!/bin/bash

EMACS=emacs
EMACSCLIENT=emacsclient

detectEmacs() {
    if hash emacs26 2>/dev/null; then
	EMACS=emacs26
    elif hash emacs25 2>/dev/null; then
	EMACS=emacs25
    else
	EMACS=emacs
    fi
}


detectEmacsClient() {
    if hash emacsclient26 2>/dev/null; then
	EMACSCLIENT=emacsclient26
    elif hash emacsclient25 2>/dev/null; then
	EMACSCLIENT=emacsclient25
    elif hash emacsclient.emacs25 2> /dev/null; then

	EMACSCLIENT=emacsclient.emacs25
    else
	EMACSCLIENT=emacsclient
    fi
}

detectEmacsClient
detectEmacs

echo "Using Emacs: $EMACS"
echo "Using Emacsclient: $EMACSCLIENT"
# this is  EOS desktop session
export EOS_DESKTOP=true

# Emacs should internally setup GNOME shell
#export EOS_EMACS_GNOME_SHELL_SETUP=true

#invocation #1
#export EOS_EMACS_GNOME_SETUP=true
#killall ${EMACS}
#${EMACS}

#invocation #2
#${EMACS} -rv --daemon -f exwm-enable
#${EMACSCLIENT} -a '' -c



# Register with gnome-session so that it does not kill the whole session thinking it is dead.
test -n "$DESKTOP_AUTOSTART_ID" && {
    dbus-send --print-reply --session --dest=org.gnome.SessionManager "/org/gnome/SessionManager" org.gnome.SessionManager.RegisterClient "string:i3-exwm" "string:$DESKTOP_AUTOSTART_ID"
}

procs=("/usr/bin/gnome-flashback" "/usr/lib/gnome-settings-daemon/gsd-xsettings" "/usr/bin/gnome-keyring-daemon -f")
procslength=${#procs[@]}

for (( i=0; i<${procslength}; i++ )); do
    ${procs[$i]} &
    pids[$i]=$!
done

sleep 3

export `/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh`

killall ${EMACS}
${EMACS}


for pid in ${pids[*]}; do
    kill $pid
done

test -n "$DESKTOP_AUTOSTART_ID" && {
dbus-send --print-reply --session --dest=org.gnome.SessionManager "/org/gnome/SessionManager" org.gnome.SessionManager.Logout "uint32:1"
}
