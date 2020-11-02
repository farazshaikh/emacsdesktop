#!/bin/bash

EMACS=emacs
EMACSCLIENT=emacsclient

detectEmacs() {
    if hash emacs27 2>/dev/null; then
	EMACS=emacs27
    elif hash emacs26 2>/dev/null; then
	EMACS=emacs26
    elif hash emacs25 2>/dev/null; then
	EMACS=emacs25
    else
	EMACS=emacs
    fi
}


detectEmacsClient() {
    if hash emacsclient27 2>/dev/null; then
	EMACSCLIENT=emacsclient27
    elif hash emacsclient26 2>/dev/null; then
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

procs=("/usr/bin/gnome-flashback"
       "/usr/lib/gnome-settings-daemon/gsd-xsettings"
       "/usr/lib/gnome-settings-daemon/gnome-settings-daemon"
       "/usr/lib/gnome-settings-daemon/gsd-power"
       "/usr/lib/gnome-settings-daemon/gsd-print-notifications"
       "/usr/lib/gnome-settings-daemon/gsd-rfkill"
       "/usr/lib/gnome-settings-daemon/gsd-screensaver-proxy"
       "/usr/lib/gnome-settings-daemon/gsd-sharing"
       "/usr/lib/gnome-settings-daemon/gsd-smartcard"
       "/usr/lib/gnome-settings-daemon/gsd-wacom"
       "/usr/lib/gnome-settings-daemon/gsd-sound"
       "/usr/lib/gnome-settings-daemon/gsd-a11y-settings"
       "/usr/lib/gnome-settings-daemon/gsd-clipboard"
       "/usr/lib/gnome-settings-daemon/gsd-color"
       "/usr/lib/gnome-settings-daemon/gsd-datetime"
       "/usr/lib/gnome-settings-daemon/gsd-housekeeping"
       "/usr/lib/gnome-settings-daemon/gsd-keyboard"
       "/usr/lib/gnome-settings-daemon/gsd-media-keys"
       "/usr/lib/gnome-settings-daemon/gsd-mouse"
       "/usr/lib/gnome-disk-utility/gsd-disk-utility-notify"
      )
procslength=${#procs[@]}

for (( i=0; i<${procslength}; i++ )); do
    ${procs[$i]} &
    pids[$i]=$!
done

export XDG_CURRENT_DESKTOP=GNOME
export `/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh`

# Keep the Xresources setting closest to the emacs invocation. Lots
# tools (gnome settings manager and others) play with DPI setting. We
# want the dpi settings coming from the .Xresources to be final
#
# For debugging DPI issues start with
# xdpyinfo  | grep -B 2 resolution
# xrdb -q | grep dpi
# both should be around 96 for hi-res screens
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

killall ${EMACS}
#${EMACS}
dbus-launch --exit-with-session "${EMACS}" -fs

for pid in ${pids[*]}; do
    kill $pid
done

test -n "$DESKTOP_AUTOSTART_ID" && {
dbus-send --print-reply --session --dest=org.gnome.SessionManager "/org/gnome/SessionManager" org.gnome.SessionManager.Logout "uint32:1"
}
