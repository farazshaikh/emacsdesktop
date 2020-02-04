# You may need to comment out the next line to disable access control.
#xhost +SI:localuser:$USER

# Set themes, etc.
#gnome-settings-daemon &
xrdb -merge ~/.Xresources


# Set fallback cursor.
xsetroot -cursor_name left_ptr

# Set keyboard repeat rate.
xset r rate 300 40

# enable keyring
#eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
#export SSH_AUTH_SOCK

# If Emacs is started in server mode, `emacsclient` is a convenient way to
# edit files in place (used by e.g. `git commit`).
export VISUAL=emacsclient
export EDITOR="$VISUAL"

#setup monitor resolution
#xrandr -s 2560x1600
# Get name of connected output
OUTPUT=$(xrandr -q | grep " connected " | awk '{ print $1 }' ) 


# Following lines are for vmware linux guests: vmware doesn't pass macbook's native resolution to guest, so we must add resolutions manually with proper timings (use gft or cvt to get timings)
xrandr --newmode 2880x1800 209.25 2880 3048 3344 3808 1800 1803 1809 1834 -hsync +vsync # 15" retina display
xrandr --addmode $OUTPUT 2880x1800

xrandr --newmode 3840x2160 712.75  3840 4160 4576 5312  2160 2163 2168 2237 -hsync +vsync

xrandr --addmode $OUTPUT 3840x2160

# Resolution and DPI
# Actually retina's DPI is about 220, but 192 DPI is more natural for linux resources, since 192 is 2x of 96 DPI which is default for linux

#15 inch
#xrandr --output $OUTPUT --mode 2880x1800 --dpi 192 --preferred

#13 inch
xrandr --output $OUTPUT --mode 2560x1600 --dpi 192 --preferred

# Finally launch emacs.
exec dbus-launch emacs
#exec dbus-launch emacsclient -c

