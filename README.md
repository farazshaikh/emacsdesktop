ACME a desktop enviroment with gnome and emacs window-manager
====

```
wget -qO- https://raw.githubusercontent.com/farazshaikh/Misc/master/emacs/installexwm.sh | bash -x
```

NOTE: Single user installation.

Does an installation for a specific USER.  For example if user SCOTT
installs the desktop, only user SCOTT can login into the ACME desktop.
Other user can login into existing DESKTOP session like ubuntu, gnome,
kde etc


Login Details:

UI Login:
GDM/LigthDM login will start showing up a session entry named
EmacsDesktop.

Login from the Terminal:
Also provided is a ~/.xinitrc, using this one can start the desktop
under the X server by invoking [#] startx from the command prompt.
