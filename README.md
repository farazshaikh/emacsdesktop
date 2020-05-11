Emacs Desktop Environment (EDesktop)
====

EDesktop is a emacs+gnome based desktop environment.  It uses the EXMW
window manager in conjunction with gnome desktop to provide a complete
desktop experience within Emacs.

EDesktop aims to seamlessly extend Emacs windows management to a
general desktop environment.  In EDesktop application windows are
treated a normal Emacs buffers and controlled using standard Emacs
commands. EDesktop uses EXWM which can be seen as tiling window
manager.

EDesktop is targeted for developers who work with Emacs as their
primary IDE, and would like to a have clutter free desktop
environment.  A desktop environment where everything runs under the
aegis of Emacs.

Screenshots better describe the above concept.



# Screen Shots:

![EDesktop Screenshot](https://github.com/farazshaikh/Misc/blob/master/screenshots/EDesktop.png)


# Installation
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

<!--  LocalWords:  EDesktop
 -->
