Emacs Desktop Environment (EDesktop)
====

EDesktop is an Emacs+Gnome based desktop environment.  It uses the EXWM
window manager in conjunction with gnome desktop to provide a complete
desktop experience from within Emacs.

EDesktop aims to seamlessly extend Emacs windows management to a
general desktop environment.  In EDesktop application windows are
treated a standard Emacs buffers and are controlled using Emacs window
management commands (Ex `C-x b`, `C-x 1`, `winner-undo`, `windmove-*`
etc). EDesktop uses EXWM which can be seen as a tiling window manager.

EDesktop is targeted for developers who work with Emacs as their
primary IDE, and would like to a have uniform way of handling Emacs
and other desktop windows.  A desktop environment where everything
runs under the aegis of Emacs.

Screenshots better describe EDesktop in concept.

# Screenshots:

### EDesktop hosting code, browser, terminal, Netflix window (also seen is a system tray with Wireless, Bluetooth and Sound applets)
![EDesktop Screenshot](https://github.com/farazshaikh/Misc/blob/master/screenshots/EDesktop.jpg)

### EDesktop startup screen
![EDesktop Screenshot](https://github.com/farazshaikh/Misc/blob/master/screenshots/EDesktopdash.jpg)

### EDesktop Language Server Protocol Integration
![EDesktop LSP](https://github.com/farazshaikh/emacsdesktop/blob/master/screenshots/EDesktoplsp.jpg)

# Installation (Only Single-user installation is supported)
```
wget -qO- https://raw.githubusercontent.com/farazshaikh/emacsdesktop/master/emacs/installexwm.sh | bash -x
```

NOTE: Single user installation.

The installation is done by checking out the EDesktop project under
${HOME}/.eos folder. Then necessary sym-links are created to files in
the git local repository.

For example, .emacs will be replaced with sym-link to .emacs provided by
EDesktop

~/.emacs -> ~/.eos/emacsdesktop/emacs/.emacs

After installation use can log in via GDM greeter and choose EDesktop
from the session drop-down list.

Only the user that installed the EDesktop will be able to launch the
EDesktop session.

### Login Details:
#### UI Login:

GDM/LigthDM login will include a session entry named EmacsDesktop.
![EDesktop Login](https://github.com/farazshaikh/emacsdesktop/blob/master/screenshots/EDesktoplogin.jpg)

#### Login from the Terminal:
Also provided is a ~/.xinitrc, using this one can start the desktop
under the X server by invoking `[#] startx` from the command prompt.

# Running Applications
  [Super+D] Starts a ivy-counsel based Linux application launcher.

# Managing application

  Applications are treated as Emacs buffers. All commands that are
applicable to Emacs buffers are now applicable to application buffers.

For example, `C-x k` will kill an application in the same way it kills a
buffer.

# Window Management

  EDesktop application windows can be managed as Emacs windows. For
  example `C-x 1` will make an application be the only window
  visible. `C-x 2` will split the window vertically and `C-x 3` will
  split horizontally.

  Winner mode is enabled by default which tracks all changes to
  window. These changes can be cycled (undo/redo) using with `winner`
  commands.

  In addition to the above commands EDesktop provides a bunch of ergonomic shortcuts like.

  Super-t  Finds existing or opens a new terminal
  Super-g  Finds existing or opens a new chrome browser

# Key binding/EDesktop Hydra

  EDesktop specific bindings are invoked using the super key. All key
  bindings are also accessible using the EDesktop hydra which is bound to [Super+E]
![EDesktop Hydra Login](https://github.com/farazshaikh/emacsdesktop/blob/master/screenshots/EDesktophydra.jpg)

# Git/Magit terminal and Language Server Protocol (LSP) integration.

  EDesktop comes with some git and terminal integration. For example,
invoking `# git difftool` from a terminal with EDesktop will open a
`ediff` session under EDesktop.  Upon completion of the EDiff session
the window configuration will be restored.

 Similarly running `# ec filename` from a command line user EDesktop
 will open the file under Emacs.

![EDesktop Hydra ec](https://github.com/farazshaikh/emacsdesktop/blob/master/screenshots/EDesktopec.jpg)
![EDesktop Hydra ec](https://github.com/farazshaikh/emacsdesktop/blob/master/screenshots/EDesktopdiff.jpg)

# Logout
  Exit Emacs `C-x c`

<!-- LocalWords: EDesktop -->
