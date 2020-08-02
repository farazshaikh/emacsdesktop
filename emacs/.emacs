;;; package -- Summary EOS_DESKTOP
;;         EXWM/Gnome based Desktop Environment using Emacs.
;;
;;; Commentary:
;;   Use EXWM to as a window manager and gnome and the desktop to provide a development
;;   friendly desktop.
;;
;;  -> Environment variable EOS_DESKTOP must be set run Emacs in a
;;  window manager/Desktop replacement mode.
;;
;; -> ~/.eosinstall file indicates the the installation of EOS_DESKTOP
;;  has been completed Removin this file will trigger an
;;  re-installation.  During installation packages will downloaded
;;  and installed only if they are not found on the system.
;;
;;  -> To do a full reinstallation remove the file ~/.eosinstall AND
;;  remove all the packages from ~/.emacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(set-background-color "#1c1e1f")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; Following setting saves 0.2 seconds an brings startup times down to 1.1sec
(setq gc-cons-threshold (* 250 1000 1000))

(defun es/check-version()
  "Check that Emacs version is at the least supported version."
  (if (version< emacs-version  "24.4")
      (error "Script depends on Emacs version being greater than 24.4")
    (message "Version greater or equal to 24.4")))
(es/check-version)


;; Package Mgmt and EOS installation
(defun es/setup-package-mgmt()
  "Setup the package management for EOS."
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("elpy" . "http://jorgenschaefer.github.io/packages/")))
  ;;elget
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil t)
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (goto-char (point-max))
       (eval-print-last-sexp))))
  (package-initialize)

  ;; quelpa
  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

  (message "es/setup-package-mgmt"))

(defun es/unsafe-signature-override()
  "Override package signature check requirements."
  (package-initialize)
  (unless (package-installed-p 'gnu-elpa-keyring-update)
    (progn
      (setq package-check-signature nil)
      (es/setup-package-mgmt)
      (package-install 'gnu-elpa-keyring-update)
      (setq package-check-signature t)))
  (message "es/setup-package-mgmt"))
;;(es/unsafe-signature-override)

(defun es/install-packages ()
  "Install all required packages."
  (interactive)
  (message "installing missing packages")
  (package-refresh-contents)
  (setq package-selected-packages
        '(auto-complete
          auto-complete-c-headers
          auto-complete-chunk
          auto-complete-clang
          auto-complete-clang-async
	  auto-complete-exuberant-ctags
          auto-complete-nxml

          ;; COMPANY & ITS BACKENDS
          company
          company-lsp
          company-quickhelp
          company-c-headers
          ;;company-cmake
          company-irony
          company-irony-c-headers
          company-go
          company-jedi

          ;; Completion engines
          function-args
          irony
          irony-eldoc
          jedi
          elpy
          ggtags

          ;; rust
          ac-racer
          flycheck-rust
          cargo

          ;; Snippets
          yasnippet
          yasnippet-snippets
          yasnippet-classic-snippets

          ;; go goodies
          go-autocomplete
          spacemacs-theme
          go-direx
          go-eldoc
          go-errcheck
          go-mode
          ;;go-play
          go-projectile
          go-snippets
          go-stacktracer
          golint
          go-eldoc

          ;; flycheck
          google-c-style
          flycheck
          flycheck-irony
          py-autopep8

          ;; javascript setup from emacs.cafe Nicolas Petton
          js2-mode
          xref-js2

          ;; emacs goodies
          free-keys
          ido-vertical-mode
          diminish

          ;; emacs next gen
          use-package))
  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package)))

  (write-region "" "" "~/.eosinstall")
  (message "es/setup-package-mgmt"))

(defvar
  es/eosinstallation nil
  "Are we in installation mode.  In installation mode all you are doing in downloading and setting up the packages.")

(defun es/install-if-needed()
  "Check if Emacs is being invoked in the installation mode."
  (interactive)
  (when (or (member "-eosinstall" command-line-args)
            (eq package-archive-contents nil)
            (not (file-exists-p "~/.eosinstall")))
    (progn
     (es/install-packages)
     (setq es/eosinstallation t))))

(es/setup-package-mgmt)
(es/install-if-needed)

;;;;;;;;;;;;;;;;;;;;;;;
;; Package Setup     ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :disabled
  :defines auto-package-update-delete-old-versions auto-package-update-hide-results
  :config
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package kaolin-themes
  :disabled
  :ensure t
  :config
  (setq custom-safe-themes t)
  (load-theme 'kaolin-bubblegum t)
  (kaolin-treemacs-theme))

(use-package monokai-pro-theme
  :ensure t
  :disabled
  :config
  (load-theme 'monokai-pro t))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

(use-package hydra :ensure t)

(use-package treemacs
  :disabled
  :ensure t
  :demand
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 nil
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   1
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         20)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 10)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (message "es/use-package-treemacs"))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(defun centaur-tabs-custom-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from
`eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'prog-mode)
     "Editing")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(helpful-mode
                        help-mode))
     "Help")
    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-src-mode
                        org-agenda-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode))
     "OrgMode")
    (t "Editing"))))


(use-package centaur-tabs
  :disabled
  :demand
  :init (setq centaur-tabs-set-bar 'over)
  :function centaur-tabs-force-update
  :config
  (centaur-tabs-mode +1)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker " ● "
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-height 10
        centaur-tabs-set-icons t
        centaur-tabs-close-button " × "
        centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-change-fonts "ubuntu-mono" 100)

  (defun centaur-tabs-group-by-custom ()
    "Custom grouping for Centaur tabs."
    (interactive)
    (defvar centaur-tabs-buffer-groups-function)
    (setq centaur-tabs-buffer-groups-function 'centaur-tabs-custom-buffer-groups)
    (centaur-tabs-force-update))
  (centaur-tabs-group-by-custom)
  (message "es/setup-package-centaur-tabs")
  :bind
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode))


(defun es/windowsetup()
  "After init hook for setting up windows."
  (interactive)
  ;; (customize-set-variable
  ;;  'display-buffer-base-action
  ;;  '((display-buffer-reuse-window display-buffer-pop-up-frame)
  ;;    (reusable-frames . 0)))
  (customize-set-variable
   'display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-same-window
                                  display-buffer-in-previous-window
                                  display-buffer-use-some-window))))

(use-package dashboard
  :ensure t
  :if window-system
  :demand
  :config
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner "~/acme.png")
  (setq dashboard-banner-logo-title "Cogito, ergo sum")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 30)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  :hook (window-setup . es/windowsetup))

(use-package winner-mode-enable
  :disabled
  :ensure t)

;; Start with the /run folder as  TMPDIR
(if (eq  system-type 'gnu/linux) (setenv "TMPDIR" (concat "/run/user/" (number-to-string (user-uid)))))

;; Load EXWM.
(defun es/setup-systray()
  (start-process "" nil "/usr/bin/python3 /usr/bin/blueman-applet")
  (start-process "" nil "/usr/lib/x86_64-linux-gnu/indicator-messages/indicator-messages-service")
  (start-process "" nil "/usr/lib/x86_64-linux-gnu/indicator-application/indicator-application-service")
;;  (start-process "" nil "zeitgeist-datahub")
  (start-process "" nil "update-notifier")
  (start-process "" nil "/usr/lib/deja-dup/deja-dup-monitor")

  (start-process "" nil "/usr/bin/nm-applet")
  (start-process "" nil "/usr/bin/blueman-applet")
  (start-process "" nil "/usr/bin/pasystray")
  (start-process "" nil "/usr/bin/xset" "dpms" "120 300 600")
  (message "es/setupsystray"))
(if (and window-system (getenv "EOS_DESKTOP") (eq system-type 'gnu/linux)) (es/setup-systray))

(defun es/set-up-gnome-desktop()
  "GNOME is used for most of the system settings."
  (setenv "XDG_CURRENT_DESKTOP" "GNOME")
  (start-process "" nil "/usr/bin/gnome-flashback")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gnome-settings-daemon")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-power")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-print-notifications")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-rfkill")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-screensaver-proxy")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-sharing")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-smartcard")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-xsettings")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-wacom")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-sound")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-a11y-settings")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-clipboard")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-color")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-datetime")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-housekeeping")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-keyboard")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-media-keys")
  (start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-mouse")
  (start-process "" nil "/usr/lib/gnome-disk-utility/gsd-disk-utility-notify")

  ;; setup gnome-keyring
  (start-process "" nil "/usr/bin/gnome-keyring-daemon --daemonize --login")
  (setq ssh-auth-sock  (shell-command-to-string  "/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh"))
  (setq ssh-auth-sock (split-string (replace-regexp-in-string "\n$" ""  ssh-auth-sock) "="))
  (setenv (car ssh-auth-sock) (car (cdr ssh-auth-sock)))
  (message "es/setup-up-gnome-desktop"))
(if (and window-system (getenv "EOS_DESKTOP") (getenv "EOS_EMACS_GNOME_SHELL_SETUP") (eq system-type 'gnu/linux)) (es/set-up-gnome-desktop))

(use-package exwm-config
  :disabled
  :init
  (exwm-config-default))

;;;;;;;;;;;;;;;;;;;;;;
;;Setup alt-tab     ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package iflipb
  :config
  (defvar iflipbTimerObj)
  (setq iflipbTimerObj nil)
  (defvar alt-tab-selection-hover-time)
  (setq alt-tab-selection-hover-time "1 sec")

  (defun timed-iflipb-auto-off ()
    (message ">")
    (setq last-command 'message))


  (defun timed-iflipb-next-buffer (arg)
    (interactive "P")
    (iflipb-next-buffer arg)

    (when iflipbTimerObj
      (cancel-timer iflipbTimerObj)
      (setq iflipbTimerObj nil))

    (setq iflipbTimerObj (run-at-time alt-tab-selection-hover-time nil 'timed-iflipb-auto-off)))


  (defun timed-iflipb-previous-buffer ()
    (interactive)
    (iflipb-previous-buffer)
    (when iflipbTimerObj
      (cancel-timer iflipbTimerObj)
      (setq iflipbTimerObj nil))
    (setq iflipbTimerObj (run-at-time alt-tab-selection-hover-time nil 'timed-iflipb-auto-off)))

  (defun iflipb-first-iflipb-buffer-switch-command ()
    "Determines whether this is the first invocation of iflipb-next-buffer or iflipb-previous-buffer this round."
    ;; (message "FR %s" last-command)
    (not (and (or (eq last-command 'timed-iflipb-next-buffer)
                  (eq last-command 'timed-iflipb-previous-buffer)))))


  ;; in iflip just flip with candidate windows that are not currently being displayed in a window
  ;; and include the current buffer
  ;; not doing so can jumble up the entire layout at other windows will swap buffers with current window
  (defun iflipb-ignore-windowed-buffers(buffer)
    ;;(message buffer)
    (if
        (or (eq (get-buffer-window buffer "visible") nil)
            (string= (buffer-name) buffer)
            )
        nil t))

  (defun setupIFlipb()
    "Alt-tab Super-tab for window switch."
    (interactive)
    (setq iflipb-wrap-around t)
    (setq iflipb-ignore-buffers 'iflipb-ignore-windowed-buffers)
    (setq iflipb-always-ignore-buffers "^[ *]")
    (global-set-key (kbd "<M-<tab>>") 'timed-iflipb-next-buffer)
    (global-set-key (kbd "C-M-i") 'timed-iflipb-next-buffer)
    (global-set-key (kbd "<M-<iso-lefttab>") 'timed-iflipb-previous-buffer)
    (exwm-input-set-key (kbd "M-<tab>") 'timed-iflipb-next-buffer)
    (exwm-input-set-key (kbd "M-<iso-lefttab>") 'timed-iflipb-previous-buffer)


    (global-set-key (kbd "<s-<tab>>") 'timed-iflipb-next-buffer)
    (global-set-key (kbd "<s-<iso-lefttab>") 'timed-iflipb-previous-buffer)
    (exwm-input-set-key (kbd "s-<tab>") 'timed-iflipb-next-buffer)
    (exwm-input-set-key (kbd "s-<iso-lefttab>") 'timed-iflipb-previous-buffer))

  (setupIFlipb))
(message "es/alt-tab")


(use-package windmove
  :ensure t
  :demand t
  :pin gnu
  :functions split-window-horizontally-and-follow split-window-vertically-and-follow winner-undo
  :config
  (defun split-window-horizontally-and-follow()
    "Focus follows the newly created window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  (defun split-window-vertically-and-follow()
    "Focus follows the newly created window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  :bind
  (("M-<left>" . 'windmove-left)
   ("M-<right>" .'windmove-right)
   ("M-<up>" .  'windmove-up)
   ("M-<down>" . 'windmove-down))

  ;; splits
  ("s-\\" . 'split-window-horizontally-and-follow)
  ("s-]" . 'split-window-vertically-and-follow)
  ("s-<backspace>" . 'delete-window)
  ("s-[" . 'delete-other-windows)
  ("s-u" . 'winner-undo)

  ;; fast moves
  ("C-<left>". backward-word)
  ("C-<right>". forward-word)
  ("C-<up>". backward-paragraph)
  ("C-<down>" . forward-paragraph)
  ("<find>" . beginning-of-line))


(use-package exwm
  :if (and
       window-system
       (getenv "EOS_DESKTOP"))
  :ensure windmove
  :pin gnu
  :demand
  :functions exwm-workspace-rename-buffer exwm-systemtray-enable exwm-randr-enable
  :hook
  (('exwm-update-class .
                       (lambda ()
                         (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                     (string= "gimp" exwm-instance-name))
                           (exwm-workspace-rename-buffer exwm-class-name))))
   ('exwm-update-title-hook .
                            (lambda ()
                              (when (or (not exwm-instance-name)
                                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                        (string= "gimp" exwm-instance-name))
                               (exwm-workspace-rename-buffer exwm-title)))))
  :config
  ;; create space
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1)

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  ;; Set floating window border
  (setq exwm-floating-border-width 3)
  (setq exwm-floating-border-color "orange")

  (setq exwm-workspace-number 4)
  ;; Per host dual monitor setup.
  ;; Map workspace 0 to the primary monitor. i.e. the attached monitor.
  ;; This is because the system tray is attached to the main workspace.
  ;; DP-1 HDMI-1 are usually the attached monitors.
  ;;
  ;; XXX: A simpler display/workspace mapping policy is to have the
  ;; highest/lowest resolution display host workspace 0
  (defvar exwm-randr-workspace-monitor-plist)
  (setq exwm-randr-workspace-monitor-plist '(0 "DP-1" 0 "HDMI-1"))
  (when (string= (system-name) "faraz-dfn-x1")
    (progn
      (setq exwm-randr-workspace-monitor-plist '(1 "eDP-1" 2 "DP-1" 0 "HDMI-1"))
      (add-hook 'exwm-randr-screen-change-hook
      (lambda ()
        (start-process-shell-command
         "xrandr" nil "xrandr --output eDP-1 --output DP-1 --output HDMI-1 --auto")))))

  ;; Access buffers from all workspaces
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
   '(
     ;; movement
     ([?\C-b] . [left])
     ([?\M-b] . [C-left])
     ([?\C-f] . [right])
     ([?\M-f] . [C-right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end])
     ([?\M-v] . [prior])
     ([?\C-v] . [next])
     ([?\C-d] . [delete])
     ([?\C-k] . [S-end delete])
     ;; cut/paste.
     ([?\C-w] . [?\C-x])  ;; Cut
     ([?\M-w] . [?\C-c])  ;; copy
     ([?\C-y] . [?\C-v])  ;; paste
     ;; search
     ([?\C-s] . (?\C-f))))
  (message "es/keyboard-setup")

  ;; setup alt-tab
  (exwm-input-set-key (kbd "<M-tab>") 'timed-iflipb-next-buffer)
  (exwm-input-set-key (kbd "<M-S-iso-lefttab>") 'timed-iflipb-previous-buffer)
  (exwm-input-set-key (kbd "s-<tab>") 'timed-iflipb-next-buffer)
  (exwm-input-set-key (kbd "s-<iso-lefttab>") 'timed-iflipb-previous-buffer)

  ;; applications
  (exwm-input-set-key (kbd "s-l") 'es/lock-screen)
  (exwm-input-set-key (kbd "s-g") 'es/app-browser)
  (exwm-input-set-key (kbd "s-t") 'es/app-terminal)
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-e") 'hydra-eos/body)
  (exwm-input-set-key (kbd "s-r") 'exwm-reset)
  (exwm-input-set-key (kbd "s-s") 'es/save-edit-position)
  (exwm-input-set-key (kbd "s-a") 'es/jump-edit-position)

  ;; window move
  (exwm-input-set-key (kbd "s-<left>") 'windmove-left)
  (exwm-input-set-key (kbd "s-<down>") 'windmove-down)
  (exwm-input-set-key (kbd "s-<up>") 'windmove-up)
  (exwm-input-set-key (kbd "s-<right>") 'windmove-right)
  ;; window resize
  (exwm-input-set-key (kbd "s-S-<right>")
                      (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 50)))
  (exwm-input-set-key (kbd "s-S-<left>")
                      (lambda () (interactive) (exwm-layout-shrink-window-horizontally 50)))
  (exwm-input-set-key (kbd "s-S-<up>")
                      (lambda () (interactive) (exwm-layout-enlarge-window             50)))
  (exwm-input-set-key (kbd "s-S-<down>")
                      (lambda () (interactive) (exwm-layout-shrink-window              50)))
    ;; window splits
  (exwm-input-set-key (kbd "s-\\") 'split-window-horizontally-and-follow)
  (exwm-input-set-key (kbd "s-]") 'split-window-vertically-and-follow)
  (exwm-input-set-key (kbd "s-<backspace>") 'delete-window)
  (exwm-input-set-key (kbd "s-[") 'delete-other-windows)
  (exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)
  (exwm-input-set-key (kbd "s-d") 'counsel-linux-app)

  ;; window undo
  (exwm-input-set-key (kbd "s-u") 'winner-undo)
  (exwm-input-set-key (kbd "s-k") 'exwm-input-release-keyboard)
  (exwm-input-set-key (kbd "s-j") 'exwm-input-grab-keyboard)
  (global-unset-key (kbd "C-z"))
  (setq exwm-input-global-keys
        `(
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch ,i))))
                    (number-sequence 0 9))
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))))

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (require 'exwm-randr)
  (exwm-randr-enable)

  ;; start the emacs x'window manager.
  (exwm-enable)

  ;; workaround to refresh screen size
  (exwm-workspace-delete)
  (exwm-workspace-add)
  :init
  (message "es/use-package/exwm"))

(defhydra hydra-eos (:exit nil :hint nil)
  "
Emacs Deskop EOS: Binding ALSO accessible under Super key i.e. s-b switch buffer
Apps^^                        EXWM^^                     Windows
-------------------------------------------------------------------------------------
[_d_] Linux application       [_w_] Workspace switch     [_<up>_] up
[_g_] Browser                 [_r_] Reset                [_<down>_] down
[_t_] Terminal                [_L_] Monitor Move left    [_<left>_] left
[_T_] New Terminal            [_R_] Monitor Move right   [_<right>_] right
[_E_] Treemacs Explorer       [_-_] Text size decrease   [_\\_] Vertical split
[_l_] lock screen             [_=_] Text size increase   [_]_] Horizontal split
[_s_] Splash                  [_s_] Save edit positon    [<backspace>] delete win
[_n_] netflix                 [_a_] Jump edit position   [_[_] delete other win
[_j_] ssh                                                [_u_] winner-undo
[_v_] Volume 200pct                                      [_b_] switch buffer
[_f_] rip grep                                           [_G_] GDM Tweaks
[_F_] ag silver searcher                                 [_S_] GDM Set Scale

"
  ("d"  counsel-linux-app)
  ("g" es/app-browser)
  ("t" es/terminal)
  ("T" es/app-terminal)
  ("E" treemacs)
  ("l" es/lock-screen)
  ("s" es/app-splash)
  ("n" es/app-netflix)
  ("j" es/ssh)
  ("v" es/volumeset)
  ("f" counsel-rg)
  ("F" counsel-ag)
  ("G" es/gdm-tweaks)
  ("S" es/gdm-set-scale)

  ("w" exwm-workspace-switch)
  ("r" exwm-workspace-reset)
  ("L" es/monitor-monitor-move-left)
  ("R" es/monitor-monitor-move-right)
  ("-" text-scale-decrease)
  ("=" text-scale-increase)
  ("s" es/save-edit-position)
  ("a" es/jump-edit-position)

  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("<right>" windmove-right)
  ("\\" split-window-horizontally-and-follow)
  ("]" split-window-vertically-and-follow)
  ("<backpsace>" delete-window)
  ("[" delete-other-windows)
  ("u" winner-undo)
  ("b" ivy-switch-buffer)
  ("q" nil :color blue))


(use-package ag
  :bind (("C-F" . counsel-ag)))   ;; for expanded results use ag command

(use-package rg
  :bind (("C-f" . counsel-rg)))    ;; for expanded results use rg command

;; magit on ssh-protected git repos
(use-package ssh-agency
  :ensure t)

(use-package flx :ensure t)


(defun ivy-switch-file-search ()
  "Switch to counsel-file-jump, preserving current input."
  (interactive)
  (let ((input (ivy--input)))
    (ivy-quit-and-run (counsel-git))))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("s-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-j" . counsel-fzf)
   ("s-d" . counsel-linux-app)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)

   :map counsel-find-file-map
   ("M-."  . ivy-switch-file-search)
   ("C-h"     . counsel-up-directory)
   ("RET" . ivy-alt-done)
   ))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         ("s-b" . 'ivy-switch-buffer)
         ("<f5>" . counsel-compile))
  :custom
  (global-set-key (kbd "C-d") 'ivy-backward-delete-char)
  (global-set-key (kbd "<f5>") 'compile)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-display-style 'fancy)
  (ivy-wrap t)
  (ivy-re-builders-alist
   '((swiper . ivy--regex)
     (t      . ivy--regex-plus)))
  :config
  (ivy-mode 1))

(use-package ivy-hydra)


(defun ivy-fix()
  "Fix ivy prefix its a work around there is unwanted interacttion in variable settings due to use package."
  (interactive)
  (message "fixing ivy prefixes")
  (setq ivy-initial-inputs-alist
                        '((counsel-minor . "^+")
                          (counsel-package . "^+")
                          (counsel-org-capture . "^")
                          (counsel-M-x . "")
                          (counsel-describe-function . "^")
                          (counsel-describe-variable . "^"))))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch)
	 ("C-c C-r" . ivy-resume))
  :hook (window-setup . ivy-fix)
  :custom
  ((ivy-use-virtual-buffers t)
   (ivy-display-style 'fancy))
  :config
  (ivy-mode 1)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package ivy-posframe
  :ensure t
  :config
  (if (and window-system (getenv "EOS_DESKTOP"))
      (setq ivy-posframe-parameters
        '((parent-frame nil)  ;; Required for EXWM
          (left-fringe . 30)
          (right-fringe . 30)
          (ivy-posframe-border-width 1)))
    (setq ivy-posframe-parameters
          '((left-fringe . 30)
            (right-fringe . 30)
            (ivy-posframe-border-width 1))))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  (setq ivy-posframe-display-functions-alist
        '((swiper-isearch  . ivy-posframe-display-at-window-bottom-left)
          (counsel-switch-buffer . ivy-posframe-display-at-window-bottom-left)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display-at-point)))
  (setq ivy-posframe-width 110
        ivy-posframe-height 30)
   (ivy-posframe-mode 1))


(setq ivy-posframe-border-width 3)

(use-package whitespace
  :hook
  (prog-mode-hook . whitespace-mode)
  :init
  (setq whitespace-global-modes '(not exwm-mode treemacs-mode Term-mode VTerm))
  :custom
  (whitespace-style (quote (face empty tabs lines-tail whitespace))))

(use-package flycheck
  :ensure t
  :hook (prog-mode . company-mode)
  :init
  (setq flycheck-global-modes '(not exwm-mode treemacs-mode)))

(use-package flyspell
  :init
  (defun flyspell-local-vars ()
    ;;(add-hook 'hack-local-variables-hook #'flyspell-buffer)
    )
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  (flyspell-mode . flyspell-local-vars))

(use-package flyspell-correct-ivy
  :bind ("C-;" . flyspell-correct-wrapper)
  :init
  (global-eldoc-mode -1)
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package git-gutter
  :diminish
  :hook (after-init . global-git-gutter-mode)
  :init (setq git-gutter:visual-line t
              git-gutter:disabled-modes '(asm-mode image-mode)
              git-gutter:modified-sign "*"
              git-gutter:added-sign "+"
              git-gutter:deleted-sign "x")

  :bind
  ("C-c g" . hydra-git-gutter/body))


(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (git-gutter-mode -1) :color blue))


(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status))
  :config
  (with-eval-after-load 'magit-log
    (define-key magit-log-mode-map (kbd "<M-tab>") nil))
  (with-eval-after-load 'magit-status
    (define-key magit-status-mode-map (kbd "<M-tab>") nil))
  (with-eval-after-load 'magit-diff
    (define-key magit-diff-mode-map (kbd "<M-tab>") nil))
  :custom
  ((magit-auto-revert-mode nil)
   (magit-diff-arguments (quote ("--no-ext-diff" "-M" "-C")))
   (magit-diff-refine-hunk t)
   (magit-expand-staged-on-commit (quote full))
   (magit-fetch-arguments (quote ("--prune")))
   (magit-log-auto-more t)
   (magit-log-cutoff-length 20)
   (magit-no-confirm (quote (stage-all-changes unstage-all-changes)))
   (magit-process-connection-type nil)
   (magit-push-always-verify nil)
   (magit-push-arguments (quote ("--set-upstream")))
   (magit-refresh-file-buffer-hook nil)
   (magit-save-some-buffers nil)
   (magit-set-upstream-on-push (quote askifnotset))
   (magit-stage-all-confirm nil)
   (magit-status-verbose-untracked nil)
   (magit-unstage-all-confirm nil)
   (magithub-message-confirm-cancellation nil)
   (magithub-use-ssl t)))


;;https://ladicle.com/post/config/#smerge
(use-package smerge-mode
  :diminish
  :preface
  (with-eval-after-load 'hydra
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue)))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body))))))


;; persistent-scratch
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets  :ensure t )
(use-package yasnippet-classic-snippets :ensure t )
(use-package popup  :ensure t )
(use-package function-args
  :config
  (fa-config-default))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :functions lsp-session lsp--persist-session
  :config
  (defun lsp-clear-session-blacklist()
    "Clear the list of blacklisted folders."
    (interactive)
    (setf (lsp-session-folders-blacklist (lsp-session)) nil)
    (lsp--persist-session (lsp-session)))
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-restart 'auto-restart)
  (lsp-enable-file-watchers nil)
  (lsp-file-watch-threshold 64)

  (lsp-rust-wait-to-build 10000)
  (lsp-rust-build-on-save t)
  (lsp-rust-jobs 2)
  ;; `company-lsp' is automatically enabled
  ;; (lsp-enable-completion-at-point nil)
  (lsp-file-watch-ignored '(
                            "[/\\\\]\\.direnv$"
                                        ; SCM tools
                            "[/\\\\]\\.git$"
                            "[/\\\\]\\.cargo$"
                            "[/\\\\]\\.hg$"
                            "[/\\\\]\\.bzr$"
                            "[/\\\\]_darcs$"
                            "[/\\\\]\\.svn$"
                            "[/\\\\]_FOSSIL_$"
                                        ; IDE tools
                            "[/\\\\]\\.idea$"
                            "[/\\\\]\\.ensime_cache$"
                            "[/\\\\]\\.eunit$"
                            "[/\\\\]node_modules$"
                            "[/\\\\]\\.fslckout$"
                            "[/\\\\]\\.tox$"
                            "[/\\\\]\\.stack-work$"
                            "[/\\\\]\\.bloop$"
                            "[/\\\\]\\.metals$"
                            "[/\\\\]target$"
                                        ; Autotools output
                            "[/\\\\]\\.deps$"
                            "[/\\\\]build-aux$"
                            "[/\\\\]autom4te.cache$"
                            "[/\\\\]\\.reference$"
                                        ; rls cargo etc
                            "[/\\\\]\\result???$"
                            "[/\\\\]\\target???$"
                            "[/\\\\]\\.cargo-home???$"
                                        ; ccls cache
                            "[/\\\\]\\.ccls-cache$"
                                        ; all hidden folders
                            "[/\\\\]\\.$"
                            ))
  :bind (:map lsp-mode-map
              ("C-c C-l" . hydra-lsp/body)
              ("C-c C-f" . lsp-format-buffer))
  :hook (((prog-mode) . 'display-line-numbers-mode)
         ((prog-mode) . lsp)))

(use-package lsp-ui
  :ensure t
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ([remap xref-find-apropos] . lsp-ui-find-workspace-symbol)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-glance t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-mode t)
  (lsp-ui-sideline-show-code-actions t)
  :config
  ;;  Use lsp-ui-doc-webkit only in GUI
  (setq lsp-ui-doc-use-webkit nil
        lsp-ui-peek-enable t
        lsp-ui-imenu-enable t
        lsp-ui-flycheck-enable t)
  ;;WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;;https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))



(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature       [_c_] clear blacklist
 [_e_] descirbe ession"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)
  ("e" lsp-describe-session)
  ("c" lsp-clear-session-blacklist)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))

(use-package unicode-fonts :if window-system :ensure t)
(use-package all-the-icons-dired :if window-system :ensure t)
(use-package all-the-icons
  :if window-system
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :config
  (message "es/use-package-all-the-icons")
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

(use-package spaceline :ensure t
  :custom-face
  (spaceline-highlight-face ((t (:foreground "black"))))
  :config
  (use-package fancy-battery :ensure t
    :config
    (setq fancy-battery-show-percentage t)
    (fancy-battery-mode)))

(use-package spaceline-config
  :ensure spaceline
  :functions
  spaceline-toggle-minor-modes-off
  spaceline-toggle-buffer-encoding-off
  spaceline-toggle-buffer-encoding-abbrev-off
  spaceline-toggle-time-on
  :config
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (setq powerline-default-separator 'slant)
  ;;(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-define-segment line-column
    "The current line and column numbers."
    "l:%l c:%2c")
  (spaceline-define-segment time
    "The current time."
    (format-time-string "%H:%M"))
  (spaceline-define-segment date
    "The current date."
    (format-time-string "%h %d"))
  (spaceline-toggle-time-on)
  (spaceline-emacs-theme 'date 'time))

(use-package auto-complete :ensure t)
(use-package auto-complete-config
  :disabled
  :requires auto-complete
  :ensure t)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (message "es/use-package-company")
   (setq company-idle-delay 0
         company-tooltip-align-annotations t
         company-tooltip-idle-delay 0
         company-minimum-prefix-length 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  :custom
  (company-lsp-cache-cadidates 'auto))

;; Python
(use-package elpy
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (add-hook 'python-mode-hook 'elpy-mode)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  :config
  (message "es/use-package-elpy")
  :custom
  ((elpy-rpc-python-command "python3")
   (elpy-rpc-backend "jedi")
   (jedi:setup-keys t)                      ; optional
   (jedi:complete-on-dot t)
   (python-python-command "/usr/bin/ipython")))

;; enable autopep8 formatting on save
(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; C/C++
(use-package company-c-headers :ensure t )
(use-package ccls
  :diminish
  :config
  (message "es/use-package-ccls")
  (setq ccls-executable "/snap/bin/ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (add-hook 'compilation-mode '(lamda ()
                                      (next-error-follow-minor-mode t)))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(use-package ggtags
  :ensure t
  :diminish)

(use-package company-c-headers
  :ensure t
  :diminish)

(use-package clang-format+
  :custom
  (clang-format-executable "clang-format-9")
  (clang-format-style "Google")
  :hook
  ((c-mode c++-mode glsl-mode) . clang-format+-mode))

;; YAS
(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind
  (:map yas-minor-mode-map
        ("C-c & t" . yas-describe-tables)
        ("C-c & &" . org-mark-ring-goto)))
(use-package yasnippet-snippets
  :defer)

;; Rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t))
(setq-default abbrev-mode 1)

;; Some custom configuration to ediff
(use-package ediff
  :ensure t
  :functions
  ediff-janitor ediff-cleanup-mess
  :custom
  ((ediff-split-window-function 'split-window-horizontally)
   (ediff-window-setup-function 'ediff-setup-windows-plain)
   (ediff-keep-variants nil))
  :config

  (defvar my-ediff-bwin-config nil "Window configuration before ediff.")
  (defcustom my-ediff-bwin-reg ?b
    "*Register to be set up to hold `my-ediff-bwin-config' configuration."
    :type 'integer ;; supress linter
    :group 'ediff)
  (defvar my-ediff-bwin-reg)

  (defvar my-ediff-awin-config nil "Window configuration after ediff.")
  (defcustom my-ediff-awin-reg ?e
    "*Register to be used to hold `my-ediff-awin-config' window configuration."
    :type 'integer    ;; supress linter
    :group 'ediff)
  (defvar my-ediff-awin-reg)

  (defun my-ediff-bsh ()
    "Function to be called before any buffers or window setup for ediff."
    (setq my-ediff-bwin-config (current-window-configuration))
    (when (characterp my-ediff-bwin-reg)
      (set-register my-ediff-bwin-reg
                    (list my-ediff-bwin-config (point-marker)))))

  (defun my-ediff-ash ()
    "Function to be called after buffers and window setup for ediff."
    (setq my-ediff-awin-config (current-window-configuration))
    (when (characterp my-ediff-awin-reg)
      (set-register my-ediff-awin-reg
                    (list my-ediff-awin-config (point-marker)))))

  (defun my-ediff-qh ()
    "Function to be called when ediff quits."
    (ediff-janitor nil nil)
    (ediff-cleanup-mess)
    (when my-ediff-bwin-config
      (set-window-configuration my-ediff-bwin-config)))

  ;; FRZ: TODO hooks cannot be placed in :hook section
  (add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
  (add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
  (add-hook 'ediff-quit-hook 'my-ediff-qh)
  (message "es/workarounds"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Specific Setup                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun es/setup-project-dfn()
  "Setup DFN project."
  (interactive)
  (setenv "RUST_SRC_PATH" "/home/emacs/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")
  (setenv "WRK" (concat (concat "/home/" (getenv "USER") "/dfn/dfinity/.")))
  (setq compile-command
        "cd $WRK/rs;\n\
 source ~/.nix-profile/etc/profile.d/nix.sh;\n \
 nix-shell --run \"cargo build\"")
  )


(defun es/setup-project-sp()
  "Setup SP project."
  (interactive)
  (setenv "WRK" "/storvisor/work/cypress")
  (setq compile-command
   "cd $WRK; source ./setvars.sh debug; DBUILDCMD=\"make -j32 BUILDTYPE=debug\" ./docker/build_template/build.sh  buildcmd")
)


(defun es/setup-project-excb()
  "Setup Excubito Project."
  (interactive)
  (setenv "WRK" (concat (concat "/home/" (getenv "USER") "/excubito_workspace/hazen/.")))
)
(es/setup-project-dfn)

;; Setup projectile
(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path (getenv "WRK"))
  (projectile-mode))

(message "!!es/packages-loaded!!")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup common variables across packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1c1e1f" "#e74c3c" "#b6e63e" "#e2c770" "#268bd2" "#fb2874" "#66d9ef" "#d6d6d4"])
 '(c-basic-offset 3)
 '(c-echo-syntactic-information-p t)
 '(c-insert-tab-function (quote insert-tab))
 '(c-report-syntactic-errors t)
 '(clang-format-executable "clang-format-9" t)
 '(clang-format-style "Google" t)
 '(column-number-mode t)
 '(company-lsp-cache-cadidates (quote auto) t)
 '(compilation-scroll-output (quote first-error))
 '(dabbrev-case-fold-search nil)
 '(display-buffer-base-action
   (quote
    ((display-buffer-reuse-window display-buffer-same-window display-buffer-in-previous-window display-buffer-use-some-window))))
 '(ediff-keep-variants nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elpy-rpc-backend "jedi" t)
 '(elpy-rpc-python-command "python3")
 '(exwm-layout-show-all-buffers t)
 '(fci-rule-color "#555556")
 '(global-eldoc-mode nil)
 '(global-hl-line-mode t)
 '(global-set-key [f5] t)
 '(ido-mode t nil (ido))
 '(ido-vertical-define-keys (quote C-n-and-C-p-only))
 '(ido-vertical-mode 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-count-format "%d/%d ")
 '(ivy-display-style (quote fancy))
 '(ivy-re-builders-alist (quote ((swiper . ivy--regex) (t . ivy--regex-plus))) t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(jedi:complete-on-dot t t)
 '(jedi:setup-keys t t)
 '(load-home-init-file t t)
 '(lsp-auto-guess-root nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-file-watch-ignored
   (quote
    ("[/\\\\]\\.direnv$" "[/\\\\]\\.git$" "[/\\\\]\\.cargo$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]\\result???$" "[/\\\\]\\target???$" "[/\\\\]\\.cargo-home???$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.$")))
 '(lsp-file-watch-threshold 64)
 '(lsp-prefer-flymake nil)
 '(lsp-restart (quote auto-restart))
 '(lsp-rust-build-on-save t)
 '(lsp-rust-jobs 2)
 '(lsp-rust-wait-to-build 10000)
 '(lsp-ui-doc-border "black" t)
 '(lsp-ui-doc-enable t t)
 '(lsp-ui-doc-glance t t)
 '(lsp-ui-doc-header t t)
 '(lsp-ui-doc-include-signature t t)
 '(lsp-ui-doc-position (quote bottom) t)
 '(lsp-ui-sideline-enable t t)
 '(lsp-ui-sideline-ignore-duplicate t t)
 '(lsp-ui-sideline-mode t t)
 '(lsp-ui-sideline-show-code-actions t t)
 '(lsp-ui-sideline-update-mode (quote line))
 '(magit-auto-revert-mode nil)
 '(magit-diff-arguments (quote ("--no-ext-diff" "-M" "-C")) t)
 '(magit-diff-refine-hunk t)
 '(magit-expand-staged-on-commit (quote full) t)
 '(magit-fetch-arguments (quote ("--prune")) t)
 '(magit-log-auto-more t)
 '(magit-log-cutoff-length 20 t)
 '(magit-no-confirm (quote (stage-all-changes unstage-all-changes)))
 '(magit-process-connection-type nil)
 '(magit-push-always-verify nil t)
 '(magit-push-arguments (quote ("--set-upstream")) t)
 '(magit-refresh-file-buffer-hook nil t)
 '(magit-save-some-buffers nil t)
 '(magit-set-upstream-on-push (quote askifnotset) t)
 '(magit-stage-all-confirm nil t)
 '(magit-status-verbose-untracked nil t)
 '(magit-unstage-all-confirm nil t)
 '(magithub-message-confirm-cancellation nil t)
 '(magithub-use-ssl t t)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(normal-erase-is-backspace-mode 0)
 '(objed-cursor-color "#e74c3c")
 '(package-selected-packages
   (quote
    (clang-format+ persistent-scratch git-gutter ivy-prescient flycheck-posframe exwm-randr exwm-systemtray auto-package-update spaceline-config golden-ratio rg ripgrep lsp-ivy eglot flyspell-correct-ivy haskell-mode haskell-emacs xwidgete ssh-agency vterm mini-modeline ivy-posframe rust-playground fancy-battery doome-themes doom-themes realgud page-break-lines quelpa-use-package elisp-cache dashboard clues-theme monokai-pro-theme spaceline-all-the-icons spaceline powerline-evil auto-complete auto-complete-c-headers auto-complete-chunk auto-complete-clang auto-complete-clang-async auto-complete-etags auto-complete-exuberant-ctags auto-complete-nxml company company-lsp company-quickhelp company-c-headers company-cmake company-irony company-irony-c-headers company-go company-jedi function-args irony irony-eldoc jedi elpy ggtags ac-racer flycheck-rust cargo yasnippet yasnippet-snippets yasnippet-classic-snippets go-autocomplete spacemacs-theme go-direx go-eldoc go-errcheck go-mode go-play go-snippets go-stacktracer golint go-eldoc google-c-style flycheck flycheck-irony py-autopep8 powerline company-tern js2-mode xref-js2 free-keys ido-vertical-mode ag iflipb kaolin-themes diminish use-package general centaur-tabs treemacs flx swiper ivy ivy-hydra counsel hydra lsp-ui lsp-mode lsp-treemacs git-timemachine magit)))
 '(pdf-view-midnight-colors (cons "#d6d6d4" "#1c1e1f"))
 '(python-python-command "/usr/bin/ipython" t)
 '(ring-bell-function
   (lambda nil
     (let
         ((orig-fg
           (face-foreground
            (quote mode-line))))
       (set-face-foreground
        (quote mode-line)
        "#6495ED")
       (run-with-idle-timer 0.1 nil
                            (lambda
                              (fg)
                              (set-face-foreground
                               (quote mode-line)
                               fg))
                            orig-fg))))
 '(rustic-ansi-faces
   ["#1c1e1f" "#e74c3c" "#b6e63e" "#e2c770" "#268bd2" "#fb2874" "#66d9ef" "#d6d6d4"])
 '(safe-local-variable-values (quote ((buffer-reado-only . t))))
 '(savehist-mode 1)
 '(scroll-step 1)
 '(select-enable-clipboard t)
 '(send-mail-function (quote smtpmail-send-it))
 '(set-fill-column 80)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(smtpmail-auth-credentials (quote (("smtp.gmail.com" 587 "faraz@email.com" nil))))
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-starttls-credentials (quote (("smtp.gmail.com" 587 nil nil))))
 '(speedbar-default-position (quote left))
 '(standard-indent 3)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(vc-annotate-background "#1c1e1f")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b6e63e")
    (cons 40 "#c4db4e")
    (cons 60 "#d3d15f")
    (cons 80 "#e2c770")
    (cons 100 "#ebb755")
    (cons 120 "#f3a73a")
    (cons 140 "#fd971f")
    (cons 160 "#fc723b")
    (cons 180 "#fb4d57")
    (cons 200 "#fb2874")
    (cons 220 "#f43461")
    (cons 240 "#ed404e")
    (cons 260 "#e74c3c")
    (cons 280 "#c14d41")
    (cons 300 "#9c4f48")
    (cons 320 "#77504e")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil)
 '(which-function-mode t)
 '(whitespace-style (quote (face empty tabs lines-tail whitespace)))
 '(winner-mode t))
(message "es/customizations-applied")

;;;;;;;;;;;;;;;;;;;
;; Coloring      ;;
;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:background "#2d2e2e" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(ivy-posframe-border ((t (:inherit internal-border :background "white" :foreground "white"))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(spaceline-highlight-face ((t (:foreground "black")))))
(message "es/custom-set-faces")

;;;;;;;;;;;;;;;
;;Workarounds;;
;;;;;;;;;;;;;;;
(defun exwm-workspace-ui()
  "Workspace UI for exwm."
  (easy-menu-define exwm-workspace-menu nil
    "Menu for Exwm Workspace.
Also used in `exwm-mode-line-workspace-map'."
    '("Exwm Workspace"
      ["Add workspace" exwm-workspace-add]
      ["Delete current workspace" exwm-workspace-delete]
      ["Move workspace to" exwm-workspace-move]
      ["Swap workspaces" exwm-workspace-swap]
      ["Move X window to" exwm-workspace-move-window]
      ["Move X window from" exwm-workspace-switch-to-buffer]
      ["Toggle minibuffer" exwm-workspace-toggle-minibuffer]
      ["Switch workspace" exwm-workspace-switch]
      ;; Place this entry at bottom to avoid selecting others by accident.
      ("Switch to" :filter
       (lambda (&rest _args)
         (mapcar (lambda (i)
                   `[,(format "workspace %d" i)
                     (lambda ()
                       (interactive)
                       (exwm-workspace-switch ,i))
                     (/= ,i exwm-workspace-current-index)])
                 (number-sequence 0 (1- (exwm-workspace--count))))))))

  (defvar exwm-mode-line-workspace-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] 'exwm-workspace-switch)
      (define-key map [mode-line mouse-3] exwm-workspace-menu)
      map)
    "Local keymap for EXWM mode line string.  See `exwm-mode-line-format'.")

  (defvar exwm-mode-line-format)
  (defcustom exwm-mode-line-format
    `("["
      (:propertize (:eval (format "WS-%d" exwm-workspace-current-index))
                   local-map ,exwm-mode-line-workspace-map
                   face bold
                   mouse-face mode-line-highlight
                   help-echo "mouse-1: Switch to / add / delete to EXWM workspaces.
mouse-2: EXWM Workspace menu.
")
      "]")
    "EXWM workspace in the mode line."
    :type 'sexp
    :group 'exwm)
  (add-to-list 'mode-line-misc-info exwm-mode-line-format t))
(exwm-workspace-ui)



;;https://stackoverflow.com/questions/12965814/emacs-how-can-i-eliminate-whitespace-mode-in-auto-complete-pop-ups/27960576#27960576
(defun my:force-modes (rule-mode &rest modes)
  "RULE-MODE MODES switch on/off several modes depending of state of the controlling minor mode."
    (let ((rule-state (if rule-mode 1 -1)
                      ))
      (mapcar (lambda (k) (funcall k rule-state)) modes)
      )
    )
(defvar my:prev-whitespace-mode nil)
(make-variable-buffer-local 'my:prev-whitespace-mode)
(defvar my:prev-whitespace-pushed nil)
(make-variable-buffer-local 'my:prev-whitespace-pushed)
(defun my:push-whitespace (&rest skip)
  "SKIP docstring :(."
  (if my:prev-whitespace-pushed () (progn
                                     (setq my:prev-whitespace-mode whitespace-mode)
                                     (setq my:prev-whitespace-pushed t)
                                     (my:force-modes nil 'whitespace-mode)
                                     ))
  )

(defun my:pop-whitespace (&rest skip)
  "SKIP docstring :(."
  (if my:prev-whitespace-pushed (progn
                                  (setq my:prev-whitespace-pushed nil)
                                  (my:force-modes my:prev-whitespace-mode 'whitespace-mode)
                                  ))
  )
(advice-add 'popup-draw :before #'my:push-whitespace)
(advice-add 'popup-delete :after #'my:pop-whitespace)
;; End workaround auto complete and whitespace


;; Compilation buffer colorize
(when (require 'ansi-color nil t)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;;  terminal mode settings
(add-hook 'ansi-term-mode-hook '(lambda ()
      (setq term-buffer-maximum-size 0)
      (setq-default show-trailing-whitespace f)
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper/Utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun es/save-edit-position()
  (interactive)
  (point-to-register ?p))

(defun es/jump-edit-position()
  (interactive)
  (jump-to-register ?p))

(defun es/toggle-show-trailing-whitespace ()
  "Toggle 'show-trailing-whitespace' between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun es/tags-create (dir-name)
     "Create tags file Arguments DIR-NAME."
     (interactive "DDirectory: ")
     (eshell-command
      (format "find %s -type f -name \"*.hpp\" -o -name \"*.cpp\" -o -name \"*.[ch]\" | xargs etags -f %s/TAGS" dir-name dir-name))
     (eshell-command
      (format "cd %s; gtags -i -q" dir-name))
)

(defun es/copy-rectangle-as-kill ()
    "Copy a rectangle as kill."
    (interactive)
    (save-excursion
    (kill-rectangle (mark) (point))
    (exchange-point-and-mark)
    (yank-rectangle)))

(define-minor-mode es/sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) es/sticky-buffer-mode))

(define-minor-mode es/sticky-buffer-mode-clear
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) nil))


(defun es/mark-directory-readonly(name)
  "Mark a directory NAME to be opened readonly under Emacs."
  (interactive "sDirectory Name:")
  (setq name (concat name "/./.dir-locals.el"))
  (message (concat "Created : " name))
  (unless (file-exists-p name)
    (progn
      (write-region "" nil name)
      (f-append-text "((nil . ((buffer-reado-only . t))))" 'utf-8 name))))

(defun es/adb-reverse()
  "Start a reverse ADB session to debug android applications running a emulator."
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb"
   nil
   "adb devices | head -n2  | tail -n1 | cut -f 1 | xargs -I{} adb -s {} reverse tcp:8081 tcp:8081"))

(defun es/adb-reload()
  "Emulates the key R on the android mobile emulator.  Used for reloading a react native app."
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb" nil "adb shell input keyevent R"))


(defun es/adb-shake()
  "Emulates a shake on the mobile emulator."
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb" nil  "adb shell input keyevent 82"))
(message "es/helper-utilities")


;;;;;;;;;;;;;
;; GO LANG ;;
;;;;;;;;;;;;;
(require 'go-autocomplete)
(require 'auto-complete-config)
(defconst es/_goroot "/home/farazl/excubito_workspace/scratch/go/golang/go"  "Go toolchain root.")
(defun ac-go-mode-setup()
  "Auto complete setup for go."
  ;;(setenv "PATH" (concat (getenv "PATH") ":" (concat es/_goroot "/bin")))
  (local-set-key (kbd "M-.") 'godef-jump)
)

(setenv "GOPATH" (getenv "WRK"))
(defun go-set-gopath(_gopath)
  "Set up the path for GO workspace."
  (interactive "Set Go PATH:")
  (setenv "GOPATH" _gopath)
  )

;;(require 'go-eldoc)
(add-hook 'go-mode-hook 'ac-go-mode-setup)
;;(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'ac-go-mode-setup)


;;;;;;;;;;;;;;;;
;; JavaScript ;;
;;;;;;;;;;;;;;;;
(require 'company)
(require 'company-tern)
(defun js2-mode-setup()
  "Setup Tern mode for javascript."
  (tern-mode)
  (company-mode)
  (add-to-list 'company-backends 'company-tern)
  ;;  (auto-complete-mode)  // either AC + or company may Complete
  ;; Disable completion keybindings, as we use xref-js2 instead
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (local-set-key (kbd "s-a") 'adbShake)
)

(add-hook 'js2-mode-hook 'js2-mode-setup)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(put 'downcase-region 'disabled nil)
(message "es/legacy-lang-setup")

;; Application invocations
(defun find-named-buffer(buffPrefix)
  "Find a named buffer BUFFPREFIX."
  (defvar named-buffer)
  (setq named-buffer nil)
  (cl-loop for buf in (buffer-list)  do
           (when (string-prefix-p buffPrefix (buffer-name buf))
             (progn
               (setq named-buffer buf)
                (cl-return)
               )
             )
           )
  named-buffer)

(defun es/app-netflix()
  "Start Netflix."
  (interactive)
  (defvar es/browser-bufname)
  (defvar es/browser-binary)
  (defvar es/browser-invocation)

  (setq es/browser-binary "/usr/bin/google-chrome")
  (setq es/browser-invocation (concat es/browser-binary))
  (setq es/browser-invocation (concat es/browser-invocation "  --app=http://netflix.com"))
  (message "Opening Web browser")
  (start-process-shell-command
   es/browser-binary nil  es/browser-invocation))

(defun es/volumeset()
  "Workaround for low sound an X1, allow you set the volume to more that 100%."
  (interactive)
  (defvar es/pulse-cmd-binary)
  (setq es/pulse-cmd-binary "/usr/bin/pactl")
  (start-process-shell-command
   es/pulse-cmd-binary nil "/usr/bin/pactl list | grep -oP \'Sink #\K([0-9]+)\' | while read -r i ; /usr/bin/pactl -- set-sink-volume $i 200"))

(defun es/app-browser()
  "Find existing or open a new browser window."
  (interactive)
  (defvar es/browser-bufname)
  (defvar es/browser-binary)
  (defvar es/browser-invocation)
  (setq es/browser-bufname "Google-chrome")
  (setq es/browser-binary "/usr/bin/google-chrome")
  (setq es/browser-invocation (concat es/browser-binary))

  (defvar es/browser)
  (setq es/browser (find-named-buffer es/browser-bufname))
  (if (eq es/browser nil)
      (progn
        (message "Opening Web browser")
        (start-process-shell-command
         es/browser-binary nil  es/browser-invocation))
    (progn
      (message "Web browser")
      (switch-to-buffer es/browser))
    ))

(defun es/app-terminal()
  "Find existing or open a new terminal window."
  (interactive)
  (defvar es/terminal)
  (defvar es/termbufname)
  (setq es/termbufname "Gnome-terminal")
  (setq es/terminal (find-named-buffer es/termbufname))
  (if (eq es/terminal nil)
      (progn
        (message "Opening terminal")
        (es/app-terminal-new))
    (progn
      (message "terminal")
      (switch-to-buffer es/terminal))
    ))

(defun es/app-splash()
  "EOS Splash screen."
  (interactive)
  (defvar es/splashbufname)
  (setq es/splashbufname "feh")
  (defvar es/splash)
  (setq es/splash (find-named-buffer es/splashbufname))
  (if (eq es/splash nil)
      (progn
        (defvar es/splashbinary)
        (defvar es/splashinvocation)
        (setq es/splashbinary "feh")
        (setq es/splashinvocation (concat es/splashbinary
                                      " ~/acme.png"
                                      ))
        (message "Opening splash")
        (start-process-shell-command
         es/splashbinary nil es/splashinvocation))
    (progn
      (message "splash")
      (switch-to-buffer es/splash))
    ))

(defun es/app-terminal-new()
  "Start a new terminal."
  (interactive)
  (defvar es/term-bufname)
  (defvar es/term-binary)
  (defvar es/term-invocation)
  (setq es/term-bufname "Gnome-terminal")
  (setq es/term-binary "/usr/bin/gnome-terminal")
  (setq es/term-invocation es/term-binary)
  (progn
    (message "Opening terminal")
    (start-process-shell-command
     es/term-binary nil es/term-invocation))
  )

(defun es/lock-screen()
  "Lock screen command for es."
  (interactive)
  (start-process-shell-command
   "/usr/bin/gnome-screensaver-command" nil  "/usr/bin/gnome-screensaver-command -l"))


;; Swap monitors
(defun es/monitor-monitor-move-left()
  "Move primary monitor to left."
  (interactive)
  (defvar es/commandMoveLeft)
  (setq es/commandMoveLeft
        "`xrandr  | grep -w connected | cut -f 1  -d  \" \"  | paste -s -d _ |  sed  's/_/ --left-of /;s/^/xrandr --output /'`")
  (shell-command es/commandMoveLeft))

(defun es/monitor-move-right()
  "Move primary monitor to right."
  (interactive)
  (defvar es/commandMoveRight)
  (setq es/commandMoveRight
        "`xrandr  | grep -w connected | cut -f 1  -d  \" \"  | paste -s -d _ |  sed  's/_/ --right-of /;s/^/xrandr --output /'`")
  (shell-command es/commandMoveRight))

;; Scale/Descale gdm
(defun es/gdm-set-scale(scale-factor)
  "Set gnome desktop scaling factor to SCALE-FACTOR."
  (interactive "scale-factor: ")
  (defvar es/gsettings-binary)
  (defvar es/gsettings-invocation)
  (setq es/gsettings-binary "/usr/bin/gsettings")
  (setq es/gsettings-invocation
        (concat es/gsettings-binary  " set org.gnome.desktop.interface text-scaling-factor "  scale-factor))
  (message es/gsettings-invocation)
  (start-process-shell-command
   es/gsettings-binary nil es/gsettings-invocation))

(defun es/gdm-tweaks()
  "Open gnome tweaks."
  (interactive)
  (start-process-shell-command
   "/usr/bin/gnome-tweaks" nil  "/usr/bin/gnome-tweaks"))


(defun es/ssh(hostName)
  "SSH to a host HOSTNAME."
  (interactive "suserName@Host:")
  (defvar ssh-bufname)
  (defvar ssh-binary)
  (defvar ssh-invocation)
  (setq ssh-bufname "XTerm")
  (setq ssh-binary "/usr/bin/xterm")
  (setq ssh-invocation (concat ssh-binary
                                " -bg black -fg white "
                                " -fa 'Ubuntu Mono'"
                                " -e 'ssh -Y " hostName "'"
                                ))
  (start-process-shell-command  ssh-binary nil ssh-invocation))
(message "es/app-setup")

;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Setup  ;;
;;;;;;;;;;;;;;;;;;;;;
(defun es/input-decode-map-putty()
  "Keys for iterm2.  You have to edit corresponding entries in iterm."
  (interactive)
  (define-key input-decode-map "\e[A" [(meta up)])
  (define-key input-decode-map "\e[B" [(meta down)])
  (define-key input-decode-map "\ef" [(meta right)])
  (define-key input-decode-map "\eb" [(meta left)])
  ;; putty sends escape sequences
  (define-key input-decode-map "\e\eOA" [(meta up)])
  (define-key input-decode-map "\e\eOB" [(meta down)])
  (define-key input-decode-map "\e\eOC" [(meta right)])
  (define-key input-decode-map "\e\eOD" [(meta left)]))

(defun es/input-decode-map-xterm-compatibility()
  "Key bindinds based on xterm.defaullts presets set by iterm2."
  (interactive)
  (define-key input-decode-map "\e[1;5A" [(ctrl up)])
  (define-key input-decode-map "\e[1;5B" [(ctrl down)])
  (define-key input-decode-map "\e[1;5C" [(ctrl right)])
  (define-key input-decode-map "\e[1;5D" [(ctrl left)])

  (define-key input-decode-map "\e[1;3A" [(meta up)])
  (define-key input-decode-map "\e[1;3B" [(meta down)])
  (define-key input-decode-map "\e[1;3C" [(meta right)])
  (define-key input-decode-map "\e[1;3D" [(meta left)]))

(add-hook 'tty-setup-hook 'es/input-decode-map-xterm-compatibility)

;; windmove gnome terminal keys
(defvar real-keyboard-keys
  '(("M-<up>"        . "\M-[1;3A")
    ("M-<down>"      . "\M-[1;3B")
    ("M-<right>"     . "\M-[1;3C")
    ("M-<left>"      . "\M-[1;3D")
    ("C-<return>"    . "\C-j")
    ("C-<delete>"    . "\M-[3;5~")
    ("C-<up>"        . "\M-[1;5A")
    ("C-<down>"      . "\M-[1;5B")
    ("C-<right>"     . "\M-[1;5C")
    ("C-<left>"      . "\M-[1;5D"))
  "An assoc list of pretty key strings and their terminal equivalents.")

(defun key (desc)
  "Elint DESC suppress."
  (or (and window-system (read-kbd-macro desc))
      (or (cdr (assoc desc real-keyboard-keys))
          (read-kbd-macro desc))))

(message "!!!es/load-complete!!!")
(server-start)
(message "!!!es/load-complete!!!")

;; Make gc pauses faster by decreasing the threshold.  This works in
;; conjunction with gc setting set up in the starting of the file
(setq gc-cons-threshold (* 128 1000 1000))

(provide '.emacs)
;;; .emacs ends here
