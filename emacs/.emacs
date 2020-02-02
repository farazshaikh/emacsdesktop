;;; package -- Summary EOS
;;         EXWM/Gnome based Desktop Environment using Emacs.
;;; Commentary:
;;         Use EXWM to as a window manager and gnome and the desktop to provide a development
;;         friendly desktop.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
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
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("elpy" . "http://jorgenschaefer.github.io/packages/")))
  ;;elget
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil t)
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp))))
  (package-initialize)

  ;; quelpa
  (unless (require 'quelpa nil t)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/bootstrap.el")
      (eval-buffer)))
  (message "es/setup-package-mgmt"))

(defun es/unsafe-signature-override() 
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
          auto-complete-etags
          auto-complete-exuberant-ctags
          auto-complete-nxml

          ;; COMPANY & ITS BACKENDS
          company
          company-lsp
          company-quickhelp
          company-c-headers
          company-cmake
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
          go-play
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
          spaceline

          ;; javascript setup from emacs.cafe Nicolas Petton
          company-tern
          js2-mode
          xref-js2

          ;; emacs goodies
          free-keys
          ido-vertical-mode
          ag
          exwm
          iflipb
	  kaolin-themes
          diminish

          ;; emacs next gen
          use-package
          general
          centaur-tabs
          treemacs
          flx
          swiper
          ivy
          ivy-hydra
          counsel
          hydra
          lsp-ui
          lsp-mode
          lsp-treemacs

          ;;
          git-gutter
          git-timemachine
          magit))
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
(use-package kaolin-themes
  :disabled
  :ensure t
  :config
  ;;(load-theme 'kaolin-bubblegum t)
  (kaolin-treemacs-theme))

(use-package monokai-pro-theme
  :disabled
  :ensure t
  :config
  (kaolin-treemacs-theme)
  (load-theme 'monokai-pro t))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))

(use-package dashboard
  :ensure t
  :config
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner "~/acme.png")
  (setq dashboard-banner-logo-title "Cogito, ergo sum")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

;; Load EXWM.
(defun es/set-up-gnome-desktop()
  "GNOME is used for mosto of the system settings."
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
  (start-process "" nil "/usr/lib/gnome-disk-utility/gsd-disk-utility-(not )otify")
  (start-process "" nil "/usr/bin/python3 /usr/bin/blueman-applet")
  (start-process "" nil "/usr/lib/x86_64-linux-gnu/indicator-messages/(insert )ndicator-messages-service")
  (start-process "" nil "/usr/lib/x86_64-linux-gnu/indicator-application/indicator-application-service")
;;  (start-process "" nil "zeitgeist-datahub")
  (start-process "" nil "update-notifier")
  (start-process "" nil "/usr/lib/deja-dup/deja-dup-monitor")

  (start-process "" nil "/usr/bin/nm-applet")
  (start-process "" nil "/usr/bin/blueman-applet")
  (start-process "" nil "/snap/bin/pa-applet")
  (start-process "" nil "/usr/bin/xset" "dpms" "120 300 600")
  (message "es/setup-up-gnome-desktop"))
(es/set-up-gnome-desktop)

(use-package exwm-config
  :disabled
  :init
  (exwm-config-default))

(use-package exwm
  :ensure t
  :pin gnu
  :init
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
  (setq exwm-workspace-number 10)

  ;; create space
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1)

  ;; applications
  (exwm-input-set-key (kbd "s-l") 'es/lock-screen)
  (exwm-input-set-key (kbd "s-g") 'es/app-browser)
  (exwm-input-set-key (kbd "s-t") 'es/app-terminal)
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-e") 'treemacs)
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
  (exwm-input-set-key (kbd "s-\\") 'split-window-horizontally)
  (exwm-input-set-key (kbd "s-]") 'split-window-vertically)
  (exwm-input-set-key (kbd "s-<backspace>") 'delete-window)
  (exwm-input-set-key (kbd "s-[") 'delete-other-windows)
  (exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)

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
  ;; Per host setup
  (setq exwm-randr-workspace-output-plist '(1 "eDP-1"))
  (when (string= system-name "faraz-dfn-x1")
    (progn
      (setq exwm-randr-workspace-output-plist '(0 "DP-1" 1 "eDP-1"))
      (add-hook 'exwm-randr-screen-change-hook
      (lambda ()
        (start-process-shell-command
         "xrandr" nil "xrandr --output HDMI-2 --output eDP-1 --auto")))))
  (exwm-randr-enable)
  (exwm-enable)

  ;; workaround to refresh screen size
  (exwm-workspace-delete)
  (exwm-workspace-add)
  (message "es/use-package/exwm"))



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

(defun centaur-tabs-group-by-custom ()
  "Custom grouping for Centaur tabs."
  (interactive)
  (setq centaur-tabs-buffer-groups-function 'centaur-tabs-custom-buffer-groups)
  (centaur-tabs-force-update))

(use-package centaur-tabs
  :demand
  :init (setq centaur-tabs-set-bar 'over)
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
  (centaur-tabs-group-by-custom)
  (message "es/setup-package-centaur-tabs")
  :bind
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode))


(use-package general
  :init
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

(use-package flx :ensure t)

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  (setq ivy-wrap t)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex)
          (t      . ivy--regex-plus))))

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
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
         ("C-x C-j" . counsel-fzf)
         ("s-d" . counsel-linux-app))
  :hook (window-setup . ivy-fix)
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))



(use-package whitespace
  :hook
  (prog-mode-hook . whitespace-mode)
  :init
  (setq whitespace-global-modes '(not exwm-mode treemacs-mode))
  :custom
  (whitespace-style (quote (face empty tabs lines-tail whitespace))))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (setq flycheck-global-modes '(not exwm-mode treemacs-mode)))

(use-package hydra :ensure t)

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
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))

(use-package magit
    :ensure t
    :init
    (progn
    (bind-key "C-x g" 'magit-status)
    ))


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
  :custom

  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook (((prog-mode) . 'display-line-numbers-mode)
         ((prog-mode) . lsp)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
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
        lsp-ui-flycheck-enable t
        lsp-file-watch-threshold 2000
        gc-cons-threshold 100000000)
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\result???$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\target???$")
  ;;WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;;https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))


(defun es/after-init-hook-windowsetup()
  "After init hook for setting up windows."
  (treemacs)
  (customize-set-variable
   'display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-same-window
                                  display-buffer-in-previous-window
                                  display-buffer-use-some-window)))

  ;; (customize-set-variable
  ;;  'display-buffer-base-action
  ;;  '((display-buffer-reuse-window display-buffer-pop-up-frame)
  ;;    (reusable-frames . 0)))
  (switch-to-buffer "*dashboard*")
  (delete-other-windows))


(use-package treemacs
  :ensure t
  :defer t
  :diminish
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
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
  :hook (after-init . es/after-init-hook-windowsetup)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))



(use-package unicode-fonts :ensure t)
(use-package all-the-icons-dired :ensure t)
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
  :config
  (use-package fancy-battery :ensure t
    :config
    (setq fancy-battery-show-percentage t)
    (fancy-battery-mode))
  (use-package spaceline-config
    :config
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-buffer-encoding-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (setq powerline-default-separator 'slant)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
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
    (spaceline-emacs-theme 'date 'time)))


(use-package auto-complete :ensure t)
(use-package auto-complete-config
  :disabled
  :requires auto-complete
  :ensure t
  )

(use-package company :ensure t
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
  (setq company-lsp-cache-cadidates 'auto))

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
  (setq elpy-rpc-python-command "python3"
        elpy-rpc-backend "jedi"
        jedi:setup-keys t                      ; optional
        jedi:complete-on-dot t
        python-python-command "/usr/bin/ipython"))

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
(use-package ggtags :ensure t  :diminish)
(use-package company-c-headers :ensure t :diminish)

(message "es/packages-loaded")
;; GO Lang


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup common variables across packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 3)
 '(c-echo-syntactic-information-p t)
 '(c-insert-tab-function (quote insert-tab))
 '(c-report-syntactic-errors t)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(custom-safe-themes
   (quote
    ("1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "9743d1941d0559264aa21a542e55043c032d779024cd70468d1123c320699fd1" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "d5d2ab76985738c142adbe6a35dc51c8d15baf612fdf6745c901856457650314" "f11e219c9d043cbd5f4b2e01713c2c24a948a98bed48828dc670bd64ae771aa1" "09cadcc2784baa744c6a7c5ebf2a30df59c275414768b0719b800cabd8d1b842" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "53993d7dc1db7619da530eb121aaae11c57eaf2a2d6476df4652e6f0bd1df740" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "855eb24c0ea67e3b64d5d07730b96908bac6f4cd1e5a5986493cbac45e9d9636" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" default)))
 '(dabbrev-case-fold-search nil)
 '(display-buffer-base-action
   (quote
    ((display-buffer-reuse-window display-buffer-same-window display-buffer-in-previous-window display-buffer-use-some-window))))
 '(global-hl-line-mode t)
 '(ido-mode t nil (ido))
 '(ido-vertical-define-keys (quote C-n-and-C-p-only))
 '(ido-vertical-mode 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(lsp-auto-guess-root nil)
 '(lsp-prefer-flymake nil)
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
 '(normal-erase-is-backspace-mode 0)
 '(package-selected-packages
   (quote
    (fancy-battery doome-themes doom-themes realgud page-break-lines quelpa-use-package elisp-cache dashboard clues-theme monokai-pro-theme spaceline-all-the-icons spaceline powerline-evil auto-complete auto-complete-c-headers auto-complete-chunk auto-complete-clang auto-complete-clang-async auto-complete-etags auto-complete-exuberant-ctags auto-complete-nxml company company-lsp company-quickhelp company-c-headers company-cmake company-irony company-irony-c-headers company-go company-jedi function-args irony irony-eldoc jedi elpy ggtags ac-racer flycheck-rust cargo yasnippet yasnippet-snippets yasnippet-classic-snippets go-autocomplete spacemacs-theme go-direx go-eldoc go-errcheck go-mode go-play go-projectile go-snippets go-stacktracer golint go-eldoc google-c-style flycheck flycheck-irony py-autopep8 powerline company-tern js2-mode xref-js2 free-keys ido-vertical-mode ag exwm iflipb kaolin-themes diminish use-package general centaur-tabs treemacs flx swiper ivy ivy-hydra counsel hydra lsp-ui lsp-mode lsp-treemacs git-gutter git-timemachine magit)))
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
 '(safe-local-variable-values (quote ((buffer-reado-only . t))))
 '(savehist-mode 1)
 '(scroll-step 1)
 '(set-fill-column 80)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(speedbar-default-position (quote left))
 '(standard-indent 3)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
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
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
(message "es/custom-set-faces")

;;;;;;;;;;;;;;;
;;Workarounds;;
;;;;;;;;;;;;;;;
(defun exwm-workspace-ui()
  (interactive)
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
    :type 'sexp)
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


;; Some custom configuration to ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-keep-variants nil)

(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
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

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)
(message "es/workarounds")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper/Utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

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
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb"
   nil
   "adb devices | head -n2  | tail -n1 | cut -f 1 | xargs -I{} adb -s {} reverse tcp:8081 tcp:8081"))

(defun es/adb-reload()
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb" nil "adb shell input keyevent R"))


(defun es/adb-shake()
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb" nil  "adb shell input keyevent 82"))
(message "es/helper-utilities")



;;;;;;;;;;;;;;;;;;;
;; Emacs email   ;;
;;;;;;;;;;;;;;;;;;;
;; Tools
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com"
                                   587
                                   "faraz@email.com"
                                   nil)))
(message "es/email")


;;;;;;;;;;;;;
;; GO LANG ;;
;;;;;;;;;;;;;
(require 'go-autocomplete)
(require 'auto-complete-config)
(defconst _goroot "/home/farazl/excubito_workspace/scratch/go/golang/go"  "golanguage root")
(defun ac-go-mode-setup()
  ;;(setenv "PATH" (concat (getenv "PATH") ":" (concat _goroot "/bin")))
  (local-set-key (kbd "M-.") 'godef-jump)
)

(setenv "GOPATH" (getenv "WRK"))
(defun go-set-gopath(_gopath)
  (interactive "Set Go PATH:")
  (setenv "GOPATH" _gopath)
  )

(require 'go-eldoc)
(add-hook 'go-mode-hook 'ac-go-mode-setup)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'ac-go-mode-setup)


;;;;;;;;;;;;;;;;
;; JavaScript ;;
;;;;;;;;;;;;;;;;
(require 'company)
(require 'company-tern)
(defun js2-mode-setup()
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






;;;;;;;;;;;;;;;
;;    rust   ;;
;;;;;;;;;;;;;;;
;;http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;;packages rustic cargo racer
(defun rustModeSetup()
  (interactive)
  ;; # cargo install rustfmt
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
  ;; # rustup default nightly
  ;; # cargo install racer
  ;; # rustup component add rust-src
  (ac-config-default)
  (ac-racer-setup)
  (auto-complete-mode t)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)

  ;; Company mode
  (add-hook 'racer-mode-hook 'company-mode)
  (add-hook 'racer-mode-hook 'company-quickhelp-mode)

  ;; Auto complete mode
  ;;(add-hook 'racer-mode-hook 'auto-complete-mode)
  ;;(add-hook 'racer-mode-hook 'ac-racer-setup)

  (add-hook 'racer-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  )
;;(rustModeSetup)

(put 'downcase-region 'disabled nil)
(message "es/legacy-lang-setup")

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




;; Application invocations
(defun find-named-buffer(buffPrefix)
  (interactive)
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
  (interactive)
  (setq browser-binary "/usr/bin/google-chrome")
  (setq browser-invocation (concat browser-binary))
  (setq browser-invocation (concat browser-invocation "  --app=http://netflix.com"))
  (message "Opening Web browser")
  (start-process-shell-command
   browser-binary nil  browser-invocation))


(defun es/app-browser()
  (interactive)
  (setq browser-bufname "Google-chrome")
  (setq browser-binary "/usr/bin/google-chrome")
  (setq browser-invocation (concat browser-binary))

  (setq browser (find-named-buffer browser-bufname))
  (if (eq browser nil)
      (progn
        (message "Opening Web browser")
        (start-process-shell-command
         browser-binary nil  browser-invocation))
    (progn
      (message "Web browser")
      (switch-to-buffer browser))
    ))


(defun es/app-terminal()
  "Find existing or open a new terminal window."
  (interactive)
  (setq term-bufname "Gnome-terminal")
  (setq terminal (find-named-buffer term-bufname))
  (if (eq terminal nil)
      (progn
        (message "Opening terminal")
        (es/app-terminal-new))
    (progn
      (message "terminal")
      (switch-to-buffer terminal))
    ))

(defun es/app-splash()
  (interactive)
  (setq term-bufname "feh")
  (setq term-binary "feh")
  (setq term-invocation (concat term-binary
                                " ~/wallpaper.jpg"
                                ))

  (setq terminal (find-named-buffer term-bufname))
  (if (eq terminal nil)
      (progn
        (message "Opening terminal")
        (start-process-shell-command
         term-binary nil term-invocation))
    (progn
      (message "terminal")
      (switch-to-buffer terminal))
    ))

(defun es/app-terminal-new()
  (interactive)
  (setq term-bufname "Gnome-terminal")
  (setq term-binary "/usr/bin/gnome-terminal")
  (setq term-invocation term-binary)

  (setq terminal (find-named-buffer term-bufname))
  (progn
    (message "Opening terminal")
    (start-process-shell-command
     term-binary nil term-invocation))
  )

(defun es/lock-screen()
  "Lock Screen command for es."
  (interactive)
  (start-process-shell-command
   "/usr/bin/gnome-screensaver-command" nil  "/usr/bin/gnome-screensaver-command -l"))


;; Swap monitors
(defun es/monitor-monitor-move-left()
  "Move primary monitor to left."
  (interactive)
  (setq commandMoveLeft
        "`xrandr  | grep -w connected | cut -f 1  -d  \" \"  | paste -s -d _ |  sed  's/_/ --left-of /;s/^/xrandr --output /'`")
  (shell-command commandMoveLeft))

(defun es/monitor-move-right()
  "Move primary monitor to right."
  (interactive)
  (setq commandMoveRight
        "`xrandr  | grep -w connected | cut -f 1  -d  \" \"  | paste -s -d _ |  sed  's/_/ --right-of /;s/^/xrandr --output /'`")
  (shell-command commandMoveRight))

;; Scale/Descale gdm
(defun es/gdm-set-scale(scale-factor)
  "Set gnome desktop scaling factor to SCALE-FACTOR."
  (interactive "scale-factor: ")
  (setq gsettings-binary "/usr/bin/gsettings")
  (setq gsettings-invocation
        (concat gsettings-binary  " set org.gnome.desktop.interface text-scaling-factor "  scale-factor))
  (message gsettings-invocation)
  (start-process-shell-command
   gsettings-binary nil gsettings-invocation)
  )

(defun es/gdm-tweaks()
  "Open gnome tweaks."
  (interactive)
  (start-process-shell-command
   "/usr/bin/gnome-tweaks" nil  "/usr/bin/gnome-tweaks"))


(defun es/ssh(hostName)
  (interactive "suserName@Host:")
  (setq ssh-bufname "XTerm")
  (setq ssh-binary "/usr/bin/xterm")
  (setq ssh-invocation (concat ssh-binary
                                " -bg black -fg white "
                                " -fa 'Ubuntu Mono'"
                                " -e 'ssh -Y " hostName "'"
                                ))
  (start-process-shell-command  ssh-binary nil ssh-invocation))


(defun es/workspace-setup()
  (interactive)
  ;; setup workspaces in addition to the 0th workspace
  (exwm-workspace-add)
  (start-process-shell-command "" nil "/usr/bin/slack")
  (exwm-workspace-add)
  (es/browser)
  (exwm-workspace-add)
  (es/app-terminal)
  (exwm-workspace-add))
(message "es/app-setup")

;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Setup  ;;
;;;;;;;;;;;;;;;;;;;;;
(defun es/input-decode-map-putty()
  (interactive)
  ;; keys for iterm2 - you have to edit the default keys for the profile for [option][direction]
  ;; xterm.defaults presets
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
  (interactive)
  ;; key bindinds based on xterm.defaullts presets set by iterm2
  (define-key input-decode-map "\e[1;5A" [(ctrl up)])
  (define-key input-decode-map "\e[1;5B" [(ctrl down)])
  (define-key input-decode-map "\e[1;5C" [(ctrl right)])
  (define-key input-decode-map "\e[1;5D" [(ctrl left)])

  (define-key input-decode-map "\e[1;3A" [(meta up)])
  (define-key input-decode-map "\e[1;3B" [(meta down)])
  (define-key input-decode-map "\e[1;3C" [(meta right)])
  (define-key input-decode-map "\e[1;3D" [(meta left)])
  )

(add-hook 'tty-setup-hook 'es/input-decode-map-xterm-compatibility)
(global-set-key [(meta up)] 'windmove-up)
(global-set-key [(meta down)] 'windmove-down)
(global-set-key [(meta right)] 'windmove-right)
(global-set-key [(meta left)] 'windmove-left)

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

(global-set-key (key "M-<left>") 'windmove-left)          ; move to left windnow
(global-set-key (key "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (key "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (key "M-<down>") 'windmove-down)          ; move to downer window
(global-set-key (key "C-<left>") 'backward-word)          ; move forward word
(global-set-key (key "C-<right>") 'forward-word)          ; move backward word
(global-set-key (key "C-<up>") 'backward-paragraph)       ; move forward word
(global-set-key (key "C-<down>") 'forward-paragraph)      ; move backward word
(global-set-key (kbd "<find>") 'beginning-of-line)        ; beginning of line
(define-key global-map [select] 'end-of-line)             ; end of line

(global-set-key (kbd "C-F") 'rgrep)
(global-set-key (kbd "C-f") 'ag)

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
;; (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

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
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))
(message "es/keyboard-setup")





;;;;;;;;;;;;;;;;;;;;;;
;;Setup alt-tab     ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'iflipb)
(setq iflipbTimerObj nil)
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
      nil t)
  )

(defun setupIFlipb()
  (interactive)
  (setq iflipb-wrap-around t)
  (setq iflipb-ignore-buffers 'iflipb-ignore-windowed-buffers)
  (setq iflipb-always-ignore-buffers "^[ *]")
  (global-set-key (kbd "<M-<tab>>") 'timed-iflipb-next-buffer)
  (global-set-key (kbd "<M-<iso-lefttab>") 'timed-iflipb-previous-buffer)
  (exwm-input-set-key (kbd "M-<tab>") 'timed-iflipb-next-buffer)
  (exwm-input-set-key (kbd "M-<iso-lefttab>") 'timed-iflipb-previous-buffer))

(setupIFlipb)
(message "es/alt-tab")

;; HELP DOCUMENTATION AND OTHER STUFF
(defvar emacsDesktopHelp
"
 .ooooo.  ooo. .oo.  .oo.    .oooo.    .ooooo.   .oooo.o
d88. .88b .888P.Y88bP.Y88b  .P  )88b  d88. ..Y8 d88(  .8
888ooo888  888   888   888   .oP.888  888       ..Y88b.
888    .o  888   888   888  d8(  888  888   .o8 o.  )88b
.Y8bod8P. o888o o888o o888o .Y888..8o .Y8bod8P. 8..888P.



      .o8                     oooo            .
     .888                     .888          .o8
 .oooo888   .ooooo.   .oooo.o  888  oooo  .o888oo  .ooooo.  oo.ooooo.
d88. .888  d88. .88b d88(  .8  888 .8P.     888   d88. .88b  888. .88b
888   888  888ooo888 ..Y88b.   888888.      888   888   888  888   888
888   888  888    .o o.  )88b  888 .88b.    888 . 888   888  888   888
.Y8bod88P. .Y8bod8P. 8..888P. o888o o888o   .888. .Y8bod8P.  888bod8P.
                                                             888
                                                            o888o

 Emacs Desktop Environment[EDE] Help.
 ....................................
 EDE is fully function emacs based desktop environment built using EXWM
 This provides a full fledged desktop experience for software development.
 EDE manages applications and emacs buffers using single window managment
 system. In a nutshell you can manage your application windows the same way
 as would manager your emacs window.

 Keys
 ....
 Win/Apple/SuperKey: This is windows keys or the apple command key.
 This will be referred to as the super key while describing key bindings

 Window Management:
 .................
 Window splitting and movement using emacs key bindings is available
 i.e. C-x-3 C-x-2 for horizontal and veritcal splitting and C-x-o for focus
 change works.

 In addition to this the following shortcuts are used.
 Super+|                             Split window vertically
 Super+]                             Split window horizontally
 Super+[                             Close all window except current
 Super+z                             Undo recent window managment action
 Ctrl-g   Super+z                    Redo recent window managment action
 Super+[Up/Down/Shift/Left]          Move focus to window above/below/left/right
                                     adjacent window
 Super+Shift+[[Up/Down/Shift/Left]]  Resize current window in the key direction
 Super+b                             Menu based application switching
 Alt-tab                             Windows style application switching


 Application Management:
 .......................
 S-d                                 Launch apps. Present menu lising all available apps
 C-x-k                               Kill buffer/application
 S-g                                 Find Or launch default browser (chromoim-browser)_
 S-t                                 Find Or launch a new terminal (xterm)
 S-l                                 Lock screen using default screen saver
 S-&                                 Run bash command

 Workspace (Multiple desktops)
 .............................
 S-w                                 Interactively Switch to workspace
 S-{0..9}                            Switch to workspace 0 ... 9


 Git Integration
 ...............
 Git diff and merge conflicts are trasparently handled withing the EDE editior
 ee  <filename> bash alias  open file in current emacs desktop

 Programming Language Suport
 ............................
 C
 C++                Using irony
 Python             Using Elpy
 Rust               Using rustic
 Golang
 JavaScript/Node    Using tern

 Monitor Support:
 ................
 Auto-detects monitor resolution aka. retina/HD/UHD and does scaling for app
 Working for 13inch, 15inch Macbook Pro Retina
 1440p and 4K monitors

 One can over ride detected Monitor Setting by calling the interactive
 functions
 Alt-x RetinaSetup
 Alt-x MonitorSetup

 Miscellaneous
 -------------
 Alt-x GetToNetflix                   Start Netflix using chromium HTML-5
 Alt-x GetToSplash                    Splash Screen
 Alt-x LockScreen
 Alt-x MonitorMoveLeft                Re-positon dual monitor setups
 Alt-x MonitorMoveRight               Re-positon dual monitor setups
 Alt-x ssh                            Open up secure-shell (interactive)
")


(defun  EmacsDesktopGetSplash ()
  (with-current-buffer
      (get-buffer-create "EmacsDesktopSplash")
    (insert emacsDesktopHelp)
    (goto-char (point-min)))
  (get-buffer-create "EmacsDesktopSplash"))

;;(setf initial-buffer-choice 'EmacsDesktopGetSplash)

(server-start)
(message "!!!es/load-complete!!!")
(provide '.emacs)
;;;
