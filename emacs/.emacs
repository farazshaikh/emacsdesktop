;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No point in supporting multiple version, there is way to much work needed for that ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (version< emacs-version  "24.4")
 (error "Script depends on emacs version being greater than 24.4")
 (message "Version greater or equal to 24.4"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Package Managment system Initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)
;;(setq package-check-signature nil)
(setq inverse-video t)


;; check up and set installation mode.
(defvar eosinstallation nil "Are we installing Emacs os ?")
(when (or (member "-eosinstall" command-line-args)
          (eq package-archive-contents nil)
          (not (file-exists-p "~/.eosinstall")))
  (progn
    (setq eosinstallation t)
    (message "emacs in running in installation mode")))

;; eat up the command line args in the end
(defun eosinstall-fn (switch)
  (message "emacs running in eosinstall mode")
  (setq eosinstallation t)
  )
(add-to-list 'command-switch-alist '("-eosinstall" . eosinstall-fn))

;; install packages
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto install packages if not installed ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list the packages you want
;; basically all the packages needs for auto-complete of common languages C/C++/Python/JS
;; Auto-complete used to be my preferred package for completions - Now moving to Company
(setq package-list '(;; Auto complete and IT's backends
                     auto-complete
                     auto-complete-c-headers
                     auto-complete-chunk
                     auto-complete-clang
                     auto-complete-clang-async
                     auto-complete-etags
                     auto-complete-exuberant-ctags
                     auto-complete-nxml

                     ;; COMPANY & ITS BACKENDS
                     company
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
                     powerline

                     ;; javascript setup from emacs.cafe Nicolas Petton
                     company-tern
                     js2-mode
                     xref-js2

                     ;; emacs goodies
                     free-keys
                     ido-vertical-mode
                     ag
                     exwm
                     dmenu
                     iflipb

                     ;; emacs next gen
                     use-package
                     general
                     centaur-tabs
                     treemacs
                     swiper
                     ivy
                     counsel
                     hydra
                     lsp-ui
                     lsp-mode
                     lsp-treemacs

                     ;;
                     git-gutter
                     git-timemachine
                     magit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install the missing packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when
    (eq eosinstallation t)
  (progn (message "refreshing package list")
         (package-refresh-contents)
         ;; required for irony mode
         (shell-command "apt install cmake libclang-dev")
         (dolist (package package-list)
           (unless (package-installed-p package)
             (package-install package)))
         (write-region "" "" "~/.eosinstall")
	 ;;(require 'irony)
	 ;;(irony-mode t)
	 ;;(irony-install-server)
         )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Specific Setup                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SetupProjectDFN()
  (interactive)
  (setenv "RUST_SRC_PATH" "/home/emacs/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")
  (setenv "WRK" (concat (concat "/home/" (getenv "USER") "/dfn/dfinity/.")))
  (setq compile-command
"cd $WRK/rs;\n\
 source ~/.nix-profile/etc/profile.d/nix.sh;\n \
 nix-shell --run \"cargo build\" &&\n \
 sudo cp ./target/debug/client ~/.cache/dfinity/versions/0.4.7/")
)


(defun SetupProjectSP()
  (interactive)
  (setenv "WRK" "/storvisor/work/cypress")
  (setq compile-command
   "cd $WRK; source ./setvars.sh debug; DBUILDCMD=\"make -j32 BUILDTYPE=debug\" ./docker/build_template/build.sh  buildcmd")
)


(defun SetupProjectEXCB()
  (interactive)
  (setenv "WRK" (concat (concat "/home/" (getenv "USER") "/excubito_workspace/hazen/.")))
)

(SetupProjectDFN)


;;;;;;;;;;;;;;;;;;;;;;;
;; Centaur Tabs      ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun centaur-tabs-custom-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
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
    (t "Editing")
    )))

(defun centaur-tabs-group-by-custom ()
  "Custom grouping for Centaur tabs"
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
        centaur-tabs-height 30
        centaur-tabs-set-icons t
        centaur-tabs-close-button " × "
        centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-group-by-custom)
  :bind
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode))

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))



;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages ;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package general
  :init
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))


(use-package counsel
:ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;; (with-eval-after-load 'ivy
;;   (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
;;         ivy-re-builders-alist)
;;   (push (cons t #'ivy--regex-fuzzy) ivy-re-builders-alist))


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

)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
         ("C-x C-j" . counsel-fzf))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))


(use-package whitespace
  :hook (prog-mode-hook . whitespace-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))


(use-package hydra
  :ensure t)

(use-package git-gutter
  :diminish
  :hook (after-init . global-git-gutter-mode)
  :init (setq git-gutter:visual-line t
              git-gutter:disabled-modes '(asm-mode image-mode)
              git-gutter:modified-sign "*"
              git-gutter:added-sign "✚"
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

(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet-classic-snippets
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((prog-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-glance t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;;  Use lsp-ui-doc-webkit only in GUI
  (setq lsp-ui-doc-use-webkit nil)
  ;;WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;;https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))


(use-package treemacs
  :ensure t
  :defer t
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
          treemacs-indentation                   2
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
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))



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
 '(company-idle-delay 0)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0)
 '(compilation-scroll-output (quote first-error))
 '(custom-safe-themes
   (quote
    ("f0a76ae259b7be77e59f98501957eb45a10af0839dd9eb29fdd5691ed74771d4" "ed573618e4c25fa441f12cbbb786fb56d918f216ae4a895ca1c74f34a19cfe67" "58c2c8cc4473c5973e77f4b78a68c0978e68f1ddeb7a1eb34456fce8450be497" "f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" "2f945b8cbfdd750aeb82c8afb3753ebf76a1c30c2b368d9d1f13ca3cc674c7bc" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "a7928e99b48819aac3203355cbffac9b825df50d2b3347ceeec1e7f6b592c647" "3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" default)))
 '(dabbrev-case-fold-search nil)
 '(dmenu-prompt-string "Run App: ")
 '(global-hl-line-mode t)
 '(ido-mode t nil (ido))
 '(ido-vertical-define-keys (quote C-n-and-C-p-only))
 '(ido-vertical-mode 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(load-home-init-file t t)
 '(lsp-auto-guess-root nil)
 '(lsp-prefer-flymake nil)
 '(lsp-ui-doc-border "white" t)
 '(lsp-ui-doc-enable t t)
 '(lsp-ui-doc-glance t t)
 '(lsp-ui-doc-header t t)
 '(lsp-ui-doc-include-signature t t)
 '(lsp-ui-doc-position (quote at-point) t)
 '(lsp-ui-sideline-enable nil t)
 '(lsp-ui-sideline-ignore-duplicate t t)
 '(lsp-ui-sideline-show-code-actions nil t)
 '(package-selected-packages
   (quote
    (kaolin-themes lsp-treemacs lsp-ui magit git-timemachine hydra general git-gutter swiper ivy centaur-tabs adoc-mode ac-racer clippy eldoc-overlay function-args company-tern ac-js2 js2-mode tern react-snippets flycheck-rust cargo racer rustic xclip powerline dmenu iflipb smart-mode-line mode-line-bell free-keys ag yasnippet-snippets yasnippet-classic-snippets py-autopep8 jedi google-c-style golint go-stacktracer go-snippets go-projectile go-play go-errcheck go-direx go-autocomplete flycheck elpy edebug-x company-irony-c-headers company-irony cmake-mode auto-complete-nxml auto-complete-exuberant-ctags auto-complete-etags auto-complete-clang-async auto-complete-clang auto-complete-chunk auto-complete-c-headers)))
 '(python-python-command "/usr/bin/ipython")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workaround auto complete and whitespace show YELLOW/RED boxes								     ;;
;; https://stackoverflow.com/questions/12965814/emacs-how-can-i-eliminate-whitespace-mode-in-auto-complete-pop-ups/27960576#27960576 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:force-modes (rule-mode &rest modes)
    "switch on/off several modes depending of state of
    the controlling minor mode
  "
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
  (if my:prev-whitespace-pushed () (progn
                                     (setq my:prev-whitespace-mode whitespace-mode)
                                     (setq my:prev-whitespace-pushed t)
                                     (my:force-modes nil 'whitespace-mode)
                                     ))
  )

(defun my:pop-whitespace (&rest skip)
  (if my:prev-whitespace-pushed (progn
                                  (setq my:prev-whitespace-pushed nil)
                                  (my:force-modes my:prev-whitespace-mode 'whitespace-mode)
                                  ))
  )

(require 'popup)
(advice-add 'popup-draw :before #'my:push-whitespace)
(advice-add 'popup-delete :after #'my:pop-whitespace)
;; End workaround auto complete and whitespace



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper/Utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compileloop ()
  (interactive)
  (setq compilation-scroll-output t)
  (setq compilation-finish-function
        (lambda (buffer msg) (compile-internal compile-command "No more errors")))
  (compile-internal compile-command "No more errors.")
  ;;(compile compile-command)
  ;;(shell-command "/bin/bash /home/faraz-local-home/compile-loop.sh &")
  ;;(shell-command "ls")
)


(defun compile2 ()
  (interactive)
  (setq compilation-scroll-output t)
  (setq compilation-finish-function
        (lambda (buffer msg)))
  (compile compile-command)
  ;;(shell-command "/bin/bash /home/faraz-local-home/compile-loop.sh &")
  ;;(shell-command "ls")
)


(defun toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun tags-create (dir-name)
     "Create tags file."
     (interactive "DDirectory: ")
     (eshell-command
      (format "find %s -type f -name \"*.hpp\" -o -name \"*.cpp\" -o -name \"*.[ch]\" | xargs etags -f %s/TAGS" dir-name dir-name))
     (eshell-command
      (format "cd %s; gtags -i -q" dir-name))
)

(defun copy-rectangle-as-kill ()
    (interactive)
    (save-excursion
    (kill-rectangle (mark) (point))
    (exchange-point-and-mark)
    (yank-rectangle)))


;;;;;;;;;;;;;;;;;;;
;; Coloring      ;;
;;;;;;;;;;;;;;;;;;;
;; ediff
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:foreground "white" :background "cornflower blue"))))
 '(company-scrollbar-bg ((t (:background "cornflower blue"))))
 '(company-scrollbar-fg ((t (:background "white"))))
 '(company-tooltip ((t (:background "cornflower blue" :foreground "white"))))
 '(company-tooltip-annotation ((t (:foreground "white"))))
 '(company-tooltip-selection ((t (:foreground "white" :background "black" :box (:line-width 1 :color "white")))))
 '(ediff-current-diff-A ((((class color)) (:background "blue" :foreground "white"))))
 '(ediff-current-diff-B ((((class color)) (:background "blue" :foreground "white" :weight bold))))
 '(ediff-current-diff-C ((((class color)) (:background "yellow3" :foreground "black" :weight bold))))
 '(ediff-even-diff-Ancestor ((((class color)) (:background "light grey" :foreground "black" :weight bold))))
 '(ediff-even-diff-C ((((class color)) (:background "light grey" :foreground "black" :weight bold))))
 '(ediff-fine-diff-B ((((class color)) (:background "cyan3" :foreground "black"))))
 '(ediff-fine-diff-C ((((class color)) (:background "Turquoise" :foreground "black" :weight bold))))
 '(hl-line ((t (:weight extra-bold))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(mode-line ((t (:background "cornflower blue" :foreground "white" :box (:line-width 1 :color "white") :height 0.9))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((t (:box (:line-width 1 :color "white")))))
 '(mode-line-inactive ((t (:background "grey" :foreground "black" :height 0.9)))))


(defun mark-directory-readonly(name)
  "Mark a directory to be opened readonly under emacs"
  (interactive "sDirectory Name:")
  (setq name (concat name "/./.dir-locals.el"))
  (message (concat "Created : " name))
  (unless (file-exists-p name)
    (progn
      (write-region "" nil name)
      (f-append-text "((nil . ((buffer-reado-only . t))))" 'utf-8 name))
    )
)



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

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Auto Complete and browsing  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compilation buffer colorize
(when (require 'ansi-color nil t)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))


;;;;;;;;;;;;;;;
;;    python ;;
;;;;;;;;;;;;;;;
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq elpy-rpc-backend "jedi")
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;;;;;;;;;;;;;;;;;;;
;; terminal mode ;;
;;;;;;;;;;;;;;;;;;;
(add-hook 'ansi-term-mode-hook '(lambda ()
      (setq term-buffer-maximum-size 0)
      (setq-default show-trailing-whitespace f)
))


;;;;;;;;;;;
;; C C++ ;;
;;;;;;;;;;;
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-clang-async)

(require 'company)
(require 'company-c-headers)
(require 'company-irony)

(require 'function-args)
(fa-config-default)

(require 'flycheck-irony)
(require 'ggtags)
(add-hook 'compilation-mode '(lamda ()
      (next-error-follow-minor-mode t)
      ))

;; System includes       ;;
(defcustom mycustom-system-include-paths
  '(
    "/usr/include/c++/5"
    "/usr/include/x86_64-linux-gnu/c++/5"
    "/usr/include/c++/5/backward"
    "/usr/lib/gcc/x86_64-linux-gnu/5/include"
    "/usr/local/include"
    "/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed"
    "/usr/include/x86_64-linux-gnu"
    "/usr/include"
    )
  "system list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
  )

(setq projectPath2 (getenv "WRK"))
(defcustom project-include-paths
           '("./"
             "./include/"
             "/work//include/"
           )
  "project list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
  )

;; Clang includes        ;;
(setq clangincludes project-include-paths)
(setq clangincludes (append clangincludes mycustom-system-include-paths))
(defcustom clangincludes clangincludes
  "This is a list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
)


(setq ccppCompletions "ironycompany")
;;(setq ccppCompletions "clangac")



;; IRONY and company based c/c++ completions
(defun irony--check-expansion ()
  (save-excursion (if (looking-at "\\_>") t
                    (backward-char 1)
                    (if (looking-at "\\.") t
                      (backward-char 1)
                      (if (looking-at "->") t nil)))))
(defun irony--indent-or-complete ()
  "Indent or Complete" (interactive)
  (cond ((and (not (use-region-p)) (irony--check-expansion))
         (message "complete") (company-complete-common))
        (t (message "indent") (call-interactively 'c-indent-line-or-region))))
(defun irony-mode-keys () "Modify keymaps used by `irony-mode'."
       (local-set-key (kbd "TAB") 'irony--indent-or-complete)
       (local-set-key [tab] 'irony--indent-or-complete))

(defun ccppIronySetup ()
  (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
  (eval-after-load 'company  '(add-to-list 'company-backends 'company-irony))
  (add-hook 'irony-mode-hook 'irony-eldoc)

  (flycheck-mode)
  (company-mode t)
  (irony-mode t)
  (ggtags-mode)


  (flyspell-prog-mode)
  (yas-minor-mode)
  (irony-cdb-autosetup-compile-options)

  (irony-mode-keys)
)

(defun ccppClangAsyncSetup ()
  (ggtags-mode t)
  (flyspell-prog-mode)
  (yas-minor-mode)
  (auto-complete-mode t)

  (add-to-list 'ac-clang-cflags " -std=c++11")
  (setq ac-clang-cflags
      (mapcar (lambda (item)(concat "-I" item))
              (append
               clangincludes
               )
              )
      )
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (ac-clang-launch-completion-process)

  ;; ac-omni-completion-sources is made buffer local so
  ;; you need to add it to a mode hook to activate on
  ;; whatever buffer you want to use it with.  This
  ;; example uses C mode (as you probably surmised).
  ;; auto-complete.el expects ac-omni-completion-sources to be
  ;; a list of cons cells where each cell's car is a regex
  ;; that describes the syntactical bits you want AutoComplete
  ;; to be aware of. The cdr of each cell is the source that will
  ;; supply the completion data.  The following tells autocomplete
  ;; to begin completion when you type in a . or a ->
  (add-to-list 'ac-omni-completion-sources
               (cons "\\." '(ac-source-gtags)))
  (add-to-list 'ac-omni-completion-sources
               (cons "->" '(ac-source-gtags)))
  ;; ac-sources was also made buffer local in new versions of
  ;; autocomplete.  In my case, I want AutoComplete to use
  ;; semantic and yasnippet (order matters, if reversed snippets
  ;; will appear before semantic tag completions).
  (setq ac-sources
        (append
         '(ac-source-semantic ac-source-yasnippet ac-source-clang-async)
         ac-sources))
  )

(if (string= ccppCompletions "ironycompany")
    (progn (add-hook 'c++-mode-hook 'ccppIronySetup)
           (add-hook 'c-mode-hook 'ccppIronySetup)
           (message "using irony company for c/c++ completions"))
  (progn
    (ac-config-default)
    (auto-complete-mode t)
    (add-hook 'c++-mode-hook 'ccppClangAsyncSetup)
    (add-hook 'c-mode-hook 'ccppClangAsyncSetup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (message "using clang-async autocomplete for c/c++ completions")))

(put 'erase-buffer 'disabled nil)


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

(defun adbReverse()
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb"
   nil
   "adb devices | head -n2  | tail -n1 | cut -f 1 | xargs -I{} adb -s {} reverse tcp:8081 tcp:8081"))



(defun adbReload()
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb" nil "adb shell input keyevent R"))


(defun adbShake()
  (interactive)
  (start-process-shell-command
   "/usr/bin/adb" nil  "adb shell input keyevent 82"))


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
(rustModeSetup)



(put 'downcase-region 'disabled nil)


;;;;;;;;;;;;;;;
;; EXWM      ;;
;;;;;;;;;;;;;;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)



(defun ShowTime()
;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode t)
(defface egoge-display-time
   '((((type x w32 mac))
      ;; #060525 is the background colour of my default face.
      (:foreground "white" :inherit bold))
     (((type tty))
      (:foreground "white")))
   "Face used to display the time in the mode line.")
 ;; This causes the current time in the mode line to be displayed in
 ;; `egoge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat dayname "-" monthname "-" day " " 12-hours ":" minutes " " am-pm)
                    'face 'egoge-display-time))))
(ShowTime)
;;(powerline-center-theme)



;; Load EXWM.
(require 'exwm)

;; Fix problems with Ido (if you use it).
(require 'exwm-config)
(exwm-config-ido)


;;;;;;;;;;;;;;;;;;;;;;;
;; KeyBoard Mappings ;;
;; put this after    ;;
;; exwm init so that ;;
;; super becomes     ;;
;; accessible        ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun input_decode_map_putty()
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

(defun input_decode_map_xterm_compatibility()
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

(add-hook 'tty-setup-hook 'input_decode_map_xterm_compatibility)




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
  "An assoc list of pretty key strings
and their terminal equivalents.")

(defun key (desc)
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


;; backspace issues, toggle to resolve backspace issue
(normal-erase-is-backspace-mode 0)
(global-set-key (kbd "C-F") 'rgrep)
(global-set-key (kbd "C-f") 'ag)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)



;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

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

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;; (setq exwm-workspace-minibuffer-position 'top)




;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(1 "Virtual1"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output DP-1 --right-of DP-2 --auto")))
(exwm-randr-enable)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(exwm-enable)

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
  named-buffer
  )



;; Application Specific setting for scaling etc
(cl-defstruct appSettings
  emacsAttributeFaceHeight
  browserScalingFactor
  terminalFontSize)

(setq RetinaAppSettings (make-appSettings
                        :emacsAttributeFaceHeight 100
                        :browserScalingFactor "1.5"
                        :terminalFontSize "10"))

(setq MonitorAppSettings (make-appSettings
                          :emacsAttributeFaceHeight 60
                          :browserScalingFactor "1"
                          :terminalFontSize "6"))

(setq NoAppSettings (make-appSettings
                     :emacsAttributeFaceHeight 100
                     :browserScalingFactor "1"
                     :terminalFontSize "15"))


(defun ApplyAppSettings(applySettings)
  ;; emacs
  ;;(set-face-attribute 'default nil :font "Ubuntu Mono")
  ;;(set-face-attribute 'default nil :height (appSettings-emacsAttributeFaceHeight applySettings))
    ;; browser
  (setq browserScaleFactor (appSettings-browserScalingFactor  applySettings))
    ;; terminal
  (setq terminalFontSize (appSettings-terminalFontSize  applySettings))
)

(defun MonitorSetup()
  (interactive)
  (ApplyAppSettings MonitorAppSettings))


(defun RetinaSetup()
  (interactive)
  (ApplyAppSettings RetinaAppSettings))

(defun NoSetup()
  (interactive)
  (ApplyAppSettings NoAppSettings))

(defun GuessMonitorSetup()
  (setq monitorResolutionThreshold 2000)   ;; beyond which we consider the display to be a monitor
  (setq displayResolutionX
        (shell-command-to-string "xrandr  -q | grep current | cut -f 2 -d ',' | cut -f 3 -d' '"))
  (setq displayResolutionY
        (shell-command-to-string "xrandr  -q | grep current | cut -f 2 -d ',' | cut -f 5 -d' '"))
  (message (concat displayResolutionX "*" displayResolutionY))
  (if (> (string-to-number displayResolutionX) monitorResolutionThreshold)
      (MonitorSetup)
    (RetinaSetup)
    )
  )

;;(GuessMonitorSetup)
(NoSetup)

;;(setenv "GDK_SCALE" "0.5")
;;(setenv "GDK_DPI_SCALE" "0.5")

;; GNOME is used for mosto of the system settings
(setenv "XDG_CURRENT_DESKTOP" "GNOME")
(start-process "" nil "/usr/lib/gnome-settings-daemon/gsd-xsettings")
(start-process "" nil "/usr/lib/gnome-settings-daemon/gnome-settings-daemon")
(start-process "" nil "/usr/bin/nm-applet")
(start-process "" nil "/usr/bin/blueman-applet")
(start-process "" nil "/snap/bin/pa-applet")
(start-process "" nil "/usr/bin/redshift-gtk")
(start-process "" nil "/usr/bin/xset" "dpms" "120 300 600")

;; Application invocations
(defun GetToNetflix()
  (interactive)
  (setq browser-binary "/usr/bin/google-chrome")
  (setq browser-invocation (concat browser-binary))
  (setq browser-invocation (concat browser-invocation "  --app=http://netflix.com"))
  (message "Opening Web browser")
  (start-process-shell-command
   browser-binary nil  browser-invocation))


(defun GetToBrowser()
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


(defun GetToTerminal()
  (setq term-bufname "Gnome-terminal")
  (interactive)
  (setq terminal (find-named-buffer term-bufname))
  (if (eq terminal nil)
      (progn
        (message "Opening terminal")
        (NewTerminal))
  (progn
    (message "terminal")
    (switch-to-buffer terminal))
  ))

(defun GetToSplash()
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

(defun NewTerminal()
  (interactive)
  ;;  (setq term-bufname "XTerm")
  ;;  (setq term-binary "/usr/bin/xterm")
  ;;  (setq term-bufname "XTerm")
  ;;  (setq term-binary "/usr/bin/uxterm")
  ;;  (setq term-invocation (concat term-binary
  ;;                                " -bg black -fg white "
  ;;                                 " -fa 'Ubuntu Mono' -fs " terminalFontSize
  ;;                                ;;" -e 'screen -DR'"
  ;;                                ))

 (setq term-bufname "Gnome-terminal")
 (setq term-binary "/usr/bin/gnome-terminal")
 (setq term-invocation term-binary)

  (setq terminal (find-named-buffer term-bufname))
  (progn
      (message "Opening terminal")
      (start-process-shell-command
        term-binary nil term-invocation))
  )



(defun LockScreen()
  (interactive)
  (start-process-shell-command
   "/usr/bin/slock" nil  "/usr/bin/slock"))



;; Swap monitors
(defun MonitorMoveLeft()
  (interactive)
  (setq commandMoveLeft
        "`xrandr  | grep -w connected | cut -f 1  -d  \" \"  | paste -s -d _ |  sed  's/_/ --left-of /;s/^/xrandr --output /'`")
  (shell-command commandMoveLeft))

(defun MonitorMoveRight()
  (interactive)
  (setq commandMoveRight
        "`xrandr  | grep -w connected | cut -f 1  -d  \" \"  | paste -s -d _ |  sed  's/_/ --right-of /;s/^/xrandr --output /'`")
  (shell-command commandMoveRight))

;; Scale/Descale gdm
(defun gdm-set-scale(scale-factor)
  (interactive "scale-factor: ")
  (setq gsettings-binary "/usr/bin/gsettings")
  (setq gsettings-invocation
        (concat gsettings-binary  " set org.gnome.desktop.interface text-scaling-factor "  scale-factor))
  (message gsettings-invocation)
  (start-process-shell-command
   gsettings-binary nil gsettings-invocation)
  )
;;(gdm-set-scale "1")

(defun ssh(hostName)
  (interactive "suserName@Host:")
  (setq ssh-bufname "XTerm")
  (setq ssh-binary "/usr/bin/xterm")
  (setq ssh-invocation (concat ssh-binary
                                " -bg black -fg white "
                                " -fa 'Ubuntu Mono'"
                                " -e 'ssh -Y " hostName "'"
                                ))
  (start-process-shell-command
   ssh-binary nil ssh-invocation))



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




;; i3 like window mgmt
(defun i3WindowMgmtKeys ()
  (interactive)

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
                       (start-process-shell-command command nil command)))
          ))

  ;;(setq exwm-workspace-show-all-buffers t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Shortcuts to programs using Super Key
  (global-set-key (key "s-g") 'GetToBrowser)
  (exwm-input-set-key (kbd "s-g") 'GetToBrowser)


  ;; Shortcuts to programs using Super Key
  (global-set-key (key "s-t") 'GetToTerminal)
  (exwm-input-set-key (kbd "s-t") 'GetToTerminal)

  (exwm-input-set-key (kbd "s-l") 'LockScreen)
  (global-set-key (kbd "s-l") 'LockScreen)

  (exwm-input-set-key (kbd "s-d") 'dmenu)
  (global-set-key (kbd "s-d") 'dmenu)

  (exwm-input-set-key (kbd "s-r") 'exwm-reset)
  (global-set-key (kbd "s-r") 'exwm-reset)

  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (global-set-key (kbd "s-w") 'exwm-workspace-switch)

;;  (exwm-input-set-key (kbd "s-e") 'neotree-toggle)
;;  (global-set-key (kbd "s-e") 'neotree-toggle)

  ;; Window Mgmt using Emacs style Alt-Key
  ;; window move
  (exwm-input-set-key (kbd "s-<left>") 'windmove-left)
  (exwm-input-set-key (kbd "s-<down>") 'windmove-down)
  (exwm-input-set-key (kbd "s-<up>") 'windmove-up)
  (exwm-input-set-key (kbd "s-<right>") 'windmove-right)

  ;; window size
  (exwm-input-set-key (kbd "s-S-<right>")
                      (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 50)))
  (exwm-input-set-key (kbd "s-S-<left>")
                      (lambda () (interactive) (exwm-layout-shrink-window-horizontally 50)))
  (exwm-input-set-key (kbd "s-S-<up>")
                      (lambda () (interactive) (exwm-layout-enlarge-window             50)))
  (exwm-input-set-key (kbd "s-S-<down>")
                      (lambda () (interactive) (exwm-layout-shrink-window              50)))

  (global-set-key (kbd "s-S-<right>")
                      (lambda () (interactive) (enlarge-window-horizontally 5)))
  (global-set-key (kbd "s-S-<left>")
                      (lambda () (interactive) (shrink-window-horizontally 5)))
  (global-set-key (kbd "s-S-<up>")
                      (lambda () (interactive) (enlarge-window             5)))
  (global-set-key (kbd "s-S-<down>")
                      (lambda () (interactive) (shrink-window              5)))


  ;; window splits
  (exwm-input-set-key (kbd "s-\\") 'split-window-horizontally)
  (exwm-input-set-key (kbd "s-]") 'split-window-vertically)
  (exwm-input-set-key (kbd "s-<backspace>") 'delete-window)
  (exwm-input-set-key (kbd "s-[") 'delete-other-windows)
  (exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)

  ;; window undo
  (exwm-input-set-key (kbd "s-u") 'winner-undo)
  (global-set-key (kbd "s-u") 'winner-undo)


  (exwm-input-set-key (kbd "s-k") 'exwm-input-release-keyboard)
  (exwm-input-set-key (kbd "s-j") 'exwm-input-grab-keyboard)
  )

(i3WindowMgmtKeys)


(defun SetupWorkSpace()
  (interactive)
  ;; setup workspaces in addition to the 0th workspace
  (exwm-workspace-add)
  (start-process-shell-command "" nil "/usr/bin/slack")
  (exwm-workspace-add)
  (GetToBrowser)
  (exwm-workspace-add)
  (NewTerminal)
  (exwm-workspace-add)
  )
;;(SetupWorkSpace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open any startup apps     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(GetToTerminal)
;;(GetToBrowser)
;;(GetToSplash)

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
  "Determines whether this is the first invocation of
  iflipb-next-buffer or iflipb-previous-buffer this round."
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
(server-start)
(treemacs)




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
  (get-buffer-create "EmacsDesktopSplash")
)

;;(setf initial-buffer-choice 'EmacsDesktopGetSplash)
