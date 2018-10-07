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
(setq inverse-video t)


;; check up and set installation mode.
(defvar frinstallation nil "are we installing fremacs")
(when (or (member "-frinstall" command-line-args)
          (eq package-archive-contents nil))
  (progn
    (setq frinstallation t)
    (message "emacs in running in installation mode")))

;; eat up the command line args in the end
(defun frinstall-fn (switch)
  (message "emacs running in frinstall mode")
  (setq frinstallation t)
  )
(add-to-list 'command-switch-alist '("-frinstall" . frinstall-fn))

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
                     company-c-headers
                     company-cmake
                     company-irony
                     company-irony-c-headers
                     company-go
                     company-jedi

                     ;; Completion engines
                     irony
                     irony-eldoc
                     jedi
                     elpy
                     ggtags

                     ;; Snippets
                     yasnippet-classic-snippets
                     yasnippet-snippets

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

                     ;; emacs goodies
                     free-keys
                     ido-vertical-mode
                     ag
                     exwm
                     dmenu
                     iflipb

                     ;; UI
                     spacemacs-theme))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install the missing packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when
    (eq frinstallation t)
  (progn (message "refreshing package list")
         (package-refresh-contents)
         ;; required for irony mode
         (shell-command "apt install cmake libclang-dev")
         (dolist (package package-list)
           (unless (package-installed-p package)
             (package-install package)))
	 ;;(require 'irony)
	 ;;(irony-mode t)
	 ;;(irony-install-server)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup common variables across packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-echo-syntactic-information-p t)
 '(c-insert-tab-function (quote insert-tab))
 '(c-report-syntactic-errors t)
 '(column-number-mode t)
 '(company-tooltip-align-annotations t)
 '(compilation-scroll-output (quote first-error))
 '(compile-command
   "cd $WRK; source ./setvars.sh debug; DBUILDCMD=\"make -j32 BUILDTYPE=debug\" ./docker/build_template/build.sh  buildcmd")
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(dabbrev-case-fold-search nil)
 '(dmenu-prompt-string "Run App: ")
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(ido-mode t nil (ido))
 '(ido-vertical-define-keys (quote C-n-and-C-p-only))
 '(ido-vertical-mode 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(load-home-init-file t t)
 '(package-selected-packages
   (quote
    (xclip powerline dmenu iflipb smart-mode-line mode-line-bell free-keys ag yasnippet-snippets yasnippet-classic-snippets spacemacs-theme py-autopep8 jedi google-c-style golint go-stacktracer go-snippets go-projectile go-play go-errcheck go-direx go-autocomplete flycheck elpy edebug-x company-irony-c-headers company-irony cmake-mode auto-complete-nxml auto-complete-exuberant-ctags auto-complete-etags auto-complete-clang-async auto-complete-clang auto-complete-chunk auto-complete-c-headers)))
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

;; theme
;;(load-theme 'spacemacs-dark)
(setenv "WRK" (concat (concat "/home/" (getenv "USER") "/excubito_workspace/hazen/.")))
(setenv "WRK" "/storvisor/work/cypress")



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

(require 'whitespace)
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
 '(company-echo-common ((t (:foreground "firebrick"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "navy"))))
 '(company-scrollbar-bg ((t (:background "gray"))))
 '(company-scrollbar-fg ((t (:background "dark blue"))))
 '(company-template-field ((t (:background "dark blue" :foreground "white"))))
 '(company-tooltip ((t (:background "gray" :foreground "black"))))
 '(company-tooltip-annotation ((t (:foreground "black"))))
 '(company-tooltip-common ((t (:foreground "black" :slant italic))))
 '(company-tooltip-selection ((t (:background "blue"))))
 '(ediff-current-diff-A ((((class color)) (:background "blue" :foreground "white"))))
 '(ediff-current-diff-B ((((class color)) (:background "blue" :foreground "white" :weight bold))))
 '(ediff-current-diff-C ((((class color)) (:background "yellow3" :foreground "black" :weight bold))))
 '(ediff-even-diff-Ancestor ((((class color)) (:background "light grey" :foreground "black" :weight bold))))
 '(ediff-even-diff-C ((((class color)) (:background "light grey" :foreground "black" :weight bold))))
 '(ediff-fine-diff-B ((((class color)) (:background "cyan3" :foreground "black"))))
 '(ediff-fine-diff-C ((((class color)) (:background "Turquoise" :foreground "black" :weight bold))))
 '(hl-line ((t (:weight extra-bold))))
 '(mode-line ((t (:background "cornflower blue" :foreground "white" :box (:line-width 1 :color "white") :height 0.9))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((t (:box (:line-width 1 :color "white")))))
 '(mode-line-inactive ((t (:background "cornflower blue" :foreground "white" :height 0.9)))))



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

;;;;;;;;;;;;;;;;;;
;; YASnippets   ;;
;;;;;;;;;;;;;;;;;;
(defun insert-function-header(functionname)
  "Insert a c function header"
  (interactive "sEnter Function Name:")
  (insert (format
"/*
 *  %s
 *
 *
 *
 *  Parameters:
 *                          -
 *
 *  Results:
 *      errOk on success.
 *
 *  Side Effects:
 *      None
 */

%s() {
}

" functionname functionname))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Auto Complete and browsing  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (ggtags-mode)
  (flyspell-prog-mode)
  (yas-minor-mode)

  (company-mode t)
  (irony-mode t)
  (irony-cdb-autosetup-compile-options)
  (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (irony-mode-keys)
)

(defun ccppClangAsyncSetup ()
  (ggtags-mode t)
  (flyspell-prog-mode)
  (yas-minor-mode)
  (auto-complete-mode t)

  ;;(add-to-list 'ac-clang-cflags " -std=c++11")
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
    (global-auto-complete-mode t)
    (add-hook 'c++-mode-hook 'ccppClangAsyncSetup)
    (add-hook 'c-mode-hook 'ccppClangAsyncSetup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (message "using clang-async autocomplete for c/c++ completions")))










(put 'erase-buffer 'disabled nil)


;;(add-to-list 'load-path "~/.emacs.d/")
;; (require 'auto-complete-clang-async)
;; (defun ac-cc-mode-setup ()
;;   ;;(add-to-list 'ac-clang-cflags " -std=c++11")
;;   (setq ac-clang-cflags
;;       (mapcar (lambda (item)(concat "-I" item))
;;               (append
;;                clangincludes
;;                )
;;               )
;;       )
;;   (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
;;   (ac-clang-launch-completion-process)
;;   )


(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))




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
(defun js-mode-setup()
  (tern-mode t)
  (add-to-list 'ac-modes 'js3-mode)
  (global-auto-complete-mode t)
)
(add-hook 'js3-mode-hook 'ac-js-mode-setup)
(add-hook 'js3-mode-hook 'js-mode-setup)


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
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOC" [(meta right)])
(define-key input-decode-map "\e\eOD" [(meta left)])
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

;; ;; Set global EXWM key bindings
;; (exwm-input-set-key (kbd "M-r") #'exwm-restart)
;; (exwm-input-set-key (kbd "M-w") #'exwm-workspace-switch)
;; (exwm-input-set-key (kbd "M-d") 'dmenu)
;; (exwm-input-set-key (kbd "M-t") #'exwm-floating-toggle-floating)
;; (exwm-input-set-key (kbd "M-s") #'exwm-workspace-switch-to-buffer)
;; (exwm-input-set-key (kbd "M-f") #'exwm-layout-toggle-fullscreen)

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ([?\s-d] . dmenu)
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))))

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
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(exwm-enable)
(start-process "" nil "/usr/bin/nm-applet")


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
                        :emacsAttributeFaceHeight 150
                        :browserScalingFactor "2"
                        :terminalFontSize "14"))

(setq MonitorAppSettings (make-appSettings
                          :emacsAttributeFaceHeight 60
                          :browserScalingFactor "1"
                          :terminalFontSize "6"))

(defun ApplyAppSettings(applySettings)
    ;; emacs
  (set-face-attribute 'default nil :height (appSettings-emacsAttributeFaceHeight applySettings))
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


;; Application invocations
(defun GetToBrowser()
  (interactive)
  (setq browser-bufname "Chromium-browser")
  (setq browser-binary "/usr/bin/chromium-browser")
  (setq browser-invocation (concat browser-binary
                                   " --incognito --force-device-scale-factor="
                                   browserScaleFactor))

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
  (interactive)
  (setq term-bufname "XTerm")
  (setq term-binary "/usr/bin/xterm")
  (setq term-invocation (concat term-binary
                                " -bg black -fg white "
                                " -fa 'Monospace' -fs " terminalFontSize
                                ;;" -e 'screen -DR'"
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
  (setq term-bufname "XTerm")
  (setq term-binary "/usr/bin/xterm")
  (setq term-invocation (concat term-binary
                                " -bg black -fg white "
                                " -fa 'Monospace' -fs " terminalFontSize
                                ;;" -e 'screen -DR'"
                                ))

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


(defun ssh(hostName)
  (interactive "suserName@Host:")
  (setq ssh-bufname "XTerm")
  (setq ssh-binary "/usr/bin/xterm")
  (setq ssh-invocation (concat ssh-binary
                                " -bg black -fg white "
                                " -fa 'Monospace' -fs " terminalFontSize
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
          ([?\s-l] . 'LockScreen)
          ([?\s-d] . dmenu)
          ([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([s-f2] . (lambda ()
                      (interactive)
                      (start-process "" nil "/usr/bin/slock")))

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


  ;; Window Mgmt using Emacs style Alt-Key
  (exwm-input-set-key (kbd "s-<left>") 'windmove-left)
  (exwm-input-set-key (kbd "s-<down>") 'windmove-down)
  (exwm-input-set-key (kbd "s-<up>") 'windmove-up)
  (exwm-input-set-key (kbd "s-<right>") 'windmove-right)
  (exwm-input-set-key (kbd "s-S-<right>") 'enlarge-window-horizontally)
  (exwm-input-set-key (kbd "s-S-<left>") 'shrink-window-horizontally)
  (exwm-input-set-key (kbd "s-S-<up>") 'enlarge-window)
  (exwm-input-set-key (kbd "s-S-<down>") 'shrink-window)

  (exwm-input-set-key (kbd "s-k") 'exwm-input-release-keyboard)
  (exwm-input-set-key (kbd "s-j") 'exwm-input-grab-keyboard)

  (exwm-input-set-key (kbd "M-z") 'winner-undo)
  (global-set-key (kbd "M-z") 'winner-undo)
)

(i3WindowMgmtKeys)
(RetinaSetup)
;;(GetToTerminal)
;;(GetToBrowser)

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


(defun setupIFlipb()
  (interactive)
  (setq iflipb-wrap-around t)
  (global-set-key (kbd "<M-<tab>>") 'timed-iflipb-next-buffer)
  (global-set-key (kbd "<M-<iso-lefttab>") 'timed-iflipb-previous-buffer)
  (exwm-input-set-key (kbd "M-<tab>") 'timed-iflipb-next-buffer)
  (exwm-input-set-key (kbd "M-<iso-lefttab>") 'timed-iflipb-previous-buffer))

(setupIFlipb)
