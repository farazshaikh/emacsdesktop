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
	 (irony-install-server)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)
(setenv "WRK" (concat (concat "/home/" (getenv "USER") "/excubito_workspace/hazen/.")))


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
   "cd $WRK/source/server; source ../../devsetup/go/setenv.sh;     go get ./src/excubito/...; go test ./src/excubito/...;
    go install ./...")
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(dabbrev-case-fold-search nil)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(ido-mode t nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(package-selected-packages
   (quote
    (yasnippet-snippets yasnippet-classic-snippets spacemacs-theme py-autopep8 jedi google-c-style golint go-stacktracer go-snippets go-projectile go-play go-errcheck go-direx go-autocomplete flycheck elpy edebug-x company-irony-c-headers company-irony cmake-mode auto-complete-nxml auto-complete-exuberant-ctags auto-complete-etags auto-complete-clang-async auto-complete-clang auto-complete-chunk auto-complete-c-headers)))
 '(python-python-command "/usr/bin/ipython")
 '(ring-bell-function
   (lambda nil
     (let
         ((orig-fg
           (face-foreground
            (quote mode-line))))
       (set-face-foreground
        (quote mode-line)
        "#F2804F")
       (run-with-idle-timer 0.1 nil
                            (lambda
                              (fg)
                              (set-face-foreground
                               (quote mode-line)
                               fg))
                            orig-fg))))
 '(scroll-step 1)
 '(set-fill-column 80)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(standard-indent 3)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(which-function-mode t)
 '(whitespace-style (quote (face empty tabs lines-tail whitespace))))

;; theme
;;(load-theme 'spacemacs-dark)
(setenv "WRK" (concat (concat "/home/" (getenv "USER") "/excubito_workspace/hazen/.")))



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



;;;;;;;;;;;;;;;;;;;;;;;
;; KeyBoard Mappings ;;
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
 '(mode-line-inactive ((t (:background "black" :foreground "white")))))



;;;;;;;;;;;;;;;;;;;
;; Emacs Tools   ;;
;;;;;;;;;;;;;;;;;;;


;; email
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

