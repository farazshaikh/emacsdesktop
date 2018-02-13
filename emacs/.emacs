;;; No point in supporting multiple version, there is way to much work needed for that
(if (version< emacs-version  "24.4")
 (error "Script depends on emacs version being greater than 24.4")
 (message "Version greater or equal to 24.4"))


;; Python setting copied from
;; https://realpython.com/blog/python/emacs-the-best-python-editor/
;; install elpy from elpy repo
;; install jedi (python part, emacs part, epc part)
;; fly-check and not fly-make
;; follow elpy-config
;; autopep8 will need to installed via pip

;; ELPA packages
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


;; el-get
(unless (require 'el-get nil t)
  (setq el-get-install-branch "master")
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp))
  (el-get-emacswiki-refresh el-get-recipe-path-emacswiki t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto install packages if not installed ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; list the packages you want
(setq package-list '(auto-complete
                     auto-complete-auctex
                     auto-complete-c-headers
                     auto-complete-chunk
                     auto-complete-clang
                     auto-complete-clang-async
                     auto-complete-etags
                     auto-complete-exuberant-ctags
                     auto-complete-nxml
                     jedi
                     elpy
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
                     google-c-style
                     flycheck
		     py-autopep8))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Tabs and others
(setq set-fill-column 80)
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
 '(compile-command
   "cd $WRK/source/server; source ../../devsetup/go/setenv.sh; go get ./src/excubito/...; go test ./src/excubito/...;  go install ./...")
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(global-hl-line-mode t)
 '(ido-mode t nil (ido))
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(python-python-command "/usr/bin/ipython")
 '(show-paren-mode t)
 '(standard-indent 3)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(which-function-mode t))

;; theme
(load-theme 'spacemacs-dark)
(setq inhibit-startup-message t) ;; hide the startup message

;; No tabs
(setq-default indent-tabs-mode nil)
(setq scroll-step 1)
(setq-default show-trailing-whitespace t)
;; enforce 80 column rule
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail whitespace))
(global-whitespace-mode t)
;; dynamic abbrevation completion must be case sensitive
(setq dabbrev-case-fold-search nil)
(setenv "WRK" (concat (concat "/home/" (getenv "USER") "/excubito_workspace/hazen/.")))

;; Workaround auto complete and whitespace show YELLOW/RED boxes
;; https://stackoverflow.com/questions/12965814/emacs-how-can-i-eliminate-whitespace-mode-in-auto-complete-pop-ups/27960576#27960576
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper defuns          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Dance         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun copy-rectangle-as-kill ()
    (interactive)
    (save-excursion
    (kill-rectangle (mark) (point))
    (exchange-point-and-mark)
    (yank-rectangle)))



;;;;;;;;;;;;;;;;;;
;; templates    ;;
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
;; (require 'xcscope)
;; (setq cscope-do-not-update-database t)

;; python completions
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(elpy-use-ipython)
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



(add-hook 'ansi-term-mode-hook '(lambda ()
      (setq term-buffer-maximum-size 0)
      (setq-default show-trailing-whitespace f)
))

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(add-hook 'c-mode-hook '(lambda ()
      ;;(gtags-mode t)
      (auto-complete-mode t)
      (flyspell-prog-mode)
))

(add-hook 'compilation-mode '(lamda ()
      (next-error-follow-minor-mode t)
))

(defun dotArrowHooks ()
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

(require 'auto-complete)
(add-hook 'c++-mode-hook 'dotArrowHooks)
(add-hook 'c-mode-hook 'dotArrowHooks)
(put 'erase-buffer 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System includes       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang includes        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq clangincludes project-include-paths)
(setq clangincludes (append clangincludes mycustom-system-include-paths))
(defcustom clangincludes clangincludes
  "This is a list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang completionSync  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (require 'auto-complete-clang)
;; (setq clang-completion-suppress-error 't)
;; (setq ac-clang-flags
;;       (mapcar (lambda (item)(concat "-I" item))
;;               (append
;;                clangincludes
;;                )
;;               )
;;       )
;; ;;(add-to-list 'ac-clang-flags " -std=c++11")
;; (defun my-ac-clang-mode-common-hook()
;;   (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang)
;; )
;; (add-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang completionAsync ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
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
)

;; go mode setup
;; setup AC via gocode
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
(add-hook 'go-mode-hook 'go-eldoc-setup)




(defun ac-js-mode-setup()
  (tern-mode t)
  (add-to-list 'ac-modes 'js3-mode)
  (global-auto-complete-mode t)
)

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (add-hook 'js3-mode-hook 'ac-js-mode-setup)
  (add-hook 'go-mode-hook 'ac-go-mode-setup)
  (global-auto-complete-mode t)
)

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(my-ac-config)
(put 'downcase-region 'disabled nil)
