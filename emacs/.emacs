;; ELPA packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


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
 '(compile-command "cd $WRK/cypress; make -j8 DMSETUP=true")
 '(global-hl-line-mode t)
 '(ido-mode t nil (ido))
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(muse-project-alist (quote (("StorvisorPlanner" ("~/storvisorplans" :default "index" :major-mode planner-mode :visit-link planner-visit-link)))))
 '(show-paren-mode t)
 '(standard-indent 3)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(which-function-mode t))

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
(setenv "WRK" "/storvisor/work")


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
 '(hl-line ((t (:weight extra-bold)))))



;;;;;;;;;;;;;;;;;;;
;; Emacs Tools   ;;
;;;;;;;;;;;;;;;;;;;
;;planner
(setq planner-project "StorvisorPlanner")
     (setq muse-project-alist
           '(("StorvisorPlanner"
             ("~/storvisorplans"   ;; Or wherever you want your planner files to be
             :default "index"
             :major-mode planner-mode
             :visit-link planner-visit-link))))
(require 'planner)


;; email
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com"
                                   587
                                   "user@gmail.com"
                                   nil)))


;;;;;;;;;;;;;;;;;;
;; templates    ;;
;;;;;;;;;;;;;;;;;;
(defun insert-function-header ()
  "Insert a c function header"
  (interactive)
  (insert
"/*
 *  .n
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
 */")

)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Auto Complete and browsing  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xcscope)
(setq cscope-do-not-update-database t)

;; python completions
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

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
))

(require 'auto-complete)
(add-hook 'c++-mode-hook 'c-mode-common-hook '(lambda ()
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
          (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
  ))
(put 'erase-buffer 'disabled nil)

;;clang AC
(defcustom mycustom-system-include-paths
           '("./include/"
            "/opt/local/include"
            "/usr/include"
            "/usr/include/c++/4.7"
            "/usr/include/c++/4.7/x86_64-linux-gnu"
            "/usr/include/c++/4.7/backward"
            "/usr/lib/gcc/x86_64-linux-gnu/4.7/include"
            "/usr/local/include"
            "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed"
            "/usr/include/x86_64-linux-gnu"
            "/usr/include"
           )
  "This is a list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
  )

(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-clang)
(setq clang-completion-suppress-error 't)
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (append
               mycustom-system-include-paths
               )
              )
      )
(add-to-list 'ac-clang-flags " -std=c++11")

(defun my-ac-clang-mode-common-hook()
  (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang)
)

(add-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook)
