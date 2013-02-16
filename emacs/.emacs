(setenv "WRK" "~/")

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
 '(compile-command "cd $WRK/; make -j8")
 '(iswitchb-mode t)
 '(load-home-init-file t t)
 '(show-paren-mode t)
 '(standard-indent 3)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(which-function-mode t)
 '(muse-project-alist (quote (("Planner" ("~/devplans" :default "index" :major-mode planner-mode :visit-link planner-visit-link)))))
)

;; No tabs
(setq-default indent-tabs-mode nil)
(setq scroll-step 1)
;;(global-hl-line-mode 0)
;;3C(set-face-background 'hl-line "gray")

(setq-default show-trailing-whitespace t)


;; dynamic abbrevation completion must be case sensitive
(setq dabbrev-case-fold-search nil)

;; setup cscope
;;(require 'xcscope)
;;(setq cscope-do-not-update-database t)
;;(load-file "/usr/local/share/emacs/site-lisp/xcscope.el")


;; full filename in path
;;  (when (and
;;         (not window-system)
;;         (or
;;          (string= (getenv "TERM") "screen-bce")
;;          (string= (getenv "TERM") "dumb")
;;          (string-match "^xterm" (getenv "TERM"))))
;;    (require 'xterm-title)
;;    (xterm-title-mode 1))
;; (setq frame-title-format
;;      '("%S" (buffer-file-name "%f"
;;                   (dired-directory dired-directory "%b"))))

;; windmove window putty keys
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

(defun tags-create (dir-name)
     "Create tags file."
     (interactive "DDirectory: ")
     (eshell-command
      (format "find %s -type f -name \"*.[ch]\" | xargs etags -f %s/TAGS" dir-name dir-name)))


;;(load-file "$WRK/work/cedet-1.1/common/cedet.el")


;; enforce 80 column rule
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; latex
;;(load "auctex.el" nil t t)
;;(setq TeX-auto-save t)
;;(setq TeX-parse-self t)

;; GIT

;; backspace issues, toggle to resolve backspace issue
(normal-erase-is-backspace-mode 0)


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
 '(ediff-fine-diff-C ((((class color)) (:background "Turquoise" :foreground "black" :weight bold)))))

;;planner
(setq planner-project "Planner")
     (setq muse-project-alist
           '(("Planner"
             ("~/devplans"   ;; Or wherever you want your planner files to be
             :default "index"
             :major-mode planner-mode
             :visit-link planner-visit-link))))
;;(require 'planner)


;; email
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com"
                                   587
                                   "yourname@gmail.com"
                                   nil)))


;; templates
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

(defun insert-file-header ()
  "Insert a c function header"
  (interactive)
  (insert
"/*
 * fileName.x --
 *
 *      Short Description
 *      Bugs: fshaikh@cs.cmu.edu
 */")
)

(defun insert-gpl-licence ()
  "Insert a c function header"
  (interactive)
  (insert
"/*  This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/> */")
)
