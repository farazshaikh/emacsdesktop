;;; packages --- Load the emacs.org file
;;;
;;; Commentary:
;;; Simple loader for a org based initialization file
(require 'org)
(org-babel-load-file
 (expand-file-name "~/.eos/emacsdesktop/emacs/emacs.org"
                   user-emacs-directory))
;; Emacs Desktop
;; (org-babel-load-file
;; (expand-file-name "~/.eos/emacsdesktop/emacs/emacs_desktop.org"
;;                   user-emacs-directory))

;;; .emacs ends here
