(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dap-utils-unzip-script "bash -c 'mkdir -p %2$s; unzip -qq %1$s -d %2$s'")
 '(safe-local-variable-values '((buffer-reado-only . t)))
 '(uniquify-buffer-name-style 'reverse nil (uniquify)))

(message "es/customizations-applied")

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

(message "es/customizations-font-applied")
(provide 'emacs_custom)
;;; .emacs_custom.el ends here
