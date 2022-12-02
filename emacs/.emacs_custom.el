(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dap-utils-unzip-script "bash -c 'mkdir -p %2$s; unzip -qq %1$s -d %2$s'")
 '(safe-local-variable-values '((buffer-reado-only . t)))
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(warning-suppress-types '((lsp-mode) ((flycheck syntax-checker)))))

(message "es/customizations-applied")
(message "es/customizations-font-applied")
(provide 'emacs_custom)
;;; .emacs_custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro"))))
 '(company-tooltip-selection ((t (:box (:line-width 2 :style released-button)))))
 '(corfu-current ((t (:box (:line-width 2 :style released-button)))))
 '(ivy-current-match ((t (:box (:line-width 2 :style released-button)))))
 '(ivy-posframe-border ((t (:inherit internal-border :background "white" :foreground "white")))))
