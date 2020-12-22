(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dap-utils-unzip-script "bash -c 'mkdir -p %2$s; unzip -qq %1$s -d %2$s'")
 '(package-selected-packages
   '(ivy-posframe posframe helm-posframe pdf-tools vterm default-text-scale adoc-mode centaur-tabs yasnippet-snippets yasnippet-classic-snippets yaml-mode xref-js2 windower use-package-hydra unicode-fonts undo-tree ssh-agency spacemacs-theme spaceline rust-playground rg quelpa py-autopep8 pulseaudio-control persistent-scratch org-plus-contrib monokai-pro-theme magit lsp-ui lsp-ivy lsp-haskell kaolin-themes jedi ivy-hydra irony-eldoc iflipb ido-vertical-mode google-c-style golint go-stacktracer go-snippets go-projectile go-errcheck go-direx go-autocomplete git-timemachine git-gutter ggtags function-args free-keys flyspell-correct-ivy flycheck-rust flycheck-irony flx fill-column-indicator fancy-battery exwm elpy doom-themes direnv diminish desktop-environment delight dashboard dap-mode counsel-projectile company-quickhelp company-posframe company-lsp company-jedi company-irony-c-headers company-irony company-go company-c-headers company-box company-ansible clang-format+ ccls cargo auto-complete-nxml auto-complete-exuberant-ctags auto-complete-clang-async auto-complete-clang auto-complete-chunk auto-complete-c-headers ansible all-the-icons-dired ag ac-racer))
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
