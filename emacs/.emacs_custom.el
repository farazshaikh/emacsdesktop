(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.direnv$" "[/\\\\]\\.git$" "[/\\\\]\\.cargo$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]\\result???$" "[/\\\\]\\target???$" "[/\\\\]\\.cargo-home???$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.$") nil nil "Customized with use-package lsp-mode")
 '(package-selected-packages
   '(exec-path-from-shell consult-dash dash-docs org-tempo ob-async org-babel-eval-in-repl org-babel ob-rust catppuccin-theme emacs-jupyter jupyter code-cells ellama all-the-icons yasnippet-snippets yaml-mode writeroom-mode vterm vertico-prescient use-package-hydra treemacs toml-mode toc-org ssh-agency solidity-flycheck savekill rust-mode rg quelpa-use-package py-autopep8 projectile persistent-scratch pdf-tools org-superstar org-bullets orderless modus-themes marginalia magit lsp-ui lsp-ltex lsp-latex lsp-ivy lsp-haskell lsp-grammarly go-autocomplete git-timemachine git-gutter flycheck-rust flycheck-grammarly eglot editorconfig doom-modeline direnv diminish default-text-scale dedicated corfu-terminal corfu-prescient corfu-candidate-overlay company-solidity company-quickhelp company-prescient company-c-headers company-box clipetty clang-format+ cargo adoc-mode))
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
 '(jupyter-repl-traceback ((t nil))))
