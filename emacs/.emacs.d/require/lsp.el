(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-pyright)
(straight-use-package 'helm-lsp)
(straight-use-package 'helm-xref)
(setq lsp-keymap-prefix "C-c l")

(lsp)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
;  (require 'dap-cpptools)
;  (yas-global-mode))

(provide 'lsp)
;;; lsp.el ends here
