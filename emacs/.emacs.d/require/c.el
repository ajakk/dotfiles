;; C mode use kernel style
(c-add-style "linux-tabs-only" '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))

(setq tab-width 8)
(setq indent-tabs-mode t)
(setq c-basic-offset 8)
(c-set-style "linux-tabs-only")
(etags-update-mode)

(straight-use-package 'lsp-mode)
(straight-use-package 'helm-lsp)
(straight-use-package 'helm-xref)
(setq lsp-keymap-prefix "C-c l")

(lsp)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
;  (require 'dap-cpptools)
;  (yas-global-mode))

(provide 'c)
;;; c.el ends here
