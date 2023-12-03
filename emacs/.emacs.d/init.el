;;
;; Custom Functions
;;

;;; Code:

(add-to-list 'load-path "~/.emacs.d/require")

(require 'straight)
(require 'ui)

(defun c-lineup-arglist-tabs-only (ignored)
  "IGNORED Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(setq straight-vc-git-default-clone-depth 1)

(straight-use-package 'magit)

(straight-use-package 'etags-update)
(require 'etags-update)

(straight-use-package 'markdown-mode)

(straight-use-package 'dockerfile-mode)

(straight-use-package 'mediawiki)

(straight-use-package 'company-mode)
(company-mode)
(setq company-idle-delay 0.0)

;; Flycheck
(straight-use-package 'flycheck)
(global-flycheck-mode)

;; Gtags
(straight-use-package 'gtags)
(straight-use-package 'ggtags)

;; neotree (like nerdtree)
(straight-use-package 'neotree)

;; Go
(straight-use-package 'go-mode)
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; Rust
(straight-use-package 'rust-mode)

;; Java
;; *.gradle file mode
(straight-use-package 'groovy-mode)

;; CMake files
(straight-use-package '(el-patch :type git :host github :repo "alamaison/emacs-cmake-project"))

(straight-use-package 'meson-mode)

(straight-use-package 'jinja2-mode)

(straight-use-package 'puppet-mode)

(straight-use-package 'snakemake-mode)

;; Emacs Speaks Statistics - for R
(straight-use-package 'ess)

;; Web Dev
(straight-use-package 'web-mode)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

;; COBOL
(straight-use-package 'cobol-mode)

(straight-use-package 'mu4e)
(require 'mu4e)

(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(straight-use-package 'org-autolist)
(add-hook 'org-mode-hook 'org-autolist-mode)

(straight-use-package 'esup)
(straight-use-package 'ansible)
(straight-use-package 'yaml-mode)
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

(straight-use-package 'evil)

(straight-use-package 'evil-collection)
(setq-default evil-want-keybinding nil)
(evil-collection-init)

(straight-use-package 'evil-fringe-mark)
(evil-fringe-mark-mode)
(setq-default left-fringe-width 8)
(setq-default evil-fringe-mark-side 'left-fringe)

(straight-use-package 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
;(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;(require 'evil-org-agenda)
;(evil-org-agenda-set-keys)

(evil-mode 1)
(evil-set-undo-system 'undo-redo)

(straight-use-package 'evil-commentary)
(evil-commentary-mode)

(straight-use-package 'terraform-mode)

(straight-use-package 'tabspaces)
(defvar tabspaces-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'tabspaces-clear-buffers)
    (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
    (define-key map (kbd "d") 'tabspaces-close-workspace)
    (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
    (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
    (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
    (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
    (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
    map)
  "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")
(tabspaces-mode)

;;
;; Miscellaneous
;;

;; Associate Dockerfiles with dockerfile-mode
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Associate *.css and *.html files with web-mode
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Associate *.check files with C/l-mode
(add-to-list 'auto-mode-alist '("\\.check?\\'" . c-mode))

;; Mutt configs are configs
(add-to-list 'auto-mode-alist '("\\muttrc?\\'" . conf-mode))

;; mutt mail temporary files are mail
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; use snakemake for snake files
(add-to-list 'auto-mode-alist '("\\.snake?\\'" . snakemake-mode))

;; need to ensure magit (provides git-commit-mode) is initialized when
;; editing commit messages
(add-to-list 'auto-mode-alist
             '("/COMMIT_EDITMSG\\'" . (lambda ()
                                        (require 'git-commit)
                                        (git-commit-mode))))

;; desktop-save-mode doesn't work as one would expect for emacsclient
;; if something, for example, calls $EDITOR when $EDITOR is
;; emacsclient, we definitely don't want to (desktop-read), so only do
;; that on the condition we're not in emacsclient
;; https://emacs.stackexchange.com/a/61705
(if (not (daemonp))
    (desktop-save-mode 1)
  (defun restore-desktop (frame)
    "Restores desktop and cancels hook after first frame opens.
     So the daemon can run at startup and it'll still work"
    (with-selected-frame frame
      (desktop-save-mode 1)
      (desktop-read)
      (remove-hook 'after-make-frame-functions 'restore-desktop)))
  (add-hook 'after-make-frame-functions 'restore-desktop))
(setq savehist-mode t)

(setq comp-deferred-compilation t)
(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)

;; Tab presses indent to 4 spaces by default
(setq-default tab-width 4)

;; Tab inserts spaces
(setq-default indent-tabs-mode nil)

;; This causes builds with npm to fail on the lockfiles with ".#*" names
;; Don't create these files for JS files
(add-hook 'js-mode-hook
		  (setq create-lockfiles nil))

(add-hook 'python-mode-hook
		  (lambda () (require 'ajak-python)
			; this doesn't actually work from ajak-python. why?
			(straight-use-package 'lsp-mode)
			(straight-use-package 'lsp-pyright)
			(lsp)))
(add-hook 'c-mode-hook
		  (lambda () (require 'c)))

;; no more '*~' files
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)

(require 'bindings)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(custom-enabled-themes nil)
 '(flycheck-checker-error-threshold 1000)
 '(helm-minibuffer-history-key "M-p")
 '(warning-suppress-types '((lsp-mode))))

(provide 'init)
