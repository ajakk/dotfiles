(require 'flycheck)


(flycheck-define-checker pkgcheck
  "A checker for ebuild files."
  :modes 'ebuild-mode
  :predicate (lambda () (buffer-file-name))
  :command ("pkgcheck"
            "scan"
            "--exit="
            "--format={package}-{version}.ebuild:0:{level}:{name}: {desc}"
            "--reporter=FormatReporter"
            "--verbose"
            (eval (buffer-file-name)))
  :error-patterns
  ((info
    line-start (file-name) ":" line ":" "info" ":" (message) line-end)
   (info
    line-start (file-name) ":" line ":" "style" ":" (message) line-end)
   (warning
    line-start (file-name) ":" line ":" "warning" ":" (message) line-end)
   (error
    line-start (file-name) ":" line ":" "error" ":" (message) line-end)))


;;;###autoload
(defun flycheck-pkgcheck-setup ()
  "Flycheck pkgcheck setup."
  (interactive)
  (add-to-list 'flycheck-checkers 'pkgcheck t))

;;;###autoload
(add-hook 'ebuild-mode-hook 'flycheck-pkgcheck-setup)


(provide 'flycheck-pkgcheck)
