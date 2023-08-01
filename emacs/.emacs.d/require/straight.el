;; Packages managed by straight.el

(setq straight-repository-branch "develop")
(setq straight-recipe-overrides '(straight :type git :host github
          :repo ,(format "%s/straight.el" straight-repository-user)
          :files ("straight*.el")
          :branch "develop"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
;	  (bootstrap-version 6))
;  (unless (file-exists-p bootstrap-file)
;	(with-current-buffer
;		(url-retrieve-synchronously
;		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;		 'silent 'inhibit-cookies)
;	  (goto-char (point-max))
;	  (eval-print-last-sexp)))
;  (load bootstrap-file nil 'nomessage))

(defun +straight-clean-up-temp-file ()
  "Delete current Emacs sessions's straight stderr file."
  (when (and (boundp 'straight--process-stderr)
             (file-exists-p straight--process-stderr))
    (delete-file straight--process-stderr)))

(add-hook 'kill-emacs-hook #'+straight-clean-up-temp-file)

(provide 'straight)
;;; straight.el ends here
