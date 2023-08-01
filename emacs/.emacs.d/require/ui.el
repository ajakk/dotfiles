;;; package -- summary
;;; Commentary:
;;;
;;; Code:

;; This is JeffDwork's function from
;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle split of two windows between horizontal and vertical."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; Pretty up the UI a little bit to make Emacs in X feel more like Emacs in terminal
(when (display-graphic-p)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

(menu-bar-mode -1)

(setq-default column-number-mode t)
(setq-default show-trailing-whitespace t)

(straight-use-package 'molokai-theme)
(load-theme 'molokai t)

(straight-use-package 'telephone-line)
(telephone-line-mode 1)

(straight-use-package 'helm)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(straight-use-package 'helm-tramp)

(provide 'ui)
;;; ui.el ends here
