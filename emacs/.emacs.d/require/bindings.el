;;
;; Keybinds
;;
;; Commented out for lack of use
;;(global-set-key (kbd "C-c b") 'browse-url-of-file)

(defun indent-doc ()
  "Indent buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Autospaced document"))

(global-set-key (kbd "C-h C-l") 'indent-doc)

;; More ergonomic left-right buffer switching
(global-set-key (kbd "C-c f") 'next-buffer)
(global-set-key (kbd "C-c b") 'previous-buffer)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x 6") 'toggle-window-split)

;;
;; International keys
;;
;; German
(define-key key-translation-map (kbd "C-; d a") (kbd "ä"))
(define-key key-translation-map (kbd "C-; d o") (kbd "ö"))
(define-key key-translation-map (kbd "C-; d u") (kbd "ü"))
(define-key key-translation-map (kbd "C-; d A") (kbd "Ä"))
(define-key key-translation-map (kbd "C-; d O") (kbd "Ö"))
(define-key key-translation-map (kbd "C-; d U") (kbd "Ü"))

;; Kurmanji Kurdish
(define-key key-translation-map (kbd "C-; k s") (kbd "ş"))
(define-key key-translation-map (kbd "C-; k i") (kbd "î"))
(define-key key-translation-map (kbd "C-; k e") (kbd "ê"))
(define-key key-translation-map (kbd "C-; k c") (kbd "ç"))
(define-key key-translation-map (kbd "C-; k u") (kbd "û"))
(define-key key-translation-map (kbd "C-; k S") (kbd "Ş"))
(define-key key-translation-map (kbd "C-; k I") (kbd "Î"))
(define-key key-translation-map (kbd "C-; k E") (kbd "Ê"))
(define-key key-translation-map (kbd "C-; k C") (kbd "Ç"))
(define-key key-translation-map (kbd "C-; k U") (kbd "Û"))

;; evil bindings for neotree
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

(provide 'bindings)
;;; bindings.el ends here
