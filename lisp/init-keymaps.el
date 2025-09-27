;;; init-keymaps.el --- Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; can use:     C-j C-o ( C-; C-' C-, C-. NOT WORK IN PUTTY)
;; can use:     M-a M-e M-q
;;    used:     M-a M-e M-c M-n M-p M-q M-r M-t M-y
;; already use: M-h M-i M-j M-k M-l M-m M-o M-u M-; M-' M-/ M-[ M-]
;; keep origin: M-g M-s M-v M-b M-f M-w M-x M-z M-d M-. M-,

;; Select widnow via `M-1'...`M-9'

(global-set-key (kbd "M-;") 'avy-goto-char-2)
(global-set-key (kbd "M-s ;") 'avy-goto-char-timer)

(global-set-key (kbd "M-'") 'consult-imenu)
(global-set-key (kbd "M-s '") 'consult-eglot-symbols)

(global-set-key (kbd "M-s ,") 'goto-last-change)

(global-set-key (kbd "M-m") 'consult-bookmark)
(global-set-key (kbd "M-s m") 'my/bookmark-at-point)

(global-set-key (kbd "M-j") 'consult-ripgrep)
(global-set-key (kbd "M-k") 'consult-line)
(global-set-key (kbd "M-l") 'dumb-jump-go)
(global-set-key (kbd "M-\\") 'color-rg-search-symbol-in-project)

(global-set-key (kbd "M-s j") 'my/grep-from-ynak)
(global-set-key (kbd "M-s k") 'my/consult-line-from-ynak)
(global-set-key (kbd "M-s l") 'vertico-repeat) ;; vertico-suspend
(global-set-key (kbd "M-s \\") 'my/color-rg-search-symbol-in-project-from-ynak)

(global-set-key (kbd "M-r") 'rg-menu)

(global-set-key (kbd "M-u") 'symbol-overlay-remove-all)

(global-set-key (kbd "M-s ]") 'diff-hl-next-hunk) ;FIXME: for windows
(global-set-key (kbd "M-s [") 'diff-hl-previous-hunk)


(provide 'init-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keymaps.el ends here
