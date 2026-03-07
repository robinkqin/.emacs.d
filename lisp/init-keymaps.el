;;; init-keymaps.el --- Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Select widnow via `M-1'...`M-9'

(global-set-key (kbd "C-c ;") 'avy-goto-char-2)
(global-set-key (kbd "C-c '") 'avy-goto-char-timer)

(global-set-key (kbd "C-c j") 'dumb-jump-go)

(global-set-key (kbd "M-s m") 'my/bookmark-at-point)
(global-set-key (kbd "M-s ,") 'consult-bookmark)

(global-set-key (kbd "M-s M-s") 'consult-ripgrep)
(global-set-key (kbd "M-s g") 'consult-git-grep)
(global-set-key (kbd "M-s G") 'consult-grep)

(global-set-key (kbd "M-s j") 'my/grep-from-yank)
(global-set-key (kbd "M-s k") 'my/consult-line-from-yank)
(global-set-key (kbd "M-s l") 'vertico-repeat) ;; vertico-suspend
(global-set-key (kbd "M-s \\") 'color-rg-search-symbol-in-project)
;;(global-set-key (kbd "M-s \\") 'my/color-rg-search-symbol-in-project-from-yank)

(global-set-key (kbd "M-s ;") 'my/consult-line-other-window)
(global-set-key (kbd "M-s '") 'my/consult-grep-other-window-project)

(global-set-key (kbd "M-s ]") 'diff-hl-next-hunk)
(global-set-key (kbd "M-s [") 'diff-hl-previous-hunk)

(global-set-key (kbd "C-x _") 'split-window-vertically-instead)
(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)

(provide 'init-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keymaps.el ends here
