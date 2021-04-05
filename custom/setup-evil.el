(use-package evil
  :defer t
  :init
  (evil-mode 1))

(use-package evil-leader
  :defer t
  :init
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC"))

(evil-leader/set-key
  ;;"/" 'swiper-isearch-thing-at-point
  ;;";" 'swiper-all-thing-at-point
  ;;"SPC" 'counsel-M-x

  ;; c l n t u v x y z ' .
  "a" 'avy-goto-char-2
  ;;"b" 'counsel-switch-buffer
  "d" 'kill-current-buffer
  ;;"e" 'counsel-find-file

  ;;"i" 'evil-jump-forward
  ;;"o" 'evil-jump-backward
  ;;"j" 'evil-scroll-page-down
  ;;"k" 'evil-scroll-page-up

  "p" 'projectile-command-map
  ;;"r" 'counsel-recentf
  ;;"s" 'my-rg
  ;;"," 'rg

  ;;"v" 'magit

  ;;"qq" 'save-buffers-kill-terminal

  ;;"ff" 'counsel-git
  ;;"fe" 'my-git-grep

  ;;"fd" 'my-gtags-find-definition
  ;;"fa" 'my-gtags-find-reference
  ;;"fs" 'my-gtags-find-symbol

  ;;"ft" 'counsel-etags-list-tag
  ;;"fg" 'counsel-etags-list-tag-in-current-file
  ;;"gg" 'counsel-etags-find-tag-at-point

  ;;"gc" 'counsel-gtags-create-tags
  ;;"gu" 'counsel-gtags-update-tags

  "mm" 'highlight-symbol-at-point
  "mr" 'highlight-regexp
  "mu" 'unhighlight-regexp

  "ww" 'other-window
  "wo" 'delete-other-windows
  "wc" 'evil-window-delete
  "wv" 'evil-window-vsplit
  "ws" 'evil-window-split
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up

  "]]" 'git-gutter:next-hunk
  "[[" 'git-gutter:previous-hunk

  "hf" 'counsel-describe-function
  "hv" 'counsel-describe-variable
  "hs" 'counsel-describe-symbol
  "hr" 'info-emacs-manual
  "hb" 'describe-bindings
  "hk" 'describe-key
  "hm" 'describe-mode
  "hi" 'info
  )

(provide 'setup-evil)
