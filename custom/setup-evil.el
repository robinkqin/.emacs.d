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
  "/" 'helm-swoop
  ;;";" 'swiper-all-thing-at-point
  "SPC" 'helm-M-x

  ;; c l n t u v x y z ' .
  "a" 'avy-goto-char-2
  "b" 'switch-to-buffer
  "d" 'kill-current-buffer
  "e" 'helm-find-files

  "i" 'evil-jump-forward
  "o" 'evil-jump-backward
  "j" 'evil-scroll-page-down
  "k" 'evil-scroll-page-up

  "p" 'projectile-command-map
  "r" 'helm-recentf
  ;;"s" 'my-rg
  ;;"," 'rg

  "v" 'magit

  "qq" 'save-buffers-kill-terminal

  ;;"ff" 'counsel-git
  "fe" 'helm-grep-do-git-grep

  "fd" 'helm-gtags-dwim
  "fs" 'helm-gtags-find-symbol
  "fa" 'helm-gtags-find-tag

  ;;"ft" 'counsel-etags-list-tag
  ;;"fg" 'counsel-etags-list-tag-in-current-file
  ;;"gg" 'counsel-etags-find-tag-at-point

  "gc" 'helm-gtags-create-tags
  "gu" 'helm-gtags-update-tags

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
