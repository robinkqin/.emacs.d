(menu-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

(desktop-save-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(use-package better-defaults
  :defer t)

(use-package smooth-scrolling
  :defer t
  :init
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 3))

(use-package rainbow-delimiters
  :defer t
  :init
  (rainbow-delimiters-mode 1))

(use-package super-save
  :defer t
  :init
  (super-save-mode 1)
  (setq super-save-idle-duration 1)
  (setq super-save-auto-save-when-idle t))

(use-package avy
  :defer t)

(use-package undo-tree
  :defer t
  :init
  (global-undo-tree-mode 1))

;; Package: projejctile
(use-package projectile
  :defer t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
(windmove-default-keybindings)

(use-package rg
  :defer t
  :init
  (rg-enable-default-bindings))

(use-package magit
  :defer t)

(use-package git-gutter
  :defer t
  :init
  (global-git-gutter-mode 1)
  (custom-set-variables
   '(git-gutter:update-interval 2)))

(use-package vterm
  :defer t)

(use-package which-key
  :defer t
  :init
  (which-key-mode 1))

;;(use-package which-key
;;  :defer t
;;  :init
;;  (keyfreq-mode)
;;  (keyfreq-autosave-mode 1)
;;  (setq keyfreq-excluded-commands
;;        '(self-insert-command
;;          forward-char
;;          backward-char
;;          previous-line
;;          next-line)))

(provide 'setup-general)
