;; -*- lexical-binding: t -*-

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen 1)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(display-time-mode 1)
(desktop-save-mode 1)
(global-auto-revert-mode 1)
(setq make-backup-files nil)

(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default c-basic-offset 4 c-default-style "bsd")
(define-key global-map (kbd "RET") 'newline-and-indent)

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "proxy.com:8080")
;;         ("https" . "proxy.com:8080")))
;;
;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;;                   (cons "Input your LDAP UID !"
;;                         (base64-encode-string "USERNAME:PASSWORD")))))

(setq package-archives '(("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
			 ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(require 'package)
(package-initialize)
;;(package-refresh-contents)

;;Common Lisp Extension
(require 'cl)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

(defvar my/packages '(
                      use-package
                      gruvbox-theme
                      better-defaults
                      smooth-scrolling
                      rainbow-delimiters
                      evil
                      evil-leader
                      undo-tree
                      avy
                      ivy
                      counsel
                      counsel-gtags
                      swiper
                      rg
                      projectile
                      ;; company
                      which-key))

(setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

;;use-package
(require 'use-package)

;;theme
(load-theme 'gruvbox 1)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;evil
(require 'evil)
(require 'evil-leader)
(require 'undo-tree)
(evil-mode 1)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")

;;avy/ivy/counsel/swiper
(require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers 1)
(setq ivy-count-format "%d/%d ")
(setq enable-recursive-minibuffers 1)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;counsel gtags
(require 'counsel-gtags)
(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

;;rg
(require 'rg)
(rg-enable-default-bindings)

;;projectile
(projectile-mode 1)

;;company
;;(Global-Company-Mode 1)

(global-set-key (kbd "M-p") 'keyboard-quit)

(define-key evil-normal-state-map "\M-n" 'evil-force-normal-state)
(define-key evil-visual-state-map "\M-n" 'evil-change-to-previous-state)
(define-key evil-insert-state-map "\M-n" 'evil-normal-state)
(define-key evil-replace-state-map "\M-n" 'evil-normal-state)
;;(define-key evil-normal-state-map (kbd "ff") 'counsel-find-file)

(evil-leader/set-key
  "/" 'swiper
  "SPC" 'counsel-M-x

  ;; c l n t u v x y z ' , .
  "a" 'avy-goto-char-2
  "b" 'counsel-switch-buffer
  "d" 'kill-current-buffer
  "e" 'counsel-find-file

  "i" 'evil-jump-forward
  "o" 'evil-jump-backward
  "j" 'evil-scroll-page-down
  "k" 'evil-scroll-page-up

  "p" 'projectile-command-map
  "r" 'counsel-recentf
  "s" 'counsel-rg
  ";" 'rg

  "qq" 'save-buffers-kill-terminal

  "ff" 'counsel-git
  "fs" 'save-buffer
  "fe" 'counsel-git-grep

  "gd" 'counsel-git-dwim
  "gg" 'counsel-gtags-find-definition
  "gr" 'counsel-gtags-find-reference
  "gs" 'counsel-gtags-find-symbol
  "gt" 'counsel-gtags-create-tags
  "gu" 'counsel-gtags-update-tags

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

  "hf" 'counsel-describe-function
  "hv" 'counsel-describe-variable
  "hs" 'counsel-describe-symbol
  "hk" 'describe-key
  )

;;which-key
(require 'which-key)
(which-key-mode 1)
