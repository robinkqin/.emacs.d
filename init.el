(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen 1)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

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
		      which-key
		      ))

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
(load-theme 'gruvbox t)

;;evil
(require 'evil)
(require 'evil-leader)
(require 'undo-tree)
(evil-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(setq evil-leader/in-all-states 1)

;;avy/ivy/counsel/swiper
(require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")
(setq enable-recursive-minibuffers t)
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
(projectile-mode +1)

;;company
;;(global-company-mode 1)

(evil-leader/set-key
  "/" 'swiper
  "SPC" 'counsel-M-x

  "j" 'evil-scroll-page-down
  "k" 'evil-scroll-page-up
  "i" 'evil-jump-forward
  "o" 'evil-jump-backward

  "p" 'projectile-command-map

  "hf" 'counsel-describe-function
  "hv" 'counsel-describe-variable
  "hs" 'counsel-describe-symbol
  "hk" 'describe-key

  "bb" 'counsel-switch-buffer
  "bd" 'kill-buffer

  "xf" 'counsel-find-file
  "ff" 'counsel-git
  "fm" 'counsel-recentf
  "fs" 'save-buffer

  "fe" 'counsel-rg
  ;;"fa" 'counsel-rg
  ;;"fo" 'counsel-rg
  ;;"fp" 'counsel-rg
  ;;"fn" 'counsel-rg

  "ge" 'counsel-git-grep
  "gd" 'counsel-git-dwim
  "gc" 'counsel-gtags-create-tags
  "gu" 'counsel-gtags-update-tags
  "gg" 'counsel-gtags-find-definition
  "gr" 'counsel-gtags-find-reference
  "gs" 'counsel-gtags-find-symbol
  "go" 'counsel-gtags-go-backward
  "gi" 'counsel-gtags-go-forward

  "ww" 'other-window
  "wo" 'delete-other-windows
  "wc" 'evil-window-delete
  "wv" 'evil-window-vsplit
  "ws" 'evil-window-split
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  )

;;which-key
(require 'which-key)
(which-key-mode 1)
