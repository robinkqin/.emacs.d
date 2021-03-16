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
(require 'undo-tree)
(evil-mode 1)

;;avy/ivy/counsel/swiper
(require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "C-c C-r") 'ivy-resume)
;;(global-set-key (kbd "C-h f") 'counsel-describe-function)
;;(global-set-key (kbd "C-h v") 'counsel-describe-variable)
;;(global-set-key (kbd "C-h o") 'counsel-describe-symbol)
;;(global-set-key (kbd "C-h l") 'counsel-find-library)
;;(global-set-key (kbd "C-h S") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "C-h u") 'counsel-unicode-char)
(global-set-key (kbd "M-p o") 'counsel-recentf)
(global-set-key (kbd "M-p f") 'counsel-git)
(global-set-key (kbd "M-p g") 'counsel-rg)
;;(global-set-key (kbd "M-p g") 'counsel-git-grep)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;counsel gtags
(require 'counsel-gtags)
(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

(with-eval-after-load 'counsel-gtags
  (define-key counsel-gtags-mode-map (kbd "M-p d") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-p r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-p s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
  )

;;rg
(require 'rg)
(rg-enable-default-bindings)

;;projectile
(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; mac os
(define-key projectile-mode-map (kbd "M-p p") 'projectile-command-map)

;;company
;;(global-company-mode 1)

;;which-key
(require 'which-key)
(which-key-mode 1)
