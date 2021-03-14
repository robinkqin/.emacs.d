(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

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
;; (package-refresh-contents)

;; Common Lisp Extension
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
		      avy
		      ivy
		      counsel
		      swiper
		      ;; ripgrep
		      ;; fzf
		      ;; gtags
		      ;; ctags
		      projectile
		      ;; lsp-mode
		      ;; yasnippet
		      ;; flycheck
		      company
		      ;; magit
		      org
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
(require 'gruvbox-theme)
(load-theme 'gruvbox t)


;;evil
(require 'evil)
(evil-mode 1)


;;avy/ivy/counsel/swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
;; (global-set-key "\C-s" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f5> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f5> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f5> s") 'counsel-describe-symbol)
;; (global-set-key (kbd "<f5> l") 'counsel-find-library)
;; (global-set-key (kbd "<f5> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f5> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c f") 'counsel-git)
;; (global-set-key (kbd "C-c g") 'counsel-git-grep)
;; (global-set-key (kbd "C-c r") 'counsel-rg)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


;;ripgrep


;;fzf


;;gtags


;;ctags


;;projectile
(projectile-mode +1)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; mac os
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;yasnippet


;;flycheck


;;company
(global-company-mode 1)


;;lsp-mode


;;magit


;;org
(require 'org)
(setq org-src-fontify-natively t)
(setq org-agenda-files '("~/org"))
;; (global-set-key (kbd "C-c a") 'org-agenda)


;;which-key
(require 'which-key)
(which-key-mode 1)
