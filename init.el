;; -*- lexical-binding: t -*-

;;#cat /usr/share/X11/xkb/rules/base.lst  | grep ctrl
;;#setxkbmap -option ctrl:swapcaps
;;#setxkbmap -option ctrl:nocaps
;;setxkbmap -option ctrl:swap_lalt_lctl

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen 1)
(setq ring-bell-function 'ignore)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(display-time-mode 1)
(desktop-save-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

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

(require 'cl)
(require 'package)
(package-initialize)
;;(package-refresh-contents)

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
                      super-save
                      evil
                      evil-leader
                      undo-tree
                      avy
                      ivy
                      counsel
                      counsel-etags
                      counsel-gtags
                      swiper
                      rg
                      projectile
                      magit
                      git-gutter
                      company
                      company-ctags
                      yasnippet
                      vterm
                      which-key
                      keyfreq))

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
(require 'better-defaults)

;;theme
(load-theme 'gruvbox 1)

(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(super-save-mode 1)
(setq super-save-idle-duration 1)
(setq super-save-auto-save-when-idle t)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 3)

(require 'undo-tree)
(global-undo-tree-mode)

;;evil
(require 'evil)
(require 'evil-leader)
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

;;(global-set-key (kbd "M-p") 'evil-force-normal-state)
;;(global-set-key (kbd "M-[") 'minibuffer-keyboard-quit)
;;;;(define-key key-translation-map (kbd "M-]") (kbd "C-g"))

;;etags
(require 'counsel-etags)
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      'counsel-etags-virtual-update-tags 'append 'local)))
(setq counsel-etags-update-interval 60)
(with-eval-after-load 'counsel-etags
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (push "build" counsel-etags-ignore-directories)
  (push "build_clang" counsel-etags-ignore-directories)
  ;; counsel-etags-ignore-filenames supports wildcast
  (push "GPATH" counsel-etags-ignore-filenames)
  (push "GTAGS" counsel-etags-ignore-filenames)
  (push "GRTAGS" counsel-etags-ignore-filenames)
  (push "TAGS" counsel-etags-ignore-filenames)
  (push ".projectile" counsel-etags-ignore-filenames)
  (push "*.json" counsel-etags-ignore-filenames))
;;(setq counsel-etags-extra-tags-files '("/usr/include/TAGS" "/usr/local/include/TAGS"))

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
(add-hook 'after-init-hook 'global-company-mode)

;;company-ctags
;;find . -name "*.[ch]" | ctags -e -L -
;;(setq company-ctags-extra-tags-files '("$HOME/TAGS" "/usr/include/TAGS"))
(with-eval-after-load 'company
  (company-ctags-auto-setup))

(require 'yasnippet)
(yas-global-mode 1)

(require 'vterm)

(defun my-gtags-find-definition ()
  (interactive)
  (counsel-gtags-find-definition (thing-at-point 'symbol)))

(defun my-gtags-find-reference ()
  (interactive)
  (counsel-gtags-find-reference (thing-at-point 'symbol)))

(defun my-gtags-find-symbol ()
  (interactive)
  (counsel-gtags-find-symbol (thing-at-point 'symbol)))

(defun my-git-grep ()
  (interactive)
  (counsel-git-grep (thing-at-point 'symbol)))

(defun my-rg ()
  (interactive)
  (counsel-rg (thing-at-point 'symbol)))

(evil-leader/set-key
  "/" 'swiper-isearch-thing-at-point
  ";" 'swiper-all-thing-at-point
  "SPC" 'counsel-M-x

  ;; c l n t u v x y z ' .
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
  "s" 'my-rg
  "," 'rg

  "v" 'magit

  "qq" 'save-buffers-kill-terminal

  "ff" 'counsel-git
  "fe" 'my-git-grep

  "fd" 'my-gtags-find-definition
  "fa" 'my-gtags-find-reference
  "fs" 'my-gtags-find-symbol

  "ft" 'counsel-etags-list-tag
  "fg" 'counsel-etags-list-tag-in-current-file
  "gg" 'counsel-etags-find-tag-at-point

  "gc" 'counsel-gtags-create-tags
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

(global-git-gutter-mode 1)
(custom-set-variables
 '(git-gutter:update-interval 2))

;;which-key
(require 'which-key)
(which-key-mode 1)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        forward-char
        backward-char
        previous-line
        next-line))

;;sshfs remotehost:/remote/directory/ ~/local/directory -o reconnect -C
