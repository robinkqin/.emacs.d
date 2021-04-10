(require 'package)

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

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;(add-to-list 'load-path "~/.emacs.d/custom")

;;(require 'setup-general)
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

;;(load-theme 'tango-dark)
(use-package gruvbox-theme
  :defer t
  :init
  (load-theme 'gruvbox t))

(use-package better-defaults
  :defer t)

(use-package smooth-scrolling
  :defer t
  :init
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 3))

(use-package rainbow-delimiters
  :defer t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(use-package smartparens
  :defer t
  :init
  (progn
    (require 'smartparens-config)))
(add-hook 'prog-mode-hook 'smartparens-mode)

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

(use-package keyfreq
  :defer t
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line)))

;;(require 'setup-helm)
(use-package helm
  :defer t
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    ;; To fix error at compile:
    ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
    ;; (with-helm-buffer helm-echo-input-in-header-line)
    (if (version< "26.0.50" emacs-version)
        (eval-when-compile (require 'helm-lib)))

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-google-suggest-use-curl-p t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ;; helm-quick-update t ; do not display invisible candidates
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

          ;; you can customize helm-do-grep to execute ack-grep
          ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

          helm-echo-input-in-header-line t

          ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
          helm-buffer-skip-remote-checking t

          helm-mode-fuzzy-match t

          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          ;; helm-apropos-fuzzy-match t
          helm-buffer-skip-remote-checking t
          helm-locate-fuzzy-match t
          helm-display-header-line nil)

    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-c r") 'helm-recentf)
    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
    ;;(global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h o") 'helm-occur)

    ;;(global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
    ;;(global-set-key (kbd "C-c h g") 'helm-google-suggest)

    (global-set-key (kbd "C-c h x") 'helm-register)
    ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    ;; show minibuffer history with Helm
    (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
    (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

    (define-key global-map [remap find-tag] 'helm-etags-select)

    (define-key global-map [remap list-buffers] 'helm-buffers-list)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; PACKAGE: helm-swoop                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Locate the helm-swoop folder to your path
    (use-package helm-swoop
      :defer t
      :bind (("C-c h o" . helm-swoop)
             ("C-c s" . helm-multi-swoop-all))
      :config
      ;; When doing isearch, hand the word over to helm-swoop
      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

      ;; From helm-swoop to helm-multi-swoop-all
      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t)

      ;; If this value is t, split window inside the current window
      (setq helm-swoop-split-with-multiple-windows t)

      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      (setq helm-swoop-split-direction 'split-window-vertically)

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      (setq helm-swoop-speed-or-color t))

    (helm-mode 1)

    (use-package helm-projectile
      :defer t
      :init
      (helm-projectile-on)
      (setq projectile-completion-system 'helm)
      (setq projectile-indexing-method 'alien))))

;;(require 'setup-helm-gtags)
;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :defer t
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

(use-package helm-ag
  :defer t)
;;(custom-set-variables
;; '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
;; '(helm-ag-command-option "--all-text")
;; '(helm-ag-insert-at-point 'symbol)
;; '(helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'")))
;;(custom-set-variables
;; '(helm-ag-base-command "rg --no-heading")
;; '(helm-ag-success-exit-status '(0 2)))

;;(require 'setup-ggtags)
(use-package ggtags
  :defer t)

(ggtags-mode 1)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

;;(dolist (map (list ggtags-mode-map))
;;  (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
;;  (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
;;  (define-key map (kbd "C-c g r") 'ggtags-find-reference)
;;  (define-key map (kbd "C-c g f") 'ggtags-find-file)
;;  (define-key map (kbd "C-c g c") 'ggtags-create-tags)
;;  (define-key map (kbd "C-c g u") 'ggtags-update-tags)
;;  (define-key map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;;  (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
;;  (define-key map (kbd "M-,") 'pop-tag-mark)
;;  (define-key map (kbd "C-c <") 'ggtags-prev-mark)
;;  (define-key map (kbd "C-c >") 'ggtags-next-mark))

;;(require 'setup-company)
;; Package: yasnippet
;; GROUP: Editing -> Yasnippet
;; Package: yasnippet
(use-package yasnippet
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;; company
(use-package company
  :defer t
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;;(require 'setup-evil)
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
  ";" 'helm-do-grep-ag
  ;;";" 'helm-grep-do-git-grep
  "SPC" 'helm-M-x

  ;; c t u y z , .
  "a" 'avy-goto-char-2
  "b" 'helm-buffers-list

  "e" 'helm-find-files
  "f" 'helm-projectile-find-file
  "r" 'helm-recentf

  "j" 'helm-gtags-dwim
  "k" 'helm-gtags-find-rtag
  "l" 'helm-gtags-pop-stack
  "o" 'helm-gtags-resume

  "d" 'helm-imenu
  "g" 'helm-gtags-select

  "s" 'rg-dwim

  "n" 'evil-scroll-page-down
  "p" 'evil-scroll-page-up

  "i" 'projectile-command-map
  "v" 'magit
  "x" 'kill-current-buffer

  "]]" 'git-gutter:next-hunk
  "[[" 'git-gutter:previous-hunk

  "mm" 'highlight-symbol-at-point
  "mr" 'highlight-regexp
  "mu" 'unhighlight-regexp

  "qq" 'save-buffers-kill-terminal

  "ww" 'other-window
  "wo" 'delete-other-windows
  "wc" 'evil-window-delete
  "wv" 'evil-window-vsplit
  "ws" 'evil-window-split
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up

  "hh" 'helm-man-woman
  "hf" 'describe-function
  "hv" 'describe-variable
  "hs" 'describe-symbol
  "hr" 'info-emacs-manual
  "hb" 'describe-bindings
  "hk" 'describe-key
  "hm" 'describe-mode
  "hi" 'info
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" default))
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   '(helm-gtags helm-projectile helm-swoop helm yasnippet projectile company use-package smooth-scrolling better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
