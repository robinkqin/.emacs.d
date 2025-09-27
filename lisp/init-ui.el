;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(declare-function childframe-completion-workable-p "init-funcs")

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;;;; Initial frame
;;(setq initial-frame-alist '((top . 0.5)
;;                            (left . 0.5)
;;                            (width . 0.628)
;;                            (height . 0.8)
;;                            (fullscreen)))

;; Title
(setq frame-title-format '("%b")
      icon-title-format frame-title-format)

;; Theme
;;(load-theme 'wombat t)
(use-package doom-themes
  :functions doom-themes-visual-bell-config
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :init (load-theme 'doom-one t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; WORKAROUND: Visual bell on 29+
  ;; @see https://github.com/doomemacs/themes/issues/733
  (with-no-warnings
    (defun my-doom-themes-visual-bell-fn ()
      "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
      (let ((buf (current-buffer))
            (cookies (mapcar (lambda (face)
                               (face-remap-add-relative face 'doom-themes-visual-bell))
                             (if (facep 'mode-line-active)
                                 '(mode-line-active solaire-mode-line-active-face)
                               '(mode-line solaire-mode-line-face)))))
        (force-mode-line-update)
        (run-with-timer 0.15 nil
                        (lambda ()
                          (with-current-buffer buf
                            (mapc #'face-remap-remove-relative cookies)
                            (force-mode-line-update))))))
    (advice-add #'doom-themes-visual-bell-fn :override #'my-doom-themes-visual-bell-fn)))

;; Mode-line
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-time-icon nil
        doom-modeline-minor-modes t))

;;(use-package hide-mode-line
;;  :autoload turn-off-hide-mode-line-mode
;;  :hook (((eat-mode
;;           eshell-mode shell-mode
;;           term-mode vterm-mode
;;           embark-collect-mode lsp-ui-imenu-mode
;;           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)))

;;;; A minor-mode menu for mode-line
;;(use-package minions
;;  :hook (after-init . minions-mode))

;;;; Icons
;;(use-package nerd-icons
;;  :commands nerd-icons-install-fonts
;;  :functions font-available-p
;;  :config
;;  ;; Install nerd fonts automatically only in GUI
;;  ;; For macOS, may install via "brew install font-symbols-only-nerd-font"
;;  (when (and (display-graphic-p)
;;             (not (font-available-p nerd-icons-font-family)))
;;    (nerd-icons-install-fonts t)))

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode
          conf-mode toml-ts-mode
          yaml-mode yaml-ts-mode)
         . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(use-package time
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-format "%H:%M"
              display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Smooth scrolling
(use-package ultra-scroll
  :when emacs/>=29p
  :hook (after-init . ultra-scroll-mode))

;;(when (childframe-completion-workable-p)
;;  ;; Display transient in child frame
;;  (use-package transient-posframe
;;    :diminish
;;    :hook (after-init . transient-posframe-mode)))


(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
