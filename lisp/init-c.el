;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

;;(use-package cc-mode
;;  :ensure nil
;;  :bind (:map c-mode-base-map
;;			  ("C-c c" . compile))
;;  :init
;;  (setq-default indent-tabs-mode nil
;;                tab-width 4
;;                c-basic-offset 4
;;                c-default-style "linux"))
;;
;;(defun my/c-hook ()
;;  (setq indent-tabs-mode nil))
;;(add-hook 'c-mode-hook #'my/c-hook)
;;(add-hook 'c++-mode-hook #'my/c-hook)

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  ;;:bind (:map c-mode-base-map
  ;;       ("<f12>" . compile))
  :init (setq-default c-basic-offset 4))

(when (my/treesit-available-p)
  (use-package c-ts-mode
    :init (setq c-ts-mode-indent-offset 4)))

;;(add-to-list 'auto-mode-alist '("\.cu$" . c++-ts-mode))
;;(add-to-list 'auto-mode-alist '("\.cuh$" . c++-ts-mode))

(add-to-list 'auto-mode-alist '("\.mu$" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\.muh$" . c++-ts-mode))

(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
