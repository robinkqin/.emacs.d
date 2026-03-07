;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

(declare-function my/treesit-available-p "init-const")

;; C/C++ Mode
(use-package cc-mode
  :init (setq-default c-basic-offset 4))

(when (my/treesit-available-p)
  (use-package c-ts-mode
    :functions my/treesit-available-p
    :init
    (setq c-ts-mode-indent-offset 4)

    (when (boundp 'major-mode-remap-alist)
      (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
      (add-to-list 'major-mode-remap-alist
                   '(c-or-c++-mode . c-or-c++-ts-mode)))))

;;(defun my/c-hook ()
;;  (setq indent-tabs-mode nil))
;;(add-hook 'c-mode-hook #'my/c-hook)
;;(add-hook 'c++-mode-hook #'my/c-hook)

(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
