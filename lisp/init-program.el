;;; init-program.el --- program configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function my/treesit-available-p "init-const")
(declare-function childframe-workable-p "init-const")

;; Tree-sitter support
(when (my/treesit-available-p)
  ;; Automatic Tree-sitter grammar management
  (use-package treesit-auto
    :functions my/treesit-available-p
    :hook (after-init . global-treesit-auto-mode)
    :init (setq treesit-auto-install 'prompt)))

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish
  :functions childframe-workable-p
  :init (setq eldoc-echo-area-use-multiline-p nil))

;; Cross-referencing commands
(use-package xref
  :autoload xref-show-definitions-completing-read
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;;;; Code styles
;;(use-package editorconfig
;;  :diminish
;;  :hook after-init)

;;;; Run commands quickly
;;(use-package quickrun
;;  :bind (("C-<f5>" . quickrun)
;;         ("C-c X"  . quickrun)))

;; Browse devdocs.io documents using EWW
(use-package devdocs
  :autoload devdocs--available-docs
  :commands (devdocs-install devdocs-lookup)
  :bind (:map prog-mode-map
         ("M-<f1>" . devdocs-dwim)
         ("C-h D"  . devdocs-dwim))
  :init
  (defvar devdocs-major-mode-docs-alist
    '(((c-mode c-ts-mode)           . ("c"))
      ((c++-mode c++-ts-mode)       . ("cpp"))
      (emacs-lisp-mode              . ("elisp"))
      ((python-mode python-ts-mode) . ("python~3.14"))
      ((rust-mode rust-ts-mode)     . ("rust")))
    "Alist of major-mode and docs.")

  (mapc (lambda (item)
          (let ((modes (car item))
                (docs  (cdr item)))
            (when (nlistp modes)
              (setq modes (list modes)))
            (dolist (m modes)
              (add-hook (intern (format "%s-hook" m))
                        (lambda ()
                          (setq-local devdocs-current-docs docs))))))
        devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.

Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

;;Example .dumbjump
;;  -tests
;;  -node_modules
;;  -build
;;  -images
;;  +../some-lib/src
;;  +/usr/lib/src
;;  +*.cu
;;  +*.cuh

;;Jump to definition
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (when (executable-find "rg")
    (setq dumb-jump-prefer-searcher 'rg)
    (setq dumb-jump-force-searcher 'rg))
  (setq dumb-jump-selector 'completing-read))

(use-package csv-mode)
(use-package cmake-mode)
;;(use-package lua-mode)
(use-package yaml-mode)

(use-package cuda-mode)

(add-to-list 'auto-mode-alist '("\.mu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\.muh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\.mu$" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\.muh$" . c++-ts-mode))

(use-package format-all
  ;;:bind ("C-c f" . #'format-all-region-or-buffer)
  :diminish format-all-mode)

(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
