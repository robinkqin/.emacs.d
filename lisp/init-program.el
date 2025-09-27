;;; init-program.el --- program configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function my/treesit-available-p "init-const")
(declare-function childframe-workable-p "init-const")

;; Tree-sitter support
(when (my/treesit-available-p)
  (use-package treesit-auto
    :hook (after-init . global-treesit-auto-mode)
    :init (setq treesit-auto-install 'prompt))

  ;; Code folding indicators using Tree-sitter
  (use-package treesit-fold-indicators
    :ensure treesit-fold
    :hook (after-init . global-treesit-fold-indicators-mode)
    :init (setq treesit-fold-indicators-priority -1)))

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish
  ;;:config
  ;;(when (childframe-workable-p)
  ;;  (use-package eldoc-box
  ;;    :custom
  ;;    (eldoc-box-lighter nil)
  ;;    (eldoc-box-only-multi-line t)
  ;;    (eldoc-box-clear-with-C-g t)
  ;;    :custom-face
  ;;    (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
  ;;    (eldoc-box-body ((t (:inherit tooltip))))
  ;;    :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
  ;;    :config
  ;;    ;; Prettify `eldoc-box' frame
  ;;    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
  ;;          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

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
;;  :hook (after-init . editorconfig-mode))

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
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (python-mode     . ("python~3.13"))
      (rustic-mode     . ("rust"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of major-mode and docs.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
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
;;(use-package vimrc-mode)
(use-package yaml-mode)

;;(use-package protobuf-mode
;;  :hook (protobuf-mode . (lambda ()
;;                           (setq imenu-generic-expression
;;                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;;;; Fish shell mode and auto-formatting
;;(use-package fish-mode
;;  :commands fish_indent-before-save
;;  :defines eglot-server-programs
;;  :hook (fish-mode . (lambda ()
;;                       "Integrate `fish_indent` formatting with Fish shell mode."
;;                       (add-hook 'before-save-hook #'fish_indent-before-save)))
;;  :config
;;  (with-eval-after-load 'eglot
;;    (add-to-list 'eglot-server-programs
;;                 '(fish-mode . ("fish-lsp" "start")))))

(use-package cuda-mode)

(use-package format-all
  ;;:bind ("C-c f" . #'format-all-region-or-buffer)
  :diminish format-all-mode)

(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
