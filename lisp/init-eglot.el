;;; init-eglot.el --- eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode
                                 'ron-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init (setq eglot-autoshutdown t
              eglot-events-buffer-config '(:size 0 :format 'short)
              eglot-send-changes-idle-time 0.5))

;;(setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
;;;;(setq eglot-ignored-server-capabilities
;;;;      '(:documentOnTypeFormattingProvider
;;;;        :documentFormattingProvider
;;;;        :documentRangeFormattingProvider))

(use-package consult-eglot
  :after consult eglot
  :bind (:map eglot-mode-map
         ("C-M-." . consult-eglot-symbols)))

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
