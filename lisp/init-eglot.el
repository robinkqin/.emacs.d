;;; init-eglot.el --- eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
  :after consult eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

;;;; Emacs LSP booster
;;(use-package eglot-booster
;;  :when (and emacs/>=29p (executable-find "emacs-lsp-booster"))
;;  :ensure nil
;;  :init (unless (package-installed-p 'eglot-booster)
;;          (package-vc-install "https://github.com/jdtsmith/eglot-booster"))
;;  :hook (after-init . eglot-booster-mode))

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
