;;; init-eglot.el --- eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(use-package eglot
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (eglot-ensure))))
            ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
     :init
     (setq read-process-output-max (* 1024 1024)) ; 1MB
     (setq eglot-autoshutdown t
           eglot-events-buffer-size 0
           eglot-send-changes-idle-time 0.5)
     :config
     (use-package consult-eglot
       :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

     ;; Emacs LSP booster
     (when (and emacs/>=29p (executable-find "emacs-lsp-booster"))
       ;;(unless (package-installed-p 'eglot-booster)
       ;;  (and (fboundp #'package-vc-install)
       ;;       (package-vc-install "https://github.com/jdtsmith/eglot-booster")))
       ;; https://github.com/blahgeek/emacs-lsp-booster
       ;; https://github.com/jdtsmith/eglot-booster
       (use-package eglot-booster
         :ensure nil
         :load-path "~/data/workbench/emacs/eglot-booster/" ;; FIXME:
         :autoload eglot-booster-mode
         :init (eglot-booster-mode 1))))

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
