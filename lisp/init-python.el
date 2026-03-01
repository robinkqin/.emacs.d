;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

;; Python Mode
(use-package python
  :ensure nil
  :defines eglot-server-programs
  :functions exec-path-from-shell-copy-env
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Type checker & language server: `ty'
  (when (executable-find "ty")
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '((python-mode python-ts-mode)
                     . ("ty" "server")))))

  ;; Linter & formatter: `ruff'
  (when (executable-find "ruff")
    (use-package flymake-ruff
      :hook (python-base-mode . flymake-ruff-load)))

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
