;; init-check.el --- Initialize flymake configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package flymake
  :diminish
  :functions my/elisp-flymake-byte-compile
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook prog-mode
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-margin-indicator-position 'right-margin)
  :config
  ;; Check elisp with `load-path'
  (defun my/elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my/elisp-flymake-byte-compile))

;; Display Flymake errors with overlays
(use-package flyover
  :diminish
  :custom
  (flyover-checkers '(flymake))
  (flyover-background-lightness 60)
  (flyover-icon-background-tint-percent 50)
  (flyover-display-mode 'hide-on-same-line)
  :hook flymake-mode)

(provide 'init-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
