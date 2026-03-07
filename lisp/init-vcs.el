;; init-vcs.el --- Initialize version control system.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Git
;; See `magit-define-global-key-bindings'
(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  (git-commit-major-mode 'git-commit-elisp-text-mode)
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Unbind conflicting shortcuts due to conflict with `ace-window'
  (unbind-key "M-1" magit-mode-map)
  (unbind-key "M-2" magit-mode-map)
  (unbind-key "M-3" magit-mode-map)
  (unbind-key "M-4" magit-mode-map))

;; Show TODOs in Magit
(use-package magit-todos
  :after magit-status
  :commands magit-todos-mode
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (magit-todos-mode 1))

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
