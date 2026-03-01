;; init-ai.el --- AI...	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI...
;;

;;; Code:

;; FIXME: Eager macro-expansion failure
;; @see https://github.com/karthink/gptel/issues/1272
;; Interact with ChatGPT or other LLMs
(use-package gptel
  :disabled
  :functions gptel-make-openai
  :custom
  (gptel-model 'gpt-4o)
  ;; Put the apikey to `auth-sources'
  ;; Format: "machine {HOST} login {USER} password {APIKEY}"
  ;; The LLM host is used as HOST, and "apikey" as USER.
  (gptel-backend (gptel-make-openai "Github Models"
                   :host "models.inference.ai.azure.com"
                   :endpoint "/chat/completions?api-version=2024-05-01-preview"
                   :stream t
                   :key 'gptel-api-key
                   :models '(gpt-4o))))

;; Generate commit messages for magit
(use-package gptel-magit
  :disabled
  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode
    :commands agent-shell-insert
    :defines magit-mode-map
    :functions (magit-message
                magit-commit-create magit-staged-files
                magit-commit-p magit-thing-at-point)
    :custom (agent-shell-display-action '(display-buffer-reuse-window))
    :bind (("<f12>" . agent-shell-toggle)
           :map magit-mode-map
           ("C-c C-g" . my/agent-shell-magit-generate-commit)
           ("C-c C-r" . my/agent-shell-review-magit-commit))
    :config
    (with-eval-after-load 'magit
      (defun my/agent-shell-magit-generate-commit ()
        "Generate conventional message and commit stage changes in magit."
        (interactive)
        (if-let ((changes (magit-staged-files)))
            (progn
              (magit-message "Generating commit...")
              (agent-shell-insert
               :submit t
               :text "Load git-commit skill. \
Generate conventional messages and commit the staged changes. \
Switch to the magit-status buffer and refresh it. \
Display a summary message in the echo area."))
          (user-error "No staged changes")))

      (defun my/agent-shell-review-magit-commit ()
        "Send the commit from magit to agent-shell for reviews."
        (interactive)
        (if-let ((commit (magit-commit-p (magit-thing-at-point 'git-revision t))))
            (agent-shell-insert
             :submit t
             :text (format "Review commit: %s" commit))
          (user-error "No magit commit at point"))))))

(use-package copilot-chat)

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
