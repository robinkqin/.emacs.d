;; init-ai.el --- AI...	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI...
;;

;;; Code:

;;;; Interact with ChatGPT or other LLMs
;;(use-package gptel
;;  :functions gptel-make-openai
;;  :custom
;;  (gptel-model 'gpt-4o)
;;  ;; Put the apikey to `auth-sources'
;;  ;; Format: "machine {HOST} login {USER} password {APIKEY}"
;;  ;; The LLM host is used as HOST, and "apikey" as USER.
;;  (gptel-backend (gptel-make-openai "Github Models"
;;                   :host "models.inference.ai.azure.com"
;;                   :endpoint "/chat/completions?api-version=2024-05-01-preview"
;;                   :stream t
;;                   :key 'gptel-api-key
;;                   :models '(gpt-4o))))
;;
;;;; Generate commit messages for magit
;;(use-package gptel-magit
;;  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(use-package agent-shell
  :diminish agent-shell-ui-mode
  :commands agent-shell-insert
  :custom (agent-shell-display-action '(display-buffer-reuse-window)))

;;(use-package copilot-chat)

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
