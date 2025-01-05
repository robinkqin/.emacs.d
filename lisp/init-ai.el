;; init-ai.el --- AI...	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI...
;;

;;; Code:

(use-package gptel
  :ensure t
  :config
  (setq gptel-model   'deepseek-chat
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key "your-api-key"  ;can be a function that returns the key
          :models '(deepseek-chat deepseek-coder))))


;;https://github.com/Aider-AI/aider.git
;;https://github.com/tninja/aider.el.git
;;how to install:
;;python -m pip install aider-install
;;aider-install

;;(when (executable-find "aider")
;;  (message "load aider...")
;;  (use-package aider
;;    ;;:straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;    :ensure nil
;;    :load-path "~/data/workbench/emacs/ai/aider.el/"
;;    :config
;;    ;; use DeepSeek model
;;    ;; (setq aider-args '("--model" "deepseek/deepseek-chat"))
;;    ;; (setenv "DEEPSEEK_API_KEY" "your-api-key")
;;    ;; Or Use claude-3-5-sonnet cause it is best in aider benchmark
;;    ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
;;    ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;;    ;; Or use chatgpt model since it is most well known
;;    ;; (setq aider-args '("--model" "gpt-4o-mini"))
;;    ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
;;    ;; Or use gemini v2 model since it is very good and free
;;    ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
;;    ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
;;    ;;
;;    ;; Optional: Set a key binding for the transient menu
;;    (global-set-key (kbd "C-c a") 'aider-transient-menu))
;;  (message "aider loaded..."))


(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
