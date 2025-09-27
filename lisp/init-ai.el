;; init-ai.el --- AI...	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI...
;;

;;; Code:


;;https://aider.chat/docs/install.html
;;https://github.com/Aider-AI/aider.git
;;how to install:
;;python -m pip install aider-install
;;aider-install

;;Set API_KEY in .bashrc
;;export OPENAI_API_KEY="your_api_key_here"
;;export GITHUB_TOKEN="your_github_token_here"

;;;;1. create api_key file
;;touch ~/.api_keys
;;chmod 600 ~/.api_keys
;;;;2. add to ~/.api_keys
;;export OPENAI_API_KEY="your_api_key_here"
;;export GITHUB_TOKEN="your_github_token_here"
;;;;3. add to .bashrc
;;if [ -f ~/.api_keys ]; then
;;    source ~/.api_keys
;;fi

;;https://github.com/MatthewZMD/aidermacs
;;https://emacs-china.org/t/aidermacs-emacs-ai-emacs-aider/28916
;;(use-package aidermacs
;;  :bind (("C-c a" . aidermacs-transient-menu))
;;  ;;:config
;;  ;;;; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
;;  ;;(setenv "ANTHROPIC_API_KEY" "sk-...")
;;  ;;;; defun my-get-openrouter-api-key yourself elsewhere for security reasons
;;  ;;(setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
;;  :custom
;;  ;; See the Configuration section below
;;  (aidermacs-use-architect-mode t)
;;  (aidermacs-default-model "sonnet"))


;;https://github.com/tninja/aider.el
;;https://emacs-china.org/t/ai-aider-emacs-aider-el/28064
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


;;https://emacs-china.org/t/gptel-0-98-tool-use/29171
;;(use-package gptel
;;  :ensure t
;;  :config
;;  (setq gptel-model   'deepseek-chat
;;        gptel-backend
;;        (gptel-make-openai "DeepSeek"
;;          :host "api.deepseek.com"
;;          :endpoint "/chat/completions"
;;          :stream t
;;          :key "your-api-key"  ;can be a function that returns the key
;;          :models '(deepseek-chat deepseek-coder))))

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

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
