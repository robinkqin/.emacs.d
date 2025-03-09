;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some usefule Utilities.
;;

;;; Code:

;; Display available keybindings in popup
(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  :config
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c @" "hideshow")
  (which-key-add-key-based-replacements "C-c c" "consult")
  (which-key-add-key-based-replacements "C-c d" "dict")
  (which-key-add-key-based-replacements "C-c l" "link-hint")
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  (which-key-add-key-based-replacements "C-c t" "hl-todo")
  (which-key-add-key-based-replacements "C-c C-z" "browse")

  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x 8 e" "emoji")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x c" "colorful")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x r" "rect & bookmark")
  (which-key-add-key-based-replacements "C-x t" "tab & treemacs")
  (which-key-add-key-based-replacements "C-x x" "buffer")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x X" "edebug")

  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c \"" "org-plot")
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c C-v" "org-babel")
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c C-x" "org-misc")

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c ," "overseer")
  (which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-x" "markdown-toggle")

  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-x" "markdown-toggle")

  (when (childframe-completion-workable-p)
    (use-package which-key-posframe
      :diminish
      :custom-face
      (which-key-posframe ((t (:inherit tooltip))))
      (which-key-posframe-border ((t (:inherit posframe-border :background unspecified))))
      :init
      (setq which-key-posframe-border-width 2
            which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-center
            which-key-posframe-parameters '((left-fringe . 8)
                                            (right-fringe . 8)))
      (which-key-posframe-mode 1))))

;;;; Persistent the scratch buffer
;;(use-package persistent-scratch
;;  :diminish
;;  :bind (:map persistent-scratch-mode-map
;;         ([remap kill-buffer] . (lambda (&rest _)
;;                                  (interactive)
;;                                  (user-error "Scratch buffer cannot be killed")))
;;         ([remap revert-buffer] . persistent-scratch-restore)
;;         ([remap revert-this-buffer] . persistent-scratch-restore))
;;  :hook ((after-init . persistent-scratch-autosave-mode)
;;         (lisp-interaction-mode . persistent-scratch-mode))
;;  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
;;              persistent-scratch-backup-directory
;;              (expand-file-name "persistent-scratch" user-emacs-directory)))

;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :init (setq rg-show-columns t)
  :config (add-to-list 'rg-custom-type-aliases '("tmpl" . "*.tmpl")))

;; Search
(use-package webjump
  :ensure nil
  :bind ("C-c /" . webjump)
  :init (setq webjump-sites
              '(;; Emacs
                ("Emacs Home Page" .
                 "www.gnu.org/software/emacs/emacs.html")
                ("Xah Emacs Site" . "ergoemacs.org/index.html")
                ("(or emacs irrelevant)" . "oremacs.com")
                ("Mastering Emacs" .
                 "https://www.masteringemacs.org/")

                ;; Search engines.
                ("DuckDuckGo" .
                 [simple-query "duckduckgo.com"
                               "duckduckgo.com/?q=" ""])
                ("Google" .
                 [simple-query "www.google.com"
                               "www.google.com/search?q=" ""])
                ("Bing" .
                 [simple-query "www.bing.com"
                               "www.bing.com/search?q=" ""])

                ("Baidu" .
                 [simple-query "www.baidu.com"
                               "www.baidu.com/s?wd=" ""])
                ("Wikipedia" .
                 [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

;;(use-package keyfreq
;;  :init
;;  (keyfreq-mode)
;;  (keyfreq-autosave-mode 1)
;;  (setq keyfreq-excluded-commands
;;        '(self-insert-command
;;          forward-char
;;          backward-char
;;          previous-line
;;          next-line)))

(provide 'init-utils)
