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
  ;; Key prefix descriptions
  (dolist (map '(("M-s h" . "highlight")
                 ("M-s s" . "symbol-overlay")
                 ("C-c &" . "yasnippet")
                 ("C-c @" . "hideshow")
                 ("C-c c" . "consult")
                 ("C-c d" . "dict")
                 ("C-c l" . "link-hint")
                 ("C-c n" . "org-roam")
                 ("C-c o" . "org")
                 ("C-c t" . "hl-todo")
                 ("C-c C-a" . "activities")
                 ("C-c C-z" . "browse")
                 ("C-x 8" . "unicode")
                 ("C-x 8 e" . "emoji")
                 ("C-x @" . "modifior")
                 ("C-x a" . "abbrev")
                 ("C-x c" . "colorful")
                 ("C-x n" . "narrow")
                 ("C-x p" . "project")
                 ("C-x r" . "rect & bookmark")
                 ("C-x t" . "tab & treemacs")
                 ("C-x w" . "window & highlight")
                 ("C-x w ^" . "window")
                 ("C-x C-a" . "edebug")
                 ("C-x RET" . "coding-system")
                 ("C-x X" . "edebug")
                 ("C-x v b" . "vc-branch")
                 ("C-x v M" . "vc-mergebase")))
    (which-key-add-key-based-replacements (car map) (cdr map)))

  ;; Mode-specific key replacements
  (dolist (mode-map '((org-mode
                       ("C-c \"" . "org-plot")
                       ("C-c C-v" . "org-babel")
                       ("C-c C-x" . "org-misc"))
                      (python-mode
                       ("C-c C-t" . "python-skeleton"))
                      (markdown-mode
                       ("C-c C-a" . "markdown-link")
                       ("C-c C-c" . "markdown-command")
                       ("C-c C-s" . "markdown-style")
                       ("C-c C-t" . "markdown-header")
                       ("C-c C-x" . "markdown-toggle"))
                      (gfm-mode
                       ("C-c C-a" . "markdown-link")
                       ("C-c C-c" . "markdown-command")
                       ("C-c C-s" . "markdown-style")
                       ("C-c C-t" . "markdown-header")
                       ("C-c C-x" . "markdown-toggle"))))
    (let ((mode (car mode-map))
          (maps (cdr mode-map)))
      (dolist (map maps)
        (which-key-add-major-mode-key-based-replacements
          mode (car map) (cdr map))))))

;; Show 'which-key' in child frame
(use-package which-key-posframe
  :diminish
  :functions childframe-completion-workable-p
  :commands which-key-posframe-mode
  :custom-face
  (which-key-posframe-border ((t (:inherit posframe-border :background unspecified))))
  :hook ((which-key-mode server-after-make-frame)
         .
         (lambda ()
           (if (childframe-completion-workable-p)
               (which-key-posframe-mode 1)
             (which-key-posframe-mode -1))))
  :init
  (setq which-key-posframe-border-width 2
        which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-center
        which-key-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8))))

;;;; Persistent the scratch buffer
;;(use-package persistent-scratch
;;  :diminish
;;  :bind (:map persistent-scratch-mode-map
;;         ([remap kill-buffer] . (lambda (&rest _)
;;                                  (interactive)
;;                                  (user-error "Scratch buffer cannot be killed")))
;;         ([remap revert-buffer] . persistent-scratch-restore)
;;         ([remap revert-buffer-quick] . persistent-scratch-restore))
;;  :hook ((after-init . persistent-scratch-autosave-mode)
;;         (lisp-interaction-mode . persistent-scratch-mode))
;;  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
;;              persistent-scratch-backup-directory
;;              (expand-file-name "persistent-scratch" user-emacs-directory)))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

;; Writable `grep' buffer
(use-package wgrep
  :init (setq wgrep-auto-save-buffer t
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

;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3
        proced-enable-color-flag t))

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

(use-package file-info
  :bind ("C-c c i" . file-info-show))
(use-package reveal-in-folder)

;; (use-package keyfreq
;;   :init
;;   (keyfreq-mode)
;;   (keyfreq-autosave-mode 1)
;;   (setq keyfreq-excluded-commands
;;         '(self-insert-command
;;           forward-char
;;           backward-char
;;           previous-line
;;           next-line)))

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
