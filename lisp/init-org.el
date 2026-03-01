;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defconst my/org-directory (expand-file-name "~/data/org"))

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  ;; To speed up startup, don't put to init section
  (setq org-modules nil                 ; Faster loading
        org-directory my/org-directory
        org-capture-templates
        `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+olp+datetree
                                ,(concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("b" "Book" entry (file+olp+datetree
                             ,(concat org-directory "/book.org"))
	       "* Topic: %^{Description}  %^g %? Added: %U"))

        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        ;; Agenda styling
        org-agenda-files (list my/org-directory)
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (add-to-list 'org-file-apps
               '("\\.\\(x?html?\\|pdf\\)\\'"
                 .
                 (lambda (file _link)
                   (my/browse-url-of-file (browse-url-file-url file)))))

  ;; Add md/gfm backends
  (add-to-list 'org-export-backends 'md)
  (use-package ox-gfm
    :init (add-to-list 'org-export-backends 'gfm))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (perl       . t)
      (python     . t)
      (ruby       . t)
      (js         . t)
      (css        . t)
      (sass       . t)
      (C          . t)
      (java       . t)
      (shell      . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-powershell
    :init (cl-pushnew '(powershell . t) load-language-alist))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-alist))

  ;; Install: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist))

;; Prettify UI
(use-package org-modern
  :after org
  :diminish
  :autoload global-org-modern-mode
  :init (global-org-modern-mode 1))

;; Paste with org-mode markup and link
(use-package org-rich-yank
  :after org
  :diminish
  :bind (:map org-mode-map
         ("C-M-y" . org-rich-yank)))

;; Auto-toggle Org elements
(use-package org-appear
  :diminish
  :hook org-mode
  :custom
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-inside-latex t)
  (org-appear-manual-linger t)
  (org-appear-delay 0.5))

;; Table of contents
(use-package toc-org
  :diminish
  :hook org-mode)

;; Preview
(use-package org-preview-html
  :after org
  :diminish
  :functions xwidget-workable-p
  :bind (:map org-mode-map
         ("C-c C-h" . org-preview-html-mode))
  :init (when (xwidget-workable-p)
          (setq org-preview-html-viewer 'xwidget)))

;; Presentation
(if emacs/>=29.2p
    (use-package dslide
      :after org
      :diminish
      :bind (:map org-mode-map
             ("s-<f7>" . dslide-deck-start)))
  (use-package org-tree-slide
    :after org
    :diminish
    :defines org-tree-slide-mode-map
    :bind (:map org-mode-map
           ("s-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :custom (org-tree-slide-skip-outline-level 3)))

;; Pomodoro
(use-package org-pomodoro
  :after org
  :diminish
  :custom-face
  (org-pomodoro-mode-line ((t (:inherit warning))))
  (org-pomodoro-mode-line-overtime ((t (:inherit error))))
  (org-pomodoro-mode-line-break ((t (:inherit success))))
  :bind (:map org-mode-map
         ("C-c C-x m" . org-pomodoro))
  :init (with-eval-after-load 'org-agenda
          (bind-keys :map org-agenda-mode-map
            ("K" . org-pomodoro)
            ("C-c C-x m" . org-pomodoro))))

;; Roam
(when (and (fboundp 'sqlite-available-p) (sqlite-available-p))
  (use-package org-roam
    :diminish
    :functions my/browse-url org-roam-db-autosync-mode
    :defines org-roam-graph-viewer
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today))
    :init
    (setq org-roam-directory my/org-directory
          org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
          org-roam-graph-viewer #'my/browse-url)
    :config
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))
    (add-to-list 'org-agenda-files org-roam-directory)

    ;; Keep Org-roam session automatically synchronized
    (org-roam-db-autosync-mode))

  (use-package org-roam-ui
    :bind ("C-c n u" . org-roam-ui-mode)
    :init (setq org-roam-ui-browser-function #'my/browse-url)))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
