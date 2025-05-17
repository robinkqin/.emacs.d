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
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (prettify-symbols-mode 1)))
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
  (when (xwidget-workable-p)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (my/webkit-browse-url (concat "file://" file) t)))
          org-file-apps))

  ;; Add md/gfm backends
  (add-to-list 'org-export-backends 'md)
  (use-package ox-gfm
    :init (add-to-list 'org-export-backends 'gfm))

  ;; Prettify UI
  (use-package org-modern
    :hook ((org-mode . org-modern-mode)
           (org-agenda-finalize . org-modern-agenda)
           (org-modern-mode . (lambda ()
                                "Adapt `org-modern-mode'."
                                ;; Disable Prettify Symbols mode
                                (setq prettify-symbols-alist nil)
                                (prettify-symbols-mode -1)))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      ;;(perl       . t)
      (python     . t)
      ;;(ruby       . t)
      ;;(js         . t)
      ;;(css        . t)
      ;;(sass       . t)
      (C          . t)
      ;;(java       . t)
      (shell      . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  ;;(use-package ob-go
  ;;  :init (cl-pushnew '(go . t) load-language-alist))
  ;;
  ;;(use-package ob-powershell
  ;;  :init (cl-pushnew '(powershell . t) load-language-alist))
  ;;
  ;;(use-package ob-rust
  ;;  :init (cl-pushnew '(rust . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  (use-package org-rich-yank
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
                ("C-c M-o" . org-mime-htmlize)
                :map org-mode-map
                ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Auto-toggle Org LaTeX fragments
  (use-package org-fragtog
    :diminish
    :hook (org-mode . org-fragtog-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish
    :bind (:map org-mode-map
                ("C-c C-h" . org-preview-html-mode))
    :init (when (featurep 'xwidget-internal)
            (setq org-preview-html-viewer 'xwidget)))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("s-<f7>" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
                ("<left>" . org-tree-slide-move-previous-tree)
                ("<right>" . org-tree-slide-move-next-tree)
                ("S-SPC" . org-tree-slide-move-previous-tree)
                ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :init (setq org-tree-slide-header nil
                org-tree-slide-slide-in-effect t
                org-tree-slide-heading-emphasis nil
                org-tree-slide-cursor-init t
                org-tree-slide-modeline-display 'outside
                org-tree-slide-skip-done nil
                org-tree-slide-skip-comments t
                org-tree-slide-skip-outline-level 3))

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-mode-map
                ("C-c C-x m" . org-pomodoro))
    :init
    (with-eval-after-load 'org-agenda
      (bind-keys :map org-agenda-mode-map
                 ("K" . org-pomodoro)
                 ("C-c C-x m" . org-pomodoro)))))

;; Roam
(when (and (fboundp 'sqlite-available-p) (sqlite-available-p))
  (use-package org-roam
    :diminish
    :functions my/browse-url org-roam-db-autosync-enable
    :defines org-roam-graph-viewer
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today))
    :init
    (setq org-roam-directory (file-truename my/org-directory)
          org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
          org-roam-graph-viewer #'my/browse-url)
    :config
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))
    (add-to-list 'org-agenda-files (format "%s/%s" org-roam-directory "roam"))

    (org-roam-db-autosync-enable))

  (use-package org-roam-ui
    :bind ("C-c n u" . org-roam-ui-mode)
    :init (setq org-roam-ui-browser-function #'my/browse-url)))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
