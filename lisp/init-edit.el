;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map))

  ;; For WSL
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic)
      (when (daemonp)
        (advice-add #'browse-url :override #'browse-url-generic)))))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows t
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;;;; Kill text between the point and the character CHAR
;;(use-package avy-zap
;;  :bind (("M-z" . avy-zap-to-char-dwim)
;;         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Quickly follow links
(use-package link-hint
  :bind (("M-o" . link-hint-open-link)
         ("C-c l o" . link-hint-open-link)
         ("C-c l c" . link-hint-copy-link))
  :init
  (with-eval-after-load 'embark
    (setq link-hint-action-fallback-commands
          (list :open (lambda ()
                        (condition-case _
                            (progn
                              (embark-dwim)
                              t)
                          (error
                           nil)))))))

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

;; Minor mode to aggressively keep your code always indented
;;(use-package aggressive-indent)

;;;; Show number of matches in mode-line while searching
;;(use-package anzu
;;  :diminish
;;  :bind (([remap query-replace] . anzu-query-replace)
;;         ([remap query-replace-regexp] . anzu-query-replace-regexp)
;;         :map isearch-mode-map
;;         ([remap isearch-query-replace] . anzu-isearch-query-replace)
;;         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
;;  :hook (after-init . global-anzu-mode))

;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Visual `align-regexp'
;;(use-package ialign)

;;;; Edit multiple regions in the same way simultaneously
;;(use-package iedit
;;  :defines desktop-minor-mode-table
;;  :bind (("C-;" . iedit-mode)
;;         ("C-x r RET" . iedit-rectangle-mode)
;;         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
;;         :map esc-map ("C-;" . iedit-execute-last-modification)
;;         :map help-map ("C-;" . iedit-mode-toggle-on-function))
;;  :config
;;  ;; Avoid restoring `iedit-mode'
;;  (with-eval-after-load 'desktop
;;    (add-to-list 'desktop-minor-mode-table
;;                 '(iedit-mode nil))))

;;;; Increase selected region by semantic units
;;(use-package expand-region
;;  :bind ("C-=" . er/expand-region)
;;  :config
;;  (when (my/treesit-available-p)
;;    (defun treesit-mark-bigger-node ()
;;      "Use tree-sitter to mark regions."
;;      (let* ((root (treesit-buffer-root-node))
;;             (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
;;             (node-start (treesit-node-start node))
;;             (node-end (treesit-node-end node)))
;;        ;; Node fits the region exactly. Try its parent node instead.
;;        (when (and (= (region-beginning) node-start) (= (region-end) node-end))
;;          (when-let* ((node (treesit-node-parent node)))
;;            (setq node-start (treesit-node-start node)
;;                  node-end (treesit-node-end node))))
;;        (set-mark node-end)
;;        (goto-char node-start)))
;;    (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node)))

;; Multiple cursors
;;(use-package multiple-cursors)

;;;; Smartly select region, rectangle, multi cursors
;;(use-package smart-region
;;  :hook (after-init . smart-region-on))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))


;; Treat undo history as a tree
(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

;;;; Goto last change
;;(use-package goto-chg
;;  :bind ("C-," . goto-last-change))

;;;; Handling capitalized subwords in a nomenclature
;;(use-package subword
;;  :ensure nil
;;  :diminish
;;  :hook ((prog-mode . subword-mode)
;;         (minibuffer-setup . subword-mode)))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
    :color amaranth :quit-key ("q" "C-g"))
   ("Fold"
    (("t" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("g" hs-toggle-hiding "toggle hiding")
     ("c" hs-cycle "cycle block")
     ("s" hs-show-block "show block")
     ("h" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ;;("C-v" pager-page-down "↘")
     ;;("M-v" pager-page-up "↖")
     ("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))))
  :bind (:map hs-minor-mode-map
         ("C-~" . hideshow-hydra/body)
         ("C-S-<escape>" . hideshow-hydra/body))
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

;; Copy&paste GUI clipboard from text terminal
(unless sys/win32p
  (use-package xclip
    :hook (after-init . xclip-mode)
    :config
    ;; HACK: fix bug in xclip-mode on WSL
    (when (eq xclip-method 'powershell)
      (setq xclip-program "powershell.exe"))

    ;; @see https://github.com/microsoft/wslg/issues/15#issuecomment-1796195663
    (when (eq xclip-method 'wl-copy)
      (set-clipboard-coding-system 'gbk) ; for wsl
      (setq interprogram-cut-function
            (lambda (text)
              (start-process "xclip"  nil xclip-program "--trim-newline" "--type" "text/plain;charset=utf-8" text))))))

;;;; Open files as another user
;;(unless sys/win32p
;;  (use-package sudo-edit))

;; Hanlde minified code
(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup nil))

;;(defun read-only-setup ()
;;  (read-only-mode))
;;(add-hook 'find-file-hook #'read-only-setup)


(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
