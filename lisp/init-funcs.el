;;; init-funcs.el --- functions configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-dos-eol ()
  "Delete `^M' characters in current region or buffer.
Same as `replace-string' `C-q' `C-m' `RET' `RET'."
  (interactive)
  (save-excursion
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "\r" nil t)
        (replace-match "" nil t)
        (setq count (1+ count)))
      (message "Removed %d " count))
    (widen)))

;; File and buffer
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

;; Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun selected-region-or-symbol-at-point ()
  "Return the selected region, otherwise return the symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun my/webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.

POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "URL: ")))
  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))

;; Browse URL
(defun my/browse-url (url)
  "Open URL using a configurable method.
See `browse-url' for more details."
  (interactive)
  (if (xwidget-workable-p)
      (my/webkit-browse-url url t)
    (browse-url url)))

(defun my/browse-url-of-file (file)
  "Use a web browser to display FILE.
Display the current buffer's file if FILE is nil or if called
interactively.  Turn the filename into a URL with function
`browse-url-file-url'.  Pass the URL to a browser using the
`browse-url' function then run `browse-url-of-file-hook'."
  (interactive)
  (if (xwidget-workable-p)
      (my/webkit-browse-url (browse-url-file-url file) t)
    (browse-url-of-file file)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

;; Rearrange split windows
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun my/open-init-file()
  "Open init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "<f12>") 'my/open-init-file)

(defun my/highlight-toggle ()
  "Toggle symbol highlighting using `highlight-symbol-at-point'."
  (interactive)
  (let ((regexp (find-tag-default-as-symbol-regexp))
        (highlighted (cl-find-if #'boundp
                                 '(hi-lock-interactive-lighters
                                   hi-lock-interactive-patterns))))
    (if (and highlighted (assoc regexp (symbol-value highlighted)))
        (unhighlight-regexp regexp)
      (highlight-symbol-at-point))))
(defun my/unhighlight-symbol-all ()
  "Remove all highlight symbols."
  (interactive)
  (unhighlight-regexp t))

(defun my/bookmark-at-point ()
  "Bookmark at point symbol."
  (interactive)
  (bookmark-set (thing-at-point 'symbol)))

;;(defun my/copy-symbol-at-point ()
;;  "Copy symbol at point."
;;  (interactive)
;;  (let ((sym (thing-at-point 'symbol)))
;;    (when sym
;;      (kill-new sym))))
;;(global-set-key (kbd "C-c y") 'my/copy-symbol-at-point)


(require 'project)
(defun my/project-root-dir (&optional interactive)
  "Return the root directory of the current project using only built-in project.el.
If not in a project, return `default-directory`.
If called INTERACTIVE, show the root in the minibuffer and copy to kill ring."
  (interactive "p")
  (let* ((project (project-current))
         (root (if (and project (fboundp 'project-root))
                   (project-root project)
                 default-directory)))
    (when interactive
      (message "Project root: %s" root)
      (kill-new root))
    root))

(defun my/dwim-other-window
    (dwim-fn &optional input-fn directory-fn)
  "Search symbol at point in current or other window.
DWIM-FN should be a function that takes one or two arguments:
 input and optionally directory.
INPUT-FN is a function to get the initial input (default: symbol at point).
DIRECTORY-FN is a function to get the directory (for project search)."
  (interactive)
  (let* ((input (if input-fn
                    (funcall input-fn)
                  (thing-at-point 'symbol t)))
         (directory (when directory-fn (funcall directory-fn))))
    (unless input
      (user-error "No symbol at point"))
    (let ((original-window (selected-window))
          (original-pos (point))
          (original-buffer (current-buffer))
          (success nil))
      (condition-case err
          (progn
            (if (one-window-p)
                (if directory
                    (funcall dwim-fn directory input)
                  (funcall dwim-fn input))
              (let ((target-window (next-window)))
                (select-window target-window)
                (if directory
                    (funcall dwim-fn directory input)
                  (funcall dwim-fn input))))
            (setq success t))
        ('quit
         ;; Restore original window, buffer, and point on quit
         (when (window-live-p original-window)
           (select-window original-window)
           (when (buffer-live-p original-buffer)
             (set-buffer original-buffer))
           (goto-char original-pos))
         (signal (car err) (cdr err))))
      (unless success
        (when (window-live-p original-window)
          (select-window original-window))))))

(defun my/consult-line-other-window ()
  "Search symbol at point in current or other window using 'consult-line'."
  (interactive)
  (my/dwim-other-window #'consult-line))

(defun my/consult-grep-other-window-project ()
  "Search symbol at point in current project using 'consult-ripgrep'.
Gets the project root from the target window to ensure correct project detection."
   (interactive)
   (my/dwim-other-window
    #'consult-ripgrep
    nil
   (lambda ()
     ;; Determine the target window (current or next)
     (let ((target-window (if (one-window-p) (selected-window) (next-window))))
       ;; Get project root in the context of the target window
       (with-selected-window target-window
         (my/project-root-dir))))))


(defun my/dwim-from-yank (dwim-fn &optional dir)
  "Use DWIM-FN to search for the most recent 'kill-ring' entry.
If DIR is non-nil, use it as the search directory.
If 'kill-ring' is empty, signal an error."
  (interactive)
  (if (null kill-ring)
      (user-error "kill-ring is empty")
    (let ((input (substring-no-properties (car kill-ring))))
      (if dir
          (funcall dwim-fn dir input)
        (funcall dwim-fn input)))))

(defun my/grep-from-yank ()
  "Grep the most recently yanked entry in the current project."
  (interactive)
  (my/dwim-from-yank #'consult-ripgrep (my/project-root-dir)))

(defun my/consult-line-from-yank ()
  "Search the most recently yanked entry in the current buffer."
  (interactive)
  (my/dwim-from-yank #'consult-line))


(require 'color-rg)
(setq color-rg-search-no-ignore-file nil)
(when sys/win32p
  (setq color-rg-command-prefix "powershell"))
(defun my/color-rg-search-symbol-in-project-from-ynak ()
  "Color-rg symbol at point."
  (interactive)
  (color-rg-search-input (substring-no-properties (car kill-ring))
                         (my/project-root-dir)))


(require 'xref)
(defun my--push-point-to-xref-marker-stack (&rest r)
  "Push R to stack."
  (xref-push-marker-stack (point-marker)))
(dolist (func '(find-function
                consult-imenu consult-imenu-multi
                consult-line consult-grep consult-git-grep consult-ripgrep
                consult-outline consult-eglot-symbols
                beginning-of-buffer end-of-buffer jump-to-register mark-whole-buffer
                beginend-prog-mode-goto-end beginend-prog-mode-goto-beginning
                mwim-beginning-of-code-or-line mwim-end-of-code-or-line
                ;;next-buffer previous-buffer switch-to-buffer describe-function
                describe-variable find-file-at-point xref-find-definitions
                gud-cont gud-finish gud-break gud-until
                gud-remove gud-up gud-down gud-go
                avy-goto-word-0 avy-goto-word-1
                embark-act keyboard-escape-quit
                embark-next-symbol embark-previous-symbol
                dumb-jump-go))
  (advice-add func :before 'my--push-point-to-xref-marker-stack))

(defun my/xref-find-references-at-point ()
  "Find references at point."
  (interactive)
  (xref-find-references (thing-at-point 'symbol)))

(defun my/xref-find-references-from-yank ()
  "Find references from yank."
  (interactive)
  (xref-find-references (substring-no-properties (car kill-ring))))

;;(defun my--pop-xref-marker-stack (&rest r)
;;  ;;(xref-pop-marker-stack)
;;  (message "xref go back"))
;;(dolist (func '(gud-break
;;                gud-remove))
;;  (advice-add func :after 'my--pop-xref-marker-stack))

;; https://emacs-china.org/t/xxx-thing-at-point/18047
(defvar my/fly-commands
  '(query-replace-regexp
    flush-lines keep-lines
    consult-line consult-man
    consult-eglot-symbols
    consult-grep consult-git-grep consult-ripgrep
    isearch-forward
    isearch-backward
    bookmark-set
    consult-bookmark
    highlight-regexp
    xref-find-references xref-find-apropos))
(defvar my/fly-back-commands
  '(self-insert-command
    ;;delete-forward-char kill-word kill-sexp
    ;;end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
    yank yank-pop org-yank))
(defun my/fly-back-to-present ()
  "Remove hook."
  (remove-hook 'pre-command-hook 'my/fly-back-to-present t)
  (cond ((and (memq last-command my/fly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command my/fly-back-commands)
         (delete-region (point) (point-max)))))
(defun my/fly-time-travel ()
  "Check command."
  (when (memq this-command my/fly-commands)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some
                                    (lambda (thing) (thing-at-point thing t))
                                    '(region url symbol))
                                   ;; '(symbol url region sexp))
                                   ""))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'my/fly-back-to-present nil t)))
(add-hook 'minibuffer-setup-hook #'my/fly-time-travel)


(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
