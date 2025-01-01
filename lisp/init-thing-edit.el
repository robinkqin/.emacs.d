;;; init-thing-edit.el --- Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;https://github.com/manateelazycat/thing-edit
(require 'thing-edit)

;;https://github.com/lyjdwh/avy-thing-edit?tab=readme-ov-file
(require 'avy-thing-edit)

(defvar my/thing-edit-map (make-sparse-keymap)
  "Keybinding for `thing-edit'")
;; Copy.
(define-key my/thing-edit-map (kbd "w") 'thing-copy-word)
(define-key my/thing-edit-map (kbd "s") 'thing-copy-symbol)
(define-key my/thing-edit-map (kbd "m") 'thing-copy-email)
(define-key my/thing-edit-map (kbd "f") 'thing-copy-filename)
(define-key my/thing-edit-map (kbd "u") 'thing-copy-url)
(define-key my/thing-edit-map (kbd "x") 'thing-copy-sexp)
(define-key my/thing-edit-map (kbd "t") 'thing-copy-page)
(define-key my/thing-edit-map (kbd "v") 'thing-copy-sentence)
;;(define-key my/thing-edit-map (kbd "g") 'thing-copy-block)
(define-key my/thing-edit-map (kbd "o") 'thing-copy-whitespace)
(define-key my/thing-edit-map (kbd "i") 'thing-copy-list)
(define-key my/thing-edit-map (kbd "c") 'thing-copy-comment)
(define-key my/thing-edit-map (kbd "h") 'thing-copy-defun)
(define-key my/thing-edit-map (kbd "p") 'thing-copy-parentheses)
(define-key my/thing-edit-map (kbd "l") 'thing-copy-region-or-line)
(define-key my/thing-edit-map (kbd "a") 'thing-copy-to-line-beginning)
(define-key my/thing-edit-map (kbd "e") 'thing-copy-to-line-end)
;; Cut.
(define-key my/thing-edit-map (kbd "SPC w") 'thing-cut-word)
(define-key my/thing-edit-map (kbd "SPC s") 'thing-cut-symbol)
(define-key my/thing-edit-map (kbd "SPC m") 'thing-cut-email)
(define-key my/thing-edit-map (kbd "SPC f") 'thing-cut-filename)
(define-key my/thing-edit-map (kbd "SPC u") 'thing-cut-url)
(define-key my/thing-edit-map (kbd "SPC x") 'thing-cut-sexp)
(define-key my/thing-edit-map (kbd "SPC t") 'thing-cut-page)
(define-key my/thing-edit-map (kbd "SPC v") 'thing-cut-sentence)
;;(define-key my/thing-edit-map (kbd "SPC g") 'thing-cut-block)
(define-key my/thing-edit-map (kbd "SPC o") 'thing-cut-whitespace)
(define-key my/thing-edit-map (kbd "SPC i") 'thing-cut-list)
(define-key my/thing-edit-map (kbd "SPC c") 'thing-cut-comment)
(define-key my/thing-edit-map (kbd "SPC h") 'thing-cut-defun)
(define-key my/thing-edit-map (kbd "SPC l") 'thing-cut-region-or-line)
(define-key my/thing-edit-map (kbd "SPC p") 'thing-cut-parentheses)
(define-key my/thing-edit-map (kbd "SPC a") 'thing-cut-to-line-beginning)
(define-key my/thing-edit-map (kbd "SPC e") 'thing-cut-to-line-end)
;; Replace
(define-key my/thing-edit-map (kbd "W") 'thing-replace-word)
(define-key my/thing-edit-map (kbd "S") 'thing-replace-symbol)
(define-key my/thing-edit-map (kbd "M") 'thing-replace-email)
(define-key my/thing-edit-map (kbd "F") 'thing-replace-filename)
(define-key my/thing-edit-map (kbd "U") 'thing-replace-url)
(define-key my/thing-edit-map (kbd "X") 'thing-replace-sexp)
(define-key my/thing-edit-map (kbd "T") 'thing-replace-page)
(define-key my/thing-edit-map (kbd "V") 'thing-replace-sentence)
;;(define-key my/thing-edit-map (kbd "G") 'thing-replace-block)
(define-key my/thing-edit-map (kbd "O") 'thing-replace-whitespace)
(define-key my/thing-edit-map (kbd "I") 'thing-replace-list)
(define-key my/thing-edit-map (kbd "C") 'thing-replace-comment)
(define-key my/thing-edit-map (kbd "H") 'thing-replace-defun)
(define-key my/thing-edit-map (kbd "P") 'thing-replace-parentheses)
(define-key my/thing-edit-map (kbd "L") 'thing-replace-region-or-line)
(define-key my/thing-edit-map (kbd "A") 'thing-paste-to-line-beginning)
(define-key my/thing-edit-map (kbd "E") 'thing-paste-to-line-end)

(global-set-key (kbd "M-h") my/thing-edit-map)


;;(require 'markmacro)
;;(global-unset-key (kbd "M-m"))
;;(global-set-key (kbd "M-m w") 'markmacro-mark-words)
;;(global-set-key (kbd "M-m l") 'markmacro-mark-lines)
;;(global-set-key (kbd "M-m c") 'markmacro-mark-chars)
;;(global-set-key (kbd "M-m i") 'markmacro-mark-imenus)
;;(global-set-key (kbd "M-m a") 'markmacro-apply-all)
;;(global-set-key (kbd "M-m b") 'markmacro-apply-all-except-first)
;;(global-set-key (kbd "M-m r") 'markmacro-rect-set)
;;(global-set-key (kbd "M-m d") 'markmacro-rect-delete)
;;(global-set-key (kbd "M-m f") 'markmacro-rect-replace)
;;(global-set-key (kbd "M-m k") 'markmacro-rect-insert)
;;(global-set-key (kbd "M-m j") 'markmacro-rect-mark-columns)
;;(global-set-key (kbd "M-m s") 'markmacro-rect-mark-symbols)


(provide 'init-thing-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-thing-edit.el ends here
