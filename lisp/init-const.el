;;; init-const.el --- Define constants.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a Windows system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above?")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

(defconst emacs/>=31p
  (>= emacs-major-version 31)
  "Emacs is 31 or above.")

(defun my/treesit-available-p ()
  "Check whether tree-sitter is available.
  Native tree-sitter is introduced since 29.1."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (and (>= emacs-major-version 26)
       nil ;; force disable childframe
       (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and nil
       (childframe-workable-p)))

(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
