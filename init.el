;;; init.el --- Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; alias ec='emacsclient -t -a ""'
;; definition/reference: xref, eglot
;; completion: corfu
;; format: format-all
;; refactor: eglot, color-rg, symbol-overlay
;; dict: fanyi, go-translate, immersive-translate

;; tools: git, make, clangd, clang-format, ripgrep, fd, cmake, xclip
;;        bear, marksman, fzf, shellcheck, translate-shell, aspell
;; pip: epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
;;      requests compiledb cmake-language-server pyflakes autopep8
;; check: clang++ main.cpp; clang main.c; clang++ -v; libstdc++.a; clang -v

;; compile_flags.txt, compile_commands.json:
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1
;; compiledb -n make
;; bear -- make

;; export PATH=$HOME/.local/bin:${PATH}
;; export TERM=xterm-256color
;; export COLORTERM=truecolor

;;(eglot-upgrade-eglot)
;;(treesit-auto-install-all)
;;(devdocs-install)

(when (version< emacs-version "28.1")
  (error "This requires Emacs 28.1 and above!"))

;; Optimize Garbage Collection for Startup
(setq gc-cons-threshold most-positive-fixnum)

;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)

;; Add "lisp" and "site-lisp" to the beginning of `load-path`
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; Add subdirectories inside "site-lisp" to `load-path`
(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.

Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Ensure these functions are called after `package-initialize`
(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; Initialize load paths explicitly
(update-load-path)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(require 'init-const)

(require 'init-package)

(require 'init-fonts)
(require 'init-base)

(require 'init-ui)
;;(require 'init-hydra)

(require 'init-edit)
(require 'init-completion)
;;(require 'init-snippet)

(require 'init-bookmark)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-workspace)
(require 'init-window)

;;(require 'init-markdown)
;;(require 'init-org)
;;(require 'init-reader)

(require 'init-dict)
(require 'init-utils)

(require 'init-vcs)
;;(require 'init-check)
(require 'init-eglot)
(require 'init-gdb)
;;(require 'init-dap)

(require 'init-program)
(require 'init-c)

;;(require 'init-matchit)
;;(require 'init-citre)

(require 'init-elisp)
(require 'init-python)

;;(require 'init-eshell)
(require 'init-shell)

(require 'init-misc)

(require 'init-ai)

(require 'init-functions)
(require 'init-keymaps)

;; Load `custom-file'
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
