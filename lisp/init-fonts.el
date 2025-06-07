;;; init-fonts.el --- fonts configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-macs) ;; for cl-loop

;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;;(set-clipboard-coding-system 'utf-8) ;FIXME: for windows
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
;;(set-selection-coding-system 'utf-8) ;FIXME: for windows
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(if sys/win32p
    (add-to-list 'process-coding-system-alist
                 '("cmdproxy" utf-8 . gbk))
  (set-selection-coding-system 'utf-8))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun my/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Jetbrains Mono" "Cascadia Code" "Fira Code"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 175)
                                                      (sys/win32p 135)
                                                      (t 235))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t
                                      (if (< emacs-major-version 28)'symbol 'emoji)
                                      (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
                           "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.0)))
                      (set-fontset-font t 'han (font-spec :family font))))))

(my/setup-fonts)
(add-hook 'window-setup-hook #'my/setup-fonts)
(add-hook 'server-after-make-frame-hook #'my/setup-fonts)

(defun my/set-frame-font (fontconfig)
  (interactive "sFontconfig(Ubuntu-20): ")
  (let ((font (or (if (string-empty-p fontconfig)
                      "Ubuntu-20"
                    fontconfig))))
    (set-face-attribute 'variable-pitch (selected-frame) :font font)))

(provide 'init-fonts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
