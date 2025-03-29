;; init-eaf.el --- EAF...	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; EAF...
;;

;;; Code:

(defconst my/eaf-path (expand-file-name "~/data/workbench/emacs/emacs-application-framework/"))

;;(use-package eaf
;;  :load-path my/eaf-path
;;  :custom
;;  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;  (eaf-browser-continue-where-left-off t)
;;  (eaf-browser-enable-adblocker t)
;;  (browse-url-browser-function 'eaf-open-browser)
;;  :config
;;  (defalias 'browse-web #'eaf-open-browser)
;;  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

(when (file-exists-p my/eaf-path)
  (add-to-list 'load-path my/eaf-path)
  (require 'eaf)
  (require 'eaf-vue-tailwindcss)
  (require 'eaf-file-manager)
  (require 'eaf-jupyter)
  (require 'eaf-js-video-player)
  (require 'eaf-camera)
  (require 'eaf-org-previewer)
  (require 'eaf-git)
  (require 'eaf-demo)
  (require 'eaf-markdown-previewer)
  (require 'eaf-2048)
  (require 'eaf-pyqterminal)
  (require 'eaf-map)
  (require 'eaf-file-browser)
  (require 'eaf-music-player)
  (require 'eaf-browser)
  (require 'eaf-file-sender)
  (require 'eaf-mindmap)
  (require 'eaf-system-monitor)
  (require 'eaf-pdf-viewer)
  (require 'eaf-video-player)
  (require 'eaf-image-viewer)
  (require 'eaf-rss-reader)
  (require 'eaf-terminal)
  (require 'eaf-vue-demo)
  (require 'eaf-markmap)
  (require 'eaf-airshare)
  ;;(setq eaf-enable-debug nil)
  (message "eaf loaded..."))

(provide 'init-eaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
