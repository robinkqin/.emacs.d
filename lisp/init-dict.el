;; init-dict.el --- Initialize dictionaries.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; A multi dictionaries interface
(use-package fanyi
  :bind (("C-c d f" . fanyi-dwim)
         ("C-c d d" . fanyi-dwim2)
         ("C-c d h" . fanyi-from-history)))

(use-package gt
  :bind (("C-c g"   . gt-translate)
         ("C-c G"   . gt-translate-prompt)
         ("C-c u"   . gt-use-text-utility)
         ("C-c d g" . gt-translate)
         ("C-c d G" . gt-translate-prompt)
         ("C-c d p" . gt-speak)
         ("C-c d s" . gt-setup)
         ("C-c d u" . gt-use-text-utility))
  :hook (after-load-theme . gt-configure)
  :init
  (setq gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.4)))
  :config
  (with-no-warnings
    (defun gt-configure ()
      "Set appearance and presets of go-translate."
      (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
            gt-pop-posframe-backcolor (face-background 'tooltip nil t)
            gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)

            gt-preset-translators
            `((default . ,(gt-translator
                           :taker (list (gt-taker :pick nil :if 'selection)
                                        (gt-taker :text 'paragraph
                                                  :if '(Info-mode help-mode helpful-mode devdocs-mode))
                                        (gt-taker :text 'buffer :pick 'fresh-word
                                                  :if (lambda (translatror)
                                                        (and (not (derived-mode-p 'fanyi-mode))
                                                             buffer-read-only)))
                                        (gt-taker :text 'word))
                           :engines (if (childframe-workable-p)
                                        (list (gt-bing-engine :if 'not-word)
                                              (gt-youdao-dict-engine :if 'word))
                                      (list (gt-bing-engine :if 'not-word)
                                            (gt-youdao-dict-engine :if 'word)
                                            (gt-youdao-suggest-engine :if 'word)
                                            (gt-google-engine :if 'word)))
                           :render (list (gt-posframe-pop-render
                                          :if (lambda (translator)
                                                (and (childframe-workable-p)
                                                     (not (derived-mode-p
                                                           'Info-mode
                                                           'help-mode
                                                           'helpful-mode
                                                           'devdocs-mode))
                                                     (not (member (buffer-name) '("COMMIT_EDITMSG")))))
                                          :frame-params (list :accept-focus nil
                                                              :width 70
                                                              :height 15
                                                              :left-fringe 16
                                                              :right-fringe 16
                                                              :border-width 1
                                                              :border-color gt-pin-posframe-bdcolor))
                                         (gt-overlay-render :if 'read-only)
                                         (gt-insert-render
                                          :if (lambda (translator)
                                                (member (buffer-name) '("COMMIT_EDITMSG"))))
                                         (gt-buffer-render))))
              (multi-dict . ,(gt-translator
                              :taker (gt-taker :prompt t)
                              :engines (list (gt-bing-engine)
                                             (gt-youdao-dict-engine)
                                             (gt-youdao-suggest-engine :if 'word)
                                             (gt-google-engine))
                              :render (gt-buffer-render)))
              (Text-Utility . ,(gt-text-utility
                                :taker (gt-taker :pick nil)
                                :render (gt-buffer-render))))))

    (gt-configure)

    (defun gt--translate (dict)
      "Translate using DICT from the preset translators."
      (gt-start (alist-get dict gt-preset-translators)))

    (defun gt-translate-prompt ()
      "Translate with prompt using the multiple dictionaries."
      (interactive)
      (gt--translate 'multi-dict))

    (defun gt-use-text-utility ()
      "Handle the texts with the utilities."
      (interactive)
      (gt--translate 'Text-Utility))))

(use-package immersive-translate
  :init
  ;; use translate-shell
  (setq immersive-translate-backend 'trans)

  ;; use Baidu Translation
  ;;(setq immersive-translate-backend 'baidu
  ;;      immersive-translate-baidu-appid "your-appid")

  ;; use ChatGPT
  ;; (setq immersive-translate-backend 'chatgpt
  ;;       immersive-translate-chatgpt-host "api.openai.com")

  ;;:hook
  ;;(add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
  ;;(add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)
  :bind (("C-c d a" . immersive-translate-abort)
         ("C-c d b" . immersive-translate-buffer)
         ("C-c d p" . immersive-translate-paragraph)
         ("C-c d c" . immersive-translate-clear)))

;;;; OSX dictionary
;;(when sys/macp
;;  (use-package osx-dictionary
;;    :bind (("C-c d i" . osx-dictionary-search-input)
;;           ("C-c d x" . osx-dictionary-search-pointer))))

(provide 'init-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
