;;; init-misc.el --- misc configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package pyim)
(use-package pyim-wbdict)

(setq default-input-method "pyim")
(setq pyim-page-length 9)

(require 'pyim-wbdict)
(setq pyim-default-scheme 'wubi)
(pyim-wbdict-v86-enable)           ;86版五笔用户使用这个命令
;; (pyim-wbdict-v98-enable)        ;98版五笔用户使用这个命令
;; (pyim-wbdict-v98-morphe-enable) ;98版五笔（单字）用户使用这个命令
;; (pyim-wbdict-v86-single-enable) ;86版五笔用户使用这个命令

;;(use-package pyim-basedict)
;;(require 'pyim-basedict)         ; 拼音词库设置，五笔用户 *不需要* 此行设置
;;(pyim-basedict-enable)           ; 拼音词库，五笔用户 *不需要* 此行设置


;;(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;;(use-package whitespace
;;  :ensure nil
;;  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
;;  :config
;;  (setq whitespace-style '(face trailing)))

;;(put 'narrow-to-region 'disabled nil)
;;;;;;(defun narrow-to-region-pop-mark (_ _) (pop-mark))
;;;;;;(advice-add #'narrow-to-region :after #'narrow-to-region-pop-mark)

(provide 'init-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
