;; init-dap.el --- Initialize DAP configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Debug Adapter Protocol (DAP) configurations.
;;

;;; Code:

;;(eval-when-compile
;;  (require 'init-const))

(when emacs/>=29p
  (use-package dape
    :bind (("<f5>" . dape))
    :custom (dape-buffer-window-arrangment 'right)
    :config
    ;; Save buffers on startup, useful for interpreted languages
    (add-hook 'dape-on-start-hooks
              (defun dape--save-on-start ()
                (save-some-buffers t t)))))

(provide 'init-dap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dap.el ends here
