;;; init-gdb.el --- gdb configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;(pretty-hydra-define my/gud-hydra
;;  (:title (pretty-hydra-title "GUD Debug" 'codicon "nf-cod-debug")
;;   :color pink :quit-key ("q" "C-g"))
;;  ("Stepping"
;;   (("r" gud-cont "continue")
;;    ("s" gud-step "step")
;;    ("n" gud-next "next")
;;    ("t" gud-until "until")
;;    ("g" gud-go "go")
;;    ("f" gud-finish "finish")
;;    ("q" gud-stop-subjob "stop" :exit t))
;;
;;   "Breakpoints"
;;   (("b" gud-break "break")
;;    ("d" gud-remove "delete")
;;    ("l" gud-refresh "refresh"))
;;
;;   "Info"
;;   (("p" gud-print "print")
;;    ("u" gud-up "up")
;;    ("o" gud-down "down")
;;    ("w" gud-watch "watch")
;;    ("m" gdb-many-windows "toggle windows"))))
;;(global-set-key (kbd "M-<f6>") 'my/gud-hydra/body)

;; set gdb multi-windows when open
(setq gdb-many-windows t)

;;;; customize the gdb multi-windows
(defadvice gdb-setup-windows (after my/setup-gdb-windows activate)
  "MY GDB UI"
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil nil 'left))  ;; code and output
        (win2 (split-window-below (/ (* (window-height) 3) 4)))  ;; stack
        )
    (select-window win2)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win3 (split-window nil (/ (* (window-height) 3) 4))))  ;; io
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3))
    (select-window win0)
    ))


(provide 'init-gdb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gdb.el ends here
