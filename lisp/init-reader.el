;; init-reader.el --- Initialize readers.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; PDF/EPUB/RSS readers.
;;

;;; Code:

;; PDF reader
(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-themed-minor-mode
               pdf-view-midnight-minor-mode
               pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
           (pdf-tools-enabled . pdf-isearch-minor-mode))
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
           ("C-s" . isearch-forward))
    :init (setq pdf-view-use-scaling t
                pdf-view-use-imagemagick nil
                pdf-annot-activate-created-annotations t)
    :config
    ;; Activate the package
    (pdf-tools-install t nil t nil)

    ;; Recover last viewed position
    (use-package saveplace-pdf-view
      :when (ignore-errors (pdf-info-check-epdfinfo) t)
      :autoload (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
      :init
      (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
      (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))))

(provide 'init-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reader.el ends here
