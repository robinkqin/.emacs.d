;; Package: yasnippet
;; GROUP: Editing -> Yasnippet
;; Package: yasnippet
(use-package yasnippet
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;; company
(use-package company
  :defer t
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

(provide 'setup-company)
