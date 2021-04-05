(require 'package)

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "proxy.com:8080")
;;         ("https" . "proxy.com:8080")))
;;
;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;;                   (cons "Input your LDAP UID !"
;;                         (base64-encode-string "USERNAME:PASSWORD")))))

(setq package-archives '(("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

;;(unless (package-installed-p 'gruvbox-theme)
;;  (package-install 'gruvbox-theme))
;;(require 'gruvbox-theme)
;;(load-theme 'gruvbox)
(load-theme 'tango-dark)

(require 'setup-general)
(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-ggtags)
(require 'setup-company)
(require 'setup-evil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   '(helm-gtags helm-projectile helm-swoop helm yasnippet projectile company use-package smooth-scrolling better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
