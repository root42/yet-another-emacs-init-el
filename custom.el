;;
;; Customize
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 3)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd"))))
 '(c-offsets-alist (quote ((arglist-intro . +) (arglist-close . 0))))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso)))
 '(electric-indent-mode t) '(ecb-layout-name "left3")
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(fill-column 110)
 '(global-auto-complete-mode t)
 '(global-subword-mode t)
 '(indent-tabs-mode nil)
 '(keyboard-coding-system (quote utf-8))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(nxml-attribute-indent 3)
 '(nxml-child-indent 3)
 '(nxml-outline-child-indent 3)
 '(scroll-conservatively 1000)
 '(sgml-basic-offset 3)
 '(show-paren-mode t)
 '(tabbar-mode t nil (tabbar))
 '(truncate-lines t)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((t (:background "DarkOrange1"))))
 '(ecb-directory-face ((t (:inherit ecb-default-highlight-face :background "DarkOrange1"))))
 '(hl-line ((t (:background "dark cyan" :distant-foreground "white")))))
