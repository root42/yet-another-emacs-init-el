;;; package --- Some custom setting defaults.

;;; Commentary:
;;; Copy this file to your .emacs.d directory and customize to your likings.

;;; Code:

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
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (nuance-misterioso)))
 '(electric-indent-mode t)
 '(ecb-compile-window-height 10)
 '(ecb-layout-name "left3")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(fill-column 110)
 '(global-auto-complete-mode t)
 '(global-subword-mode t)
 '(indent-tabs-mode nil)
 '(ispell-dictionary "deutsch")
 '(ispell-program-name "aspell")
 '(keyboard-coding-system (quote utf-8))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(nxml-attribute-indent 3)
 '(nxml-child-indent 3)
 '(nxml-outline-child-indent 3)
 '(scroll-conservatively 1000)
 '(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-traditional))
 '(semantic-completion-displayor-format-tag-function (quote ignore))
 '(server-mode t)
 '(sgml-basic-offset 3)
 '(show-paren-mode t)
 '(standard-indent 3)
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-scroll-left-button
   (quote
    (("[<]"
      (:type pbm :data "P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 128 16 48 255 255 255 255 255 255 255
255 144 28 86 128 0 255 255 255 255 255 255 160 44 92 159 135 113 0
255 255 255 255 160 44 97 165 144 129 120 117 0 255 255 176 44 98 175
174 146 127 126 127 128 0 255 255 0 160 184 156 143 136 134 135 137
138 0 255 255 176 32 67 144 146 144 145 146 148 149 0 255 255 255 255
160 42 75 140 154 158 159 160 0 255 255 255 255 255 255 160 40 74 154
170 171 0 255 255 255 255 255 255 255 255 160 41 82 163 0 255 255 255
255 255 255 255 255 255 255 160 32 48 255 255 255 255 255 255 255 255
255 255 255 255 255 255
" :ascent center :mask
(heuristic t)
:margin 1))
     "[=]")))
'(tabbar-scroll-right-button
(quote
 (("[>]"
   (:type pbm :data "P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
48 32 160 255 255 255 255 255 255 255 255 255 255 44 161 71 32 160 255
255 255 255 255 255 255 255 36 157 163 145 62 32 160 255 255 255 255
255 255 30 128 133 137 142 124 50 32 160 255 255 255 255 29 120 121
124 126 126 124 105 42 32 176 255 255 31 126 127 128 128 128 128 126
124 89 32 255 255 33 134 135 136 137 137 138 119 49 32 176 255 255 34
143 144 145 146 128 54 32 160 255 255 255 255 36 152 153 134 57 32 160
255 255 255 255 255 255 38 141 60 32 160 255 255 255 255 255 255 255
255 48 32 160 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255
" :ascent center :mask
(heuristic t)
:margin 1))
  "[=]")))
 '(tabbar-use-images nil)
 '(truncate-lines t)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :family "Source Code Pro")))))

;;; custom.el ends here
