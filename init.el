;;
;; Initialize package management
;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;
;; After init hook
;;
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()

  ;;
  ;; Make sure all packages are installed
  ;; 
  (defun ensure-package-installed (&rest packages)
    "Assure every package is installed, ask for installation if it’s not.
     Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       ;; (package-installed-p 'evil)
       (if (package-installed-p package)
	   nil
	 (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	     (package-install package)
	   package)))
     packages))

  ;; make sure to have downloaded archive description.
  ;; Or use package-archive-contents as suggested by Nicolas Dudebout
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  (ensure-package-installed 
   'cider
   'clojure-mode
   'exec-path-from-shell
   'flycheck
   'neotree
   'js2-mode
   'paredit
   'helm
   'skewer-mode
   'tabbar
   'magit
   'auctex
   'reftex
   )

  ;;
  ;; Python
  ;;
  ;; (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/")
  ;; (autoload 'pymacs-apply "pymacs")
  ;; (autoload 'pymacs-call "pymacs")
  ;; (autoload 'pymacs-eval "pymacs" nil t)
  ;; (autoload 'pymacs-exec "pymacs" nil t)
  ;; (autoload 'pymacs-load "pymacs" nil t)

  ;; (require 'pymacs)
  ;; (pymacs-load "ropemacs" "rope-")

  ;;
  ;; Helm mode
  ;;
  (global-set-key (kbd "C-c h") 'helm-mini)
  ;(helm-mode 1)

  ;;
  ;; OS X specifics
  ;;
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  ;;
  ;; Autocomplete
  ;;
  ; (require 'auto-complete)
  ; (global-auto-complete-mode)
  (global-company-mode)

  ;;
  ;; Programming stuff
  ;;
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'hl-line-mode)

  ;;
  ;; Paredit stuff
  ;;
  (eval-after-load "paredit"
    '(progn
       (define-key paredit-mode-map (kbd "<C-right>") nil)
       (define-key paredit-mode-map (kbd "<C-left>") nil)
       )
    )

  ;;
  ;; Lisp stuff
  ;;
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)

  ;;
  ;; Clojure stuff
  ;;
  (require 'clojure-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-mode-hook 
	    '(lambda () (add-hook 'after-save-hook 
				  '(lambda () 
				     (if (and (boundp 'cider-mode) cider-mode)
					 (cider-namespace-refresh)
				       )))))

  (defun cider-namespace-refresh ()
    (interactive)
    (cider-interactive-eval
     "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

  (define-key clojure-mode-map (kbd "C-c C-r") 'cider-namespace-refresh)

  ;;
  ;; LaTeX stuff
  ;;
  (load "auctex.el" nil t t)

  (add-hook 'TeX-mode-hook 'linum-mode)
  (add-hook 'TeX-mode-hook 'hl-line-mode)
  (add-hook 'TeX-mode-hook 'flyspell-mode)

  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
	   (change (if (string= dic "deutsch") "english" "deutsch")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)
      ))
  ;;
  ;; Global key shortcuts:  
  ;;
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (define-key c-mode-base-map [f3] 'ff-find-other-file)))
  (global-set-key (kbd "<S-f3>") 'find-tag)
  (global-set-key [f4] (lambda() (interactive) (switch-to-buffer (other-buffer (current-buffer) nil))))
  (global-set-key [f5] 'tabbar-backward-tab)
  (global-set-key [f6] 'tabbar-forward-tab)
  (global-set-key (kbd "<f7>")   'fd-switch-dictionary)
  (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  (global-set-key [f8] (lambda() (interactive) (kill-buffer (current-buffer))))
  (global-set-key [f9] 'compile)
  (global-set-key "\M-?" 'grep)
  (global-set-key "\M-n" 'next-error)
  (global-set-key "\M-\S-n" 'first-error)
  (global-set-key "\M-<" (lambda() (interactive) (other-frame 1)))
  (global-set-key "\M->" (lambda() (interactive) (other-frame -1)))
  (global-set-key (kbd "M-/") 'company-complete)
  (global-set-key (kbd "C-x C-o") '(lambda() (interactive (other-window -1))))

  ;;
  ;; Emacs server
  ;;
  (server-start)

  ;;
  ;; Neotree
  ;;
  (neotree)
  
  ;;
  ;; Customize
  ;;
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(column-number-mode t)
   '(custom-enabled-themes (quote (misterioso)))
   '(global-auto-complete-mode t)
   '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
   '(ns-alternate-modifier (quote none))
   '(ns-command-modifier (quote meta))
   '(scroll-conservatively 1000)
   '(show-paren-mode t)
   '(truncate-lines t)
   '(visible-bell t)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso)))
 '(global-auto-complete-mode t)
 '(haskell-mode-hook (quote (turn-on-haskell-indent flycheck-mode)))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(scroll-conservatively 1000)
 '(show-paren-mode t)
 '(truncate-lines t)
 '(visible-bell t))
