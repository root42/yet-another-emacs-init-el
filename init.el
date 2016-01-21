(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;;
;; Initialize package management
;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;
;; After init hook
;;
(add-hook 'emacs-startup-hook 'my-emacs-startup-hook)
(defun my-emacs-startup-hook ()

  ;;
  ;; Set up the coding system
  ;;
  (prefer-coding-system 'utf-8)
  ;; ;; Suggested by bbatsov in https://github.com/clojure-emacs/clojure-mode/issues/252
  ;; (set-language-environment 'utf-8)
  ;; (setq locale-coding-system 'utf-8)
  ;; (set-default-coding-systems 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)

  ;;
  ;; CEDET / Semantic
  ;;
  (load-file "~/bin/yet-another-emacs-init-el/cedet/cedet-devel-load.el")
  (semantic-mode 1)
  
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
         (package-install package)
         package
         ))
     packages))

  ;; make sure to have downloaded archive description.
  ;; Or use package-archive-contents as suggested by Nicolas Dudebout
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  (ensure-package-installed 
   'auctex
   'cider
   'clojure-mode
   'company
   'exec-path-from-shell
   'flycheck
   'ggtags
   'helm
   'jedi
   'js2-mode
   'magit
   'neotree
   'paredit
   'reftex
   'skewer-mode
   'tabbar
   'ecb
   )

  ;;
  ;; Customize stuff
  ;;
  (setq custom-file (expand-file-name "custom.el" (or (file-name-directory user-init-file) default-directory)))
  (load custom-file)

  ;;
  ;; Python
  ;;
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
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
    (exec-path-from-shell-initialize)
    (setq ns-right-alternate-modifier nil)
    (global-set-key (kbd "A-SPC") 'just-one-space)
    )

  ;;
  ;; Autocomplete
  ;;
  (global-company-mode)

  ;;
  ;; Programming stuff
  ;;
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'nxml-mode-hook 'linum-mode)
  (add-hook 'nxml-mode-hook 'hl-line-mode)
  ;; Uncomment when this bug is fixed: https://github.com/alpaker/Fill-Column-Indicator/issues/54
  ;(add-hook 'prog-mode-hook 'fci-mode)

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
  				  'my-cider-refresh)))

  (defun my-cider-refresh ()
    (if (and (boundp 'cider-mode) cider-mode)
        (cider-refresh 1)
      )
    )
  
  (define-key clojure-mode-map (kbd "C-c C-r") 'cider-refresh)

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
  ;; C++/C stuff
  ;;
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (and (derived-mode-p 'c-mode 'c++-mode 'java-mode) (require 'ggtags nil 'noerror))
		(ggtags-mode 1))))
  
  ;;
  ;; Global key shortcuts:  
  ;;
  (global-set-key [f1] 'cider-refresh)
  (global-set-key [f2] 'dabbrev-completion)
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
  (global-set-key (kbd "<C-return>") 'company-complete)
  (global-set-key (kbd "C-x C-o") '(lambda() (interactive (other-window -1))))

  ;;
  ;; Emacs server
  ;;
  (server-start)

  ;;
  ;; Neotree
  ;;
  ;(neotree)

  ;;
  ;; ECB
  ;;
  (setq ecb-examples-bufferinfo-buffer-name nil)
  (ecb-activate)
  
  ;;
  ;; Calendar
  ;;
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
		      :height 1.0)
  (setq calendar-intermonth-text
	'(propertize
	  (format "%2d"
		  (car
		   (calendar-iso-from-absolute
		    (calendar-absolute-from-gregorian (list month day year)))))
	  'font-lock-face 'calendar-iso-week-face))

  ;;
  ;; Compilation buffer
  ;;
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

)
