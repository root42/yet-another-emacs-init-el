;;; package --- This is yet another Emacs init.el file

;;; Commentary:

;;; Code:

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(defvar user-true-init-file-directory (file-name-directory (file-truename user-init-file)))

;;
;; After init hook
;;
(add-hook 'after-init-hook 'my-emacs-startup-hook)
(defun my-emacs-startup-hook ()
  "The actual startup function."
  
  ;;
  ;; Initialize package management
  ;;
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; make sure to have downloaded archive description.
  (when (not package-archive-contents)
    (package-refresh-contents nil))

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
         (package-install package nil)
         package
         ))
     packages))

  (ensure-package-installed
   'auctex
   'auto-complete-clang
   'cider
   'clojure-mode
   'company
   'csv-mode
   'ecb
   'eclipse-theme
   'exec-path-from-shell
   'flycheck
   'flycheck-clang-analyzer
   'ggtags
   'helm
   ;; 'jedi ;; Needs working pip and jediserver
   'js2-mode
   'magit
   'markdown-mode
   'org-bullets
   'paredit
   'p4
   'reftex
   'skewer-mode
   'tabbar
   'use-package
   )

  ;;
  ;; Nuance setup
  ;;
  (load-file (format "%snuance.el" user-true-init-file-directory))
  
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
  (load-file (format "%scedet/cedet-devel-load.el" user-true-init-file-directory))
  (semantic-mode 1)
  (global-semantic-idle-completions-mode nil)
  
  ;;
  ;; Customize stuff
  ;;
  (setq custom-file (expand-file-name "custom.el" (or (file-name-directory user-init-file) default-directory)))
  (load custom-file)
  (global-set-key (kbd "<f1>") '(lambda () (interactive) (load custom-file)))

  ;;
  ;; Python
  ;;
  (add-hook 'python-mode-hook 'jedi:setup)
  (defvar jedi:complete-on-dot t)

  ;;
  ;; Helm mode
  ;;
  (global-set-key (kbd "C-c h") 'helm-mini)
  (helm-mode 1)

  ;;
  ;; Linux/Unix specifics
  ;;
  (when (memq window-system '(x))
    (set-fontset-font
     t 'symbol
     (font-spec :family "Symbola") nil 'prepend)    
    )
  
  ;;
  ;; OS X specifics
  ;;
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (setq ns-right-alternate-modifier nil)
    (global-set-key (kbd "A-SPC") 'just-one-space)
    (set-fontset-font
     t 'symbol
     (font-spec :family "Apple Color Emoji") nil 'prepend)
    )

  ;;
  ;; Autocomplete
  ;;
  (require 'auto-complete-clang)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (define-key ac-mode-map  [(control tab)] 'auto-complete)
  (defun ac-emacs-lisp-mode-setup ()
    (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers)))
  (defun my-ac-config ()
    (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (global-auto-complete-mode t))
  (defun my-ac-cc-mode-setup ()
    (setq ac-sources (append '(ac-source-clang) ac-sources)))
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (my-ac-config)
  
  ;(global-company-mode)
  (global-flycheck-mode)
  (use-package flycheck-clang-analyzer
               :ensure t
               :after flycheck
               :config (flycheck-clang-analyzer-setup))

  ;;
  ;; Text stuff
  ;; 
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'hl-line-mode)
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map (kbd "<C-down-mouse-3>") #'flyspell-correct-word)
       (define-key flyspell-mouse-map (kbd "<C-mouse-3>") 'undefined) ))
  
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
  (define-key clojure-mode-map (kbd "<f1>") 'cider-refresh)

  ;;
  ;; LaTeX stuff
  ;;
  (load "auctex.el" nil t t)

  (add-hook 'TeX-mode-hook 'linum-mode)
  (add-hook 'TeX-mode-hook 'hl-line-mode)
  (add-hook 'TeX-mode-hook 'flyspell-mode)

  (defun fd-switch-dictionary()
    "Switches ispell dictionary between english and german."
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
  (add-to-list 'auto-mode-alist '("\\.hdf\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.model\\'" . c++-mode))
 
  ;;
  ;; Global key shortcuts:  
  ;;
  (global-set-key (kbd "<f2>") 'dabbrev-completion)
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (define-key c-mode-base-map (kbd "<f3>") 'nuance-find-other-file)))
  (global-set-key (kbd "S-<f3>") 'ggtags-find-tag)
  (global-set-key (kbd "<f4>") (lambda() (interactive) (switch-to-buffer (other-buffer (current-buffer) nil))))
  (global-set-key (kbd "<f5>") 'tabbar-backward-tab)
  (global-set-key (kbd "<f6>") 'tabbar-forward-tab)
  (global-set-key (kbd "<f7>")   'fd-switch-dictionary)
  (global-set-key (kbd "C-S-<tab>") 'tabbar-backward-tab)
  (global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
  (global-set-key (kbd "<f8>") (lambda() (interactive) (kill-buffer (current-buffer))))
  (global-set-key (kbd "<f9>") 'compile)
  (global-set-key (kbd "C-<f9>") 'nuance-compile-current-module)
  (global-set-key (kbd "M-?") 'grep)
  (global-set-key (kbd "M-n") 'next-error)
  (global-set-key (kbd "M-S-n") 'first-error)
  (global-set-key (kbd "M->") (lambda() (interactive) (other-frame 1)))
  (global-set-key (kbd "M->") (lambda() (interactive) (other-frame -1)))
  (global-set-key (kbd "M-/") 'auto-complete)
  (global-set-key (kbd "C-<return>") 'auto-complete)
  (global-set-key (kbd "C-x C-o") '(lambda() (interactive (other-window -1))))
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "s-<left>") 'previous-buffer)
  (global-set-key (kbd "s-<right>") 'next-buffer)

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
  (when nuance-enable-ecb
    (ecb-activate))
  
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
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  ;;
  ;; Dired
  ;;
  (require 'dired)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)

  ;;
  ;; Flyspell
  ;;
  (eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)
     (define-key flyspell-mouse-map [down-mouse-2] nil)
     (define-key flyspell-mouse-map [mouse-2] nil)))

  ;;
  ;; org mode
  ;;
  ;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  ;;
  ;; xml mode
  ;;
  (add-to-list 'auto-mode-alist
              (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                    'nxml-mode))

  ;;
  ;; Buffer menu
  ;;
  (defun mouse-buffer-menu-alist (buffers)
  (let (tail
    (maxlen 0)
    head)
    (setq buffers
      (sort buffers
        (function (lambda (elt1 elt2)
                (string< (buffer-name elt1) (buffer-name elt2))))))
    (setq tail buffers)
    (while tail
      (or (eq ?\s (aref (buffer-name (car tail)) 0))
      (setq maxlen
        (max maxlen
             (length (buffer-name (car tail))))))
      (setq tail (cdr tail)))
    (setq tail buffers)
    (while tail
      (let ((elt (car tail)))
    (if (/= (aref (buffer-name elt) 0) ?\s)
        (setq head
          (cons
           (cons
            (format
             (format "%%-%ds  %%s%%s" maxlen)
             (buffer-name elt)
             (if (buffer-modified-p elt) "*" " ")
             (with-current-buffer elt
               (if buffer-read-only "%" " "))
                     )
            elt)
           head))))
      (setq tail (cdr tail)))
    ;; Compensate for the reversal that the above loop does.
    (nreverse head)))

  ;;
  ;; Revert all buffers
  ;;
  (defun revert-all-file-buffers ()
    "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
    (interactive)
    (dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (when (and filename
                   (not (buffer-modified-p buf)))
          (if (file-readable-p filename)
              ;; If the file exists and is readable, revert the buffer.
              (with-current-buffer buf
                (revert-buffer :ignore-auto :noconfirm :preserve-modes))
            ;; Otherwise, kill the buffer.
            (let (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buf)
              (message "Killed non-existing/unreadable file buffer: %s" filename))))))
    (message "Finished reverting buffers containing unmodified files."))

  )
