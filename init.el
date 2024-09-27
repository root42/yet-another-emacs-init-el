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
  (or (package-installed-p 'use-package)
      (package-install 'use-package))
  
  ;;
  ;; Set up the coding system
  ;;
  (prefer-coding-system 'utf-8)
  
  ;;
  ;; Customize stuff
  ;;
  (setq custom-file (expand-file-name "custom.el" (or (file-name-directory user-init-file) default-directory)))
  (load custom-file)
  (global-set-key (kbd "<f1>") #'(lambda () (interactive) (load custom-file)))
  (setq-default truncate-lines t)
  (setq native-comp-async-report-warnings-errors 'silent)

  ;;
  ;; Themes
  ;;
  (use-package modus-themes
    :ensure t)
  (load-theme 'modus-vivendi-tinted)
  
  ;;
  ;; Linux/Unix specifics
  ;;
  (when (memq window-system '(x))
    (set-fontset-font
     t 'symbol
     (font-spec :family "Symbola") nil 'prepend)
    )
  
  ;;
  ;; macOS specifics
  ;;
  (when (and (equal emacs-version "27.2")
             (eql system-type 'darwin))
    (defvar gnutls-algorithm-priority)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  (when (memq window-system '(mac ns))
    (use-package exec-path-from-shell
      :ensure t)
    (exec-path-from-shell-initialize)
    (defvar ns-right-alternate-modifier nil)
    (global-set-key (kbd "A-SPC") 'just-one-space)
    (defvar ns-alternate-modifier)
    (defvar ns-command-modifier)
    (setq ns-alternate-modifier 'alt)
    (setq ns-command-modifier 'meta)
    (set-fontset-font
     t 'symbol
     (font-spec :family "Apple Color Emoji") nil 'prepend)
    )
  (setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

  ;;
  ;; Scrolling
  ;;
  (setq scroll-margin 1
	scroll-step 1
	scroll-conservatively 10000
	scroll-preserve-screen-position 1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
  
  ;;
  ;; Text stuff
  ;;
  (setq-default indent-tabs-mode nil)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'hl-line-mode)
  
  ;;
  ;; Programming stuff
  ;;
  (add-hook 'nxml-mode-hook 'display-line-numbers-mode)
  (add-hook 'nxml-mode-hook 'hl-line-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
  (add-hook 'prog-mode-hook 'subword-mode)
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'highlight-symbol-mode)
  
  (defun comment-or-uncomment-line-or-region ()
    "Comments or uncomments the current line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      )
    )

  (use-package nano-theme
    :ensure t)
  (use-package nano-modeline
    :ensure t)
  (require 'nano-modeline)
  (defun my-nano-modeline-which-function (&optional name)
    "Which function are we in"
    (propertize
     (or (which-function) "" )
     'face (nano-modeline-face 'name)))
  (defun my-nano-modeline-prog-mode (&optional default)
    "Nano line for prog mode. Can be made DEFAULT mode."
    (funcall nano-modeline-position
             '((nano-modeline-buffer-status) " "
               (nano-modeline-buffer-name) " "
               (my-nano-modeline-which-function) " "
               (nano-modeline-git-info "⎇ "))
             '((nano-modeline-cursor-position)
               (nano-modeline-window-dedicated))
             default))
  (add-hook 'prog-mode-hook #'my-nano-modeline-prog-mode)
  (nano-modeline-text-mode t)
  (setq nano-modeline-position 'nano-modeline-footer)
  
  ;;
  ;; Projectile
  ;;
  (use-package projectile
    :ensure t)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (use-package helm-projectile
    :ensure t)
  
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
  ;; Modus Themes
  ;;
  (use-package modus-themes
    :ensure t)
  
  ;;
  ;; Lisp & Clojure stuff
  ;;
  (use-package cider
    :ensure t)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)

  ;;
  ;; CMake
  ;;
  (use-package cmake-mode
    :ensure t)

  ;;
  ;; yaml
  ;;
  (use-package yaml-mode
    :ensure t)
  
  ;;
  ;; C++/C stuff
  ;;
  (defun set-tabs-to-four-spaces ()
      (setq tab-width 4))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
  (add-hook 'c++-mode-hook 'set-tabs-to-four-spaces)

  ;;
  ;; asm-mode
  ;;
  (use-package nasm-mode
    :ensure t)
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
  
  ;;
  ;; ReStructured Text
  ;;
  (add-to-list 'auto-mode-alist '("\\.hts\\'" . rst-mode))

  ;;
  ;; eglot
  ;;
  (use-package eglot
    :ensure t)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "/home/linuxbrew/.linuxbrew/opt/llvm@13/bin/clangd"))
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (eglot-ensure)
     (local-set-key (kbd "M-n") #'flymake-goto-next-error)
     (local-set-key (kbd "M-p") #'flymake-goto-prev-error)))
 

  ;; Syntax checking.
  ;; Should be automatic.
  ;; http://www.flycheck.org/en/latest/
  ;; (use-package flycheck
  ;;   :ensure t
  ;;   :config
  ;;   (global-flycheck-mode))
    
  ;; (use-package flycheck-rtags
  ;;   :ensure t)
  ;; (setq rtags-autostart-diagnostics t)
  ;; (defun my-flycheck-rtags-setup ()
  ;;   (flycheck-select-checker 'rtags)
  ;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  ;;   (setq-local flycheck-check-syntax-automatically nil)
  ;;   )
  ;; (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
  ;; (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
  ;; (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)

  ;;
  ;; Flymake
  ;;
  (use-package consult
    :ensure t
    :config
    (global-set-key (kbd "M-g f") 'consult-flymake)
    ) 
  
  ;; Auto-completions.
  ;; There's also `C-M-i`, but this is async.
  ;; Also look at `company-flx` for better sorting.
  ;; https://company-mode.github.io/
  (use-package company
    :ensure t
    :config
    (global-company-mode))
  (use-package company-rtags
    :ensure t
    :config
    (push 'company-rtags company-backends)
    )
  
  ;;
  ;; Helm mode
  ;;
  (use-package helm
    :ensure t
    :config
    (global-set-key (kbd "C-c h") 'helm-mini)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (helm-mode 1)
    )
  (define-key helm-map (kbd "<left>") 'helm-previous-source)
  (define-key helm-map (kbd "<right>") 'helm-next-source)
  (customize-set-variable 'helm-imenu-lynx-style-map t)
  (customize-set-variable 'helm-semantic-lynx-style-map t)
  (customize-set-variable 'helm-occur-use-ioccur-style-keys t)
  (customize-set-variable 'helm-grep-use-ioccur-style-keys t)

  (use-package highlight-symbol
    :ensure t)

  (use-package paredit
    :ensure t)

  (use-package magit
    :ensure t)
  (setq magit-log-margin '(t "%F %R" magit-log-margin-width t 18))

  (use-package cff
    :ensure t)
  
  ;;
  ;; Global key shortcuts:
  ;;
  (global-set-key (kbd "<f2>") 'dabbrev-completion)
  (add-hook 'c-mode-common-hook
	    #'(lambda ()
	       (define-key c-mode-base-map (kbd "<f3>") 'cff-find-other-file)))
  (global-set-key (kbd "<f4>") (lambda() (interactive) (switch-to-buffer (other-buffer (current-buffer) nil))))
  (global-set-key (kbd "<f7>")   'fd-switch-dictionary)
  (global-set-key (kbd "<f8>") (lambda() (interactive) (kill-buffer (current-buffer))))
  (global-set-key (kbd "<f9>") 'compile)
  (global-set-key (kbd "M-?") 'grep)
  (global-set-key (kbd "M-n") 'next-error)
  (global-set-key (kbd "C-c r n") 'rtags-next-match)
  (global-set-key (kbd "C-c r N") 'rtags-previous-match)
  (global-set-key (kbd "M->") (lambda() (interactive) (other-frame 1)))
  (global-set-key (kbd "M->") (lambda() (interactive) (other-frame -1)))
  (global-set-key (kbd "M-/") 'xref-find-references)
  (global-set-key (kbd "C-<return>") 'company-complete)
  (global-set-key (kbd "C-x C-o") #'(lambda() (interactive (other-window -1))))
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "s-<left>") 'previous-buffer)
  (global-set-key (kbd "s-<right>") 'next-buffer)
  (global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region)
  
  ;;
  ;; Emacs server
  ;;
  (server-start)
  (add-hook 'server-switch-hook #'raise-frame)

  ;;
  ;; Calendar
  ;;
  (require 'calendar)
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
		      :height 1.0)
  (defvar calendar-week-start-day 1)
  (defvar calendar-intermonth-text
    '(propertize
      (format "%2d"
              (car
               (calendar-iso-from-absolute
                (calendar-absolute-from-gregorian (list month day year)))))
      'font-lock-face 'font-lock-function-name-face))

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
  ;; Dockerfile-mode
  ;;
  ;; (add-to-list 'load-path "~/Programs/dockerfile-mode/")
  ;; (require 'dockerfile-mode)
  ;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  
  ;;
  ;; xml mode
  ;;
  (add-to-list 'auto-mode-alist
              (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "gdml") t) "\\'")
                    'nxml-mode))

  ;;
  ;; Buffer menu
  ;;
  (setq-default mouse-buffer-menu-mode-mult 0)
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
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
