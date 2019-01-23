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
  ;; (defun ensure-package-installed (&rest packages)
  ;;   "Assure every package is installed, ask for installation if it’s not.
  ;;    Return a list of installed packages or nil for every skipped package."
  ;;   (mapcar
  ;;    (lambda (package)
  ;;      ;; (package-installed-p 'evil)
  ;;      (if (package-installed-p package)
  ;;          nil
  ;;        (package-install package nil)
  ;;        package
  ;;        ))
  ;;    packages))

  ;; (ensure-package-installed
   ;;  'auctex
   ;;  'cider
   ;;  'clojure-mode
   ;;  'company
   ;;  'csv-mode
   ;;  'eclipse-theme
   ;;  'exec-path-from-shell
   ;;  'flycheck
   ;;  'ggtags
   ;;  'helm
   ;;  'highlight-symbol
   ;;  ;; 'jedi ;; Needs working pip and jediserver
   ;;  'js2-mode
   ;;  'magit
   ;;  'markdown-mode
   ;;  'org-bullets
   ;;  'paredit
   ;;  'p4
   ;;  'reftex
   ;;  'skewer-mode
   ;;  'tabbar
   ;; 'use-package
   ;; )

  ;;
  ;; Nuance setup
  ;;
  ;;(load-file (format "%snuance.el" user-true-init-file-directory))
  
  ;;
  ;; Set up the coding system
  ;;
  (prefer-coding-system 'utf-8)
  
  ;;
  ;; Customize stuff
  ;;
  (setq custom-file (expand-file-name "custom.el" (or (file-name-directory user-init-file) default-directory)))
  (load custom-file)
  (global-set-key (kbd "<f1>") '(lambda () (interactive) (load custom-file)))

  ;;
  ;; Python
  ;;
  ;; (add-hook 'python-mode-hook 'jedi:setup)
  ;; (defvar jedi:complete-on-dot t)

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
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'nxml-mode-hook 'linum-mode)
  (add-hook 'nxml-mode-hook 'hl-line-mode)
  ;; Uncomment when this bug is fixed: https://github.com/alpaker/Fill-Column-Indicator/issues/54
  (add-hook 'prog-mode-hook 'fci-mode)
  
  (defun comment-or-uncomment-line-or-region ()
    "Comments or uncomments the current line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      )
    )

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
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)

  ;;
  ;; C++/C stuff
  ;;
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda ()
  ;;             (when (and (derived-mode-p 'c-mode 'c++-mode 'java-mode) (require 'ggtags nil 'noerror))
  ;;       	(ggtags-mode 1))))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  (rtags-enable-standard-keybindings)
  (defun use-rtags (&optional useFileManager)
    (and (rtags-executable-find "rc")
         (cond ((not (gtags-get-rootpath)) t)
               ((and (not (eq major-mode 'c++-mode))
                     (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
               (useFileManager (rtags-has-filemanager))
               (t (rtags-is-indexed)))))

  (defun tags-find-symbol-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
        (gtags-find-tag)))
  (defun tags-find-references-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
        (gtags-find-rtag)))
  (defun tags-find-symbol ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-symbol)))
  (defun tags-find-references ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
  (defun tags-find-file ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))
  (defun tags-imenu ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

  (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
  (define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
  (define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
  (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

  (define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key global-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key global-map (kbd "M-;") (function tags-find-file))
  (define-key global-map (kbd "C-.") (function tags-find-symbol))
  (define-key global-map (kbd "C-,") (function tags-find-references))
  (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key global-map (kbd "M-i") (function tags-imenu))

  (setq company-clang-arguments
        '(
          "-std=c++0x"
          "-pthread"
          "-Wno-everything"
          "-DBUILD_COMPILER_DIR\=\"ilglcg\""
          "-DBUILD_DIR\=\"neuraln\""
          "-DBUILD_HAS_PAPI\=1"
          "-DDEBUG\=1"
          "-DDEBUGO\=1"
          "-DINHOUSE\=1"
          "-include" "/home/aschmitz/Programs/mrec/mrecutil/mrec.h"
          "-I/home/aschmitz/Programs/mrec/alien/lint"
          "-I/home/aschmitz/Programs/mrec/alien/pal"
          "-I/home/aschmitz/Programs/mrec/alien/pal/common"
          "-I/home/aschmitz/Programs/mrec/alien/pal/crc32"
          "-I/home/aschmitz/Programs/mrec/alien/pal/linkedlist"
          "-I/home/aschmitz/Programs/mrec/alien/pal/win32"
          "-I/home/aschmitz/Programs/mrec/alien/papi"
          "-I/home/aschmitz/Programs/mrec/alien/sort"
          "-I/home/aschmitz/Programs/mrec/alien/xercesc/include/xercesc/util/MsgLoaders/Win32"
          "-I/home/aschmitz/Programs/mrec/alien/zlib"
          "-I/home/aschmitz/Programs/mrec/alien/zstd/common"
          "-I/home/aschmitz/Programs/mrec/alien/zstd/compress"
          "-I/home/aschmitz/Programs/mrec/alien/zstd/decompress"
          "-I/home/aschmitz/Programs/mrec/alien/zstd/inc"
          "-I/home/aschmitz/Programs/mrec/audiosrc"
          "-I/home/aschmitz/Programs/mrec/audiosrc.inc"
          "-I/home/aschmitz/Programs/mrec/bin/ilglcx/mrec.env/lib/python2.7/site-packages/wheel/test/headers.dist"
          "-I/home/aschmitz/Programs/mrec/builtinc/ilglcx"
          "-iquote/home/aschmitz/Programs/mrec/channel"
          "-iquote/home/aschmitz/Programs/mrec/channel.inc"
          "-iquote/home/aschmitz/Programs/mrec/credits"
          "-iquote/home/aschmitz/Programs/mrec/dfutil"
          "-iquote/home/aschmitz/Programs/mrec/fileutil"
          "-iquote/home/aschmitz/Programs/mrec/frame.inc"
          "-iquote/home/aschmitz/Programs/mrec/fst"
          "-iquote/home/aschmitz/Programs/mrec/fstgraph"
          "-iquote/home/aschmitz/Programs/mrec/fstgraph.inc"
          "-iquote/home/aschmitz/Programs/mrec/fst.inc"
          "-iquote/home/aschmitz/Programs/mrec/germ.inc"
          "-iquote/home/aschmitz/Programs/mrec/hello1"
          "-iquote/home/aschmitz/Programs/mrec/kernel"
          "-iquote/home/aschmitz/Programs/mrec/lm"
          "-iquote/home/aschmitz/Programs/mrec/lm.inc"
          "-iquote/home/aschmitz/Programs/mrec/make.inc/ilglcx/MREC_ilglcx.env/lib/python2.7/site-packages/wheel/test/headers.dist"
          "-iquote/home/aschmitz/Programs/mrec/mrecutil"
          "-iquote/home/aschmitz/Programs/mrec/net"
          "-iquote/home/aschmitz/Programs/mrec/net.inc"
          "-iquote/home/aschmitz/Programs/mrec/neuraln"
          "-iquote/home/aschmitz/Programs/mrec/neuraln.inc"
          "-iquote/home/aschmitz/Programs/mrec/nistplay"
          "-iquote/home/aschmitz/Programs/mrec/pel.inc"
          "-iquote/home/aschmitz/Programs/mrec/phoneme.inc"
          "-iquote/home/aschmitz/Programs/mrec/prefilt"
          "-iquote/home/aschmitz/Programs/mrec/prefilt.inc"
          "-iquote/home/aschmitz/Programs/mrec/pypkg/ilglcx/sphinx.env/lib/python2.7/site-packages/wheel/test/headers.dist"
          "-iquote/home/aschmitz/Programs/mrec/rdspec"
          "-iquote/home/aschmitz/Programs/mrec/recogctl"
          "-iquote/home/aschmitz/Programs/mrec/sdapi"
          "-iquote/home/aschmitz/Programs/mrec/sdclidll"
          "-iquote/home/aschmitz/Programs/mrec/sdspec"
          "-iquote/home/aschmitz/Programs/mrec/sdspec/builtsrc/ilglcx"
          "-iquote/home/aschmitz/Programs/mrec/sdxg1"
          "-iquote/home/aschmitz/Programs/mrec/sdxg2"
          "-iquote/home/aschmitz/Programs/mrec/sdxgspec"
          "-iquote/home/aschmitz/Programs/mrec/sigproc"
          "-iquote/home/aschmitz/Programs/mrec/sigproc.inc"
          "-iquote/home/aschmitz/Programs/mrec/svsd"
          "-iquote/home/aschmitz/Programs/mrec/svsdclidll"
          "-iquote/home/aschmitz/Programs/mrec/svsdspec"
          "-iquote/home/aschmitz/Programs/mrec/user.inc"
          "-iquote/home/aschmitz/Programs/mrec/voc.inc"
          "-iquote/home/aschmitz/Programs/mrec/wavelib"
          "-iquote/home/aschmitz/Programs/mrec/wavelib.inc"
          "-iquote/home/aschmitz/Programs/mrec/word.inc"
          "-iquote/home/aschmitz/Programs/mrec/ilglcx"
          "-iquote/home/aschmitz/Programs/mrec/sdspec/builtsrc/ilglcg"
          )
        )

  ;; Syntax checking.
  ;; Should be automatic.
  ;; http://www.flycheck.org/en/latest/
  (use-package flycheck
    :ensure t
    :config
    (global-flycheck-mode))

  (setq flycheck-clang-args
        '(
          "-W"
          "-Wall"
          "-Wshadow"
          "-Werror"
          )
        )
  
  (setq flycheck-clang-definitions
        '(
          "BUILD_COMPILER_DIR=\"ilglcg\""
          "BUILD_DIR=\"neuraln\""
          "BUILD_HAS_PAPI=1"
          "DEBUG=1"
          "DEBUGO=1"
          "INHOUSE=1"
          )
        )

  (setq flycheck-clang-includes
        '(
          "/home/aschmitz/Programs/mrec/mrecutil/mrec.h"
          )
        )
  
  (setq flycheck-clang-include-path
        '(
          "/home/aschmitz/Programs/mrec/alien/lint"
          "/home/aschmitz/Programs/mrec/alien/pal"
          "/home/aschmitz/Programs/mrec/alien/pal/common"
          "/home/aschmitz/Programs/mrec/alien/pal/crc32"
          "/home/aschmitz/Programs/mrec/alien/pal/linkedlist"
          "/home/aschmitz/Programs/mrec/alien/pal/win32"
          "/home/aschmitz/Programs/mrec/alien/papi"
          "/home/aschmitz/Programs/mrec/alien/sort"
          "/home/aschmitz/Programs/mrec/alien/xercesc/include/xercesc/util/MsgLoaders/Win32"
          "/home/aschmitz/Programs/mrec/alien/zlib"
          "/home/aschmitz/Programs/mrec/alien/zstd/common"
          "/home/aschmitz/Programs/mrec/alien/zstd/compress"
          "/home/aschmitz/Programs/mrec/alien/zstd/decompress"
          "/home/aschmitz/Programs/mrec/alien/zstd/inc"
          "/home/aschmitz/Programs/mrec/audiosrc"
          "/home/aschmitz/Programs/mrec/audiosrc.inc"
          "/home/aschmitz/Programs/mrec/bin/ilglcx/mrec.env/lib/python2.7/site-packages/wheel/test/headers.dist"
          "/home/aschmitz/Programs/mrec/builtinc/ilglcx"
          "/home/aschmitz/Programs/mrec/channel"
          "/home/aschmitz/Programs/mrec/channel.inc"
          "/home/aschmitz/Programs/mrec/credits"
          "/home/aschmitz/Programs/mrec/dfutil"
          "/home/aschmitz/Programs/mrec/fileutil"
          "/home/aschmitz/Programs/mrec/frame.inc"
          "/home/aschmitz/Programs/mrec/fst"
          "/home/aschmitz/Programs/mrec/fstgraph"
          "/home/aschmitz/Programs/mrec/fstgraph.inc"
          "/home/aschmitz/Programs/mrec/fst.inc"
          "/home/aschmitz/Programs/mrec/germ.inc"
          "/home/aschmitz/Programs/mrec/hello1"
          "/home/aschmitz/Programs/mrec/kernel"
          "/home/aschmitz/Programs/mrec/lm"
          "/home/aschmitz/Programs/mrec/lm.inc"
          "/home/aschmitz/Programs/mrec/make.inc/ilglcx/MREC_ilglcx.env/lib/python2.7/site-packages/wheel/test/headers.dist"
          "/home/aschmitz/Programs/mrec/mrecutil"
          "/home/aschmitz/Programs/mrec/net"
          "/home/aschmitz/Programs/mrec/net.inc"
          "/home/aschmitz/Programs/mrec/neuraln"
          "/home/aschmitz/Programs/mrec/neuraln.inc"
          "/home/aschmitz/Programs/mrec/nistplay"
          "/home/aschmitz/Programs/mrec/pel.inc"
          "/home/aschmitz/Programs/mrec/phoneme.inc"
          "/home/aschmitz/Programs/mrec/prefilt"
          "/home/aschmitz/Programs/mrec/prefilt.inc"
          "/home/aschmitz/Programs/mrec/pypkg/ilglcx/sphinx.env/lib/python2.7/site-packages/wheel/test/headers.dist"
          "/home/aschmitz/Programs/mrec/rdspec"
          "/home/aschmitz/Programs/mrec/recogctl"
          "/home/aschmitz/Programs/mrec/sdapi"
          "/home/aschmitz/Programs/mrec/sdclidll"
          "/home/aschmitz/Programs/mrec/sdspec"
          "/home/aschmitz/Programs/mrec/sdspec/builtsrc/ilglcx"
          "/home/aschmitz/Programs/mrec/sdxg1"
          "/home/aschmitz/Programs/mrec/sdxg2"
          "/home/aschmitz/Programs/mrec/sdxgspec"
          "/home/aschmitz/Programs/mrec/sigproc"
          "/home/aschmitz/Programs/mrec/sigproc.inc"
          "/home/aschmitz/Programs/mrec/svsd"
          "/home/aschmitz/Programs/mrec/svsdclidll"
          "/home/aschmitz/Programs/mrec/svsdspec"
          "/home/aschmitz/Programs/mrec/user.inc"
          "/home/aschmitz/Programs/mrec/voc.inc"
          "/home/aschmitz/Programs/mrec/wavelib"
          "/home/aschmitz/Programs/mrec/wavelib.inc"
          "/home/aschmitz/Programs/mrec/word.inc"
          "/home/aschmitz/Programs/mrec/ilglcx"
          "/home/aschmitz/Programs/mrec/sdspec/builtsrc/ilglcg"
          )
        )
  
  ;; Auto-completions.
  ;; There's also `C-M-i`, but this is async.
  ;; Also look at `company-flx` for better sorting.
  ;; https://company-mode.github.io/
  (use-package company
    :ensure t
    :config
    (global-company-mode))

  ;; ;; Language Server Protocol Plugin.
  ;; ;; The actual plugin used to communicate with cquery.
  ;; ;; https://github.com/emacs-lsp/lsp-mode
  ;; (use-package lsp-mode
  ;;   :ensure t
  ;;   :config
  ;;   (setq lsp-prefer-flymake nil)
  ;;   )

  ;; ;; Flycheck and other IDE-feature support for LSP.
  ;; ;; This has the "fancy features" and should be customized.
  ;; ;; Personally, I turned the symbol highlighting off.
  ;; ;; https://github.com/emacs-lsp/lsp-ui
  ;; (use-package lsp-ui
  ;;   :ensure t
  ;;   :config
  ;;   (setq lsp-ui-sideline-ignore-duplicate t)
  ;;   (add-hook 'lsp-mode-hook #'lsp-ui-mode))

  ;; ;; LSP backend for Company.
  ;; ;; https://github.com/tigersoldier/company-lsp
  ;; (use-package company-lsp
  ;;   :ensure t
  ;;   :config
  ;;   (setq company-lsp-enable-recompletion t)
  ;;   (add-to-list 'company-backends 'company-lsp))

  ;; Client to configure and auto-start cquery.
  ;; https://github.com/cquery-project/emacs-cquery
  ;; (use-package cquery
  ;;   :ensure t
  ;;   :config
  ;;   (setq cquery-executable "/home/linuxbrew/.linuxbrew/bin/cquery")
  ;;   (setq cquery-extra-init-params '(:completion (:detailedLabel t))))

  ;; (require 'lsp-clients)
  ;; (require 'cquery)
  ;; (setq cquery-executable "/home/linuxbrew/.linuxbrew/bin/cquery")
  ;; ;; (require 'ccls)
  ;; ;; (setq ccls-executable "/home/aschmitz/.local/bin/ccls")
  ;; (add-hook 'python-mode-hook 'lsp)
  ;; (add-hook 'c++-mode-hook 'lsp)

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "~/bin/clojure-lsp")
  ;;                   :major-modes '(clojure-mode)
  ;;                   :server-id 'clojure-lsp))

  ;;
  ;; Helm mode
  ;;
  (use-package helm
    :ensure t
    :config
    (global-set-key (kbd "C-c h") 'helm-mini)
    (helm-mode 1)
    )

  (use-package highlight-symbol
    :ensure t)

  (use-package paredit
    :ensure t)

  (use-package magit
    :ensure t)
  
  ;;
  ;; Global key shortcuts:  
  ;;
  (global-set-key (kbd "<f2>") 'dabbrev-completion)
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (define-key c-mode-base-map (kbd "<f3>") 'cff-find-other-file)))
  (global-set-key (kbd "<f4>") (lambda() (interactive) (switch-to-buffer (other-buffer (current-buffer) nil))))
  (global-set-key (kbd "<f7>")   'fd-switch-dictionary)
  (global-set-key (kbd "<f8>") (lambda() (interactive) (kill-buffer (current-buffer))))
  (global-set-key (kbd "<f9>") 'compile)
  (global-set-key (kbd "M-?") 'grep)
  (global-set-key (kbd "M-n") 'next-error)
  (global-set-key (kbd "M-S-n") 'first-error)
  (global-set-key (kbd "M->") (lambda() (interactive) (other-frame 1)))
  (global-set-key (kbd "M->") (lambda() (interactive) (other-frame -1)))
  (global-set-key (kbd "M-/") 'xref-find-references)
  (global-set-key (kbd "C-<return>") 'company-complete)
  (global-set-key (kbd "C-x C-o") '(lambda() (interactive (other-window -1))))
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "s-<left>") 'previous-buffer)
  (global-set-key (kbd "s-<right>") 'next-buffer)
  (global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region)

  ;;
  ;; Emacs server
  ;;
  (server-start)

  ;;
  ;; Neotree
  ;;
  ;(neotree)
  
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
              (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "gdml") t) "\\'")
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
