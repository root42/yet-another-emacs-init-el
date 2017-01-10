;;; package --- Aixigo specific code for code completion and navigation

;;; Commentary:

;;; Code:

;;
;; Aixigo settings
;;

(require 'cc-mode)
(provide 'aixigo)

;;
;; Customizations
;;
(defgroup aixigo nil
  "This customization group contains all the aixigo specific settings for Emacs.")

(defcustom aixigo-find-command "find"
  "The command used to find include directories.  On OS X, gfind is needed, on Linux find."
  :group 'aixigo
  :type 'directory
  )

(defcustom aixigo-enable-ecb t
  "Enable ECB if non nil."
  :group 'aixigo
  :type 'boolean
  )

(defvar aixigo-clang-includes)
(defvar aixigo-project-name)
(defvar aixigo-project-branch)

;;
;; Default Font
;;
(add-to-list 'default-frame-alist '(font . "Source Code Pro 15" ))

;;
;; Toggle between light and dark themes
;;

(setq custom-theme-directory user-true-init-file-directory)
(setq aixigo-themes '(aixigo-misterioso eclipse))
(setq aixigo-themes-index 0)

(defun aixigo-cycle-theme ()
  (interactive)
  (setq aixigo-themes-index (% (1+ aixigo-themes-index) (length aixigo-themes)))
  (aixigo-load-indexed-theme))

(defun aixigo-load-indexed-theme ()
  (aixigo-try-load-theme (nth aixigo-themes-index aixigo-themes)))

(defun aixigo-try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

(global-set-key (kbd "<f12>") 'aixigo-cycle-theme)

;;
;; Utility functions for setting up the project paths
;;
(defun aixigo-get-system-includes ()
  "Return the default C++ system search paths, as returned by the system C pre-processor."
  (let (
        (fname (buffer-file-name))
        )
    (with-temp-buffer
      (setq default-directory (aixigo-get-current-project-path-base fname))
      (shell-command
       "echo | gcc -x c++ -v -E /dev/null 2>&1 | egrep '^ .*include' | sed 's/^ //g'"
       (current-buffer)
       )
      (split-string (buffer-string) "\n" t)
      )
    )
  )

(defun aixigo-get-project-includes ()
  "Return a list of all C++ include directories for the given project, which can be passed to the compiler."
  (interactive)
  (let (
        (fname (buffer-file-name))
        )
    (with-temp-buffer
      (setq default-directory (aixigo-get-current-project-path-base fname))
      (shell-command
       (format "%s %s/modules/ %s/models/ -name include -type d | sort"
               aixigo-find-command
               (aixigo-get-local-file-part (aixigo-get-current-project-path-base fname))
               (aixigo-get-local-file-part (aixigo-get-current-project-path-base fname))
               )
       (current-buffer)
       )
      (split-string (buffer-string) "\n" t)
      )
    )
  )

(defun aixigo-get-external-modules-includes ()
  "Return the path to the external modules headers for a given project."
  (interactive)
  (let (
        (fname (buffer-file-name))
        )
    (with-temp-buffer
      (setq default-directory (aixigo-get-current-project-path-base fname))
      (shell-command
       (format "echo %s/build/include/external_modules ; %s %s/build/include/external_modules -maxdepth 1 -mindepth 1 -type d | sort"
               (aixigo-get-local-file-part (aixigo-get-current-project-path-base fname))
               aixigo-find-command
               (aixigo-get-local-file-part (aixigo-get-current-project-path-base fname))
               )
       (current-buffer)
       )
      (split-string (buffer-string) "\n" t)
      )
    )
  )

(defun aixigo-get-current-project-module-and-branch ( &optional optionalfname )
  "Return a tuple with the project name, module name and branch of the current buffer or given OPTIONALFNAME."
  (interactive)
  (let (
        (fname (if optionalfname (file-truename optionalfname) (file-truename buffer-file-name)))
        )
    (if (string-match ".*/\\(.*\\)/\\(.*\\)/\\(platform_modules\\|modules\\|models\\)/\\([^/]*\\)/" fname)
        (list
         (match-string 1 fname)
         (match-string 2 fname)
         (match-string 3 fname)
         )
      (error "Could not determine project")
      )
    )
  )

(defun aixigo-get-current-project ( &optional optionalfname )
  "Return the project name of current buffer or OPTIONALFNAME."
  (nth 0 (aixigo-get-current-project-module-and-branch optionalfname))
  )

(defun aixigo-get-current-branch ( &optional optionalfname )
  "Return the branch name of the current buffer OPTIONALFNAME."
  (nth 1 (aixigo-get-current-project-module-and-branch optionalfname))
  )

(defun aixigo-get-current-module ( &optional optionalfname )
  "Return the module name of current buffer or OPTIONALFNAME."
  (nth 2 (aixigo-get-current-project-module-and-branch optionalfname))
  )

(defun aixigo-get-current-project-path-base ( &optional optionalfname )
  "Return the project path base string for current buffer or OPTIONALFNAME."
  (let (
        (fname (if optionalfname (file-truename optionalfname) (file-truename buffer-file-name)))
        )
    (string-match (format "\\(.*/%s\\)/.*" (aixigo-get-current-branch fname)) fname)
    (match-string 1 fname)
    )
  )

(defun aixigo-compile-current-module ( &optional optionalfname)
  "Return the path base string for the current buffer or OPTIONALFNAME."
  (interactive)
  (let (
        (fname (if optionalfname (file-truename optionalfname) (file-truename buffer-file-name)))
        )
    (if (string-match ".*/\\(platform_modules\\|modules\\|models\\)/[^/]*/" fname)
        (if (aixigo-is-local-file-p fname)
            (compile (format "cd %s && ./install" (aixigo-get-local-file-part (match-string 0 fname))))
          (compile (format "ssh %s \"cd %s && ./install\"" (aixigo-get-remote-host-part fname) (aixigo-get-local-file-part (match-string 0 fname))))
          )
      (error "Could not determine project module to compile")
      )
    )
  )

(defun aixigo-setup-current-project ()
  "Set up the project specific variables according to the current buffer's project."
  (interactive)
  (let (
        (project-settings (aixigo-get-current-project-module-and-branch))
        )

    (setq aixigo-project-name (nth 0 project-settings))
    (setq aixigo-project-branch (nth 1 project-settings))

    (setq aixigo-clang-includes
          (append (aixigo-get-project-includes)
                  (aixigo-get-external-modules-includes)
                  )
          )

    ;;(setq company-clang--prefix "-W")
    ;;(company-clang-set-prefix "-W")
    (setq company-clang-arguments
          (mapcar (lambda (item) (concat "-I" item))
                  aixigo-clang-includes
                  )
          )
    (setq ac-clang-flags
          (mapcar (lambda (item) (concat "-I" item))
                  aixigo-clang-includes
                  )
          )
    (setq flycheck-clang-args
          (mapcar (lambda (item) (concat "-I" item))
               aixigo-clang-includes
           )
          )
    )

  (setq frame-title-format '( "" "%b @ Aixmacs24 " aixigo-project-name "/" aixigo-project-branch))
  
  )

(defun aixigo-is-local-file-p (file-name)
  "Return t if FILE-NAME is a local file name, or nil if it is handled by tramp."
  (if (string-match-p "/[[:graph:]^/]*:[[:graph:]^/]*:/.*" file-name )
      nil
    t
    )
  )

(defun aixigo-get-local-file-part (file-name)
  "Return the local part of FILE-NAME."
  (if (aixigo-is-local-file-p file-name)
      file-name
    (tramp-file-name-localname (tramp-dissect-file-name file-name))
    )
  )

(defun aixigo-get-remote-host-part (file-name)
  "Return the host portion of FILE-NAME or nil if it is a local file."
  (if (aixigo-is-local-file-p file-name)
      nil
    (tramp-file-name-host (tramp-dissect-file-name file-name))
    )
  )

(defun aixigo-header-p (file-name)
  "Return true if FILE-NAME seems to be a header file, nil otherwise."
  (if (string-match "\\.\\(h\\|hdf\\)$" file-name)
      t
    nil
    )
  )

(defun aixigo-find-other-file ()
  "Try to find C++ header or implementation of current buffer using GNU global."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (let ((other-file-name
           (with-temp-buffer
             (shell-command
              (if (aixigo-header-p file-name)
                  (format "global -P '%s/(src/|)%s.%s'"
                      (file-name-nondirectory (directory-file-name (file-name-directory file-name)))
                      (file-name-sans-extension (file-name-nondirectory file-name))
                      "c"
                      )
                (format "global -P '%s/%s.%s'"
                        (if (string= (file-name-nondirectory (directory-file-name (file-name-directory file-name))) "src")
                            (file-name-nondirectory (directory-file-name (file-name-directory (directory-file-name (file-name-directory (directory-file-name file-name))))))
                          (file-name-nondirectory (directory-file-name (file-name-directory file-name)))
                          )
                        (file-name-sans-extension (file-name-nondirectory file-name))
                        "h"
                        )
                )
              (current-buffer)
              )
             (car (last (split-string (buffer-string) "\n" t)))
             )))
      (if other-file-name
          (find-file other-file-name)
        (error "Could not find other file"))
      )
    )
  )

(defun aixigo-comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )

(defun aixigo-should-buffer-be-killed ( buffer )
  "Test if the given BUFFER should be killed.
ECB buffers, messages and scratch will never be killed."
  (not
   ( some
    ( lambda (x)
      ( string-prefix-p x ( buffer-name buffer ) )
      )
    '( "*scratch*" "*Messages*" " *ECB" )
    )
   )
  )

(defun aixigo-filter (condp lst)
  "Use condition CONDP to filter list LST.  Return the filtered list."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)
        )
  )

(defun kill-all-buffers ()
  "Kill all buffers, except those given in aixigo-should-buffer-be-killed."
  (interactive)
  ( mapc 'kill-buffer
        ( aixigo-filter
         'aixigo-should-buffer-be-killed
         ( buffer-list )
         )
        )
  )

(define-key c-mode-base-map (kbd "C-/") 'aixigo-comment-or-uncomment-line-or-region)

(defun display-buffer-at-bottom--display-buffer-at-bottom-around (orig-fun &rest args)
  "Bugfix for ECB: cannot use `display-buffer-at-bottom', call
`display-buffer-use-some-window' instead in ECB frame."
  (if (and ecb-minor-mode (equal (selected-frame)
                                 ecb-frame))
      (apply 'display-buffer-use-some-window args)
    (apply orig-fun args)))
(advice-add 'display-buffer-at-bottom :around #'display-buffer-at-bottom--display-buffer-at-bottom-around)

;;; aixigo.el ends here
