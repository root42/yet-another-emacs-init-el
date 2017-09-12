;;; package --- Nuance specific code for code completion and navigation

;;; Commentary:

;;; Code:

;;
;; Nuance settings
;;

(require 'cc-mode)
(require 'p4)
(provide 'nuance)

;;
;; Customizations
;;
(defgroup nuance nil
  "This customization group contains all the nuance specific settings for Emacs.")

(defcustom nuance-find-command "find"
  "The command used to find include directories.  On OS X, gfind is needed, on Linux find."
  :group 'nuance
  :type 'directory
  )

(defcustom nuance-enable-ecb t
  "Enable ECB if non nil."
  :group 'nuance
  :type 'boolean
  )

(defvar nuance-clang-includes)
(defvar nuance-project-name)
(defvar nuance-project-branch)

;;
;; Toggle between light and dark themes
;;

(setq custom-theme-directory user-true-init-file-directory)
(setq nuance-themes '(nuance-misterioso eclipse))
(setq nuance-themes-index 0)

(defun nuance-cycle-theme ()
  (interactive)
  (setq nuance-themes-index (% (1+ nuance-themes-index) (length nuance-themes)))
  (nuance-load-indexed-theme))

(defun nuance-load-indexed-theme ()
  (nuance-try-load-theme (nth nuance-themes-index nuance-themes)))

(defun nuance-try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

(global-set-key (kbd "<f12>") 'nuance-cycle-theme)

;;
;; Utility functions for setting up the project paths
;;
(defun nuance-get-system-includes ()
  "Return the default C++ system search paths, as returned by the system C pre-processor."
  (let (
        (fname (buffer-file-name))
        )
    (with-temp-buffer
      (setq default-directory (ggtags-current-project-root))
      (shell-command
       "echo | gcc -x c++ -v -E /dev/null 2>&1 | egrep '^ .*include' | sed 's/^ //g'"
       (current-buffer)
       )
      (split-string (buffer-string) "\n" t)
      )
    )
  )

(defun nuance-get-project-includes ()
  "Return a list of all C++ include directories for the given project, which can be passed to the compiler."
  (interactive)
  (let (
        (fname (buffer-file-name))
        )
    (with-temp-buffer
      (setq default-directory (ggtags-current-project-root))
      (shell-command
       (format "if [ -e includes.txt ]; then cat includes.txt; else %s/includes.sh > includes.txt ; cat includes.txt ; fi" user-true-init-file-directory)
       ;; "find . -name '*.vcxproj' | xargs egrep -h '<AdditionalIncludeDirectories>' | sed -E 's/ *<\\/?AdditionalIncludeDirectories>//g' | sed 's/\\\\/\\//g' | sed 's/;/\\n/g' | sort | uniq | egrep -v '^[%$]'"
       ;; "global -Pa '.*\.hp?p?' | awk -F'/' '{ for (i=1; i<NF-1; i++) printf(\"%s/\", $i); printf(\"\\n\"); for (i=1; i<NF; i++) printf(\"%s/\", $i); printf(\"\\n\"); }' | sort | uniq"
       (current-buffer)
       )
      (split-string (buffer-string) "\n" t)
      )
    )
  )

(defun nuance-get-current-project-module-and-branch ( &optional optionalfname )
  "Return a tuple with the project name, module name and branch of the current buffer or given OPTIONALFNAME."
  (interactive)
  '("speechIF1" "master")
  )

(defun nuance-get-current-project ( &optional optionalfname )
  "Return the project name of current buffer or OPTIONALFNAME."
  (nth 0 (nuance-get-current-project-module-and-branch optionalfname))
  )

(defun nuance-get-current-branch ( &optional optionalfname )
  "Return the branch name of the current buffer OPTIONALFNAME."
  (nth 1 (nuance-get-current-project-module-and-branch optionalfname))
  )

(defun nuance-get-current-module ( &optional optionalfname )
  "Return the module name of current buffer or OPTIONALFNAME."
  (nth 2 (nuance-get-current-project-module-and-branch optionalfname))
  )

(defun nuance-get-current-project-path-base ( &optional optionalfname )
  "Return the project path base string for current buffer or OPTIONALFNAME."
  (let (
        (fname (if optionalfname (file-truename optionalfname) (file-truename buffer-file-name)))
        )
    (string-match (format "\\(.*/%s\\)/.*" (nuance-get-current-branch fname)) fname)
    (match-string 1 fname)
    )
  )

(defun nuance-compile-current-module ( &optional optionalfname)
  "Return the path base string for the current buffer or OPTIONALFNAME."
  (interactive)
  (let (
        (fname (if optionalfname (file-truename optionalfname) (file-truename buffer-file-name)))
        )
    (if (string-match ".*/\\(platform_modules\\|modules\\|models\\)/[^/]*/" fname)
        (if (nuance-is-local-file-p fname)
            (compile (format "cd %s && ./install" (nuance-get-local-file-part (match-string 0 fname))))
          (compile (format "ssh %s \"cd %s && ./install\"" (nuance-get-remote-host-part fname) (nuance-get-local-file-part (match-string 0 fname))))
          )
      (error "Could not determine project module to compile")
      )
    )
  )

(setq debug-on-error t)
(defun nuance-setup-current-project ()
  "Set up the project specific variables according to the current buffer's project."
  (interactive)
  (let (
        (project-settings (nuance-get-current-project-module-and-branch))
        )
    (setq nuance-project-name (nth 0 project-settings))
    (setq nuance-project-branch (nth 1 project-settings))
    (setq nuance-clang-includes
          (append (nuance-get-project-includes)
                  )
          )

    (setq nuance-general-clang-arguments '("-std=c++11" "-DSAC_SAS" "-Wno-unused-parameter" "-Wno-unused-variable"))
    (setq company-clang-arguments
          (append
           nuance-general-clang-arguments
           (mapcar (lambda (item) (concat "-I" item))
                   (append (nuance-get-system-includes) nuance-clang-includes)
                   )
           )
          )
    (setq ac-clang-flags
          (append
           nuance-general-clang-arguments
           (mapcar (lambda (item) (concat "-I" item))
                   (append (nuance-get-system-includes) nuance-clang-includes)
                   )
           )
          )
    (setq flycheck-clang-args
           nuance-general-clang-arguments
          )
    (setq flycheck-clang-include-path
          nuance-clang-includes
          )
    )

  (setq frame-title-format '( "" "%b @ Nuancemacs25 " nuance-project-name "/" nuance-project-branch))
  
  )

(defun nuance-is-local-file-p (file-name)
  "Return t if FILE-NAME is a local file name, or nil if it is handled by tramp."
  (if (string-match-p "/[[:graph:]^/]*:[[:graph:]^/]*:/.*" file-name )
      nil
    t
    )
  )

(defun nuance-get-local-file-part (file-name)
  "Return the local part of FILE-NAME."
  (if (nuance-is-local-file-p file-name)
      file-name
    (tramp-file-name-localname (tramp-dissect-file-name file-name))
    )
  )

(defun nuance-get-remote-host-part (file-name)
  "Return the host portion of FILE-NAME or nil if it is a local file."
  (if (nuance-is-local-file-p file-name)
      nil
    (tramp-file-name-host (tramp-dissect-file-name file-name))
    )
  )

(defun nuance-header-p (file-name)
  "Return true if FILE-NAME seems to be a header file, nil otherwise."
  (if (string-match "\\.\\(h\\|hdf\\)$" file-name)
      t
    nil
    )
  )

(defun nuance-find-other-file ()
  "Try to find C++ header or implementation of current buffer using GNU global."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (let ((other-file-name
           (with-temp-buffer
             (shell-command
              (if (nuance-header-p file-name)
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

(defun nuance-comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )

(defun nuance-should-buffer-be-killed ( buffer )
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

(defun nuance-filter (condp lst)
  "Use condition CONDP to filter list LST.  Return the filtered list."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)
        )
  )

(defun kill-all-buffers ()
  "Kill all buffers, except those given in nuance-should-buffer-be-killed."
  (interactive)
  ( mapc 'kill-buffer
        ( nuance-filter
         'nuance-should-buffer-be-killed
         ( buffer-list )
         )
        )
  )

(define-key c-mode-base-map (kbd "C-/") 'nuance-comment-or-uncomment-line-or-region)

(defun display-buffer-at-bottom--display-buffer-at-bottom-around (orig-fun &rest args)
  "Bugfix for ECB: cannot use `display-buffer-at-bottom', call
`display-buffer-use-some-window' instead in ECB frame."
  (if (and ecb-minor-mode (equal (selected-frame)
                                 ecb-frame))
      (apply 'display-buffer-use-some-window args)
    (apply orig-fun args)))
(advice-add 'display-buffer-at-bottom :around #'display-buffer-at-bottom--display-buffer-at-bottom-around)

;;; nuance.el ends here
