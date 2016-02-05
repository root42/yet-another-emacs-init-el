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

(defvar aixigo-clang-includes)
(defvar aixigo-project-name)
(defvar aixigo-project-branch)

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
  (let (
        (fname (buffer-file-name))
        )
    (append (format "%s/build/include/external_modules" (aixigo-get-local-file-part (aixigo-get-current-project-path-base fname)))
            (with-temp-buffer
              (setq default-directory (aixigo-get-current-project-path-base fname))
              (shell-command
               (format "%s %s/build/include/external_modules -maxdepth 1 -mindepth 1 -type d | sort"
                       aixigo-find-command
                       (aixigo-get-local-file-part (aixigo-get-current-project-path-base fname))
                       )
               (current-buffer)
               )
              (message (buffer-string))
              (split-string (buffer-string) "\n" t)
              )
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
      (progn
        (message "Could not determine project.")
        nil)
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
  (interactive)
  (let (
        (fname (if optionalfname (file-truename optionalfname) (file-truename buffer-file-name)))
        )
    (string-match (format "\\(.*/%s\\)/.*" (aixigo-get-current-branch fname)) fname)
    (match-string 1 fname)
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

    (setq company-clang-arguments
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
  "Returns t if file-name is a local file name, or nil if it is handled by tramp."
  (if (string-match-p "/[[:alnum:]]*:[[:alnum:]]*:/.*" file-name)
      nil
    t
    )
  )

(defun aixigo-get-local-file-part (file-name)
  "Returns the local part of a path."
  (if (aixigo-is-local-file-p file-name)
      file-name
    (tramp-file-name-localname (tramp-dissect-file-name file-name))
    )
  )

(defun aixigo-header-p (file-name)
  "Returns true if buffer seems to be a header file, nil otherwise."
  (if (string-match "\\.\\(h\\|hdf\\)$" file-name)
      t
    nil
    )
  )

(defun aixigo-find-other-file ()
  "Tries to find C++ header or implementation of current buffer using GNU global."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (find-file
     (with-temp-buffer
       (shell-command
        (format "global -P '%s/%s.%s'"
                (file-name-nondirectory (directory-file-name (file-name-directory file-name)))
                (file-name-sans-extension (file-name-nondirectory file-name))
                (if (aixigo-header-p file-name)
                    "c"
                  "h"
                  )
                )
        (current-buffer)
        )
       (car (last (split-string (buffer-string) "\n" t)))
       )
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

;;; aixigo.el ends here
