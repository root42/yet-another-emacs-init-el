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

(defcustom aixigo-project-name "commerzbank_zpk_performance_analyzer"
  "This is the current aixigo project name."
  :group 'aixigo
  )

;;
;; Utility functions for setting up the project paths
;;
(defun aixigo-get-system-includes ()
  "Returns the default C++ system search paths, as returned by the system C pre-processor."
  (with-temp-buffer
    (shell-command
     "echo | gcc -x c++ -v -E /dev/null 2>&1 | egrep '^ .*include' | sed 's/^ //g'"
     (current-buffer) 
     )
    (split-string (buffer-string) "\n" t)
    )
  )

(defun aixigo-get-project-path (project-name)
  "Returns the absolute path to the project source, based on the user login name and given project name."
  (format aixigo-project-path-base (user-login-name) project-name) )

(defun aixigo-get-project-includes-full (project-name)
  "Returns a list of all directories containing the C++ header files of the given project."
  (aixigo-get-certain-directories-full project-name "include")
)

(defun aixigo-get-project-sources-full (project-name)
  "Returns a list of all directories containing the C++ header files of the given project."
  (aixigo-get-certain-directories-full project-name "src")
)

(defun aixigo-get-certain-directories-full (project-name directory-name)
  "Returns a list of all directories of a certain name (e.g. src, include) for the given project."
  (with-temp-buffer
    (shell-command 
     ;; find ./HEAD/models/ ./HEAD/modules/ -wholename "*/include/*" -and -not -name "*CVS" -type d -printf \"%p\"\\n | sort
     (format "%s %s/%s/models/ %s/%s/modules/ \\( -wholename \"*/%s/*\" -or -wholename \"*/%s\" \\) -and -not -name \"*CVS\" -type d -printf %%p\\\\n | sort" 
             aixigo-find-command
             (aixigo-get-project-path project-name)
             aixigo-project-branch
             (aixigo-get-project-path project-name)
             aixigo-project-branch
             directory-name
             directory-name
             )
     (current-buffer) )
    (split-string (buffer-string) "\n" t) ) )

(defun aixigo-get-project-includes (project-name)
  "Returns a list of all C++ include directories for the given project, which can be passed to the compiler."
  (if (not (equal project-name nil) )
      (with-temp-buffer
        (shell-command 
         ;; find ./HEAD/modules/ ./HEAD/models/ -wholename "*/include/*" -and -not -name "*CVS" -type d -printf \"%p\"\\n | sort
         (format "%s %s/%s/modules/ %s/%s/models/ -name include -type d -printf %%p\\\\n | sort" 
                 aixigo-find-command
                 (aixigo-get-project-path project-name) 
                 aixigo-project-branch
                 (aixigo-get-project-path project-name) 
                 aixigo-project-branch
                 )
         (current-buffer) )
        (split-string (buffer-string) "\n" t) ) 
    )
  )

(defun aixigo-get-external-modules-include (project-name)
  "Returns the path to the external modules headers for a given project."
  (list (concat (aixigo-get-project-path project-name) "/" aixigo-project-branch "/build/include/external_modules/")
        (concat (aixigo-get-project-path project-name) "/" aixigo-project-branch "/build/include/external_modules/xercesc/")
        (concat (aixigo-get-project-path project-name) "/" aixigo-project-branch "/build/include/external_modules/xerces/")
        (concat (aixigo-get-project-path project-name) "/" aixigo-project-branch "/build/include/tools/")
        )
  )

(defun aixigo-setup-project (project-path)
  "Setup project in given path."
  )

(defun aixigo-choose-project ()
  "Choose a new project to use."
  (interactive)
  (setq aixigo-project-path (read-directory-name "Choose directory containing a project: " ))
  (aixigo-setup-project aixigo-project-path)
  )

(defun aixigo-header-p (file-name)
  "Returns true if buffer seems to be a header file, nil otherwise"
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
