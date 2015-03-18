;;; malabar-project.el --- Project handling for malabar-mode
;;
;; Copyright (c) 2009, 2010 Espen Wiborg <espenhw@grumblesmurf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA.
;;

;(require 'malabar-groovy)

(require 'malabar-util)
(require 'malabar-variables)
(require 'cl-lib)
(require 'dash)



;; (defun malabar-setup-compilation-buffer (&optional for-files)
;;   (malabar-setup-compilation-buffer-1 for-files (malabar-find-project-file)))

;; (defun malabar-setup-compilation-buffer-1 (for-files project-file)
;;   (setq malabar-compilation-project-file project-file)
;;   (malabar-groovy-setup-compilation-buffer for-files))

;; (defun malabar--clean-compilation-messages (buffer &optional message)
;;   (when (equal buffer (get-buffer malabar-groovy-compilation-buffer-name))
;;     (with-current-buffer buffer
;;       (remove-hook 'after-change-functions 'font-lock-after-change-function t)
;;       (font-lock-fontify-buffer)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (let (locus)
;;           (ignore-errors
;;             (while t
;;               ;; Grubbing in compile's internals, here
;;               (setq locus (compilation-next-error 1 nil (point)))
;;               (setq file (car (car (nth 2 (car locus)))))
;;               (unless (file-readable-p file)
;;                 (let ((end (or (text-property-not-all (point) (point-max) 'message locus)
;;                                (point-max)))
;;                       (file (malabar-project-locate-source-file
;;                              file malabar-compilation-project-file)))
;;                   (if (null file)
;;                       (set-text-properties (point) end nil)
;;                     (rplaca (car (nth 2 (car locus))) file)
;;                     (put-text-property (point) end 'message locus)))))))))))

;; (add-hook 'compilation-finish-functions 'malabar--clean-compilation-messages)


	 

(defun malabar-project-classpath-list (project-info scope)
  "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar 'identity (cdr (assq 'classpath (assq scope project-info)))))

;;;###autoload
(defun malabar-project-resources (project-info scope)
    "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar (lambda (r) (cdr (assq 'directory r)))
	  (cdr (assq 'resources (assq scope project-info)))))

;;;###autoload
(defun malabar-project-sources (project-info scope)
    "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar 'identity (cdr (assq 'sources (assq scope project-info)))))

(defun malabar-project-elements (project-info scope)
    "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar 'identity (cdr (assq 'elements (assq scope project-info)))))

(defun malabar-project-additional-classpath ()
  (interactive)
  "Returns a list of classpath elements outside the normal
  project control.  See `malabar-package-additional-classpath'"
  (-filter 'file-exists-p
	  (mapcar (lambda (p) (concat malabar-mode-project-dir p))
		  malabar-package-additional-classpath)))


(defun malabar-project-locate (file project-info)
  "Given the name of a FILE, location that in the directories which are part of PROJECT-INFO"
  (locate-file file 
               (append (malabar-project-source-directories project-info)
                       (malabar-project-test-source-directories project-info))))

(defun malabar-project-locate-source-file (filename project-file)
  "Locate filename (a basename, i.e. without path) in the project sources."
  (or (malabar-project-locate-in-source-path filename project-file)
      (malabar-project-locate-in-test-source-path filename project-file)))

(defun malabar-project-locate-in-source-path (filename project-file)
  (malabar--locate-in-path filename
                           (malabar-project-source-directories project-file)))

(defun malabar-project-locate-in-test-source-path (filename project-file)
  (malabar--locate-in-path filename
                           (malabar-project-test-source-directories project-file)))

(defun malabar--locate-in-path (filename path)
  (catch 'found
    (dolist (dir path)
      (malabar--find-file filename dir))))

;; (defun malabar-project (buffer)
;;   (malabar-project-expression (malabar-find-project-file buffer)))

;; (defvar malabar-project-active-profiles nil
;;   "Alist of (project-file . active-profiles).")

;; (defun malabar-project-expression (project-file)
;;   (format "Projects.get('%s', %s)"
;;           project-file
;;           (malabar--make-groovy-list
;;            (cdr (assoc project-file malabar-project-active-profiles)))))

;; (defun malabar-project-coordinate (project-file)
;;   (malabar-groovy-eval-and-lispeval
;;    (format "Utils.printAsLisp(%s.coordinate)"
;;            (malabar-project-expression project-file))))

;; (defun malabar-project-classpath (buffer)
;;   (concat (malabar-project buffer) "." (malabar-classpath-of-buffer buffer)))

;; (defun malabar-classpath-of-buffer (&optional buffer)
;;   (let ((file (file-name-nondirectory (buffer-file-name buffer))))
;;     (if (malabar-project-locate-in-source-path file (malabar-find-project-file buffer))
;;         "compileClasspath"
;;       "testClasspath")))

(defun malabar--project-file (dir)
  "Return the directory containing the project file"
  (ede-find-project-root "pom.xml" dir))
  


(defun malabar-project-populate-buffer-locals (&optional dir buffer)
  "Starting with file, populate the buffer locals for BUFFER (defaulting to (current-buffer))

  DIR defaults to default-directory

  creates buffer local
   malabar-mode-project-file - the full path to the pom.xml
   malabar-mode-project-dir  - the directory containing the pom.xml
   malabar-mode-project-name - the name of the directory containing the pom.xml

  returns the full path to the pom.xml"
  
  (with-current-buffer (or buffer (current-buffer))
    (let ((project-dir (malabar--project-file dir)))
      (setq malabar-mode-project-dir project-dir )
      (setq malabar-mode-project-manager "maven" )
      (setq malabar-mode-project-file (format "%spom.xml" project-dir ))
      (setq malabar-mode-project-name (file-name-nondirectory (directory-file-name project-dir))))
    
    malabar-mode-project-file))

(defun malabar-find-project-file (&optional buffer)
  "Return the full path to the pom.xml.  Also populate the
malabar project locals if needed"
  (with-current-buffer (or buffer (current-buffer))
    (or malabar-mode-project-file (malabar-project-populate-buffer-locals nil buffer))))

;; (defun malabar-project-exists-p (&optional buffer)
;;   (-when-let (file (buffer-file-name (or buffer (current-buffer))))
;;     (malabar--project-for-file file)))

;; (defun malabar--project-for-file (file)
;;   (-when-let (dir (locate-dominating-file file "pom.xml"))
;;     (funcall malabar-util-groovy-file-filter (malabar--project-file dir))))


(defun malabar--sibling-projects (project-file)
  "If the PROJECT-FILE (full path to the pom.xml) is a module of a larger project,
return a list of the other modules in the project, nil otherwise"
  
  (let ((parent (file-name-directory
                 (directory-file-name (file-name-directory project-file)))))
    (when (malabar--project-file parent)
      (remove nil
              (mapcar 'malabar--project-file
                      (cl-remove-if-not 'file-accessible-directory-p
                                     (directory-files parent 'full "^[^\\.]")))))))

(defun malabar-visit-project-file ()
  "Visits the project file."
  (interactive)
  (ede-edit-file-target))


(defun malabar-build-project (clean-p &rest goals)
  (when clean-p
    (setq goals (cons 'clean goals)))
  (malabar-ede-maven-execute malabar-mode-project-dir
			     goals))

;; (defun malabar-execute-maven (project-file goals definitions profiles)
;;   (malabar-setup-compilation-buffer)
;;   (display-buffer malabar-groovy-compilation-buffer-name t)
;;   (malabar-groovy-eval-as-compilation
;;    (format "%s.run(%s, %s, %s)"
;;            (malabar-project-expression project-file)
;;            (malabar--make-groovy-list goals)
;;            (malabar--make-groovy-list profiles)
;;            (malabar--make-groovy-map definitions))))
;;;###autoload
(defun malabar-install-project (clean-p)
  "Runs 'mvn install' on the current project.  With prefix
argument, cleans the project first ('mvn clean install')."
  (interactive "P")
  (malabar-build-project clean-p "install"))
;;;###autoload
(defun malabar-package-project (clean-p)
  "Runs 'mvn package' on the current project.  With prefix
argument, cleans the project first ('mvn clean package')."
  (interactive "P")
  (malabar-build-project clean-p "package"))

(defvar malabar-maven-command-line-history nil
  "Minibuffer history for `malabar-run-maven-command`.")
;;;###autoload
(defun malabar-run-maven-command (command-line)
  "Prompts for and executes an (almost) arbitrary Maven command line.
Honors profile activation, property definitions and lifecycle
phases/goals.  E.g.: ``-DskipTests=true -Pdev-mode install`` will
run the install lifecycle with the dev-mode profile active,
skipping tests."
  (interactive (list (read-from-minibuffer "mvn command line: " nil nil nil
                                           'malabar-maven-command-line-history)))
  (let ((parsed-command (malabar-parse-maven-command-line command-line)))
    (apply #'malabar-ede-maven-execute malabar-mode-project-dir
           (car parsed-command))))

(defun malabar-source-from-class (class-element)
  (cond 
   ((file-directory-p (expand-file-name class-element))  class-element)
   ((string-match "[.]jar$" class-element) 
    (let ((rtnval (replace-regexp-in-string "[.]jar$" "-sources.jar" class-element)))
      (if (file-readable-p (expand-file-name rtnval)) rtnval class-element)))
   (t class-element)))


(defun malabar-project-test-source-directories (project-info)
  "Return as a list all the source classpath elements.  Includes
the both runtime and test source, resource and dependencies"
  (-filter 'file-exists-p 
	   (apply #'append
		  (malabar-project-additional-classpath)
		  (malabar-project-resources project-info 'test)
		  (malabar-project-resources project-info 'runtime)
		  (malabar-project-sources project-info 'test)
		  (malabar-project-sources project-info 'runtime)
		  (malabar-project-elements project-info 'test)
		  (malabar-project-elements project-info 'runtime)
		  (mapcar #' malabar-source-from-class (malabar-project-classpath-list project-info 'test))
		  nil)))


(defun malabar-project-source-directories (project-info)
  "Return as a list all the source classpath elements.  Includes the runtime source, resource and dependencies"
  (-filter 'file-exists-p 
	   (apply #'append
		  (malabar-project-additional-classpath)
		  (malabar-project-resources project-info 'runtime)
		  (malabar-project-sources project-info 'runtime)
		  (malabar-project-elements project-info 'runtime)
		  (mapcar #' malabar-source-from-class (malabar-project-classpath-list project-info 'runtime ))
		  nil)))



;; (defun malabar-project-logging-debug (&optional buffer)
;; "Set the loging level to DEBUG for the project that owns the
;; current buffer or BUFFER if it is not nil."
;;   (interactive)
;;   (let ((buffer (or buffer (current-buffer))))
;;     (malabar-eval-on-project "verbose(true)" buffer)))

;; (defun malabar-project-logging-info (&optional buffer)
;; "Set the loging level to DEBUG for the project that owns the
;; current buffer or BUFFER if it is not nil."
;;   (interactive)
;;   (let ((buffer (or buffer (current-buffer))))
;;     (malabar-eval-on-project "verbose(false)" buffer)))


;     (format "Utils.printAsLisp(%s.compiler.LOGGER.setThreshold(0))"
     ;;(format "%s.verbose(true)"
;	     (malabar-project-expression 
;	      (malabar-find-project-file buffer)))))))


;; (defun malabar-project-logging-infoold (&optional buffer)
;; "Set the loging level to INFO for the project that owns current
;; buffer or BUFFER if it is not nil."
;;   (interactive)
;;   (let ((buffer (or buffer (current-buffer))))
;;     (malabar-groovy-eval
;;      (format "Utils.printAsLisp(%s.compiler.LOGGER.setThreshold(1))"
;; ;;     (format "%s.verbose(false)"
;; 	     (malabar-project-expression 
;; 	      (malabar-find-project-file buffer))))))

;; (defun malabar-project-reset-all-projects() 
;;   "Reset the projects and force malabar mode to re-read the pom."
;;   (interactive)
;;   (malabar-groovy-eval "Projects.resetProjects()"))


(provide 'malabar-project)

;;; malabar-project ends here
