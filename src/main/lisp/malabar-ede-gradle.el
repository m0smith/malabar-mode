;; -*- lexical-binding: t -*-
;;; malabar-ede-gradle.el --- Gradle integration with EDE

;; Copyright (c) Matthew O. Smith <matt@m0smith.com>
;;
;; Author: 
;;     Espen Wiborg <espenhw@grumblesmurf.org>
;;     Matthew Smith <matt@m0smith.com>
;; URL: http://www.github.com/m0smith/malabar-mode
;; Version: 1.6-M8
;; Package-Requires: ((fringe-helper "1.0.1"))
;; Keywords: java, gradle, language, malabar

;;; License:

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gradle Integration with EDE
;;

;;; Code:

(require 'malabar-project)
(require 'malabar-reflection)

(defvar ede-gradle-project-list nil
  "List of projects created by option `ede-gradle'.")

(defun malabar-gradle-extract-classpath (pom-file)
  (interactive "DProject dir:")
  (let ((pi (malabar-project-info "gradle" pom-file)))
    (-filter 'file-exists-p 
	     (apply #'append
		    (malabar-project-additional-classpath)
		    (malabar-project-resources pi 'test)
		    (malabar-project-resources pi 'runtime)
		    (malabar-project-sources pi 'test)
		    (malabar-project-sources pi 'runtime)
		    (malabar-project-elements pi 'test)
		    (malabar-project-elements pi 'runtime)
		    (malabar-project-classpath-list pi 'test)
		    nil))))

(defun malabar-ede-gradle-execute (dir tasks &rest options)
  "Execute command-line gradle tasks.  See
 `ede-maven2-gradle-command' and `ede-maven2-gradle-options'"
  (interactive "DDir:\nsTask:")
  (add-to-list (make-local-variable 'compilation-environment)
	       (format "JAVA_HOME=%s" (malabar-project-java-home)))
  (let ((default-directory dir))
    (compile (combine-and-quote-strings
	      (append (list ede-maven2-gradle-command)
		      ede-maven2-gradle-options
		      (if (listp tasks) tasks (list tasks))
		      options)))))




;;;###autoload
(defclass ede-malabar-gradle-project (ede-maven2-project)
  ((tracking-symbol :initform 'ede-gradle-project-list)
   (file-header-line :initform ";; EDE Gradle project wrapper")
   (current-targets :initform nil :initarg :current-targets)
   (pom :initform nil
	:initarg :pom
	:documentation "Parsed build.gradle file")
   )
  "Project Type for Gradle based Java projects."
  :method-invocation-order :depth-first)

(defmethod initialize-instance ((_this ede-malabar-gradle-project)
                                &rest _fields)
  "Make sure the all targets as setup."
  (call-next-method)
  ;;(ede-normalize-file/directory this "build.gradle")
  ;; TODO: add analysis of build.gradle
  )



(defun malabar-ede-gradle-project-compile-project (proj command)
  (apply #'malabar-ede-gradle-execute 
	 (ede-project-root-directory proj)
	 (or command (oref proj :current-targets))
	 (oref proj :target-options)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;the 2 compile methods below currently do much the same thing.
 ;;  - 1st one tries to find the "root project" and compile it
 ;;  - 2nd compiles the child project the current file is a member of
 ;;gradle error messages are recognized by emacs23

 (defmethod project-compile-project ((proj ede-malabar-gradle-project) &optional command)
   "Compile the entire current project PROJ.
 Argument COMMAND is the command to use when compiling."
   ;; we need to be in the proj root dir for this to work
   (malabar-ede-gradle-project-compile-project proj command))


   ;; (let ((default-directory (ede-project-root-directory proj)))
   ;;   (compile (combine-and-quote-strings
   ;; 	      (append (list ede-maven2-gradle-command)
   ;; 		      ede-maven2-gradle-options
   ;; 		      (list (or command (oref proj :current-targets)))
   ;; 		      (oref proj :target-options))))))

 ;;; Classpath-related...
 (defconst gradle-outfile-name "gradle-classpath")

 (defmethod ede-java-classpath ((proj ede-malabar-gradle-project))
   "Get classpath for gradle project"
   (ede-jvm-get-classpath-from-command proj ede-gradle-execute-mvn-to-get-classpath
				       gradle-outfile-name ede-maven2-gradle-command
				       `(,nil ,nil ,nil "--batch-mode" "dependency:build-classpath"
					      ,(concat "-Dmdep.outputFile=" gradle-outfile-name))))

 ;; TODO: really should be based on content of build.gradle file. But we need parser for it...
 ;; TODO: add caching...
 (defmethod ede-source-paths ((proj ede-malabar-gradle-project) mode)
   "Get the base to all source trees in the current project for MODE."
   (let ((dir (ede-project-root-directory proj)))
     (mapcar (lambda (x) (concat dir x))
	     (cond
	      ((eq mode 'java-mode) '("src/main/java" "src/test/java"))
	      ((eq mode 'clojure-mode) '("src/main/clojure" "src/test/clojure"))))))

 ;; TODO: re-implement when build.gradle parser will be available
 (defmethod project-rescan ((proj ede-malabar-gradle-project))
   "Rescan the EDE proj project THIS."
   (when (ede-jvm-base-file-updated-p proj)
     ;; TODO: fill information
     (oset proj :pom nil)
     ))


 (defclass malabar-jvm-target (ede-jvm-base-target)
   nil
   "")

 (defmethod project-compile-target ((obj malabar-jvm-target) &optional command)
   "Compile the current target OBJ.
 Argument COMMAND is the command to use for compiling the target."
   (when (oref obj :project)
     (add-to-list (make-local-variable 'compilation-environment)
		  (format "JAVA_HOME=%s" (malabar-project-java-home)))
     (project-compile-project (oref obj :project) (or command (oref obj :name)))))


 (defun malabar-gradle-create-target (name dir project)
   (malabar-jvm-target name
		       :name name
		       :path (expand-file-name "build.gradle" dir)
		       :project project))


 (defun malabar-gradle-load (dir &optional _rootproj)
   "Return a Gradle Project object if there is a match.
 Return nil if there isn't one.
 Argument DIR is the directory it is created for.
 ROOTPROJ is nil, since there is only one project."
   (message "Calling malabar-gradle-load on %s" dir)
   (let ((rtnval (or (ede-files-find-existing dir ede-gradle-project-list)
		     ;; Doesn't already exist, so lets make one.
		     (let* ((target-names '("install"))
			    (this
			     (ede-malabar-gradle-project "Malabar Gradle"
							 :name "Malabar gradle dir" ; TODO: make fancy name from dir here.
							 :directory dir
							 :file (expand-file-name "build.gradle" dir)
							 :current-targets  target-names
							 :classpath (malabar-gradle-extract-classpath (expand-file-name "build.gradle" dir)))))
		       (oset this targets 
			     (mapcar (lambda (n) (malabar-gradle-create-target n dir this)) target-names))
		       (ede-add-project-to-global-list this)
		       (setq malabar-mode-project-manager "gradle")
		       ;; TODO: the above seems to be done somewhere else, maybe ede-load-project-file
		       ;; this seems to lead to multiple copies of project objects in ede-projects
		       ;; TODO: call rescan project to setup all data
		       (message "%s" this)
		       this))))
     (when rtnval 
       (message "Setting malabar-mode-project-manager %s" "gradle")
       (setq malabar-mode-project-manager "gradle"))
     rtnval))

(defun ede-malabar-project-root (&optional dir)
  "Get the root directory for DIR."
  (ede-find-project-root "build.gradle" dir))

(ede-add-project-autoload
 (ede-project-autoload "malabar-gradle"
		       :name "MALABAR GRADLE"
		       :file 'malabar-ede-gradle
		       :proj-file  "build.gradle"
		       :proj-root 'ede-malabar-project-root
		       :load-type 'malabar-gradle-load
		       :class-sym 'ede-malabar-gradle-project
		       :new-p nil
		       :safe-p t
		       )
 'unique)

(provide 'malabar-ede-gradle)

;;; malabar-ede-mave ends here
