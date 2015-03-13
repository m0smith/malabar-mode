;; -*- lexical-binding: t -*-
;;; malabar-reflection.el --- Reflection handling for malabar-mode
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
(eval-when-compile (require 'cl))
(require 'compile)
;(require 'malabar-project)
;(require 'malabar-groovy)
(require 'malabar-import)
(require 'malabar-util)

(require 'arc-mode)

(defvar malabar-java-primitive-types-with-defaults
  '(("byte" . "0")
    ("short" . "0")
    ("int" . "0")
    ("long" . "0L")
    ("float" . "0.0f")
    ("double" . "0.0d")
    ("char" . "'\\0'")
    ("boolean" . "false")))

(defun malabar--type-variable-name-p (class)
  (< (length class) 3))

(defun malabar--primitive-type-p (class)
  (or (assoc class malabar-java-primitive-types-with-defaults)
      (equal class "void")))

(defun malabar--parametrized-type-p (class)
  (position ?< class))

(defun malabar--array-type-p (class)
  (position ?\[ class))

(defun malabar--array-element-type (class)
  (substring class 0 (malabar--array-type-p class)))

(defun malabar--raw-type (class)
  (malabar--array-element-type
   (substring class 0 (malabar--parametrized-type-p class))))

(defun malabar-qualify-class-name-in-buffer (class &optional buffer)
  (cond ((malabar--type-variable-name-p class)
         class)
        ((malabar--primitive-type-p class)
         class)
        ((malabar--array-type-p class)
         (malabar-qualify-class-name-in-buffer (malabar--array-element-type class) buffer))
        ((malabar--parametrized-type-p class)
         (malabar-qualify-class-name-in-buffer (malabar--raw-type class) buffer))
        (t
         (let* ((buffer (or buffer (current-buffer)))
                (package (malabar-get-package-name buffer))
                (class-tag (malabar-class-defined-in-buffer-p class buffer)))
           (if class-tag
               (if package
                   (concat package "." class)
                 class)
             (or (malabar-find-imported-class class buffer)
                 (concat (or package "") "." class)))))))

(define-cached-function malabar-get-class-info (classname &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (unless (consp classname)
      (or (malabar--get-class-info-from-source classname buffer)
	  (let* ((repo  (expand-file-name malabar-package-maven-repo))
		 (readtable (cons  '(?\" malabar-json-read-string) json-readtable)))
	    (malabar-service-call "tags" (list "repo" repo 
					       "pm"   malabar-mode-project-manager
					       "pmfile" (expand-file-name malabar-mode-project-file)
					       "class" classname)
				  buffer 'list 'plist readtable))))))


;; (defun malabar-which (classname &optional buffer)
;;   "Find which JAR or DIRECTORY has classname in the project which
;; manages buffer.  Buffer need not be visiting a java file.  Any
;; file which is part of the project will work."
;;   (interactive "sClassname:\nbBuffer:")
;;   (let* ((buf (or (get-buffer buffer) (current-buffer)))
;; 	 (mbuf (malabar-project buf))
;; 	 (cmd (format "%s.whichAsLisp( '%s' )" mbuf classname))
;; 	 (rtnval (malabar-groovy-eval-and-lispeval cmd)))
;;     (kill-new rtnval)
;;     (message "Copied %s" rtnval)
;;     rtnval))


;; (defun malabar-classpath-test (&optional buffer)
;;   "Return the test classpath as a string for BUFFER"
;;   (interactive)
;;   (let ((s  (car (malabar-eval-on-project "testClasspath.asClassPath()" buffer))))
;;     (substring s 1 (- (length s) 1))))

;; (defun malabar-classes-directory (&optional buffer)
;;   "Return the test classpath as a string for BUFFER"
;;   (interactive)
;;   (let ((s  (car (malabar-eval-on-project "classesDirectory" buffer))))
;;     (substring s 1 (- (length s) 1))))


(defun malabar--get-class-info-from-source (classname buffer)
  (let ((use-dialog-box nil)
	(project-info (malabar-project-info malabar-mode-project-manager (malabar-find-project-file buffer))))
    (-when-let (source-buffer (or (malabar--load-local-source classname project-info)
				  (and malabar-load-source-from-sibling-projects
				       (malabar--load-sibling-source buffer classname project-info ))
                                 (malabar--load-archived-source classname buffer)
                                 (malabar--load-extra-source classname)))
      (-when-let (tag (malabar--get-class-info-from-buffer source-buffer))
        (malabar--class-info-set-from-source tag)))))

(defun malabar-project-info (pm pmfile &optional repo)
  "Get the project info for a project file"
  (interactive (list
		(completing-read "Project Manager: " malabar-known-project-managers nil nil nil nil malabar-mode-project-manager)
		(read-file-name  "Project file (pom, build.gradle):")))
  (let ((pmfile (or pmfile malabar-mode-project-file))
	(pm (or pm malabar-mode-project-manager))
	(repo (or repo (expand-file-name malabar-package-maven-repo))))
    (unless pmfile
      (error "The malabar project file is not set"))
    (let ((rtnval (malabar-service-call "pi" (list 
					      "repo" repo 
					      "pm" pm 
					      "pmfile" (expand-file-name pmfile)))))
      (when (called-interactively-p 'interactive)
	(message "%s" rtnval))
      rtnval)))
	


(defun malabar--load-local-source (classname project-info)
  ;; First, try resolving in local project
  (malabar--load-project-source classname project-info))

(defun malabar--load-project-source (classname project-info)
  "Take a CLASSNAME like 'org.apache.log4j.Logger' and open the
corresponding source file, if it exists in the current project"
  (-when-let (file (malabar-project-locate (malabar-class-name-to-filename classname)
                                          project-info))
    ;; Defined in this project
    (or (find-buffer-visiting file)            
        (find-file-noselect file))))

(defun malabar--load-sibling-source (buffer classname project-info)
  "Look for CLASSNAME as org.apache.log4j.Logger' in a sibling
project.  A sibling is a different module of this same project
defined by having a parent pom."
  (some (lambda (_project)
	  (malabar--load-project-source classname project-info))
	(malabar--sibling-projects (malabar-find-project-file buffer))))

(defun malabar--load-archived-source (classname buffer)
  ;; Not defined here
  (-when-let (source-jar (malabar--get-source-jar classname buffer))
    (let ((buffer-name
           (malabar--archived-source-buffer-name classname source-jar)))
      (or (get-buffer buffer-name)
          (let ((source-buffer (malabar--load-source-from-zip classname
                                                              source-jar
                                                              buffer-name)))
            (and (buffer-live-p source-buffer)
                 source-buffer))))))

(defun malabar--archived-source-buffer-name (classname archive)
  (concat (if malabar-hide-non-local-source-buffers " " "")
          (file-name-nondirectory (malabar-class-name-to-filename classname))
          " (" (file-name-nondirectory archive) ")"))

(defun malabar--load-extra-source (classname)
  (let ((class-file-name (malabar-class-name-to-filename classname)))
    (some (lambda (path)
            (cond ((file-directory-p path)
                   (let ((file (expand-file-name class-file-name path)))
                     (when (file-readable-p file)
                       (or (find-buffer-visiting file)            
                           (find-file-noselect file)))))
                  ((let ((case-fold-search t))
                     (string-match-p "\\.\\(zip\\|jar\\)$" path))
                   (malabar--load-source-from-zip
                    classname path
                    (malabar--archived-source-buffer-name classname path)))))
          (mapcar (lambda (p)
                    (expand-file-name (substitute-in-file-name p)))
                  malabar-extra-source-locations))))

(defun malabar-semantic-fetch-tags ()
  (unless (semantic-active-p)  
    (semantic-new-buffer-fcn))
  (let ((tags (semantic-fetch-tags)))
    (mapc (lambda (tag)
            (when (semantic-tag-of-class-p tag 'type)
              (when (equal (semantic-tag-type tag) "interface")
                ;; All interface members are public
                (loop for member in (semantic-tag-type-members tag)
                      do (semantic-tag-put-attribute
                          member :typemodifiers
                          (cl-delete-duplicates (cons "public"
                                                   (semantic-tag-modifiers member))
                                             :test #'equal))))
              (-when-let (buffer (semantic-tag-buffer tag))
                (semantic-tag-put-attribute
                 tag :superclasses
                 (mapcar (lambda (c)
                           (malabar-qualify-class-name-in-buffer (malabar--raw-type c)
                                                                 buffer))
                         (semantic-tag-type-superclasses tag))))))
          tags)
    tags))


(defun malabar-get-class-tag-at-point ()
  (malabar-semantic-fetch-tags)
  (or (semantic-current-tag-of-class 'type)
      (car (semantic-find-tags-by-class 'type (current-buffer)))))

  
(defun malabar--load-source-from-zip (classname archive buffer-name)
  ;; TODO:  This won't work for inner classes
  (let ((file-name (malabar-class-name-to-filename classname)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (setq buffer-file-name (expand-file-name (concat archive ":" file-name)))
          (setq buffer-file-truename (file-name-nondirectory file-name))
          (let ((exit-code
                 (let ((coding-system-for-read 'undecided))
                   (archive-extract-by-stdout archive file-name
                                              archive-zip-extract))))
            (if (and (numberp exit-code) (zerop exit-code))
                (progn (set-auto-mode)
                       (goto-char (point-min))
                       (setq buffer-undo-list nil
                             buffer-saved-size (buffer-size)
                             buffer-read-only t)
                       (set-buffer-modified-p nil)
                       (current-buffer))
              (kill-buffer (current-buffer))
              nil))))))

(defun malabar--get-class-info-from-buffer (buffer)
  (with-current-buffer buffer
    (malabar-get-class-tag-at-point)))

(defun malabar--class-info-from-source-p (tag)
  (semantic-tag-get-attribute tag :malabar-from-source))

(defun malabar--class-info-set-from-source (tag)
  (semantic-tag-put-attribute tag :malabar-from-source t))

(define-cached-function malabar--get-source-jar (classname buffer)
  (malabar-reflection-which classname buffer))

  ;; (malabar-groovy-eval-and-lispeval
  ;;  (format "%s.sourceJarForClass('%s')"
  ;;          (malabar-project buffer)
  ;;          classname)))

(defun malabar--get-name (tag)
  (semantic-tag-name tag))

(defun malabar--get-return-type (tag)
  (semantic-tag-type tag))

(defun malabar--get-type (tag)
  (semantic-tag-type tag))

(defun malabar--get-throws (tag)
  (semantic-tag-function-throws tag))

(defun malabar--get-arguments (tag)
  (semantic-tag-function-arguments tag))

(defun malabar--get-type-parameters (tag)
  (semantic-tag-get-attribute tag :template-specifier))

(defun malabar--get-declaring-class (tag)
  (semantic-tag-get-attribute tag :declaring-class))

(defun malabar--get-super-class (tag)
  (semantic-tag-type-superclasses tag))

(defun malabar--get-interfaces (tag)
  (semantic-tag-type-interfaces tag))

(defun malabar--get-modifiers (tag)
  (semantic-tag-modifiers tag))

(defmacro define-tag-modifier-predicates (&rest props)
  `(progn
     ,@(mapcar (lambda (p)
                 (let ((s (symbol-name p)))
                   `(defun ,(intern (format "malabar--%s-p" s)) (tag)
                      (member ,s (malabar--get-modifiers tag)))))
               props)))

(define-tag-modifier-predicates
  abstract
  public
  private
  protected
  final)

(defun malabar--interface-p (tag)
  (equal (semantic-tag-type tag) "interface"))

(defun malabar--package-private-p (tag)
  (not (or (malabar--public-p tag)
           (malabar--protected-p tag)
           (malabar--private-p tag))))

(defun malabar--method-p (tag)
  (let ((class-tag (semantic-tag-class tag)))
    (or
     (eq class-tag 'function)
     (equal class-tag "function"))))
  
(defun malabar--constructor-p (tag)
  (and (malabar--method-p tag)
       (semantic-tag-function-constructor-p tag)))

(defun malabar--class-p (tag)
  (eq (semantic-tag-class tag) 'type))

(defun malabar--field-p (tag)
  (eq (semantic-tag-class tag) 'variable))

(defun malabar-get-members (classname &optional buffer)
  (malabar--get-members (malabar-get-class-info classname buffer)))

(defun malabar--get-members (class-tag)
  (semantic-tag-type-members class-tag))

(defun malabar-get-abstract-methods (classname &optional buffer)
  (malabar--get-abstract-methods (malabar-get-class-info classname buffer)))

(defun malabar--get-methods (tag)
  (remove-if-not #'malabar--method-p (malabar--get-members tag)))

(defun malabar--get-abstract-methods (class-info)
  (remove-if-not (lambda (m)
                   (and (malabar--method-p m)
                        (malabar--abstract-p m)))
                 (malabar--get-members class-info)))

(defun malabar--arg-name-maker ()
  (lexical-let ((counter -1))
    (lambda (arg)
      (or (semantic-tag-name arg)
          (format "arg%s"
                  (incf counter))))))

(defun malabar--cleaned-modifiers (tag)
  (remove 'native (remove 'abstract (malabar--get-modifiers tag))))

(defun malabar-create-simplified-signature (tag)
  "Creates a readable signature suitable for
e.g. `malabar-choose'."
  (let ((s (semantic-format-tag-uml-prototype tag)))
    (if (assoc (substring s 0 1) semantic-format-tag-protection-image-alist)
        (substring s 1)
      s)))

(defun malabar--stringify-arguments (arguments)
  (concat "("
          (mapconcat (malabar--arg-name-maker)
                     arguments
                     ", ")
          ")"))

(defun malabar--method-signature-modifiers (tag)
  (mapconcat #'identity
             (or (remove-if (lambda (m)
                              (or (equal m "abstract")
                                  (equal m "native")
                                  (string-starts-with m "@")))
                            (malabar--get-modifiers tag))
                 '("public"))
             " "))

(defun malabar--add-to-import-list (type)
  (when (boundp 'malabar--import-candidates)
    (pushnew (malabar--raw-type type) malabar--import-candidates
             :test #'equal)))

(defun malabar--method-signature-type (tag)
  (let ((type (semantic-tag-type tag)))
    (malabar--add-to-import-list
     (malabar-qualify-class-name-in-buffer type (or (semantic-tag-buffer tag)
                                                    (current-buffer))))
    type))

(defun malabar--method-signature-parameter (tag)
  (malabar--add-to-import-list (semantic-tag-type tag))
  (semantic-format-tag-prototype tag))

(defun malabar-create-method-signature (tag)
  "Creates a method signature for insertion in a class file."
  (concat (malabar--method-signature-modifiers tag) " "
          (-when-let (tp (malabar--get-type-parameters tag))
            (concat tp " "))
          (malabar--method-signature-type tag) " "
          (semantic-tag-name tag) "("
          (mapconcat #'malabar--method-signature-parameter
                     (semantic-tag-function-arguments tag)
                     ", ")
          ")"))

(defun malabar-create-constructor-signature (tag)
  "Creates a constructor signature for insertion in a class file."
  (malabar-create-method-signature
   (semantic-tag-copy tag (semantic-tag-name (malabar-get-class-tag-at-point)))))

(defun malabar-make-choose-spec (tag)
  (cons (malabar-create-simplified-signature tag)
        tag))

(defun malabar-default-return-value (type)
  (let ((cell (assoc type malabar-java-primitive-types-with-defaults)))
    (if cell
        (cdr cell)
      "null")))

(defun malabar--class-accessible-p (qualified-class class-info)
  (or (malabar--public-p class-info)
      (equal (malabar-get-package-name) (malabar-get-package-of qualified-class))))

(define-cached-function malabar-qualify-class-name (unqualified &optional buffer)
  "A list of all matching classes or nil"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer 
      (let* ((result-array (malabar-service-call "resource" 
						 (list 
						  "pm" malabar-mode-project-manager
						  "pmfile" (expand-file-name malabar-mode-project-file)
						  "repo"(expand-file-name malabar-package-maven-repo)
						  "pattern" (format "[.]%s$" unqualified)
						  "isClass" "true"
						  "useRegex" "true"
						  "max" "100")
						 buffer)))
	(mapcar (lambda (e) (cdr (assoc 'key e))) result-array)))))


(define-cached-function malabar-reflection-which (unqualified &optional buffer)
  "The first matching class or nil"
  (with-current-buffer (or buffer (current-buffer))
    (let* ((result-array (malabar-service-call "resource" 
					       (list 
						"pm" malabar-mode-project-manager
						"pmfile" (expand-file-name malabar-mode-project-file)
						"repo"(expand-file-name malabar-package-maven-repo)
						"pattern" unqualified
						"isClass" "true"
						"useRegex" "false"
						"max" "1")
					       buffer))
	   (result-alist (if (> (length result-array) 0) (aref result-array 0)))
	   (value (cdr (assoc 'value result-alist))))
      value)))

(defun malabar--get-type-tag (typename &optional buffer)
  (malabar-get-class-info typename buffer))

(provide 'malabar-reflection)

;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
