;;; malabar-mode.el --- A better Java mode for Emacs
;;
;; Copyright (c) 2009 Espen Wiborg <espenhw@grumblesmurf.org>
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
(require 'semantic-load)
(require 'semantic-ctxt)
(require 'semantic-find)
(require 'semantic-wisent)

;; HACK: we don't want to load the old Java parser, so trick Emacs
;; into thinking it's already loaded
(provide 'wisent-java-wy)
(require 'wisent-java)

(require 'wisent-malabar-java-wy)
(require 'cl)
(require 'thingatpt)

(require 'malabar-groovy)
(require 'malabar-annotations)
(require 'malabar-abbrevs)
(require 'malabar-util)

(define-mode-local-override semantic-get-local-variables
  malabar-mode ()
  "Get local variable declarations from the current context."
  (let (result
        ;; Ignore funny syntax while doing this.
        semantic-unmatched-syntax-hook)
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (let ((these-blocks (semantic-parse-region
                             (point)
                             (save-excursion (semantic-end-of-context) (point))
                             ;; See this production in wisent-malabar-java.wy.
                             'block_statements
                             nil t)))
          (dolist (block these-blocks)
            (when (semantic-tag-type-members block)
              (push (remove* 'variable
                             (semantic-tag-type-members block)
                             :test-not #'eql
                             :key #'semantic-tag-class)
                    result))))))
    (push (semantic-tag-function-arguments (semantic-current-tag-of-class 'function)) result)
    (apply 'append result)))

(defvar malabar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-v ?\C-b] 'malabar-install-project)
    (define-key map [?\C-c ?\C-v ?\C-c] 'malabar-compile-file)
    (define-key map [?\C-c ?\C-v ?t] 'malabar-run-test)
    (define-key map [?\C-c ?\C-v ?\C-t] 'malabar-run-junit-test-no-maven)
    (define-key map [?\C-c ?\C-v ?\M-t] 'malabar-run-all-tests)
    (define-key map [?\C-c ?\C-v ?\C-z] 'malabar-import-one-class)
    (define-key map [?\C-c ?\C-v ?\C-o] 'malabar-override-method)
    (define-key map [?\C-c ?\C-v ?\C-e] 'malabar-extend-class)
    (define-key map [?\C-c ?\C-v ?\C-i] 'malabar-implement-interface)
    map)
  "Keymap for Malabar mode.")

(define-derived-mode malabar-mode java-mode "malabar"
  "A new, better, Java mode."
  ;; HACK: Since we're not loading the old java parser the installer
  ;; function isn't defined; give it a dummy definition
  (flet ((wisent-java-wy--install-parser () nil))
    (wisent-java-default-setup))
  (add-hook 'semantic-init-hooks #'malabar-semantic-setup)
  (setq semantic-lex-depth 10)
  (setq semantic-lex-analyzer 'wisent-malabar-java-lexer)
  (wisent-malabar-java-wy--install-parser)
  ;; Set up indentation of Java annotations.
  (malabar-annotations-setup)
  (malabar-abbrevs-setup)
  (malabar-groovy-start t))

(remove-hook 'java-mode-hook 'wisent-java-default-setup)

(defun malabar-semantic-setup ()
  ;; Nasty hardcode
  (remove-hook 'semantic-init-hooks 'malabar-semantic-setup)
  (semantic-idle-scheduler-mode 1))

(defun malabar-type-token-candidates ()
  (remove nil (mapcar (lambda (token)
                        (when (eq (car token) 'IDENTIFIER)
                          (buffer-substring-no-properties (cadr token) (cddr token))))
                      (semantic-lex-buffer 1000))))

(defun malabar-type-token-p (token)
  (let ((case-fold-search nil))
    (and (> (length token) 1)
         (some (lambda (re)
                 (string-match (concat "^" re "$") token))
               java-font-lock-extra-types))))

(defun malabar-class-defined-in-buffer-p (classname &optional buffer)
  (let ((tags (semantic-find-tags-by-class 'type (or buffer (current-buffer)))))
    (find classname tags
          :key #'semantic-tag-name
          :test #'equal)))

(defun malabar-find-imported-class (classname &optional buffer)
  (let ((tags (semantic-find-tags-by-class 'include (or buffer (current-buffer)))))
    (let ((import-tag (find classname tags
                            :key #'semantic-tag-name
                            :test #'equal)))
      (or (and import-tag
               (semantic-tag-name import-tag))
          (malabar-find-imported-class-from-wildcard-imports classname buffer)
          (find (concat "java.lang." classname)
                (malabar-qualify-class-name classname buffer)
                :test #'equal)))))

(defun malabar-find-imported-class-from-wildcard-imports (class &optional buffer)
  (let ((tags (semantic-find-tags-by-class 'include (or buffer (current-buffer))))
        (classes (malabar-qualify-class-name class buffer)))
    (some (lambda (tag)
            (find (concat (malabar-get-package-of (semantic-tag-name tag)) "." class)
                  classes
                  :test #'equal))
          tags)))

(defun malabar-import-candidates ()
  (let ((type-tokens (remove-if-not #'malabar-type-token-p (malabar-type-token-candidates))))
    (remove-duplicates
     (remove-if (lambda (token)
                  (or (malabar-class-defined-in-buffer-p token)
                      (malabar-find-imported-class token)))
                type-tokens)
     :test #'equal)))

(defvar malabar-import-excluded-classes-regexp-list
  '("^java\\.lang\\.[^.]+$"                 ;; Always imported
    "^sun\\."                               ;; Implementation internals
    "^com\\.sun\\.xml\\.internal\\."        ;; ...
    "\\$"                                   ;; If you want to import
                                            ;; an inner class, do it
                                            ;; yourself
    )
  "Any class that matches a regexp on this list will never be
automatically imported.")

(defun malabar-import-current-package-p (qualified-class)
  (let ((package (malabar-get-package-name)))
    (when package
      (string-match (concat "^" (regexp-quote package) "\\.[^.]+$") qualified-class))))

(defun malabar-import-exclude (qualified-class)
  (or (some (lambda (re)
              (string-match re qualified-class))
            malabar-import-excluded-classes-regexp-list)
      (malabar-import-current-package-p qualified-class)))

(defvar malabar-import-precedence-order
  '("java.util"
    "java.io"
    "java.net"
    "java.lang.reflect"
    "java.sql"
    "java.text"
    "javax.swing")
  "Sort order by package for classes to import.  A class from a
package not in this list will sort after a class from any package
in the list")

(defun malabar-get-package-of (classname)
  (let ((lastdot (position ?. classname :from-end t)))
    (if lastdot
        (substring classname 0 lastdot)
      "")))

(defun malabar-import-sort-by-precedence (class-a class-b)
  (let ((a-package (malabar-get-package-of class-a))
        (b-package (malabar-get-package-of class-b)))
    (let ((a-package-successors (member a-package malabar-import-precedence-order))
          (b-package-successors (member b-package malabar-import-precedence-order)))
      (or (member b-package a-package-successors)
          (and a-package-successors
               (null b-package-successors))))))

(defun malabar-project (buffer)
  (format "Project.makeProject('%s')" (malabar-maven-find-project-file buffer)))

(defun malabar-project-classpath (buffer)
  (concat (malabar-project buffer) "." (malabar-classpath-of-buffer buffer)))

(define-cached-function malabar-qualify-class-name (unqualified &optional buffer)
  (or (malabar-groovy-eval-and-lispeval
       (format "%s.getClasses('%s')"
               (malabar-project-classpath (or buffer (current-buffer)))
               unqualified))
      (error "Class not found %s" unqualified)))

(defun malabar-classpath-of-buffer (&optional buffer)
  (if (locate-file (buffer-file-name buffer)
                   (malabar-project-test-source-directories (malabar-maven-find-project-file buffer)))
      "testClasspath"
    "compileClasspath"))

(defun malabar-import-find-import (unqualified)
  (let* ((possible-classes
          (sort (remove-if #'malabar-import-exclude
                           (malabar-qualify-class-name unqualified))
                #'malabar-import-sort-by-precedence)))
    (when possible-classes
      (if (= 1 (length possible-classes))
          (car possible-classes)
        (malabar-choose (format "%d classes named '%s', pick one: "
                                (length possible-classes)
                                unqualified)
                        possible-classes
                        (car possible-classes))))))

(defun malabar-import-all ()
  "Attempts to add import statements for all unqualified type
names in the current buffer."
  (interactive)
  (let ((imports (remove nil
                         (mapcar #'malabar-import-find-import
                                 (malabar-import-candidates)))))
    (when imports
      (malabar-import-insert-imports imports))))

(defun malabar-import-one-class (unqualified)
  "Qualifies and adds an import statement for a single type name.
If UNQUALIFIED is NIL, prompts in the minibuffer."
  (interactive (list (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
  (if (or (malabar-class-defined-in-buffer-p unqualified)
          (malabar-find-imported-class unqualified))
      (message "Class %s does not need to be imported" unqualified)
    (let ((class-to-import (malabar-import-find-import unqualified)))
      (unless (null class-to-import)
        (malabar-import-insert-imports (list class-to-import))))))

(defun malabar-choose (prompt choices &optional default)
  "Prompts (with completion) for an element of CHOICES,
defaulting to DEFAULT.  CHOICES may be either a list of strings
or a alist; if an alist, will prompt for a car of CHOICES and
return the corresponding cdr."
  (let ((res (completing-read prompt (if (consp (car choices))
                                         (mapcar #'car choices)
                                       choices) nil t default)))
    (unless (equal "" res)
      (if (consp (car choices))
          (cdr (assoc res choices))
        res))))

(defun malabar-import-insert-imports (qualified-classes)
  (when qualified-classes
    (let* ((tags (semantic-fetch-tags))
           (last-import-tag (car (last (semantic-brute-find-tag-by-class 'include tags))))
           (package-tag (car (semantic-brute-find-tag-by-class 'package tags)))
           (class-tag (car (semantic-brute-find-tag-by-class 'type tags)))
           insertion-point)
      (cond (last-import-tag
             (setq insertion-point (1+ (semantic-tag-end last-import-tag))))
            (package-tag
             (save-excursion
               (goto-char (semantic-tag-end package-tag))
               (forward-line)
               (insert "\n")
               (setq insertion-point (point))))
            (class-tag
             (setq insertion-point
                   (let ((class-doc (semantic-documentation-for-tag class-tag 'lex)))
                     (if class-doc
                         (semantic-lex-token-start class-doc)
                       (semantic-tag-start class-tag)))))
            (t
             (setq insertion-point (point-min))))
      (save-excursion
        (goto-char insertion-point)
        (unless (and (bolp) (eolp))
          (insert "\n"))
        (goto-char insertion-point)
        (dolist (qualified-class qualified-classes)
          (when (> (length qualified-class) 0)
            (insert "import " qualified-class ";\n")
            (message "Imported %s" qualified-class)))))))

(defun malabar-maven-find-project-file (&optional buffer)
  (let ((dir (locate-dominating-file (buffer-file-name (or buffer (current-buffer)))
                                     "pom.xml")))
    (when dir
      (expand-file-name "pom.xml" dir))))

(defun malabar-build-project (goals)
  (malabar-setup-compilation-buffer)
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (concat (format "MvnServer.INSTANCE.run('%s', "
                   (malabar-maven-find-project-file))
           (mapconcat (lambda (s) (format "'%s'" s))
                      (if (atom goals)
                          (list goals)
                        goals)
                      ",")
           ")")))

(defvar malabar-compilation-project-file nil)

(defun malabar-setup-compilation-buffer ()
  (setq malabar-compilation-project-file (malabar-maven-find-project-file))
  (malabar-groovy-setup-compilation-buffer))

(defun malabar-install-project ()
  "Runs 'mvn install' on the current project."
  (interactive)
  (malabar-build-project 'install))

(defun malabar-compile-file ()
  "Compiles the current buffer."
  (interactive)
  (malabar-setup-compilation-buffer)
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (concat (format "%s.compiler.compile('%s')"
                   (malabar-project (current-buffer))
                   (buffer-file-name (current-buffer))))))

(defun malabar-get-package-tag (&optional buffer)
  (car (semantic-find-tags-by-class 'package (or buffer
                                                 (current-buffer)))))

(defun malabar-get-package-name (&optional buffer)
  (let ((package-tag (malabar-get-package-tag)))
    (when package-tag
      (semantic-tag-name package-tag))))

(defun malabar-unqualified-class-name-of-buffer (&optional buffer)
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name (or buffer (current-buffer))))))

(defun malabar-qualified-class-name-of-buffer (&optional buffer)
  (let ((class (malabar-unqualified-class-name-of-buffer buffer)))
    (malabar-qualify-class-name-in-buffer class buffer)))

(defun malabar--type-variable-name-p (class)
  (< (length class) 3))

(defvar malabar-java-primitive-types-with-defaults
  '(("byte" . "0")
    ("short" . "0")
    ("int" . "0")
    ("long" . "0L")
    ("float" . "0.0f")
    ("double" . "0.0d")
    ("char" . "'\\0'")
    ("boolean" . "false")))

(defun malabar--primitive-type-p (class)
  (assoc class malabar-java-primitive-types-with-defaults))

(defun malabar--parametrized-type-p (class)
  (position ?< class))

(defun malabar--raw-type (class)
  (substring class 0 (malabar--parametrized-type-p class)))

(defun malabar--array-type-p (class)
  (position ?\[ class))

(defun malabar--array-element-type (class)
  (substring class 0 (malabar--array-type-p class)))

(defun malabar-qualify-class-name-in-buffer (class &optional buffer)
  (cond ((malabar--type-variable-name-p class)
         class)
        ((malabar--primitive-type-p class)
         class)
        ((malabar--array-type-p class)
         (malabar-qualify-class-name-in-buffer (malabar--array-element-type class)))
        ((malabar--parametrized-type-p class)
         (malabar-qualify-class-name-in-buffer (malabar--raw-type class)))
        (t
         (let* ((buffer (or buffer (current-buffer)))
                (package (malabar-get-package-name buffer))
                (imported-class (malabar-find-imported-class class buffer)))
           (or imported-class
               (if package
                   (concat package "." class)
                 class))))))

(defun malabar-test-class-buffer-p (buffer)
  (let* ((type-tag (car (semantic-brute-find-tag-by-class 'type buffer)))
         (superclasses (semantic-tag-type-superclasses type-tag)))
    (or (member "TestCase" superclasses)
        (member "junit.framework.TestCase" superclasses)
        (member "TestSuite" superclasses)
        (member "junit.framework.TestSuite" superclasses)
        (some (lambda (member-tag)
                (remove-if-not (lambda (m)
                                 (and (string= "@" (substring m 0 1))
                                      (string-ends-with m "Test")))
                               (semantic-tag-modifiers member-tag)))
              (semantic-tag-type-members type-tag)))))

(defun malabar-project-test-source-directories (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLispList(Project.makeProject('%s').testSrcDirectories)"
           project-file)))

(defun malabar-project-source-directories (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLispList(Project.makeProject('%s').srcDirectories)"
           project-file)))

(defvar malabar-compilation-project-test-source-directories nil)

(defun malabar-class-name-to-filename (class-name)
  (concat (replace-regexp-in-string "\\." "/" class-name)
          ".java"))

(defun malabar-find-test-class-from-error ()
  (let* ((class-name (match-string-no-properties 2))
         (class-file (malabar-class-name-to-filename class-name)))
    (list
     (locate-file class-file
                  malabar-compilation-project-test-source-directories))))

(defvar malabar-test-class-suffix "Test")

(defun malabar-corresponding-test-class-name (buffer)
  (let ((package (malabar-get-package-name buffer))
        (type-tag (car (semantic-brute-find-tag-by-class 'type buffer))))
    (let ((class (concat (semantic-tag-name type-tag) malabar-test-class-suffix)))
      (if package
          (concat package "." class)
        class))))

(defun malabar-visit-corresponding-test (&optional buffer silent)
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (if (malabar-test-class-buffer-p buffer)
        buffer
      (let ((class-file (malabar-class-name-to-filename
                         (malabar-corresponding-test-class-name buffer)))
            (test-source-directories (malabar-project-test-source-directories
                                      (malabar-maven-find-project-file buffer))))
        (funcall
         (if silent #'find-file-noselect #'find-file)
         (or (locate-file class-file test-source-directories)
             (expand-file-name class-file (car test-source-directories))))))))

(defun malabar-run-test-internal (test-starter)
  (with-current-buffer (malabar-visit-corresponding-test (current-buffer) t)
    (malabar-setup-compilation-buffer)
    (setq malabar-compilation-project-test-source-directories
          (malabar-project-test-source-directories malabar-compilation-project-file))
    (display-buffer malabar-groovy-compilation-buffer-name t)
    (malabar-groovy-eval-as-compilation
     (format test-starter
             (malabar-qualified-class-name-of-buffer (current-buffer))))))

(defun malabar-run-junit-test-no-maven ()
  "Runs the current buffer (or its corresponding test) as a
standalone JUnit test."
  (interactive)
  (malabar-run-test-internal 
   (format "%s.runJunit('%%s')"
           (malabar-project (current-buffer)))))

(defun malabar-run-test ()
  "Runs the current buffer (or its corresponding test) as a test,
using 'mvn test -Dtestname'."
  (interactive)
  (malabar-run-test-internal
   (format "%s.runtest('%%s')"
           (malabar-project (current-buffer)))))

(defun malabar-run-all-tests ()
  "Runs all project tests ('mvn test')."
  (interactive)
  (malabar-build-project 'test))

(defvar malabar-failed-test-re "^  \\([[:alnum:]]+\\)(\\([[:alnum:].]+\\))$")

(add-to-list 'compilation-error-regexp-alist
             (list malabar-failed-test-re                ;; RE
                   'malabar-find-test-class-from-error)) ;; FILE

(define-cached-function malabar-get-class-info (classname &optional buffer)
  (malabar-groovy-eval-and-lispeval
   (format "%s.getClassInfo('%s')"
           (malabar-project-classpath (or buffer (current-buffer)))
           classname)))

(defsubst malabar--get-property (spec prop)
  (getf (cdr spec) prop))

(defmacro define-spec-properties (&rest props)
  `(progn
     ,@(mapcar (lambda (p)
                 `(defun ,(intern (concat "malabar--get-"
                                          (substring (symbol-name p) 1))) (spec)
                    (malabar--get-property spec ,p)))
               props)))

(define-spec-properties
  :name
  :modifiers
  :type-parameters
  :return-type
  :arguments
  :declaring-class
  :members
  :throws)

(defmacro define-spec-modifier-predicates (&rest props)
  `(progn
     ,@(mapcar (lambda (p)
                 `(defun ,(intern (format "malabar--%s-p" (symbol-name p))) (spec)
                    (member ',p (malabar--get-modifiers spec))))
               props)))

(define-spec-modifier-predicates
  abstract
  public
  private
  protected
  final
  interface)

(defun malabar--package-private-p (spec)
  (not (or (malabar--public-p spec)
           (malabar--protected-p spec)
           (malabar--private-p spec))))

(defmacro define-spec-type-predicates (&rest types)
  `(progn
     ,@(mapcar (lambda (type)
                 `(defsubst ,(intern (format "malabar--%s-p" (symbol-name type))) (spec)
                    (eq ',type (car spec))))
               types)))

(define-spec-type-predicates method constructor class field)

(defun malabar-get-members (classname &optional buffer)
  (malabar--get-members (malabar-get-class-info classname buffer)))

(defun malabar-get-abstract-methods (classname &optional buffer)
  (malabar--get-abstract-methods (malabar-get-class-info classname buffer)))

(defun malabar--get-abstract-methods (class-info)
  (remove-if-not (lambda (m)
                   (and (malabar--method-p m)
                        (malabar--abstract-p m)))
                 (malabar--get-members class-info)))

(defun malabar--arg-name-maker ()
  (lexical-let ((counter -1))
    (lambda (arg)
      (or (getf arg :name)
          (format "arg%s"
                  (incf counter))))))

(defun malabar--cleaned-modifiers (spec)
  (remove 'native (remove 'abstract (malabar--get-modifiers spec))))

(defun malabar-create-simplified-method-signature (method-spec)
  "Creates a readable method signature suitable for
e.g. `malabar-choose'."
  (assert (malabar--method-p method-spec))
  (let ((modifiers (malabar--cleaned-modifiers method-spec))
        (return-type (malabar--get-return-type method-spec))
        (name (malabar--get-name method-spec))
        (arguments (malabar--get-arguments method-spec)))
    (concat name (malabar--stringify-arguments-with-types arguments)
            " : " return-type
            " (" (mapconcat #'symbol-name modifiers " ") ")")))

(defun malabar--stringify-arguments-with-types (arguments)
  (let ((arg-name-maker (malabar--arg-name-maker)))
    (concat "("
            (mapconcat (lambda (arg)
                         (format "%s %s"
                                 (getf arg :type)
                                 (funcall arg-name-maker arg)))
                       arguments
                       ", ")
            ")")))

(defun malabar--stringify-arguments (arguments)
  (concat "("
          (mapconcat (malabar--arg-name-maker)
                     arguments
                     ", ")
          ")"))

(defun malabar-create-method-signature (method-spec)
  "Creates a method signature for insertion in a class file."
  (assert (malabar--method-p method-spec))
  (let ((modifiers (malabar--cleaned-modifiers method-spec))
        (return-type (malabar--get-return-type method-spec))
        (name (malabar--get-name method-spec))
        (arguments (malabar--get-arguments method-spec))
        (type-parameters (malabar--get-type-parameters method-spec))
        (throws (malabar--get-throws method-spec)))
    (malabar--create-method-signature-helper
     name
     modifiers type-parameters return-type arguments throws)))

(defun malabar-create-constructor-signature (method-spec)
  "Creates a constructor signature for insertion in a class file."
  (assert (malabar--constructor-p method-spec))
  (let ((modifiers (malabar--cleaned-modifiers method-spec))
        (arguments (malabar--get-arguments method-spec))
        (type-parameters (malabar--get-type-parameters method-spec))
        (throws (malabar--get-throws method-spec)))
    (malabar--create-method-signature-helper
     (semantic-tag-name (malabar-get-class-tag-at-point))
     modifiers type-parameters nil arguments throws)))

(defun malabar--create-method-signature-helper (name modifiers type-parameters
                                                     return-type arguments throws)
  (concat (mapconcat #'symbol-name modifiers " ")
          " "
          (if type-parameters
              (concat "<" (mapconcat #'identity type-parameters ", ") "> ")
            "")
          (if return-type
              (concat return-type " ")
            "")
          name
          (malabar--stringify-arguments-with-types arguments)
          (if throws
              (concat " throws "
                      (mapconcat #'identity throws ", "))
            "")))

(defun malabar-get-superclass-at-point ()
  (malabar-qualify-class-name-in-buffer (malabar-get-superclass (malabar-get-class-tag-at-point))))

(defun malabar-get-superclass (class-tag)
  (or (car (semantic-tag-type-superclasses class-tag))
       "Object"))
  
(defun malabar-override-method-make-choose-spec (method-spec)
  (cons (malabar-create-simplified-method-signature method-spec)
        method-spec))

(defun malabar-get-class-tag-at-point ()
  (or (semantic-current-tag-of-class 'type)
      (car (semantic-find-tags-by-class 'type (current-buffer)))))

(defun malabar-goto-start-of-class ()
  (interactive)
  (let ((class-tag (malabar-get-class-tag-at-point)))
    (goto-char (semantic-tag-start class-tag))))

(defun malabar-goto-end-of-class ()
  (interactive)
  (let ((class-tag (malabar-get-class-tag-at-point)))
    (goto-char (1- (semantic-tag-end class-tag)))))

(defun malabar-find-method-in-current-class (method-spec)
  (let ((class-tag (malabar-get-class-tag-at-point))
        (method-name (malabar--get-name method-spec))
        (method-argument-types
         (mapcar (lambda (arg)
                   (malabar-qualify-class-name-in-buffer (getf arg :type)))
                 (malabar--get-arguments method-spec))))
    (some (lambda (tag)
            (and (equal method-name
                        (semantic-tag-name tag))
                 (equal method-argument-types 
                        (mapcar (lambda (arg-tag)
                                  (malabar-qualify-class-name-in-buffer
                                   (semantic-tag-type arg-tag)))
                                (semantic-tag-function-arguments tag)))
                 tag))
          (semantic-tag-type-members class-tag))))

(defun malabar-overridable-method-p (method-spec)
  (and (not (malabar--final-p method-spec))
       (not (malabar-find-method-in-current-class method-spec))
       (or (malabar--public-p method-spec)
           (malabar--protected-p method-spec)
           (equal (malabar-get-package-name)
                  (malabar-get-package-of
                   (malabar--get-declaring-class method-spec))))))

(defun malabar-overridable-methods ()
  (remove-if-not (lambda (s)
                   (and (malabar--method-p s)
                        (malabar-overridable-method-p s)))
                 (malabar-get-members
                  (malabar-get-superclass-at-point))))

(defun malabar-override-method (&optional method-spec)
  "Adds a stub implementation overriding method from the
superclass to the class at point.  If METHOD-SPEC is NIL, prompts
for the method to override."
  (interactive)
  (let ((overridable-methods (malabar-overridable-methods)))
    (unless method-spec
      (setq method-spec
            (malabar-choose "Method to override: "
                            (mapcar 'malabar-override-method-make-choose-spec
                                    overridable-methods))))
    (when method-spec
      (malabar--override-method method-spec overridable-methods nil nil t))))

(defun malabar--override-method (method-spec overridable-methods
                                             suppress-annotation no-indent-defun
                                             call-super)
  (malabar-goto-end-of-class)
  (insert "\n" (if suppress-annotation
                   ""
                 "@Override\n")
          (malabar-create-method-signature method-spec) " {\n"
          "// TODO: Stub\n"
          (let ((super-call
                 (concat "super." (malabar--get-name method-spec)
                         (malabar--stringify-arguments
                          (malabar--get-arguments method-spec)))))
            (if (equal (malabar--get-return-type method-spec) "void")
                (if call-super
                    (concat super-call ";\n")
                  "")
              (concat "return "
                      (if call-super
                          super-call
                        (malabar-default-return-value (malabar--get-return-type method-spec)))
                      ";\n")))
          "}\n")
  (forward-line -2)
  (unless no-indent-defun
    (c-indent-defun))
  (back-to-indentation)
  (let ((equals-spec (find-if (lambda (spec)
                                (and (equal (malabar--get-name spec) "equals")
                                     (equal (malabar--get-declaring-class spec)
                                            "java.lang.Object")))
                              overridable-methods))
        (hashcode-spec (find-if (lambda (spec)
                                  (and (equal (malabar--get-name spec) "hashCode")
                                       (equal (malabar--get-declaring-class spec)
                                              "java.lang.Object")))
                                overridable-methods)))
    (cond ((and (equal method-spec equals-spec)
                hashcode-spec)
           (malabar-override-method hashcode-spec))
          ((and (equal method-spec hashcode-spec)
                equals-spec)
           (malabar-override-method equals-spec)))))

(defun malabar-default-return-value (type)
  (let ((cell (assoc type malabar-java-primitive-types-with-defaults)))
    (if cell
        (cdr cell)
      "null")))

(defun malabar-compute-package-name (&optional buffer)
  (let* ((dir (file-name-directory (buffer-file-name buffer)))
         (source-directories (malabar-project-source-directories
                              (malabar-maven-find-project-file buffer))))
    (replace-regexp-in-string
     "/" "."
     (substring dir (1+ (length
                         (find dir source-directories
                               :test #'(lambda (dir src-dir)
                                         (string-starts-with dir src-dir)))))
                (1- (length dir))))))

(defun malabar-update-package ()
  "Updates the package statement in the current buffer to match
the class's location in the file system, adding one if it is not
present."
  (interactive)
  (let ((computed-package (malabar-compute-package-name (current-buffer)))
        (actual-package (malabar-get-package-name (current-buffer))))
    (unless (equal computed-package actual-package)
      (let ((package-tag (malabar-get-package-tag (current-buffer))))
        (save-excursion
          (if (null package-tag)
              (progn (goto-char (point-min))
                     (malabar-forward-comment)
                     (unless (eolp)
                       (insert "\n\n")
                       (forward-line -2)))
            (goto-char (semantic-tag-start package-tag))
            (zap-to-char 1 ?\;))
          (insert "package " computed-package ";")
          ;; Work around a bug in semantic
          (semantic-parse-tree-set-needs-rebuild))))))

(defun malabar-forward-comment ()
  (interactive)
  (c-forward-single-comment)
  (unless (bolp)
    (forward-line 1)))

(defun malabar-prompt-for-and-qualify-class (prompt &optional class)
  (let* ((class (or class
                    (read-from-minibuffer prompt)))
         (qualified-class (or (malabar-import-find-import class)
                              (malabar-qualify-class-name-in-buffer class)))
         (class-info (malabar-get-class-info qualified-class)))
    (list class qualified-class class-info)))

(defun malabar--class-accessible-p (qualified-class class-info)
  (or (malabar--public-p class-info)
      (equal (malabar-get-package-name) (malabar-get-package-of qualified-class))))

(defun malabar--override-all (methods suppress-annotation call-super)
  (let ((method-count (length methods))
        (counter 0)
        (overridable-methods (malabar-overridable-methods)))
    (message nil)
    (working-status-forms "Overriding methods...%s" nil
      (with-caches 
       (dolist (method methods)
         (working-status (/ (* (incf counter) 100) method-count) (malabar--get-name method))
         (malabar--override-method method overridable-methods suppress-annotation
                                   t call-super)))
      (working-status t "done"))
    (let ((class-tag (malabar-get-class-tag-at-point)))
      (indent-region (semantic-tag-start class-tag) (semantic-tag-end class-tag)))))

(defun malabar-implement-interface (&optional interface)
  "Adds INTERFACE to the current class's implements clause and
adds stub implementations of all the interface's methods."
  (interactive)
  (destructuring-bind (interface qualified-interface interface-info)
      (malabar-prompt-for-and-qualify-class "Interface to implement: " interface)
    (unless (malabar--interface-p interface-info)
      (error "You cannot implement %s, it is not an interface"
             qualified-interface))
    (unless (malabar--class-accessible-p qualified-interface interface-info)
      (error "You cannot implement %s, it is not accessible from %s"
             qualified-interface (malabar-get-package-name)))
    (malabar--implement-interface-move-to-insertion-point)
    (if (semantic-tag-type-interfaces (malabar-get-class-tag-at-point))
        (insert ", ")
      (unless (bolp)
        (newline))
      (insert "implements ")
      (indent-according-to-mode))
    (insert interface)
    (when (malabar--get-type-parameters interface-info)
      (insert (concat "<"
                      (mapconcat #'identity (malabar--get-type-parameters interface-info)
                                 ", ")
                      ">")))
    (unless (eolp)
      (newline-and-indent))
    (malabar--override-all (malabar--get-abstract-methods interface-info) t nil)))

(defun malabar--implement-interface-move-to-insertion-point ()
  (malabar-goto-start-of-class)
  (skip-chars-forward "^{")
  (when (semantic-tag-type-interfaces (malabar-get-class-tag-at-point))
    (search-backward
     (car (last (semantic-tag-type-interfaces (malabar-get-class-tag-at-point)))))
    (goto-char (match-end 0))))

(defun malabar-extend-class (&optional class)
  "Alters the class at point to extend CLASS, adding stub
implementations of any abstract methods from CLASS and of
accessible constructors."
  (interactive)
  (unless (equal "class" (semantic-tag-type (malabar-get-class-tag-at-point)))
    (error "Only classes can extend other classes; this is an %s"
           (semantic-tag-type (malabar-get-class-tag-at-point))))
  (unless (equal "java.lang.Object" (malabar-get-superclass-at-point))
    (error "Java is limited to single inheritance, class already extends %s"
           (malabar-get-superclass-at-point)))
  (destructuring-bind (class qualified-class class-info)
      (malabar-prompt-for-and-qualify-class "Class to extend: " class)
    (when (equal qualified-class "java.lang.Enum")
      (error "You cannot extend %s, see the Java Language Specification" qualified-class))
    (unless (malabar--class-accessible-p qualified-class class-info)
      (error "You cannot extend %s, it is not accessible from %s"
             qualified-class (malabar-get-package-name)))
    (when (malabar--final-p class-info)
      (error "You cannot extends %s, it is declared final"
             qualified-class))
    (when (malabar--interface-p class-info)
      (error "You cannot extends %s, it is an interface"
             qualified-class))
    (let* ((members (malabar--get-members class-info))
           (accessible-constructors
            (remove-if-not (lambda (s)
                             (and (malabar--constructor-p s)
                                  (malabar-overridable-method-p s)))
                           members)))
      (unless accessible-constructors
        (error "You cannot extends %s, it has no accessible constructors"
               qualified-class))
      (let ((type-params (malabar--get-type-parameters class-info)))
        (unless (malabar-find-imported-class qualified-class)
          (malabar-import-insert-imports (list qualified-class)))
        (let* ((class-tag (malabar-get-class-tag-at-point))
               (class-start (semantic-tag-start class-tag)))
          (goto-char class-start)
          (skip-chars-forward "^{")
          (search-backward "implements" class-start t)
          (insert "extends " class
                  (if type-params
                      (concat "<" (mapconcat #'identity type-params ", ") ">")
                    ""))
          (indent-according-to-mode)
          (newline-and-indent)
          (semantic-clear-toplevel-cache)
          (malabar--extend-class-move-to-constructor-insertion-point)
          (mapc (lambda (constructor)
                  (insert (malabar-create-constructor-signature constructor) " {\n"
                          "// TODO: Stub\n"
                          "super" (malabar--stringify-arguments
                                   (malabar--get-arguments constructor)) ";"
                          "}\n\n")
                  (forward-line -2)
                  (c-indent-defun)
                  (forward-line 2))
                accessible-constructors)
          (malabar--override-all (malabar--get-abstract-methods class-info) nil t))))))

(defun malabar--extend-class-move-to-constructor-insertion-point ()
  (let ((class-tag (malabar-get-class-tag-at-point)))
    (let ((first-function-tag
           (car (remove* 'function 
                         (semantic-tag-type-members class-tag)
                         :test-not #'eql
                         :key #'semantic-tag-class)))
          (last-field-tag
           (car (last (remove* 'variable 
                               (semantic-tag-type-members class-tag)
                               :test-not #'eql
                               :key #'semantic-tag-class)))))
      (cond ((and first-function-tag
                  last-field-tag)
             (goto-char (min (semantic-tag-start first-function-tag)
                             (semantic-tag-end last-field-tag))))
            (first-function-tag
             (goto-char (semantic-tag-start first-function-tag)))
            (last-field-tag
             (goto-char (semantic-tag-end last-field-tag)))
            (t
             (malabar-goto-end-of-class)))
      (c-forward-single-comment)
      (let* ((found-comment (c-backward-single-comment))
             (back-lines (cond ((not (or first-function-tag last-field-tag))
                                0)
                               ((looking-back "{")
                                -1)
                               (found-comment
                                -2)
                               (t -1)))
             (insert-lines (if (or first-function-tag last-field-tag)
                               2
                             1)))
        (when found-comment
          (forward-line 0))
        (newline insert-lines)
        (forward-line back-lines)))))

(defun malabar--expression-at-point ()
  (let* ((point (point))
         (pseudo-statement-start (progn (c-syntactic-skip-backward "^;,=" nil t) (point))))
    (buffer-substring-no-properties pseudo-statement-start point)))

(defun malabar--expression-components (expression)
  (let ((result nil))
    (with-temp-buffer
      (insert expression)
      (goto-char (point-min))
      (ignore-errors
        (while t
          (let ((start (point)))
                (while (and (not (eobp))
                            (not (looking-at "\\.")))
                  (forward-sexp 1))
                (push (buffer-substring start (point)) result)
                (forward-char 1)))))
    (mapcar (lambda (exp)
              (cons exp (malabar--expression-kind exp)))
            (nreverse result))))

(defun malabar--expression-kind (expression)
  (cond ((string-match-p "^new .*)$" expression)
         'constructor-call)
        ((string-match-p ")$" expression)
         'function-call)
        ((string-match-p "]$" expression)
         'array-reference)
        ((string-match-p "\"$" expression)
         'string-literal)
        ((string= "this" expression)
         'this-reference)
        ((string= "super" expression)
         'super-reference)
        ((string-match-p "^[a-zA-Z_][a-zA-Z0-9_]*$" expression)
         'variable)
        (t
         'unknown)))

(defun malabar--resolve-type-of (exp-and-kind &optional relative-type)
  (let ((expression (car exp-and-kind))
        (kind (cdr exp-and-kind)))
    (or (case kind  ;; Some expression kinds have statically determinable types
          (string-literal "java.lang.String")
          (constructor-call
           (string-match "^new \\([A-Za-z_][^(]*\\)" expression)
           (match-string 1 expression))
          ((array-reference unknown)
           (error "Cannot (yet) resolve type of %s (%s)" expression kind)))
        (cond (relative-type
               ;; TODO: Resolve exp-and-kind in relative-type
               nil)
              ;; Resolve exp-and-kind in class at point
              (t
               (malabar--resolve-type-of-locally exp kind (malabar-get-class-tag-at-point)))))))

(defun malabar--find-tag-named (name tag-list)
  (find name tag-list
        :key #'semantic-tag-name
        :test #'equal))

(defun malabar--find-member-named (name type-tag)
  (malabar--find-tag-named name (semantic-tag-type-members type-tag)))

(defun malabar--resolve-type-of-locally (exp kind class-tag)
  (save-excursion
    (let* ((expression (if (eq kind 'function-call)
                           (progn (string-match "^[^(]+" exp)
                                  (match-string 0 exp))
                         exp))
           (local-type (let ((local-variable (and (eq kind 'variable)
                                                  (malabar--find-tag-named expression (semantic-get-local-variables))))
                             (member (malabar--find-member-named expression class-tag)))
                         (semantic-tag-type (or local-variable
                                                member)))))
      (or local-type
          (malabar--resolve-type-of (cons exp kind) (malabar-get-superclass class-tag))
          (some (lambda (i)
                  (malabar--resolve-type-of (cons exp kind) i))
                (semantic-tag-type-interfaces class-tag))
          (when (and (not (member "static" (semantic-tag-modifiers class-tag)))
                     (not (semantic-up-context (semantic-tag-start class-tag) 'type)))
            (malabar--resolve-type-of-locally exp kind (semantic-current-tag-of-class 'type)))
          (error "Failed to resolve type of %s (%s)" exp kind)))))

(provide 'malabar-mode)
