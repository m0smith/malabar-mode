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
(require 'wisent-malabar-java-wy)
(require 'cl)
(require 'malabar-groovy)
(require 'thingatpt)

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
                             ;; See this production in wisent-java.wy.
                             'block_statements
                             nil t)))
          (dolist (block these-blocks)
            (when (semantic-tag-type-members block)
              (push (remove-if-not (lambda (tag)
                                     (semantic-tag-of-class-p tag 'variable))
                                   (semantic-tag-type-members block))
                    result))))))
    (apply 'append result)))

(defvar malabar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-v ?\C-b] 'malabar-install-project)
    (define-key map [?\C-c ?\C-v ?\C-c] 'malabar-compile-file)
    (define-key map [?\C-c ?\C-v ?\C-t] 'malabar-run-test)
    (define-key map [?\C-c ?\C-v ?\C-z] 'malabar-import-one-class)
    map)
  "Keymap for Malabar mode.")

(define-derived-mode malabar-mode java-mode "malabar"
  "A new, better, Java mode."
  ;; Funky stuff here
  (add-hook 'semantic-init-hooks #'malabar-semantic-setup)
  (setq semantic-lex-depth 10)
  (setq semantic-lex-analyzer 'wisent-malabar-java-lexer)
  (wisent-malabar-java-wy--install-parser)
  )

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

(defun malabar-class-defined-in-current-buffer-p (classname)
  (let ((tags (semantic-find-tags-by-class 'type (current-buffer))))
    (find classname tags
          :key #'semantic-tag-name
          :test #'equal)))

(defun malabar-class-imported-p (classname)
  (let ((tags (semantic-find-tags-by-class 'include (current-buffer))))
    (find classname tags
          :key (lambda (tag)
                 (substring (semantic-tag-name tag)
                            (1+ (position ?. (semantic-tag-name tag) :from-end t))))
          :test #'equal)))

(defun malabar-import-candidates ()
  (let ((type-tokens (remove-if-not #'malabar-type-token-p (malabar-type-token-candidates))))
    (remove-duplicates
     (remove-if (lambda (token)
                  (or (malabar-class-defined-in-current-buffer-p token)
                      (malabar-class-imported-p token)))
                type-tokens)
     :test #'equal)))

(defvar malabar-import-excluded-classes-regexp-list
  '("^java\\.lang\\.[^.]+$"                 ;; Always imported
    "^sun\\."                               ;; Implementation internals
    "^com\\.sun\\.xml\\.internal\\."        ;; ...
    "\\$"                                   ;; If you want to import
                                            ;; an inner class, do it
                                            ;; yourself
    ))

(defun malabar-import-current-package-p (qualified-class)
  (let ((package (malabar-get-package-name)))
    (when package
      (string-match (concat "^" (regexp-quote package) "\\.[^.]+$") qualified-class))))

(defun malabar-import-exclude (qualified-class)
  (or (some (lambda (re)
              (string-match re qualified-class))
            malabar-import-excluded-classes-regexp-list)
      (malabar-import-current-package-p qualified-class)))

(defun malabar-import-find-import (unqualified)
  (let* ((classpath (if (malabar-test-class-buffer-p (current-buffer))
                        "testClasspath"
                      "compileClasspath"))
         (possible-classes
          (remove-if #'malabar-import-exclude
                     (malabar-groovy-eval-and-lispeval
                      (format "Project.makeProject('%s').%s.getClasses('%s')"
                              (malabar-maven-find-project-file)
                              classpath
                              unqualified)))))
    (when possible-classes
      (if (= 1 (length possible-classes))
          (car possible-classes)
        (malabar-choose (format "%d classes named '%s', pick one: "
                                (length possible-classes)
                                unqualified)
                        possible-classes)))))

(defun malabar-import-all ()
  (interactive)
  (let ((imports (remove nil
                         (mapcar #'malabar-import-find-import
                                 (malabar-import-candidates)))))
    (when imports
      (malabar-import-insert-imports imports))))

(defun malabar-import-one-class (unqualified)
  (interactive (list (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
  (if (or (malabar-class-defined-in-current-buffer-p unqualified)
          (malabar-class-imported-p unqualified))
      (message "Class %s does not need to be imported" unqualified)
    (let ((class-to-import (malabar-import-find-import unqualified)))
      (unless (null class-to-import)
        (malabar-import-insert-imports (list class-to-import))))))

(defun malabar-choose (prompt choices)
  (let ((res (completing-read prompt choices nil t)))
    (unless (equal "" res)
      res)))

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

(defun malabar-maven-find-project-file ()
  (let ((dir (locate-dominating-file (buffer-file-name (current-buffer)) "pom.xml")))
    (when dir
      (expand-file-name "pom.xml" dir))))

(defun malabar-maven-define-project (pom-file)
  (malabar-groovy-eval (format "Project.makeProject('%s')" pom-file)))

(defun malabar-make-project ()
  (let ((project-file (malabar-maven-find-project-file)))
    (when project-file
      (let ((result (malabar-maven-define-project project-file)))
        (when (equal "null" (cdr result))
          (eval (car (read-from-string (car result)))))))))

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
  (with-current-buffer (get-buffer-create malabar-groovy-compilation-buffer-name)
    (setq buffer-read-only nil)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (buffer-enable-undo (current-buffer))
    (compilation-mode)
    (setq buffer-read-only nil)))

(defun malabar-install-project ()
  (interactive)
  (malabar-build-project 'install))

(defun malabar-compile-file ()
  (interactive)
  (malabar-setup-compilation-buffer)
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (concat (format "Project.makeProject('%s').compiler.compile('%s')"
                   (malabar-maven-find-project-file)
                   (buffer-file-name (current-buffer))))))

(defun malabar-get-package-name ()
  (let ((package (car (semantic-brute-find-tag-by-class 'package (current-buffer)))))
    (when package
      (semantic-tag-name package))))

(defun malabar-unqualified-class-name-of-buffer (&optional buffer)
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name (or buffer (current-buffer))))))

(defun malabar-test-class-buffer-p (buffer)
  (let* ((type-tag (car (semantic-brute-find-tag-by-class 'type (current-buffer))))
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

(defvar malabar-compilation-project-test-source-directories nil)

(defun malabar-find-test-class-from-error ()
  (let* ((class-name (match-string-no-properties 2))
         (class-file (concat (replace-regexp-in-string "\\." "/" class-name)
                             ".java")))
    (list
     (some (lambda (d)
             (let ((f (expand-file-name class-file d)))
               (when (file-exists-p f)
                 f)))
           malabar-compilation-project-test-source-directories))))

(defun malabar-run-test ()
  (interactive)
  (assert (malabar-test-class-buffer-p (current-buffer)))
  (malabar-setup-compilation-buffer)
  (setq malabar-compilation-project-test-source-directories
        (malabar-project-test-source-directories malabar-compilation-project-file))
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (concat (format "Project.makeProject('%s').runtest('%s')"
                   (malabar-maven-find-project-file)
                   (malabar-unqualified-class-name-of-buffer (current-buffer))))))

(defvar malabar-failed-test-re "^  \\([[:alnum:]]+\\)(\\([[:alnum:].]+\\))$")

(add-to-list 'compilation-error-regexp-alist
             (list malabar-failed-test-re                ;; RE
                   'malabar-find-test-class-from-error)) ;; FILE
                   
(provide 'malabar-mode)
