;;; malabar-test.el --- Test handling for malabar-mode
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
(require 'malabar-misc)
(require 'malabar-util)

(defcustom malabar-test-class-suffix "Test"
  "The suffix of a test class."
  :group 'malabar-mode
  :type 'string)

(defcustom malabar-test-class-buffer-p-function 'malabar-test-class-buffer-p/simple
  "The predicate function for check test class name."
  :group 'malabar-mode
  :type '(choice
	  (const :tag "Simple" malabar-test-class-buffer-p/simple)
	  (const :tag "Strict" malabar-test-class-buffer-p/strict)
	  (function :tag "Other Function")))


(defvar malabar-failed-maven-test-re "^  \\([[:alnum:]]+\\)(\\([[:alnum:].]+\\))$")
(defvar malabar-failed-junit-test-re "^  Failure point:  \\([^:]+\\):\\([0-9]+\\)$")

(defun malabar-test-class-buffer-p (buffer)
  "Return non-nil when buffer is test class."
  (funcall malabar-test-class-buffer-p-function buffer))

(defun malabar-test-class-buffer-p/strict (buffer)
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

(defun malabar-test-class-buffer-p/simple (buffer)
  (let ((filename (buffer-file-name buffer)))
    (and filename (string-match "Test$" (file-name-sans-extension filename)))))

(defun malabar-find-test-class-from-error ()
  (let* ((class-name (match-string-no-properties 2))
         (class-file (malabar-class-name-to-filename class-name)))
    (list
     (locate-file class-file
                  malabar-compilation-project-test-source-directories))))

(defun malabar-find-test-class-from-junit-failure ()
  (let ((class-file (match-string-no-properties 1)))
    (list (or (malabar--locate-in-path class-file malabar-compilation-project-test-source-directories)
              class-file))))

(defun malabar-corresponding-test-class-name (buffer)
  (let ((package (malabar-get-package-name buffer))
        (type-tag (car (semantic-brute-find-tag-by-class 'type buffer))))
    (let ((class (concat (semantic-tag-name type-tag) malabar-test-class-suffix)))
      (if package
          (concat package "." class)
        class))))

(defun malabar-find-corresponding-test (&optional buffer)
  "Returns test source file corresponding to BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (if (malabar-test-class-buffer-p buffer)
	(buffer-file-name buffer)
      (let ((class-file (malabar-class-name-to-filename
                         (malabar-corresponding-test-class-name buffer)
                         (file-name-extension (buffer-file-name buffer) t)))
            (test-source-directories (malabar-project-test-source-directories
                                      (malabar-find-project-file buffer))))
        (or (locate-file class-file test-source-directories)
	    (expand-file-name class-file (car test-source-directories)))))))
  

(defun malabar-visit-corresponding-test (&optional buffer silent)
  "Returns buffer visiting the test source file corresponding to BUFFER."
  (interactive)
  (let ((filename (malabar-find-corresponding-test buffer)))
    (make-directory (file-name-directory filename) t)
    (if silent
	(find-file-noselect filename t)
      (find-file filename))))

(defun malabar-run-test-internal (test-starter &optional requires-qualification)
  (malabar-setup-compilation-buffer '()) ;; HACK
  (setq malabar-compilation-project-test-source-directories
        (malabar-project-test-source-directories malabar-compilation-project-file))
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (format test-starter
           (if requires-qualification
               (malabar-qualified-class-name-of-buffer (current-buffer))
             (malabar-unqualified-class-name-of-buffer (current-buffer))))))

(put 'with-existing-corresponding-test-buffer 'lisp-indent-function 1)
(defmacro* with-existing-corresponding-test-buffer ((buffer silent) &body body)
  `(when (file-exists-p (malabar-find-corresponding-test ,buffer))
     (with-current-buffer (malabar-visit-corresponding-test ,buffer ,silent)
       ,@body)))

(defun malabar-run-junit-test ()
  "Runs the current buffer (or its corresponding test) as a
standalone JUnit test.  NOP if there is no corresponding test."
  (interactive)
  (let* ((cur-buf (current-buffer))
         (files-to-compile (list (buffer-file-name cur-buf))))
    (with-existing-corresponding-test-buffer (cur-buf t)
      (unless (eq cur-buf (current-buffer))
        (push (buffer-file-name (current-buffer)) files-to-compile))
      (malabar-compile-files (nreverse files-to-compile) nil)
      (malabar-run-test-internal 
       (format "%s.runJunit('%%s')"
               (malabar-project cur-buf))
       t))))

(defun malabar-run-test ()
  "Runs the current buffer (or its corresponding test) as a test,
using 'mvn test -Dtestname'."
  (interactive)
  (with-existing-corresponding-test-buffer ((current-buffer) nil)
    (malabar-run-test-internal
     (format "%s.runtest('%%s')"
             (malabar-project (current-buffer))))))

(defun malabar-run-all-tests (clean-p)
  "Runs all project tests ('mvn test').  With prefix argument,
cleans the project first ('mvn clean test')."
  (interactive "P")
  (malabar-build-project clean-p 'test))

(add-to-list 'compilation-error-regexp-alist
             (list malabar-failed-maven-test-re          ;; RE
                   'malabar-find-test-class-from-error)) ;; FILE

(add-to-list 'compilation-error-regexp-alist
             (list malabar-failed-junit-test-re                  ;; RE
                   'malabar-find-test-class-from-junit-failure   ;; FILE
                   2))                                           ;; LINE

(provide 'malabar-test)
;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
