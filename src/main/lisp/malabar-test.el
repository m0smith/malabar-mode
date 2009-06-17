;;; malabar-test.el --- Test handling for malabar-mode
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
(require 'malabar-misc)
(require 'malabar-util)

(defcustom malabar-test-class-suffix "Test"
  "The suffix of a test class."
  :group 'malabar-mode
  :type 'string)

(defvar malabar-failed-maven-test-re "^  \\([[:alnum:]]+\\)(\\([[:alnum:].]+\\))$")
(defvar malabar-failed-junit-test-re "^  Failure point:  \\([^:]+\\):\\([0-9]+\\)$")

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

(defun malabar-visit-corresponding-test (&optional buffer silent)
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (if (malabar-test-class-buffer-p buffer)
        buffer
      (let ((class-file (malabar-class-name-to-filename
                         (malabar-corresponding-test-class-name buffer)
                         (file-name-extension (buffer-file-name buffer) t)))
            (test-source-directories (malabar-project-test-source-directories
                                      (malabar-find-project-file buffer))))
        (funcall
         (if silent #'find-file-noselect #'find-file)
         (or (locate-file class-file test-source-directories)
             (expand-file-name class-file (car test-source-directories))))))))

(defun malabar-run-test-internal (test-starter &optional requires-qualification)
  (with-current-buffer (malabar-visit-corresponding-test (current-buffer) t)
    (malabar-setup-compilation-buffer)
    (setq malabar-compilation-project-test-source-directories
          (malabar-project-test-source-directories malabar-compilation-project-file))
    (display-buffer malabar-groovy-compilation-buffer-name t)
    (malabar-groovy-eval-as-compilation
     (format test-starter
             (if requires-qualification
                 (malabar-qualified-class-name-of-buffer (current-buffer))
               (malabar-unqualified-class-name-of-buffer (current-buffer)))))))

(defun malabar-run-junit-test ()
  "Runs the current buffer (or its corresponding test) as a
standalone JUnit test."
  (interactive)
  (let ((cur-buf (current-buffer)))
    (malabar-compile-file)
    (with-current-buffer (malabar-visit-corresponding-test cur-buf t)
      (unless (eq cur-buf (current-buffer))
        (malabar-compile-file))
      (malabar-run-test-internal 
       (format "%s.runJunit('%%s')"
               (malabar-project cur-buf))
       t))))

(defun malabar-run-test ()
  "Runs the current buffer (or its corresponding test) as a test,
using 'mvn test -Dtestname'."
  (interactive)
  (malabar-run-test-internal
   (format "%s.runtest('%%s')"
           (malabar-project (current-buffer)))))

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
