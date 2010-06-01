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
  "Returns (TEST-BUF . EXISTED) where TEST-BUF is a buffer
visiting the test class corresponding to BUFFER and EXISTED is T
if the file already existed."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (if (malabar-test-class-buffer-p buffer)
        (cons buffer t)
      (let ((class-file (malabar-class-name-to-filename
                         (malabar-corresponding-test-class-name buffer)
                         (file-name-extension (buffer-file-name buffer) t)))
            (test-source-directories (malabar-project-test-source-directories
                                      (malabar-find-project-file buffer))))
        (let ((filename (or (locate-file class-file test-source-directories)
                            (expand-file-name class-file (car test-source-directories)))))
          (cons (if silent
                    (find-file-noselect filename t)
                  (find-file filename))
                (file-exists-p filename)))))))

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
  (let ((res (gensym))
        (buf (gensym))
        (exist-p (gensym)))
    `(let* ((,res (malabar-visit-corresponding-test ,buffer ,silent))
            (,buf (car ,res))
            (,exist-p (cdr ,res)))
       (if ,exist-p
           (with-current-buffer ,buf
             ,@body)
         ;; the visit-function created a buffer, kill it
         (kill-buffer ,buf)))))

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
