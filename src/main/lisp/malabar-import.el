;;; malabar-import.el --- Import stuff for malabar-mode
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
(require 'cc-vars)

(require 'malabar-util)
(require 'malabar-reflection)

(defcustom malabar-import-excluded-classes-regexp-list
  '("^java\\.lang\\.[^.]+$"                 ;; Always imported
    "^sun\\."                               ;; Implementation internals
    "^com\\.sun\\..*\\.internal\\."         ;; ...
    "\\$"                                   ;; If you want to import
                                            ;; an inner class, do it
                                            ;; yourself
    )
  "Any class that matches a regexp on this list will never be
automatically imported."
  :group 'malabar-mode
  :type '(repeat regexp))

(defcustom malabar-import-precedence-order
  '("java.util"
    "java.io"
    "java.net"
    "java.lang.reflect"
    "java.sql"
    "java.text"
    "javax.swing")
  "Sort order by package for classes to import.  A class from a
package not in this list will sort after a class from any package
in the list."
  :group 'malabar-mode
  :type '(repeat string))

(defcustom malabar-import-post-insert-function nil
  "Function run after inserting imports."
  :group 'malabar-mode
  :type '(radio (const nil)
                (function-item malabar-import-sort-imports)
                (function-item malabar-import-group-imports)
                function))

(defun malabar-type-token-candidates ()
  (remove nil (mapcar (lambda (token)
                        (when (eq (car token) 'IDENTIFIER)
                          (buffer-substring-no-properties (cadr token) (cddr token))))
                      (semantic-lex-buffer 1000))))

(defun malabar-type-token-p (token)
  (let ((case-fold-search nil))
    (and (> (length token) 1)
         (some (lambda (re)
                 (string-match-p (concat "^" re "$") token))
               java-font-lock-extra-types))))

(defun malabar-find-imported-class (classname &optional buffer)
  (let ((tags (semantic-find-tags-by-class 'include (or buffer (current-buffer)))))
    (let ((import-tag (find classname tags
                            :key #'semantic-tag-name
                            :test (lambda (classname tag)
                                    (string-ends-with tag (concat "." classname))))))
      (or (when (> (count ?. classname) 1)
            classname)
          (when import-tag
            (semantic-tag-name import-tag))
          (malabar-find-imported-class-from-wildcard-imports classname buffer)
          (find (concat "java.lang." classname)
                (malabar-qualify-class-name classname buffer)
                :test #'equal)))))

(defun malabar-find-imported-class-from-wildcard-imports (class &optional buffer)
  (let ((tags
         (remove-if-not (lambda (package)
                          (string-ends-with package "*"))
                        (semantic-find-tags-by-class 'include (or buffer (current-buffer)))
                        :key #'semantic-tag-name))
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

(defun malabar-import-current-package-p (qualified-class)
  (let ((package (malabar-get-package-name)))
    (when package
      (string-match-p (concat "^" (regexp-quote package) "\\.[^.]+$") qualified-class))))

(defun malabar-import-exclude (qualified-class)
  (or (some (lambda (re)
              (string-match-p re qualified-class))
            malabar-import-excluded-classes-regexp-list)
      (malabar-import-current-package-p qualified-class)))

(defun malabar-import-sort-by-precedence (class-a class-b)
  (let ((a-package (malabar-get-package-of class-a))
        (b-package (malabar-get-package-of class-b)))
    (let ((a-package-successors (member a-package malabar-import-precedence-order))
          (b-package-successors (member b-package malabar-import-precedence-order)))
      (or (member b-package a-package-successors)
          (and a-package-successors
               (null b-package-successors))))))

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

(defun malabar--type-at-point ()
  (save-excursion
    (skip-chars-forward "A-Za-z0-9._")
    (mapconcat 'identity (semantic-ctxt-current-thing) ".")))

(defun malabar-import-and-unqualify (qualified)
  "Imports QUALIFIED if necessary, and unqualifies all
occurrences of qualified in buffer, unless another class of the
same name is already imported."
  (interactive (list (read-from-minibuffer "Class: " (malabar--type-at-point))))
  (let* ((unqualified (malabar-get-classname qualified))
         (existing-import (malabar-find-imported-class unqualified)))
    (cond ((equal existing-import qualified)
           ;; Already imported, just unqualify
           (message "Class %s does not need to be imported" qualified)
           (malabar--conditional-replace (regexp-quote qualified) unqualified
                                         (point-min) (point-max)
                                         (lambda ()
                                           (not (semantic-current-tag-of-class 'include)))))
          ((null existing-import)
           (malabar--conditional-replace (regexp-quote qualified) unqualified
                                         (point-min) (point-max)
                                         (lambda ()
                                           (not (semantic-current-tag-of-class 'include))))
           (malabar-import-insert-imports (list qualified)))
          (t
           (message "Import of %s would clash with %s"
                    qualified existing-import)))))

(defun malabar--import-handle-import-candidates (candidates)
  (dolist (candidate candidates)
    (unless (or (malabar--primitive-type-p candidate)
                (malabar--type-variable-name-p candidate))
      (malabar-import-and-unqualify candidate))))

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
            (message "Imported %s" qualified-class)))
        (when malabar-import-post-insert-function
          (funcall malabar-import-post-insert-function))))))

(defun malabar-import--imports-region ()
  (semantic-parse-tree-set-needs-rebuild)
  (let* ((tags (semantic-fetch-tags))
         (import-tags (semantic-brute-find-tag-by-class 'include tags))
         (first-import (car import-tags))
         (last-import (car (last import-tags))))
    (list (semantic-tag-start first-import)
          (semantic-tag-end last-import))))

(defun malabar-import-sort-imports ()
  "Sort imports alphabetically, removing blank lines."
  (interactive)
  ;; This screws any inline comments on imports.  Watch me care.
  (destructuring-bind (start end)
      (malabar-import--imports-region)
    (sort-lines nil start end)
    (delete-matching-lines "^\\s-*$" start end)))

(defcustom malabar-import-group-token-count 2
  "The number of tokens to consider when grouping imports.
Mostly this controls the positioning of blank lines.  If the
symbol ALL, consider the entire package."
  :group 'malabar-mode
  :type '(choice integer
                 (const all)))

(defun malabar-import-group-imports ()
  "Sort imports, and then group them by
`malabar-import-group-token-count' tokens."
  (interactive)
  ;; This screws any inline comments on imports.  Watch me care.
  (malabar-import-sort-imports)
  (let* ((region (malabar-import--imports-region))
         (start (car region))
         (end (cadr region)))
    (goto-char start)
    (let ((last-tag (semantic-current-tag-of-class 'include))
          (tag (progn (forward-line 1)
                      (semantic-current-tag-of-class 'include))))
      (flet ((package (include-tag)
                      (let ((package-tokens
                             (nreverse
                              (split-string
                               (semantic-tag-name include-tag)
                               "\\."))))
                        (nreverse (if (eq malabar-import-group-token-count 'all)
                                      (cdr package-tokens)
                                    (last package-tokens
                                          malabar-import-group-token-count))))))
        (while tag
          (unless (equal (package last-tag) (package tag))
            (insert "\n"))
          (forward-line 1)
          (setq last-tag tag)
          (setq tag (semantic-current-tag-of-class 'include)))))))

(provide 'malabar-import)
;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
