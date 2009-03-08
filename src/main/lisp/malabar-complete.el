;;; malabar-complete.el --- Code completion for malabar-mode
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
(require 'cl)
(require 'cc-engine)
(require 'semantic-ctxt)

(require 'malabar-util)
(require 'malabar-project)
(require 'malabar-reflection)
(require 'malabar-misc)
(require 'malabar-groovy)

(defun malabar--expression-at-point ()
  (save-excursion
    (let* ((point (point))
           (pseudo-statement-start (progn (c-syntactic-skip-backward "^;,=:" nil t) (point))))
      (string-trim (buffer-substring-no-properties pseudo-statement-start point)))))

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

(defun malabar--expression-components-with-types (components)
  (let ((result nil))
    (dolist (exp-and-kind components)
      (push (cons (car exp-and-kind)
                  (malabar--resolve-type-of exp-and-kind
                                            (malabar-qualify-class-name-in-buffer
                                             (cdr (car-safe result)))))
            result))
    (nreverse result)))

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
          (this-reference
           (or relative-type
               (malabar-unqualified-class-name-of-buffer)))
          ;; TODO: Static dereferencing
          (super-reference
           (if relative-type
               (malabar--get-super-class
                (malabar-get-class-info
                 (malabar-qualify-class-name-in-buffer relative-type)))
             (malabar-get-superclass-at-point)))
          ((array-reference unknown)
           (error "Cannot (yet) resolve type of %s (%s)" expression kind)))
        (cond (relative-type
               (malabar--resolve-type-of-in-type
                expression kind
                (malabar-qualify-class-name-in-buffer relative-type)))
              (t
               (malabar--resolve-type-of-locally expression kind
                                                 (malabar-get-class-tag-at-point)))))))

(defun malabar--resolve-type-of-in-type (exp kind type)
  (let ((expression (if (eq kind 'function-call)
                        (malabar--invoked-function-name exp)
                      exp))
        (candidates (remove* (ecase kind
                               (variable 'field)
                               (function-call 'method))
                             (malabar-get-members type)
                             :test-not #'eq
                             :key #'car)))
    (if (eq kind 'variable)
        (malabar--get-type (find expression
                                 candidates
                                 :test #'equal
                                 :key #'malabar--get-name))
      (let ((arg-count (malabar--invocation-argument-count exp)))
        ;; TODO: Filter methods on argument types
        (malabar--get-return-type
         (find-if (lambda (method)
                    (and (equal expression (malabar--get-name method))
                         (= (length (malabar--get-arguments method))
                            arg-count)))
                  candidates))))))

(defun malabar--find-tags-named (name tag-list)
  (remove* name
           tag-list
           :key #'semantic-tag-name
           :test-not #'equal))

(defun malabar--find-members-named (name type-tag)
  (malabar--find-tags-named name (semantic-tag-type-members type-tag)))

(defun malabar--invocation-argument-count (expression)
  (if (string-match-p "()" expression)
      0
    (1+ (count ?, expression))))

(defun malabar--invoked-function-name (expression)
  (string-match "^[^(]+" expression)
  (match-string 0 expression))

(defun malabar--resolve-type-of-locally (exp kind class-tag)
  (save-excursion
    (let* ((expression (if (eq kind 'function-call)
                           (malabar--invoked-function-name exp)
                         exp))
           (candidates (let ((local-variables
                              (when (eq kind 'variable)
                                (save-excursion
                                  ;; HACK!
                                  (forward-line 0)
                                  (let ((old-point (point)))
                                    (unwind-protect
                                        (progn
                                          (forward-line)
                                          (comment-region old-point (point))
                                          (semantic-fetch-tags)
                                          (malabar--find-tags-named
                                           expression
                                           (semantic-get-local-variables)))
                                      (uncomment-region old-point (point)))))))
                             (members (malabar--find-members-named expression class-tag)))
                         (or local-variables
                             ;; TODO: Filter on argument types
                             (let ((arg-count (malabar--invocation-argument-count exp)))
                               (remove-if-not (lambda (args)
                                                (= (length args) arg-count))
                                              members
                                              :key #'semantic-tag-function-arguments))))))
      (or
       ;; local match
       (when candidates
         (if (= (length candidates) 1)
             ;; TODO: Deal with arrays here
             (semantic-tag-type (car candidates))
           (error "Failed to resolve type of %s (%s): Multiple matches (flying pigs)"
                  exp kind)))
       ;; Try superclass
       (malabar--resolve-type-of (cons exp kind) (malabar-get-superclass class-tag))
       ;; Maybe in an interface?
       (some (lambda (i)
               (malabar--resolve-type-of (cons exp kind) i))
             (semantic-tag-type-interfaces class-tag))
       ;; If we are a non-static inner class, try our outer
       (when (and (not (member "static" (semantic-tag-modifiers class-tag)))
                  (not (semantic-up-context (semantic-tag-start class-tag) 'type)))
         (malabar--resolve-type-of-locally exp kind (semantic-current-tag-of-class 'type)))
       ;; TILT
       (error "Failed to locally resolve type of %s (%s)" exp kind)))))

(defun malabar-complete ()
  (interactive)
  (when (member (char-before (point)) '(?\] ?\)))
    (error "Cannot complete here."))
  (destructuring-bind (seed &rest completions)
      (malabar--complete-internal (malabar--expression-at-point))
    (malabar--insert-completion
     seed
     (if (null (cdr completions)) ;; Single completion
         (car completions)
       (malabar-choose "Completion: "
                       (mapcar 'malabar-make-choose-spec
                               completions))))))

(defun malabar--insert-completion (seed spec)
  (insert (substring (cond ((malabar--field-p spec)
                            (malabar--get-name spec))
                           ((malabar--method-p spec)
                            (concat (malabar--get-name spec)
                                    (malabar--stringify-arguments
                                     (malabar--get-arguments spec))))
                           (t (error "Cannot insert a %s" (car spec))))
                     (length seed))))
    
(defun malabar--complete-internal (expression)
  (if (position ?. expression)
      ;; go back to before last dot, find type, and complete using last component
      (let* ((components (malabar--expression-components expression))
             (completion-seed (caar (last components)))
             (last-component-type
              (cdar
               (last (malabar--expression-components-with-types (butlast components))))))
        (let* ((raw-type (malabar--raw-type last-component-type))
               (local-type-tag (malabar-class-defined-in-buffer-p raw-type)))
          (cons completion-seed
                (remove-if-not (lambda (spec)
                                 (and (malabar--get-name spec)
                                      (string-starts-with (malabar--get-name spec)
                                                          completion-seed)))
                               (if local-type-tag
                                   (mapcar #'malabar--tag-to-spec
                                           (semantic-tag-type-members local-type-tag))
                                 (malabar-get-members
                                  (malabar-qualify-class-name-in-buffer
                                   last-component-type)))))))
    (malabar--complete-internal "this.")))

(defun malabar--tag-to-spec (tag)
  (let ((type (ecase (semantic-tag-class tag)
                (variable 'field)
                (type
                 (if (semantic-tag-get-attribute tag :enum-constant-flag)
                     'field
                   'class))
                ;; TODO: class
                (function
                 (if (semantic-tag-function-constructor-p tag)
                     'constructor
                   'method)))))
    `(,type
      ,@(unless (eq type 'constructor)
          (list :name (semantic-tag-name tag)))
      :modifiers (semantic-tag-modifiers tag)
      ;; TODO :type-parameters
      ;; TODO: Deal with arrays here, too
      ,@(when (eq type 'method)
          (list :return-type (semantic-tag-type tag)))
      ,@(when (eq type 'field)
          (list :type (semantic-tag-type tag)))
      ,@(when (or (eq type 'method)
                  (eq type 'constructor))
          (list :arguments (mapcar (lambda (arg)
                                     (list :name (semantic-tag-name arg)
                                           :type (semantic-tag-type arg)))
                                   (semantic-tag-function-arguments tag))
                :throws (semantic-tag-function-throws tag))))))
        
(provide 'malabar-complete)

;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
