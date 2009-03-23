;;; malabar-project.el --- Reflection handling for malabar-mode
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
(require 'malabar-project)
(require 'malabar-groovy)
(require 'malabar-util)

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
                (class-tag (malabar-class-defined-in-buffer-p class buffer)))
           (if class-tag
               (if package
                   (concat package "." class)
                 class)
             (or (malabar-find-imported-class class buffer)
                 (find (concat (or package "") "." class)
                       (malabar-qualify-class-name class buffer)
                       :test #'equal)))))))

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
  :super-class
  :interfaces
  :throws
  :type)

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

(defun malabar-create-simplified-signature (spec)
  "Creates a readable signature suitable for
e.g. `malabar-choose'."
  (let ((modifiers (malabar--cleaned-modifiers spec))
        (type (cond ((malabar--method-p spec)
                     (malabar--get-return-type spec))
                    ((malabar--field-p spec)
                     (malabar--get-type spec))
                    ((malabar--class-p spec)
                     "class")
                    (t
                     (error "Can't create signature for a %s" (car spec)))))
        (name (malabar--get-name spec))
        (arguments (malabar--get-arguments spec)))
    (concat name (if (malabar--method-p spec)
                     (malabar--stringify-arguments-with-types arguments)
                   "")
            " : " type
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

(defun malabar-make-choose-spec (spec)
  (cons (malabar-create-simplified-signature spec)
        spec))

(defun malabar-default-return-value (type)
  (let ((cell (assoc type malabar-java-primitive-types-with-defaults)))
    (if cell
        (cdr cell)
      "null")))

(defun malabar--class-accessible-p (qualified-class class-info)
  (or (malabar--public-p class-info)
      (equal (malabar-get-package-name) (malabar-get-package-of qualified-class))))

(define-cached-function malabar-qualify-class-name (unqualified &optional buffer)
  (or (malabar-groovy-eval-and-lispeval
       (format "%s.getClasses('%s')"
               (malabar-project-classpath (or buffer (current-buffer)))
               unqualified))
      (error "Class not found %s" unqualified)))

(defun malabar--get-type-tag (typename &optional buffer)
  (let ((class-info (malabar-get-class-info typename buffer)))
    (when class-info
      (semantic-tag-new-type (malabar--get-name class-info)
                             (cond ((malabar--interface-p class-info)
                                    "interface")
                                   ;; TODO enums
                                   (t
                                    "class"))
                             (remove nil
                                     (mapcar (lambda (spec)
                                               (cond ((malabar--field-p spec)
                                                      (malabar--as-variable-tag spec))
                                                     ((malabar--method-p spec)
                                                      (malabar--as-function-tag spec))
                                                     (t
                                                      nil)))
                                             (malabar--get-members class-info)))
                             (cons (malabar--get-super-class class-info)
                                   (malabar--get-interfaces class-info))))))

(defun malabar--as-variable-tag (spec)
  nil)

(defun malabar--as-function-tag (spec)
  (semantic-tag-new-function (malabar--get-name spec)
                             (malabar--get-return-type spec)
                             (let ((arg-name-maker (malabar--arg-name-maker)))
                               (mapcar (lambda (arg)
                                         (semantic-tag-new-variable
                                          (funcall arg-name-maker arg)
                                          (getf arg :type)))
                                       (malabar--get-arguments spec)))
                             :typemodifiers (mapcar #'symbol-name
                                                    (malabar--get-modifiers spec))))

(provide 'malabar-reflection)

;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
