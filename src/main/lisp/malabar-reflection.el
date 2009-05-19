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
  (or (assoc class malabar-java-primitive-types-with-defaults)
      (equal class "void")))

(defun malabar--parametrized-type-p (class)
  (position ?< class))

(defun malabar--raw-type (class)
  (malabar--array-element-type
   (substring class 0 (malabar--parametrized-type-p class))))

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
                 (concat (or package "") "." class)))))))

(define-cached-function malabar-get-class-info (classname &optional buffer)
  (malabar-groovy-eval-and-lispeval
   (format "%s.getClassInfo('%s')"
           (malabar-project-classpath (or buffer (current-buffer)))
           classname)))

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
  final
  interface)

(defun malabar--package-private-p (tag)
  (not (or (malabar--public-p tag)
           (malabar--protected-p tag)
           (malabar--private-p tag))))

(defun malabar--method-p (tag)
  (eq (semantic-tag-class tag) 'function))

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

(defun malabar--cleaned-modifiers (tag)
  (remove 'native (remove 'abstract (malabar--get-modifiers spec))))

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

(defun malabar-create-method-signature (tag)
  "Creates a method signature for insertion in a class file."
  (let ((tag (semantic-tag-copy tag)))
    (semantic-tag-put-attribute tag :typemodifiers
                                (remove "abstract"
                                        (remove "native"
                                                (malabar--get-modifiers tag))))
    (semantic-format-tag-prototype tag)))

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
  (malabar-groovy-eval-and-lispeval
   (format "%s.getClasses('%s')"
           (malabar-project-classpath (or buffer (current-buffer)))
           unqualified)))

(defun malabar--get-type-tag (typename &optional buffer)
  (malabar-get-class-info typename buffer))

(provide 'malabar-reflection)

;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
