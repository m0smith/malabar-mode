;;; malabar-codegen.el --- Code generation for malabar-mode
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
(require 'cc-cmds)

(require 'malabar-util)
(require 'malabar-reflection)
(require 'malabar-import)
(require 'malabar-misc)

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
                            (mapcar 'malabar-make-choose-spec
                                    overridable-methods))))
    (when method-spec
      (malabar--override-method method-spec overridable-methods nil nil))))

(defun malabar--override-method (method-spec overridable-methods
                                             suppress-annotation no-indent-defun)
  (malabar-goto-end-of-class)
  (let ((call-super (not (malabar--abstract-p method-spec))))
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
                          (malabar-default-return-value
                           (malabar--get-return-type method-spec)))
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
             (malabar-override-method equals-spec))))))

(defun malabar--override-all (methods suppress-annotation)
  (let ((method-count (length methods))
        (counter 0)
        (overridable-methods (malabar-overridable-methods)))
    (message nil)
    (working-status-forms "Overriding methods...%s" nil
      (with-caches 
       (dolist (method methods)
         (working-status (/ (* (incf counter) 100) method-count) (malabar--get-name method))
         (malabar--override-method method overridable-methods suppress-annotation t)))
      (working-status t "done"))
    (let ((class-tag (malabar-get-class-tag-at-point)))
      (indent-region (semantic-tag-start class-tag) (semantic-tag-end class-tag)))))

(defun malabar-implement-interface (&optional interface implement-keyword)
  "Adds INTERFACE to the current class's implements clause and
adds stub implementations of all the interface's methods."
  (interactive)
  (unless implement-keyword
    (setq implement-keyword "implement"))
  (destructuring-bind (interface qualified-interface interface-info)
      (malabar-prompt-for-and-qualify-class (format "Interface to %s: "
                                                    implement-keyword)
                                            interface)
    (unless (malabar--interface-p interface-info)
      (error "You cannot %s %s, it is not an interface"
             implement-keyword qualified-interface))
    (unless (malabar--class-accessible-p qualified-interface interface-info)
      (error "You cannot %s %s, it is not accessible from %s"
             implement-keyword qualified-interface (malabar-get-package-name)))
    (malabar--implement-interface-move-to-insertion-point)
    (if (semantic-tag-type-interfaces (malabar-get-class-tag-at-point))
        (insert ", ")
      (unless (bolp)
        (newline))
      (insert implement-keyword "s ")
      (indent-according-to-mode))
    (insert interface)
    (when (malabar--get-type-parameters interface-info)
      (insert (concat "<"
                      (mapconcat #'identity (malabar--get-type-parameters interface-info)
                                 ", ")
                      ">")))
    (unless (eolp)
      (newline-and-indent))
    (malabar--override-all (malabar--get-abstract-methods interface-info) t)))

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
  (let* ((class-tag (malabar-get-class-tag-at-point))
         (this-kind (semantic-tag-type class-tag)))
    (if (equal this-kind "interface")
        (malabar-implement-interface class "extend")
      (unless (equal this-kind "class")
        (error "Only classes and interfaces can extend other types; this is an %s"
               this-kind))
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
            (let ((class-start (semantic-tag-start class-tag)))
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
                                       (malabar--get-arguments constructor)) ";\n"
                              "}\n\n")
                      (forward-line -2)
                      (c-indent-defun)
                      (forward-line 2))
                    accessible-constructors)
              (malabar--override-all (malabar--get-abstract-methods class-info) nil))))))))

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

(provide 'malabar-codegen)

;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
