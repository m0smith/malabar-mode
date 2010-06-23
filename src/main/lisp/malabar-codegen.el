;;; malabar-codegen.el --- Code generation for malabar-mode
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
(require 'cl)
(require 'cc-cmds)
(if (and (<= emacs-major-version 23)
          (< emacs-minor-version 2))
    (require 'cc-subword)
  (require 'subword)
  (fset 'subword-capitalize 'c-capitalize-subword))

(if malabar-use-external-cedet
    (require 'srecode-getset)
  (require 'srecode/semantic)
  (require 'srecode/getset))

(require 'malabar-variables)
(require 'malabar-util)
(require 'malabar-reflection)
(require 'malabar-import)
(require 'malabar-misc)

(defun malabar-overridable-method-p (method-tag)
  (and (not (malabar--final-p method-tag))
       (not (malabar-find-method-in-current-class method-tag))
       (or (malabar--public-p method-tag)
           (malabar--protected-p method-tag)
           (equal (malabar-get-package-name)
                  (malabar-get-package-of
                   (malabar--get-declaring-class method-tag))))))

(defun malabar-overridable-methods ()
  (remove-if-not (lambda (s)
                   (and (malabar--method-p s)
                        (malabar-overridable-method-p s)))
                 (malabar-get-members
                  (malabar-get-superclass-at-point))))

(defun malabar-override-method (&optional method-tag)
  "Adds a stub implementation overriding method from the
superclass to the class at point.  If METHOD-TAG is NIL, prompts
for the method to override."
  (interactive)
  (let ((overridable-methods (malabar-overridable-methods)))
    (unless method-tag
      (setq method-tag
            (malabar-choose "Method to override: "
                            (mapcar 'malabar-make-choose-spec
                                    overridable-methods))))
    (when method-tag
      (malabar--override-methods (list method-tag) t overridable-methods))))

(defun malabar--override-methods (methods call-super &optional overridable-methods)
  (let* ((method-count (length methods))
         (progress-reporter (make-progress-reporter "Overriding methods..." 0 method-count))
         (counter 0)
         (overridable-methods (or overridable-methods
                                  (malabar-overridable-methods))))
    (message nil)
    (let ((malabar--import-candidates nil))
      (with-caches 
       (dolist (method methods)
         (progress-reporter-update progress-reporter (incf counter))
         (malabar--override-method method overridable-methods call-super t)))
      (malabar--import-handle-import-candidates malabar--import-candidates))
    (progress-reporter-done progress-reporter)
    (let ((class-tag (malabar-get-class-tag-at-point)))
      (indent-region (semantic-tag-start class-tag) (semantic-tag-end class-tag)))))

(defun malabar--override-method (method-tag overridable-methods
                                            is-extension no-indent-defun)
  (malabar-goto-end-of-class)
  (insert "\n" (if is-extension
                   "@Override\n"
                 "")
          (malabar-create-method-signature method-tag) " {\n"
          "// TODO: Stub\n"
          (let ((call-super (and is-extension
                                 (not (malabar--abstract-p method-tag))))
                (super-call
                 (concat "super." (malabar--get-name method-tag)
                         (malabar--stringify-arguments
                          (malabar--get-arguments method-tag)))))
            (if (equal (malabar--get-return-type method-tag) "void")
                (if call-super
                    (concat super-call ";\n")
                  "")
              (concat "return "
                      (if call-super
                          super-call
                        (malabar-default-return-value
                         (malabar--get-return-type method-tag)))
                      ";\n")))
          "}\n")
    (forward-line -2)
    (unless no-indent-defun
      (c-indent-defun))
    (back-to-indentation)
    (flet ((find-tag-from-class (name declaring-class tags)
                                (find-if (lambda (tag)
                                           (and (equal (malabar--get-name tag)
                                                       name)
                                                (equal (malabar--get-declaring-class tag)
                                                       declaring-class)))
                                         tags)))
      (let ((equals-tag (find-tag-from-class "equals" "java.lang.Object"
                                             overridable-methods))
            (hashcode-tag (find-tag-from-class "hashCode" "java.lang.Object"
                                               overridable-methods)))
        (cond ((and (equal method-tag equals-tag)
                    hashcode-tag)
               (malabar-override-method hashcode-tag))
              ((and (equal method-tag hashcode-tag)
                    equals-tag)
               (malabar-override-method equals-tag))))))

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
    (insert qualified-interface)
    (when-let (type-parameters (malabar--get-type-parameters interface-info))
      (insert type-parameters))
    (unless (eolp)
      (newline-and-indent))
    (malabar--override-methods (malabar--get-methods interface-info) nil)
    (malabar--instantiate-type-parameters interface-info)
    (malabar-import-and-unqualify qualified-interface)))

(defun malabar--instantiate-type-parameters (tag)
  (let ((type-instances (malabar--query-for-type-parameters tag))
        (case-fold-search nil)
        (import-candidates nil))
    (while type-instances
      (unless (equal (caar type-instances) (cdar type-instances))
        (goto-char (point-min))
        (let ((re (concat "\\<" (caar type-instances) "\\>"))
              (rep (cdar type-instances)))
          (while (re-search-forward re nil t)
            (replace-match rep t t)))
        (push (cdar type-instances) import-candidates))
      (setq type-instances (cdr type-instances)))
    (malabar--import-handle-import-candidates import-candidates)))

(defun malabar--query-for-type-parameters (tag)
  ;; FIXME: We shouldn't need to parse this here!
  ;; FIXME: Deals badly with nested parameters
  (let ((type-parameters
         (malabar--parse-type-parameters (malabar--get-type-parameters tag))))
    (with-caches
     (mapcar (lambda (p)
               (cons p
                     (let ((read-param (second (malabar-prompt-for-and-qualify-class
                                                (format "Value for type parameter %s: " p)))))
                       (if (equal "" read-param)
                           p
                         read-param))))
             type-parameters))))
                 
(defun malabar--parse-type-parameters (type-parameters)
  (let ((str (subst-char-in-string
              ?> ?\)
              (subst-char-in-string
               ?< ?\(
               (subst-char-in-string ?, ?\  type-parameters t)
               t)
              t)))
    (when str
      (mapcar 'symbol-name (car (read-from-string str))))))
              
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
        ;; FIXME:  This doesn't make any sense...
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
               (all-constructors (remove-if-not #'malabar--constructor-p members))
               (accessible-constructors
                (remove-if-not #'malabar-overridable-method-p 
                               all-constructors)))
          (unless (or accessible-constructors
                      (null all-constructors))
            (error "You cannot extends %s, it has no accessible constructors"
                   qualified-class))
          (let ((type-params (malabar--get-type-parameters class-info))
                (class-start (semantic-tag-start class-tag)))
            (goto-char class-start)
            (skip-chars-forward "^{")
            (search-backward "implements" class-start t)
            (insert "extends " qualified-class
                    (if type-params
                        type-params
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
            (malabar--override-methods (malabar--get-abstract-methods class-info) t)
            (malabar--instantiate-type-parameters class-info)
            (malabar-import-and-unqualify qualified-class)))))))

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

(defun subword-capitalize-string (string)
  "Perform `subword-capitalize' on STRING and return the result."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (subword-capitalize 1)
    (buffer-substring-no-properties (point-min) (point-max))))

;; FIXME: This inserts in the wrong place
(defun malabar-insert-getset (all)
  (interactive "P")
  ;; We need the latest and greatest parse
  (malabar-semantic-fetch-tags)
  (if (not all)
      (srecode-insert-getset)
    (let* ((class (srecode-auto-choose-class (point)))
           (members (reverse (semantic-tag-type-members class)))
           (member-names (mapcar #'semantic-tag-name members)))
      (dolist (field members)
        (when (eq 'variable (semantic-tag-class field))
          (let* ((cap-name (subword-capitalize-string (semantic-tag-name field)))
                 (setter-name (concat "set" cap-name))
                 (getter-name (concat "get" cap-name)))
            (unless (or (member setter-name member-names)
                        (member getter-name member-names))
              (srecode-insert-getset class field))))))))

(define-mode-local-override semantic-tag-static-p
  malabar-mode (tag &optional parent)
  "Return non-nil if TAG is static in PARENT class.
Overrides `semantic-tag-static-p'."
  (semantic-tag-get-attribute tag :static-flag))

(define-mode-local-override semantic-format-tag-prototype malabar-mode
  (tag &optional parent color)
  "As -default, but insert the template-specifier in the right place."
  (case (semantic-tag-class tag)
    ((variable)
     (let* ((type (semantic-format-tag-type tag color))
            (const (semantic-tag-get-attribute tag :constant-flag))
            (tm (semantic-tag-get-attribute tag :typemodifiers))
            (mods (append
                   (if const '("const") nil)
                   (cond ((stringp tm) (list tm))
                         ((consp tm) tm)
                         (t nil))))
            (array (let ((deref
                          (semantic-tag-get-attribute
                           tag :dereference))
                         (r ""))
                     (while (and deref (/= deref 0))
                       (setq r (concat r "[]")
                             deref (1- deref)))
                     r)))
       (when mods
         (setq mods (concat (mapconcat 'identity mods " ") " ")))
       (concat (or mods "") type (or array "") " " (semantic-tag-name tag))))
    ((function type)
     (let ((def
            (replace-regexp-in-string
             "," ", "
             (replace-regexp-in-string
              " (" "("
              (semantic-format-tag-prototype-default
               tag parent color)
              nil t)
             nil t)))
       (if (and (malabar--get-type-parameters tag)
                (string-match (regexp-quote (semantic-tag-type tag)) def))
           (replace-match (concat (malabar--get-type-parameters tag) " "
                                  (match-string 0 def))
                          t t def)
         def)))
    (include
     (format "import %s%s;" (if (semantic-tag-static-p tag) "static " "") (semantic-tag-name tag)))))

(provide 'malabar-codegen)

;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
