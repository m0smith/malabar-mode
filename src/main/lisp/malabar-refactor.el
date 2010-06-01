;;; malabar-refactor.el --- Refactoring for Malabar
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
(require 'thingatpt)

(defcustom malabar-refactor-query-p nil
  "If non-NIL, the `malabar-refactor-' commands use
`query-replace' to do the replacement."
  :type 'boolean
  :group 'malabar-mode)

(defvar malabar-refactor--java-scopes '((public "public" "public ")
                                       (protected "protected" "protected ")
                                       (package "package private" "")
                                       (private "private" "private "))
  "Java scopes with descriptive strings.")

(defcustom malabar-refactor-extract-constant-default-scope 'public
  "The default scope of the constant
`malabar-refactor-extract-constant' extracts."
  :type `(choice ,@(mapcar #'(lambda (c)
                               `(const :tag ,(nth 1 c) ,(car c)))
                           malabar-refactor--java-scopes))
  :group 'malabar-mode)

(defun malabar-refactor--read-scope (prompt &optional default-value)
  (intern
   (completing-read (concat prompt " (default " default-value "): ")
                    (mapcar #'(lambda (c) (symbol-name (car c)))
                            malabar-refactor--java-scopes)
                    nil t nil nil default-value)))

(defun malabar-refactor--replace-function (regexp-p)
  (cond ((and malabar-refactor-query-p regexp-p)
         'query-replace-regexp)
        (malabar-refactor-query-p
         'query-replace)
        (regexp-p
         'replace-regexp)
        (t
         'replace-string)))

(defun malabar-refactor--constant-expression-at-point ()
  (save-excursion
    (when (in-string-p)
      (skip-syntax-forward "^\""))
    (let* ((thing-type (if (or (in-string-p)
                               (eq (char-syntax (char-before)) ?\")
                               (eq (char-syntax (char-after)) ?\"))
                           'sexp
                         'symbol))
           (thing (thing-at-point thing-type))
           (type (malabar-refactor--type-of thing))
           (thing-start (car (bounds-of-thing-at-point thing-type)))
           (real-thing (if (char-equal (char-before thing-start) ?-)
                           (concat "-" thing)
                         thing)))
      (if (malabar-refactor--constant-type-p type)
          (cons real-thing type)
        (error "Not a constant expression: %s" thing)))))

(defmacro regexp-case (expr &rest clauses)
  "Evaluate EXPR (which must evaluate to a string) and choose
among clauses on that value. Each clause looks like (REGEXP
BODY-FORM).  EXPR is evaluated and attempted matched against each
REGEXP in turn, using `string-match'; the BODY-FORM corresponding
to the first match is evaluated.  If no clause succeeds,
`regexp-case' returns NIL.  A REGEXP of t is allowed in the final
clause, and matches if no other clauses match.
\n(fn EXPR (REGEXP BODY-FORM)...)"
  (let ((var (make-symbol "-regexp-case-")))
    `(let ((,var ,expr))
       (cond ,@(mapcar (lambda (c)
                         (if (eq (car c) t)
                             `(t ,(nth 1 c))
                           `((string-match ,(car c) ,var) ,(nth 1 c))))
                       clauses)))))

(put 'regexp-case 'lisp-indent-function 1)

(defun malabar-refactor--type-of (thing)
  (regexp-case thing
    ("^[0-9]+$" 'int)
    ("^[0-9]+[lL]$" 'long)
    ("^[0-9]*.[0-9]+$" 'double)
    ("^[0-9]*.?[0-9]*[fF]$" 'float)
    ("^'.*'$" 'char)
    ("^\".*\"$" 'String)
    (t 'Object)))

(defvar malabar-refactor--constant-types '(int long double float char String))

(defun malabar-refactor--constant-type-p (type)
  (memq type malabar-refactor--constant-types))

(defun malabar-refactor-extract-constant (name &optional scope value-spec)
  "Extracts the 'thing' at point as a constant named NAME.

Interactively, will prompt for the name and, with a prefix
argument the scope.  Scope will default to
`malabar-refactor-extract-constant-default-scope'.

Known constant types are int, long, double, float, char and
String."
  (interactive
   (let ((valspec (malabar-refactor--constant-expression-at-point)))
     (progn (barf-if-buffer-read-only)
            (list (read-string (format "Make %s a constant named: "
                                       (car valspec)))
                  (and current-prefix-arg
                       (malabar-refactor--read-scope
                        "Constant scope"
                        (nth 1 (assoc malabar-refactor-extract-constant-default-scope
                                      malabar-refactor--java-scopes))))
                  valspec))))
  (unless scope
    (setq scope malabar-refactor-extract-constant-default-scope))
  (unless value-spec
    (setq value-spec (malabar-refactor--constant-expression-at-point)))
  (let ((constant-spec (format "%sstatic final %s %s = %s;\n"
                               (nth 2 (assoc scope malabar-refactor--java-scopes))
                               (cdr value-spec)
                               name
                               (car value-spec))))
    (save-excursion
      (undo-boundary)
      (goto-char (point-min))
      (funcall (malabar-refactor--replace-function t)
               (let ((expr-delimiter "\\([\s(),=+;]\\)"))
                 (concat expr-delimiter (regexp-quote (car value-spec)) expr-delimiter))
               (concat "\\1" name "\\2"))
      (let ((first-member-tag
             (car (semantic-tag-type-members
                   (car (semantic-find-nonterminal-by-token
                         'type (malabar-semantic-fetch-tags)))))))
        (goto-char (semantic-tag-start first-member-tag))
        (forward-line 0)
        (insert constant-spec)
        (forward-line -1)
        (indent-according-to-mode))
      (undo-boundary))))

(provide 'malabar-refactor)
