;;; malabar-abbrevs.el --- A better Java mode for Emacs
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

(require 'skeleton)
(require 'cl)
(require 'malabar-variables)

(defun malabar-abbrevs-delete-abbrev ()
  "Delete the abbrev (prior to expanding)."
  (when last-abbrev-text
    (backward-delete-char-untabify (length last-abbrev-text))))

(defmacro define-malabar-abbrev-skeleton (name docstring interactor &rest skeleton)
  "Define name as a skeleton which, as its first action, executes
`malabar-abbrevs-delete-abbrev'."
  `(define-skeleton ,name
     ,docstring
     ,interactor '(malabar-abbrevs-delete-abbrev)
     ,@skeleton))

(define-malabar-abbrev-skeleton malabar-abbrevs-create-test
  "Create a test method"
  nil
  > "@Test" \n
  "public void " _ "() throws Exception {" \n
  "fail(\"Unfinished test\");" \n
  "}" > \n)

(defcustom malabar-abbrevs-abbrev-regexp
  "\\(?:^\\|\\s-\\)\\(#?\\w+\\)\\W*"
  "The regexp to recognize abbrevs.  Group one is used for abbrev
lookup."
  :group 'malabar-mode
  :type 'regexp)
  
(defun malabar-abbrevs-setup ()
  (abbrev-table-put malabar-mode-abbrev-table :regexp malabar-abbrevs-abbrev-regexp)
  (mapc (lambda (abbr)
          (define-abbrev malabar-mode-abbrev-table (first abbr) (second abbr)
            (unless (stringp (second abbr))
              (second abbr))
            :case-fixed t :system 'force))
        malabar-case-fixed-abbrevs))

(provide 'malabar-abbrevs)
