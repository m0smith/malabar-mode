;;; malabar-abbrevs.el --- A better Java mode for Emacs
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

(defvar malabar-case-fixed-abbrevs
  '(("pu" . "public")
    ("pri" . "private")
    ("pro" . "protected")
    ("st" . "static")
    ("vo" . "void")
    ("ab" . "abstract")
    ("bo" . "boolean")
    ("cl" . "class")
    ("impl" . "implements")
    ("ext" . "extends")
    ("pa" . "package")
    ("re" . "return")))

(defun malabar-abbrevs-setup ()
  (mapc (lambda (abbr)
          (define-abbrev local-abbrev-table (car abbr) (cdr abbr) nil
            :case-fixed t :system 'force))
        malabar-case-fixed-abbrevs))

(provide 'malabar-abbrevs)
