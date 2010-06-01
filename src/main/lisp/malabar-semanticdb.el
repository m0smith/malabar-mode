;;; malabar-semanticdb.el --- A better Java mode for Emacs
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
;; Borrows heavily from semanticdb-java

(if malabar-use-external-cedet
    (require 'semanticdb-search)
  (require 'semantic/db-find))
(require 'eieio)
(require 'eieio-opt)
(require 'eieio-base)

(require 'malabar-reflection)
(require 'malabar-misc)

(defvar-mode-local malabar-mode semanticdb-find-default-throttle 
  '(project system omniscience))

(defclass semanticdb-table-malabar (semanticdb-abstract-table)
  ((major-mode :initform malabar-mode)))

(defclass semanticdb-project-database-malabar
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-malabar
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Java system.")

(defvar-mode-local malabar-mode semanticdb-project-system-databases
  (list
   (semanticdb-project-database-malabar "Malabar Java")))

(defvar malabar--java-typecache (make-hash-table :test 'equal)
  "The Java type cache.  Clear it with `malabar-clear-typecache'
if it gives you trouble.")

(defun malabar-clear-typecache ()
  "Clear all cached type lookup information."
  (interactive)
  (clrhash malabar--java-typecache))

(define-mode-local-override semanticdb-typecache-find malabar-mode
  (type &optional path find-file-match)
  "Mandatory docstring."
  (let ((default-answer (semanticdb-typecache-find-default type path find-file-match)))
    (or default-answer
        (and (stringp type)
             (or (gethash type malabar--java-typecache)
                 (puthash type 
                          (malabar--get-type-tag (malabar-qualify-class-name-in-buffer type))
                          malabar--java-typecache))))))

(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-malabar))
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-malabar "malabar")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-malabar) filename)
  (car (semanticdb-get-database-tables obj)))

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-malabar) &optional buffer)
  (save-excursion
    (set-buffer buffer)
    (eq (or mode-local-active-mode major-mode) 'malabar-mode)))

(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-malabar) name &optional tags)
  (call-next-method))

(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-malabar) name &optional tags)
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-malabar) regex &optional tags)
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-malabar) prefix &optional tags) 
  (semanticdb-find-tags-for-completion-method table prefix tags))

(define-mode-local-override semanticdb-find-translate-path malabar-mode (path brutish)
  (message "semanticdb-find-translate-path java-mode %s " path)
  (semanticdb-find-translate-path-default path t))

(defun malabar-semanticdb-root (dir)
  (when-let (project-dir (locate-dominating-file dir "pom.xml"))
    (expand-file-name project-dir)))
(pushnew 'malabar-semanticdb-root semanticdb-project-root-functions)

(provide 'malabar-semanticdb)
