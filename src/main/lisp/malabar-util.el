;;; malabar-util.el --- A better Java mode for Emacs
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
(defun string-starts-with (string start)
  (string= (substring string 0 (length start)) start))

(defun string-ends-with (string end)
  (string= (substring string (- (length string) (length end))) end))

(defun string-trim (string)
  (when (string-match "\\`[\r\n\t ]+" string)
    (setq string (replace-match "" t t string)))
  (when (string-match "[\r\n\t ]+\\'" string)
    (setq string (replace-match "" t t string)))
  string)

(defun string-delete-whitespace (string) 
  (replace-regexp-in-string "[\r\n\t ]+" "" string t t))
    
(defun string-with-newline (string)
  (if (string-ends-with string "\n")
      string
    (concat string "\n")))

(defvar malabar--caches nil)

(def-edebug-spec with-caches t)
(defmacro with-caches (&rest forms)
  "Executes FORMS with all defined caches bound to new
hash-tables with `equal' as test."
  `(let ,(mapcar (lambda (cache-name)
                   (list cache-name '(make-hash-table :test 'equal)))
                 malabar--caches)
     ,@forms))

(def-edebug-spec define-cached-function defun)
(defmacro define-cached-function (name lambda-list &optional doc &rest body)
  "Defines NAME as a function which, when invoked within the
scope of `with-caches', memoizes its return in a unique cache
keyed by the function's first parameter."
  (declare (indent defun)
           (doc-string 3))
  (let ((gensym (gensym))
        (cache-name (gensym))
        (key (first lambda-list)))
    `(progn
       (add-to-list 'malabar--caches ',cache-name)
       (defun ,name ,lambda-list
         ,@(when (stringp doc)
             (list doc))
         (or (and (boundp ',cache-name)
                  ,cache-name
                  (gethash ,key ,cache-name))
             (let ((,gensym (progn ,@(if (stringp doc)
                                         body
                                       (cons doc body)))))
               (when (and (boundp ',cache-name)
                          ,cache-name)
                 (puthash ,key ,gensym ,cache-name))
               ,gensym))))))

(provide 'malabar-util)
