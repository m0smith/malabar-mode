;;; malabar-util.el --- A better Java mode for Emacs
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
(require 'semantic)

(defmacro* when-let ((var value) &body body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))
(put 'when-let 'lisp-indent-function 1)

(defun string-starts-with (string start)
  (string-match-p (concat "^" (regexp-quote start)) string))

(defun string-ends-with (string end)
  (string-match-p (concat (regexp-quote end) "$") string))

(defun string-trim (string)
  (save-match-data 
    (when (string-match "\\`[\r\n\t ]+" string)
      (setq string (replace-match "" t t string)))
    (when (string-match "[\r\n\t ]+\\'" string)
      (setq string (replace-match "" t t string)))
    string))

(defun string-delete-whitespace (string) 
  (save-match-data (replace-regexp-in-string "[\r\n\t ]+" "" string t t)))
    
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

(defun malabar-choose (prompt choices &optional default)
  "Prompts (with completion) for an element of CHOICES,
defaulting to DEFAULT.  CHOICES may be either a list of strings
or a alist; if an alist, will prompt for a car of CHOICES and
return the corresponding cdr."
  (let ((res (completing-read prompt (if (consp (car choices))
                                         (mapcar #'car choices)
                                       choices) nil t default)))
    (unless (equal "" res)
      (if (consp (car choices))
          (cdr (assoc res choices))
        res))))

(defun malabar--type-tags-in-buffer (&optional buffer)
  (semantic-find-tags-by-class 'type
                               (semantic-flatten-tags-table (or buffer
                                                                (current-buffer)))))
(defun malabar-class-defined-in-buffer-p (classname &optional buffer)
  (let ((tags (malabar--type-tags-in-buffer buffer)))
    (find classname tags
          :key #'semantic-tag-name
          :test #'equal)))

(defun malabar-get-package-of (classname)
  (let ((lastdot (position ?. classname :from-end t)))
    (if lastdot
        (substring classname 0 lastdot)
      "")))

(defun malabar-get-classname (classname)
  (let ((lastdot (position ?. classname :from-end t)))
    (if lastdot
        (substring classname (1+ lastdot))
      classname)))

(defun malabar-class-name-to-filename (class-name &optional suffix)
  (concat (replace-regexp-in-string "\\." "/" class-name)
          (or suffix ".java")))

(defun malabar--find-file (file directory)
  (when (file-accessible-directory-p directory)
    (dolist (dir-file (directory-files directory 'full "^[^\\.]"))
      (if (file-accessible-directory-p dir-file)
          (malabar--find-file file dir-file)
        (and (file-readable-p dir-file)
             (string= file (file-name-nondirectory dir-file))
             (throw 'found dir-file))))))

(defun malabar-get-package-tag (&optional buffer)
  (car (semantic-find-tags-by-class 'package (or buffer
                                                 (current-buffer)))))

(defun malabar-get-package-name (&optional buffer)
  (when-let (package-tag (malabar-get-package-tag buffer))
    (semantic-tag-name package-tag)))

(defun malabar-get-class-tag-at-point ()
  (malabar-semantic-fetch-tags)
  (or (semantic-current-tag-of-class 'type)
      (car (semantic-find-tags-by-class 'type (current-buffer)))))

(defun malabar--conditional-replace (regexp replacement start end predicate)
  "Replaces REGEXP with REPLACEMENT from START to END, but only
when PREDICATE return non-NIL."
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp end t)
      (when (funcall predicate)
        (replace-match replacement nil t)))))

(defun malabar-parse-maven-command-line (command-line)
  "Returns a three-element list (goals definitions profiles)
gleaned from COMMAND-LINE."
  (let ((goals nil)
        (definitions nil)
        (profiles nil)
        (tokens (split-string command-line)))
    (dolist (token tokens)
      (cond ((string-starts-with token "-D")
             ;; define
             (let ((definition-tokens (split-string (substring token 2) "=")))
               (push (cons (first definition-tokens)
                           (second definition-tokens))
                     definitions)))
            ((string-starts-with token "-P")
             ;; profiles
             (setq profiles (nconc (split-string (substring token 2) ",")
                                   profiles)))
            ((string-starts-with token "-")
             ;; other option, ignore for now
             )
            (t
             ;; goal
             (push token goals))))
    (list (nreverse goals)
          (nreverse definitions)
          profiles)))
    
(defun malabar--make-groovy-list (l)
  (concat "["
          (mapconcat (lambda (s) (format "'%s'" s))
                     l
                     ",")
          "]"))

(defun malabar--make-groovy-map (l)
  (if (null l)
      "[:]"
    (concat "["
            (mapconcat (lambda (c) (format "'%s': '%s'" (car c) (cdr c)))
                       l
                       ",")
            "]")))

(provide 'malabar-util)
;; Local Variables:
;; byte-compile-warnings:(not cl-functions)
;; End:
