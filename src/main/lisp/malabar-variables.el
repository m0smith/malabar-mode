;;; malabar-variables.el --- Variables for malabar-mode
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
(cond (malabar-use-external-cedet
       (require 'srecode-map)
       (require 'semantic-ia))
      (t
       (require 'srecode/map)
       (require 'semantic/ia)))

(defgroup malabar-mode nil
  "A better Java mode")

(defcustom malabar-mode-key-prefix [?\C-c ?\C-v]
  "The prefix key for malabar-mode commands."
  :group 'malabar-mode
  :type 'sexp)

(defvar malabar-refactor-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c] 'malabar-refactor-extract-constant)
    map)
  "Keymap for Malabar's refactorings.")

(defvar malabar-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map [?\C-b] 'malabar-install-project)
      (define-key prefix-map [?\C-c] 'malabar-compile-file)
      (define-key prefix-map [?\C-g] 'malabar-insert-getset)
      (define-key prefix-map [?t]    'malabar-run-test)
      (define-key prefix-map [?\C-t] 'malabar-run-junit-test)
      (define-key prefix-map [?\M-t] 'malabar-run-all-tests)
      (define-key prefix-map [?\C-z] 'malabar-import-one-class)
      (define-key prefix-map [?z] 'malabar-import-all)
      (define-key prefix-map [?\C-o] 'malabar-override-method)
      (define-key prefix-map [?\C-e] 'malabar-extend-class)
      (define-key prefix-map [?\C-i] 'malabar-implement-interface)
      (define-key prefix-map [?.]    (if malabar-use-external-cedet
                                         'semantic-ia-complete-symbol-menu
                                       'semantic-ia-complete-symbol))
      (define-key prefix-map [?\C-.] 'semantic-ia-complete-symbol)
      (define-key prefix-map [?\C-p] 'malabar-visit-project-file) 
      (define-key prefix-map [?\C-y] 'malabar-jump-to-thing)
      (define-key prefix-map [?\C-r] malabar-refactor-map)
      (define-key map malabar-mode-key-prefix prefix-map))
    (define-key map ":" 'malabar-electric-colon)
    map)
  "Keymap for Malabar mode.")

(defvar malabar-compilation-project-file nil)
(defvar malabar-compilation-project-test-source-directories nil)

(defcustom malabar-case-fixed-abbrevs
  '(("pu" "public")
    ("pri" "private")
    ("pro" "protected")
    ("st" "static")
    ("vo" "void")
    ("ab" "abstract")
    ("bo" "boolean")
    ("cl" "class")
    ("impl" "implements")
    ("ext" "extends")
    ("pa" "package")
    ("re" "return")
    ("#Test" malabar-abbrevs-create-test))
  "The list of abbrevs which should be recognized only in the
specified case."
  :group 'malabar-mode
  :type '(alist :key-type string :value-type (group (choice string
                                                            function))))

(defcustom malabar-srecode-template-directory
  (file-name-as-directory (expand-file-name "srecode"
                                            (file-name-directory load-file-name)))
  "The directory where malabar-mode's srecode templates live."
  :group 'malabar-mode
  :type 'directory
  :set (lambda (symbol value)
         (when (boundp symbol)
           (setq srecode-map-load-path (remove (symbol-value symbol) srecode-map-load-path)))
         (set-default symbol value)
         (add-to-list 'srecode-map-load-path value)
         (srecode-map-update-map t)))

(defcustom malabar-hide-non-local-source-buffers t
  "Whether to hide source buffers loaded from outside the current
project from the buffer list (by prefixing the buffer name with a
space).

A value of t means always hide.
A value of nil means never hide."
  :group 'malabar-mode
  :type '(choice (const :tag "Hide" t)
                 (const :tag "Don't hide" nil)))

(defcustom malabar-load-source-from-sibling-projects t
  "Whether to load source from sibling projects.
Note that this will not work reliably with a 'flat' project
layout."
  :group 'malabar-mode
  :type '(choice (const :tag "Load from siblings" t)
                 (const :tag "Don't load from siblings" nil)))

(defcustom malabar-extra-source-locations nil
  "List of extra source locations.
Each location may be a directory or a JAR/ZIP file.  Malabar-mode
will look for the source code of a Java class in these locations
if the class is not otherwise resolvable."
  :group 'malabar-mode
  :type '(repeat (file :tag "Path")))

(defcustom malabar-electric-elvis-p t
  "Whether inserting a colon should electrically expand the Elvis
operator ('?:').

See `malabar-electric-colon'."
  :group 'malabar-mode
  :type '(boolean))

(provide 'malabar-variables)
