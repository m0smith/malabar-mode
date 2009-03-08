;;; malabar-variables.el --- Variables for malabar-mode
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
(defvar malabar-mode-key-prefix [?\C-c ?\C-v]
  "The prefix key for malabar-mode commands.")

(defvar malabar-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map [?\C-b] 'malabar-install-project)
      (define-key prefix-map [?\C-c] 'malabar-compile-file)
      (define-key prefix-map [?t] 'malabar-run-test)
      (define-key prefix-map [?\C-t] 'malabar-run-junit-test)
      (define-key prefix-map [?\M-t] 'malabar-run-all-tests)
      (define-key prefix-map [?\C-z] 'malabar-import-one-class)
      (define-key prefix-map [?\C-o] 'malabar-override-method)
      (define-key prefix-map [?\C-e] 'malabar-extend-class)
      (define-key prefix-map [?\C-i] 'malabar-implement-interface)
      (define-key prefix-map [?.] 'malabar-complete)
      (define-key prefix-map [?\C-.] 'malabar-complete)
      (define-key map malabar-mode-key-prefix prefix-map))
    map)
  "Keymap for Malabar mode.")

(defvar malabar-compilation-project-file nil)
(defvar malabar-compilation-project-test-source-directories nil)

(defvar malabar-case-fixed-abbrevs
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
    ("#Test" hook malabar-abbrevs-create-test))
  "The list of abbrevs which should be recognized only in the
specified case.")

(provide 'malabar-variables)
