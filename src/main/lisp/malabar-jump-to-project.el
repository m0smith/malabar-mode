;;; malabar-jump-to-project.el --- Navigation between projects
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

(require 'malabar-groovy)
(require 'malabar-project)
(require 'malabar-util)

(defvar malabar-jump-to-project-history nil "`malabar-jump-to-project' history list.")

(defun malabar-find-file-in-project ()
  "For navigating to a file in another project.
Prompts for a Maven project coordinate
\(groupId:artifactId:version), with completion for all
known projects, then opens an interactive 'Find file'
at the selected project's home directory."
  (interactive)
  (let ((table (malabar-jump-to-project-coordinate-hash-table)))
    (if (> (hash-table-count table) 0)
        (let* ((default (when-let (project-file (malabar-find-project-file))
                          (malabar-project-coordinate project-file)))
               (prompt (if default
                           (concat "Jump to project (default " default "): ")
                         "Jump to project: "))
               (coordinate (completing-read prompt table nil t nil
                                            'malabar-jump-to-project-history default)))
          (if (not (string= coordinate ""))
              (malabar-jump-to-project--show-find-file (gethash coordinate table))
            (signal 'quit nil)))
      (message "Malabar hasn't visited any project files yet!"))))

(defun malabar-jump-to-project-coordinate-hash-table ()
  (malabar-groovy-eval-and-lispeval
   "Utils.printAsLispHashTable(Projects.projectsCoordinateMap)"))

(defun malabar-jump-to-project--show-find-file (project-file)
  (let* ((project-dir (file-name-directory project-file))
         (file (read-file-name "Find file: " project-dir project-dir
                               (confirm-nonexistent-file-or-buffer))))
    (find-file file t)))

(provide 'malabar-jump-to-project)
