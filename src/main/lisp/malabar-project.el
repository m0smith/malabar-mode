;;; malabar-project.el --- Project handling for malabar-mode
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
(require 'malabar-variables)
(require 'malabar-groovy)
(require 'malabar-util)

(defun malabar-setup-compilation-buffer ()
  (setq malabar-compilation-project-file (malabar-find-project-file))
  (malabar-groovy-setup-compilation-buffer))

(defun malabar-project (buffer)
  (format "Project.makeProject('%s')" (malabar-find-project-file buffer)))

(defun malabar-project-classpath (buffer)
  (concat (malabar-project buffer) "." (malabar-classpath-of-buffer buffer)))

(defun malabar-classpath-of-buffer (&optional buffer)
  (let ((file (file-name-nondirectory (buffer-file-name buffer))))
    (if (catch 'found
          (dolist (dir (malabar-project-test-source-directories
                        (malabar-find-project-file buffer)))
            (malabar--find-file file dir)))
        "testClasspath"
      "compileClasspath")))

(defun malabar-find-project-file (&optional buffer)
  (let ((dir (locate-dominating-file (buffer-file-name (or buffer (current-buffer)))
                                     "pom.xml")))
    (when dir
      (expand-file-name "pom.xml" dir))))

(defun malabar-visit-project-file ()
  "Visits the project file."
  (interactive)
  (find-file-other-window (malabar-find-project-file)))

(defun malabar-build-project (goals)
  (malabar-setup-compilation-buffer)
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (concat (format "MvnServer.INSTANCE.run('%s', "
                   (malabar-find-project-file))
           (mapconcat (lambda (s) (format "'%s'" s))
                      (if (atom goals)
                          (list goals)
                        goals)
                      ",")
           ")")))

(defun malabar-install-project ()
  "Runs 'mvn install' on the current project."
  (interactive)
  (malabar-build-project 'install))

(defun malabar-project-test-source-directories (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLispList(Project.makeProject('%s').testSrcDirectories)"
           project-file)))

(defun malabar-project-source-directories (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLispList(Project.makeProject('%s').srcDirectories)"
           project-file)))

(provide 'malabar-project)
