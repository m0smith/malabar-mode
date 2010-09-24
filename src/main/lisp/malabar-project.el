;;; malabar-project.el --- Project handling for malabar-mode
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
(require 'malabar-variables)
(require 'malabar-groovy)
(require 'malabar-util)

(defun malabar-setup-compilation-buffer (&optional for-files)
  (malabar-setup-compilation-buffer-1 for-files (malabar-find-project-file)))

(defun malabar-setup-compilation-buffer-1 (for-files project-file)
  (setq malabar-compilation-project-file project-file)
  (malabar-groovy-setup-compilation-buffer for-files))

(defun malabar--clean-compilation-messages (buffer &optional message)
  (when (equal buffer (get-buffer malabar-groovy-compilation-buffer-name))
    (with-current-buffer buffer
      (remove-hook 'after-change-functions 'font-lock-after-change-function t)
      (font-lock-fontify-buffer)
      (save-excursion
        (goto-char (point-min))
        (let (locus)
          (ignore-errors
            (while t
              ;; Grubbing in compile's internals, here
              (setq locus (compilation-next-error 1 nil (point)))
              (setq file (car (car (nth 2 (car locus)))))
              (unless (file-readable-p file)
                (let ((end (or (text-property-not-all (point) (point-max) 'message locus)
                               (point-max)))
                      (file (malabar-project-locate-source-file
                             file malabar-compilation-project-file)))
                  (if (null file)
                      (set-text-properties (point) end nil)
                    (rplaca (car (nth 2 (car locus))) file)
                    (put-text-property (point) end 'message locus)))))))))))

(add-hook 'compilation-finish-functions 'malabar--clean-compilation-messages)

(defun malabar-project-locate (file project-file)
  (locate-file file 
               (append (malabar-project-source-directories project-file)
                       (malabar-project-test-source-directories project-file))))

(defun malabar-project-locate-source-file (filename project-file)
  "Locate filename (a basename, i.e. without path) in the project sources."
  (or (malabar-project-locate-in-source-path filename project-file)
      (malabar-project-locate-in-test-source-path filename project-file)))

(defun malabar-project-locate-in-source-path (filename project-file)
  (malabar--locate-in-path filename
                           (malabar-project-source-directories project-file)))

(defun malabar-project-locate-in-test-source-path (filename project-file)
  (malabar--locate-in-path filename
                           (malabar-project-test-source-directories project-file)))

(defun malabar--locate-in-path (filename path)
  (catch 'found
    (dolist (dir path)
      (malabar--find-file filename dir))))

(defun malabar-project (buffer)
  (malabar-project-expression (malabar-find-project-file buffer)))

(defvar malabar-project-active-profiles nil
  "Alist of (project-file . active-profiles).")

(defun malabar-project-expression (project-file)
  (format "Projects.get('%s', %s)"
          project-file
          (malabar--make-groovy-list
           (cdr (assoc project-file malabar-project-active-profiles)))))

(defun malabar-project-coordinate (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLisp(%s.coordinate)"
           (malabar-project-expression project-file))))

(defun malabar-project-classpath (buffer)
  (concat (malabar-project buffer) "." (malabar-classpath-of-buffer buffer)))

(defun malabar-classpath-of-buffer (&optional buffer)
  (let ((file (file-name-nondirectory (buffer-file-name buffer))))
    (if (malabar-project-locate-in-source-path file (malabar-find-project-file buffer))
        "compileClasspath"
      "testClasspath")))

(defun malabar-find-project-file (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (or (when-let (file (buffer-file-name buffer))
          (malabar--project-for-file file))
        (error "No POM found for buffer %s" buffer))))

(defun malabar--project-for-file (file)
  (when-let (dir (locate-dominating-file file "pom.xml"))
    (malabar--project-file dir)))

(defun malabar--project-file (dir)
  (let ((file (expand-file-name "pom.xml" dir)))
    (and (file-readable-p file)
         file)))

(defun malabar--sibling-projects (project-file)
  (let ((parent (file-name-directory
                 (directory-file-name (file-name-directory project-file)))))
    (when (malabar--project-file parent)
      (remove nil
              (mapcar 'malabar--project-file
                      (remove-if-not 'file-accessible-directory-p
                                     (directory-files parent 'full "^[^\\.]")))))))

(defun malabar-visit-project-file ()
  "Visits the project file."
  (interactive)
  (find-file-other-window (malabar-find-project-file)))

(defun malabar-build-project (clean-p &rest goals)
  (when clean-p
    (setq goals (cons 'clean goals)))
  (malabar-execute-maven (malabar-find-project-file)
                         goals
                         nil
                         nil))

(defun malabar-execute-maven (project-file goals definitions profiles)
  (malabar-setup-compilation-buffer)
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (format "%s.run(%s, %s, %s)"
           (malabar-project-expression project-file)
           (malabar--make-groovy-list goals)
           (malabar--make-groovy-list profiles)
           (malabar--make-groovy-map definitions))))

(defun malabar-install-project (clean-p)
  "Runs 'mvn install' on the current project.  With prefix
argument, cleans the project first ('mvn clean install')."
  (interactive "P")
  (malabar-build-project clean-p 'install))

(defvar malabar-maven-command-line-history nil
  "Minibuffer history for `malabar-run-maven-command`.")

(defun malabar-run-maven-command (command-line)
  "Prompts for and executes an (almost) arbitrary Maven command line.
Honors profile activation, property definitions and lifecycle
phases/goals.  E.g.: ``-DskipTests=true -Pdev-mode install`` will
run the install lifecycle with the dev-mode profile active,
skipping tests."
  (interactive (list (read-from-minibuffer "mvn command line: " nil nil nil
                                           'malabar-maven-command-line-history)))
  (let ((parsed-command (malabar-parse-maven-command-line command-line)))
    (apply #'malabar-execute-maven (malabar-find-project-file)
           parsed-command)))

(defun malabar-project-test-source-directories (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLispList(%s.testSrcDirectories)"
           (malabar-project-expression project-file))))

(defun malabar-project-source-directories (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLispList(%s.srcDirectories)"
           (malabar-project-expression project-file))))

(provide 'malabar-project)
