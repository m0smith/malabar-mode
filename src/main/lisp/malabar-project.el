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

(defun malabar-project-locate-source-file (filename project-file)
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

(defun malabar-project-expression (project-file)
  (format "Projects['%s']" project-file))

(defun malabar-project-classpath (buffer)
  (concat (malabar-project buffer) "." (malabar-classpath-of-buffer buffer)))

(defun malabar-classpath-of-buffer (&optional buffer)
  (let ((file (file-name-nondirectory (buffer-file-name buffer))))
    (if (malabar-project-locate-in-source-path file (malabar-find-project-file buffer))
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
   (format "Utils.printAsLispList(%s.testSrcDirectories)"
           (malabar-project-expression project-file))))

(defun malabar-project-source-directories (project-file)
  (malabar-groovy-eval-and-lispeval
   (format "Utils.printAsLispList(%s.srcDirectories)"
           (malabar-project-expression project-file))))

(provide 'malabar-project)
