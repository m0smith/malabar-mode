;;; malabar-mode.el --- A better Java mode for Emacs

;; Copyright (c) Matthew O. Smith <matt@m0smith.com>
;;
;; Author: 
;;     Espen Wiborg <espenhw@grumblesmurf.org>
;;     Matthew Smith <matt@m0smith.com>
;; URL: http://www.github.com/m0smith/malabar-mode
;; Version: 1.6-M8
;; Package-Requires: ((fringe-helper "1.0.1"))
;; Keywords: java, maven, language, malabar

;;; License:

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Java Major Mode
;;

;;; Code:

(require 'groovy-mode)


(defun malabar-groovy-send-string (str)
  "Send a string to the inferior Groovy process."
  (interactive "r")

  (save-excursion
    (save-restriction
      (let ((proc (groovy-proc)))

      (with-current-buffer (process-buffer proc)
	(while (and
		(goto-char comint-last-input-end)
		(not (re-search-forward comint-prompt-regexp nil t))
		(accept-process-output proc)))
	(goto-char (process-mark proc))
	(insert-before-markers str)
	(move-marker comint-last-input-end (point))
	(comint-send-string proc str)
	(comint-send-string proc "\n")
	)
      )
    )))



(defun malabar-groovy-init-hook ()
  "Called when the inferior groovy is started"
  (interactive)
  (message "Calling hook")
  (malabar-groovy-send-string "def malabar = { classLoader = new groovy.lang.GroovyClassLoader();")
  (malabar-groovy-send-string "Map[] grapez = [[group: 'com.software-ninja' , module:'malabar', version:'2.0.1-SNAPSHOT']]")
  (malabar-groovy-send-string "groovy.grape.Grape.grab(classLoader: classLoader, grapez)")
  (malabar-groovy-send-string "classLoader.loadClass('com.software_ninja.malabar.Malabar').newInstance().startCL(classLoader); }; malabar();"))
;;  (malabar-groovy-send-string "this.getClass().classLoader.rootLoader.addURL(new File(\"C:/Users/Smith/.m2/repository/com/software-ninja/malabar/2.0.0-SNAPSHOT/malabar-2.0.0-SNAPSHOT.jar\").toURL());")
;;  (malabar-groovy-send-string "new com.software_ninja.malabar.Malabar().start();"))

(add-hook 'inferior-groovy-mode-hook 'malabar-groovy-init-hook)
    
;;; Project

(require 'json)

(defvar url-http-end-of-headers)



(defun malabar-project-info (pom &optional repo)
  "Get the project info for a "
  (interactive "fPOM File:")
  (let* ((repo (or repo (expand-file-name "~/.m2/repository")))
	 (url (format "http://localhost:4428/pi/?repo=%s&pom=%s" repo (expand-file-name pom))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun malabar-project-classpath (project-info)
  ""
  (interactive)
  (cdr (assq 'classpath (assq 'test project-info))))

(setq project-info (malabar-project-info "~/projects/malabar-mode-jar/pom.xml"))

