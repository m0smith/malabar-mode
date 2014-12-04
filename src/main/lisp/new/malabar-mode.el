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
(require 'semantic/db-javap)


;; 
;; init
;;

(setq ede-maven2-execute-mvn-to-get-classpath nil)

(semantic-mode 1)
(global-ede-mode 1)

;;
;; Groovy
;;

(defun malabar-run-groovy ()
  (interactive)
  (run-groovy "C:/Users/lpmsmith/.gvm/groovy/2.3.7/bin/groovysh -Dhttp.proxyHost=proxy.ihc.com -Dhttp.proxyPort=8080 -Dgroovy.grape.report.downloads=true -Djava.net.useSystemProxies=true"))


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
  (malabar-groovy-send-string "Map[] grapez = [[group: 'com.software-ninja' , module:'malabar', version:'2.0.3']]")
  (malabar-groovy-send-string "groovy.grape.Grape.grab(classLoader: classLoader, grapez)")
  (malabar-groovy-send-string "classLoader.loadClass('com.software_ninja.malabar.MalabarStart').newInstance().startCL(classLoader); }; malabar();"))

(add-hook 'inferior-groovy-mode-hook 'malabar-groovy-init-hook)


;;
;; EDE
;;

(defun malabar-maven2-load (dir &optional rootproj)
  "Return a Maven Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-files-find-existing dir ede-maven2-project-list)
      ;; Doesn't already exist, so lets make one.
       (let ((this
             (ede-maven2-project "Malabar Maven"
                                 :name "Malabar maven dir" ; TODO: make fancy name from dir here.
                                 :directory dir
                                 :file (expand-file-name "pom.xml" dir)
				 :current-target "package"
				 :classpath (mapcar 'identity (malabar-project-classpath (malabar-project-info (expand-file-name "pom.xml" dir))))
                                 )))
         (ede-add-project-to-global-list this)
         ;; TODO: the above seems to be done somewhere else, maybe ede-load-project-file
         ;; this seems to lead to multiple copies of project objects in ede-projects
	 ;; TODO: call rescan project to setup all data
	 (message "%s" this)
	 this)))


(ede-add-project-autoload
 (ede-project-autoload "malabar-maven2"
		       :name "MALABAR MAVEN2"
		       :file 'ede/maven2
		       :proj-file "pom.xml"
		       :proj-root 'ede-maven2-project-root
		       :load-type 'malabar-maven2-load
		       :class-sym 'ede-maven2-project
		       :new-p nil
		       :safe-p t
		       )
 'unique)

    
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

;;(setq project-info (malabar-project-info "~/projects/malabar-mode-jar/pom.xml"))

