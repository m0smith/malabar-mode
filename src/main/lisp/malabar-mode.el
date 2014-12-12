;; -*- lexical-binding: t -*-
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
(require 'dash)

;;;
;;; Variables

(defcustom malabar-keymap-prefix (kbd "C-c C-v")
  "Prefix for key bindings of Malabar.

Changing this variable outside Customize does not have any
effect.  To change the keymap prefix from Lisp, you need to
explicitly re-define the prefix key:

    (define-key malabar-mode-map malabar-keymap-prefix nil)
    (setq malabar-keymap-prefix (kbd \"C-c f\"))
    (define-key malabar-mode-map malabar-keymap-prefix
                malabar-command-map)

Please note that Malabar's manual documents the default
keybindings.  Changing this variable is at your own risk."
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'string
  :risky t
  :set
  (lambda (variable key)
    (when (and (boundp variable) (boundp 'malabar-mode-map))
      (define-key malabar-mode-map (symbol-value variable) nil)
      (define-key malabar-mode-map key malabar-command-map))
    (set-default variable key)))

(defcustom malabar-server-host "localhost"
  "The host where the server is running"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'string)

(defcustom malabar-server-port "4428"
  "The port where the server is running"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'string)

(defcustom malabar-package-maven-repo "~/.m2/repository"
  "Where to find the maven repo"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'string)

(defcustom malabar-package-additional-classpath '( "build/classes/main" "build/classes/test" )
  "JARS and DIRS relative to the package root to add to the
classpath.  These are added to every project.  May need to
restart the *groovy* process to see changes to effect"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type '(repeat (string :tag "Jar/Zip/Dir")))


(defcustom malabar-install-directory
  (file-name-as-directory (file-name-directory load-file-name))
  "The directory where malabar-mode was installed"
  :group 'malabar
  :type 'directory)

;;; 
;;; init
;;;

(setq ede-maven2-execute-mvn-to-get-classpath nil)

(semantic-mode 1)
(global-ede-mode 1)

;;;
;;; Groovy
;;;


(defun malabar-run-groovy ()
  (interactive)
  (run-groovy (format "%s %s" (expand-file-name "~/.gvm/groovy/2.3.7/bin/groovysh")
		      " -Dhttp.proxyHost=proxy.ihc.com -Dhttp.proxyPort=8080 -Dgroovy.grape.report.downloads=true -Djava.net.useSystemProxies=true")))


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
  (message "Starting malabar server")
  (malabar-groovy-send-string "
     malabar = { classLoader = new groovy.lang.GroovyClassLoader(); 
         Map[] grapez = [[group: 'com.software-ninja' , module:'malabar', version:'2.0.4-SNAPSHOT']]; 
         groovy.grape.Grape.grab(classLoader: classLoader, grapez);
         classLoader.loadClass('com.software_ninja.malabar.MalabarStart').newInstance().startCL(classLoader); }; 
     malabar();"))

(add-hook 'inferior-groovy-mode-hook 'malabar-groovy-init-hook)

(defun malabar-groovy-send-classpath-element  (element)
  "Send a JAR, ZIP or DIR to the classpath of the running *groovy*"
  (interactive "fJAR, ZIP or DIR:")
  (malabar-groovy-send-string 
   (format "this.getClass().classLoader.rootLoader.addURL(new File('%s').toURL())" 
	   (expand-file-name element))))

(defun malabar-groovy-send-classpath  (pom &optional repo)
  "Add the classpath for POM to the runnning *groovy*."
  (interactive "fPOM File:")
  (mapcar 'malabar-groovy-send-classpath-element (malabar-project-classpath 
		     (malabar-project-info pom repo))))

(defun malabar-groovy-send-classpath-of-buffer  ( &optional buffer repo)
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (let ((pom malabar-mode-project-file))
	(malabar-groovy-send-classpath pom repo)))))

(defun malabar-groovy-send-buffer (&optional buffer)
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (groovy-send-region-and-go (point-min) (point-max)))))
  
;;;
;;; flycheck
;;;

(require 'flycheck)

(defun malabar-flycheck-command ( checker cback )
  "Use flycheck to search the current buffer for compiler errrors."
  (if (not (comint-check-proc "*groovy*"))
      (funcall cback 'finished nil)
    (let* ((pom-path malabar-mode-project-file)
	   (buffer (current-buffer))
	   (script (if (buffer-modified-p) (buffer-file-name) (buffer-file-name))))
      
      (malabar-parse-script-raw
       (lambda (status)
	 ;(message "%s %s %s" status (current-buffer) url-http-end-of-headers)
	 (condition-case err
	     (progn
	       (goto-char url-http-end-of-headers)
	       (let ((error-list (malabar-flycheck-error-parser (json-read) checker buffer)))
		 (kill-buffer (current-buffer))
		 ;(message "ERROR LIST:%s" error-list)
		 (with-current-buffer buffer
		   (funcall cback 'finished error-list))))
	   (error (let ((msg (error-message-string err)))
		    (message "flycheck error: %s" msg)
		    (funcall cback 'errored msg)))))
       pom-path script))))




(defun malabar-flycheck-error-new (checker error-info buffer)
  (flycheck-error-new
   :buffer buffer
   :checker checker
   :filename (buffer-file-name buffer)
   :line (cdr (assq     'line error-info))
   :column (cdr (assq   'startColumn error-info))
   :message (cdr (assq  'message error-info))
   :level 'error))

   

(defun malabar-flycheck-error-parser (output checker buffer)
  "Parse errors in OUTPUT which is a JSON array"
  (let ((rtnval (mapcar (lambda (e)
			  (malabar-flycheck-error-new checker e buffer))
			output)))
    rtnval))
	

(flycheck-define-generic-checker 'jvm-mode-malabar
  "Integrate flycheck with the malabar JVM service."
  :start #'malabar-flycheck-command
  :modes '(java-mode groovy-mode)
)

(add-to-list 'flycheck-checkers 'jvm-mode-malabar)


;;
;; EDE
;;

(defun malabar-maven2-extract-classpath (pom-file)
  (interactive "fPOM:")
  (let ((pi (malabar-project-info pom-file)))
    (-filter 'file-exists-p 
	     (apply #'append
		    (malabar-project-additional-classpath)
		    (malabar-project-resources pi 'test)
		    (malabar-project-resources pi 'runtime)
		    (malabar-project-sources pi 'test)
		    (malabar-project-sources pi 'runtime)
		    (malabar-project-elements pi 'test)
		    (malabar-project-elements pi 'runtime)
		    (malabar-project-classpath pi' test)
		    nil))))


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
				 :current-target "install"
				 :classpath (malabar-maven2-extract-classpath (expand-file-name "pom.xml" dir))
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

;;;    
;;; Project
;;;

(require 'json)

(defvar url-http-end-of-headers)


(defun malabar-url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg))
			      "="
			      (url-hexify-string (cdr arg))))
		    args
		    "&")))
    (url-retrieve url 'malabar-kill-url-buffer)))

(defun malabar-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun malabar-post-additional-classpath ()
  (interactive)
  (let ((url (format "http://%s:%s/add/"
		     malabar-server-host
		     malabar-server-port)))
    (malabar-url-http-post url (list
				(cons "pm"        malabar-mode-project-file)
				(cons "relative"  (json-encode malabar-package-additional-classpath))))))


(defun malabar-parse-script-raw (callback pom script &optional repo)
  "Parse the SCRIPT and call CALLBACK with the results buffer"
  (interactive "fPOM File:\nfJava File:")
  (let* ((repo (or repo (expand-file-name malabar-package-maven-repo)))
	 (url (format "http://%s:%s/parse/?repo=%s&pm=%s&script=%s" 
		      malabar-server-host
		      malabar-server-port
		      repo (expand-file-name pom) (expand-file-name script))))
    ;(message "URL %s" url)
    (url-retrieve url callback)))




(defun malabar-project-info (pom &optional repo)
  "Get the project info for a "
  (interactive "fPOM File:")
  (let* ((repo (or repo (expand-file-name malabar-package-maven-repo)))
	 (url (format "http://%s:%s/pi/?repo=%s&pm=%s" 
		      malabar-server-host
		      malabar-server-port
		      repo (expand-file-name pom))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun malabar-project-classpath (project-info scope)
  "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar 'identity (cdr (assq 'classpath (assq scope project-info)))))

(defun malabar-project-resources (project-info scope)
    "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar (lambda (r) (cdr (assq 'directory r)))
	  (cdr (assq 'resources (assq scope project-info)))))

(defun malabar-project-sources (project-info scope)
    "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar 'identity (cdr (assq 'sources (assq scope project-info)))))

(defun malabar-project-elements (project-info scope)
    "SCOPE is either 'test or 'runtime"
  (interactive)
  (mapcar 'identity (cdr (assq 'elements (assq scope project-info)))))

(defun malabar-project-additional-classpath ()
  (interactive)
  "Returns a list of classpath elements outside the normal
  project control.  See `malabar-package-additional-classpath'"
  (-filter 'file-exists-p
	  (mapcar (lambda (p) (concat malabar-mode-project-dir p))
		  malabar-package-additional-classpath)))


;;;
;;; Reflection
;;;


(defun malabar-where (name)
  (interactive "sClass:")
  (let ((fr (semanticdb-find-tags-by-name-regexp name)))
    (caar fr)))

(defun malabar-get-tag-name (tag-class &optional buffer)
  (let* ((buffer (or buffer (current-buffer)))
	 (tag (car (semantic-find-tags-by-class tag-class buffer))))
    (when tag
      (semantic-tag-name tag))))


(defun malabar-get-fully-qualified-class-name (&optional buffer)
  (format "%s.%s" (malabar-get-tag-name 'package buffer)
	  (malabar-get-tag-name 'type buffer)))

(defun malabar-fully-qualified-class-name-kill-ring-save (&optional buffer)
  (interactive)
  (let ((s (malabar-get-fully-qualified-class-name buffer)))
    (kill-new s)
    (message "Copied %s" s)))


(defun malabar-semantic-heirarchy (typename)
  "Display classes typename extends and interfaces it implements."
  ;; @todo - use a fancy completing reader.
  (interactive "sType Name: ")

  ;; When looking for a tag of any name there are a couple ways to do
  ;; it.  The simple `semanticdb-find-tag-by-...' are simple, and
  ;; you need to pass it the exact name you want.
  ;;
  ;; The analyzer function `semantic-analyze-tag-name' will take
  ;; more complex names, such as the cpp symbol foo::bar::baz,
  ;; and break it up, and dive through the namespaces.

  ;; For some reason, it only uses the classname and not the binary class name.
  (let ((class (semantic-analyze-find-tag (car (last (split-string typename "[.]")))))
	(cb (current-buffer)))

    (when (not (semantic-tag-p class))
      (error "Cannot find class %s" class))
    (let ((bname  "*Malabar Heirarchy*"))
      (with-output-to-temp-buffer bname
	
	;; There are many semantic-format-tag-* fcns.
	;; The summarize routine is a fairly generic one.
	(princ (semantic-format-tag-summarize class))
	(princ "\n")
	(princ "\tExtends:\n")
	(let ((supers (semantic-tag-type-superclasses class)))
	  (dolist (ele supers)
	    (princ  "\t\t")
	    (with-current-buffer bname
	      (let ((button (insert-button ele)))
		(button-put button 'buffer cb)
		(button-put button 'action 'malabar-semantic-button-handler)))
	    (princ "\n")))
	(princ "\tImplements:\n")
	(let ((interfaces (semantic-tag-type-interfaces class)))
	  (dolist (ele interfaces)
	    (princ  "\t\t")
	    (with-current-buffer bname
	      (let ((button (insert-button ele)))
		(button-put button 'buffer cb)
		(button-put button 'action 'malabar-semantic-button-handler)))
	    (princ "\n")))))))

(defun malabar-semantic-button-handler (button)
  "Handle the button for `malabar-semantic-heirarchy` to be able
to open referenced classes.  Expects the button property 'buffer
to hold the original buffer where `malabar-semantic-heirarchy`
was called."
  (let ((label (button-label button)))
    (with-current-buffer (button-get button 'buffer)
      (malabar-semantic-heirarchy label))))


;;;
;;; TEST
;;;

(defvar malabar-java-stack-trace-dirs (list "src/main/java" "src/main/groovy" "src/test/java""src/test/groovy"))

(defun malabar-java-stack-trace-best-filename (package2 file)
  (interactive "sPackage:\nsFile:")
  (let* ((root malabar-mode-project-dir)
	 (files (-filter 'file-exists-p 
			 (mapcar (lambda (dir) 
				   (let ((rtnval (concat root dir "/"
							 package2 "/"
							 file)))
				     (message "POSSIBLE FILE: %s %s %s" (current-buffer) root rtnval)
				     rtnval))
				 malabar-java-stack-trace-dirs))))
    (message "FILES++ :%s" files)
    (if (> (length files) 0)
	(elt files 0)
      (concat root (elt malabar-java-stack-trace-dirs  0) "/" package2 "/" file))))
      
	 
(defun malabar-java-stack-trace-regexp-to-filename ()
  "Generates a relative filename from java-stack-trace regexp match data."
  (let* ((root malabar-mode-project-dir)
	 (package (match-string 1))
	 (package2 (replace-regexp-in-string "\\." "/" package))
	 (class (match-string 2))
	 (method (match-string 3))
	 (file (match-string 4))
	 (line (match-string 5)))
    (malabar-java-stack-trace-best-filename package2 file)))
	 

(add-to-list 'compilation-error-regexp-alist 'malabar-java-stack-trace)
(add-to-list 'compilation-error-regexp-alist-alist
	     '(malabar-java-stack-trace .
				("^[[:space:]]at[[:space:]]\\([a-zA-Z.$_0-9]+\\)[.]\\([a-zA-Z.$_0-9]+\\)[.]\\([a-zA-Z.$_0-9]+\\)(\\([^:)]*\\):\\([0-9]+\\))"
				 malabar-java-stack-trace-regexp-to-filename 5)))

(defun malabar-project-copy-buffer-locals ( src-buffer)
  (interactive "bSource Buffer:")
  (let ((target-buffer (current-buffer)))
    (with-current-buffer src-buffer
      (let ((name malabar-mode-project-name)
	    (dir malabar-mode-project-dir)
	    (file malabar-mode-project-file))
	(with-current-buffer target-buffer
	  (setq malabar-mode-project-dir dir)
	  (setq malabar-mode-project-file file)
	  (setq malabar-mode-project-name name))))))

(defun malabar-stack-trace-buffer ( &optional buffer)
  ""
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (let ((beg (region-beginning))
	    (end (region-end))
	    (active (region-active-p)))

	
	(with-current-buffer (pop-to-buffer (format "*Malabar Stack Trace<%s>*" malabar-mode-project-name))
	  (malabar-project-copy-buffer-locals buffer)
	  (compilation-mode)
	  (malabar-project-copy-buffer-locals buffer)
	  (setq inhibit-read-only t)
	  (when active
	    (let ((start (point-max)))
	    (goto-char start)
	    (insert-buffer-substring buffer beg end)
	    (goto-char start))))))))
	  

(define-derived-mode malabar-unittest-list-mode tabulated-list-mode "malabar-mode" 
  "Used by `malabar-test-run' to show the test failures"
  (setq tabulated-list-format [("Desc" 50 t)
                               ("Msg" 80 nil)
			       ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Desc" nil))
  (tabulated-list-init-header))

(defun print-current-line-id ()
  (interactive)
   (message (concat "current line ID is: " (tabulated-list-get-id))))

(defun malabar-unittest-show-stacktrace (button)
  (let* ((buffer (current-buffer))
	 (entry (tabulated-list-get-entry))
	 (trace (elt entry 3)))
    (pop-to-buffer (format "*Malabar Trace<%s>*" (tabulated-list-get-id)) nil)
    (insert trace)
    (malabar-project-copy-buffer-locals buffer)
    (compilation-mode)
    (malabar-project-copy-buffer-locals buffer)
    (goto-char (point-min))))


(defun malabar-unittest-list (results buffer)
  (interactive)
  (let ((results (mapcar (lambda (r) 
			   (let (( id (elt r 0))
				 ( msg (elt r 1))
				 ( exmsg (elt r 2))
				 ( trace (elt r 2)))
			     (when (null msg) (aset r 1 ""))
			     (when (null exmsg) (aset r 2 ""))
			     (aset r 0 (cons id (list 'action 'malabar-unittest-show-stacktrace )))
			     (list id  r))) results)))
    (if (= (length results) 0)
	(message "Success")
      (with-current-buffer buffer
	(pop-to-buffer (format "*Malabar Test Results<%s>*" malabar-mode-project-name) nil)
	(malabar-unittest-list-mode)
	(malabar-project-copy-buffer-locals buffer)
	(setq tabulated-list-entries results)
	(tabulated-list-print t)))))


(defun malabar-run-test (use-method &optional buffer repo pom )
  "Runs the current buffer as a unit test, using jUnit.  

   USE-METHOD: if USE-METHOD is non-nil or With a  prefix arg, 
               ask for a name of a method in and only run that unit test

   BUFFER: the buffer or default to (current-buffer)

   REPO:   the maven repo dir default=malabar-package-maven-repo

   pom:    the dir to the pom file.  Default: search up the file tree for a pom.xml"

  (interactive "P\n")
  (let* ((repo (or repo (expand-file-name malabar-package-maven-repo)))
	 (buffer (or buffer (current-buffer)))
	 (script (buffer-file-name buffer))
	 (pom (or pom malabar-mode-project-file))
	 (method (if use-method (format "&method=%s" (read-string "Method Name:")) ""))
	 (url (format "http://%s:%s/test/?repo=%s&pm=%s&script=%s%s" 
		      malabar-server-host
		      malabar-server-port
		      repo 
		      (expand-file-name pom) 
		      (expand-file-name script)
		      method)))
    (save-some-buffers)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (malabar-unittest-list (json-read) buffer))))
    
	
	

;;;
;;; MODE
;;;

(defun malabar-version (&optional show-version)
  "Get the Malabar version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'malabar)))
    (when show-version
      (message "Malabar version: %s" version))
    version))

(defun malabar-cheatsheet ()
  "Open the cheat sheet for malabar-mode"
  (interactive)
  (find-file-read-only-other-window 
   (expand-file-name (concat malabar-install-directory "malabar-cheatsheet.org"))))


(defvar malabar-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?p] 'ede-compile-target)
    ;; (define-key map [?\C-b] 'malabar-install-project)
    ;; (define-key map [?\C-c] 'malabar-compile-file)
    ;; (define-key map [?\C-g] 'malabar-insert-getset)
    (define-key map [?t]    'malabar-run-test)
    (define-key map [?\?]   'malabar-cheatsheet)
    ;; (define-key map [?\C-t] 'malabar-run-junit-test)
    ;; (define-key map [?\M-t] 'malabar-run-all-tests)
    ;; (define-key map [?\C-z] 'malabar-import-one-class)
    ;; (define-key map [?z]    'malabar-import-all)
    ;; (define-key map [?\C-o] 'malabar-override-method)
    ;; (define-key map [?\C-e] 'malabar-extend-class)
    ;; (define-key map [?\C-i] 'malabar-implement-interface)

    (define-key map [?i] 'semantic-ia-describe-class)
    (define-key map [?h] 'malabar-semantic-heirarchy)
    ;; (define-key map [?.] (if malabar-use-external-cedet
    ;; 				       'semantic-ia-complete-symbol-menu
    ;; 				     'semantic-ia-complete-symbol))
    (define-key map [?\C-.] 'semantic-ia-complete-symbol)   
    (define-key map [?*] 'malabar-fully-qualified-class-name-kill-ring-save)
    ;;  (define-key map [?w] 'malabar-which) 
    (define-key map [?\C-p] 'ede-edit-file-target)
      ;; (define-key map [?\C-y] 'malabar-jump-to-thing)
    ;;   (define-key map [?\C-r] malabar-refactor-map)
    ;;   (define-key map malabar-mode-key-prefix prefix-map))
    ;; (define-key map "\M-n" 'next-error)
    ;; (define-key map "\M-p" 'previous-error)
    ;; (define-key map ":" 'malabar-electric-colon)
    (define-key map (kbd "C-k") 'malabar-groovy-send-buffer)
    (define-key map (kbd "C-#") 'malabar-stack-trace-buffer)
    (define-key map "s" 'malabar-groovy-send-classpath-of-buffer)
    (define-key map "S" 'malabar-groovy-send-classpath-element)
    (define-key map "V" 'malabar-version)
    map)
  "Keymap of Malabar interactive commands.")


(defvar malabar-mode-menu-map
  (easy-menu-create-menu
   "JVM"
   '(["Enable JVM Support" malabar-mode
      :style toggle :selected malabar-mode
      :enable  malabar-mode ]
     ["Send classpath" malabar-groovy-send-classpath-of-buffer malabar-mode]
     ["Send classpath element" malabar-groovy-send-classpath-element malabar-mode]
     ["Send buffer" malabar-groovy-send-buffer malabar-mode]
     "---"
     ["Show Malabar version" malabar-version t]))

  "Menu of `malabar-mode'.")

(easy-menu-add-item nil '("Development") malabar-mode-menu-map "JVM")




(defvar malabar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map malabar-keymap-prefix malabar-command-map)
    ;; We place the menu under a custom menu key.  Since this menu key is not
    ;; present in the menu of the global map, no top-level menu entry is added
    ;; to the global menu bar.  However, it still appears on the mode line
    ;; lighter.
    (define-key map [menu-bar malabar] malabar-mode-menu-map)
    map)
  "Keymap of `malabar-mode'.")

;;;###autoload
(define-minor-mode malabar-mode
  "Support and integeration for JVM languages"
  :lighter " JVM"
  :keymap malabar-mode-map

  
  (make-variable-buffer-local 'malabar-mode-project-file)
  (make-variable-buffer-local 'malabar-mode-project-dir)
  (make-variable-buffer-local 'malabar-mode-project-name)
  (let ((project-dir (ede-find-project-root "pom.xml")))
    (setq malabar-mode-project-dir project-dir )
    (setq malabar-mode-project-file (format "%spom.xml" project-dir ))
    (setq malabar-mode-project-name (file-name-nondirectory (directory-file-name project-dir))))

  (malabar-post-additional-classpath))



(add-hook 'groovy-mode-hook 'malabar-mode)
(add-hook 'java-mode-hook   'malabar-mode)


(provide 'malabar-mode)

;;(setq project-info (malabar-project-info "~/projects/malabar-mode-jar/pom.xml"))

