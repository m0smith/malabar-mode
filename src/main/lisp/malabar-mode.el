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
(eval-when-compile 
  (require 'cl)
  (require 'gud))
(require 'groovy-mode)
(require 'semantic/db-javap)
(require 'url-vars)
(require 'ede/maven2)

(require 'malabar-variables)
(require 'malabar-abbrevs)
(require 'malabar-project)
(require 'malabar-reflection)
(require 'malabar-service)
(require 'malabar-ede-maven)
(require 'malabar-mode-autoloads)


;;; 
;;; init
;;;

(defvar url-http-end-of-headers)


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

(defvar malabar-mode-post-groovy-to-be-called nil
  "Can take three values:

    nil - The groovy process is not running

    'init - The groovy process is running but the initialization of malabar-mode is not complete

    'running - The groovy process is running and initialized")


(defun malabar-groovy-init-hook ()
  "Called when the inferior groovy is started"
  (interactive)
  (message "Starting malabar server")
  (setq malabar-mode-post-groovy-to-be-called 'init)
  (malabar-groovy-send-string 
   (format "
     malabar = { classLoader = new groovy.lang.GroovyClassLoader(); 
         Map[] grapez = [[group: 'com.software-ninja' , module:'malabar', version:'%s']]; 
         groovy.grape.Grape.grab(classLoader: classLoader, grapez);
         classLoader.loadClass('com.software_ninja.malabar.MalabarStart').newInstance().startCL(classLoader); }; 
     malabar();" malabar-server-jar-version)))

(add-hook 'inferior-groovy-mode-hook 'malabar-groovy-init-hook)


(defun malabar-mode-load-class (&optional buffer)
  "Load the file pointed to by BUFFER (default current-buffer) into the running *groovy*"
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((name (cl-gensym)))
      (malabar-groovy-send-string 
       (format "%s = { def cl = this.getClass().classLoader; cl.clearCache(); def cls = cl.parseClass(new File('%s')); cl.setClassCacheEntry(cls); cls};%s();" name (buffer-file-name) name))
      (switch-to-groovy t))))

(defun malabar-groovy-send-classpath-element  (element)
  "Send a JAR, ZIP or DIR to the classpath of the running *groovy*"
  (interactive "fJAR, ZIP or DIR:")
  (malabar-groovy-send-string 
   (format "this.getClass().classLoader.rootLoader.addURL(new File('%s').toURL())" 
	   (expand-file-name element))))

(defun malabar-groovy-send-classpath  (pom &optional repo)
  "Add the classpath for POM to the runnning *groovy*."
  (interactive "fPOM File:")
  (mapcar 'malabar-groovy-send-classpath-element 
	  (malabar-project-classpath-list (malabar-project-info pom repo) 'test)))

(defun malabar-groovy-classpath-string  (pom &optional repo)
  "Add the classpath for POM to the runnning *groovy*."
  (interactive "fPOM File:")
  (mapconcat 'identity (malabar-project-test-source-directories (malabar-project-info pom repo))
	     path-separator))


(defun malabar-groovy-classpath-string-of-buffer  ( &optional buffer repo)
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (let ((pom malabar-mode-project-file))
	(malabar-groovy-classpath-string pom repo)))))

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
;;; JSON
;;;

(defun malabar-json-read-string ()
    "Read the JSON string at point.

If the string starts with a :, intern the result.

See `json-read-string'"

    (let ((rtnval (json-read-string)))
      (if (eq ?: (string-to-char rtnval))
	  (intern rtnval)
	rtnval)))


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
       (lambda (_status)
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


;;;
;;; JDK
;;;

(defun malabar-project-buffer-p (pm &optional buffer)
  "Return the buffer is it is a member of the project managed by
  PM (maybe a pom file)"
  (with-current-buffer (or buffer (current-buffer))
    (when (and (equal malabar-mode-project-file pm)
	       buffer-file-name)
      (current-buffer))))

(defun malabar-project-parse-file-async (&optional buffer)
  "Parse the file in the current buffer"
  (with-current-buffer (or buffer (current-buffer))
    (malabar-parse-script-raw
     (lambda (_status) (kill-buffer (current-buffer)))
     malabar-mode-project-file (buffer-file-name))))

(defun malabar-project-update-port (pm port)
  (add-to-list 'malabar-mode-project-service-alist 
	       (list pm port)
	       nil 
	       (lambda (x y) nil)))
	
(defun malabar-project-port (pm &optional no-default)
  "If NO-DEFAULT is non-nil, only return a found port"
  (let ((pm (or pm malabar-mode-project-file)))
    (or (cadr (assoc pm malabar-mode-project-service-alist))
	(and (not no-default) malabar-server-port))))

(defun malabar-jdk-file (dir f)
  (concat (file-name-as-directory dir) f))

(defun malabar-jdk-find-home (rt-dir)
  "Return the java home for a rt.jar full path, if it exists, or nil"
  (when (and rt-dir (file-exists-p rt-dir))
    (substring rt-dir 0 
	       (- (length rt-dir) 
		  (length cedet-java-core-jar-name)))))

; Based on `cedet-java-try-to-list-jdk-dirs'
(defun malabar-jdk-try-to-list-jdk-dirs (basedirs all-res)
  "Searches for JDKs in specified directories (basedirs) and using specified regexes (all-res)

ALL-RES is a list if strings to use a a regex of files in each
BASEDIRS or if :self then just use the directory itself"
  (-flatten
   (mapcar (lambda (dir)
	     (when (file-exists-p dir)
	       (mapcar (lambda (hamster)
			 (let ((files (cond
				       ((equal hamster :self) (list dir))
				       ( t (directory-files dir t hamster)))))
			   (mapcar (lambda (file)
				     (cedet-java-create-rt-file-name file))
				   files)))
		       all-res)))
	   basedirs)))
  




  ;; (let (rt-path
  ;; 	(bdirs (copy-sequence basedirs)))
  ;;   (while (and (null rt-path) (not (null bdirs)))
  ;;     (when (file-exists-p (car bdirs))
  ;; 	(let* ((res (copy-sequence all-res)))
  ;; 	  (while (and (null rt-path) (not (null res)))
  ;; 	    (let ((files (directory-files (car bdirs) t (car res))))
  ;; 	      (while (and (null rt-path) (not (null files)))
  ;; 		(when (and (car files) (file-directory-p (car files)))
  ;; 		  (let ((fname (cedet-java-create-rt-file-name (car files))))
  ;; 		    (when (and fname (file-exists-p fname))
  ;; 		      (setq rt-path fname))))
  ;; 		(setq files (cdr files))))
  ;; 	    (setq res (cdr res)))))
  ;;     (setq bdirs (cdr bdirs)))
  ;;   rt-path))


;  "This code is stolen from `cedet-java-find-jdk-core-jar' which uses
; it to find the current JDK.  It would be useful to see all the
; install JDKs"
(defun malabar-jdk-installed-jvm-roots ()
  "Return a list of all the installed JVMs.  Searches the common
install locations in addition to the directories in
`malabar-jdk-extra-locations'"
  (let* 
      ((funcs (list
	       (lambda () (cedet-java-create-rt-file-name cedet-java-jdk-root))
	       (lambda () (cedet-java-create-rt-file-name (getenv "JAVA_HOME")))
	       (lambda () (cedet-java-create-rt-file-name (getenv "JDK_HOME")))
	       (lambda () (cedet-java-check-symlinks "/etc/alternatives/java"))
	       (lambda () (cedet-java-check-symlinks "/usr/bin/java"))
	       ;; Linux...
	       (lambda () (malabar-jdk-try-to-list-jdk-dirs '("/usr/lib/jvm" "/usr/local/lib/jvm")
							   '("default-java" ".*sun.*" ".*jdk.*" ".*gcj.*")))
	       ;; Mac OS X
	       (lambda () (malabar-jdk-try-to-list-jdk-dirs '("/Library/Java/JavaVirtualMachines/"
							     "/System/Library/Java/JavaVirtualMachines/")
							   '(".*[jJ][dD][kK].*")))
	       ;; TODO: Check Windows (How it will behave on Non-English Windows?)
	       (lambda () (malabar-jdk-try-to-list-jdk-dirs 
			   (mapcar (lambda (d)
				     (malabar-jdk-file d "Java")) 
				   (append
				    (list (getenv "PROGRAMFILES")
					  (getenv "ProgramFiles(x86)")
					  (getenv "ProgramW6432"))
				    malabar-jdk-extra-locations))
			   '(".*jdk.*" ".*jre.*")))
	       (lambda () (malabar-jdk-try-to-list-jdk-dirs malabar-jdk-extra-locations 
							    '(".*jdk.*" ".*jre.*" :self))))))
     (-flatten (mapcar (lambda (f) (mapcar #'malabar-jdk-find-home (funcall f)))
		       funcs))))


(defun malabar-jdk-installed-jvms ()
  "Return an alist of JVM name as a string to 
    '(HOME)"
  (let ((roots (malabar-jdk-installed-jvm-roots)))
    (mapcar (lambda (root)
	      (cons (file-name-nondirectory (directory-file-name root))
		     (list root) ))
	    roots)))


(defun malabar-jdk-stop (&optional pm)
  (let* ((pm (or pm malabar-mode-project-file))
	 (port (malabar-project-port pm t)))
    (when port
      (message "Stopping %s port %s" malabar-mode-project-name port)
      (condition-case err
	  (malabar-service-call "stop" (list "pm" pm))
	(error err (message "%s" err)))
      (malabar-project-update-port malabar-mode-project-file nil))))

(defun malabar-jdk-start (jdk)
  (interactive (list (completing-read "JDK:" (malabar-jdk-installed-jvms))))
  (malabar-jdk-stop)

  (let* ((jdk-alist (malabar-jdk-installed-jvms))
	 (port (+ 49152 (random (- 65535 49152))))
	 (jdk (cadr (assoc jdk jdk-alist)))
	 (cwd (ede-find-project-root "pom.xml"))
	 (rtnval (malabar-service-call "spawn"
				       (list "port" (number-to-string port)
					     "version" malabar-server-jar-version
					     "jdk" jdk
					     "cwd" cwd
					     "class" "com.software_ninja.malabar.Malabar"))))
    (malabar-project-update-port malabar-mode-project-file port)
    (malabar-post-additional-classpath*)
    ;; Reparse all file buffers in the project using the new jdk
    (mapcar #'malabar-project-parse-file-async
	    (-filter (lambda (b) (malabar-project-buffer-p malabar-mode-project-file b)) 
		       (buffer-list)))
    (message "%s is using service port %s" malabar-mode-project-name port)
    rtnval))

;;;    
;;; Project
;;;

(require 'json)

(defvar url-http-end-of-headers)


(defun malabar-url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (setq url-request-method "POST"
	url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))
	url-request-data (mapconcat (lambda (arg)
				      (concat (url-hexify-string (car arg))
					      "="
					      (url-hexify-string (cdr arg))))
				    args
				    "&"))
    (url-retrieve url 'malabar-kill-url-buffer))

(defun malabar-kill-url-buffer (_status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))


(defun malabar-post-additional-classpath* ()
  (let ((url (format "http://%s:%s/add/"
		     malabar-server-host 
		     (malabar-project-port malabar-mode-project-file))))
    (malabar-url-http-post url (list
				(cons "pm"        malabar-mode-project-file)
				(cons "relative"  (json-encode malabar-package-additional-classpath))))))
 

(defun malabar-post-additional-classpath ()
  (interactive)
  (when (equal malabar-mode-post-groovy-to-be-called 'init)
    (setq malabar-mode-post-groovy-to-be-called 'running)
    (malabar-post-additional-classpath*)))


(defun malabar-parse-script-raw (callback pom script &optional repo)
  "Parse the SCRIPT and call CALLBACK with the results buffer"
  (interactive "fPOM File:\nfJava File:")
  
  (setq url-request-method "GET"
	url-request-extra-headers nil
	url-request-data nil)
  
  (let* ((repo (or repo (expand-file-name malabar-package-maven-repo)))
	 (url (format "http://%s:%s/parse/?repo=%s&pm=%s&script=%s" 
		      malabar-server-host
		      (malabar-project-port (expand-file-name pom))
		      repo (expand-file-name pom) (expand-file-name script))))
    ;(message "URL %s" url)
    (url-retrieve url callback)))

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
  (let* ((package (match-string 1))
	 (package2 (replace-regexp-in-string "\\." "/" package))
	 ;(class (match-string 2))
	 ;(method (match-string 3))
	 (file (match-string 4))
	 ;(line (match-string 5))
	 )
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
      (let* ((active (region-active-p))
	     (beg (if active (region-beginning)))
	     (end (if active (region-end))))
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

(defun malabar-unittest-show-stacktrace (_button)
  (let* ((buffer (current-buffer))
	 (entry (tabulated-list-get-entry))
	 (trace (elt entry 3)))
    (pop-to-buffer (format "*Malabar Trace<%s>*" (tabulated-list-get-id)) nil)
    (setq inhibit-read-only t)
    (erase-buffer)
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
				 ;( trace (elt r 2))
				 )
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
	 (pom (or pom malabar-mode-project-file)))
	 
    (malabar-unittest-list (malabar-service-call "test"
						 (list "repo"   repo
						       "pm"     (expand-file-name pom)
						       "script" (expand-file-name script)
						       "method" (if use-method (read-string "Method Name:") nil)))
			   buffer)))
    
	
	
(defun malabar-jdb ()
  "Start the JDB debugger for the class in the current buffer.
The current buffer must have a java file with a main method"
  (interactive)
  (let ((classpath (malabar-groovy-classpath-string-of-buffer))
	(classname (malabar-get-fully-qualified-class-name)))
    (jdb (format "%s -classpath%s %s" gud-jdb-command-name classpath classname))))

(defun malabar-compute-package-name (&optional buffer)
  "Return the package name of the expected buffer, which is based
  on the directory relative to project source directory.  For example, if the file is
       src/main/java/com/m0smith/app/Test.java, 
  this function will  return \"com.m0smith.app\".  

  This function does not really care if the buffer is looking at a java file, but the file must
  be in the source or test source directory."

  (let* ((dir (expand-file-name (file-name-directory (buffer-file-name buffer))))
         (project-file (malabar-find-project-file buffer))
	 (project-info (malabar-project-info project-file))
         (source-directories (append (malabar-project-source-directories
                                      project-info)
                                     (malabar-project-test-source-directories
                                      project-info))))
    (replace-regexp-in-string
     "/" "."
     (substring dir (1+ (length
                         (cl-find dir source-directories
                               :test #'(lambda (dir src-dir)
                                         (string-starts-with dir (expand-file-name src-dir))))))
                (1- (length dir))))))


(defun malabar-update-package ()
  "Updates the package statement in the current buffer to match
the class's location in the file system, adding one if it is not
present."
  (interactive)
  (let ((computed-package (malabar-compute-package-name (current-buffer)))
        (actual-package (malabar-get-package-name (current-buffer))))
    (unless (equal computed-package actual-package)
      (let ((package-tag (malabar-get-package-tag (current-buffer))))
        (save-excursion
          (if (null package-tag)
              (progn (goto-char (point-min))
                     (malabar-forward-comment)
                     (unless (eolp)
                       (insert "\n\n")
                       (forward-line -2)))
            (goto-char (semantic-tag-start package-tag))
            (zap-to-char 1 ?\;))
          (insert "package " computed-package ";")
          ;; Work around a bug in semantic
          (semantic-parse-tree-set-needs-rebuild))))))

(defun malabar-forward-comment ()
  (c-forward-single-comment)
  (unless (bolp)
    (forward-line 1)))

(defun malabar-prompt-for-and-qualify-class (prompt &optional class)
  (let* ((class (or class
                    (read-from-minibuffer prompt)))
         (qualified-class (or (malabar-import-find-import class)
                              (malabar-qualify-class-name-in-buffer class)))
         (class-info (malabar-get-class-info qualified-class)))
    (list class qualified-class class-info)))

(defun malabar-goto-start-of-class ()
  "Move point the start of the class definition"
  (let ((class-tag (malabar-get-class-tag-at-point)))
    (goto-char (semantic-tag-start class-tag))))

(defun malabar-goto-end-of-class ()
  "Move point to the end of the class definition, that is, the
closing curly brace on the class"
  (let ((class-tag (malabar-get-class-tag-at-point)))
    (goto-char (1- (semantic-tag-end class-tag)))))

(defun malabar--implement-interface-move-to-insertion-point ()
  (malabar-goto-start-of-class)
  (skip-chars-forward "^{")
  (when (semantic-tag-type-interfaces (malabar-get-class-tag-at-point))
    (search-backward
     (car (last (semantic-tag-type-interfaces (malabar-get-class-tag-at-point)))))
    (goto-char (match-end 0))))


(defun malabar-first-member-of-class ()
" Returns the tag for the first member of the class or nil if
there are no members.
"
  (car (semantic-tag-type-members
	(car (semantic-brute-find-tag-by-class
	      'type (malabar-semantic-fetch-tags))))))

(defun malabar-goto-tag (tag)
  "Move point to begining of TAG and return the new point.  

When TAG is nil, point remains unchanged and return nil.  "
  (when tag
    (goto-char (semantic-tag-start tag))))



(defun malabar--add-delegate-var (qualified-class is-extension)
"
If IS-EXTENSION is a string, insert a field name IS-EXTENSION with type QUALIFIED-CLASS.  If not, do nothing and return nil.

Issue: gh-83
"
  (when (stringp is-extension)
    (let ((tag (malabar-first-member-of-class))
	  (field (concat "private " qualified-class " " is-extension ";\n")))
      (if tag
	  (malabar-goto-tag tag)
	(progn
	  (malabar-goto-end-of-class)
	  (insert "\n")))
    (insert field))))

(defun malabar-find-method-in-current-class (method-tag)
  (let ((class-tag (malabar-get-class-tag-at-point))
        (method-name (malabar--get-name method-tag))
        (method-argument-types
         (mapcar (lambda (arg)
                   (malabar-qualify-class-name-in-buffer (malabar--get-type arg)))
                 (malabar--get-arguments method-tag))))
    (cl-some (lambda (tag)
            (and (equal method-name
                        (semantic-tag-name tag))
                 (equal method-argument-types 
                        (mapcar (lambda (arg-tag)
                                  (malabar-qualify-class-name-in-buffer
                                   (semantic-tag-type arg-tag)))
                                (semantic-tag-function-arguments tag)))
                 tag))
          (semantic-tag-type-members class-tag))))


(defun malabar-overridable-method-p (method-tag)
  (and (not (malabar--final-p method-tag))
       (not (malabar-find-method-in-current-class method-tag))
       (or (malabar--public-p method-tag)
           (malabar--protected-p method-tag)
           (equal (malabar-get-package-name)
                  (malabar-get-package-of
                   (malabar--get-declaring-class method-tag))))))

(defun malabar-get-superclass (class-tag)
  (or (car (semantic-tag-type-superclasses class-tag))
       "Object"))

(defun malabar-get-superclass-at-point ()
  (malabar-qualify-class-name-in-buffer (malabar-get-superclass (malabar-get-class-tag-at-point))))


(defun malabar-overridable-methods ()
  (cl-remove-if-not (lambda (s)
                   (and (malabar--method-p s)
                        (malabar-overridable-method-p s)))
                 (malabar-get-members
                  (malabar-get-superclass-at-point))))


(defun malabar--override-methods (methods call-super &optional overridable-methods)
  (let* ((method-count (length methods))
         (progress-reporter (make-progress-reporter "Overriding methods..." 0 method-count))
         (counter 0)
         (overridable-methods (or overridable-methods
                                  (malabar-overridable-methods))))
    (message nil)
    (let ((malabar--import-candidates nil))
      (with-caches 
       (dolist (method methods)
         (progress-reporter-update progress-reporter (incf counter))
         (malabar--override-method method overridable-methods call-super t)))
      (malabar--import-handle-import-candidates malabar--import-candidates))
    (progress-reporter-done progress-reporter)
    (let ((class-tag (malabar-get-class-tag-at-point)))
      (indent-region (semantic-tag-start class-tag) (semantic-tag-end class-tag)))))


(defun malabar-override-method (&optional method-tag)
  "Adds a stub implementation overriding method from the
superclass to the class at point.  If METHOD-TAG is NIL, prompts
for the method to override."
  (interactive)
  (let ((overridable-methods (malabar-overridable-methods)))
    (unless method-tag
      (setq method-tag
            (malabar-choose "Method to override: "
                            (mapcar 'malabar-make-choose-spec
                                    overridable-methods))))
    (when method-tag
      (malabar--override-methods (list method-tag) t overridable-methods))))


(defun malabar--override-method-stub (method-tag is-extension)
  "Create the method body stub.  Method tag is the method to override. 

If is-extension is nil then do not call the the super or a
delegate.  If it is true, call the super only if the method is
not abstract.  If it is a string, call same method on that
variable named.
 
Issue: gh-83"
  (concat
   "// TODO: Stub\n"
   (let* ((call-super (or (stringp is-extension)
			  (and is-extension
			       (not (malabar--abstract-p method-tag)))))
	  (extension-var (if (stringp is-extension) is-extension "super"))
	  (super-call
	   (concat extension-var "." (malabar--get-name method-tag)
		   (malabar--stringify-arguments
		    (malabar--get-arguments method-tag)))))
     (if (equal (malabar--get-return-type method-tag) "void")
	 (if call-super
	     (concat super-call ";\n")
	   "")
       (concat "return "
	       (if call-super
                          super-call
		 (malabar-default-return-value
		  (malabar--get-return-type method-tag)))
	       ";\n")))))


(defun malabar--override-method (method-tag overridable-methods
                                            is-extension no-indent-defun)
  "Create an overridden method at the end of the current class"
  (malabar-goto-end-of-class)
  (insert "\n" 
	  (if (malabar--add-override-annotation? is-extension)
	      "@Override\n" 
	    "")
          (malabar-create-method-signature method-tag) " {\n"
	  (malabar--override-method-stub method-tag is-extension)
          "}\n")
    (forward-line -2)
    (unless no-indent-defun
      (c-indent-defun))
    (back-to-indentation)
    (cl-flet ((find-tag-from-class (name declaring-class tags)
                                (cl-find-if (lambda (tag)
                                           (and (equal (malabar--get-name tag)
                                                       name)
                                                (equal (malabar--get-declaring-class tag)
                                                       declaring-class)))
                                         tags)))
      (let ((equals-tag (find-tag-from-class "equals" "java.lang.Object"
                                             overridable-methods))
            (hashcode-tag (find-tag-from-class "hashCode" "java.lang.Object"
                                               overridable-methods)))
        (cond ((and (equal method-tag equals-tag)
                    hashcode-tag)
               (malabar-override-method hashcode-tag))
              ((and (equal method-tag hashcode-tag)
                    equals-tag)
               (malabar-override-method equals-tag))))))

(defun malabar--add-override-annotation? (_is-extension)
  "Return non-nil if the @Override tag should be added.
   Issue: gh-84"
  t)


(defun malabar--parse-type-parameters (type-parameters)
  (let ((str (subst-char-in-string
              ?> ?\)
              (subst-char-in-string
               ?< ?\(
               (subst-char-in-string ?, ?\  type-parameters t)
               t)
              t)))
    (when str
      (mapcar 'symbol-name (car (read-from-string str))))))


(defun malabar--query-for-type-parameters (tag)
  ;; FIXME: We shouldn't need to parse this here!
  ;; FIXME: Deals badly with nested parameters
  (let ((type-parameters
         (malabar--parse-type-parameters (malabar--get-type-parameters tag))))
    (with-caches
     (mapcar (lambda (p)
               (cons p
                     (let ((read-param (second (malabar-prompt-for-and-qualify-class
                                                (format "Value for type parameter %s: " p)))))
                       (if (equal "" read-param)
                           p
                         read-param))))
             type-parameters))))


(defun malabar--instantiate-type-parameters (tag)
  (let ((type-instances (malabar--query-for-type-parameters tag))
        (case-fold-search nil)
        (import-candidates nil))
    (while type-instances
      (unless (equal (caar type-instances) (cdar type-instances))
        (goto-char (point-min))
        (let ((re (concat "\\<" (caar type-instances) "\\>"))
              (rep (cdar type-instances)))
          (while (re-search-forward re nil t)
            (replace-match rep t t)))
        (push (cdar type-instances) import-candidates))
      (setq type-instances (cdr type-instances)))
    (malabar--import-handle-import-candidates import-candidates)))


(defun malabar-implement-interface* (&optional interface-in implement-keyword is-extension)
  "Adds INTERFACE to the current class's implements clause and
adds stub implementations of all the interface's methods.

IS-EXTENSION can be set to the name of a delegate to call.

"
  (unless implement-keyword
    (setq implement-keyword "implement"))
  (destructuring-bind (_interface qualified-interface interface-info)
      (malabar-prompt-for-and-qualify-class (format "Interface to %s: "
                                                    implement-keyword)
                                            interface-in)
    (unless (malabar--interface-p interface-info)
      (error "You cannot %s %s, it is not an interface"
             implement-keyword qualified-interface))
    (unless (malabar--class-accessible-p qualified-interface interface-info)
      (error "You cannot %s %s, it is not accessible from %s"
             implement-keyword qualified-interface (malabar-get-package-name)))
    (malabar--implement-interface-move-to-insertion-point)
    (if (semantic-tag-type-interfaces (malabar-get-class-tag-at-point))
        (insert ", ")
      (unless (bolp)
        (newline))
      (insert implement-keyword "s ")
      (indent-according-to-mode))
    (insert qualified-interface)
    ;(-when-let (type-parameters (malabar--get-type-parameters interface-info))
    ;  (insert type-parameters))
    (unless (eolp)
      (newline-and-indent))
    (malabar--add-delegate-var qualified-interface is-extension)
    (malabar--override-methods (malabar--get-methods interface-info) is-extension)
    (malabar--instantiate-type-parameters interface-info)
    (malabar-import-and-unqualify qualified-interface)))


(defun malabar-implement-interface (&optional interface implement-keyword)
  "Adds INTERFACE to the current class's implements clause and
adds stub implementations of all the interface's methods."
  (interactive)
  (malabar-implement-interface* interface implement-keyword))

(defun malabar-delegate-interface (delegate &optional interface implement-keyword)
  "Adds INTERFACE to the current class's implements clause and
adds stub implementations of all the interface's methods.

Issue: gh-83"
  (interactive "sName:")
  (malabar-implement-interface* interface implement-keyword delegate))


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

(defun malabar-which (class-name &optional buffer)
  (interactive "sClass:")
  (message "%s" (malabar-reflection-which class-name buffer)))

(defun malabar-electric-colon (arg)
  "Acts as `c-electric-colon'.

In addition, if `malabar-electric-elvis-p' is non-nil, the colon is
not inside a literal and a prefix ARG hasn't been supplied, the
command performs the following transform:

'foo ?:' => 'foo != null ? foo :'."
  (interactive "*P")
  (let ((looking-at-elvis-p (char-equal (char-before (point)) ??)))
    (c-electric-colon arg)
    (when (and malabar-electric-elvis-p
               looking-at-elvis-p
               c-electric-flag
               (not (c-save-buffer-state () (c-in-literal)))
               (not arg))
      (let ((end (point)))
        (forward-sexp -1)
        (while (not (eql (char-syntax (char-before (point))) ?\s))
          (forward-sexp -1))
        (let ((value (string-trim (buffer-substring-no-properties (point) (- end 2)))))
          (forward-char (length value))
          (delete-char (- end (point)))
          (insert " != null ? " value " :"))))))


(defun malabar-jump-to-thing (point)
  "Jumps to the definition of the 'thing' at point.
More technically, uses `semantic-analyze-current-context' output
to identify an origin for the code at point, taking type
membership into account.  This function is much like
`semantic-ia-fast-jump', only a little smarter."
  (interactive "d")
  (let* ((ctxt (semantic-analyze-current-context point))
         (prefix (and ctxt (reverse (oref ctxt prefix))))
         (first (first prefix))
         (second (second prefix)))
    (cond ((semantic-tag-p first)
           (malabar--jump-to-thing-helper first))
          ((semantic-tag-p second)
           ;; so, we have a tag and a string
           ;; let's see if the string is a subtag of the type of the tag
           (let* ((type (car (reverse (oref ctxt prefixtypes))))
                  ;; TODO: prompt with completion if more than one match
                  (first-tag
                   (car (semantic-find-tags-by-name first
                                                    (semantic-tag-type-members type)))))
             (cond ((semantic-tag-with-position-p first-tag)
                    (malabar--jump-to-thing-helper first-tag))
                   ((and (semantic-tag-with-position-p type)
                         (y-or-n-p (format "Could not find `%s'. Jump to %s? "
                                           first (semantic-tag-name type))))
                    (malabar--jump-to-thing-helper type))
                   ((y-or-n-p (format "Could not find `%s'. Jump to %s? "
                                      first (semantic-tag-name second)))
                    (malabar--jump-to-thing-helper second)))))
          ((semantic-tag-of-class-p (semantic-current-tag) 'include)
           (semantic-decoration-include-visit))
          (t
           (error "Could not find suitable jump point for %s" first)))))

(defun malabar--jump-to-thing-helper (destination)
  (when (not (or (semantic-tag-with-position-p destination)
                 (semantic-tag-file-name destination)))
    (error "Tag %s has no suitable position defined"
           (semantic-format-tag-name destination)))
  ;; Lifted from semantic-ia--fast-jump-helper
  ;; Once we have the tag, we can jump to it.  Here
  ;; are the key bits to the jump:

  ;; 1) Push the mark, so you can pop global mark back, or
  ;;    use semantic-mru-bookmark mode to do so.
  (push-mark)
  (when (fboundp 'push-tag-mark)
    (push-tag-mark))
  ;; 2) Visits the tag.
  (semantic-go-to-tag destination)
  ;; 3) go-to-tag doesn't switch the buffer in the current window,
  ;;    so it is like find-file-noselect.  Bring it forward.
  (switch-to-buffer (current-buffer))
  ;; 4) Fancy pulsing.
  (pulse-momentary-highlight-one-line (point)))


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
    (define-key map [?\C-z] 'malabar-import-one-class)
    (define-key map [?z]    'malabar-import-all)
    ;; (define-key map [?\C-o] 'malabar-override-method)
    ;; (define-key map [?\C-e] 'malabar-extend-class)
    (define-key map [?\C-i] 'malabar-implement-interface)

    (define-key map [?i] 'semantic-ia-describe-class)
    (define-key map [?h] 'malabar-semantic-heirarchy)
    ;; (define-key map [?.] (if malabar-use-external-cedet
    ;; 				       'semantic-ia-complete-symbol-menu
    ;; 				     'semantic-ia-complete-symbol))
    (define-key map [?\C-.] 'semantic-ia-complete-symbol)   
    (define-key map [?*] 'malabar-fully-qualified-class-name-kill-ring-save)
    (define-key map [?w] 'malabar-which) 
    (define-key map [?\C-p] 'ede-edit-file-target)
    (define-key map [?\C-y] 'malabar-jump-to-thing)
    ;;   (define-key map [?\C-r] malabar-refactor-map)
    ;;   (define-key map malabar-mode-key-prefix prefix-map))
    ;; (define-key map "\M-n" 'next-error)
    ;; (define-key map "\M-p" 'previous-error)
    (define-key map ":" 'malabar-electric-colon)
    (define-key map (kbd "C-k") 'malabar-groovy-send-buffer)
    (define-key map (kbd "C-#") 'malabar-stack-trace-buffer)
    (define-key map "s" 'malabar-groovy-send-classpath-of-buffer)
    (define-key map "S" 'malabar-groovy-send-classpath-element)
    (define-key map "l" 'malabar-mode-load-class)
    (define-key map "V" 'malabar-version)
    (define-key map "D" 'malabar-jdb)
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
    (define-key map ":" 'malabar-electric-colon)
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
  (let ((project-dir (ede-find-project-root "pom.xml")))
    (setq malabar-mode-project-dir project-dir )
    (setq malabar-mode-project-file (format "%spom.xml" project-dir ))
    (setq malabar-mode-project-name (file-name-nondirectory (directory-file-name project-dir))))
  
  (malabar-post-additional-classpath)
  (malabar-abbrevs-setup))

(make-variable-buffer-local 'malabar-mode-project-file)
(make-variable-buffer-local 'malabar-mode-project-dir)
(make-variable-buffer-local 'malabar-mode-project-name)



(add-hook 'groovy-mode-hook 'malabar-mode)
(add-hook 'java-mode-hook   'malabar-mode)


(provide 'malabar-mode)

