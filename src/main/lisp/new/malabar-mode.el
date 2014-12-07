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
      (let ((pom (format "%spom.xml" (ede-find-project-root "pom.xml"))))
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


(defadvice flycheck-start-command-checker (around flycheck-start-using-function act)
  "Put some nice docs here"
  (message "CHECKER: %s CALLBACK %s" checker callback)
  (let ((func (get checker 'flycheck-command-function)))
    (if func
	(apply func checker callback (flycheck-checker-substituted-arguments checker))
      ad-do-it)))


(defadvice flycheck-interrupt-command-checker (around flycheck-check-process-first-malabar act)
  (when process
      ad-do-it))

(defun malabar-flycheck-command ( checker callback source source-original)
  ""
  (let* ((pom (ede-find-project-root "pom.xml"))
	 (pom-path (format "%spom.xml" pom)))
    (message "command args:%s %s %s %s" (current-buffer) pom-path  source source-original)
    (let ((output (malabar-parse-script-raw pom-path source)))
      (message "parsed: %s" output)
      (flycheck-finish-checker-process checker 0 flycheck-temporaries output callback))))


(defun malabar-flycheck-error-new (checker error-info)
  ;;(message "error-info %s" error-info)
  (flycheck-error-new
   :buffer (current-buffer)
   :checker checker
   :filename (cdr (assq 'sourceLocator error-info))
   :line (cdr (assq     'line error-info))
   :column (cdr (assq   'startColumn error-info))
   :message (cdr (assq  'message error-info))
   :level 'error))

   

(defun malabar-flycheck-error-parser (output checker buffer)
  "Parse errors in result"
  (let ((rtnval (mapcar (lambda (e) (malabar-flycheck-error-new checker e)) (json-read-from-string output))))
    ;(flycheck-safe-delete-temporaries)
    rtnval))
	

(flycheck-define-checker jvm-mode-malabar
  ""
       :command ("echo" source source-original ) ;; use a real command
       :modes (java-mode groovy-mode)
       :error-parser malabar-flycheck-error-parser
)

(put 'jvm-mode-malabar 'flycheck-command-function 'malabar-flycheck-command)
(add-to-list 'flycheck-checkers 'jvm-mode-malabar)
(message "%s" (symbol-plist 'jvm-mode-malabar))

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

;;;    
;;; Project
;;;

(require 'json)

(defvar url-http-end-of-headers)



(defun malabar-parse-script-raw (pom script &optional repo)
  "Parse the SCRIPT "
  (interactive "fPOM File:\nfJava File:")
  (let* ((repo (or repo (expand-file-name "~/.m2/repository")))
	 (url (format "http://localhost:4428/parse/?repo=%s&pom=%s&script=%s" repo (expand-file-name pom) (expand-file-name script))))
    (message "URL %s" url)
    (with-current-buffer (url-retrieve-synchronously url)
      (message "parse buffer %s" (current-buffer))
      (goto-char url-http-end-of-headers)
      (buffer-substring (point) (point-max)))))

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


(defvar malabar-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?p] 'ede-compile-target)
    ;; (define-key map [?\C-b] 'malabar-install-project)
    ;; (define-key map [?\C-c] 'malabar-compile-file)
    ;; (define-key map [?\C-g] 'malabar-insert-getset)
    ;; (define-key map [?t]    'malabar-run-test)
    ;; (define-key map [?\?]   'malabar-cheatsheat)
    ;; (define-key map [?\C-t] 'malabar-run-junit-test)
    ;; (define-key map [?\M-t] 'malabar-run-all-tests)
    ;; (define-key map [?\C-z] 'malabar-import-one-class)
    ;; (define-key map [?z]    'malabar-import-all)
    ;; (define-key map [?\C-o] 'malabar-override-method)
    ;; (define-key map [?\C-e] 'malabar-extend-class)
    ;; (define-key map [?\C-i] 'malabar-implement-interface)

    (define-key map [?i] 'semantic-ia-describe-class)
    ;; (define-key map [?h] 'malabar-semantic-heirarchy)
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
  :keymap malabar-mode-map)

(provide 'malabar-mode)

;;(setq project-info (malabar-project-info "~/projects/malabar-mode-jar/pom.xml"))

