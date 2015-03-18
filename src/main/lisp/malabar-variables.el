;;; malabar-variables.el --- Variables for malabar-mode
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


;;;
;;; Variables

;;; Customization
(defgroup malabar nil
  "Modern JVM Integration for GNU Emacs."
  :prefix "malabar-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/m0smith/malabar-mode/"))


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

(defcustom malabar-server-jar-version "2.3.1"
  "The version of the malabar-mode-jar to fetch when starting"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'string)


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



(defun malabar-groovysh-version-dir-> ( &rest files)
  "Expect FILES to be a list of diretories with names like
/a/fine/path/12.4.5.  Return non-nil if they are order largest to
smallest"
  (-all? (lambda (ls) (not (apply 'version-list-< ls)))
	 (-partition-in-steps 2 1
			      (-map 
			       'version-to-list
			       (--mapcat (last (split-string it "[/]")) files)))))

(defun malabar-groovy-groovysh-guess* ()
  (let ((execs '("bin/groovysh" "bin/groovysh.bat"))
	(version-dirs (sort  (directory-files "~/.gvm/groovy" t "[0-9]$") 'malabar-groovysh-version-dir->)))
    (car
     (-filter 'file-executable-p
	      (-table-flat 'expand-file-name execs version-dirs)))))

(defun malabar-groovy-groovysh-guess ()
  "On Windows the ~/.gvm/groovy/current might be a unfollowable symlink."
  (let ((exec "~/.gvm/groovy/current/bin/groovysh"))
    (if (file-executable-p (expand-file-name exec))
	exec
      (or (malabar-groovy-groovysh-guess*) exec))))

(defcustom malabar-groovy-grooysh (malabar-groovy-groovysh-guess)
  "Where to find the groovysh executable"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'string)

(defcustom malabar-groovy-grooysh-debug nil
  "If non-nil, turn on debugging of the groovysh"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'boolean)

(defcustom malabar-groovy-proxy-host ""
  "Proxy host for Groovy/Grape/Ivy to use to find dependencies.   Also see `malabar-groovy-proxy-port'"
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type 'string)

(defcustom malabar-groovy-proxy-port ""	
  "Proxy port for Groovy/Grape/Ivy to use to find dependencies.  Also see `malabar-groovy-proxy-host'"
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


(defcustom malabar-hide-non-local-source-buffers t
  "Whether to hide source buffers loaded from outside the current
project from the buffer list (by prefixing the buffer name with a
space).

A value of t means always hide.
A value of nil means never hide."
  :group 'malabar
  :type '(choice (const :tag "Hide" t)
                 (const :tag "Don't hide" nil)))

(defcustom malabar-extra-source-locations nil
  "List of extra source locations.
Each location may be a directory or a JAR/ZIP file.  Malabar-mode
will look for the source code of a Java class in these locations
if the class is not otherwise resolvable."
  :group 'malabar
  :type '(repeat (file :tag "Path")))

(defcustom malabar-jdk-extra-locations nil
  "List of extra JVM locations.
Each location may be a directory.  Malabar-mode
will look for installed JVMs in these locations."
  :group 'malabar
  :type '(repeat (file :tag "Path")))


(defcustom malabar-load-source-from-sibling-projects t
  "Whether to load source from sibling projects.
Note that this will not work reliably with a 'flat' project
layout."
  :group 'malabar
  :type '(choice (const :tag "Load from siblings" t)
                 (const :tag "Don't load from siblings" nil)))



(defcustom malabar-case-fixed-abbrevs
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
    ("sysout" malabar-abbrevs-sysout)
    ("main" malabar-abbrevs-main)
    ("#Test" malabar-abbrevs-create-test))
  "The list of abbrevs which should be recognized only in the
specified case."
  :group 'malabar
  :package-version '(malabar . "2.0")
  :type '(alist :key-type string :value-type (group (choice string
                                                            function))))

(defcustom malabar-electric-elvis-p t
  "Whether inserting a colon should electrically expand the Elvis
operator ('?:').

See `malabar-electric-colon'."
  :group 'malabar
  :type '(boolean))


(defcustom malabar-known-project-managers '("maven" "gradle")
  "A list of known project managers to pick from for
`malabar-mode-project-manager'.  Adding an entry here does not
magically make it happen.  This is used mostly for pick lists."
  :group 'malabar
  :type '(repeat (string :tag "Project Manager")))

(defvar malabar-compilation-project-file nil)
(defvar malabar-mode-project-dir nil)
(defvar malabar-mode-project-file nil)
(defvar malabar-mode-project-manager nil)
(defvar malabar-mode-project-name nil)
(defvar malabar-mode-project-parser "groovy")
(defvar malabar-mode-project-service-alist nil
  "An alist of PM to a list of:
    ( PORT )" )
(defvar malabar-groovy-compilation-buffer-name nil)

;; 
;; External references
;;

(defvar url-http-end-of-headers)

(provide 'malabar-variables)

;;; malabar-variables ends here
