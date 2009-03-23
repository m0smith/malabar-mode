;;; malabar-groovy.el --- A better Java mode for Emacs
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
(require 'comint)
(require 'ansi-color)
(require 'working)
(require 'cl)

(require 'malabar-util)

(defgroup malabar-groovy nil
  "Customization of malabar-mode's inferior Groovy."
  :group 'malabar-mode)

(defvar malabar-groovy-comint-name "Malabar Groovy")

(defvar malabar-groovy-compile-server-comint-name "Malabar Compile Server")

(defvar malabar-groovy-eval-server-comint-name "Malabar Eval Server")

(defvar malabar-groovy-compilation-buffer-name "*Malabar Compilation*")
(get-buffer-create malabar-groovy-compilation-buffer-name)

(defvar malabar-groovy-buffer-name
  (concat "*" malabar-groovy-comint-name "*"))

(defvar malabar-groovy-compile-server-buffer-name
  (concat "*" malabar-groovy-compile-server-comint-name "*"))

(defvar malabar-groovy-eval-server-buffer-name
  (concat "*" malabar-groovy-eval-server-comint-name "*"))

(defcustom malabar-groovy-java-command "java"
  "The command to invoke Java.  Include the full path if
necessary."
  :group 'malabar-groovy
  :type 'string)

(defcustom malabar-groovy-server-class "org.grumblesmurf.malabar.GroovyServer"
  "The class name of the Malabar Groovy server.  Don't touch
unless you know what you're doing."
  :group 'malabar-groovy
  :type 'string)

(defcustom malabar-groovy-lib-dir "~/malabar/lib"
  "The location of all Malabar's JARs."
  :group 'malabar-groovy
  :type 'directory)

(defcustom malabar-groovy-extra-classpath '("~/src/malabar/target/classes")
  "Extra classpath elements to pass to groovysh (mainly useful
for hacking on Malabar itself)."
  :group 'malabar-groovy
  :type '(repeat (choice directory file)))

(defcustom malabar-groovy-mode-hook '()
  "Hook that gets called when entering malabar-groovy-mode."
  :group 'malabar-groovy
  :type 'hook)

(defcustom malabar-groovy-prompt-regexp "^groovy:[^>]*> "
  "Regexp to recognize the groovysh prompt."
  :group 'malabar-groovy
  :type 'regexp)

(defcustom malabar-groovy-initial-statements
  '("import org.grumblesmurf.malabar.*"
    "import java.lang.reflect.*")
  "Statements to execute immediately after starting groovysh."
  :group 'malabar-groovy
  :type '(repeat string))

(defcustom malabar-groovy-compile-server-port 5555
  "The port on which the Groovy compile server should listen."
  :group 'malabar-groovy
  :type 'integer)

(defcustom malabar-groovy-eval-server-port 6666
  "The port on which the Groovy eval server should listen."
  :group 'malabar-groovy
  :type 'integer)

(defcustom malabar-groovy-java-options nil
  "Extra options to pass to Java."
  :group 'malabar-groovy
  :type '(repeat string))

(defun malabar-groovy-mode ()
  "A major mode for the Groovy console."
  (interactive)
  (delay-mode-hooks (comint-mode))
  ;;(ansi-color-for-comint-mode-on)
  ;; Set prompt regexp
  (setq comint-prompt-regexp malabar-groovy-prompt-regexp)
  (setq comint-process-echoes t)
  (setq major-mode 'malabar-groovy-mode)
  (setq mode-name malabar-groovy-comint-name)
  (setq mode-line-process '(":%s"))
  ;; Set keymap?
  ;; set comint-input-filter
  (run-mode-hooks 'malabar-groovy-mode-hook))

(defun malabar-groovy--wait-for-prompt (buffer initial-points-alist)
  (while (not (with-current-buffer buffer
                (save-excursion
                  (goto-char (point-max))
                  (re-search-backward malabar-groovy-prompt-regexp
                                      (cdr (assoc buffer initial-points-alist)) t))))
    (accept-process-output (get-buffer-process buffer))))

(defun malabar-groovy-stop ()
  "Stop the inferior Groovy."
  (interactive)
  (malabar-groovy-eval-in-process (get-buffer-process malabar-groovy-buffer-name)
                                  "exit")
  (message nil))

(defun malabar-groovy-start (&optional silent)
  "Start groovy and wait for it to come up.  If SILENT is NIL,
pop to the Groovy console buffer."
  (interactive)
  (unless (malabar-groovy-live-p)
    (working-status-forms "Starting Groovy...%s" "done"
      (let ((initial-points-alist (mapcar (lambda (b)
                                            (with-current-buffer (get-buffer-create b)
                                              (cons b (point))))
                                          (list malabar-groovy-buffer-name
                                                malabar-groovy-compile-server-buffer-name
                                                malabar-groovy-eval-server-buffer-name))))
        (working-dynamic-status nil "starting process")
        (with-current-buffer (get-buffer malabar-groovy-buffer-name)
          (unless silent
            (display-buffer malabar-groovy-buffer-name))
          (apply #'make-comint
                 malabar-groovy-comint-name
                 malabar-groovy-java-command
                 nil
                 "-cp"
                 (mapconcat #'expand-file-name
                            (append malabar-groovy-extra-classpath
                                    (directory-files malabar-groovy-lib-dir t
                                                     ".*\\.jar$"))
                            path-separator)
                 (append malabar-groovy-java-options
                         (list malabar-groovy-server-class
                               "-c" (number-to-string malabar-groovy-compile-server-port)
                               "-e" (number-to-string malabar-groovy-eval-server-port))))
          (malabar-groovy-mode))
        (working-dynamic-status nil "waiting for main prompt")
        (malabar-groovy--wait-for-prompt malabar-groovy-buffer-name initial-points-alist)
        (working-dynamic-status nil "connecting to servers")
        (make-comint malabar-groovy-compile-server-comint-name
                     (cons "localhost"
                           (number-to-string malabar-groovy-compile-server-port)))
        (make-comint malabar-groovy-eval-server-comint-name
                     (cons "localhost"
                           (number-to-string malabar-groovy-eval-server-port)))
        (working-dynamic-status nil "waiting for server prompts")
        (malabar-groovy--wait-for-prompt malabar-groovy-compile-server-buffer-name
                                         initial-points-alist)
        (malabar-groovy--wait-for-prompt malabar-groovy-eval-server-buffer-name
                                         initial-points-alist)
        (working-dynamic-status nil "evaluating initial statements")
        (dolist (process (list (get-buffer-process malabar-groovy-compile-server-buffer-name)
                               (get-buffer-process malabar-groovy-eval-server-buffer-name)
                               (get-buffer-process malabar-groovy-buffer-name)))
          (dolist (stmt malabar-groovy-initial-statements)
            (malabar-groovy-eval-in-process process stmt)))
        (with-current-buffer malabar-groovy-compile-server-buffer-name
          (malabar-groovy--init-compile-server-buffer))
        (with-current-buffer malabar-groovy-eval-server-buffer-name
          (malabar-groovy--init-eval-buffer)))))
  (unless silent
    (pop-to-buffer malabar-groovy-buffer-name)))

(defun malabar-groovy-eval-in-process (process string)
  (let ((string (string-with-newline string)))
    (unless working-message
      (message "Invoking Groovy, please wait..."))
    (comint-send-string process string)))

(defun malabar-groovy-live-p ()
  (comint-check-proc malabar-groovy-buffer-name))

(defvar malabar-groovy--eval-output (cons "" ""))

(defvar malabar-groovy--eval-buffer (get-buffer-create " *Malabar Groovy eval*"))

(defvar malabar-groovy--eval-callback nil)

(defun malabar-groovy--init-compile-server-buffer ()
  (malabar-groovy-mode)
  (add-hook 'comint-redirect-hook
            'malabar-groovy--compile-handle-exit
            nil t))
  
(defun malabar-groovy--init-eval-buffer ()
  (malabar-groovy-mode)
  (when (assq 'comint-output-filter-functions (buffer-local-variables))
    ;; HACK: There is no reliable way to remove a closure from this
    ;; list; just clear it, but only if it is already buffer-local
    (setq comint-output-filter-functions nil))
  (add-hook 'comint-output-filter-functions
            (malabar-groovy--watch-for-prompt 'malabar-groovy--eval-callback
                                              (current-buffer))
            nil t))

(defun malabar-groovy--watch-for-prompt (hook buffer)
  (lexical-let ((hook hook)
                (buffer buffer))
    (lambda (string)
      (when (string-match malabar-groovy-prompt-regexp string)
        (run-hook-with-args hook buffer)
        (set hook nil)))))

(defun malabar-groovy--eval-get-output (buffer)
  (setq malabar-groovy--eval-output
        (with-current-buffer buffer
          (cons (buffer-substring-no-properties
                 (save-excursion
                   (goto-char (point-max))
                   (re-search-backward malabar-groovy-prompt-regexp nil nil 2))
                 (point-max))
                ""))))

(defun malabar-groovy--eval-fix-output (cell)
  (let* ((string (car cell))
         (output (substring string
                            (1+ (position ?\n string))
                            (1+ (position ?\n string :from-end t))))
         (start-of-return (string-match "\n?===> " output)))
    (cons (substring output 0 start-of-return)
          (when start-of-return
            (substring output (match-end 0) (1- (length output)))))))

(defun malabar-groovy-eval (string)
  "Pass STRING to groovysh for evaluation."
  (unless (malabar-groovy-live-p)
    (malabar-groovy-start t))
  (when (malabar-groovy-live-p)
    (let ((groovy-process (get-buffer-process malabar-groovy-eval-server-buffer-name)))
      (setq malabar-groovy--eval-callback 'malabar-groovy--eval-get-output) 
      (malabar-groovy-eval-in-process groovy-process string)
      (while (not (string-match-p (regexp-quote string)
                                  (car malabar-groovy--eval-output)))
        (setq malabar-groovy--eval-callback 'malabar-groovy--eval-get-output) 
        (accept-process-output groovy-process))
      (malabar-groovy--eval-fix-output malabar-groovy--eval-output))))

(defun malabar-groovy-eval-and-lispeval (string)
  "Pass STRING to groovysh for evaluation, and read the output for Lisp use."
  (car (read-from-string (car (malabar-groovy-eval string)))))

(defun malabar-groovy-setup-compilation-buffer ()
  (with-current-buffer (get-buffer-create malabar-groovy-compilation-buffer-name)
    (setq buffer-read-only nil)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (buffer-enable-undo (current-buffer))
    (compilation-mode)
    (setq buffer-read-only nil)))

(defvar malabar-groovy--compilation-backlog nil)

(defun malabar-groovy-eval-as-compilation (string)
  "Passes STRING to groovysh for evaluation in the compile server."
  (unless (malabar-groovy-live-p)
    (malabar-groovy-start t))
  (when (malabar-groovy-live-p)
    (let* ((groovy-process (get-buffer-process malabar-groovy-compile-server-buffer-name))
           (thunk (lexical-let ((string string)
                                (groovy-process groovy-process))
                    (lambda ()
                      (setq compilation-in-progress
                            (cons groovy-process compilation-in-progress))
                      (comint-redirect-send-command-to-process
                       string malabar-groovy-compilation-buffer-name
                       groovy-process nil)))))
      (if (memq groovy-process compilation-in-progress)
          (add-to-list 'malabar-groovy--compilation-backlog thunk t)
        (funcall thunk)))))

(defun malabar-groovy--process-backlog (buffer message)
  (message "%s" message)
  (when (and (equal (buffer-name buffer) malabar-groovy-compilation-buffer-name)
             malabar-groovy--compilation-backlog)
    (if (equal message "finished\n")
        (funcall (pop malabar-groovy--compilation-backlog))
      (message "Compilation failed, clearing backlog")
      (setq malabar-groovy--compilation-backlog nil))))

(add-hook 'compilation-finish-functions 'malabar-groovy--process-backlog)

(defun malabar-groovy--compile-handle-exit ()
  (with-current-buffer malabar-groovy-compilation-buffer-name
    (let ((result (progn (goto-char (point-max))
                         (re-search-backward "===> \\(.*\\)$")
                         (match-string-no-properties 1))))
      (replace-match "" t t)
      (setq compilation-in-progress
            (delq (get-buffer-process malabar-groovy-compile-server-buffer-name)
                  compilation-in-progress))
      (apply #'compilation-handle-exit 'exit
             (if (equal result "true")
                 (list 0 "finished\n")
               (list 1 "exited abnormally"))))))

(provide 'malabar-groovy)
