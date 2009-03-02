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

(require 'malabar-util)

(defvar malabar-groovy-comint-name "Malabar Groovy")

(defvar malabar-groovy-compiler-comint-name "Malabar Compile Server")

(defvar malabar-groovy-evaluator-comint-name "Malabar Eval Server")

(defvar malabar-groovy-buffer-name
  (concat "*" malabar-groovy-comint-name "*"))

(defvar malabar-groovy-compiler-buffer-name
  (concat "*" malabar-groovy-compiler-comint-name "*"))

(defvar malabar-groovy-evaluator-buffer-name
  (concat "*" malabar-groovy-evaluator-comint-name "*"))

(defvar malabar-groovy-java-command "java"
  "The command to invoke Java.  Include the full path if
necessary.")

(defvar malabar-groovy-server-class "org.grumblesmurf.malabar.GroovyServer"
  "The class name of the Malabar Groovy server.  Don't touch
unless you know what you're doing.")

(defvar malabar-groovy-lib-dir "~/malabar/lib"
  "The location of all Malabar's JARs.")

(defvar malabar-groovy-extra-classpath '("~/src/malabar/target/classes")
  "Extra classpath elements to pass to groovysh (mainly useful
for hacking on Malabar itself).")

(defvar malabar-groovy-mode-hook '()
  "Hook that gets called when entering malabar-groovy-mode.")

(defvar malabar-groovy-prompt-regexp "^groovy:[^>]*> "
  "Regexp to recognize the groovysh prompt.")

(defvar malabar-groovy-initial-statements
  '("import org.grumblesmurf.malabar.*"
    "import java.lang.reflect.*")
  "Statements to execute immediately after starting groovysh.")

(defvar malabar-groovy-compiler-port 5555
  "The port on which the Groovy compile server should listen.")

(defvar malabar-groovy-evaluator-port 6666
  "The port on which the Groovy eval server should listen.")

(defvar malabar-groovy-java-options nil
  "Extra options to pass to Java.")

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

(defvar malabar-groovy-comint-filter nil)

(defun malabar-groovy-start (&optional silent)
  "Start groovy and wait for it to come up.  If SILENT is NIL,
pop to the Groovy console buffer."
  (interactive)
  (unless (malabar-groovy-live-p)
    (working-status-forms "Starting Groovy...%s" "done"
      (let ((initial-point
             (with-current-buffer (get-buffer-create malabar-groovy-buffer-name)
               (point))))
        (working-dynamic-status nil "starting process")
        (set-buffer (get-buffer malabar-groovy-buffer-name))
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
                             "-c" (number-to-string malabar-groovy-compiler-port)
                             "-e" (number-to-string malabar-groovy-eval-port))))
        (unless silent
          (pop-to-buffer malabar-groovy-buffer-name))
        (malabar-groovy-mode)
        (working-dynamic-status nil "waiting for prompt")
        (while (not (with-current-buffer malabar-groovy-buffer-name
                      (save-excursion
                        (goto-char (point-max))
                        (re-search-backward malabar-groovy-prompt-regexp initial-point t))))
          (accept-process-output (get-buffer-process malabar-groovy-buffer-name)))
        (setq malabar-groovy-comint-filter
              (process-filter (get-buffer-process malabar-groovy-buffer-name)))
        (working-dynamic-status nil "evaluating initial statements")
        (make-comint malabar-groovy-compiler-comint-name
                     (cons "localhost" (number-to-string malabar-groovy-compiler-port)))
        (make-comint malabar-groovy-evaluator-comint-name
                     (cons "localhost" (number-to-string malabar-groovy-evaluator-port)))
        (dolist (process (list (get-buffer-process malabar-groovy-compiler-buffer-name)
                               (get-buffer-process malabar-groovy-evaluator-buffer-name)
                               (get-buffer-process malabar-groovy-buffer-name)))
          (dolist (stmt malabar-groovy-initial-statements)
            (malabar-groovy-eval-in-process process stmt)))
        (mapc #'malabar-groovy-eval malabar-groovy-initial-statements)))))

(defun malabar-groovy-eval-in-process (process string)
  (let ((string (if (string-ends-with string "\n")
                    string
                  (concat string "\n"))))
    (comint-send-string process string)))

(defun malabar-groovy-live-p ()
  (comint-check-proc malabar-groovy-buffer-name))

(defvar malabar-groovy--eval-output (cons "" ""))

(defvar malabar-groovy--eval-buffer (get-buffer-create " *Malabar Groovy eval*"))

(defun malabar-groovy--eval-filter (process output)
  (let ((end-of-return (string-match malabar-groovy-prompt-regexp output)))
    (rplaca malabar-groovy--eval-output
            (concat (car malabar-groovy--eval-output)
                    (substring output 0 end-of-return)))
    (unless end-of-return
      (accept-process-output process))))

(defun malabar-groovy--eval-fix-output (cell)
  (let* ((output (car cell))
         (start-of-return (string-match "===> " output)))
    (rplaca cell (substring output 0 start-of-return))
    (when start-of-return
      (rplacd cell (substring output (match-end 0) (1- (length output)))))
    cell))

(defun malabar-groovy-eval (string)
  "Pass STRING to groovysh for evaluation."
  (unless (malabar-groovy-live-p)
    (malabar-groovy-start t))
  (when (malabar-groovy-live-p)
    (let ((groovy-process (get-buffer-process malabar-groovy-buffer-name)))
      (let ((string (if (string-ends-with string "\n")
                        string
                      (concat string "\n"))))
        (setq malabar-groovy--eval-output (cons "" ""))
        (set-process-filter groovy-process #'malabar-groovy--eval-filter)
        (process-send-string groovy-process string)
        (accept-process-output groovy-process)
        (set-process-filter groovy-process malabar-groovy-comint-filter)
        (rplaca malabar-groovy--eval-output
                (substring (car malabar-groovy--eval-output) (length string)))
        (malabar-groovy--eval-fix-output malabar-groovy--eval-output)))))

(defun malabar-groovy-eval-and-lispeval (string)
  "Pass STRING to groovysh for evaluation, and read the output for Lisp use."
  (car (read-from-string (car (malabar-groovy-eval string)))))

(defun malabar-groovy-eval-as-compilation (string)
  "Passes STRING to groovysh for evaluation in a buffer in
`compilation-mode'."
  (unless (malabar-groovy-live-p)
    (malabar-groovy-start t))
  (when (malabar-groovy-live-p)
    (let ((groovy-process (get-buffer-process malabar-groovy-buffer-name)))
      (let ((string (if (string-ends-with string "\n")
                        string
                      (concat string "\n"))))
        (set-process-filter groovy-process
                            (malabar-groovy--compilation-filter malabar-groovy-comint-filter))
        (process-send-string groovy-process string)))))

(defun malabar-groovy--compilation-filter (old-filter)
  (lexical-let ((old-filter old-filter))
    (lambda (process output)
      (with-current-buffer (get-buffer-create malabar-groovy-compilation-buffer-name)
        (goto-char (point-max))
        (let ((end (string-match malabar-groovy-prompt-regexp output))
              (original-output output))
          (when end
            (setq output (substring output 0 end))
            (set-process-filter process old-filter))
          (insert output)
          (when end
            (let ((result (progn (goto-char (point-max))
                                 (re-search-backward "^===> \\(.*\\)$")
                                 (match-string-no-properties 1))))
              (message "%s" result)
              (apply #'compilation-handle-exit 'exit
                     (if (equal result "true")
                         (list 0 "finished\n")
                       (list 0 "exited abnormally")))))
          (when compilation-scroll-output
            (set-window-point (get-buffer-window malabar-groovy-compilation-buffer-name)
                              (point-max))))))))

(provide 'malabar-groovy)
