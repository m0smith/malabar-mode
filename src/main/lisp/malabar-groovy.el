;;; malabar-groovy.el --- A better Java mode for Emacs
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
(require 'comint)
(require 'ansi-color)
(require 'cl)

(require 'malabar-util)
(require 'fringe-helper)

(defgroup malabar-groovy nil
  "Customization of malabar-mode's inferior Groovy."
  :group 'malabar-mode)

(defvar malabar-groovy-comint-name "Malabar Groovy")

(defvar malabar-groovy-compile-server-comint-name "Malabar Compile Server")

(defvar malabar-groovy-eval-server-comint-name "Malabar Eval Server")

(defvar malabar-groovy-compilation-buffer-name "*Malabar Compilation*")
(get-buffer-create malabar-groovy-compilation-buffer-name)

(defvar malabar-groovy-time-out-waiting-for-groovysh 10)

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

(defcustom malabar-groovy-compile-server-port 0
  "The port on which the Groovy compile server should listen.
By default this variable has a value of 0, which means the system will
pick an available port, whose number will be stored back in this
variable once the compile server has started."
  :group 'malabar-groovy
  :type 'integer)

(defcustom malabar-groovy-eval-server-port 0
  "The port on which the Groovy eval server should listen.
By default this variable has a value of 0, which means the system will
pick an available port, whose number will be stored back in this
variable once the eval server has started."
  :group 'malabar-groovy
  :type 'integer)

(defcustom malabar-groovy-java-options nil
  "Extra options to pass to Java."
  :group 'malabar-groovy
  :type '(repeat string))

(defvar malabar-groovy--last-compilation-command nil)

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

(defun malabar-groovy--check-groovysh-time-out (start-time time-out-msg)
  (when (> (- (float-time) start-time) malabar-groovy-time-out-waiting-for-groovysh)
        (error time-out-msg)))

(defun malabar-groovy--match-buffer (buffer regexp count initial-points-alist time-out-msg)
  (let ((start-time (float-time)))
    (while (not (with-current-buffer buffer
                  (save-excursion
                    (goto-char (point-max))
                    (re-search-backward regexp
                                        (cdr (assoc buffer initial-points-alist)) t))))
      (accept-process-output (get-buffer-process buffer) malabar-groovy-time-out-waiting-for-groovysh)
      (malabar-groovy--check-groovysh-time-out start-time time-out-msg)))
  (with-current-buffer buffer (match-string-no-properties count)))

(defun malabar-groovy--wait-for-prompt (buffer initial-points-alist)
  (malabar-groovy--match-buffer buffer malabar-groovy-prompt-regexp 0 initial-points-alist
                                "Error starting groovy: time-out waiting for prompt"))

(defun malabar-groovy--get-server-ports-from-buffer (buffer initial-points-alist)
  (let ((time-out-msg "Error starting groovy: time-out waiting for port"))
    (setq malabar-groovy-compile-server-port
          (string-to-number
           (malabar-groovy--match-buffer buffer "compile-server: port=\\([0-9]*\\)" 1 initial-points-alist time-out-msg)))
    (setq malabar-groovy-eval-server-port
          (string-to-number
           (malabar-groovy--match-buffer buffer "eval-server: port=\\([0-9]*\\)" 1 initial-points-alist time-out-msg)))))

(defun malabar-groovy-stop ()
  "Stop the inferior Groovy."
  (interactive)
  (let ((start-time (float-time)) (groovy-process (get-buffer-process malabar-groovy-buffer-name)))  
    (malabar-groovy-eval-in-process groovy-process "exit")
    (while (malabar-groovy-live-p)
      (sit-for 0.1)
      (malabar-groovy--check-groovysh-time-out start-time "Time-out waiting for Groovy to stop.")))
  (message nil))

(defun malabar-groovy-start (&optional silent)
  "Start Groovy and wait for it to come up.  If SILENT is NIL,
pop to the Groovy console buffer."
  (interactive)
  (unless (malabar-groovy-live-p)
    (let ((reporter (make-progress-reporter "Starting Groovy... " 0 7)))
      (let ((initial-points-alist (mapcar (lambda (b)
                                            (with-current-buffer (get-buffer-create b)
                                              (cons b (point))))
                                          (list malabar-groovy-buffer-name
                                                malabar-groovy-compile-server-buffer-name
                                                malabar-groovy-eval-server-buffer-name))))
        (progress-reporter-force-update reporter 1 "Starting Groovy...starting process ")
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
        (progress-reporter-force-update reporter 2 "Starting Groovy...requesting ports ")
        (malabar-groovy--get-server-ports-from-buffer malabar-groovy-buffer-name initial-points-alist)
        (progress-reporter-force-update reporter 3 "Starting Groovy...waiting for main prompt ")
        (malabar-groovy--wait-for-prompt malabar-groovy-buffer-name initial-points-alist)
        (progress-reporter-force-update reporter 4 "Starting Groovy...connecting to servers ")
        (make-comint malabar-groovy-compile-server-comint-name
                     (cons "localhost"
                           (number-to-string malabar-groovy-compile-server-port)))
        (make-comint malabar-groovy-eval-server-comint-name
                     (cons "localhost"
                           (number-to-string malabar-groovy-eval-server-port)))
        (progress-reporter-force-update reporter 5 "Starting Groovy...waiting for server prompts ")
        (malabar-groovy--wait-for-prompt malabar-groovy-compile-server-buffer-name
                                         initial-points-alist)
        (malabar-groovy--wait-for-prompt malabar-groovy-eval-server-buffer-name
                                         initial-points-alist)
        (progress-reporter-force-update reporter 6 "Starting Groovy...evaluating initial statements ")
        (dolist (process (list (get-buffer-process malabar-groovy-compile-server-buffer-name)
                               (get-buffer-process malabar-groovy-eval-server-buffer-name)
                               (get-buffer-process malabar-groovy-buffer-name)))
          (dolist (stmt malabar-groovy-initial-statements)
            (malabar-groovy-eval-in-process process stmt)))
        (with-current-buffer malabar-groovy-compile-server-buffer-name
          (malabar-groovy--init-compile-server-buffer))
        (with-current-buffer malabar-groovy-eval-server-buffer-name
          (malabar-groovy--init-eval-buffer))
        (progress-reporter-force-update reporter 7 "Starting Groovy...")
        (progress-reporter-done reporter))))
  (unless silent
    (pop-to-buffer malabar-groovy-buffer-name)))

(defun malabar-groovy-restart ()
  "Stops Groovy (if running) and (re)starts Groovy and waits
for it to come up."
  (interactive)
  (when (malabar-groovy-live-p) (malabar-groovy-stop))
  (malabar-groovy-start))

(defun malabar-groovy-eval-in-process (process string)
  (let ((string (string-with-newline string))
        (current-message (current-message)))
    (unless current-message
      (message "Invoking Groovy, please wait..."))
    (comint-send-string process string)
    (if current-message
      (message "%s" current-message)
      (message nil))))

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

(defconst malabar-groovy--eval-log-output-marker-re
  (concat "^" (regexp-opt (mapcar (lambda (level)
                                    (concat "[" (symbol-name level) "]"))
                                  '(DEBUG INFO WARN ERROR FATAL)))))

(defun malabar-groovy--eval-fix-output (cell)
  (let* ((string (car cell))
         (output (mapconcat
                  'identity
                  (cdr      ;; Lose first...
                   (butlast ;; ...and last lines
                    (remove-if (lambda (s)
                                 (string-match-p malabar-groovy--eval-log-output-marker-re
                                                 s))
                               (split-string (car cell) "\n"))))
                  "\n"))
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

(defcustom malabar-groovy-compilation-font-lock-keywords
  '((malabar-groovy-highlight-compilation-message
     (1 '(face nil invisible t) nil t) ;
     (2 '(face nil invisible t) nil t) ; hide the class
     (3 '(face nil invisible t) nil t) ;
     (4 (compilation-face '(2 . 3)))
     (5 compilation-line-face nil t)
     (6 compilation-column-face nil t)
     (7 '(face nil invisible t) nil t) ; hide the position info
     (0 (compilation-error-properties 4 5 nil 6 nil '(2 . 3) nil)
        append)))
  "Font lock keywords for Malabar compilation."
  :group 'malabar-mode
  :type '(alist))

(defun malabar-groovy-setup-compilation-buffer (&optional for-files)
  (with-current-buffer (get-buffer-create malabar-groovy-compilation-buffer-name)
    (setq buffer-read-only nil)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (buffer-enable-undo (current-buffer))
    (malabar-groovy-reset-compiler-notes for-files)
    (setq mode-name "Compilation")
    (malabar-compilation-minor-mode t)
    (when for-files
      ;; Compiling a single file (or set of files), do magic
      ;; We do error message parsing slightly differently
      (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
      (font-lock-add-keywords nil
                              (append
                               malabar-groovy-compilation-font-lock-keywords
                               compilation-mode-font-lock-keywords)
                              'set))
    (setq buffer-read-only nil)))

(defvar malabar-compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'malabar-groovy-rerun-last-compilation)
    map))

(define-minor-mode malabar-compilation-minor-mode
  nil nil nil malabar-compilation-minor-mode-map
  (compilation-minor-mode malabar-compilation-minor-mode))

(defun malabar-groovy-rerun-last-compilation ()
  (interactive)
  (malabar-groovy-setup-compilation-buffer)
  (malabar-groovy-eval-as-compilation malabar-groovy--last-compilation-command))

(defvar malabar-groovy--compiler-notes nil)

(defun malabar-groovy-reset-compiler-notes (&optional files)
  "Remove all compiler notes from the given files, or all files if nil."
  (interactive)
  (let ((files-to-process (or files
                              (delete-duplicates
                               (mapcar (lambda (n)
                                         (getf n :file))
                                       malabar-groovy--compiler-notes)))))
    (mapc (lambda (f)
            (when-let (buf (get-file-buffer f))
              (remove-overlays
               (with-current-buffer buf
                 (mapc 'fringe-helper-remove malabar-groovy--fringe-overlays)
                 (remove-overlays (point-min) (point-max)
                                  'malabar-compiler-note t)))))
          files-to-process)
    (setq malabar-groovy--compiler-notes
          (remove-if (lambda (f)
                       (member f files-to-process))
                     malabar-groovy--compiler-notes
                     :key (lambda (n)
                            (getf n :file))))))

(defun malabar-groovy-highlight-compilation-message (limit)
  ;; CLASS::FILE::LINE::COLUMN::START::END::POS::Message
  ;; CLASS is either ERROR, MANDATORY_WARNING, WARNING, NOTE or OTHER
  (when (re-search-forward "^\\(ERROR::\\)?\\(\\(?:MANDATORY_\\)?WARNING::\\)?\\(NOTE::\\|OTHER::\\)?\\(.*?\\)::\\(.*?\\)::\\(.*?\\)\\(::.*?::.*?::.*?\\)::" limit 'move)
    (when malabar-compilation-project-file
      (let ((file-start (match-beginning 4)))
        (put-text-property file-start
                           (+ file-start
                              (length (file-name-directory
                                       malabar-compilation-project-file)))
                           'invisible t)))
    ;; Hide some colons
    (mapc (lambda (n)
            (put-text-property (match-end n) (1+ (match-end n)) 'invisible t))
          '(4 5 6 7))
    (pushnew (list :class (cond ((match-beginning 1)
                                 'error)
                                ((match-beginning 2)
                                 'warning)
                                ((match-beginning 3)
                                 'info))
                   :file (match-string-no-properties 4)
                   :message
                   (buffer-substring-no-properties (match-end 0)
                                                   (save-excursion
                                                     (end-of-line)
                                                     (point)))
                   :position-info
                   (let ((positions (match-string-no-properties 7)))
                     (mapcar #'1+
                             (car
                              (read-from-string
                               (concat "("
                                       (replace-regexp-in-string "::" " " positions)
                                       ")"))))))
             malabar-groovy--compiler-notes
             :test #'equal)))

(defvar malabar-groovy--compilation-backlog nil)

(defun malabar-groovy-eval-as-compilation (string &optional silent)
  "Passes STRING to groovysh for evaluation in the compile server."
  (unless (malabar-groovy-live-p)
    (malabar-groovy-start t))
  (when (malabar-groovy-live-p)
    (let* ((groovy-process (get-buffer-process malabar-groovy-compile-server-buffer-name))
           (thunk (lexical-let ((string string)
                                (groovy-process groovy-process)
                                (silent silent))
                    (lambda ()
                      (setq compilation-in-progress
                            (cons groovy-process compilation-in-progress))
                      (setq malabar-groovy--last-compilation-command string)
                      (comint-redirect-send-command-to-process
                       string malabar-groovy-compilation-buffer-name
                       groovy-process nil silent)))))
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
               (list 1 "exited abnormally"))))
    (mapcar #'malabar-groovy--add-compiler-annotation
            malabar-groovy--compiler-notes)))

(defface malabar-error-face '((t (:underline "red")))
  "Face used in code buffer for error annotations.")

(defface malabar-warning-face '((t (:underline "orange")))
  "Face used in code buffer for warning annotations.")

(defface malabar-info-face '((t (:underline "blue")))
  "Face used in code buffer for info annotations.")

(defvar malabar-groovy--fringe-overlays nil)
(make-variable-buffer-local 'malabar-groovy--fringe-overlays)

(defun malabar-groovy--add-compiler-annotation (compiler-note)
  (let ((file (getf compiler-note :file)))
    (when (file-exists-p file)
      (save-excursion
        (with-current-buffer (find-file-noselect file)
          (let* ((modified (buffer-modified-p))
                 (buffer-undo-list t)
                 (position-info (getf compiler-note :position-info))
                 (beg (first position-info))
                 (end (second position-info))
                 (class (getf compiler-note :class)))
            (let ((overlay (make-overlay beg end nil nil t)))
              (overlay-put overlay 'malabar-compiler-note t)
              (overlay-put overlay 'face (case class
                                           (error 'malabar-error-face)
                                           (warning 'malabar-warning-face)
                                           (info 'malabar-info-face)))
              (overlay-put overlay 'help-echo (getf compiler-note :message)))
            (push (fringe-helper-insert-region
                   beg end (fringe-lib-load (case class
                                              (error fringe-lib-exclamation-mark)
                                              (warning fringe-lib-question-mark)
                                              (info fringe-lib-wave)))
                   'left-fringe (when (eq class 'error) 'font-lock-warning-face))
                  malabar-groovy--fringe-overlays)
            (set-buffer-modified-p modified)))))))

(provide 'malabar-groovy)
