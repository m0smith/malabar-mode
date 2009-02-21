;;; malabar-groovy.el --- A better Java mode for Emacs
;;
;; Copyright (c) 2009 Espen Wiborg <espenhw@grumblesmurf.org>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.
;;
(require 'comint)
(require 'ansi-color)
(require 'working)

(defvar malabar-groovy-comint-name "Malabar Groovy")

(defvar malabar-groovy-buffer-name
  (concat "*" malabar-groovy-comint-name "*"))

(defvar malabar-groovy-compilation-buffer-name
  (concat "*" malabar-groovy-comint-name " Compilation*"))

(defvar malabar-groovy-command "groovysh")

(defvar malabar-groovy-options '("--color=false"))

(defvar malabar-groovy-lib-dir "~/malabar/lib")

(defvar malabar-groovy-extra-classpath '("~/src/malabar/target/classes"))

(defvar malabar-groovy-mode-hook '())

(defvar malabar-groovy-prompt-regexp "^groovy:[^>]*> ")

(defvar malabar-groovy-initial-statements
  '("import org.grumblesmurf.malabar.*"))

(defun malabar-groovy-mode ()
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
  
(defun malabar-groovy-start (&optional silent)
  (interactive)
  (unless (malabar-groovy-live-p)
    (working-status-forms "Starting Groovy...%s" "done"
      (let ((initial-point
             (with-current-buffer (get-buffer-create malabar-groovy-buffer-name)
               (point))))
        (working-dynamic-status nil "starting process")
        (set-buffer (apply #'make-comint
                           malabar-groovy-comint-name
                           malabar-groovy-command
                           nil
                           "-cp"
                           (mapconcat #'expand-file-name
                                      (append malabar-groovy-extra-classpath
                                              (directory-files malabar-groovy-lib-dir t
                                                               ".*\\.jar$"))
                                      path-separator)
                           malabar-groovy-options))
        (malabar-groovy-mode)
        (working-dynamic-status nil "waiting for prompt")
        (while (not (with-current-buffer malabar-groovy-buffer-name
                      (save-excursion
                        (goto-char (point-max))
                        (re-search-backward malabar-groovy-prompt-regexp initial-point t))))
          (accept-process-output (get-buffer-process malabar-groovy-buffer-name)))
        (working-dynamic-status nil "evaluating initial statements")
        (mapc #'malabar-groovy-eval malabar-groovy-initial-statements))))
  (unless silent
    (pop-to-buffer malabar-groovy-buffer-name)))

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
  (unless (malabar-groovy-live-p)
    (malabar-groovy-start t))
  (when (malabar-groovy-live-p)
    (let ((groovy-process (get-buffer-process malabar-groovy-buffer-name)))
      (let ((old-filter (process-filter groovy-process))
            (string (if (string-ends-with string "\n")
                        string
                      (concat string "\n"))))
        (setq malabar-groovy--eval-output (cons "" ""))
        (set-process-filter groovy-process #'malabar-groovy--eval-filter)
        (process-send-string groovy-process string)
        (accept-process-output groovy-process)
        (set-process-filter groovy-process old-filter)
        (rplaca malabar-groovy--eval-output
                (substring (car malabar-groovy--eval-output) (length string)))
        (malabar-groovy--eval-fix-output malabar-groovy--eval-output)))))

(defun malabar-groovy-eval-as-compilation (string)
  (unless (malabar-groovy-live-p)
    (malabar-groovy-start t))
  (when (malabar-groovy-live-p)
    (let ((groovy-process (get-buffer-process malabar-groovy-buffer-name)))
      (let ((old-filter (process-filter groovy-process))
            (string (if (string-ends-with string "\n")
                        string
                      (concat string "\n"))))
        (set-process-filter groovy-process
                            (malabar-groovy--compilation-filter old-filter))
        (process-send-string groovy-process string)))))

(defun malabar-groovy--compilation-filter (old-filter)
  (lexical-let ((old-filter old-filter))
    (lambda (process output)
      (with-current-buffer (get-buffer-create malabar-groovy-compilation-buffer-name)
        (goto-char (point-max))
        (let ((end (string-match malabar-groovy-prompt-regexp output))
              (original-output output))
          (when end
            (setq output (substring output 0 (string-match "^===> " output)))
            (set-process-filter process old-filter))
          (insert output)
          (when end
            (let ((result (substring original-output (match-end 0) (1- end))))
              (message "%s" result)
              (apply #'compilation-handle-exit 'exit
                     (if (equal result "true")
                         (list 0 "finished\n")
                       (list 0 "exited abnormally")))))
          (when compilation-scroll-output
            (set-window-point (get-buffer-window malabar-groovy-compilation-buffer-name)
                              (point-max))))))))

(defun string-ends-with (string end)
  (string= (substring string (- (length string) (length end))) end))
    
(provide 'malabar-groovy)
