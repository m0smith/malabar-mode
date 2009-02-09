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

(defvar malabar-groovy-comint-name "Malabar Groovy")

(defvar malabar-groovy-buffer-name
  (concat "*" malabar-groovy-comint-name "*"))

(defvar malabar-groovy-command "groovysh")

(defvar malabar-groovy-options '("--color=false"))

(defvar malabar-groovy-lib-dir "~/malabar/lib")

(defvar malabar-groovy-extra-classpath '("~/src/malabar/target/classes"))

(defvar malabar-groovy-mode-hook '())

(defvar malabar-groovy-prompt-regexp "^groovy:[^>]*> ")

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
    (set-buffer (apply #'make-comint
                       malabar-groovy-comint-name
                       malabar-groovy-command
                       nil
                       "-cp"
                       (mapconcat #'expand-file-name
                                  (append malabar-groovy-classpath
                                          (directory-files malabar-groovy-lib-dir t
                                                           ".*\\.jar$"))
                                  path-separator)
                       malabar-groovy-options))
    (malabar-groovy-mode)
    (malabar-groovy-eval "import org.grumblesmurf.malabar.*"))
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
    (rplacd cell (substring output (match-end 0) (1- (length output))))
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

(defun string-ends-with (string end)
  (string= (substring string (- (length string) (length end))) end))
    
(provide 'malabar-groovy)
