;;; malabar-mode.el --- A better Java mode for Emacs
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

(require 'semantic-load)
(require 'semantic-ctxt)
(require 'semantic-find)
(require 'wisent-malabar-java-wy)
(require 'cl)
(require 'malabar-groovy)

(define-mode-local-override semantic-get-local-variables
  malabar-mode ()
  "Get local variable declarations from the current context."
  (let (result
        ;; Ignore funny syntax while doing this.
        semantic-unmatched-syntax-hook)
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (let ((these-blocks (semantic-parse-region
                             (point)
                             (save-excursion (semantic-end-of-context) (point))
                             ;; See this production in wisent-java.wy.
                             'block_statements
                             nil t)))
          (dolist (block these-blocks)
            (when (semantic-tag-type-members block)
              (push (remove-if-not (lambda (tag)
                                     (semantic-tag-of-class-p tag 'variable))
                                   (semantic-tag-type-members block))
                    result))))))
    (apply 'append result)))

(define-derived-mode malabar-mode java-mode "malabar"
  "A new, better, Java mode."
  ;; Funky stuff here
  (malabar-semantic-setup)
  )

(defun malabar-semantic-setup ()
  ;; Nasty hardcode
  (setq semantic-lex-depth 10)
  (setq semantic-lex-analyzer 'wisent-malabar-java-lexer)
  (wisent-malabar-java-wy--install-parser)
  (semantic-idle-scheduler-mode 1)
  (remove-hook 'java-mode-hook 'wisent-java-default-setup))

(defun malabar-type-token-candidates ()
  (remove nil (mapcar (lambda (token)
                        (when (eq (car token) 'IDENTIFIER)
                          (buffer-substring-no-properties (cadr token) (cddr token))))
                      (semantic-lex-buffer 1000))))

(defun malabar-type-token-p (token)
  (let ((case-fold-search nil))
    (and (> (length token) 1)
         (some (lambda (re)
                 (string-match (concat "^" re "$") token))
               java-font-lock-extra-types))))

(defun malabar-class-defined-in-current-buffer-p (classname)
  (let ((tags (semantic-find-tags-by-class 'type (current-buffer))))
    (find classname tags
          :key #'semantic-tag-name
          :test #'equal)))

(defun malabar-class-imported-p (classname)
  (let ((tags (semantic-find-tags-by-class 'include (current-buffer))))
    (find classname tags
          :key (lambda (tag)
                 (substring (semantic-tag-name tag)
                            (1+ (position ?. (semantic-tag-name tag) :from-end t))))
          :test #'equal)))

(defun malabar-import-candidates ()
  (let ((type-tokens (remove-if-not #'malabar-type-token-p (malabar-type-token-candidates))))
    (remove-if (lambda (token)
                 (or (malabar-class-defined-in-current-buffer-p token)
                     (malabar-class-imported-p token)))
               type-tokens)))

(provide 'malabar-mode)
