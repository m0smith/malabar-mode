;;; malabar-mode.el --- A better Java mode for Emacs
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
(require 'semantic-load)
(require 'semantic-ctxt)
(require 'semantic-find)
(require 'semantic-wisent)

;; HACK: we don't want to load the old Java parser, so trick Emacs
;; into thinking it's already loaded
(provide 'wisent-java-wy)
(require 'wisent-java)

(require 'wisent-malabar-java-wy)
(require 'cl)
(require 'thingatpt)
(require 'compile)

(require 'malabar-abbrevs)
(require 'malabar-annotations)
(require 'malabar-codegen)
(require 'malabar-groovy)
(require 'malabar-import)
(require 'malabar-misc)
(require 'malabar-project)
(require 'malabar-reflection)
(require 'malabar-test)
(require 'malabar-util)
(require 'malabar-variables)
(require 'malabar-semanticdb)

(define-derived-mode malabar-mode java-mode "malabar"
  "A new, better, Java mode."
  ;; HACK: Since we're not loading the old java parser the installer
  ;; function isn't defined; give it a dummy definition
  (flet ((wisent-java-wy--install-parser () nil))
    (wisent-java-default-setup))
  (add-hook 'semantic-init-hooks #'malabar-semantic-setup)
  (setq semantic-lex-depth 10)
  (setq semantic-lex-analyzer 'wisent-malabar-java-lexer)
  (wisent-malabar-java-wy--install-parser)
  ;; Set up indentation of Java annotations.
  (malabar-annotations-setup)
  (malabar-abbrevs-setup)
  (malabar-groovy-start t))

(remove-hook 'java-mode-hook 'wisent-java-default-setup)

(defun malabar-semantic-setup ()
  (remove-hook 'semantic-init-hooks 'malabar-semantic-setup)
  (semantic-idle-scheduler-mode 1))

(defun malabar-compile-file ()
  "Compiles the current buffer."
  (interactive)
  (malabar-setup-compilation-buffer)
  (display-buffer malabar-groovy-compilation-buffer-name t)
  (malabar-groovy-eval-as-compilation
   (concat (format "%s.compiler.compile('%s')"
                   (malabar-project (current-buffer))
                   (buffer-file-name (current-buffer))))))

(defun malabar-compute-package-name (&optional buffer)
  (let* ((dir (file-name-directory (buffer-file-name buffer)))
         (source-directories (malabar-project-source-directories
                              (malabar-find-project-file buffer))))
    (replace-regexp-in-string
     "/" "."
     (substring dir (1+ (length
                         (find dir source-directories
                               :test #'(lambda (dir src-dir)
                                         (string-starts-with dir src-dir)))))
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
  (interactive)
  (c-forward-single-comment)
  (unless (bolp)
    (forward-line 1)))

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
                             ;; See this production in wisent-malabar-java.wy.
                             'block_statements
                             nil t)))
          (dolist (block these-blocks)
            (when (semantic-tag-type-members block)
              (push (remove* 'variable
                             (semantic-tag-type-members block)
                             :test-not #'eql
                             :key #'semantic-tag-class)
                    result))))))
    ;; Add this and super
    (push (list (semantic-tag-new-variable "this" (semantic-tag-name (semantic-current-tag-of-class 'type)))
                (semantic-tag-new-variable "super" (semantic-tag-type-superclasses (semantic-current-tag-of-class 'type))))
          result)
    (apply 'append result)))

(provide 'malabar-mode)
