;;; malabar-mode.el --- A better Java mode for Emacs
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

;; Only external CEDET defines cedet-emacs-min-version
(defvar malabar-use-external-cedet (boundp 'cedet-emacs-min-version)
"Whether or not to use the external version of CEDET.")

(cond (malabar-use-external-cedet
       (require 'semantic-load)
       (require 'semantic-ctxt)
       (require 'semantic-find)
       (require 'semantic-wisent)
       (require 'semantic-ia)
       ;; HACK: we don't want to load the old Java parser, so trick Emacs
       ;; into thinking it's already loaded
       (provide 'wisent-java-wy)
       (require 'wisent-java))
      (t
       (require 'semantic)
       (require 'semantic/ctxt)
       (require 'semantic/find)
       (require 'semantic/wisent)
       (require 'semantic/ia)
       ;; HACK: we don't want to load the old Java parser, so trick Emacs
       ;; into thinking it's already loaded
       (provide 'semantic/wisent/javat-wy)
       (require 'semantic/wisent/java-tags)
       ;; Compatibility hack
       (require 'semantic/wisent/comp)
       (provide 'wisent-comp)
       (require 'semantic/lex)
       (require 'semantic/wisent/javat-wy)
       (provide 'semantic-lex)))

(require 'wisent-malabar-java-wy)
(require 'cl)
(require 'thingatpt)
(require 'compile)
(require 'easymenu)

(require 'srecode)

(require 'malabar-variables)
(require 'malabar-abbrevs)
(require 'malabar-annotations)
(require 'malabar-codegen)
(require 'malabar-groovy)
(require 'malabar-import)
(require 'malabar-jump-to-project)
(require 'malabar-misc)
(require 'malabar-project)
(require 'malabar-reflection)
(require 'malabar-test)
(require 'malabar-util)
(require 'malabar-semanticdb)
(require 'malabar-refactor)

(easy-menu-define malabar-mode-menu malabar-mode-map
  "Menu for Malabar mode"
  '("Malabar"
    ("Build"
     ["Install project" malabar-install-project]
     ["Compile file" malabar-compile-file])
    ("Test"
     ["Run test" malabar-run-test]
     ["Run JUnit test" malabar-run-junit-test]
     ["Run all tests" malabar-run-all-tests])
    ("Source"
     ["Insert getter/setter" malabar-insert-getset]
     ["Import class" malabar-import-one-class]
     ["Import all classes" malaber-import-all]
     ["Override method" malabar-override-method]
     ["Extend class" malabar-extend-class]
     ["Implement interface" malabar-implement-interface])
    ("Refactor"
     ["Extract constant" malabar-refactor-extract-constant])
    ("Navigation"
     ["Visit project file" malabar-visit-project-file]
     ["Jump to thing" malabar-jump-to-thing]
     ["Visit corresponding test" malabar-visit-corresponding-test])
    "---"
    ["Start Groovy console" malabar-groovy-start]
    ["Stop Groovy console" malabar-groovy-stop]
    ["Run Maven command" malabar-run-maven-command]
    ))

(define-derived-mode malabar-mode java-mode "malabar"
  "A new, better, Java mode."
  ;; HACK: Since we're not loading the old java parser the installer
  ;; function isn't defined; give it a dummy definition
  (flet ((wisent-java-wy--install-parser () nil)
         (wisent-java-tags-wy--install-parser () nil)) ;; For Emacs 23.2+
    (wisent-java-default-setup))
  (setq semantic-lex-depth 10)
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semantic-lex-analyzer 'wisent-malabar-java-lexer)
  (wisent-malabar-java-wy--install-parser)
  (srecode-minor-mode 1)
  ;; Set the menu
  (easy-menu-add malabar-mode-menu malabar-mode-map)
  ;; Set up indentation of Java annotations.
  (malabar-annotations-setup)
  (malabar-abbrevs-setup)
  (malabar-groovy-start t))

(remove-hook 'java-mode-hook 'wisent-java-default-setup)

(defun malabar-electric-colon (arg)
  "Acts as `c-electric-colon'.

In addition, if `malabar-electric-elvis' is non-nil, the colon is
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

(defun malabar-compile-file-silently ()
  "Compiles the current buffer without displaying the result."
  (interactive)
  (malabar-compile-file t))

(defun malabar-compile-file (&optional silent)
  "Compiles the current buffer."
  (interactive)
  (malabar-compile-files (list (buffer-file-name (current-buffer))) silent))

(defun malabar-compile-files (files-to-compile silent)
  (let ((project-files (delete-dups (mapcar 'malabar--project-for-file files-to-compile))))
    (when (cdr project-files)
      (error "You cannot compile files from multiple projects in one operation"))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (malabar-setup-compilation-buffer-1 files-to-compile (car project-files))
    (unless silent
      (display-buffer malabar-groovy-compilation-buffer-name t))
    (malabar-groovy-eval-as-compilation
     (mapconcat (lambda (f)
                  (concat (format "%s.compiler.compile('%s')"
                                  (malabar-project-expression (car project-files))
                                  f)))
                files-to-compile
                "; ")
     silent)))

(defun malabar-compute-package-name (&optional buffer)
  (let* ((dir (file-name-directory (buffer-file-name buffer)))
         (project-file (malabar-find-project-file buffer))
         (source-directories (append (malabar-project-source-directories
                                      project-file)
                                     (malabar-project-test-source-directories
                                      project-file))))
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
  (c-forward-single-comment)
  (unless (bolp)
    (forward-line 1)))

(define-mode-local-override semantic-documentation-for-tag
  malabar-mode (&optional tag nosnarf)
  "Massage some Javadoc syntax stuff."
  (let ((old-at-syntax (string (char-syntax ?@))))
    (unwind-protect
        (progn (modify-syntax-entry ?@ "w")
               (let ((docstring (semantic-documentation-for-tag-java-mode tag nosnarf)))
                 (with-temp-buffer
                   (insert docstring)
                   (goto-char 0)
                   ;; Fontify code markup
                   (while (re-search-forward "{@code \\([^}]+\\)}" nil t)
                     (put-text-property (match-beginning 0) (match-beginning 1)
                                        'invisible t)
                     (put-text-property (match-beginning 1) (match-end 1)
                                        'face '(:slant oblique))
                     (put-text-property (match-end 1) (match-end 0)
                                        'invisible t))
                   ;; Make parameter documentation stand out
                   (let ((param-re "^\\(@param\\)\\s-*\\(\\w+\\)"))
                     (goto-char 0)
                     (when (re-search-forward param-re nil t)
                       (goto-char (match-beginning 0))
                       (insert "\nParameters: \n"))
                     (while (re-search-forward param-re nil t)
                       (put-text-property (match-beginning 1) (match-end 1)
                                          'invisible t)
                       (put-text-property (match-beginning 2) (match-end 2)
                                          'face '(:slant oblique))
                       (goto-char (match-beginning 0))
                       (insert "\n")
                       (goto-char (match-end 0))))
                   (goto-char 0)
                   (when (re-search-forward "^@return" nil t)
                     (replace-match "\nReturns:\n\n"))
                   (fill-region 0 (point-max))
                   (goto-char 0)
                   (while (re-search-forward (concat "\\(" paragraph-start "\\){2,}") nil t)
                     (replace-match "\n\n"))
                   (buffer-string))))
      (modify-syntax-entry ?@ old-at-syntax))))

(defun malabar-show-javadoc (point)
  (interactive "d")
  (let* ((context (semantic-analyze-current-context point))
         (prefix (reverse (oref context prefix))))
    (cond ((stringp (car prefix))
           (message "Incomplete symbol name."))
          ((semantic-tag-p (car prefix))
           (let ((doc (semantic-documentation-for-tag (car prefix))))
             (with-output-to-temp-buffer "*TAG DOCUMENTATION*"
               (princ "Tag: ")
               (princ (semantic-format-tag-prototype (car prefix)))
               (princ "\n")
               (princ "\n")
               (if doc
                   (with-current-buffer standard-output
                     (insert doc))
                 (princ "  Documentation unavailable.")))))
          (t
           (message "Unknown tag.")))))

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

(provide 'malabar-mode)
