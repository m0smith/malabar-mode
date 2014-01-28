;;; malabar-flycheck.el --- Flycheck integration
;;
;; Copyright (c) 2014 Matthew O. Smith <matt@m0smith.com>
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

;(require 'flycheck)
;(require 'dash)

;; (defun javac ()
;;   (let ((buf (current-buffer)))
;;     (with-current-buffer (pop-to-buffer "*l*")
;;       (call-process "javac" nil t nil 
;; 		    "-cp" (malabar-classpath-test buf)
;; 		    (malabar-util-expand-file-name (buffer-file-name buf))))))


(defun malabar-flycheck-error-column (lines)
  "Find the column of the error by looking for the magic caret.  Expand tabs to 8 spaces."
  (let* ((caret-line (-first (lambda (s) (string-match "\\([\t ]*\\^\\).*" s)) lines))
	 (g1 (when caret-line (match-string 1 caret-line)))
	 (g2 (when g1 (replace-regexp-in-string " " "!" g1))))
    (when  g2 (length g2))))
    

(defun malabar-flycheck-error-line? (line)
  "Look for the line that has the error line and message"
  (string-match "\\(.*\\):\\([0-9]+\\): *\\(.*\\):\\(.*\\)" line))

(defun malabar-flycheck-error-create (lines)
  "Return the flycheck error"
  (let* ((first-line (car lines)))

    (if (malabar-flycheck-error-line? first-line)
	(let ((level (match-string 3 first-line)))
	  (flycheck-error-new :line (string-to-number (match-string 2 first-line))
			      :message (concat (match-string 4 first-line) " \n"
					       (mapconcat 'identity (cdr lines) " \n"))
			      :level (intern level)
			      :column (malabar-flycheck-error-column lines))))))

(defun malabar-flycheck-error-parser (output checker buffer)
 "Look in OUTPUT for compiler errors.  

Return a list of `flycheck-error`, one for each error returned."
 (let* ((ss (split-string output "[\n\r]+"))
	(sss (-drop-while (lambda (s) (string-match "^\\[.*" s)) ss)))
   (-filter 'identity
	    (mapcar 'malabar-flycheck-error-create 
		    (-partition-by-header 'malabar-flycheck-error-line? sss)))))


;; Wait to make sure the dependencies are created

(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker malabar-mode-javac
       "Syntax java code on the fly.  Unfortunately, with cygwin
the file name needs to be converted to something windwos
friendly."
       :command ("javac"
		 "-d" (eval (malabar-util-expand-file-name 
			(flycheck-substitute-argument 'temporary-directory nil)))
		 "-cp" (eval (malabar-classpath-test))
		 (eval (malabar-util-expand-file-name 
			(flycheck-substitute-argument 'source nil))))
       :error-parser malabar-flycheck-error-parser
       :modes malabar-mode)
     
     
     (add-to-list 'flycheck-checkers 'malabar-mode-javac)))




  
