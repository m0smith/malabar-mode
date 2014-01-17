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

(require 'flycheck)
(require 'dash)

;; (defun javac ()
;;   (let ((buf (current-buffer)))
;;     (with-current-buffer (pop-to-buffer "*l*")
;;       (call-process "javac" nil t nil 
;; 		    "-cp" (malabar-classpath-test buf)
;; 		    (malabar-util-expand-file-name (buffer-file-name buf))))))


(defun malabar-flycheck-error-parser (output checker buffer)
 "Look in OUTPUT for compiler errors.  

Return a list of `flycheck-error`, one for each error returned."
  (let* ((ss (split-string output "[\n\r]+"))
	 (sss (-drop-while (lambda (s) (string-match "^\\[.*" s)) ss)))
    (-filter 'identity
	    (mapcar (lambda (s) (if (string-match "\\(.*\\):\\([0-9]+\\):.*" (car s))
				    (flycheck-error-new :line (string-to-number(match-string 2 (car s)))
							:column nil 
							:message (mapconcat 'identity s " ")
							:level 'error)))
		    (-partition 3 sss)))))


(eval-after-load 'malabar-mode
  '(progn
     (flycheck-define-checker malabar-mode-javac
       "Syntax java code on the fly"
       :command ("javac"
		 "-verbose"
		 "-d" (eval (malabar-flycheck-target-directory))
		 "-cp" (eval (malabar-classpath-test))
		 (eval (malabar-util-expand-file-name (buffer-file-name))))
       :error-parser malabar-flycheck-error-parser
       :modes malabar-mode)
     
     
     (defun malabar-flycheck-enable ()
       "Enable flycheck in malabar-mode"
       (setq flycheck-checker 'malabar-mode-javac)
       (flycheck-mode 1))

     (defun malabar-flycheck-target-directory (&optional buffer)
       "Determine and create the directory to write class files. "
       (let* ((d (malabar-classes-directory buffer))
	      (pd (format "%s/%s" (file-name-directory (directory-file-name d)) "flycheck")))
	 (make-directory pd t)
	 (malabar-util-expand-file-name pd)))
          
     (add-hook 'malabar-mode-hook #'malabar-flycheck-enable)))
  
