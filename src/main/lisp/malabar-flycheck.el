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

;; (defun javac ()
;;   (let ((buf (current-buffer)))
;;     (with-current-buffer (pop-to-buffer "*l*")
;;       (call-process "javac" nil t nil 
;; 		    "-cp" (malabar-classpath-test buf)
;; 		    (malabar-util-expand-file-name (buffer-file-name buf))))))

(eval-after-load 'malabar-mode
  '(progn
     (flycheck-define-checker malabar-mode-javac
       "Syntax java code on the fly"
       :command ("javac"
		 "-cp" (eval (malabar-classpath-test))
		 (eval (malabar-util-expand-file-name (buffer-file-name))))
       :error-parser flycheck-parse-checkstyle
       :modes malabar-mode)
     
     
     (defun malabar-flycheck-enable ()
       (setq flycheck-checker 'malabar-mode-javac)
       (flycheck-mode 1))
     
     (add-hook 'malabar-mode-hook #'malabar-flycheck-enable)))
  
