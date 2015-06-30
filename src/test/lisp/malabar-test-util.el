;;; -*- emacs-lisp -*-
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


(defun malabar-test-util-wait-for-server (retries)
  "Make sure the malabar server is up.  Wait RETRIES seconds"
  (with-current-buffer "*groovy*"
    (save-match-data
      (when (> retries 0)
	(let ((s (buffer-substring-no-properties (point-min) (min 1000 (point-max)))))
	  (if (not (string-match "HttpServerImpl" s))
	      (progn
		(message "Malabar Server is not up, %d tries left %s ...." retries s )
		(sit-for 1 t)
		(malabar-test-util-wait-for-server (1- retries)))
	    t))))))
      
    

(defun malabar-test-util-find-file-into-buffer (file-name)
  "Find a file into an existing buffer, usually a temp buffer.  Sets java and malabar modes"
  (insert-file-contents-literally file-name t)
  (setq buffer-file-name file-name)
  (setq default-directory (file-name-directory file-name))
  (java-mode)
  (malabar-java-mode))

(provide 'malabar-test-util)
