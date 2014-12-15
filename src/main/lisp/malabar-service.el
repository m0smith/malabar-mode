;;; malabar-project.el --- Project handling for malabar-mode
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

;(require 'malabar-groovy)

(require 'malabar-util)
(require 'malabar-variables)
(eval-when-compile (require 'cl))
(require 'dash)
;;; malabar-project.el --- Project handling for malabar-mode
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

;(require 'malabar-groovy)

;(require 'malabar-util)
(eval-when-compile (require 'cl))
(require 'dash)
(require 'malabar-variables)


;;;
;;;  Service
;;;

;;;###autoload
(defun malabar-service-call (service args-plist &optional buffer)
  "SERVICE is a known service to the malabat server 

   ARGS-PLIST is a list of '(key val key val ...). If pm is not
  in the list, is is pulled from buffer.  Skip entries with a nil key or value"
  
  (with-current-buffer (or buffer (current-buffer))
    (let* ((args-alist (-partition-all 2 args-plist))
	   (args-alist (-filter (lambda (c) (and (not (null (car c))) (not (null (cadr c))))) args-alist))
	   (args-alist (if (assoc "pm" args-alist) args-alist (append args-alist `(("pm" ,malabar-mode-project-file)))))
	   (args (mapconcat (lambda (c) (concat (car c) "=" (cadr c))) args-alist "&"))
	   (url (format "http://%s:%s/%s/?%s"
			malabar-server-host
			malabar-server-port
			service
			args)))
      (with-current-buffer (url-retrieve-synchronously url)
	(goto-char url-http-end-of-headers)
	(let ((rtnval (json-read)))
	  (kill-buffer (current-buffer))
	  rtnval)))))



(provide 'malabar-project)
