;; -*- lexical-binding: t -*-
;;; malabar-service.el --- Project handling for malabar-mode
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

(require 'url-vars)
(require 'url-dav)
(require 'malabar-util)
(require 'malabar-variables)
(eval-when-compile (require 'cl))
(require 'dash)

;;;
;;;  Service
;;;

;;;###autoload
(defun malabar-service-call (service args-plist &optional buffer array-type object-type readtable)
  "SERVICE is a known service to the malabat server 

   ARGS-PLIST is a list of '(key val key val ...). If pm is not
  in the list, is is pulled from buffer.  Skip entries with a nil key or value

  ARRAY-TYPE is for the JSON reader and can be 'list or 'vector.  Default to vector.

  OBJECT-TYPE is for the JSON reader and can be `alist', `plist',
  or `hash-table'.  Default to `alist'.

  READTABLE is the JSON readtable, default to `json-reatable'."

  (setq url-request-method "GET"
	url-request-extra-headers nil
	url-request-data nil)
  
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
	(setq json-array-type (or array-type 'vector)
	      json-object-type (or object-type 'alist))
	
	(let ((json-readtable-old (when readtable 
				    (let ((r json-readtable))
				      (setq json-readtable readtable) 
				      r)))
	      (rtnval (json-read)))
	  (kill-buffer (current-buffer))
	  (when readtable (setq json-readtable json-readtable-old))
	  rtnval)))))



(provide 'malabar-service)
