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
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name))
  (add-to-list 'load-path (replace-regexp-in-string "test" "main"
                                                    (file-name-directory load-file-name)
                                                    t t)))

(require 'elk-test)

(setq elk-test-run-on-define nil)

(mapc #'load (directory-files (file-name-directory load-file-name) t "\\.elk\\'"))

(apply #'elk-test-group "all-tests" (mapcar #'car elk-test-alist))

(let ((failures (remove nil (mapcar (lambda (r)
                                      (when (cdr r)
                                        r))
                                    (elk-test-run "all-tests")))))
  (mapc (lambda (r)
          (message "***FAIL: %s: %s" (car r) (cdr r)))
        failures)
  (when (and failures noninteractive)
    (kill-emacs 1)))
