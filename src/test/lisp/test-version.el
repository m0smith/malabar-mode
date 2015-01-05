;; -*- lexical-binding: t -*-
;;; test-version.el --- JVM Integration mode for EMACS

;; Copyright (c) Matthew O. Smith <matt@m0smith.com>
;;
;; Author: 
;;     Matthew O. Smith <matt@m0smith.com>
;; URL: http://www.github.com/m0smith/malabar-mode

;;; License:

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for malabar-version function
;;

;;; Code:

(defun malabar-ert-load (name)
  (let ((dir (expand-file-name "main/lisp" (file-name-directory (directory-file-name (file-name-directory (directory-file-name  default-directory)))))))
    (add-to-list 'load-path dir)
    (load-file (expand-file-name name dir))))

(ert-deftest malabar-version-test ()
  (malabar-ert-load "malabar-mode.el")
  (should (string-prefix-p "2.0.1" (malabar-version)))))




;;; test-version.el ends here

