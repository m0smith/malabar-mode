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


(defun malabar-ert-dir (path)
  (let ((dir (expand-file-name path (file-name-directory (directory-file-name (file-name-directory (directory-file-name  default-directory)))))))
    dir))

(defun malabar-ert-load (name)
  (let ((dir (malabar-ert-dir "main/lisp")))
    (add-to-list 'load-path dir)
    (load-file (expand-file-name name dir))))

(ert-deftest malabar-version-test ()
  (malabar-ert-load "malabar-mode.el")
  (should (string-prefix-p "2.0.1" (malabar-version))))

(ert-deftest malabar-run-maven-command-test () (should (= 1 1)))

(ert-deftest malabar-ert-run-maven-command-test ()
  "Unit test to verify that `malabar-run-maven-command' is running correctly"
  (malabar-ert-load "malabar-mode.el")
  (let* ((source (expand-file-name "AppTest.java" 
				   (malabar-ert-dir "test/project/basic/src/test/java/com/software_ninja")))
	 (buffer (find-file-noselect source)))
    (with-current-buffer buffer
      (should (string-prefix-p "*compilation*" (buffer-name (malabar-run-maven-command "clean")))))))

(ert-deftest malabar-ert-run-test ()
  (malabar-ert-load "malabar-mode.el")
  (let* ((source (expand-file-name "AppTest.java" 
				   (malabar-ert-dir "test/project/basic/src/test/java/com/software_ninja")))
	(buffer (find-file-noselect source)))
    (with-current-buffer buffer
      (should (equal 1 (length (malabar-http-run-test nil)))))))




;;; test-version.el ends here

