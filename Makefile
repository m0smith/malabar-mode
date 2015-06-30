
### Makefile for malabar-mode
##
## Copyright (c) 2009, 2010 Espen Wiborg <espenhw@grumblesmurf.org>
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 2 of the
## License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.
##

EMACS=emacs

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))

all: test

test: test-elk test-ert

test-elk:
#	cd $(mkfile_dir) && $(EMACS) -Q --batch -l src/test/lisp/init.el -l src/test/lisp/all-tests.el
	cd $(mkfile_dir) && $(EMACS) -Q --batch -f toggle-debug-on-error -l src/test/lisp/init.el  -l src/test/lisp/all-tests.el

test-ert:
	cd $(mkfile_dir) && $(EMACS) -Q --batch  -l ert -l src/test/lisp/init.el \
	         -l src/test/lisp/init-malabar.el  \
	         -l src/test/lisp/malabar-test-util.el  \
	         -l src/test/lisp/malabar-mode.ert \
	         -l src/test/lisp/malabar-ede-maven.ert \
                 -f ert-run-tests-batch-and-exit

