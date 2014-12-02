* Install groovy (2.3.7)
* Install gradle
* Clone https://github.com/m0smith/malabar-mode/tree/develop
* Clone https://github.com/m0smith/malabar-mode-jar
* Clone https://github.com/alexott/cedet/tree/devel
** Run make to byte-compile
* Install emacs package groovy-mode.  Do not use marmalade version as it is old.
* Install malabar-mode-jar (gradle install)
* Add to .emacs: 
(load-file "~/emacs/cedet-bzr/cedet-devel-load.el")
(load-file "~/projects/malabar-mode/src/main/lisp/new/malabar-mode.el")

* in emacs (malabar-run-groovy)
* Edit a java file in a maven2 project
