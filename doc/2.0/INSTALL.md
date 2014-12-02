* Install groovy (2.3.7)
* Install gradle
* Clone https://github.com/m0smith/malabar-mode/tree/develop
* Clone https://github.com/m0smith/malabar-mode-jar
* Install emacs package groovy-mode.  Do not use marmalade version as it is old.
* Install malabar-mode-jar (gradle install)
* Add to .emacs: (load-file "~/projects/malabar-mode/src/main/lisp/new/malabar-mode.el")
* in emacs (malabar-run-groovy)
* Edit a java file in a maven2 project
