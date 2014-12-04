* Install groovy (2.3.7)
* Install gradle
* Clone https://github.com/m0smith/malabar-mode/tree/develop

* Clone https://github.com/alexott/cedet/tree/devel (git clone https://github.com/alexott/cedet.git)
** Run make to byte-compile

* Install emacs package groovy-mode.  Do not use marmalade version as it is old.

* Add to .emacs: 

    (load-file "~/projecrs/cedet/cedet-devel-load.el")
    (load-file "~/projects/malabar-mode/src/main/lisp/new/malabar-mode.el")

* in emacs (malabar-run-groovy)
* Edit a java file in a maven2 project
  