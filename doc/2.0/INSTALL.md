This is the current installation instructions.  It will get simpler as
the project progrsses.

* Install groovy (2.3.7 or later).  Ubunutu has a real old version.  Use GVM to install http://gvmtool.net/
* Install gradle
* Clone https://github.com/m0smith/malabar-mode/tree/develop

* Clone https://github.com/alexott/cedet/tree/devel (git clone https://github.com/alexott/cedet.git)

```
make all
```

* Install emacs package groovy-mode.  Do not use marmalade version as it is old.

* Add to .emacs: 

````elisp
    (load-file "~/projecrs/cedet/cedet-devel-load.el")
    (load-file "~/projects/malabar-mode/src/main/lisp/new/malabar-mode.el")
````

* in emacs (malabar-run-groovy) or C-u M-x run-groovy - you may need to enter the path to groovysh, especially on Windows
* Edit a java/groovy file in a maven2 project
* In the buffer run flycheck-mode and malabar-mode to enable the features
  