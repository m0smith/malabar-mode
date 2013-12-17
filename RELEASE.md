# Steps to release to melpa.  

**Do not forget there are three places to update the version:
* pom.xml
* src/main/pom/classpath.pom
* src/main/list/malabar-mode.el

**maven release if the java or groovy files have been updated**

**Merge the develop branch into master**

**Test the install from the melpa project dir and then test in emacs**

```
        rm -rf ~/.emacs.d/elpa/malabar-mode-*
        make clean
        make recipes/malabar-mode
        emacs --batch --exec '(package-install-file "/full/path/to/melpa/packages/malabar-mode-<MELPA-VERSION>.tar")'
```


**Update the pom.xml version to start the next development cycle**


