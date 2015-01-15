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

NOTE:  In order to get the recipe to compile with melpa within emacs started from cygwin I had to setup a couple of things

- An environment variable TAR_OPTIONS must include `--force-local` like   `export TAR_OPTIONS=--force-local`.  Not doing this results in `tar: Cannot connect to c: resolve failed`
- The `shell-file-name` has to be a valid executable.  Starting emacs from cygwin sets it to something like `/bin/bash`.  That has to be changed to something like `C:\\cygwin64\\bin\\bash.exe`

**Update the pom.xml version to start the next development cycle**


