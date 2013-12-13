# Steps to release to melpa.  

I do not think I can use the maven release plugin as the artifact is
being written to a new branch.

**Update the dist repo:**

```
	 git clone https://github.com/m0smith/malabar-mode-dist.git
```

**Edit the pom.xml and malabar-mode.el on the branch with the new VERSION**

**Update the malabar-mode-dist repo with the release**

```
        mvn clean package
        cd malabar-mode-dist
        rm -rf malabar
        unzip ~/projects/malabar-mode/target/*.zip
        git commit -a -m "<RELEASE>"
	git push
```

**Update the MELPA recipe to match**

```
  	(malabar-mode 
		      :repo "m0smith/malabar-mode-dist" 
		      :fetcher github 
		      :files (
		      	     ("." "malabar/lisp/*")
			     ("lib" "malabar/lib/*")
			     ))
```

**Test the install from the melpa project dir and then test in emacs**

```
        rm -rf ~/.emacs.d/elpa/malabar-mode-*
        make clean
        make recipes/malabar-mode
        emacs --batch --exec '(package-install-file "/full/path/to/melpa/packages/malabar-mode-<MELPA-VERSION>.tar")'
```



**Commit MELPA branch and make pull request**


**Update the pom.xml version to start the next development cycle**


