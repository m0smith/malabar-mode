
!Steps to release to melpa.  

I do not think I can use the maven release plugin as the artifact is
being written to a new branch.

1 Create a release branch:
       git branch <RELEASE>
       git push origin <RELEASE>
       git checkout <RELEASE>

2 Edit the pom.xml on the branch with the new VERSION

3 Create the dist in the RELEASE branch
        mvn clean package
        mkdir dist
        cd dist
	unzip ../target/*.zip
	cd ..
	git add dist
	git commit -a -m "<RELEASE>"
4 Update the MELPA recipe to match
  	(malabar-mode 
		      :repo "m0smith/malabar-mode" 
		      :fetcher github 
		      :commit "origin/<RELEASE>"
		      :files (
		      	     ("." "dist/malabar-<VERSION>/lisp/*")
			     ("lib" "dist/malabar-<VERSION>/lib/*")
			     ))
5 Switch back the malabar-mode master branch
  	 git checkout master
6 Update the pom.xml version to start the next development cycle
