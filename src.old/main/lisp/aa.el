;(run-groovy) C:/Users/Smith/.gvm/groovy/2.3.7/bin/groovysh --color=false -Dgroovy.grape.report.downloads=true

(defun groovy-send-string (str)
  (interactive "s")
  (save-excursion
    (save-restriction
      (let ((proc (groovy-proc)))

	(with-current-buffer (process-buffer proc)
	  (while (and
		  (goto-char comint-last-input-end)
		  (not (re-search-forward comint-prompt-regexp nil t))
		  (accept-process-output proc)))
	  (goto-char (process-mark proc))
	  (insert-before-markers str)
	  (move-marker comint-last-input-end (point))
	  (comint-send-string proc str)
	  (comint-send-string proc "\n")
	  )
	))))



(groovy-send-string "import groovy.grape.Grape")
(groovy-send-string "Grape.grab(group:'com.software-ninja', module:'malabar', version:'2.0.4-SNAPSHOT')")
(groovy-send-string "Grape.grab(group:'org.apache.maven.plugin-testing', module:'maven-plugin-testing-harness', version:'2.1')")
(groovy-send-string "import com.software_ninja.malabar.project.MavenProject")
(groovy-send-string "m = new MavenProject()")

;;(groovy-send-string "m = new com.software_ninja.malabar.project.MavenProject(); m.projectInfo('c:/users/Smith/.m2/repository','pom.xml')")

