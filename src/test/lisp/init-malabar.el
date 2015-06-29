(add-to-list 'load-path (expand-file-name "src/main/lisp/" (getenv "PWD")))
(load-file "src/main/lisp/malabar-mode.el")
(activate-malabar-mode)
(malabar-run-groovy)

;;(find-file "src/test/project/basic/src/main/java/com/software_ninja/App.java")
