;;; malabar-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (malabar-mode) "malabar-mode" "malabar-mode.el"
;;;;;;  (21647 188 0 0))
;;; Generated autoloads from malabar-mode.el

(autoload 'malabar-mode "malabar-mode" "\
Support and integeration for JVM languages

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (malabar-project-sources malabar-project-resources)
;;;;;;  "malabar-project" "malabar-project.el" (21646 28243 0 0))
;;; Generated autoloads from malabar-project.el

(autoload 'malabar-project-resources "malabar-project" "\
SCOPE is either 'test or 'runtime

\(fn PROJECT-INFO SCOPE)" t nil)

(autoload 'malabar-project-sources "malabar-project" "\
SCOPE is either 'test or 'runtime

\(fn PROJECT-INFO SCOPE)" t nil)

;;;***

;;;### (autoloads (malabar-http-call) "malabar-http" "malabar-http.el"
;;;;;;  (21647 600 0 0))
;;; Generated autoloads from malabar-http.el

(autoload 'malabar-http-call "malabar-http" "\
SERVICE is a known service to the malabat server 

   ARGS-PLIST is a list of '(key val key val ...). If pm is not
  in the list, is is pulled from buffer.  Skip entries with a nil key or value

\(fn SERVICE ARGS-PLIST &optional BUFFER)" nil nil)

;;;***

;;;### (autoloads nil nil ("malabar-import.el" "malabar-reflection.el"
;;;;;;  "malabar-semanticdb.el" "malabar-util.el" "malabar-variables.el")
;;;;;;  (21647 633 181000 0))

;;;***

(provide 'malabar-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; malabar-mode-autoloads.el ends here
