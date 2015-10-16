(require 'package)
(package-initialize)

(setq cedet_dir (getenv "CEDET_DIR"))

(if (null cedet_dir)
    (load-file "~/projects/cedet/cedet-devel-load.el")
  (load-file (concat (file-name-as-directory cedet_dir) "cedet-devel-load.el"))
)
