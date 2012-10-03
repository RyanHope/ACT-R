(defsystem :actr6.environment
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.modules)
  :serial t
  :components (
	       (:file "server")
	       (:file "env-device")
	       (:file "env-module")
	       (:file "environment-cmds")
	       (:file "handler-class")
	       (:file "handlers")
	       (:file "standalone")
	       (:file "stepper-control")
	       )
  )