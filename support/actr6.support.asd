(defsystem :actr6.support
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.framework :actr6.core-modules :actr6.commands)
  :serial t
  :components (
	       (:file "central-parameters")
	       (:file "dmi")
	       (:file "environment-colors")
	       (:file "general-pm")
	       (:file "goal-style-module")
	       (:file "production-parsing-support")
	       (:file "uni-files")
	       )
  )