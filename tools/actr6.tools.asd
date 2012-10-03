(defsystem :actr6.tools
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.environment)
  :serial t
  :components (
	       (:file "buffer-trace")
	       (:file "goal-compilation")
	       (:file "imaginal-compilation")
	       (:file "motor-compilation")
	       (:file "perceptual-compilation")
	       (:file "retrieval-compilation")
	       )
  )