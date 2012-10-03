(defsystem :actr6.core-modules
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.support)
  :serial t
  :components (
	       (:file "declarative-memory")
	       (:file "goal")
	       (:file "procedural")
	       (:file "vision")
	       (:file "motor")
	       (:file "audio")
	       (:file "speech")
	       (:file "imaginal")
	       )
  )