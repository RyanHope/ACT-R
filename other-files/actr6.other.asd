(defsystem :actr6.other
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.tools)
  :serial t
  :components (
	       (:file "bold")
	       (:file "buffer-history")
	       (:file "production-history")
	       (:file "retrieval-history")
	       (:file "env-graphic-trace")
	       )
  )