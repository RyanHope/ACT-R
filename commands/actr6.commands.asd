(defsystem :actr6.commands
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.devices)
  :serial t
  :components (
	       (:file "conflict-tree")
	       (:file "dm-commands")
	       (:file "procedural-cmds")
	       (:file "p-star-cmd")
	       )
  )