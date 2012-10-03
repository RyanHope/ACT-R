(defsystem :actr6.devices
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.core-modules)
  :serial t
  :components (
	       #+:allegro-ide
	       (:module "acl"
			:serial t
			:components ((:file "device") (:file "uwi"))
			)
	       #+:digitool
	       (:module "mcl"
			:serial t
			:components ((:file "device") (:file "uwi"))
			)
	       #+:lispworks
	       (:module "lw"
			:serial t
			:components ((:file "device") (:file "uwi"))
			)
	       (:module "virtual"
			:serial t
			:components ((:file "device") (:file "uwi"))
			)
	       )
  )