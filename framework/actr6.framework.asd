(defsystem :actr6.framework
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.stubs)
  :serial t
  :components (
	       (:file "version-string")
	       (:file "internal-structures")
	       (:file "internal-macros")
	       (:file "misc-utils")
	       (:file "meta-process")
	       (:file "system-parameters")
	       (:file "chunk-types")
	       (:file "chunks")
	       (:file "modules")
	       (:file "parameters")
	       (:file "buffers")
	       (:file "model")
	       (:file "events")
	       (:file "scheduling")
	       (:file "chunk-spec")
	       (:file "top-level")
	       (:file "device-interface")
	       (:file "generic-interface")
	       (:file "vision-categorization")
	       (:file "random")
	       (:file "printing")
	       (:file "naming-module")
	       )
  )