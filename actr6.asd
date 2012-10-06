(defsystem :actr6
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :serial t
  :depends-on (
	       :actr6.framework
	       :actr6.support
	       :actr6.core-modules
	       :actr6.devices
	       :actr6.commands
	       :actr6.modules
	       :actr6.environment
	       :actr6.tools
	       :actr6.other
	       )
  )

(defmethod perform :after ((op asdf:load-op) (system (eql (find-system :actr6))))
  (funcall (intern (symbol-name 'mp-print-versions))))
