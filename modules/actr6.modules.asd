(defsystem :actr6.modules
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :depends-on (:actr6.commands)
  :serial t
  :components #.(mapcar #'(lambda (x)
			    `(:file ,(file-namestring
				      (make-pathname :type nil :defaults (pathname x)))))
			(directory "*.lisp"))
  )