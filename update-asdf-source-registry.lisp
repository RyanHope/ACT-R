(ensure-directories-exist "~/AppData/Local/common-lisp/source-registry.conf.d/")
(with-open-file (s "~/AppData/Local/common-lisp/source-registry.conf.d/10-actr.conf" :direction :output :if-exists :supersede)
  (format s "(:tree ~S)~%" (directory-namestring (or *compile-file-truename* *load-truename*))))
