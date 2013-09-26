(ensure-directories-exist "~/.config/common-lisp/source-registry.conf/")
(with-open-file (s "~/.config/common-lisp/source-registry.conf/10-actr6.conf" :direction :output :if-exists :supersede)
  (format s "(:tree ~S)~%" (directory-namestring (or *compile-file-truename* *load-truename*))))