;;; (install-dist "http://beta.quicklisp.org/dist/quicklisp/2016-02-08/distinfo.txt" :replace t)
(defsystem actr
  :name "actr"
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "7.0.1930"
  :licence "LGPL-2.1"
  :serial t
  :components ((:file "stubs")

               (:module "framework"
                :serial t
                :components ((:file "version-string")
                             (:file "internal-structures")
                             (:file "internal-macros")
                             (:file "misc-utils")
                             (:file "system-parameters")
                             (:file "meta-process")
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
                             (:file "vision-categorization")
                             (:file "random")
                             (:file "printing")
                             (:file "naming-module")))

              (:module "support-early"
               :pathname "support"
               :serial t
               :components ((:file "central-parameters")
                            (:file "productions")
                            (:file "production-parsing-support")
                            (:file "goal-style-module")
                            (:file "general-pm")))
    

               (:module "core-modules"
                :serial t
                :components ((:file "declarative-memory")
                             (:file "goal")
                             (:file "procedural")
                             (:file "vision")
                             (:file "motor")
                             (:file "audio")
                             (:file "speech")
                             (:file "imaginal")))

               (:module "commands"
                :serial t
                :components #.(mapcar #'(lambda (x)
                                          `(:file ,(file-namestring
                                                    (make-pathname :type nil :defaults (pathname x)))))
                                      (directory "commands/*.lisp")))

               (:module "modules"
                :serial t
                :components #.(mapcar #'(lambda (x)
                                          `(:file ,(file-namestring
                                                    (make-pathname :type nil :defaults (pathname x)))))
                                      (directory "modules/*.lisp")))

               (:module "tools"
                :serial t
                :components #.(mapcar #'(lambda (x)
                                          `(:file ,(file-namestring
                                                    (make-pathname :type nil :defaults (pathname x)))))
                                      (directory "tools/*.lisp")))

               (:module "other-files"
                :serial t
                :components #.(mapcar #'(lambda (x)
                                          `(:file ,(file-namestring
                                                    (make-pathname :type nil :defaults (pathname x)))))
                                      (directory "other-files/*.lisp")))

               )
  )

(defmethod perform :before ((op asdf:load-op) (system (eql (find-system :actr))))
  #+(and :mcl (not :openmcl)) (require 'quickdraw))

(defmethod perform :after ((op asdf:load-op) (system (eql (find-system :actr))))
  (funcall (intern (symbol-name 'mp-print-versions))))
