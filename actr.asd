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
                            (:file "goal-style-module")
                            (:file "general-pm")))


               (:module "core-modules"
                :serial t
                :components ((:file "declarative-memory")
                             (:file "goal")
                             (:file "procedural")
                             (:file "vision")
                             (:file "motor")
                             (:file "motor-extension")
                             (:file "audio")
                             (:file "speech")
                             (:file "imaginal")))

              (:module "support-late"
               :pathname "support"
               :serial t
               :components ((:file "production-parsing-support")
                            (:file "uni-files")
                            (:file "environment-colors")))

                (:module "devices"
                 :components ((:module "virtual"
                               :serial t
                               :components ((:file "device") (:file "uwi")))))

                (:module "commands"
                 :serial t
                 :components ((:file "conflict-tree")
                              (:file "dm-commands")
                              (:file "procedural-cmds")
                              (:file "p-star-cmd")))

               (:module "modules"
                :serial t
                :components #.(mapcar #'(lambda (x)
                                          `(:file ,(file-namestring
                                                    (make-pathname :type nil :defaults (pathname x)))))
                                      (directory "modules/*.lisp")))

               (:module "environment"
                :serial t
                :components ((:file "handler-class")
                             (:file "server")
                             (:file "env-module")
                             #+(and :mcl (not :openmcl))(:file "mcl-fix")
                             (:file "handlers")
                             (:file "environment-cmds")
                             (:file "stepper-control")
                             (:file "env-device")))

               (:module "tools"
                :serial t
                :components ((:file "buffer-trace")
                             (:file "goal-compilation")
                             (:file "imaginal-compilation")
                             (:file "motor-compilation")
                             (:file "perceptual-compilation")
                             (:file "retrieval-compilation")))

               (:module "other"
                :pathname "other-files"
                :serial t
                :components ((:file "system-param-init")
                             (:file "bold")
                             (:file "buffer-history")
                             (:file "production-history")
                             (:file "retrieval-history")
                             (:file "env-graphic-trace")))

               )
  )

(defmethod perform :before ((op asdf:load-op) (system (eql (find-system :actr))))
  #+(and :mcl (not :openmcl)) (require 'quickdraw))

(defmethod perform :after ((op asdf:load-op) (system (eql (find-system :actr))))
  (funcall (intern (symbol-name 'mp-print-versions))))
