(defsystem :actr6
  :description "The ACT-R cognitive architecture."
  :author "Dan Bothell <db30@andrew.cmu.edu>"
  :maintainer "Dan Bothell <db30@andrew.cmu.edu>"
  :version "1.4.1261"
  :licence "LGPL-2.1"
  :serial t
  :components ((:file "stubs")
               
               (:module "framework"
                :serial t
                :components ((:file "version-string")
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
                             (:file "naming-module")))
               
               (:module "support-early"
                :pathname "support"
                :serial t
                :components ((:file "backward")
                             (:file "central-parameters")
                             (:file "goal-style-module")
                             (:file "general-pm")
                             (:file "dmi")))
               
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
               
               (:module "support-late"
                :pathname "support"
                :serial t
                :components ((:file "production-parsing-support")
                             (:file "uni-files")
                             (:file "environment-colors")
                             #+(and :clozure :darwin :apple-objc :ccl-1.8)
                             (:file "ccl-simple-view")))
               
               (:module "devices"
                :components (#+(and :clozure :darwin :apple-objc :ccl-1.8)
                             (:module "ccl-cocoa"
                              :serial t
                              :components ((:file "device") (:file "uwi")))
                             
                             #+:allegro-ide
                             (:module "acl"
                              :serial t
                              :components ((:file "device") (:file "uwi")))
                             #+:digitool
                             (:module "mcl"
                              :serial t
                              :components ((:file "device") (:file "uwi")))
                             #+:lispworks
                             (:module "lw"
                              :serial t
                              :components ((:file "device") (:file "uwi")))
                             (:module "virtual"
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
                :components ((:file "server")
                             (:file "env-device")
                             (:file "env-module")
                             (:file "environment-cmds")
                             (:file "handler-class")
                             (:file "handlers")
                             (:file "standalone")
                             (:file "stepper-control")))
               
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
                :components ((:file "bold")
                             (:file "buffer-history")
                             (:file "production-history")
                             (:file "retrieval-history")
                             (:file "env-graphic-trace")))
               
               
               )
  )

(defmethod perform :before ((op asdf:load-op) (system (eql (find-system :actr6))))
  #+(and :mcl (not :openmcl)) (require 'quickdraw))

(defmethod perform :after ((op asdf:load-op) (system (eql (find-system :actr6))))
  (funcall (intern (symbol-name 'mp-print-versions))))
