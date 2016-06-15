;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename    : generic-remote-module.lisp                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author      : Ryan M. Hope <rmh3093@gmail.com>
;;
;; Copyright   : (c)2016 Ryan M. Hope
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  (load "~/ACT-R/extras/generic-remote-module.lisp")

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
           (let ((*compile-file-pathname* nil))
             (asdf:load-system :usocket)
             (asdf:load-system :bordeaux-threads)
             (asdf:load-system :jsown)))

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defclass remote-module ()
  ((socket :accessor socket :initform nil)
   (jstream :accessor jstream :initform nil)
   (thread :accessor thread :initform nil)
   (sync-cond :accessor sync-cond :initform (bordeaux-threads:make-condition-variable))
   (sync-lock :accessor sync-lock :initform (bordeaux-threads:make-lock))
   (read-from-stream :accessor read-from-stream :initform nil)
   (wait :accessor wait :initform nil)
   (name :accessor name :initform nil)
   (version :accessor version :initform nil)
   (description :accessor description :initform nil)
   (rm-params :accessor rm-params :initform nil)
   (rm-buffers :accessor rm-buffers :initform nil)
   ))

(defmethod rm-send-raw ((instance remote-module) string)
  (write-string string (jstream instance))
  (write-char #\return (jstream instance))
  (write-char #\linefeed (jstream instance))
  (finish-output (jstream instance)))

(defmethod rm-send-command ((instance remote-module) mp model method params &key sync)
  (bordeaux-threads:with-recursive-lock-held
   ((sync-lock instance))
   (progn
    (setf (wait instance) t)
    (rm-send-raw instance (jsown:to-json (jsown:new-js ("mp" mp) ("model" model) ("method" method) ("params" params))))
    (if (and sync (wait instance))
      (bordeaux-threads:condition-wait (sync-cond instance) (sync-lock instance))))))

(defmethod rm-handle-event ((instance remote-module) mp model method params)
  (cond
    ((string= method "sync")
     (progn
      (setf (wait instance) nil)
      (bordeaux-threads:condition-notify (sync-cond instance))))
    ((string= method "init")
     (setf (name instance) (read-from-string (jsown:val params "name")))
     (setf (version instance) (jsown:val params "version"))
     (setf (description instance) (jsown:val params "description"))
     (setf (rm-params instance) (jsown:val params "params")))
    ((string= method "disconnect")
     (return-from rm-handle-event t)))
  (return-from rm-handle-event nil))

(defmethod rm-define-params ((instance remote-module))
  (loop for keyword in (jsown:keywords (rm-params instance))
    collect (define-parameter (make-keyword keyword) :documentation (jsown:val (rm-params instance) keyword))))

(defmethod rm-read-stream ((instance remote-module))
  (handler-case
   (loop
     (if (read-from-stream instance)
       (if (listen (jstream instance))
         (let ((line (read-line (jstream instance))))
           (if (and line (> (length line) 0))
             (let ((o (jsown:parse line)))
               (if (rm-handle-event instance (jsown:val o "mp") (jsown:val o "model") (jsown:val o "method") (jsown:val o "params"))
                 (return-from rm-read-stream "Disconnect")))
             (return-from rm-read-stream "Nothing to read"))))
       (return-from rm-read-stream "Stop reading")))
   (usocket:socket-error () (return-from rm-read-stream "Socket error"))
   (end-of-file () (return-from rm-read-stream "End of file"))))

(defun define-remote-module-fct (hostname port)
  (let ((cls (make-instance 'remote-module)))
    (progn
     (setf (socket cls) (usocket:socket-connect hostname port))
     (setf (jstream cls) (usocket:socket-stream (socket cls)))
     (setf (read-from-stream cls) t)
     (setf (thread cls) (bordeaux-threads:make-thread #'(lambda () (rm-read-stream cls))))
     (rm-send-command cls (current-meta-process) "" "init" nil :sync t)
     (define-module-fct
       (name cls)
       nil
       (rm-define-params cls)
       :version (version cls)
       :documentation (description cls)
       :creation #'(lambda (model) (progn (rm-send-command cls (current-meta-process) model "creation" nil :sync t) cls))
       :run-start #'(lambda (instance) (rm-send-command cls (current-meta-process) (current-model) "run-start" nil :sync t))
       :run-end #'(lambda (instance) (rm-send-command cls (current-meta-process) (current-model) "run-end" nil :sync t))
       :params #'(lambda (instance param) (rm-send-command cls (current-meta-process) (current-model) "params" param :sync t))
       )
     cls
     )
    )
  )
