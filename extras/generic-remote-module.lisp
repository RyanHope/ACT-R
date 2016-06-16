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
             (asdf:load-system :jsown)))

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(define-condition rm-error (error)
  ((text :initarg :text :reader text)))

(defclass remote-module ()
  ((socket :accessor socket :initform nil)
   (jstream :accessor jstream :initform nil)
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

(defmethod rm-send-command ((instance remote-module) mp model method params)
  (rm-send-raw instance (jsown:to-json (jsown:new-js ("mp" mp) ("model" model) ("method" method) ("params" params))))
  (rm-read-stream instance))

(defmethod rm-handle-event ((instance remote-module) mp model method params)
  (cond
    ((string= method "init")
     (progn
      (setf (name instance) (read-from-string (jsown:val params "name")))
      (setf (version instance) (jsown:val params "version"))
      (setf (description instance) (jsown:val params "description"))
      (setf (rm-params instance) (jsown:val params "params"))
      (setf (rm-buffers instance) (jsown:val params "buffers"))
      t))
    ((string= method "creation")
     instance)
    ((string= method "params")
     (jsown:val params "value"))
    ((string= method "query")
     (jsown:val params "value"))
    (t t)
    ))

(defmethod rm-define-params ((instance remote-module))
  (loop for keyword in (jsown:keywords (rm-params instance))
    collect
    (let* ((param (make-keyword keyword))
           (documentation "")
           (default-value nil))
      (let ((options (jsown:val (rm-params instance) keyword)))
        (loop for option in (jsown:keywords options)
          do (let ((opt (jsown:val options option)))
               (cond
                 ((string= option "documentation")
                  (setf documentation opt))
                 ((string= option "default-value")
                  (setf default-value opt))
                 ))))
      (define-parameter param
        :documentation documentation
        :default-value default-value))))

(defmethod rm-define-buffers ((instance remote-module))
  (loop for keyword in (jsown:keywords (rm-buffers instance))
    collect
    (let* ((name (read-from-string keyword))
           (param-name nil)
           (param-default nil))
      (let ((options (jsown:val (rm-buffers instance) keyword)))
        (loop for option in (jsown:keywords options)
          do (let ((opt (jsown:val options option)))
               (cond
                 ((string= option "param-name")
                  (setf param-name (make-keyword opt)))
                 ((string= option "param-default")
                  (setf param-default opt))
                 ))))
      (define-buffer-fct name
        :param-name param-name
        :param-default param-default))))

(defmethod rm-read-stream ((instance remote-module))
  (usocket:wait-for-input (list (socket instance)) :ready-only t)
  (let ((line (read-line (jstream instance))))
    (progn
     ;(format t "~a~%" line)
     (if (and line (> (length line) 0))
       (let ((o (jsown:parse line)))
         (rm-handle-event instance (jsown:val o "mp") (jsown:val o "model") (jsown:val o "method") (jsown:val o "params")))))))

(defun define-remote-module-fct (hostname port)
  (let ((cls (make-instance 'remote-module)))
    (progn
     (setf (socket cls) (usocket:socket-connect hostname port))
     (setf (jstream cls) (usocket:socket-stream (socket cls)))
     (rm-send-command cls (current-meta-process) "" "init" nil)
     (define-module-fct
       (name cls)
       (rm-define-buffers cls)
       (rm-define-params cls)
       :version (version cls)
       :documentation (description cls)
       :query #'(lambda (instance buffer-name slot value) (rm-send-command cls (current-meta-process) (current-model) "query" (list buffer-name slot value)))
       :request #'(lambda (instance buffer-name chunk-spec) (progn (format t "~a ~a~%" buffer-name chunk-spec) (rm-send-command cls (current-meta-process) (current-model) "request" (list buffer-name (format nil "~a" chunk-spec)))))
       :buffer-mod #'(lambda (instance buffer chunk-spec) (rm-send-command cls (current-meta-process) (current-model) "buffer-mod" (list buffer (format nil "~a" chunk-spec))))
       :creation #'(lambda (model) (rm-send-command cls (current-meta-process) model "creation" nil))
       :run-start #'(lambda (instance) (rm-send-command cls (current-meta-process) (current-model) "run-start" nil))
       :run-end #'(lambda (instance) (rm-send-command cls (current-meta-process) (current-model) "run-end" nil))
       :params #'(lambda (instance param) (rm-send-command cls (current-meta-process) (current-model) "params" (if (consp param) (list (car param) (cdr param)) param)))
       )
     cls
     )
    )
  )
