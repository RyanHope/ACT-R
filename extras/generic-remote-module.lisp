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

(define-condition rm-error (error)
  ((text :initarg :text :reader text)))

(defclass remote-module ()
  ((socket :accessor socket :initform nil)
   (jstream :accessor jstream :initform nil)
   (socket2 :accessor socket2 :initform nil)
   (jstream2 :accessor jstream2 :initform nil)
   (thread :accessor thread :initform nil)
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

(defmethod rm-send-raw2 ((instance remote-module) string)
  (write-string string (jstream2 instance))
  (write-char #\return (jstream2 instance))
  (write-char #\linefeed (jstream2 instance))
  (finish-output (jstream2 instance)))

(defmethod rm-send-command2 ((instance remote-module) mp model method params)
  (rm-send-raw2 instance (jsown:to-json (jsown:new-js ("mp" mp) ("model" model) ("method" method) ("params" params))))
  (rm-read-stream2 instance method))

(defmethod rm-send-command ((instance remote-module) mp model method params)
  (rm-send-raw instance (jsown:to-json (jsown:new-js ("mp" mp) ("model" model) ("method" method) ("params" params))))
  (rm-read-stream instance method))

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

(defmethod rm-handle-event2 ((instance remote-module) mp model method params)
  (cond
    ((string= method "define-chunks")
      (rm-send-command2 instance (current-meta-process) (current-model) "define-chunks" (define-chunks-fct (jsown:val params "chunks")) :wait t))
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

(defmethod rm-read-stream ((instance remote-module) m)
  (loop
    (usocket:wait-for-input (list (socket instance)) :ready-only t)
    (let ((line (read-line (jstream instance))))
      (progn
       (format t "~a~%" line)
       (if (and line (> (length line) 0))
         (let* ((o (jsown:parse line))
                (mp (jsown:val o "mp"))
                (model (jsown:val o "model"))
                (method (jsown:val o "method"))
                (params (jsown:val o "params"))
                (ret (rm-handle-event instance mp model method params)))
           (if (string= m method)
             (return-from rm-read-stream ret))))))))

(defmethod rm-read-stream2 ((instance remote-module) m)
  (loop
    (usocket:wait-for-input (list (socket2 instance)) :ready-only t)
    (let ((line (read-line (jstream2 instance))))
      (progn
       (format t "~a~%" line)
       (if (and line (> (length line) 0))
         (let* ((o (jsown:parse line))
                (mp (jsown:val o "mp"))
                (model (jsown:val o "model"))
                (method (jsown:val o "method"))
                (params (jsown:val o "params"))
                (ret (rm-handle-event instance mp model method params)))
           (if (string= m method)
             (return-from rm-read-stream2 ret))))))))

(defmethod incomming ((instance remote-module) inport)
  (usocket:wait-for-input (setf (socket2 instance) (usocket:socket-listen usocket:*wildcard-host* inport :reuse-address t)))
  (setf (client2 instance) (usocket:socket-accept (socket2 instance)))
  (setf (jstream2 instance) (usocket:socket-stream (socket2 instance)))
  ; (loop
  ;   (usocket:wait-for-input (list (socket2 instance)) :ready-only t)
  ;   (let ((line (read-line (jstream2 instance))))
  ;     (progn
  ;      (format t "~a~%" line)
  ;      (if (and line (> (length line) 0))
  ;        (let* ((o (jsown:parse line))
  ;               (mp (jsown:val o "mp"))
  ;               (model (jsown:val o "model"))
  ;               (method (jsown:val o "method"))
  ;               (params (jsown:val o "params")))
  ;          (rm-handle-event2 instance mp model method params))
  ;        )
  ;      )
  ;     )
  ;   )
  )

(defun define-remote-module-fct (hostname outport inport)
  (let ((cls (make-instance 'remote-module)))
    (progn
     (setf (thread cls) (bordeaux-threads:make-thread #'(lambda () (incomming cls inport))))
     (setf (socket cls) (usocket:socket-connect hostname outport))
     (setf (jstream cls) (usocket:socket-stream (socket cls)))
     (rm-send-command cls (current-meta-process) "" "init" inport)
     (define-module-fct
       (name cls)
       (rm-define-buffers cls)
       (rm-define-params cls)
       :version (version cls)
       :documentation (description cls)
       :reset (list #'(lambda (instance) (rm-send-command cls (current-meta-process) (current-model) "reset" 1)) #'(lambda (instance) (rm-send-command cls (current-meta-process) (current-model) "reset" 2)) #'(lambda (instance) (rm-send-command cls (current-meta-process) (current-model) "reset" 3)))
       :query #'(lambda (instance buffer-name slot value) (rm-send-command cls (current-meta-process) (current-model) "query" (list buffer-name slot value)))
       :request #'(lambda (instance buffer-name chunk-spec) (rm-send-command cls (current-meta-process) (current-model) "request" (list buffer-name (format nil "~a" chunk-spec))))
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
