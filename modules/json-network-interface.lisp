;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename    : json-network-interface.lisp                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author      : Ryan M. Hope <rmh3093@gmail.com>
;;
;; Copyright   : (c)2012-2013 Ryan M. Hope
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-jni-bundle
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-file-pathname* nil))
    (asdf:load-system :usocket)
    (asdf:load-system :bordeaux-threads)
    (asdf:load-system :jsown)))

#+:packaged-actr (in-package :act-r)
#-:packaged-actr (in-package :cl-user)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defclass json-interface-module ()
  ((jni-hostname :accessor jni-hostname :initform nil)
   (jni-port :accessor jni-port :initform nil)
   (jni-sync :accessor jni-sync :initform nil)
   (event-hooks :accessor event-hooks :initform (make-hash-table))
   (sync-event :accessor sync-event :initform nil)
   (socket :accessor socket :initform nil)
   (jstream :accessor jstream :initform nil)
   (thread :accessor thread :initform nil)
   (sync-cond :accessor sync-cond :initform (bordeaux-threads:make-condition-variable))
   (sync-lock :accessor sync-lock :initform (bordeaux-threads:make-lock))
   (display :accessor display :initform nil)
   (cursor-loc :accessor cursor-loc :initform '(0 0))
   (width :accessor width :initform 0)
   (height :accessor height :initform 0)
   (running :accessor running :initform nil)
   (wait :accessor wait :initform nil)))

(defun parse->json-chunk (jsown-obj)
  (let* ((keys (jsown:keywords jsown-obj))
         (name (if (find "name" (jsown:keywords jsown-obj) :test #'equal) 
                   (read-from-string (jsown:val jsown-obj "name"))
                 nil))
         (typ (read-from-string (jsown:val jsown-obj "isa")))
         (slots (jsown:val jsown-obj "slots"))
         (type-expression (if name
                              `(,name isa ,typ)
                            `(isa ,typ))))
    (loop for slot-name in (jsown:keywords slots) do
          (let* ((slot-value (jsown:val slots slot-name))
                 (slot-name (read-from-string slot-name)))
            (cond ((and (stringp slot-value) (equalp (char slot-value 0) #\:))
                   (setf slot-value (read-from-string (subseq slot-value 1))))
                  ((and (numberp slot-value) (member slot-name '(screen-x screen-y width height) :test 'equal))
                   (setf slot-value (round slot-value))))
            (setq type-expression (append type-expression `(,slot-name ,slot-value)))))
    type-expression))

(defun json->chunkpairs (loc-chunks obj-chunks)
  (pairlis (define-chunks-fct (mapcar 'parse->json-chunk loc-chunks)) 
           (define-chunks-fct (mapcar 'parse->json-chunk obj-chunks))))

(defun json->chunkpair (loc-chunk obj-chunk)
  '((define-chunks-fct (parse->json-chunk loc-chunk)) 
    (define-chunks-fct (parse->json-chunk obj-chunk))))

(defun update-display-chunks (chunks)
  (loop for chunk in chunks do
        (let ((name (read-from-string (jsown:val chunk "name")))
              (slots (jsown:val chunk "slots")))
          (loop for slot-name in (jsown:keywords slots) do
                (let* ((slot-value (jsown:val slots slot-name))
                       (slot-name (read-from-string slot-name)))
                  (cond ((and (stringp slot-value) (equalp (char slot-value 0) #\:))
                         (setf slot-value (read-from-string (subseq slot-value 1))))
                        ((and (numberp slot-value) (member slot-name '(screen-x screen-y width height) :test 'equal))
                         (setf slot-value (round slot-value))))
                  (set-chunk-slot-value-fct name slot-name slot-value))))))      

(defmethod read-stream ((instance json-interface-module))
  (handler-case
      (loop
       (if (listen (jstream instance))
           (let ((line (read-line (jstream instance))))
             (if line
                 (let* ((o (jsown:parse line))
                        (model (jsown:val o "model"))
                        (method (jsown:val o "method"))
                        (params (jsown:val o "params")))
                   (cond 
                    ((string= method "disconnect")
                     (return))
                    ((string= method "trigger-event")
                     (let ((callback (gethash (read-from-string (jsown:val params "event")) (event-hooks instance))))
                       (if callback
                           (apply callback (jsown:val params "args")))))
                    ((string= method "setup")
                     (setf (width instance) (jsown:val params "width"))
                     (setf (height instance) (jsown:val params "height")))
                    ((string= method "sync")
                     (progn
                       (setf (wait instance) nil)
                       (bordeaux-threads:condition-notify (sync-cond instance))))
                    ((string= method "update-display")
                     (print-warning "The use of JNI command 'update-display' is deprecated, use display-new instead.")
                     (schedule-event-relative 0 (lambda ()
                                                  (progn
                                                    (setf (display instance) (json->chunkpairs (jsown:val params "loc-chunks")
                                                                                               (jsown:val params "obj-chunks")))
                                                    (proc-display :clear (jsown:val params "clear"))))))
                    ((string= method "display-new")
                     (schedule-event-relative 0 (lambda ()
                                                  (progn
                                                    (setf (display instance) (json->chunkpairs (jsown:val params "loc-chunks")
                                                                                               (jsown:val params "obj-chunks")))
                                                    (proc-display :clear t)))))
                    ((string= method "display-add")
                     (schedule-event-relative 0 (lambda ()
                                                  (progn
                                                    (setf (display instance) (cons (json->chunkpair (jsown:val params "loc-chunk") 
                                                                                                    (jsown:val params "obj-chunk"))
                                                                                   (display instance)))
                                                    (proc-display :clear nil)))))
                    ((string= method "display-remove")
                     (schedule-event-relative 0 (lambda ()
                                                  (progn
                                                    (setf (display instance) (remove (jsown:val params "loc-chunk-name") (display instance) :key #'car))
                                                    (proc-display :clear nil)))))
                    ((string= method "display-update")
                     (schedule-event-relative 0 (lambda ()
                                                  (progn
                                                    (update-display-chunks (jsown:val params "chunks"))
                                                    (proc-display :clear (proc-display :clear (jsown:val params "clear")))))))
                    ((string= method "trigger-reward")
                     (trigger-reward (jsown:val params "reward")))
                    ((string= method "set-cursor-loc")
                     (setf (cursor-loc instance) (jsown:val params "loc")))
                    ((string= method "new-digit-sound")
                     (new-digit-sound (jsown:val params "digit")))
                    ((string= method "new-tone-sound")
                     (new-tone-sound (jsown:val params "frequency") (jsown:val params "duration")))
                    ((string= method "new-word-sound")
                     (new-word-sound (jsown:val params "words")))
                    ((string= method "new-other-sound")
                     (new-other-sound (jsown:val params "content") (jsown:val params "onset") (jsown:val params "delay") (jsown:val params "recode")))))
               (return-from read-stream "Nothing to read")))))
    (usocket:socket-error () (return-from read-stream "Socket error"))
    (end-of-file () (return-from read-stream "End of file"))))

(defmethod cleanup ((instance json-interface-module))
  (if (jstream instance)
      (close (jstream instance)))
  (if (socket instance)
      (usocket:socket-close (socket instance)))
  (if (sync-event instance)
      (delete-event (sync-event instance)))
  (bordeaux-threads:condition-notify (sync-cond instance))
  (setf (jstream instance) nil)
  (setf (socket instance) nil)
  (setf (thread instance) nil)
  (setf (sync-event instance) nil))

(defmethod send-raw ((instance json-interface-module) string)
  (write-string string (jstream instance))
  (write-char #\return (jstream instance))
  (write-char #\linefeed (jstream instance))
  (finish-output (jstream instance)))

(defmethod send-command ((instance json-interface-module) method params &key sync)
  (let ((mid (format nil "~a" (current-model))))
    (bordeaux-threads:with-recursive-lock-held 
        ((sync-lock instance))
      (progn
        (setf (wait instance) t)
        (send-raw instance (jsown:to-json (jsown:new-js ("model" mid) ("method" method) ("params" params))))
        (if (and sync (wait instance))
            (bordeaux-threads:condition-wait (sync-cond instance) (sync-lock instance)))))))

(defmethod send-mp-time ((instance json-interface-module))
  (if (jstream instance)
      (send-command instance "set-mp-time" (jsown:new-js ("time" (mp-time))) :sync t)))

(defmethod device-handle-keypress ((instance json-interface-module) key)
  (send-command instance "keypress" (jsown:new-js ("keycode" (char-code key)))
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-move-cursor-to ((instance json-interface-module) loc)
  (send-command instance "mousemotion" (jsown:new-js ("loc" (list (aref loc 0) (aref loc 1))))
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-handle-click ((instance json-interface-module))
  (send-command instance "mouseclick" (jsown:new-js ("button" 1))
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-speak-string ((instance json-interface-module) msg)
  (send-command instance "speak" (jsown:new-js ("message" msg))
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-update-eye-loc ((instance json-interface-module) loc)
  (when loc (setf loc (list (aref loc 0) (aref loc 1))))
  (send-command instance "gaze-loc" (jsown:new-js ("loc" loc))
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-update-attended-loc ((instance json-interface-module) loc)
  (when loc (setf loc (list (aref loc 0) (aref loc 1))))
  (send-command instance "attention-loc" (jsown:new-js ("loc" loc))
                :sync (not (numberp (jni-sync instance)))))

(defmethod disconnect ((instance json-interface-module))
  (send-command instance "disconnect" nil)
  (if (thread instance)
      (bordeaux-threads:join-thread (thread instance))))

(defmethod get-mouse-coordinates ((instance json-interface-module))
  (cursor-loc instance))

(defmethod cursor-to-vis-loc ((instance json-interface-module))
  nil)

(defmethod build-vis-locs-for ((instance json-interface-module) vis-mod)
  (declare (ignore vis-mod))
  (if (display instance)
    (mapcar 'car (display instance))))

(defmethod vis-loc-to-obj ((instance json-interface-module) vis-loc)
  (if (display instance)
    (cdr (assoc vis-loc (display instance)))))

(defun create-json-netstring-module (name)
  (declare (ignore name))
  (make-instance 'json-interface-module))

(defun reset-json-netstring-module (instance)
  (setf (running instance) nil)
  (if (and (socket instance) (jstream instance) (thread instance))
      (progn
        (send-command instance "reset" (jsown:new-js ("time-lock" (numberp (jni-sync instance)))) :sync t)
        (install-device instance))
    (if (and (current-model) (jni-hostname instance) (jni-port instance))
        (connect instance))))

(defun delete-json-netstring-module (instance)
  (if (socket instance)
      (disconnect instance)))

(defmethod connect ((instance json-interface-module))
  (handler-case
      (progn
        (setf (socket instance) (usocket:socket-connect (jni-hostname instance) (jni-port instance)))
        (setf (jstream instance) (usocket:socket-stream (socket instance)))
        (setf (thread instance) (bordeaux-threads:make-thread #'(lambda () (read-stream instance))))
        (install-device instance))
    (usocket:socket-error () 
      (progn
        (print-warning "Socket error")
        (cleanup instance)
        (return-from connect)))))

(defun run-start-json-netstring-module (instance)
  (if (and (socket instance) (current-model))
      (progn
        (if (numberp (jni-sync instance))
            (setf (sync-event instance)
                  (schedule-periodic-event (jni-sync instance) (lambda () (send-mp-time instance)) :maintenance t)))
        (send-command instance "model-run" (jsown:new-js ("resume" (running instance))) :sync t)
        (setf (running instance) t))))

(defun run-end-json-netstring-module (instance)
  (if (and (socket instance) (current-model))
      (progn
        (if (sync-event instance)
            (delete-event (sync-event instance)))
        (send-command instance "model-stop" nil))))

(defun jni-register-event-hook (event hook)
  (setf (gethash event (event-hooks (get-module json-network-interface))) hook))
  
(defun params-json-netstring-module (instance param)
  (if (consp param)
      (let ((hostname (jni-hostname instance))
            (port (jni-port instance)))
        (progn
          (let ((ret nil))
            (case (car param)
              (:jni-hostname (setf ret (setf (jni-hostname instance) (cdr param))))
              (:jni-port (setf ret (setf (jni-port instance) (cdr param))))
              (:jni-sync (setf ret (setf (jni-sync instance) (cdr param)))))
            (if (and (jni-hostname instance) (jni-port instance))
                (if (or (not (string= hostname (jni-hostname instance))) (not (equal port (jni-port instance))))
                    (connect instance)))
            ret)))
    (case param
      (:jni-hostname (jni-hostname instance))
      (:jni-port (jni-port instance))
      (:jni-sync (jni-sync instance)))))

(undefine-module jni)
(define-module-fct 
 'json-network-interface 
 nil
 (list (define-parameter :jni-hostname
                         :documentation "The hostname/fqdn of the remote environment")
       (define-parameter :jni-port
                         :documentation "The port number of the remote environment")
       (define-parameter :jni-sync
                         :documentation "The timing mode of the model. Use nil for asynchronous mode, t for synchronous mode and any positive number for time-locked mode."))
 :version "1.0"
 :documentation "Module based manager for remote TCP environments using JSON"
 :params 'params-json-netstring-module
 :creation 'create-json-netstring-module
 :reset (list nil nil 'reset-json-netstring-module)
 :delete 'delete-json-netstring-module
 :run-start 'run-start-json-netstring-module
 :run-end 'run-end-json-netstring-module
 :update nil)
