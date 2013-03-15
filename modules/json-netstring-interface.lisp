;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename    : json-interface.lisp                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author      : Ryan M. Hope <rmh3093@gmail.com>
;;
;; Copyright   : (c)2012-2013 Ryan M. Hope
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Description : This module provides a way for any any remote environment to
;;               interface with ACT-R over a TCP connection.
;;
;;               Each TCP call is comprised of a JSON encoded array terminated
;;               with a carraige return and linefeed. The array has 3 required
;;               elements:
;;                   1) The name or id of current model 
;;                   2) The method being invoked
;;                   3) An array of optional parameters for the invoked method
;;
;;               Ex: "[\"model1\",\"device-move-cursor-to\",[[234,45]]]"
;;
;;               The remote environment should implement a server interface
;;               which listens for connections from ACT-R on a given port. This
;;               module implements a client which will connect to the remote
;;               environment when the device is installed.
;;
;;               All requests for information made by the model are fulfilled
;;               with data stored in the module. It is the responsibility of
;;               the remote environment server to update module when things in
;;               the environment change.
;;
;;               All actions performed by the model on the environment are sent
;;               directly to the remote server.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODOs       : - Support chunk names
;;               - Handle 'cursor-to-vis-loc' properly
;;               - Add support for 'onset' param for new-*-sound commands
;;               - Support multiple simultaneous models
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-file-pathname* nil))
    (asdf:load-system :usocket)
    (asdf:load-system :bordeaux-threads)
    (asdf:load-system :jsown)))

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
   (running :accessor running :initform nil)))

(defun json->chunk (lst-of-lsts)
  "generate one define-chunks call from a list of chunk specs"
  (let ((expressions nil))
    (dolist (lst lst-of-lsts)
      (cond ((eql :obj (first lst))
             (let ((typ  (read-from-string (cdr (first (rest lst)))))
                   (slots (rest (rest lst)))) 
               (let ((type-expression `(isa ,typ)))
                 (dolist (s slots)
                   (let ((name-values (subseq s 2)))
                     (dolist (n-v name-values)
                       (let* ((n (read-from-string (car n-v)))
                              (v1 (cdr n-v))
                              (v (if (stringp v1)
                                     (if (equalp (char v1 0) #\:)
                                         (read-from-string (subseq v1 1))
                                       v1)
                                   v1)))
                         (if (and (numberp v) (member n '(screen-x screen-y width height) :test 'equal))
                             (setf v (round v)))
                         (setq type-expression (append type-expression `(,n ,v)))))))
                 (setq expressions (append expressions (list type-expression))))))))
   ; `(define-chunks ,@expressions)
    (funcall 'define-chunks-fct expressions)  ;;
    ))

(defun json->chunkpairs (lst-of-lsts)
  "iterate through each list in in list of json objects returns dotted lists of visloc visobj pairs"
  (let ((expressions nil))
    (dolist (lst lst-of-lsts)
      (setq expressions (append expressions (list (json->chunk lst)))))
    ;expressions
    (pairlis (first expressions ) (second expressions ))))

(defmethod read-stream ((instance json-interface-module))
  (handler-case
      (loop
       (let ((line (read-line (jstream instance))))
         (if line
             (let* ((o (jsown:parse line))
                    (model (first o))
                    (method (second o))
                    (params (third o)))
               (cond 
                ((string= method "disconnect")
                 (return))
                ((string= method "trigger-event")
                 (let ((callback (gethash (read-from-string (pop params)) (event-hooks instance))))
                   (if callback
                       (apply callback params))))
                ((string= method "setup")
                 (setf (width instance) (pop params))
                 (setf (height instance) (pop params)))
                ((string= method "sync")
                 (bordeaux-threads:condition-notify (sync-cond instance)))
                ((string= method "update-display")
                 (progn
                   (setf (display instance) (json->chunkpairs (pop params)))
                   (proc-display :clear (pop params))))
                ((string= method "trigger-reward")
                 (trigger-reward (pop params)))
                ((string= method "set-cursor-loc")
                 (setf (cursor-loc instance) (pop params)))
                ((string= method "new-digit-sound")
                 (new-digit-sound (pop params)))
                ((string= method "new-tone-sound")
                 (new-tone-sound (pop params) (pop params)))
                ((string= method "new-word-sound")
                 (new-word-sound (pop params)))
                ((string= method "new-other-sound")
                 (new-other-sound (pop params) (pop params) (pop params) (pop params)))))
           (return-from read-stream "Nothing to read"))))
    (usocket:connection-aborted-error () (return-from read-stream "Connection aborted"))
    (usocket:connection-reset-error () (return-from read-stream "Connection reset"))
    (usocket:bad-file-descriptor-error () (return-from read-stream "Bad file descriptor"))
    (usocket:socket-error () (return-from read-stream "Socket error"))
    (end-of-file () (return-from read-stream "End of file"))))

(defmethod send-raw ((instance json-interface-module) string)
  (write-string string (jstream instance))
  (write-char #\return (jstream instance))
  (write-char #\linefeed (jstream instance))
  (force-output (jstream instance)))

(defmethod send-command ((instance json-interface-module) mid method params &key sync)
  (let ((mid (format nil "~a" (current-model))))
    (send-raw instance (jsown:to-json (list mid method params)))
    (if sync
        (bordeaux-threads:with-recursive-lock-held 
            ((sync-lock instance))
          (bordeaux-threads:condition-wait (sync-cond instance) (sync-lock instance))))))

(defmethod send-mp-time ((instance json-interface-module))
  (if (jstream instance)
      (send-command instance (current-model) "set-mp-time" (list (mp-time)) :sync t)))
      
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

(defmethod device-handle-keypress ((instance json-interface-module) key)
  (send-command instance (current-model) "keypress" (list (char-code key))
                :sync (not (numberp (jni-sync instance)))))

(defmethod get-mouse-coordinates ((instance json-interface-module))
  (cursor-loc instance))

(defmethod cursor-to-vis-loc ((instance json-interface-module))
  nil)

(defmethod device-move-cursor-to ((instance json-interface-module) loc)
  (send-command instance (current-model) "mousemotion" (list (list (aref loc 0) (aref loc 1)))
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-handle-click ((instance json-interface-module))
  (send-command instance (current-model) "mouseclick" 1
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-speak-string ((instance json-interface-module) msg)
  (send-command instance (current-model) "speak" (list msg)
                :sync (not (numberp (jni-sync instance)))))

(defmethod build-vis-locs-for ((instance json-interface-module) vis-mod)
  (declare (ignore vis-mod))
  (if (display instance)
    (mapcar 'car (display instance))))

(defmethod vis-loc-to-obj ((instance json-interface-module) vis-loc)
  (if (display instance)
    (cdr (assoc vis-loc (display instance)))))

(defmethod device-update-eye-loc ((instance json-interface-module) loc)
  (when loc (setf loc (list (aref loc 0) (aref loc 1))))
  (send-command instance (current-model) "gaze-loc" (list loc)
                :sync (not (numberp (jni-sync instance)))))

(defmethod device-update-attended-loc ((instance json-interface-module) loc)
  (when loc (setf loc (list (aref loc 0) (aref loc 1))))
  (send-command instance (current-model) "attention-loc" (list loc)
                :sync (not (numberp (jni-sync instance)))))

(defmethod disconnect ((instance json-interface-module))
  (send-command instance (current-model) "disconnect" nil)
  (if (thread instance)
      (bordeaux-threads:join-thread (thread instance))))

(defun create-json-netstring-module (name)
  (declare (ignore name))
  (make-instance 'json-interface-module))

(defun reset-json-netstring-module (instance)
  (setf (running instance) nil)
  (if (and (socket instance) (jstream instance) (thread instance))
      (progn
        (send-command instance (current-model) "reset" (list (numberp (jni-sync instance))) :sync t)
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
    (usocket:connection-refused-error () 
      (progn
        (print-warning "Connection refused. Is remote environment server running?")
        (cleanup instance)
        (return-from connect)))
    (usocket:timeout-error () 
      (progn
        (print-warning "Timeout. Is remote environment server running?")
        (cleanup instance)
        (return-from connect)))))

(defun run-start-json-netstring-module (instance)
  (if (current-model)
      (progn
        (if (numberp (jni-sync instance))
            (setf (sync-event instance)
                  (schedule-periodic-event (jni-sync instance) (lambda () (send-mp-time instance)) :maintenance t)))
        (send-command instance (current-model) "model-run" (list (running instance)) :sync t)
        (setf (running instance) t))))

(defun run-end-json-netstring-module (instance)
  (if (current-model)
      (progn
        (if (sync-event instance)
            (delete-event (sync-event instance)))
        (send-command instance (current-model) "model-stop" nil))))

(defun jni-register-event-hook (event hook)
  (setf (gethash event (event-hooks (get-module json-interface))) hook))
  
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

(define-module-fct 'json-interface nil
                   (list (define-parameter :jni-hostname)
                         (define-parameter :jni-port)
                         (define-parameter :jni-sync))
                   :version "2.0"
                   :documentation "Module based manager for remote TCP environments using JSON"
                   :params 'params-json-netstring-module
                   :creation 'create-json-netstring-module
                   :reset (list nil nil 'reset-json-netstring-module)
                   :delete 'delete-json-netstring-module
                   :run-start 'run-start-json-netstring-module
                   :run-end 'run-end-json-netstring-module)
