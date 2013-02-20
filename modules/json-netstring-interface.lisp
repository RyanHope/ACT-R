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
;; TODOs       : - Handle 'cursor-to-vis-loc' properly
;;               - Add support for 'onset' param for new-*-sound commands
;;               - Support multiple simultaneous models
;;               - Add support for PAAV module
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-file-pathname* nil))
    (asdf:load-system :usocket)
    (asdf:load-system :bordeaux-threads)
    (asdf:load-system :cl-json)))

(defclass json-interface-module ()
  ((jni-hostname :accessor jni-hostname :initform nil)
   (jni-port :accessor jni-port :initform nil)
   (jni-sync :accessor jni-sync :initform nil)
   (sync-event :accessor sync-event :initform nil)
   (socket :accessor socket :initform nil)
   (jstream :accessor jstream :initform nil)
   (thread :accessor thread :initform nil)
   (ready-cond :accessor ready-cond :initform (bordeaux-threads:make-condition-variable))
   (ready-lock :accessor ready-lock :initform (bordeaux-threads:make-lock))
   (sync-cond :accessor sync-cond :initform (bordeaux-threads:make-condition-variable))
   (sync-lock :accessor sync-lock :initform (bordeaux-threads:make-lock))
   (reset-cond :accessor reset-cond :initform (bordeaux-threads:make-condition-variable))
   (reset-lock :accessor reset-lock :initform (bordeaux-threads:make-lock))
   (time-cond :accessor time-cond :initform (bordeaux-threads:make-condition-variable))
   (time-lock :accessor time-lock :initform (bordeaux-threads:make-lock))
   (display :accessor display :initform nil)
   (cursor-loc :accessor cursor-loc :initform '(0 0))))

(defmethod read-stream ((instance json-interface-module))
  (handler-case
      (loop
       (if (usocket:wait-for-input (list (socket instance)) :timeout 1)
           (let ((line (read-line (jstream instance))))
             (if line
                 (let* ((o (json:decode-json-from-string line))
                        (model (pop o))
                        (method (pop o))
                        (params (pop o)))
                   (cond 
                    ((string= method "disconnect")
                     (return))
                    ((string= method "sync")
                     (bordeaux-threads:condition-notify (sync-cond instance)))
                    ((string= method "ready")
                     (bordeaux-threads:condition-notify (ready-cond instance)))
                    ((string= method "reset")
                     (bordeaux-threads:condition-notify (reset-cond instance)))
                    ((string= method "time-set")
                     (bordeaux-threads:condition-notify (time-cond instance)))
                    ((string= method "update-display")
                     (progn
                       (setf (display instance)
                             (pairlis (eval (read-from-string (pop params)))
                                      (eval (read-from-string (pop params)))))
                       (proc-display :clear (pop params))))
                    ((string= method "trigger-reward")
                     (trigger-reward (pop params)))
                    ((string= method "set-visual-center-point")
                     (set-visual-center-point (pop params) (pop params)))
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
               (return)))
         (return)))
    (usocket:bad-file-descriptor-error () (print-warning "Bad file descriptor..."))
    (usocket:socket-error () (print-warning "Socket error..."))
    (end-of-file () (print-warning "End of file...")))
  (cleanup instance))

(defmethod send-raw ((instance json-interface-module) string)
  (write-string string (jstream instance))
  (write-char #\return (jstream instance))
  (write-char #\linefeed (jstream instance))
  (force-output (jstream instance)))

(defmethod send-command ((instance json-interface-module) mid method &rest params)
  (send-raw instance (json:encode-json-to-string (vector mid method params))))

(defmethod send-command-sync ((instance json-interface-module) mid method &rest params)
  (send-raw instance (json:encode-json-to-string (vector mid method params)))
  (if (and (jni-sync instance) (not (numberp (jni-sync instance))))
      (bordeaux-threads:with-recursive-lock-held 
          ((sync-lock instance))
        (bordeaux-threads:condition-wait (sync-cond instance) (sync-lock instance)))))

(defmethod send-mp-time ((instance json-interface-module))
  (bordeaux-threads:with-recursive-lock-held 
      ((time-lock instance))
    (progn
      (send-command instance (current-model) "set-mp-time" (mp-time))
      (bordeaux-threads:condition-wait (time-cond instance) (time-lock instance)))))

(defmethod cleanup ((instance json-interface-module))
  (if (jstream instance)
      (close (jstream instance)))
  (if (socket instance)
      (usocket:socket-close (socket instance)))
  (if (sync-event instance)
      (delete-event (sync-event instance)))
  (bordeaux-threads:condition-notify (ready-cond instance))
  (bordeaux-threads:condition-notify (reset-cond instance))
  (bordeaux-threads:condition-notify (time-cond instance))
  (setf (jstream instance) nil)
  (setf (socket instance) nil)
  (setf (thread instance) nil)
  (setf (sync-event instance) nil))

(defmethod device-handle-keypress ((instance json-interface-module) key)
  (send-command-sync instance (current-model) "keypress" (char-code key)))

(defmethod get-mouse-coordinates ((instance json-interface-module))
  (cursor-loc instance))

(defmethod cursor-to-vis-loc ((instance json-interface-module))
  nil)

(defmethod device-move-cursor-to ((instance json-interface-module) loc)
  (send-command-sync instance (current-model) "mousemotion" loc))

(defmethod device-handle-click ((instance json-interface-module))
  (send-command-sync instance (current-model) "mouseclick"))

(defmethod device-speak-string ((instance json-interface-module) msg)
  (send-command-sync instance (current-model) "speak" msg))

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
  (if (and (socket instance) (jstream instance) (thread instance))
      (bordeaux-threads:with-recursive-lock-held
          ((reset-lock instance))
        (progn
          (send-command instance (current-model) "reset")
          (bordeaux-threads:condition-wait (reset-cond instance) (reset-lock instance))
          (install-device instance)))
    (if (and (current-model) (jni-hostname instance) (jni-port instance))
        (handler-case
            (progn
              (setf (socket instance) (usocket:socket-connect (jni-hostname instance) (jni-port instance)))
              (setf (jstream instance) (usocket:socket-stream (socket instance)))
              (setf (thread instance) (bordeaux-threads:make-thread #'(lambda () (read-stream instance))))
              (install-device instance))
          (usocket:connection-refused-error () 
            (progn
              (print-warning "Connection refused. Is remote environment server running?")
              (cleanup instance)))
          (usocket:timeout-error () 
            (progn
              (print-warning "Timeout. Is remote environment server running?")
              (cleanup instance)))))))

(defun delete-json-netstring-module (instance)
  (cleanup instance))

(defun run-start-json-netstring-module (instance)
  (if (current-model)
      (progn
        (bordeaux-threads:with-recursive-lock-held
            ((ready-lock instance))
          (progn
            (if (numberp (jni-sync instance))
                (setf (sync-event instance)
                      (schedule-periodic-event (jni-sync instance) (lambda () (send-mp-time instance)) :maintenance t)))
            (send-command instance (current-model) "model-run")
            (bordeaux-threads:condition-wait (ready-cond instance) (ready-lock instance)))))))

(defun run-end-json-netstring-module (instance)
  (if (current-model)
      (progn
        (if (sync-event instance)
            (delete-event (sync-event instance)))
        (send-command instance (current-model) "model-stop"))))

(defun params-json-netstring-module (instance param)
  (if (consp param)
      (case (car param)
        (:jni-hostname (setf (jni-hostname instance) (cdr param)))
        (:jni-port (setf (jni-port instance) (cdr param)))
        (:jni-sync (setf (jni-sync instance) (cdr param))))
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
