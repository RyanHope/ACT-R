;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename    : json-interface.lisp                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author      : Ryan M. Hope <rmh3093@gmail.com>
;;
;; Copyright   : (c)2012 Ryan M. Hope
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
  ((sync :accessor sync :initform nil)
   (socket :accessor socket :initform nil)
   (stream :accessor stream :initform nil)
   (thread :accessor thread :initform nil)
   (ready-cond :accessor ready-cond :initform (bordeaux-threads:make-condition-variable))
   (ready-lock :accessor ready-lock :initform (bordeaux-threads:make-lock))
   (time-cond :accessor time-cond :initform (bordeaux-threads:make-condition-variable))
   (time-lock :accessor time-lock :initform (bordeaux-threads:make-lock))
   (display :accessor display :initform nil)
   (cursor-loc :accessor cursor-loc :initform '(0 0))))

(defmethod read-stream ((instance json-interface-module))
  (handler-case
      (loop
        (let* ((s (json:decode-json-from-string (read-line (stream instance))))
               (model (pop s))
               (method (pop s))
               (params (pop s)))
          (declare (ignore model))
          (cond 
           ((string= method "ready")
            (bordeaux-threads:condition-notify (ready-cond instance)))
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
            (new-other-sound (pop params) (pop params) (pop params) (pop params))))))
    (socket-error
     ()
     (print-warning "Socket error..")
     (cleanup instance))
    (end-of-file
     ()
     (print-warning "Remote connection closed.")
     (cleanup instance))))

(defmethod send-command ((instance json-interface-module) mid method &rest params)
  (if (socket instance)
    (progn
      (write-string (json:encode-json-to-string (vector mid method params)) (stream instance))
      (write-char #\return (stream instance))
      (write-char #\linefeed (stream instance))
      (force-output (stream instance)))))

(defmethod send-mp-time ((instance json-interface-module))
  (bordeaux-threads:with-recursive-lock-held 
      ((time-lock instance))
    (progn
      (send-command instance (current-model) "set-mp-time" (mp-time))
      (bordeaux-threads:condition-wait (time-cond instance) (time-lock instance)))))

(defmethod cleanup ((instance json-interface-module))
  (if (socket instance)
    (progn
      (usocket:socket-close (socket instance))
      (setf (socket instance) nil))))

(defmethod device-handle-keypress ((instance json-interface-module) key)
  (send-command instance (current-model) "keypress" (char-code key)))

(defmethod get-mouse-coordinates ((instance json-interface-module))
  (cursor-loc instance))

(defmethod cursor-to-vis-loc ((instance json-interface-module))
  nil)

(defmethod device-move-cursor-to ((instance json-interface-module) loc)
  (send-command instance (current-model) "mousemotion" loc))

(defmethod device-handle-click ((instance json-interface-module))
  (send-command instance (current-model) "mouseclick"))

(defmethod device-speak-string ((instance json-interface-module) msg)
  (send-command instance (current-model) "speak" msg))

(defmethod build-vis-locs-for ((instance json-interface-module) vis-mod)
  (declare (ignore vis-mod))
  (if (display instance)
    (mapcar 'car (display instance))))

(defmethod vis-loc-to-obj ((instance json-interface-module) vis-loc)
  (if (display instance)
    (cdr (assoc vis-loc (display instance)))))

(defun json-interface (host port &key sync)
  (if (current-model)
    (let ((instance (get-module json-interface)))
      (if (socket instance)
        instance
        (handler-case
            (progn
              (setf (sync instance) sync)
              (setf (socket instance) (usocket:socket-connect host port))
              (setf (stream instance) (usocket:socket-stream (socket instance)))
              (setf (thread instance) (bordeaux-threads:make-thread #'(lambda () (read-stream instance))))
              instance)
          (usocket:connection-refused-error
           ()
           (print-warning "Connection refused. Is remote environment server running?")
           nil)
          (usocket:timeout-error
           ()
           (print-warning "Timeout. Is remote environment server running?")
           nil))))))

(defun create-json-netstring-module (name)
  (declare (ignore name))
  (make-instance 'json-interface-module))

(defun reset-json-netstring-module (instance)
  (if (current-model)
    (progn
      (send-command instance (current-model) "reset")
      (cleanup instance))))

(defun delete-json-netstring-module (instance)
  (cleanup instance))

(defun run-start-json-netstring-module (instance)
  (if (current-model)
    (bordeaux-threads:with-recursive-lock-held 
        ((ready-lock instance))
      (progn
        (if (sync instance) (schedule-periodic-event (sync instance) (lambda () (send-mp-time instance)) :maintenance t))
        (send-command instance (current-model) "model-run")
        (bordeaux-threads:condition-wait (ready-cond instance) (ready-lock instance))))))

(defun run-end-json-netstring-module (instance)
  (if (current-model)
    (send-command instance (current-model) "model-stop")))

(define-module json-interface nil nil
  :version "1.0"
  :documentation "Module based manager for remote TCP environments using JSON"
  :creation create-json-netstring-module
  :reset reset-json-netstring-module
  :delete delete-json-netstring-module
  :run-start run-start-json-netstring-module
  :run-end run-end-json-netstring-module)