;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename    : ocular-motor.lisp                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author      : Ryan M. Hope <rmh3093@gmail.com>
;;
;; Copyright   : (c)2013 Ryan M. Hope
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Description : This module handles the eye-movement operations of the 
;;               superior colliculi, dorsolateral prefrontal cortex, 
;;               frontal eye fields, and cerebellum.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODOs       : 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ocular-motor-module () ())

(defun create-json-netstring-module (name)
  (declare (ignore name))
  (make-instance 'ocular-motor-module))

(defun reset-ocular-motor-module (instance)
  (declare (ignore instance)))

(defun delete-ocular-motor-module (instance)
  (declare (ignore instance)))

(defun run-start-ocular-motor-module (instance)
  (declare (ignore instance)))

(defun run-end-ocular-motor-module (instance)
  (if (current-model)
    (send-command instance (current-model) "model-stop")))

(define-module ocular-motor nil nil
  :version "1.0"
  :documentation "Ocular-Motor Module"
  :creation create-ocular-motor-module
  :reset reset-ocular-motor-module
  :delete delete-ocular-motor-module
  :run-start run-ocular-motor-module
  :run-end run-end-ocular-motor-module)
