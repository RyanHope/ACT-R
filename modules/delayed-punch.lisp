;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2009 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : delayed-punch.lisp
;;; Version     : 2.0a1
;;; 
;;; Description : Module which adds another motor : action -- a delayed punch.  
;;;             : The delayed punch generates separate hold and release events 
;;;             : using the extended-motor-actions updates to the motor module
;;;             : with the timing between the actions specified by the model at
;;;             : at the time of the request.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [X] Abstract things better from key-press module code so that
;;;             :     this doesn't directly modify that module's instance.
;;; 
;;; ----- History -----
;;;
;;; 2009.12.01 Dan [1.0a1]
;;;             : * Put this together as a full module instead of just some
;;;             :   code with the space fortress model so that the base times
;;;             :   could be set with parameters.
;;; 2009.12.08 Dan
;;;             : * Had to fix things for the update to key-press that adds the
;;;             :   delay after a press.
;;; 2009.12.16 Dan
;;;             : * Fixed the randomize-time call so that the delay was
;;;             :   randomized properly.
;;; 2010.02.12 Dan
;;;             : * Use the new hold-key finish timing parameter here too.
;;; 2010.03.03 Dan
;;;             : * Added a special version of release-all-keys and modified how
;;;             :   it sets the key down status in the table (:delayed instead
;;;             :   of just t) so that it doesn't try to release a key that's
;;;             :   going to come up on its own.
;;; 2010.09.22 Dan
;;;             : * Fixed a bug with the finish time -- it's been using the 
;;;             :   wrong delay.  Now it's set to 50ms longer than the time it
;;;             :   takes for a hold/release to finish.
;;; 2015.08.06 Dan [2.0a1]
;;;             : * Update this to work with the extended-motor-actions code
;;;             :   in extras since that was based on the key-press module.
;;;             : * The release-all-keys action in the extended-motor-actions
;;;             :   should work ok with this, although it will delay the releasing
;;;             :   of all keys until all delayed actions have completed.
;;;             : * The previous version seemed to split the finish time into
;;;             :   a 50ms fixed cost + a 50ms randomized cost for some reason.
;;; 2015.09.24 Dan [3.0]
;;;             : * Updating so that it completes the requests which means making
;;;             :   sure that the finish action occurs after the key-up to fit
;;;             :   with how we need completed requests to work for this.
;;;             : * The :key-press-finish-lag parameter is now depricated and
;;;             :   has no effect.
;;; 2015.09.25 Dan
;;;             : * Added an ignore to the empty reset function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Adds a new motor module request to give a model the ability to hold down 
;;; and then release a key as a single action with a time that can be specified
;;; by the model.
;;;
;;; Like the key-press module, it's not really based on any research.  It just 
;;; uses the timing for a punch initiation and execution (and shares it's style).
;;; The finish action now takes an additional burst cost after the up stroke.
;;; This differs from before where it could finish prior to the release of the
;;; key.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;  Adds the new motor actions:
;;;
;;;   +manual>
;;;     isa delayed-punch
;;;     hand [ left | right ]
;;;     finger [ index | middle | ring | pinkie | thumb ]
;;;     {delay [ fast | slow | # ] }
;;;
;;; That action will cause the model to press and then release a key as controlled
;;; by the extended-motor-actions extension. The release will come delay seconds 
;;; after hold.  If delay is not provided then the default delay specified with the 
;;; parameter below is used.  If delay is either fast or slow, then the value is 
;;; taken from the corresponding parameter and if it is a number then that will be 
;;; used directly as the delay time.
;;;
;;; If :randomize-time is turned on then the delay time will go through that
;;; randomization.
;;;
;;; There are three parameters for the model which set the times for the
;;; non-numeric delays possible.  The default values have no particular meaning
;;; i.e. they aren't based on any data.  So you'll probably want to set meaningful
;;; values for your task if you use it.
;;;
;;; :default-punch-delay (.075s default)
;;;
;;; This is the delay time when the model doesn't specify a delay.
;;;
;;; :fast-punch-delay (.05s default)
;;;
;;; This is the delay time when the model specifies fast.
;;;
;;; :slow-punch-delay (.1s default)
;;;
;;; This is the delay time when the model specifies slow.
;;;
;;; :key-press-finish-lag (.1 default)
;;;
;;; Depricated and has no effect.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;;
;;; The module just controls the parameters now because the extended-motor-actions
;;; extension handles all the finger control.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;;
;;; Code for the module to hold the new parameters
;;; 

(defstruct d-punch
  default slow fast lag)

(defun reset-delayed-punch-module (module)
  (declare (ignore module)))

(defun handle-delayed-punch-params (instance param)
  (cond ((consp param)
         (case (car param)
           (:default-punch-delay 
            (setf (d-punch-default instance) (cdr param)))
           (:fast-punch-delay
            (setf (d-punch-fast instance) (cdr param)))
           (:slow-punch-delay
            (setf (d-punch-slow instance) (cdr param)))
           (:key-press-finish-lag
            (setf (d-punch-lag instance) (cdr param)))))
        (t
         (case param
           (:default-punch-delay
            (d-punch-default instance))
           (:fast-punch-delay
            (d-punch-fast instance))
           (:slow-punch-delay
            (d-punch-slow instance))
           (:key-press-finish-lag
            (d-punch-lag instance))))))

(define-module-fct :delayed-punch nil
   (list
    (define-parameter :default-punch-delay
      :valid-test #'posnum
      :warning "a number"
      :default-value 0.075
      :documentation "Defualt time for a delayed-punch request.")
    (define-parameter :fast-punch-delay
      :valid-test #'posnum
      :warning "a number"
      :default-value 0.050
      :documentation "Time for a delayed-punch request with a delay of fast.")
    (define-parameter :slow-punch-delay
      :valid-test #'posnum
      :warning "a number"
      :default-value 0.1
      :documentation "Time for a delayed-punch request with a delay of slow.")
    (define-parameter :key-press-finish-lag
      :valid-test #'posnum
      :warning "a number"
      :default-value 0.1
      :documentation "Has no effect."))

  :creation (lambda (name) (declare (ignore name)) (make-d-punch))
  :reset 'reset-delayed-punch-module
  :params #'handle-delayed-punch-params
  :version "3.0"
  :documentation "A module to support a timed punch motor request.")


;;;
;;; Code to implement the requst itself
;;;


;;; Don't use defstyle because I want to share the preparation style name with a punch

(defclass delayed-punch (punch)
  ((delay :accessor delay :initarg :delay :initform .050))
  (:default-initargs
      :finger-based-style t
      :release-if-down :penalty))

(defmethod delayed-punch ((module dual-execution-motor-module) &key hand finger delay request-spec)
  (if (or (check-jam module) (check-specs 'delayed-punch hand finger delay))
      (complete-request request-spec)
    (prepare-movement module (make-instance 'delayed-punch :hand hand :finger finger :delay delay :request-spec request-spec))))

(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self delayed-punch))
  (+ (init-time mtr-mod) (key-closure-time mtr-mod)))

(defmethod compute-second-exec-time ((mtr-mod dual-execution-motor-module) (self delayed-punch))
  (setf (delay self) (randomize-time (delay self)))
  (+ (init-time mtr-mod) 
     (key-closure-time mtr-mod)
     (delay self))) 

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self delayed-punch))
  (+ (init-time mtr-mod) 
     (key-closure-time mtr-mod)
     (delay self)
     (burst-time mtr-mod)))


(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self delayed-punch))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'key-down :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (hand self) (finger self) (finger-loc-m mtr-mod (hand self) (finger self))))

  (schedule-event-relative (seconds->ms (exec2-time self))
                           'key-up :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (hand self) (finger self) (finger-loc-m mtr-mod (hand self) (finger self)))))


(defun parse-delay-time (module delay)
  (cond ((or (null delay) (eq delay 'default)) (d-punch-default module))
        ((and (numberp delay) (posnum delay))
         delay)
        ((numberp delay)
         (model-warning "Non-positive delays not allowed in a delayed-punch. Using default instead.")
         (d-punch-default module))
        ((eq delay 'slow) (d-punch-slow module))
        ((eq delay 'fast) (d-punch-fast module))
        (t (model-warning "Invalid delay value ~s to a delayed-punch. Using default instead" delay)
           (d-punch-default module))))

(defun handle-delayed-punch-request (motor chunk-spec)
  (declare (ignore motor))
  (let* ((hand (verify-single-explicit-value 
                    chunk-spec 'hand
                    :motor 'delayed-punch))
         (finger (verify-single-explicit-value 
                      chunk-spec 'finger
                      :motor 'delayed-punch))
         (d-punch (get-module :delayed-punch))
         (delay (if (slot-in-chunk-spec-p chunk-spec 'delay)
                    (awhen (verify-single-explicit-value 
                            chunk-spec 'delay
                            :motor 'delayed-punch)
                           (parse-delay-time d-punch it))
                  (d-punch-default d-punch))))
    
        
    (if (and hand finger delay)
        ;(if (finger-down hand finger)
        ;    (print-warning "Delayed-punch action ignored because the ~s ~s finger is already held down." hand finger)
        ;; allow it to automatically release first and then
        ;; tap again 
        
        (schedule-event-now 'delayed-punch
                            :destination :motor
                            :params (list :hand hand :finger finger :delay delay :request-spec chunk-spec)
                            :module :motor
                            :output 'low
                            :details (format nil "~a ~a ~a ~a ~a ~a ~a" 'delayed-punch :hand hand :finger finger :delay delay))
      ;)
      (progn
        (complete-request chunk-spec)
        (print-warning "Invalid delayed-punch request with chunk-spec ~S" chunk-spec)))))


(extend-manual-requests (delayed-punch hand finger delay) handle-delayed-punch-request)



#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
