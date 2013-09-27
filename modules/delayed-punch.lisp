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
;;; Version     : 1.0a1
;;; 
;;; Description : Module which extends the key-press module to add another motor
;;;             : action -- a delayed punch.  The delayed punch generates separate
;;;             : hold and release events through the key-press module's methods
;;;             : and the timing between them is under the model's control.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Abstract things better from key-press module code so that
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Adds a new motor module request to give a model the ability to hold down 
;;; and then release a key as a single action with a time that can be specified
;;; by the model.
;;;
;;; Like the key-press module, it's not really based on any research.  It just 
;;; uses the timing for a punch initiation and execution (and shares it's style)
;;; but the finish action only spans 100ms from beginning of initiation (which
;;; would put it 40ms after the down with default timing).  That means that
;;; the module may be freed up before the actual up stroke of the finger which
;;; allows for more overlapping of finger usage, but may requre more checks in
;;; the module to avoid jamming individual fingers.
;;;
;;; Since it uses the key-press module it's not tied into any of the built-in 
;;; devices or the "real" keyboard actions that can be generated using some 
;;; Lisps by default.  
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
;;; That action will call the methods device-hold-finger and device-release-finger
;;; with the device, hand, and finger as parameters when the action is executed
;;; and then released.  The release will come delay seconds after hold.  If delay
;;; is not provided then the default delay specified with the parameter below is
;;; used.  If delay either fast or slow, then the value is taken from the corresponding
;;; parameter and if it is a number then that will be used directly as the delay time.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
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
;;; Code for the module to hold the parameters
;;; 

(defstruct d-punch
  default slow fast lag table)

(defun reset-delayed-punch-module (module)
  (clrhash (d-punch-table module)))

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
             (d-punch-slow instance))))))

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
      :owner nil))

  :creation (lambda (name) (declare (ignore name)) (make-d-punch :table (make-hash-table :size 12 :test #'equalp)))
  :reset 'reset-delayed-punch-module
  :params #'handle-delayed-punch-params
  :version "1.0a1"
  :documentation "A module to support a timed punch motor request.")


;;;
;;; Code to implement the requst itself
;;;


;;; Like with the key-press actions don't use defstyle because
;;; I want to share the preparation style name with a punch

(defclass delayed-punch (punch)
  ((hand :accessor hand :initarg :hand :initform nil)
   (finger :accessor finger :initarg :finger :initform nil)
   (delay :accessor delay :initarg :delay :initform .050)
   )
  (:default-initargs :style-name :punch :feature-slots '(hand finger)))

(defmethod delayed-punch ((module pm-module) &key hand finger delay)
  (unless (or (check-jam module) (check-specs 'hold-key hand finger delay))
    (prepare-movement module (make-instance 'delayed-punch :hand hand :finger finger :delay delay))))


(defmethod compute-exec-time ((mtr-mod motor-module) (self delayed-punch))
  (+ (init-time mtr-mod) (key-closure-time (current-device-interface))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self delayed-punch))
  (let ((instance (get-module :delayed-punch)))
    (+ .05 (init-time mtr-mod) (burst-time mtr-mod) (randomize-time (d-punch-lag instance)))))


(defmethod queue-output-events ((mtr-mod motor-module) (self delayed-punch))
  (schedule-event-relative (exec-time self) 'held-key :module :motor 
                           :output 'medium  :destination :key-press
                           :params (list (hand self) (finger self)))
  
  (let ((release-time (+ (exec-time self) (randomize-time (delay self)))))
    (schedule-event-relative release-time 'released-key :module :motor 
                             :destination :key-press :output 'medium  
                             :params (list (hand self) (finger self)))
  
    (schedule-event-relative (+ release-time (d-punch-lag (get-module :delayed-punch))) 'delay-over :module :motor :destination :delayed-punch :params (list (hand self) (finger self)))))


(defun delay-over (module hand finger)
  (setf (gethash (list hand finger) (d-punch-table module)) nil)
  (free-finger (get-module :key-press) hand finger))

(defun parse-delay-time (delay)
  (let ((module (get-module :delayed-punch)))
    (cond ((or (null delay) (eq delay 'default)) (d-punch-default module))
          ((and (numberp delay) (posnum delay))
               delay)
          ((numberp delay)
           (model-warning "Non-positive delays not allowed in a delayed-punch using default.")
           (d-punch-default module))
          ((eq delay 'slow) (d-punch-slow module))
          ((eq delay 'fast) (d-punch-fast module)))))

(defun handle-delayed-punch-request (motor chunk-spec)
  (declare (ignore motor))
  (let* ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                   (verify-single-explicit-value 
                    (chunk-spec-slot-spec chunk-spec 'hand) 
                    :motor 'hold-key 'hand)
                 nil))
         (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'finger)
                      :motor 'hold-key 'finger)
                   nil))
         (delay (if (slot-in-chunk-spec-p chunk-spec 'delay)
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'delay)
                      :motor 'delayed-punch 'delay)
                  nil))
         (key-module (get-module :key-press))
         (d-punch (get-module :delayed-punch)))
    
    (if (and hand finger
             (or (null delay)
                 (and (numberp delay) (plusp delay))
                 (eq delay 'default)
                 (eq delay 'slow)
                 (eq delay 'fast)))
        (if (null (gethash (list hand finger) (key-press-module-table key-module)))
          (progn
            (setf (gethash (list hand finger) (key-press-module-table key-module)) :pre)
            (setf (gethash (list hand finger) (d-punch-table d-punch)) :delayed)
            (schedule-event-relative 
             0 
             'delayed-punch
             :destination :motor
             :params (list :hand hand :finger finger :delay (parse-delay-time delay))
             :module :motor
             :output 'low))
          (print-warning "The ~S ~S finger is already being held so a delayed-punch is ignored."  hand finger))
      (print-warning "Invalid delayed-punch request with chunk-spec ~S" chunk-spec))))


(defun handle-release-all (motor chunk-spec)
  (declare (ignore motor chunk-spec))
  (let ((module (get-module :key-press))
        (d-punch (get-module :delayed-punch)))
    (dolist (hand '(left right))
      (dolist (finger '(index middle ring pinkie thumb))
        (when (gethash (list hand finger) (key-press-module-table module))
          (unless (gethash (list hand finger) (d-punch-table d-punch)) ;; skip those that are :delayed
          (schedule-event-relative 
           0 
           'release-key
           :destination :motor
           :params (list :hand hand :finger finger)
           :module :motor
           :output 'low)))))))

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
