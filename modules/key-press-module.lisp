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
;;; Filename    : key-press-module.lisp
;;; Version     : 0.9a5
;;; 
;;; Description : Module which extends the motor module with 2 additional
;;;             : actions for the manual buffer isa hold-key and isa release-key.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [X] Add some real documentation...
;;; 
;;; ----- History -----
;;;
;;; 2009.03.19 Dan
;;;             : * Moved it from a hack in some other code to its own file.
;;; 2009.07.15 Dan
;;;             : * Fixed the bug with the queue-output-event methods not
;;;             :   using the computed execution time to schedule the action.
;;; 2009.10.27 Dan [0.9a3]
;;;             : * Changed the style names to match the request so that a
;;;             :   prepare looks "right".
;;;             : * Don't use defstyle now and instead create the classes directly
;;;             :   so that I can give them the same style-name to allow for the
;;;             :   features to be shared between a press and the following 
;;;             :   release.  
;;;             : * In fact, that style is going to be :punch since that's
;;;             :   the general basis for these actions.
;;;             : * Allow the release to be initiated even if the hold hasn't
;;;             :   executed yet -- set the held state at request time now.
;;;             :   However, the relased state still doesn't clear until execution
;;;             :   so a repeated hold must still delay.
;;;             : * Now, the minimun hold time is 100ms vs the previous 300ms.
;;; 2009.11.15 Dan [0.9a4]
;;;             : * Adding a buffer to the module so that one can test whether
;;;             :   a finger is down through a query.
;;; 2009.12.08 Dan [0.9a5]
;;;             : * Adding a parameter to allow specifying a time while the
;;;             :   finger stays busy after the release :key-press-busy-lag.
;;;             :   This requires making a "real" structure to hold the module
;;;             :   because it needs to store the table and the time.
;;; 2009.12.16 Dan
;;;             : * Added a call to randomize-time in the scheduling of the
;;;             :   busy lag.
;;; 2010.01.25 Dan
;;;             : * Changed the test for the busy-lag from posnum to nonneg to
;;;             :   allow for a 0 time.
;;; 2010.02.04 Dan
;;;             : * Added a release-all-fingers action to allow the model to
;;;             :   stop holding all keys without requiring a check/release
;;;             :   per finger.
;;; 2010.02.12 Dan
;;;             : * Added a new parameter :key-press-finish-lag to allow for
;;;             :   changing the timing that the motor module stays busy 
;;;             :   during a hold action -- it's now init + the finish-lag
;;;             :   instead of 2x init.
;;; 2010.09.22 Dan 
;;;             : * Added burst-time into the total time for the actions and
;;;             :   randomize the finish time.
;;;             : * Removed the busy-lag and instead just set the finger to
;;;             :   free at the finish time.
;;;             : * Track finger state at a finer grain to differentiate busy
;;;             :   from down -- it can be :pre (request until strike), :down
;;;             :   finger is held, or :post (finger released but still waiting
;;;             :   for finish time).  The query however still only tests for the
;;;             :   general busy (any of the 3 substates is true).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Adds 2 new motor module actions to give a model the ability to hold down 
;;; and release a key as separate actions, and a third action to stop holding
;;; all keys as a single action.  These are a speculative convenience
;;; at this point.
;;;
;;; It's not really based on any research.  It just uses the timing for a punch
;;; and assumes that preparation is shared between a press and release (they have
;;; the same top-level style).
;;;
;;; It's also not tied into any of the built-in devices nor the "real" keyboard 
;;; actions that can be generated using some Lisps by default.  The interface
;;; methods must be defined by the user.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;  Adds the new motor actions:
;;;
;;;   +manual>
;;;     isa hold-key
;;;     hand { left | right }
;;;     finger { index | middle | ring | pinkie | thumb }
;;;
;;; and 
;;;
;;;   +manual>
;;;     isa release-key
;;;     hand { left | right }
;;;     finger { index | middle | ring | pinkie | thumb }
;;;
;;; Those actions call the methods device-hold-finger and device-release-finger
;;; with the device, hand, and finger as parameters when the action is executed.
;;;
;;; Also adds the motor action:
;;; 
;;;   +manual>
;;;     isa release-all-fingers
;;;
;;; Which will execute a release-key action for each finger currently held
;;; down by the model.
;;;
;;; There are no default methods provided, and there's also no mapping done for 
;;; hand&finger -> key.  If that's needed, then the methods will have to get 
;;; that from the hand position info from the motor module.
;;;
;;; Adds a buffer called finger-check which responds to queries of the form:
;;;
;;; <hand>-<finger> { busy | free }
;;;
;;; where hand is either left or right and finger is one of index, middle, ring,
;;; pinkie, or thumb.
;;; 
;;; The query for busy is true from the point of a request being accepted for that
;;; finger until the corresponding release request has been handled and executed.
;;; Otherwise busy is nil and free is true.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Uses the timing of a punch action to execute (init time + key closure) and
;;; the finishing time is just twice the init time.  The assumption being that
;;; continuing to hold a key won't block other actions as long as a down&up 
;;; action will.  It's not based on any research so that should be considered
;;; before using this extension.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct key-press-module
  delay finish table)

(defun create-key-press-module (model-name)
  (declare (ignore model-name))
  (make-key-press-module :table (make-hash-table :size 12 :test #'equalp)))


(defun reset-key-press-module (module)
  (clrhash (key-press-module-table module)))


(defun key-press-finger-check (instance buffer-name slot value)
  (declare (ignore buffer-name))
  (case slot
    (state (case value
             (free t)
             (busy nil)
             (error nil)))
    (t
     (let ((key (case slot
                  (left-index '(left index))
                  (left-middle '(left middle))
                  (left-ring '(left ring))
                  (left-pinkie '(left pinkie))
                  (left-thumb '(left thumb))
                  (right-index '(right index))
                  (right-middle '(right middle))
                  (right-ring '(right ring))
                  (right-pinkie '(right pinkie))
                  (right-thumb '(right thumb)))))
       (if (and key (or (eq value 'free) (eq value 'busy)))
           (let ((state (gethash key (key-press-module-table instance))))
             (or (and (eq value 'busy) state)
                 (and (eq value 'free) (null state))))
         (print-warning "Invalid finger-check request for slot: ~S and value: ~S" slot value))))))


(defun print-finger-check-status ()
  (let ((module (get-module :key-press)))
    
    (dolist (finger '((left index) (left middle) (left ring) (left pinkie) (left thumb)
                      (right index) (right middle) (right ring) (right pinkie) (right thumb)))
  (command-output "  ~12a free     : ~S"
                  (format nil "~s-~s" (car finger) (second finger)) (null (gethash finger (key-press-module-table module)))))))


(defun handle-key-press-params (instance param)
   (cond ((consp param)
          (case (car param)
            (:key-press-busy-lag 
             (setf (key-press-module-delay instance) (cdr param)))
            (:key-press-finish-lag 
             (setf (key-press-module-finish instance) (cdr param)))))
         (t
          (case param
            (:key-press-busy-lag
             (key-press-module-delay instance))
            (:key-press-finish-lag
                (key-press-module-finish instance))))))


(define-module-fct :key-press '((finger-check nil nil (left-index left-middle left-ring left-pinkie left-thumb
                                                                  right-index right-middle right-ring right-pinkie right-thumb)
                                              print-finger-check-status))
  (list
    
    (define-parameter :key-press-finish-lag
      :valid-test #'nonneg
      :warning "a number"
      :default-value 0.05
      :documentation "Time between execution and finish of an action."))
  
   :version "0.9a5"
   :documentation "Simple module for tracking keys (just fingers right now) being held down"
   :creation 'create-key-press-module
  :reset 'reset-key-press-module 
  :params 'handle-key-press-params
  :query 'key-press-finger-check
)


;; Don't use this because I want the "style name" to be the same
;; for the pressing and releasing to avoid extra prep time between
;; a press and release of the same finger
;; (defstyle hold-key punch hand finger)

;; Instead just hack the macroexpand of that...

(defclass hold-key (punch)
  ((hand :accessor hand :initarg :hand :initform nil)
   (finger :accessor finger :initarg :finger :initform nil))
  (:default-initargs :style-name :punch :feature-slots '(hand finger)))

(defmethod hold-key ((module pm-module) &key hand finger)
  (unless (or (check-jam module) (check-specs 'hold-key hand finger))
    (prepare-movement module (make-instance 'hold-key :hand hand :finger finger))))



(defmethod compute-exec-time ((mtr-mod motor-module) (self hold-key))
  (+ (init-time mtr-mod) (key-closure-time (current-device-interface))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self hold-key))
  (let ((instance (get-module :key-press)))
    (+ (init-time mtr-mod) (burst-time mtr-mod) (randomize-time (key-press-module-finish instance)))))


(defmethod queue-output-events ((mtr-mod motor-module) (self hold-key))
  (schedule-event-relative (exec-time self) 'held-key :destination :key-press :module :motor 
                           :output 'medium  
                           :params (list (hand self) (finger self))))


;; Same issues as noted above
;; (defstyle release-held-key press-and-hold-key hand finger)

(defclass release-key (hold-key)
  ((hand :accessor hand :initarg :hand :initform nil)
   (finger :accessor finger :initarg :finger :initform nil))
  (:default-initargs :style-name :punch :feature-slots '(hand finger)))

(defmethod release-key ((module pm-module) &key hand finger)
  (unless (or (check-jam module) (check-specs 'release-key hand finger))
    (prepare-movement module (make-instance 'release-key :hand hand :finger finger))))

(defmethod queue-output-events ((mtr-mod motor-module) (self RELEASE-KEY))
  
  (schedule-event-relative (finish-time self) 'free-finger :module :motor :destination :key-press :params (list (hand self) (finger self)))
  (schedule-event-relative (exec-time self) 'released-key :destination :key-press :module :motor 
                           :output 'medium  
                           :params (list (hand self) (finger self))))

(defun held-key (module hand finger)
  (setf (gethash (list hand finger) (key-press-module-table module)) :down)
  (device-hold-finger (current-device) hand finger))


(defun released-key (module hand finger)
  (setf (gethash (list hand finger) (key-press-module-table module)) :post)
  (device-release-finger (current-device) hand finger))

(defun free-finger (module hand finger)
  (setf (gethash (list hand finger) (key-press-module-table module)) nil))

(defun handle-hold-request (motor chunk-spec)
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
                   nil)))
    
    (when (and hand finger)
      (let ((module (get-module :key-press)))
        (if (null (gethash (list hand finger) (key-press-module-table module)))
          (progn
            (setf (gethash (list hand finger) (key-press-module-table module)) :pre)
            (schedule-event-relative 
             0 
             'hold-key
             :destination :motor
             :params (list :hand hand :finger finger)
             :module :motor
             :output 'low))
        (print-warning "The ~S ~S finger is already being held.  No action taken."  hand finger))))))

(defun handle-release-request (motor chunk-spec)
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
                   nil)))
    
    (when (and hand finger)
      (let ((module (get-module :key-press)))
        (if (gethash (list hand finger) (key-press-module-table module))
          (schedule-event-relative 
           0 
           'release-key
           :destination :motor
           :params (list :hand hand :finger finger)
           :module :motor
           :output 'low)
        (print-warning "The ~S ~S finger is not being held.  No action taken."  hand finger))))))


(defun handle-release-all (motor chunk-spec)
  (declare (ignore motor chunk-spec))
  (let ((module (get-module :key-press)))
    (dolist (hand '(left right))
      (dolist (finger '(index middle ring pinkie thumb))
        (when (gethash (list hand finger) (key-press-module-table module))
          (schedule-event-relative 
           0 
           'release-key
           :destination :motor
           :params (list :hand hand :finger finger)
           :module :motor
           :output 'low))))))

(extend-manual-requests (hold-key hand finger) handle-hold-request)
(extend-manual-requests (release-key hand finger) handle-release-request)
(extend-manual-requests (release-all-fingers) handle-release-all)


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
