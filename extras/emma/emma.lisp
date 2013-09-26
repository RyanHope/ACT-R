;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author      : Mike Byrne
;;; Copyright   : (c)2000-3 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77005-1892
;;;             : byrne@acm.org
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filename    : emma.lisp
;;; Version     : 4.0a1
;;;
;;; Description : Implementation of Dario Salvucci's EMMA system for eye
;;;             : movements based on subclassing the Vision Module and making
;;;             : eye movements based on movement styles.
;;;
;;; Bugs        : 
;;;
;;; Todo        : [X] Solve "chunks created early" problem [requires some
;;;             :     rewriting of the Vision Module] - solved w/ACT-R 6.
;;;             : [ ] Tracking.
;;;             : [ ] Fix the issue with the unrequested marking.
;;;             : [ ] Make sure it works properly with respect to the chunk
;;;             :     deletion code now in the main module.
;;;
;;; ----- History -----
;;; 00.07.12 Mike Byrne
;;;             :  Incept date.
;;; 01.05.09 mdb [r2]
;;;             : Fixed vector bugs.  Thanks, Dario.
;;; 01.05.?? Dario Salvucci [r3]
;;;             : Noise added to both X and Y of movement.
;;; 01.05.28 mdb [r4]
;;;             : Some minor tinkering with Dario's last changes.
;;; 01.07.02 mdb [r5]
;;;             : Made eye movement relative to next landing spot.
;;; 01.09.21 mdb [r6]
;;;             : Fixed bug in COMPLETE-EYE-MOVE.
;;; 01.10.01 mdb [r7]
;;;             : Added PROC-S method that's actually correct.
;;; 01.11.10 mdb
;;;             : Changed trace log to use CONS rather than LIST to save
;;;             : a smidge of memory, and made tracing the eye path switchable.
;;; 2002.05.04 [r8]
;;;             : Removed unnecessary PROC-STUB stuff.
;;; 2002.05.07 
;;;             : Fixed problem of FIND-LOCATION being clobbered by new preps.
;;;             : Renamed FOCUS-ON to ENCODING-COMPLETE.
;;; 2003.05.07 mdb [r9]
;;;             : Fixed MCL-specific checking. 
;;; 2003.06.23 mdb [r10]
;;;             : Made it easier and cleaner to hook into eye spot updating 
;;;             : for non-MCL Lisps by making a more generic function call 
;;;             : to which specific devices can be tailored, paralleling how
;;;             : it's done for attended location. 
;;;             : Based the EYE-SPOT class on the new RPM-OVERLAY class from
;;;             : the 2.1.3 version of "mcl-device.lisp".
;;; 2003.07.17 mdb [r11]
;;;             : Added a CLEAR :after method to kill any in-progress shift
;;;             : and stripped some dead code.
;;;
;;; 2005.08.10 mjs 
;;;             : Made numerous changes to create an ACT-R 6.0 version. Changes
;;;             : are indicated by the initials mjs.deleted input-q references
;;;             : changed scheduling to  ACT-R 6 scheduling
;;;             : delete preparation-complete from event queue if one is outstanding
;;;             : when moving attention
;;;             : uses ACT-R 6.0 noise
;;;             : no pm- in top-level names
;;;             : redefines Vision Module for EMMA
;;; 2006.07.27 Dan
;;;             : * Added the code needed to draw the blue dot in ACL.
;;;             : * Took the "chunks created early" problem off of to do and
;;;             :   bugs because I'm pretty sure that's not an issue in 6.
;;;             : * Also added the package switches that are in all the other
;;;             :   module files for those that use them.
;;; 2006.09.08 Dan
;;;             : * Changed parameter checks from posnum to nonneg.
;;; 2007.03.16 Dan
;;;             : * Changed add-gaussian-noise to catch when there was 0 
;;;             :   deviation because can't generate a noise of 0.
;;; 2007.04.02 Dan
;;;             : * Minor tweak to the test in add-gaussian-noise because for
;;;             :   really small values of stdev (1*10^-23 for shorts) the old
;;;             :   fix still could have resulted in an error.
;;; 2007.06.25 Dan
;;;             : * Added a test to all the schedule-event-relative calls so that
;;;             :   they are never negative - I don't know which one was actually
;;;             :   the problem so I put a check in all of them.
;;; 2007.06.26 Dan
;;;             : * Modified the #+version> checks in the Allegro based code
;;;             :   because even though they're protected with a #+:allegro CLisp
;;;             :   still seems to break on reading them.
;;; 2007.11.05 Dan
;;;             : * Fixed the #+ checks that were modified with the last
;;;             :   update because I broke them with respect to ACL.
;;; 2007.11.15 Dan
;;;             : * Added a device-update-eye-loc method for the environment
;;;             :   visible-virtual windows.
;;;             :   To use that you need to have EMMA loaded after the environment
;;;             :   which means this file should go in other-files and not tools,
;;;             :   commands, or modules.
;;; 2007.11.20 Dan
;;;             : * Fixed a bug in object-frequency because stringp doesn't 
;;;             :   necessarily return the string itself for true.
;;; 2007.11.20 Dan [4.0a1]
;;;             : * Started to rework for use with the new vision module.
;;; 2007.11.21 Dan
;;;             : * Fixed a bug in the ACL display code that was still referring
;;;             :   to *mp*.
;;; 2007.11.21 Dan
;;;             : * Added a parameter (:vis-obj-freq) to set the default frequency value.
;;; 2007.11.28 Dan
;;;             : * Fixed a bug in move-attention so that when the target object
;;;             :   is no longer available there is still a scheduling of the
;;;             :   encoding complete to trigger the error and set the module back
;;;             :   to the free state.
;;; 2007.12.21 Dan
;;;             : * Changed the LispWorks code for the eye-spot to make it a
;;;             :   subclass of focus-ring to avoid an error for not having an
;;;             :   appropriate update-me method, but it doesn't draw and I don't
;;;             :   really know enough capi code to make the necessary changes to
;;;             :   the device file...
;;; 2008.04.24 Dan
;;;             : * Updated with the recent changes to the main vision module.
;;;             : * Added the requested keyword parameter to encoding-complete
;;;             :   so it doesn't complain when loaded, but it doesn't work
;;;             :   right with the emma module - unrequested attention shifts
;;;             :   will not be marked as such in the buffer because emma uses
;;;             :   move-attention to do so.  Will require modifying move-attention
;;;             :   both in emma and the core module.
;;;             : * Added the :test-feats parameter to the module definition.
;;;             : * Did not add :auto-attend because that doesn't seem like
;;;             :   the sort of thing that one would want when using emma and
;;;             :   it could lead to problems with the timing.
;;; 2008.07.08 Dan
;;;             : * Updated update-attended-loc to work right now that the
;;;             :   visicon chunks get deleted.  May need to fix other places
;;;             :   as well.
;;; 2008.07.15 Dan
;;;             : * Fixed a cut-and-paste error introduced with the last fix.
;;; 2008.09.17 Dan
;;;             : * Updated the LispWorks code for drawing the fixation spot
;;;             :   to match with the new device code from LispWorks.
;;; 2010.05.03 Dan
;;;             : * Changed the :output of the "No visual-object found" event
;;;             :   from 'high to 'medium since that's how it has always printed
;;;             :   anyway because of a bug in filter-test.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending chunks at initial load.
;;; 2011.05.16 Dan
;;;             : * Replaced pm-warning calls with model-warning.
;;; 2011.05.19 Dan
;;;             : * Changed mp-time calls to mp-time-ms calls and adjusted
;;;             :   as needed.
;;; 2011.06.17 Dan
;;;             : * Took the ":exec 'free" out of encoding-complete's state change
;;;             :   since that shouldn't happen.
;;; 2012.07.02 Dan [4.0a2]
;;;             : * Update to support one eye-spot per model in multiple window
;;;             :   and/or multiple model situations.  Like the fixation marker
;;;             :   there is a function called eye-spot-marker which can be used
;;;             :   to store the current marker instead of the *eye-spot* variable.
;;;             :   Like device-update-attended-loc the device-update-eye-loc
;;;             :   method will now occasionally get called with nil to indicate
;;;             :   that the marker should be erased.
;;;             : * Only show the eye spot when show-focus-p is true.
;;;             : * Add a modified install-device so that it can call the 
;;;             :   device-update-eye-loc method when the model switches devices
;;;             :   and always start eyes at location 0,0 when a device is installed.
;;;             : * Also updating some other code that seems to have been neglected
;;;             :   along the way (particularly the :delete-visicon-chunks,
;;;             :   :scene-change-threshold, and :viewing-distance parameters).
;;; 2012.09.07 Dan
;;;             : * Adding an eye spot for CCL since there's now a device for it.
;;;             : * Fixed a bug in the environment version of the eye-spot code.
;;; 2013.01.09 Dan
;;;             : * Replaced the #@ in the CCL code with make-point since that 
;;;             :   causes problems in all other Lisps even inside the #+CCL...
;;;             : * Removed the clearing of the eye-spot from the reset function
;;;             :   since install-device handles that and it's important that an
;;;             :   "old" spot persist to that it can be erased from the previous
;;;             :   device.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(pushnew :emma *features*)  ;;mjs indicate emma module

(defclass emma-vis-mod (vision-module)
  ((freq-ht :accessor freq-ht :initarg :freq-ht
            :initform (make-hash-table :test #'equal))
   (enc-factor :accessor enc-factor :initarg :enc-factor :initform 0.010)
   (enc-exponent :accessor enc-exponent :initarg :enc-exponent :initform 1.0)
   (eye-loc :accessor eye-loc :initform #(0 0))
   (eye-trace :accessor eye-trace :initform nil)
   (base-exe :accessor base-exe :initarg :base-exe :initform 0.020)
   (sacc-rate :accessor sacc-rate :initarg :sacc-rate :initform 0.002)
   (shift-start :accessor shift-start :initarg :shift-start :initform 0)
   (shift-duration :accessor shift-duration :initarg :shift-duration
                   :initform nil)
   (shift-target :accessor shift-target :initarg :shift-target :initform nil)
   (trace-eye-p :accessor trace-eye-p :initarg :trace-eye-p :initform nil)
;
   (prep-event :accessor prep-event :initform nil) ;;holds current preparation-complete event
   (next-loc :accessor next-loc :initform nil) ;;holds the next landing loc
   
   (default-freq :accessor default-freq :initform 0.01)
   (eye-spot :accessor eye-spot :initform nil)))

;;mjs deleted reset-module after method

(defmethod set-eye-loc ((eye-mod emma-vis-mod) (newloc vector))
  (setf (eye-loc eye-mod) newloc)
  (when (show-focus-p (current-device-interface))
    (device-update-eye-loc (device (current-device-interface)) newloc))
  (when (trace-eye-p eye-mod)
    (push (cons (mp-time) newloc) (eye-trace eye-mod))))


;;; The replacement for *eye-spot* is a slot in the module accessed with
;;; the eye-spot-marker function:

(defun eye-spot-marker ()
  (aif (get-module :vision)
       (eye-spot it)
       (print-warning "No current vision module available to return eye-spot-marker.")))

(defun set-eye-spot-marker (val)
  (aif (get-module :vision)
       (setf (eye-spot it) val)
       (print-warning "No current vision module available to set eye-spot-marker.")))

(defsetf eye-spot-marker set-eye-spot-marker)

;;; replacement install-device to also call device-update-eye-loc when the
;;; device changes

(defun install-device (device)
  "Set the device with which a model will interact"
  (verify-current-mp  
   "install-device called with no current meta-process."
   (verify-current-model
    "install-device called with no current model."
    (let ((devin (current-device-interface)))
    
      ;; If there's a current device and we're showing the
      ;; fixation ring tell the old device that it's ring
      ;; is going away by calling it with nil
           
      (when (show-focus-p devin)
        (device-update-attended-loc (device devin) nil)
        (device-update-eye-loc (device devin) nil))
      
      ;; Always start at 0,0 when looking at a new window
      
      (setf (device devin) device)
      
      (set-eye-loc (get-module :vision) #(0 0))
      
      device))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Saccade movement style

(defStyle saccade () r theta trgt)


;;; COMPUTE-EXEC-TIME      [Method]
;;; Date        : 01.05.09
;;; Description : Execution time is the sum of the base execution time and
;;;             : the distance times the rate.

(defmethod compute-exec-time ((eye-mod emma-vis-mod) (mvmt saccade))
  (rand-time
   (+ (init-time eye-mod) (* (r mvmt) (sacc-rate eye-mod)) 
      (base-exe eye-mod))))


;;; COMPUTE-FINISH-TIME      [Method]
;;; Date        : 01.05.09
;;; Description : The finish time is the same as the execution time for
;;;             : saccades.

(defmethod compute-finish-time ((eye-mod emma-vis-mod) (mvmt saccade))
  (exec-time mvmt))


;;; FEAT-DIFFERENCES      [Method]
;;; Date        : 01.05.09
;;; Description : How many features to prepare?  One for direction and one for
;;;             : distance, if they're different.

(defmethod feat-differences ((s1 saccade) (s2 saccade))
  (let ((nfeats 0))
    (unless (distance= (r s1) (r s2)) (incf nfeats))
    (unless (direction= (theta s1) (theta s2)) (incf nfeats))
    nfeats))


;;; TOTAL-TIME      [Method]
;;; Date        : 01.05.09
;;; Description : The total time for a saccade is the simple sum of the
;;;             : prep time and the execution time.

(defmethod total-time ((sacc saccade))
  (+ (fprep-time sacc) (exec-time sacc)))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Module-level stuff

;;; MOVE-ATTENTION      [Method]
;;; Date        : 01.05.09
;;; Description : Only slightly more complicated than without EMMA.  First,
;;;             : clear all the bookkeeping stuff [e.g. where attention is].
;;;             : Unless we're in the middle of execution, clear the module's
;;;             : input queue.  This will effectively destroy any previously-
;;;             : prepared movement, as it will eliminate any
;;;             : PREPARATION-COMPLETE entries in the queue.  Finally, we
;;;             : need to set up an initiate the actual eye move.
;;;             : Note that the computation of the recogntion time for the
;;;             : target assumes that the object that is there when the move
;;;             : starts and ends will be the same.  Thus, this may not work
;;;             : quite right for very dynamic displays.

(defmethod move-attention ((eye-mod emma-vis-mod) &key location scale)
  (declare (symbol scale))
  (if (eq (exec-s eye-mod) 'BUSY)
    (model-warning "Attention shift requested at ~S while one was already in progress." (mp-time))
    (progn
      (when (tracked-obj eye-mod) (remove-tracking eye-mod))
      (setf (moving-attention eye-mod) (new-name "TRGT")) 
      (clear-attended eye-mod)
      (setf (last-scale eye-mod) scale)
      
      (setf (attend-failure eye-mod) nil)  ;;; clear the failure indicator     
      
      (setf (current-marker eye-mod) location)
      
      ;;(set-clof eye-mod (dmo-to-xy location))
;;;
;;;   mjs delete preparation-complete from event queue if one is outstanding
;;;
      (unless (eq 'BUSY (exec-s eye-mod))  
        (awhen (prep-event eye-mod)
          ;(format t "~%Deleting a prep event")
               (delete-event it))
        (setf (prep-event eye-mod) nil))
      
      
;;;   emma code
      (let ((return-obj (get-obj-at-location eye-mod location scale)))
        (if return-obj
          (let* ((start-loc (next-destination eye-mod))
                 (end-loc (xy-loc location))
                 (r-theta (compute-saccade-r-theta start-loc end-loc)))
            (change-state eye-mod :proc 'BUSY)
            (setf (shift-duration eye-mod) (recog-time eye-mod return-obj r-theta))
            (setf (shift-target eye-mod) return-obj)
            (initiate-eye-move eye-mod (shift-duration eye-mod) r-theta))
          
          ;; when previous target still available assume it's still valid and continue
          (if (shift-target eye-mod)
              (let* ((return-obj (shift-target eye-mod))
                     (start-loc (next-destination eye-mod))
                     (end-loc (xy-loc location))
                     (r-theta (compute-saccade-r-theta start-loc end-loc)))
                (change-state eye-mod :proc 'BUSY)
                (setf (shift-duration eye-mod) (recog-time eye-mod return-obj r-theta))
                (initiate-eye-move eye-mod (shift-duration eye-mod) r-theta))
            (let* ((return-obj (car (define-chunks-fct `((isa visual-object screen-pos ,location)))))
                   (start-loc (next-destination eye-mod))
                   (end-loc (xy-loc location))
                   (r-theta (compute-saccade-r-theta start-loc end-loc)))
              (change-state eye-mod :proc 'BUSY)
              (setf (shift-duration eye-mod) (recog-time eye-mod return-obj r-theta))
              (initiate-eye-move eye-mod (shift-duration eye-mod) r-theta)
              (delete-chunk-fct return-obj))))))))

;;; ENCODING-COMPLETE      [Method]
;;; Date        : 01.05.10
;;; Description : This is essentially the same as ENCODING-COMPLETE for the non-EMMA
;;;             : Vision Module, but it also checks whether the target of
;;;             : the attention shift is still there.

(defmethod encoding-complete ((eye-mod emma-vis-mod) loc scale &key (requested t))
  (declare (symbol scale))
  (setf (moving-attention eye-mod) NIL)
  (change-state eye-mod  :proc 'FREE)
  
  (let ((return-obj
         (if (and (shift-target eye-mod)
                  (object-present-p eye-mod (shift-target eye-mod)))
             (shift-target eye-mod)
           (get-obj-at-location eye-mod loc scale))))
    (unless return-obj
      (clear-attended eye-mod)
      (setf (attend-failure eye-mod) t)
      (schedule-event-relative 0 'no-visual-object-found :maintenance t :module :vision :output 'medium :details "No visual-object found")
      (return-from encoding-complete nil))
    (set-attended eye-mod (chunk-visicon-entry return-obj))
    (attend-to-object eye-mod return-obj :requested requested)
    return-obj))

;;; UPDATE-ATTENDED-LOC      [Method]
;;; Date        : 01.05.10
;;; Description : Also much like the non-EMMA method. If the screen at the
;;;             : attended location changes, there's some updating to do
;;;             : and an attention shift to the new object, if any.

(defmethod update-attended-loc ((eye-mod emma-vis-mod))
  ;; if we're tracking or moving around, ignore this 
  (when (or (tracked-obj eye-mod) (moving-attention eye-mod) 
            (eq 'BUSY (exec-s eye-mod)))
    (return-from update-attended-loc nil))
  ;; when do we update?
  ;; [1] when we're looking at an object and it's gone
  ;; [2] when we're looking at nothing and something appears 
  (when (or (and (currently-attended eye-mod)
                 (or (not (chunk-p-fct (currently-attended eye-mod)))
                     (not (object-present-p eye-mod (currently-attended eye-mod)))))
            (and (current-marker eye-mod)
                 (null (currently-attended eye-mod))
                 (within-move eye-mod (xy-loc (current-marker eye-mod)))))
            
        (schedule-event-relative 0 'move-attention  ;;;mjs   
                                 :params (list 
                                            eye-mod
                                            :location (current-marker eye-mod)
                                            :scale (last-scale eye-mod))
                                 :output 'medium
                                 :details "Move-attention-attended-loc"
                                 :module :vision)
    ))


(defmethod clear :after ((eye-mod emma-vis-mod))
  (setf (moving-attention eye-mod) nil))

(defgeneric next-destination (eye-mod)
  (:documentation  "Returns the next landing location for EMMA."))

(defmethod next-destination ((eye-mod emma-vis-mod))
  (if (eq 'FREE (exec-s eye-mod))
    (eye-loc eye-mod)
    (next-loc eye-mod) ;;; mjs since no more input-q, get from eye-mod, saved by complete-eye-move
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; INITIATE-EYE-MOVE      [Method]
;;; Date        : 01.05.09
;;; Description : First, build the actual saccade movement itself.  This is a
;;;             : CLOS object whose class is generated by the DEFSTYLE call in
;;;             : this file.  Do all the state changes and queue up the
;;;             : preparation complete notice, which will then generate an
;;;             : eye movement.  Now, we only want to queue the FOCUS-ON when
;;;             : the object will be recognized before the eye movement is
;;;             : complete, so do that check and queue that event if necessary.

(defgeneric initiate-eye-move (eye-mod recog-time r-theta)
  (:documentation "Given a recognition time and a movement spec, begin an eye movement."))

(defmethod initiate-eye-move ((eye-mod emma-vis-mod) (recog-time number)
                                 (r-theta vector))
  (let ((sacc-mvmt (build-sacc-mvmt eye-mod r-theta)))
    ;; always start the preparation of the eye move
    (setf (shift-start eye-mod) (mp-time-ms))
    (change-state eye-mod :prep 'BUSY)
    (setf (last-prep eye-mod) sacc-mvmt)
    (setf (prep-event eye-mod)  ;;mjs
          (schedule-event-relative (max 0 (fprep-time sacc-mvmt))  'PREPARATION-COMPLETE
                                   :destination :vision
                                   :module :vision
                                   :output 'medium
                                   :details (concatenate 'string "Preparation-complete " (write-to-string (trgt sacc-mvmt)))))
    ;; only sometimes queue the FOCUS-ON
    (when (<= recog-time (total-time sacc-mvmt))
     (schedule-event-relative (max 0 recog-time)  'ENCODING-COMPLETE   ;;mjs
                             :destination :vision
                             :module :vision
                             :params`(,(current-marker eye-mod) ,(last-scale eye-mod))
                             :output 'medium
                             :details (concatenate 'string "Encoding-Complete " (symbol-name (current-marker eye-mod))))
)))


(defmethod initiate-eye-move ((eye-mod emma-vis-mod) (recog-time number)
                                 (r-theta list))
  (initiate-eye-move eye-mod recog-time (coerce r-theta 'vector)))

;;;mjs after method to clear prep event
(defmethod preparation-complete :after ((module emma-vis-mod))
  (setf (prep-event module) nil))


;;; COMPLETE-EYE-MOVE      [Method]
;;; Date        : 01.05.09
;;; Description : When an eye movement completes:
;;;             : [1] Update the blue POR spot on the display, the eye trace,
;;;             :     and the module's state.
;;;             : [2] Compute the new recognition time for the target based
;;;             :     on the new distance.
;;;             : [3] Initiate an eye move.  Note that this movement should
;;;             :     die if recog time based on the eye's new location is
;;;             :     small.


(defgeneric complete-eye-move (eye-mod marker xyloc)
  (:documentation "Called when an eye movement completes to handle all the updating."))

(defmethod complete-eye-move ((eye-mod emma-vis-mod) (trgt symbol) 
                                 (xyloc vector))
  ;; handle the module-level stuff
  (set-eye-loc eye-mod xyloc)
  ;; when we're still moving attention to the same target, we might need
  ;; to generate another shift.
  (when (and (moving-attention eye-mod)
             (eq trgt (moving-attention eye-mod)))
    (let* ((start-loc (eye-loc eye-mod))
           (end-loc (xy-loc (current-marker eye-mod)))
           (r-theta (compute-saccade-r-theta start-loc end-loc))
           (new-duration
            (* (recog-time eye-mod (shift-target eye-mod) r-theta)
               (- 1 (/ (ms->seconds (- (mp-time-ms) (shift-start eye-mod)))  ;;mjs pm-time
                       (shift-duration eye-mod))))))
      (setf (shift-start eye-mod) (mp-time-ms))
      (setf (shift-duration eye-mod) new-duration)
      (initiate-eye-move eye-mod new-duration r-theta)))
  (finish-movement eye-mod)
  )

(defmethod complete-eye-move ((eye-mod emma-vis-mod) (trgt symbol) 
                                 (xyloc list))
  (complete-eye-move eye-mod trgt (coerce xyloc 'vector)))

;;; PERFORM-MOVEMENT      [Method]
;;; Date        : 01.05.09
;;; Description : Performing the movement simply involves figuring out when
;;;             : to call COMPLETE-EYE-MOVE and updating the eye location.

(defmethod perform-movement ((eye-mod emma-vis-mod) (sacc saccade))
  (change-state eye-mod :exec 'BUSY)
  (let ((new-loc (polar-move-xy (eye-loc eye-mod)
                                  (vector (pm-angle-to-pixels (r sacc))
                                          (theta sacc)))))
    (setf (next-loc eye-mod) new-loc)
    (schedule-event-relative (max 0 (exec-time sacc))  'COMPLETE-EYE-MOVE ;;;mjs
                             :destination :vision
                             :module :vision
                             :params `(,(trgt sacc) ,new-loc)
                             :output 'medium
         :details  (concatenate 'string "Complete-eye-movement " (write-to-string (trgt sacc)) " " (write-to-string new-loc) ))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric compute-saccade-r-theta (start-loc end-loc)
  (:documentation "Adds gaussian noise to <end-loc> and returns r-theta vector for saccade destination."))

(defmethod compute-saccade-r-theta ((start-loc vector) (end-loc vector))
  (let* ((stddev (* 0.1 (dist start-loc end-loc)))
         (end-loc-noisy 
          (vector (add-gaussian-noise (aref end-loc 0) stddev)
                  (add-gaussian-noise (aref end-loc 1) stddev))))
    (xy-to-polar start-loc end-loc-noisy)))

(defgeneric recog-time (eye-mod obj r-theta)
  (:documentation "Based on Dario's equation, compute the recognition 
time for an object given the displacement from the current POR."))

(defmethod recog-time ((eye-mod emma-vis-mod) obj (r-theta vector))
  (let ((freq (object-frequency eye-mod obj (default-freq eye-mod)))
        (eccentricity (pm-pixels-to-angle (vr r-theta))))
    (rand-time (* (enc-factor eye-mod)
                  (- (log freq))
                  (exp (* eccentricity (enc-exponent eye-mod)))))))

(defmethod recog-time ((eye-mod emma-vis-mod) obj (r-theta list))
  (recog-time eye-mod obj (coerce r-theta 'vector)))



;;; OBJECT-FREQUENCY      [Method]
;;; Description : Implicit default of 0.01 if there's no hash table entry
;;;             : or feature-based frequency on the object.

(defgeneric object-frequency (eye-mod obj &optional default)
  (:documentation "Compute the frequency of ocurrence of a visual 
object, which is used to compute the recognition time."))

(suppress-extension-warnings)
(extend-chunks vis-obj-freq)
(unsuppress-extension-warnings)

(defmethod object-frequency ((eye-mod emma-vis-mod) obj &optional (default 0.01))
  (let ((value (fast-chunk-slot-value-fct obj 'value)))
    (if (stringp value)
        (aif (gethash value (freq-ht eye-mod))
             it
             default)
      (if (numberp (chunk-vis-obj-freq obj))
        (let ((val (chunk-vis-obj-freq obj)))
          (setf (chunk-vis-obj-freq obj) (+ val 0.01))
           val)
      default))))
  
  
(defgeneric build-sacc-mvmt (eye-mod r-theta)
  (:documentation "Constructs a saccade movement based on a displacement from the current POR."))


(defmethod build-sacc-mvmt ((eye-mod emma-vis-mod) (r-theta vector))
  (let ((sacc-mvmt (make-instance 'saccade
                       :r (pm-pixels-to-angle (vr r-theta))          ; removed noise addition 01.05.28
                       :theta (vtheta r-theta))))
    (setf (fprep-time sacc-mvmt)
          (rand-time (compute-prep-time eye-mod sacc-mvmt)))
    (setf (exec-time sacc-mvmt)
          (compute-exec-time eye-mod sacc-mvmt))
    (setf (trgt sacc-mvmt) (moving-attention eye-mod))
    sacc-mvmt))

(defmethod build-sacc-mvmt ((eye-mod emma-vis-mod) (r-theta list))
  (build-sacc-mvmt eye-mod (coerce r-theta 'vector)))


(defgeneric register-obj-freq (eye-mod name freq)
  (:documentation "Store a frequency for a particular symbol."))

(defmethod register-obj-freq ((eye-mod emma-vis-mod) (name string)
                                 (freq float))
  (setf (gethash name (freq-ht eye-mod)) freq))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Utilities

(defun add-gaussian-noise (x stddev)
  "Adds pseudo-Gaussian noise to mean x with a given stddev."
  (let* ((v (* stddev stddev))
         (s (/ (sqrt (* 3.0 v)) pi)))
    (+ x (if (zerop s) 0 (act-r-noise s))))) ;;;mjs


(defgeneric device-update-eye-loc (device xyloc)
  (:documentation "Called by EMMA when the eye position changes so it can be rendered appropriately."))


;;; DEVICE-UPDATE-EYE-LOC      [Method]
;;; Description : The base method for updating does nothing.  Redefine for
;;;             : your device--there is a method for MCL below.

(defmethod device-update-eye-loc (device xyloc)
  (declare (ignore device xyloc))
  nil)

;;;; ---------------------------------------------------------------------- ;;;;
;;;; tracking stuff  !!MCL-specific!!
;;;#| mjs need to comment out because rpm-overlay not defined when loads

#|MCL devices are no longer supported so just comment this out 
   instead of updating it with the new changes


#+(and :mcl (not :openmcl))

 
(progn
  
  (defclass eye-spot (rpm-overlay)
    ((color :accessor color :initarg :color :initform *blue-color*))
    (:default-initargs 
      :view-size (make-point 9 9) 
      :offset (make-point -5 -5)))

  (setf *eye-spot* (make-instance 'eye-spot))

;;;called by set-eye-loc  
  (defmethod device-update-eye-loc ((device window) (xyloc vector))
    (update-me *eye-spot* device xyloc))

;;;update-me for rpm-overlay is in device.lisp -- calls view-draw-contents

  (defmethod view-draw-contents ((self eye-spot))
    (with-focused-view self
      (with-fore-color (color self)
        (fill-oval self *light-gray-pattern* (make-point 0 0) (view-size self)))))
)
|#



;;; The CCL code is very similar to the MCL code and basically just copied
;;; from the attended-loc code in the ccl device by Clayton Stanley and Mike Byrne.

#+(and :clozure :darwin :apple-objc :ccl-1.8)
(progn
  (require-compiled "CCL-SIMPLE-VIEW" "ACT-R6:support;ccl-simple-view")
  
  (defclass eye-spot (focus-ring)
  ((easygui::foreground :reader color :initform *blue-color*))
  (:default-initargs 
    :view-size (make-point 9 9)
    :offset (make-point -5 -5)))

(defmethod device-update-eye-loc ((wind window) xyloc)
  (unless (aand (eye-spot-marker) (eq (type-of it) 'eye-spot)
                (view-window (eye-spot-marker)) (eq it wind))
    (setf (eye-spot-marker) (make-instance 'eye-spot)))
  (when (wptr wind)
    (update-me (eye-spot-marker) wind xyloc)))


(defmethod device-update-eye-loc ((wind window) (xyloc (eql nil)))
  (when (aand (eye-spot-marker) (eq (type-of it) 'eye-spot)
              (view-window (eye-spot-marker)) (eq it wind) (wptr wind))
    (remove-subviews wind (eye-spot-marker)))
  (setf (eye-spot-marker) nil)))
  
  


#+:lispworks  ;;mjs
(progn
  (defvar *eye-diameter* 14)
 
  (defclass eye-spot (focus-ring)
    ()
    (:default-initargs 
        :width  *eye-diameter*
      :height *eye-diameter*
      :visible-min-width *eye-diameter*  ; override CAPI:ELLIPSE default
      :visible-min-height *eye-diameter* ; override CAPI:ELLIPSE default
      :graphics-args '(:foreground :blue :thickness 3)
      :filled nil))
  
  (defvar *attention-eye-x-adjust*      
      (+ (truncate (* 0.5 *eye-diameter*)) 2))
  (defvar *attention-eye-y-adjust*
      (+ (truncate (* 0.5 *eye-diameter*)) 3))
  
  (defmethod object-x-adjustment ((object eye-spot))
    *attention-eye-x-adjust*)
  
  (defmethod object-y-adjustment ((object eye-spot))
    *attention-eye-y-adjust*)

   
  (defmethod device-update-eye-loc ((device capi:interface) xyloc )
  (let ((object (eye-spot-marker))
        (current-pinboard (pinboard device)))
    
  ;;; Make sure it's a focus-ring or nil
  ;;; and if not just clear it which could be an issue if
  ;;; someone creates different types of windows each of which
  ;;; will be setting incompatible fixtion markers, but that's
  ;;; unlikely to occur.
  
  (unless (or (null object)
              (equal (type-of object) 'eye-spot))
    (setf object (setf (eye-spot-marker) nil)))
    
    
    ;; if the current pinboard doesn't match or there is no xyloc
    ;; remove it from its current pinboard
    
    (when (and object 
               (or (null xyloc)
                   (not (equal current-pinboard (slot-value object 'capi::pinboard)))))
      (let ((item-pinboard (slot-value object 'capi::pinboard))
            (done nil))
        (when item-pinboard ;; the window may already be closed
          (capi:apply-in-pane-process 
           item-pinboard     
           #'(lambda(pinboard item)
               (setf (capi:layout-description pinboard)
                 (remove item (capi:layout-description pinboard)))
               (setq done t))
           item-pinboard
           object)
          (mp:process-wait "Waiting for visual items"
                           #'(lambda () done))))
        
        (setf object nil))      
    
    ;; if there is a location 
    
    (when xyloc
      ;; create a new ring if there isn't a valid one
      (unless object
        (setf object (setf (eye-spot-marker) (make-instance 'eye-spot))))
      
      (update-me object device xyloc))))
  
  )



#+:allegro-ide
(progn
  
  (defclass eye-view (transparent-pane)
    ()
    (:default-initargs
      :foreground-color blue
      :width 20
      :height 20))
  
  (defmethod redisplay-window ((self eye-view) &optional box)
    (declare (ignore box))
    (setf (line-width self) 3)
    (draw-circle self (make-position 7 7) 6))
  
  
  (defmethod device-update-eye-loc ((wind #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) xyloc)
  
  ;;; Make sure it's an eye-view or nil
  ;;; and if not just clear it which could be an issue if
  ;;; someone creates different types of windows each of which
  ;;; will be setting incompatible fixtion markers, but that's
  ;;; unlikely to occur.
  
  (unless (or (null (eye-spot-marker))
              (equal (type-of (eye-spot-marker)) 'eye-view)
              #+:allegro-v5.0.1 (equal (type-of (eye-spot-marker)) 'closed-stream))
    (setf (eye-spot-marker) nil))

  
  ;;; Close it if it's open and the xyloc is nil or the parent doesn't match
  
  (when (and (eye-spot-marker)
             #+:allegro-v5.0.1 (not (equal (type-of (eye-spot-marker)) 'closed-stream))
             #+(version>= 6) (cg:handle (eye-spot-marker))
             
             (or (null xyloc)
                 (not (equal wind (cg:parent (eye-spot-marker))))))
    (close (eye-spot-marker)))
  
  ;;;  If there's a location and it doesn't exist or is closed create a new one 
  
  (when (and xyloc
             (or (null (eye-spot-marker))
                 #+:allegro-v5.0.1 (equal (type-of (eye-spot-marker)) 'closed-stream)
                 #+(version>= 6) (null (cg:handle (eye-spot-marker)))))
    (setf (eye-spot-marker) (cg:make-window :eye-ring :device 'eye-view :parent wind
                                     :left (- (px xyloc) 7) 
                                     :top (- (py xyloc) 7))))
  
  ;; only update it if there's a location
  
  (when xyloc
    (update-me (eye-spot-marker) wind xyloc)))
  
  
  (defmethod update-me ((foc-ring eye-view) window xyloc)
    (declare (ignore window))
    (setf (cg:state foc-ring) :shrunk)
    (setf (cg:left foc-ring) (- (px xyloc) 7))
    (setf (cg:top foc-ring) (- (py xyloc) 7))
    (setf (cg:state foc-ring) :normal))
  
  
  ;;; hack so that when the focus-ring is present it passes the key presses on to the
  ;;; window for handling - oh yeah it actually works 
  
  (defmethod virtual-key-down :before ((focus-ring eye-view) buttons key-code)
    (virtual-key-down (parent focus-ring) buttons key-code))
  
  
  ;;; Hack to handle issues with model mouse clicks
  ;;; that occur over the fixation ring or the
  ;;; eye spot.  Since Windows handles dispatch
  ;;; from bottom up these "extra" panes actually
  ;;; prevent the click from going where we want
  ;;; it and it's easiest to just remove them
  ;;; when the model clicks.
  
  (defmethod device-handle-click ((device #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane))
  #+(version>= 6)
  (let (x y x2 y2)
    (when (and (show-focus-p (current-device-interface))
               (equal (type-of (visual-fixation-marker)) 'focal-view))
      (setf x (left (visual-fixation-marker)))
      (setf y (top (visual-fixation-marker)))
      
      (close (visual-fixation-marker))
      (setf (visual-fixation-marker) :closed)
      (cg:process-pending-events))
    
    (when (and (show-focus-p (current-device-interface))
               (equal (type-of (eye-spot-marker)) 'eye-view))
      (setf x2 (left (eye-spot-marker)))
      (setf y2 (top (eye-spot-marker)))
      
      (close (eye-spot-marker))
      (setf (eye-spot-marker) :closed)
      (cg:process-pending-events))
    
    (do-click nil :preview-seconds nil :down-seconds .0001)
    (cg:process-pending-events)
    
    (when (and (show-focus-p (current-device-interface))
               (equal (visual-fixation-marker) :closed))
      (setf (visual-fixation-marker) (cg:make-window :focus-ring :device 'focal-view :parent device
                                      :left x 
                                       :top y )))
    (when (and (show-focus-p (current-device-interface))
               (equal (eye-spot-marker) :closed))
      (setf (eye-spot-marker) (cg:make-window :eye-ring :device 'eye-view :parent device
                                      :left x2 
                                      :top y2)))
      )
    
  #-(version>= 6)
    (do-click nil :preview-seconds nil :down-seconds .0001))
  
  
  )


#+:ACTR-ENVIRONMENT

(defmethod device-update-eye-loc ((wind visible-virtual-window) xyloc)
  
  (when (and (eye-spot-marker) (not (eq wind (eye-spot-marker))))
    (send-env-window-update (list 'cleareyeloc (id wind) (current-model))))
  
  (setf (eye-spot-marker) wind)
  
  (if xyloc
      (send-env-window-update (list 'eyeloc (id wind) (px xyloc) (py xyloc) (current-model)))
    (send-env-window-update (list 'cleareyeloc (id wind) (current-model)))))


;;; Also need to redefine the AGI open-exp-window function so that
;;; it calls the device-update-eye-loc function when a window gets
;;; reused like it does for device-update-attended-loc so that things
;;; work right in that case.


(defun open-exp-window (title &key (width 300) (height 300) (visible t) (x 300) (y 300))
  "Open an experiment window"
  (if (or (stringp title) (symbolp title))
      (if (current-model)
          (let* ((instance (get-module agi))
                 (exists (assoc title (agi-window-list instance) :test 'string-equal))
                 (win (and exists (cdr exists))))
            (if (and win (open-rpm-window? win)
                     (or (and visible (rpm-window-visible-status win)) ;; both visible 
                         (not (or visible (rpm-window-visible-status win))))) ;; both not visible
                (progn
                  (remove-all-items-from-rpm-window win)
                  (device-update-attended-loc win nil)
                  (device-update-eye-loc win nil))
              (when win
                (when (open-rpm-window? win)
                  (close-rpm-window win))
                (setf (agi-window-list instance) (remove exists (agi-window-list instance)))
                (remhash (cdr exists) (global-agi-table instance))
                (setf win nil)))
            (unless win
              (setf win (make-rpm-window :visible visible :title (format nil "~a (~a)" title (current-model))
                                         :width width 
                                         :height height
                                         :x x
                                         :y y))
              (push (cons title win) (agi-window-list instance))
              (setf (gethash win (global-agi-table instance)) (cons (current-meta-process) (current-model))))
            (select-rpm-window win)
            (set-visual-center-point (round width 2) (round height 2))
            win)
        
        (print-warning "Cannot create an experiment window without a current model"))
    (print-warning "Experiment window title must be a string or symbol, but ~s was specified." title)))




;;;|#
;;;; ---------------------------------------------------------------------- ;;;;
;;;; toplevel stuff


(defun register-string-frequency (string frequency)
  "Register the frequency of a string."
  (register-obj-freq (get-module :vision) string frequency))

;;; temporary wrapper

(defun add-visual-object (name freq)
  (register-string-frequency name freq))


(defun set-eye-location (loc)
  (set-eye-loc (get-module :vision) (coerce loc 'vector)))

(defun current-eye-location ()
  (eye-loc (get-module :vision)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Init stuff

;;;mjs create EMMA module

(defun create-emma-module (model-name)
  (declare (ignore model-name))
  (make-instance 'emma-vis-mod))


(defmethod reset-emma-module ((eye-mod emma-vis-mod))
  (reset-vision-module eye-mod)
  (setf (eye-trace eye-mod) nil)
 
  (setf (shift-start eye-mod) 0)
  (setf (shift-target eye-mod) nil)
  (setf (prep-event eye-mod) nil)
  (setf (next-loc eye-mod) nil)
  (clrhash (freq-ht eye-mod))
  ;; Don't do this here because it causes an issue
  ;; for switching/reusing windows since install-device
  ;; will do the "right" thing at that point -- it
  ;; doesn't matter what the values are until there's
  ;; a window with which to interact.
  ;(set-eye-loc eye-mod #(0 0))
  ;(setf (eye-spot eye-mod) nil)
  )



(defun params-emma-module (vis-mod param)
 (aif (params-vision-module vis-mod param)
      it
      (if (consp param)
        (case (car param)
          (:VISUAL-ENCODING-FACTOR
           (setf (enc-factor vis-mod) (cdr param)))
          (:VISUAL-ENCODING-EXPONENT
           (setf (enc-exponent vis-mod) (cdr param)))
          (:EYE-SACCADE-RATE
           (setf (sacc-rate vis-mod) (cdr param)))
          (:SACCADE-BASE-TIME
           (setf (base-exe vis-mod) (cdr param)))
          (:vis-obj-freq
           (setf (default-freq vis-mod) (cdr param)))
          )
        (case param
          (:VISUAL-ENCODING-FACTOR
           (enc-factor vis-mod) )
          (:VISUAL-ENCODING-EXPONENT
           (enc-exponent vis-mod) )
          (:EYE-SACCADE-RATE
           (sacc-rate vis-mod) )
          (:SACCADE-BASE-TIME
           (base-exe vis-mod) )
          (:vis-obj-freq
           (default-freq vis-mod) )))))

;;; define the module itself  -- name :vision

(undefine-module :vision)


(define-module-fct :vision 
    (list (list 'visual-location nil '(:attended :nearest) '(attended)
                            #'(lambda ()
                               (command-output "  attended new          : ~S"
                                               (query-buffer 'visual-location 
                                                             '((attended . new))))
                               (command-output "  attended nil          : ~S"
                                               (query-buffer 'visual-location
                                                             '((attended . nil))))
                               (command-output "  attended t            : ~S"
                                               (query-buffer 'visual-location
                                                             '((attended . t)))))) 
        (list 'visual nil nil '(modality preparation execution processor last-command)
                 #'(lambda () 
                       (print-module-status (get-module :vision)))))
  (list 
   
   (define-parameter :optimize-visual
     :valid-test #'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "")
    (define-parameter :visual-attention-latency
     :valid-test #'nonneg 
     :default-value 0.085
     :warning "a non-negative number"
     :documentation "Time for a shift of visual attention")
   (define-parameter :visual-finst-span
     :valid-test #'nonneg 
     :default-value 3.0
     :warning "a non-negative number"
     :documentation "Lifespan of a visual finst")
   (define-parameter :visual-movement-tolerance
     :valid-test #'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation 
     "How far something can move while still being seen as the same object.")
   (define-parameter :visual-num-finsts
     :valid-test #'posnum 
     :default-value 4
     :warning "a positive number"
     :documentation "Number of visual finsts.")
   (define-parameter :visual-onset-span
     :valid-test #'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation "Lifespan of new visual objects being marked as NEW")
   (define-parameter :test-feats
     :valid-test #'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "Whether proc-display should use the features to compare items instead of just the chunk names")
   
   (define-parameter :viewing-distance :owner nil)
   (define-parameter :delete-visicon-chunks
     :valid-test #'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "Whether proc-display should delete and unintern the name of old chunks that were in the visicon")
   (define-parameter :scene-change-threshold
     :valid-test (lambda (x) (and (numberp x) (<= 0.0 x 1.0))) 
     :default-value 0.25
     :warning "a number in the range [0.0,1.0]"
     :documentation "Proportion of visicon which must change to signal a scene change")
   
   (define-parameter  :VISUAL-ENCODING-FACTOR
                      :valid-test #'nonneg
                      :warning "a non-negative number" 
                      :default-value  0.010
                      :documentation "Visual encoding factor-EMMA")
   (define-parameter  :VISUAL-ENCODING-EXPONENT
                      :valid-test #'nonneg
                      :warning "a non-negative number" 
                      :default-value 1.0
                      :documentation "Visual encoding exponent-EMMA")
   (define-parameter :EYE-SACCADE-RATE
                      :valid-test #'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.002
                      :documentation "Saccade rate - EMMA")
   (define-parameter :SACCADE-BASE-TIME
                      :valid-test #'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.020
     :documentation "Base saccade time - EMMA")
   (define-parameter :vis-obj-freq
                      :valid-test #'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.01
                      :documentation "Default visual object frequecny for EMMA")

   )
  :warning 'warn-vision
  :version "4.0a2-emma"
  :documentation "Vision-module with EMMA and chunks for internal object representation"
  :creation #'create-emma-module 
  :reset #'reset-emma-module 
  :query #'query-vision-module
  :request 'pm-module-request
  :params #'params-emma-module
)                 
