;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2003 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LICENSE.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : general-pm.lisp
;;; Version     : 4.0
;;; 
;;; Description : Base class for the perceptual-motor modules.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : [X] Strip out waiting-for-proc-p stuff?
;;; 
;;; ----- History -----
;;; 01.07.27 mdb
;;;             : Started 5.0 conversion. 
;;; 02.01.21 mdb
;;;             : Removed obsolete PROC-S function, renamed slot value function
;;;             : to be PROC-S.  Added INITIAITON-COMPLETE call.
;;; 2002.05.07 mdb [b6]
;;;             : Processor wasn't being set to BUSY while preparation was
;;;             : ongoing, which made Bad Things (tm) happen.  Fixed.
;;; 2002.06.05 mdb
;;;             : Added step-hook call in RUN-MODULE to support the environment.
;;; 2002.06.27 mdb [b7]
;;;             : Moved CHECK-SPECS here and made it print an actual informative
;;;             : warning message.  Wild.
;;; 2003.01.21 mdb [2.1.1]
;;;             : Updated the DM state a smidge less often.
;;; 2003.02.06 mdb
;;;             : Added a VERSION-STRING slot to the base module so each one 
;;;             : can track version numbers separately, in anticipation of 
;;;             : some more separate handling under ACT-R 6.0.
;;; 2003.04.30 mdb [2.1.2]
;;;             : Fixed bug in prepare-only motor movements not leaving
;;;             : processor free.
;;;
;;; 2004.10.20 Dan [First pass at moving things to ACT-R 6]
;;;             : Changed name to general-pm and reset version to 1.0a1
;;;             : Placed it in with the support code
;;;             :   modules that use it should have this call in them:
;;;             :   (require-compiled "GENERAL-PM" "ACT-R6:support;general-pm")
;;;             : Flagged my changes with comments starting with DAN
;;;             :
;;;             : Removed:
;;;             :   run-module    
;;;             :   new-message
;;;             :   pm-install-module
;;;             :   update-dm-state
;;;             :   print-input-queue
;;;             :   silent-events
;;;             :
;;;             : update-module gets called a little differently and if
;;;             :   possible I'd prefer to remove it.
;;;             :
;;;             : Renamed reset-module to reset-pm-module
;;;             :
;;;             : Put the spec class and methods in here
;;;             :
;;;             : Did not adjust the class definition though some things are
;;;             :  no longer necessary
;;; 2005.01.07 mdb 
;;;             : * Changed the class def to remove some obsolete stuff.
;;;             : * Added GENERIC-STATE-QUERY method.
;;;
;;; 2005.01.09 Dan
;;;             : Moved the provide to the end.
;;; 2005.01.12 Dan
;;;             : * Added the old-time and new-time parameters to update-module
;;;             :   which breaks backward compatibility but makes things 
;;;             :   cleaner for moving the device into a module.
;;;             : * Put the state case into the generic-state-query method
;;; 2005.04.23 Dan
;;;             : * Removed the stuffed slot from the attn-module class.
;;;             : * Added the print-module-status method for displaying the
;;;             :   query data for a module - a lot like print-module-state.
;;; 2005.05.11 Dan
;;;             : * Added the output parameter to queue-command so that I
;;;             :   can control the detail level for generated events.
;;;             :   Really, queue-command should be phased out, but for now
;;;             :   it's easier to just keep it around...
;;; 2005.07.22 Dan
;;;             : * Added the last-command reporting to the print-module-status
;;;             :   and the check to generic-query
;;;             : * Added the pm-module-request after method to make sure that
;;;             :   the last-cmd slot gets set for all modules.
;;; 2005.07.25 Dan
;;;             : * Changed the reset method so that last-command starts at none
;;;             :   which is the same as the value it gets on a clear.
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to declare ignored parameters in update-
;;;             :   module method (can that go away yet?).
;;;             : * Also added an ignore to queue-command for sent-by-act.
;;;             : * Updated version to 1.0.
;;; 2006.12.18 Dan
;;;             : * Took modality out of the print-module-status method.
;;; 2007.01.08 Dan
;;;             : * Took the ~% off the end of the jammed warning.
;;; 2007.05.24 Dan
;;;             : * Took the source-activation slot out of the attn-module class.
;;;             : * Removed the unnecessary partially-clear-attended method stuff.
;;; 2007.02.04 Dan
;;;             : * Adjusted preparation-complete so that it only clears the 
;;;             :   processor state if the previous movement initiation has
;;;             :   passed.  Doesn't occur in many situations, but when the
;;;             :   prepare reqeusts are made it can lead to some unexpected
;;;             :   free states of the module's processor.
;;; 2010.02.15 Dan
;;;             : * Took the :allocation class out of several slots of the
;;;             :   pm-module class because that could interfere with changing
;;;             :   things when running multiple models.
;;; 2011.04.26 Dan
;;;             : * Removed calls to pm-output and pm-warning, but left them
;;;             :   in since the old vision code in extras uses pm-output and
;;;             :   a bunch of the PM modules still use pm-warning.
;;;             : * Changed the preparation check and init-stamp to use mp-time-ms.
;;; 2011.05.17 Dan
;;;             : * Removed all usage of queue-command.
;;; 2014.04.25 Dan
;;;             : * Finally handling the todo that's been there since ACT-R/PM
;;;             :   with ACT-R 4.0 - remove the waiting-for-proc-p stuff.
;;;             : * Removed a lot of unused methods.
;;;             : * Removed some old comments and other minor cleanup while I'm
;;;             :   here.
;;; 2014.05.16 Dan [3.0]
;;;             : * Start of the conversion to deal with type-less chunks.
;;;             : * Setting the "last-command" for a pm-module checks whether 
;;;             :   it's "isa clear" or has a slot named cmd to get the value.
;;;             :   If that fails then it calls the pm-module-last-cmd-name
;;;             :   method for the module with the buffer and chunk-spec.
;;;             : * Removed the spec class and corresponding methods since 
;;;             :   audio was the only thing that used it and that's going to
;;;             :   a chunk-based search now.
;;; 2014.05.30 Dan
;;;             : * Added the test-for-clear-request function which can be used
;;;             :   by the modules to test for "isa clear" or "cmd clear" requests
;;;             :   based on the default type clear.
;;; 2014.11.07 Dan
;;;             : * Added a safety check to test-for-clear-request, now verify that
;;;             :   the modifier is always =, and cleaned up the logic in how it's
;;;             :   tested a little bit.
;;; 2015.03.16 Dan
;;;             : * Fixed a bug with preparation-complete introduced with the
;;;             :   2007.02.24 fix.  If a prepare was the first action the model
;;;             :   performed then (thus a negative init-stamp) it would leave
;;;             :   the processor busy until some other action cleared it.
;;; 2015.04.21 Dan
;;;             : * Attention modules have a new slot: unstuff-loc.  Which is
;;;             :   to be used as a flag for clearing a stuffed buffer after
;;;             :   some time has passed (value of the slot if not t which
;;;             :   indicates some module specific default).
;;;             : * And there's a method check-unstuff-buffer which can be 
;;;             :   scheduled for an attention module that will check if the
;;;             :   chunk in the buffer is an unmodified copy of the one which
;;;             :   was stuffed, or if that chunk no longer exists, just an 
;;;             :   unmodified stuffed chunk, and if that's true then it will
;;;             :   clear the buffer.
;;; 2015.06.05 Dan
;;;             : * Explicitly convert times for scheduled events to ms.
;;; 2015.07.28 Dan
;;;             : * Removed the *act-r-6.0-compatibility* hack.
;;; 2015.07.29 Dan
;;;             : * Split check-unstuff-buffer into two methods: check-unstuff-buffer
;;;             :   and unstuff-buffer.  The first is now used as a precondition
;;;             :   in an event and the other schedules the erase action.
;;;             : * Added the overstuff-loc slot to the attention module class.
;;;             : * Added an unstuff-event slot to the attention module class.
;;; 2015.08.13 Dan
;;;             : * Changed rand-time calls to randomize-time.
;;; 2015.09.16 Dan [4.0]
;;;             : * The clear method now calls complete-all-module-requests for
;;;             :   the module after it changes the state back.
;;;             : * The clear method now actually uses the feat-prep-time of the
;;;             :   specific module to schedule the change.
;;; 2015.09.21 Dan
;;;             : * Adding a prepare-spec slot to the pm-module class to use
;;;             :   for storing the spec for later completion since the prepare
;;;             :   method isn't passed the spec itself.
;;;             : * Added the request-spec slot to the movement-style class.
;;;             : * Prepare stores the request in the request-spec slot of the
;;;             :   style from the prepare-spec slot of the module.
;;;             : * Preparation-complete method completes the request if it is
;;;             :   not set to execute now i.e. it was just a prepare.
;;;             : * Finish-movement method now also requires the movement-style,
;;;             :   and it calls complete-spec on the request-spec of that style.
;;;             : * Defstyle automatically adds a request-spec keyword param as
;;;             :   one of the method keys.
;;;             : * Execute method now requires the request spec to be able to
;;;             :   complete it.
;;; 2015.09.23 Dan
;;;             : * The defstyle method for movements now completes the request
;;;             :   in the event that it is jammed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Perceptual/motor Modules base class
;;;; ---------------------------------------------------------------------- ;;;;

;;; PM-MODULE      [Class]
;;; Date        : 97.01.15, delta 2003.02.06
;;; Description : Base class for the various modules, includes input
;;;             : queue and basic state information.

(defclass pm-module ()
  ((input-queue :accessor input-q :initform nil)
   (modality-state :accessor mode-s :initform 'FREE :initarg :modality)
   (processor-state :accessor proc-s :initform 'FREE :initarg :processor)
   (preparation-state :accessor prep-s :initform 'FREE :initarg :preparation)
   (execution-state :accessor exec-s :initform 'FREE :initarg :execution)
   (state-change-flag :accessor state-change :initarg :state-change :initform nil)
   (module-name :accessor my-name :initarg :name :initform nil)
   (last-command :accessor last-cmd :initform nil :initarg :last-command)
   (last-prep :accessor last-prep :initarg :last-prep :initform nil)
   (exec-queue :accessor exec-queue :initarg :exec-queue :initform nil)
   (feature-prep-time :accessor feat-prep-time  :initarg :feat-prep-time :initform 0.050)
   (movement-initiation-time :accessor init-time :initarg :init-time :initform 0.050)
   (init-stamp :accessor init-stamp :initarg :init-stamp :initform -0.1)
   (burst-time :accessor burst-time :initarg :burst-time :initform 0.050)
   (version-string :accessor version-string :initarg :version-string :initform "")
   (prepare-spec :accessor prepare-spec :initform "")))



;;; CHANGE-STATE      [Method]
;;; Date        : 97.02.10
;;; Description : Change one or more of a module's state flags.

(defgeneric change-state (module &key proc exec prep last)
  (:documentation  "Change one or more of a module's state flags."))

(defmethod change-state ((module pm-module) &key proc exec prep last)
  (when proc (setf (proc-s module) proc))
  (when exec (setf (exec-s module) exec))
  (when prep (setf (prep-s module) prep))
  (when last (setf (last-cmd module) last))
  (if (or (eq (proc-s module) 'busy) (eq (exec-s module) 'busy)
          (eq (prep-s module) 'busy))
    (setf (mode-s module) 'busy)
    (setf (mode-s module) 'free))
 
  (setf (state-change module) t))


;;; CLEAR      [Method]
;;; Date        : 97.03.03
;;; Description : Clears a module's state, takes one feature prep time.

#| CLEAR is already a generic function in MCL.
(defgeneric clear (module)
  (:documentation  "Clears a PM module."))
|#

(defmethod clear ((module pm-module))
  (when (not (check-jam module))
    (change-state module :prep 'busy)
    (schedule-event-relative (feat-prep-time module) 'change-state :destination (my-name module) :module (my-name module) :params '(:last none :prep free))
    (schedule-event-relative (feat-prep-time module) 'complete-all-module-requests :params (list (my-name module)) :output nil :priority -1) 
    (setf (last-prep module) nil)
    (setf (exec-queue module) nil)
    (setf (init-stamp module) -0.1)))


;;; CHECK-JAM      [Method]
;;; Date        : 97.02.18
;;; Description : Modules can't take certain types of commands if they are
;;;             : already busy, and this checks the preparation state of a
;;;             : module for just this problem. 

(defgeneric check-jam (module)
  (:documentation "Returns NIL if the PM module is free, otherwise prints an error message and returns T."))

(defmethod check-jam ((module pm-module))
  (if (not (eq (prep-s module) 'busy))
    nil
    (progn
      (model-warning "Module ~S jammed at time ~S" (my-name module) (mp-time))
      t)))


;;; RESET-PM-MODULE      [Method]
;;; Date        : 97.02.18
;;; Description : When a module needs to be reset, that means both that all
;;;             : state indicators should be set to FREE and the input queue
;;;             : should be cleared.

(defgeneric reset-pm-module (module)
  (:documentation "Resets a PM module to base state:  all flags free, empty input queue."))

(defmethod reset-pm-module ((module pm-module))
  (setf (proc-s module) 'free)
  (setf (exec-s module) 'free)
  (setf (prep-s module) 'free)
  (setf (mode-s module) 'free)
  (setf (last-cmd module) 'none)
  
  (setf (input-q module) nil)
  (setf (last-prep module) nil)
  (setf (exec-queue module) nil)
  (setf (init-stamp module) -0.1))


;;; PRINT-MODULE-STATE      [Method]
;;; Date        : 98.05.28
;;; Description : For debugging help, this prints the state of the module
;;;             : to stdout.

(defgeneric print-module-state (module)
  (:documentation "Prints a representation of a PM module's state."))

(defmethod print-module-state ((mod pm-module))
  (format t "~& State of module ~S" (my-name mod))
  (format t "~% Modality:     ~S" (mode-s mod))
  (format t "~% Preparation:  ~S" (prep-s mod))
  (format t "~% Processor:    ~S" (proc-s mod))
  (format t "~% Execution:    ~S" (exec-s mod))
  (format t "~% Last command: ~S" (last-cmd mod)))



(defgeneric pm-module-last-cmd-name (module buffer-name chunk-spec)
  (:documentation "Determine the name of the last command when it's not 'isa clear' and doesn't have a cmd slot."))

(defmethod pm-module-last-cmd-name ((module pm-module) buffer-name chunk-spec)
  (declare (ignorable module buffer-name chunk-spec))
  )

(defgeneric pm-module-request (module buffer-name chunk-spec)
  (:documentation "Handles a request from a buffer."))

;;; This after method is used to make sure that all commands
;;; processed record the command in the module.

(defmethod pm-module-request :after ((module pm-module) buffer-name chunk-spec)
  (let ((last-cmd (or (let ((spec (chunk-spec-slot-spec chunk-spec)))
                        (and (= (length spec) 1)
                             (eq 'clear (spec-slot-name (first spec)))
                             (spec-slot-value (first spec))
                             'clear))
                      (let ((spec (chunk-spec-slot-spec chunk-spec 'cmd)))
                        (and (= (length spec) 1)
                             (spec-slot-value (first spec))))
                      (pm-module-last-cmd-name module buffer-name chunk-spec))))
    
    (when (and last-cmd (not (eql (last-cmd module) last-cmd)))
      (change-state module :last last-cmd))))



;;; PRINT-MODULE-STATUS 

(defgeneric print-module-status (module)
  (:documentation "Prints the module's state in query form"))

(defmethod print-module-status ((mod pm-module))
  (command-output "  preparation free      : ~S"
                  (eq (prep-s mod) 'free))
  (command-output "  preparation busy      : ~S"
                  (eq (prep-s mod) 'busy))
  (command-output "  processor free        : ~S"
                  (eq (proc-s mod) 'free))
  (command-output "  processor busy        : ~S"
                  (eq (proc-s mod) 'busy))
  (command-output "  execution free        : ~S"
                  (eq (exec-s mod) 'free))
  (command-output "  execution busy        : ~S"
                  (eq (exec-s mod) 'busy))
  (command-output "  last-command          : ~S"
                  (last-cmd mod)))
  
  



(defgeneric check-state (module &key modality preparation 
                                   execution processor last-command)
  (:documentation "Does a quick test of the state of a PM module, returning T iff all the specified states match."))


(defmethod check-state ((mod pm-module) &key modality preparation 
                          execution processor last-command)
  (cond ((and modality (not (eq modality (mode-s mod)))) nil)
        ((and preparation (not (eq preparation (prep-s mod)))) nil)
        ((and execution (not (eq execution (exec-s mod)))) nil)
        ((and processor (not (eq processor (proc-s mod)))) nil)
        ((and last-command (not (eq last-command (last-cmd mod)))) nil)
        (t t)))


(defgeneric generic-state-query (module buffer slot value)
  (:documentation "Handles BUSY/FREE tests on STATE, MODALITY, EXECUTION, PREPARATION and PROCESSOR."))

(defmethod generic-state-query ((module pm-module) buffer slot value)
  (case slot
       ((state modality)
         (case value
          (busy
           (eq (mode-s module) 'busy))
          (free
           (eq (mode-s module) 'free))
          (t (model-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (execution
        (case value
          (busy
           (eq (exec-s module) 'busy))
          (free
           (eq (exec-s module) 'free))
          (t (model-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" 
                            buffer slot value))))
       (preparation
        (case value
          (busy
           (eq (prep-s module) 'busy))
          (free
           (eq (prep-s module) 'free))
          (t (model-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" 
                            buffer slot value))))
       (processor
        (case value
          (busy
           (eq (proc-s module) 'busy))
          (free
           (eq (proc-s module) 'free))
          (t (model-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" 
                            buffer slot value))))
    (last-command 
     (eql (last-cmd module) value))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; preparation and execution stuff



;;; PREPARATION-COMPLETE      [Method]
;;; Date        : 98.07.22
;;; Description : When movement preparation completes: change the prep
;;;             : state, check to see if the movement just prepared wants
;;;             : to execute right away, and then possibly execute a 
;;;             : movement.

(defgeneric preparation-complete (module)
  (:documentation "Method to be called when movement preparation is complete."))

(defmethod preparation-complete ((module pm-module))
  (change-state module :prep 'free)
  (when (last-prep module)
    (if (exec-immediate-p (last-prep module))
      (setf (exec-queue module)
            (append (exec-queue module) (mklist (last-prep module))))
      (progn
        (when (or (minusp (init-stamp module))
                  (and (plusp (init-stamp module))
                       (>= (mp-time-ms) (+ (init-stamp module) (seconds->ms (init-time module))))))
          (change-state module :proc 'FREE))
        (complete-request (request-spec (last-prep module))))))
  (maybe-execute-movement module))



;;; MAYBE-EXECUTE-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : If there is a movement queued and the motor state is FREE,
;;;             : then execute the movment.  Also, free the processor state
;;;             : with an event if necessary.

(defgeneric maybe-execute-movement (module)
  (:documentation "If there are any movements in <module>'s execution queue, execute one."))

(defmethod maybe-execute-movement ((module pm-module))
  (when (and (exec-queue module) (eq (exec-s module) 'FREE))
    (perform-movement module (pop (exec-queue module)))))


;;; PREPARE      [Method]
;;; Date        : 98.08.21
;;; Description : Build a movement style instance via APPLY, set it to not
;;;             : automatically execute itself, and prepare it.

(defgeneric prepare (module &rest params)
  (:documentation "Prepare a movement to be executed, but don't execute it. The first of <params> should be the name of a movement style class."))

(defmethod prepare ((module pm-module) &rest params)
  (let ((inst (apply #'make-instance params)))
    (setf (exec-immediate-p inst) nil)
    (setf (request-spec inst) (prepare-spec module))
    (prepare-movement module inst)))


;;; EXECUTE      [Method]
;;; Date        : 98.08.21
;;; Description : Executing the previously prepared command requires
;;;             : [1] A previously-prepared command, and
;;;             : [2] No command currently being prepared.
;;;             : If those are OK, put the current style instance in the
;;;             : execution queue and go for it.

(defgeneric execute (module request)
  (:documentation "Tells <module> to execute the last movement prepared."))

(defmethod execute ((module pm-module) request)
  (cond ((not (last-prep module))
         (model-warning "Motor Module has no movement to EXECUTE."))
        ((eq (prep-s module) 'BUSY)
         (model-warning "Motor Module cannot EXECUTE features being prepared."))
        (t
         (setf (request-spec (last-prep module)) request)
         (setf (exec-queue module)
               (append (exec-queue module) (mklist (last-prep module))))
         (maybe-execute-movement module))))


;;; PM-PREPARE-MOTOR-MTH      [Method]
;;; Date        : 98.09.24
;;; Description : If RPM is to begin a run with features already prepared,
;;;             : this is the method to do it.  Create a movement instance,
;;;             : kill the exec-immediate, and set the last prepared movement
;;;             : to the created movement.

(defgeneric pm-prepare-mvmt-mth (module params)
  (:documentation "Create the movement specified in <params>, which should begin with the name of a movement style, and consider it prepared. To be called only at model initialization."))

(defmethod pm-prepare-mvmt-mth ((module pm-module) params)
  (let ((inst (apply #'make-instance params)))
    (setf (exec-immediate-p inst) nil)
    (setf (last-prep module) inst)))





;;;; ---------------------------------------------------------------------- ;;;;
;;;; MOVEMENT-STYLE class and methods
;;;; ---------------------------------------------------------------------- ;;;;

(defclass movement-style ()
  ((fprep-time :accessor fprep-time :initform nil :initarg :fprep-time)
   (exec-time :accessor exec-time :initform nil :initarg :exec-time)
   (finish-time :accessor finish-time :initform nil :initarg :finish-time)
   (exec-immediate-p :accessor exec-immediate-p :initform t
                     :initarg :exec-immediate-p)
   (num-features :accessor num-features :initform nil
                 :initarg :num-features)
   (style-name :accessor style-name :initarg :style-name :initform nil)
   (feature-slots :accessor feature-slots :initarg :feature-slots 
                  :initform nil)
   (request-spec :accessor request-spec :initarg :request-spec :initform nil)))


;;; PREPARE-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : Change the prep state, compute the feature prep time,
;;;             : note that we're the last feature the MM has prepared,
;;;             : and queue the preparation complete event.

(defgeneric prepare-movement (module movement)
  (:documentation "Tell <module> to prepare <movement>."))

(defmethod prepare-movement ((module pm-module) (mvmt movement-style))
  (change-state module :prep 'BUSY :proc 'BUSY)
  (setf (fprep-time mvmt) 
        (randomize-time (compute-prep-time module mvmt)))
  (setf (last-prep module) mvmt)
  (schedule-event-relative (seconds->ms (fprep-time mvmt)) 'preparation-complete :time-in-ms t :destination (my-name module) :module (my-name module)))



;;; COMPUTE-PREP-TIME      [Method]
;;; Date        : 98.07.22
;;; Description : Computing the prep time.  If this is a different kind of
;;;             : movement or a totall new movement, then just return the
;;;             : number of features times the time per feature.  If the
;;;             : old movement is similar, compute the differences (a 
;;;             : method for this must be supplied).

(defgeneric compute-prep-time (module movement)
  (:documentation "Return the feature preparation time for <movement>."))

(defmethod compute-prep-time ((module pm-module) (mvmt movement-style))
  (if (or (null (last-prep module))
          (not (eq (style-name mvmt) (style-name (last-prep module)))))
    (* (feat-prep-time module) (num-to-prepare mvmt))
    (* (feat-prep-time module)
       (feat-differences mvmt (last-prep module)))))




;;; PERFORM-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : Performing a movement has several pieces to it.  First,
;;;             : bookkeeping (exec state and start time).  Next we need
;;;             : to compute times.  Then, queue the events (movement
;;;             : specific) that reflect our output, and finally queue
;;;             : the event indicating completion of the movement.

(defgeneric perform-movement (module movement)
  (:documentation "Have <module> perform <movement>."))

(defmethod perform-movement ((module pm-module) (mvmt movement-style))
  (schedule-event-relative (seconds->ms (init-time module)) 'initiation-complete 
                           :time-in-ms t :destination (my-name module) :module (my-name module))
  
  (change-state module :proc 'BUSY :exec 'BUSY)
  
  (setf (init-stamp module) (mp-time-ms))
  
  (setf (exec-time mvmt) (compute-exec-time module mvmt))
  (setf (finish-time mvmt) (compute-finish-time module mvmt))
  (queue-output-events module mvmt)
  (queue-finish-event module mvmt))


(defmethod initiation-complete ((module pm-module))
  (change-state module :proc 'FREE))



;;; FINISH-MOVEMENT      [Method]
;;; Date        : 98.07.22
;;; Description : When a movement completes, FREE the execution state, and
;;;             : check to see if there were any movements queued.

(defgeneric finish-movement (module mvmt)
  (:documentation "Method called when a movement finishes completely."))

(defmethod finish-movement ((module pm-module) (mvmt movement-style))
  (change-state module :exec 'free)
  (complete-request (request-spec mvmt))
  (maybe-execute-movement module))



;;; COMPUTE-FINISH-TIME      [Method]
;;; Date        : 98.07.22
;;; Description : Default finish time is simply execution time plus the
;;;             : burst time--some styles will need to override this.

(defgeneric compute-finish-time (module movement)
  (:documentation "Return the finish time of <movement>."))

(defmethod compute-finish-time ((module pm-module) (mvmt movement-style))
  "Return the finish time of the movement."
  (+ (burst-time module) (exec-time mvmt)))


;;; QUEUE-FINISH-EVENT      [Method]
;;; Date        : 98.07.22
;;; Description : Queue the event that frees the exec of the MM.

(defgeneric queue-finish-event (module movement)
  (:documentation "Queue the FINISH-MOVEMENT associated with <movement>."))

(defmethod queue-finish-event ((module pm-module) (mvmt movement-style))
  (schedule-event-relative (seconds->ms (finish-time mvmt)) 'finish-movement 
                           :time-in-ms t :destination (my-name module) 
                           :params (list mvmt) :module (my-name module)
                           :details (format nil "~a" 'finish-movement)))


;;; Stubs that require overrides.

(defgeneric compute-exec-time (module movement)
  (:documentation "Return the execution time of <movement>."))

(defmethod compute-exec-time ((module pm-module) (mvmt movement-style))
  (error "No method defined for COMPUTE-EXEC-TIME."))


(defgeneric queue-output-events (module movement)
  (:documentation "Queue the events--not including the FINISH-MOVEMENT--that <movement> will generate."))

(defmethod queue-output-events ((module pm-module) (mvmt movement-style))
  (error "No method defined for QUEUE-OUTPUT-EVENTS."))


(defgeneric feat-differences (movement1 movement2)
  (:documentation "Return the number of different features that need to be prepared."))

(defmethod feat-differences ((move1 movement-style) (move2 movement-style))
  ;(declare (ignore move1 move2))
  (error "No method defined for FEAT-DIFFERENCES."))




(defgeneric num-possible-feats (movement)
  (:documentation "Return the maximum number of features that could possibly need to be prepared."))

(defmethod num-possible-feats ((mvmt movement-style))
  (1+ (length (feature-slots mvmt))))


(defgeneric num-to-prepare (movement)
  (:documentation "Return the number of features actually needed to prepare <movement>."))

(defmethod num-to-prepare ((mvmt movement-style))
  (1+ (length (remove :DUMMY
                      (remove nil
                              (mapcar #'(lambda (name)
                                          (slot-value mvmt name))
                                      (feature-slots mvmt)))))))



(defmacro defstyle (name base-class &rest params)
  "Macro that defines new motor movement styles.  Pass in the name and the base 
class [if NIL is passed, it will default to MOVEMENT-STYLE] and the base 
parameters.  This will create a class and a method for any PM Module for the 
class."
  `(progn
     (defclass ,name (,(if (not base-class) 'movement-style base-class))
       ,(build-accessors params)
       (:default-initargs
         :style-name ,(sym->key name)
         :feature-slots ',params))
     (defmethod ,name ((module pm-module) &key ,@params request-spec)
       (if (or (check-jam module) (check-specs ',name ,@params))
           (complete-request request-spec)
         (prepare-movement module
                           (make-instance ',name :request-spec request-spec
                             ,@(build-initializer params)))))))

(defun check-specs (name &rest specs)
  "If there is an invalid specification, return something, else NIL"
  (when (member nil specs)
    (model-warning "NIL specification passed to a PM command ~S: ~S" name specs)
    t))

;;; BUILD-ACCESSORS      [Function]
;;; Date        : 98.11.02
;;; Description : Helper function for DEFSTYLE.

(defun build-accessors (params)
  "From a list of parameters, a list of slot definitions."
  (let ((accum nil))
    (dolist (param params (nreverse accum))
      (push (list param :accessor param :initarg (sym->key param)
                  :initform nil) accum))))


;;; BUILD-INITIALIZER      [Function]
;;; Date        : 98.11.02
;;; Description : Helper function for DEFSTYLE.

(defun build-initializer (params)
  "From a list of parameters, build a list for the make-instance initializer."
  (let ((accum nil))
    (dolist (param params (nreverse accum))
      (push (sym->key param) accum)
      (push param accum))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Attentional modules
;;;; ---------------------------------------------------------------------- ;;;;

;;; ATTN-MODULE      [Class]
;;; Date        : 00.06.09
;;; Description : Class for modules that have attentional capability.
;;;             : CURRENTLY-ATTENDED hold the focus object
;;;             : object gets
;;;             : CURRENT-MARKER denotes the location/event currently attended

(defclass attn-module (pm-module)
  ((currently-attended :accessor currently-attended 
                       :initarg :currently-attended :initform nil)
   (current-marker :accessor current-marker :initarg :current-marker 
                   :initform nil)
   
   ;; mdb moved from vision module definition 2005.01.07
   (loc-failure :accessor loc-failure :initform nil)
   (attend-failure :accessor attend-failure :initform nil)
   
   ;; modules can "unstuff" perceptual info if needed
   (unstuff-loc :accessor unstuff-loc :initform nil)
   (unstuff-event :accessor unstuff-event :initform nil)
   (overstuff-loc :accessor overstuff-loc :initform nil)))


(defmethod reset-pm-module ((module attn-module))
  (call-next-method)
  (clear-attended module)
  (setf (current-marker module) nil))  

(defmethod clear ((module attn-module))
  (call-next-method)
  (setf (current-marker module) nil)
  (clear-attended module))


(defgeneric clear-attended (module)
  (:documentation "Set <module> so that it is attending nothing."))

(defmethod clear-attended ((module attn-module))
  (setf (currently-attended module) nil))

(defgeneric set-attended (module object)
  (:documentation "Note that <module> is now attending <object>."))

(defmethod set-attended ((module attn-module) obj)
  (setf (currently-attended module) obj))

(defmethod check-unstuff-buffer ((module attn-module) buffer chunk)
  (let ((current (buffer-read buffer)))
    (and
     current
     (multiple-value-bind (copy was-copy) (chunk-copied-from-fct current)
       (declare (ignore copy))
       (or ;; the copy of the chunk is still in the buffer unchanged
        ;; regardless of whether the original has changed
        
        (eq was-copy chunk)
        
        ;; the original is no longer a chunk (deleted or purged)
        ;; so all that can be tested is whether the chunk in the 
        ;; buffer was stuffed, was copied from some chunk, and 
        ;; hasn't been changed
        (and (null (chunk-p-fct chunk))
             was-copy
             (query-buffer buffer '(buffer unrequested))))))))

(defmethod unstuff-buffer ((module attn-module) buffer chunk)
  (declare (ignore chunk))
  (schedule-event-now 'erase-buffer :params (list buffer) :module (my-name module)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Detect the clear requests.

(defun test-for-clear-request (spec)
  (when (act-r-chunk-spec-p spec)
    (let ((main-spec (chunk-spec-slot-spec spec)))
      (and (= (length main-spec) 1)
            (eq '= (spec-slot-op (first main-spec)))
            (or (and 
                 (eq (spec-slot-name (first main-spec)) 'clear)
                 (spec-slot-value (first main-spec)))
                (and
                 (eq (spec-slot-name (first main-spec)) 'cmd)
                 (eq (spec-slot-value (first main-spec)) 'clear)))))))
     
    
(provide "GENERAL-PM")

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
