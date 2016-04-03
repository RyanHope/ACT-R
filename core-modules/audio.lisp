;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2005 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : audio.lisp
;;; Version     : 4.0
;;; 
;;; Description : Source for RPM's Audition Module
;;; 
;;; Bugs        : * 
;;; 
;;; Todo        : * [x] Should there be audio buffer stuffing as well?
;;;             : * [X] Add a set-audioloc-defaults command like the set-visloc-
;;;             :       default to give more control over stuffing so that
;;;             :       the :hear-newest-only parameter can go away.
;;;             : * [X] Change the request function so that modifiers and ranges
;;;             :       are accepted and add pitch as an option (alternatively
;;;             :       convert the audicon over to holding chunks and use the
;;;             :       more general chunk matching tools...)
;;;             : * [ ] Should it automatically stuff aural-location when there
;;;             :       are events in the audicon and the buffer is cleared 
;;;             :       instead of waiting for a change before doing so?
;;;             : * [X] Deal with possible long run timing issues due to 
;;;             :       floating point imprecision since detection time involves 
;;;             :       an addition of a "current" time which could be too big
;;;             :       to represent milliseconds with a delay that's likely to be
;;;             :       in the 50-200ms range.
;;;             : * [ ] There's a problem with using the ID and EVENT slots to
;;;             :       point to the original chunk since that means it has to
;;;             :       stay around (can't just delete it when done).  The right
;;;             :       solution is to work like vision where the object chunk
;;;             :       refers to the actual location chunk in the buffer and the
;;;             :       location chunk doesn't have a "self link" which will 
;;;             :       require changing how things map back to the module chunks.
;;; ----- History -----
;;; 
;;; 2005.01.07 mdb [act6a1]
;;;             : Transition to ACT6 stuff.
;;; 2005.01.08 Dan
;;;             : More updates to move to 6
;;;             :
;;;             : Changed aural-location to audio-event in request-audio-module
;;;             : (alternatively audio-event could be changed to aural-location
;;;             : in reset-audio-module but this way is backward compatible 
;;;             : to ACT-R 5)
;;;             :
;;;             : Removed the :offset parameter in the scheduling of find-sound
;;;             : in request-audio-module because it isn't defined in find-sound
;;;             :
;;;             : Changed find-sound to put the chunk into the buffer
;;;             :
;;;             : Changed find-sound because the chunk that comes in doesn't
;;;             : have the same name as the one in the audicon because it will
;;;             : be a copy from the buffer.  
;;;             :
;;;             : To get around that (for now at least) I've added an id slot
;;;             : to the audio-event chunk-type which will always have the
;;;             : original value.
;;;             :
;;;             : The event->dmo method was modified to set that value.
;;;             : 
;;;             : Changed audio-encoding-complete so that it sets the chunk
;;;             : in the aural buffer.  
;;;             :
;;;             : Related to the issue above with sound event names - the 
;;;             : sound put into the buffer has its event set to the "original" 
;;;             : event name (the id slot of the audio-event) which doesn't 
;;;             : correspond to the name of a chunk in DM, but it will match
;;;             : an audio-event with that value in its id slot (assuming the
;;;             : aural-location buffer has cleared so that the chunk goes to
;;;             : DM).
;;;             : 
;;;             : That seems reasonable for now at least.  
;;;             :
;;;             : Put the aural-location stuffing in:
;;;             ; Added the :around method for new-sound-event
;;;             : Added the stuff-sound-buffer function.
;;;             :
;;; 2005.01.09 Dan
;;;             : Added the clearing of the audicon to the reset-audio-module
;;;             : function.
;;;             : Added the word chunk to the audicon.
;;; 2005.01.10 Dan
;;;             : Maintain the stuffed slot of the audio module now since
;;;             : I added the buffer stuffing back in.
;;; 2005.01.11 mdb
;;;             : Put in parameter doc strings.
;;; 2005.01.21 Dan
;;;             : * Removed use of buffer-chunk and replaced with buffer-read.
;;; 2005.01.21 Dan
;;;             : * Wrapped the proclaim in an eval-when because otherwise
;;;             :   it may not actually affect the compilation.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium" to some of the events that are
;;;             :   scheduled to play friendly with the new detail level.
;;; 2005.04.23 Dan
;;;             : * Updated find-sound so that it indicates whether the chunk
;;;             :   being put into the buffer was stuffed or not.
;;;             : * Changed stuff-sound-buffer to indicate that.
;;;             : * Removed the check of stuffed from query-audio-module.
;;;             : * Added attended as a possible query but I'm unsure if I've
;;;             :   got the testing quite right...
;;; 2005.04.29 Dan
;;;             : * Added a print-audicon command that works basically like
;;;             :   print-visicon for visual - it prints a table of info for
;;;             :   the sound-events currently in the audicon.
;;; 2005.07.22 Dan
;;;             : * Updated the module definition to use the pm-module-request
;;;             :   method and renamed the audio-module-request function 
;;;             :   accordingly.
;;; 2005.08.03 Dan
;;;             : * Added a find-sound-failure event to the trace when find-
;;;             :   sound fails.  Also adjusted the find-sound event scheduling
;;;             :   so that it gets output in the medium level trace detail.
;;; 2005.08.10 Dan
;;;             : * Commented out the offset value in the audio-event request
;;;             :   because it wasn't used.
;;; 2005.12.14 Dan
;;;             : * Added :sound-decay-time parameter which seems to have been
;;;             :   lost in the move to 6.
;;; 2006.01.04 Dan
;;;             : * Removed a duplicate instance of :tone-recode-delay in the
;;;             :   case of the parameter handling in params-audio-module and 
;;;             :   replaced it with :tone-detect-delay (which is what it 
;;;             :   should have been). 
;;; 2006.03.24 Dan
;;;             : * Added a new parameter called :hear-newest-only and changed 
;;;             :   stuff-sound-buffer to include :onset :highest when that's
;;;             :   set because it seems like often one might want the newest 
;;;             :   sound to be the one that gets stuffed into the buffer. 
;;;             :   The default is nil to keep it compatible with the old
;;;             :   version for now, but should it default to t?
;;; 2006.05.03 Dan
;;;             : * Fixed a bug in pm-module-request for aural-location requests
;;;             :   in specifying the location (it was testing the onset slot.)
;;; 2006.05.03 Dan
;;;             : * Turns out that loc-failure and attend-failure weren't
;;;             :   really being set/cleared so add that in now.
;;; 2006.05.03 Dan
;;;             : * Fixed attend-sound so that it checks the current-audicon
;;;             :   so that old sounds get purged as needed.
;;; 2006.09.08 Dan
;;;             : * Changed several parameter tests from posnum to nonneg.
;;; 2006.11.20 Dan
;;;             : * Fixed the bug in stuff-sound-buffer because it should be
;;;             :   :onset 'highest not :highest...
;;;             : * Changed the version to 2.2 (dropped the a1) and updated the
;;;             :   doc string so that it doesn't say "first pass".
;;; 2006.12.20 Dan 
;;;             : * Changed the version in the class definition too...
;;; 2007.01.03 Dan
;;;             : * Changed the scheduling of the stuff-sound-buffer event
;;;             :   to :output nil and :maintenance t because the set-buffer-chunk 
;;;             :   ... requested nil shows that the stuffing occurs already - 
;;;             :   no point in two events showing for the same thing.
;;;             : * Also changed the test for the empty buffer slightly because
;;;             :   buffer-read is a bit of a hack for that purpose.
;;;             : * Took attended out of the audio-event chunk-type and made it
;;;             :   a request parameter instead (actually just started using the 
;;;             :   request parameter that was already there).
;;; 2007.01.04 Dan
;;;             : * Took the string column out of the audicon printing because
;;;             :   its going to be the same as content when it's provided.
;;;             : * Similarly, changed the content column to be printed with
;;;             :   ~s instead of ~a so that strings are differentiated from
;;;             :   symbols for content.
;;;             : * Added a column for detectable to the audicon printing.
;;;             : * Changed attend-sound to check exec-s instead of check-jam
;;;             :   because the attend-sound doesn't set the preparation state
;;;             :   to busy (which is what check-jam looks at).
;;;             : * Use randomize-time in scheduling the attend-sound-failure
;;;             :   event.
;;;             : * Removed the update function from the module definition
;;;             :   because it didn't do anything.
;;;             : * Fixed new-sound-event to better check for model/mp/module
;;;             :   and report a warning if not available - also switched the
;;;             :   return value to t/nil instead of returning the stuff-sound
;;;             :   scheduler event.
;;;             : * Converted the new-*-sound methods to functions so that
;;;             :   parameter validation is handled explicitly.  So that
;;;             :   ACT-R warnings can be printed instead of Lisp errors being
;;;             :   generated when invalid values are used (yes there are more
;;;             :   "CLOS-y" ways of doing it, but call me old fashioned...).
;;;             : * Took the optional instr parameter out of new-other-sound
;;;             :   because the string component of the sound-events doesn't
;;;             :   really have a purpose. [Or am I missing something?]
;;; 2007.01.10 Dan
;;;             : * Added location and kind optional parameters to new-other-sound.
;;;             : * Changed the call to get-articulation-time because the 
;;;             :   speech module is no longer needed as a parameter (though the
;;;             :   module itself still needs to exist).
;;; 2007.01.18 Dan
;;;             : * Turns out that stuff-sound-buffer can't be a maintenance
;;;             :   event because that event needs to be able to trigger
;;;             :   conflict-resolution because until the schedule-set-buffer-chunk
;;;             :   event gets scheduled other model events could have pushed
;;;             :   conflict-resolution into the future... (probably doesn't
;;;             :   make sense to anyone else, but I know what it means)
;;; 2007.03.29 Dan
;;;             : * Incremented version to 2.3.
;;;             : * Added the set-audloc-default command and depricated the
;;;             :   hear-newest-only parameter because of that.
;;; 2007.04.03 Dan
;;;             : * Some cleanup of the set-audloc-default command - better
;;;             :   testing of params and validity (allowing none for example).
;;; 2008.06.03 Dan
;;;             : * Added an after method for clear to clear the error states
;;;             :   as is done for vision, but maybe that needs to move up to
;;;             :   the attn-module class method.
;;; 2011.04.28 Dan
;;;             : * Added a declaim to avoid a compiler warning about an
;;;             :   undefined function.
;;; 2011.05.16 Dan
;;;             : * Replaced pm-warning calls with model-warning.
;;; 2011.05.17 Dan
;;;             : * Replaced queue-command calls with schedule-event-relative.
;;; 2011.05.18 Dan
;;;             : * Removed detectable-time since it was the same as detect-at-time
;;;             :   and there's no need for two methods that compute the same thing.
;;;             : * Starting to convert over to using ms internally for the sound-
;;;             :   event components to avoid issues with math on seconds.
;;; 2011.11.09 Dan
;;;             : * Commented out the *enable-package-locked-errors* line for ACL 
;;;             :   because it no longer seems to be necessary and it sets up a 
;;;             :   dangerous situation for users since that setting enables the
;;;             :   redefinition of CL functions without warning.
;;; 2012.05.14 Dan
;;;             : * Fixed a bug with the offset time of the audio-event chunks
;;;             :   being in ms while the onset was in seconds.
;;; 2012.05.14 Dan [2.4]
;;;             : * Added another request parameter to the aural-location buffer:
;;;             :   :finished.  It can be requested as t or nil to determine if 
;;;             :   a sound is already completed or not.
;;;             : * A corresponding finished query is also available which works
;;;             :   like the attended query -- if there is a chunk in the buffer
;;;             :   then finished tests whether the property of that chunk matches
;;;             :   the queried value.
;;;             : * Changed the finished-p method so it always returns t or nil
;;;             :   since >= only returns a generalized boolean and that matters
;;;             :   for testing in the query.
;;;             : * Updated set-audloc-default so that it also accepts finished.
;;;             : * Added a duration slot to the audio-event type so that one
;;;             :   doesn't have to do the math with offset and onset values.
;;;             : * Added the audio-event-ended function which gets scheduled 
;;;             :   by attend-sound to set the offset and duration of the chunk
;;;             :   in the aural-location buffer if it's still there when the 
;;;             :   attended sound stops.
;;; 2013.10.02 Dan
;;;             : * Commented out the optimize proclaim since that persists and
;;;             :   may or may not be useful anyway.
;;; 2014.03.17 Dan [3.0]
;;;             : * Changed the query-buffer call to be consistent with the new
;;;             :   internal code.
;;; 2014.05.16 Dan
;;;             : * Continue the conversion to type-less chunks.
;;;             : * First step is to switch over to testing the chunks in the audicon!
;;;             : * Print-audicon sorts the events by onset time (lowest first).
;;; 2014.05.19 Dan
;;;             : * New-other-sound now allows for specifying additional slots and
;;;             :   values for the event and/or the sound chunks.  They are provided
;;;             :   as 3 element lists after all the optional parameters.  The first
;;;             :   item in the list is either :evt, :sound, or :both to indicate
;;;             :   which chunk gets the slot and value specified with the second 
;;;             :   and third items respectively.
;;;             : * Testing the chunks in the audicon instead of the sound-event
;;;             :   object allows for +aural-location to specify things more freely:
;;;             :   slots can be specified more than once, any operator is allowed,
;;;             :   user specified slots for an event can be provided, and higest and
;;;             :   lowest can be used for any slot.
;;; 2014.05.30 Dan
;;;             : * Use the test-for-clear-request function in pm-module-request.
;;;             : * Use an isa sound/audio-event when creating the chunks so that
;;;             :   the "backward" slots exist when needed.
;;; 2014.08.13 Dan
;;;             : * Either I can't use the name of the audicon chunk in the ID
;;;             :   and EVENT slots or I can't delete that chunk when it leaves
;;;             :   the audicon!  I think the right solution is to get rid of 
;;;             :   the ID slot and have the EVENT refer to the actual audio-event
;;;             :   chunk that's in the buffer, but that requires a lot of reworking
;;;             :   things (like the internals of vision so that things can refer
;;;             :   to the original internal module chunk).  So, for now
;;; 2014.10.30 Dan [3.1]
;;;             : * I guess that should have said "for now leave it alone and
;;;             :   don't delete the chunks".
;;;             : * Eliminating the previously depricated :hear-newest-only
;;;             :   parameter.
;;;             : * Changed the last-command value for an aural-location request
;;;             :   from audio-event to find-sound.
;;;             : * Added an event to the queue when a set-audloc-default request
;;;             :   is processed.
;;;             : * Added some more chunks to those defined by the module.
;;; 2015.03.20 Dan
;;;             : * Failures now set the buffer failure flags using set-buffer-failure.
;;; 2015.03.23 Dan
;;;             : * Specify whether a find-sound-failure is requested or not i.e.
;;;             :   if it's stuffed then that's not requested.
;;; 2015.04.22 Dan [3.2]
;;;             : * Add the unstuff-aural-location parameter which will cause 
;;;             :   stuffed chunks to be removed either after a time indicated,
;;;             :   when the corresponding sound leaves the audicon if it is t,
;;;             :   or pushed out by a new stuffed chunk.
;;; 2015.06.02 Dan [3.3]
;;;             : * Report onset and offset times in milliseconds now to avoid
;;;             :   potential issues with float conversions and also allow the
;;;             :   new-*-sound functions to specify onset in ms by providing a
;;;             :   new optional parameter to each which if given as t means 
;;;             :   the onset time is ms instead of s.
;;; 2015.06.04 Dan
;;;             : * Convert all the parameters that are seconds to ms when they're 
;;;             :   set so it doesn't need to do so everywhere they are used.
;;;             : * Use the ms return from get-articulation-time now too.
;;;             : * All scheduled events specify time-in-ms.
;;; 2015.06.16 Dan
;;;             : * Fixed a bug in one of the calls to schedule-event-now that
;;;             :   was replacing a relative 0.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;             : * Changed default value of unstuff-aural-location to t.
;;; 2015.07.29 Dan [3.4]
;;;             : * Changed the way the check-unstuff-buffer action is scheduled
;;;             :   because that's now the precondition and unstuff-buffer is the
;;;             :   actual action.
;;;             : * Splitting the unstuff- parameter into two parameters:
;;;             :   :unstuff-aural-location can be nil, t, or # and indicates
;;;             :     whether to take a stuffed chunk out of the buffer and
;;;             :     defaults to t.
;;;             :   :overstuff-aural-location determines whether or not to 
;;;             :     stuff a new chunk overtop of a previously stuffed chunk
;;;             :     and defaults to nil.
;;;             : * When an unstuff is scheduled save it so that it can be
;;;             :   deleted if another gets scheduled.
;;; 2015.07.30 Dan
;;;             : * Fixed a bug with the unstuffing time because it was computed
;;;             :   as an absolute time but scheduled as relative when set to t.
;;; 2015.07.31 Dan
;;;             : * Changed the default values for unstuff and overstuff.  They
;;;             :   are now nil and t respectively because that seems to work
;;;             :   better for the way audio gets used which is not as frequent
;;;             :   as speech output which triggers audio percepts so don't want
;;;             :   a potentially unused system affecting performance.
;;; 2015.08.17 Dan
;;;             : * Changed the randomize-time calls which were used with times
;;;             :   in ms to randomize-time-ms.
;;; 2015.09.17 Dan [4.0]
;;;             : * Making the aural buffer trackable and having it complete
;;;             :   requests.
;;;             : * Adding the last-aural-request slot to audio-module to hold
;;;             :   the request for completion.
;;; 2015.09.22 Dan
;;;             : * Fixed some syntax issues with the previous changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GENERAL-PM" "ACT-R-support:general-pm")

(declaim (ftype (function (t &optional t) t) get-articulation-time))

(defclass audio-module (attn-module)
  ((audicon :accessor audicon :initarg :audicon :initform nil)
   (digit-detect-delay :accessor digit-detect-delay :initarg :digit-dtct-dly :initform 300)
   (digit-recode-delay :accessor digit-recode-delay :initarg :digit-rec-dly :initform 500)
   (digit-duration :accessor digit-duration :initarg :digit-duration :initform 600)
   (tone-detect-delay :accessor tone-detect-delay :initarg :tone-dtct-dly :initform 50)
   (tone-recode-delay :accessor tone-recode-delay :initarg :tone-rec-dly :initform 285)
   (sound-decay-time :accessor decay-time :initarg :decay-time :initform 3000)
   (default-spec :accessor default-spec :initform nil)
   (last-aural-request :accessor last-aural-request :initform nil))
   (:default-initargs
     :version-string "4.0"
     :name :AUDIO))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Sound events.
;;;; ---------------------------------------------------------------------- ;;;;

(defclass sound-event ()
  ((onset :accessor onset :initarg :onset :initform (mp-time-ms))
   (offset :accessor offset :initarg :offset :initform nil)
   (string :accessor snd-string :initarg :string :initform nil)
   (duration :accessor duration :initarg :duration :initform 0)
   (content :accessor content :initarg :content :initform nil)
   (content-delay :accessor delay :initarg :delay :initform nil)
   (kind :accessor kind :initarg :kind :initform 'SPEECH)
   (attended-p :accessor attended-p :initform nil :initarg :attended-p)
   (location :accessor location :initarg :location :initform 'EXTERNAL)
   (sname :accessor sname :initform (new-name-fct "SOUND") :initarg :sname)
   (ename :accessor ename :initform (new-name-fct "AUDIO-EVENT"))
   (recode :accessor recode :initarg :recode :initform nil)
   (pitch :accessor pitch :initform 'middle :initarg :pitch)
   (other-features :accessor other-features :initform nil :initarg :other-features)
   ))


(defmethod initialize-instance :after ((self sound-event) &key)
  (unless (offset self)
    (when (and (numberp (onset self)) (numberp (duration self)))
      (setf (offset self) (+ (onset self) (duration self)))))
  ;; Create the chunks up front
  (define-chunks-fct (list (list (sname self) 'isa 'sound)))
  (define-chunks-fct (list (list (ename self) 'isa 'audio-event))))

(defgeneric detect-at-time (evt)
  (:documentation "Returns the time at which an event becomes detectable."))

(defmethod detect-at-time ((evt sound-event))
  (+ (onset evt) (delay evt)))


(defgeneric detectable-p (evt)
  (:documentation  "Returns T if the given sound event is detectable."))

(defmethod detectable-p ((evt sound-event))
  (>= (mp-time-ms) (detect-at-time evt)))


(defgeneric finished-p (evt)
  (:documentation  "Returns T if the given sound-event is finished."))

(defmethod finished-p ((evt sound-event))
  (when (>= (mp-time-ms) (offset evt)) t))



(defclass digit-sound-evt (sound-event)
  ()
  (:default-initargs
    :kind 'DIGIT
    :duration  (randomize-time-ms (digit-duration (get-module :audio)))
    :delay (randomize-time-ms (digit-detect-delay (get-module :audio)))
    :recode (digit-recode-delay (get-module :audio))
    :sname (new-name-fct "DIGIT")))


(defmethod initialize-instance :after ((self digit-sound-evt) &key)
  (setf (content self) (snd-string self)))


;;; TONE-SOUND-EVENT      [Class]
;;; Date        : 97.04.03
;;; Description : Class for tone events.
;;;             : The CONTENT slot should be the tone frequency.

(defclass tone-sound-evt (sound-event)
  ()
  (:default-initargs
    :string ""
    :kind 'TONE
    :content 1000
    :delay (randomize-time-ms (tone-detect-delay (get-module :audio)))
    :recode (tone-recode-delay (get-module :audio))
    :sname (new-name-fct "TONE")))


(defmethod initialize-instance :after ((self tone-sound-evt) &key)
  (cond ((> (content self) 1500) (setf (pitch self) 'high))
        ((< (content self) 900) (setf (pitch self) 'low))))


(defclass word-sound-evt (sound-event)
  ()
  (:default-initargs
    :kind 'WORD
    :delay (randomize-time-ms (digit-detect-delay (get-module :audio)))
    :sname (new-name-fct "WORD")
    :duration 0
    :recode nil
    ))

(defmethod initialize-instance :after ((self word-sound-evt) &key)
  (when (or (null (duration self)) (zerop (duration self)))
    (setf (duration self)
          (get-articulation-time (snd-string self) t)))
  (unless (recode self)
    (setf (recode self)
          ;; change the value below to make "hearing" faster
      (max
       (/ (duration self) 2)
       (- (duration self) 150))))
  (setf (content self) (snd-string self))
  (setf (offset self) (+ (onset self) (duration self)))
  )


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Toplevel commands.
;;;; ---------------------------------------------------------------------- ;;;;


(defgeneric new-sound-event (evt)
  (:documentation "Handles the bookkeeping when a new sound event is created."))

;; handle the buffer stuffing

(defun stuff-sound-buffer (audio-mod) 
  (let ((chunk (buffer-read 'aural-location)))
    (when (or (null chunk)
              (and (overstuff-loc audio-mod)
                   ; This would seem ideal, but the old chunk may have
                   ; left the audicon already.  So just check that it's
                   ; unmodified and stuffed because that should be
                   ; sufficient.
                   ; (find (chunk-copied-from-fct chunk) (audicon audio-mod) :key 'ename) 
                   (multiple-value-bind (x unmodified-copy) (chunk-copied-from-fct chunk) (declare (ignore x)) unmodified-copy)
                   (query-buffer 'aural-location '(buffer unrequested))))
              (find-sound audio-mod (default-spec audio-mod) :stuffed t))))

(defmethod new-sound-event :around ((evt sound-event))
  (let ((evt (call-next-method)))
    (when evt
      (schedule-event (detect-at-time evt) 'stuff-sound-buffer :module :audio
                      :destination :audio :output nil :time-in-ms t)
      t)))

(defmethod new-sound-event ((evt sound-event))
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (aif (get-module :audio)
         (progn
           (push-last evt (audicon it))
           evt)
         (print-warning "No Audio module found.  Cannot create a new sound.")))))


(defun new-digit-sound (digit &optional onset time-in-ms)
  "Creates and adds a digit sound <digit>, a number, starting optionally at <onset>."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (numberp digit))
           (print-warning "Digit must be a number.  No new digit sound created."))
          ((not (or (null onset) (numberp onset)))
           (print-warning "Onset must be a number.  No new digit sound created."))
          (t
           (new-sound-event (make-instance 'digit-sound-evt 
                              :onset (cond ((null onset) (mp-time-ms))
                                           ((null time-in-ms) (safe-seconds->ms onset 'new-digit-sound))
                                           (t onset))
                              :string digit)))))))


(defun new-tone-sound (freq duration &optional onset time-in-ms)
  "Creates and adds a tone sound of <freq>, starting optionally at <onset>."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (numberp freq))
           (print-warning "Freq must be a number.  No new tone sound created."))
          ((not (numberp duration))
           (print-warning "Duration must be a number.  No new tone sound created."))
          ((not (or (null onset) (numberp onset)))
           (print-warning "Onset must be a number.  No new tone sound created."))
          (t
           (new-sound-event (make-instance 'tone-sound-evt 
                              :onset (cond ((null onset) (mp-time-ms))
                                           ((null time-in-ms) (safe-seconds->ms onset 'new-tone-sound))
                                           (t onset)) 
                              :duration (safe-seconds->ms duration 'new-tone-sound) 
                              :content freq)))))))

(defun new-other-sound (content duration delay recode 
                        &optional onset (location 'external) (kind 'speech) time-in-ms &rest other-features)
  "Creates and adds a sound <content>, lasting <duration>, with content delay <delay>, with recode time <recode>, starting optionally at <onset>."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (numberp duration))
           (print-warning "Duration must be a number.  No new sound created."))
          ((not (numberp delay))
           (print-warning "Delay must be a number.  No new sound created."))
          ((not (numberp recode))
           (print-warning "Recode must be a number.  No new sound created."))
          ((not (or (null onset) (numberp onset)))
           (print-warning "Onset must be a number.  No new sound created."))
          ((not (every (lambda (x) (and (= (length x) 3) (or (eq (car x) :evt) (eq (car x) :sound) (eq (car x) :both)))) other-features))
           (print-warning "Other features must be lists of length 3 starting with one of: :evt, :sound, or :both.  No new sound created."))
          (t
           (dolist (o other-features)
             (unless (valid-slot-name (second o))
               (extend-possible-slots (second o))))
           (new-sound-event (make-instance 'sound-event 
                              :onset (cond ((null onset) (mp-time-ms))
                                           ((null time-in-ms) (safe-seconds->ms onset 'new-other-sound))
                                           (t onset))
                              :duration (safe-seconds->ms duration 'new-other-sound) 
                              :content content 
                              :delay (safe-seconds->ms delay 'new-other-sound) 
                              :recode (safe-seconds->ms recode 'new-other-sound) 
                              :location location
                              :kind kind :other-features other-features)))))))

(defun new-word-sound (word &optional onset (location 'external) time-in-ms)
  "Creates and adds a word with optional onset time."
  (verify-current-mp 
   "No meta-process found.  Cannot create a new sound."
   (verify-current-model 
    "No current model found.  Cannot create a new sound."
    (cond ((not (stringp word))
           (print-warning "Word must be a string.  No new word sound created."))
          ((not (or (null onset) (numberp onset)))
           (print-warning "Onset must be a number.  No new word sound created."))
          (t
           (new-sound-event (make-instance 'word-sound-evt 
                              :onset (cond ((null onset) (mp-time-ms))
                                           ((null time-in-ms) (safe-seconds->ms onset 'new-word-sound))
                                           (t onset)) 
                              :string word :location location)))))))


(defun update-sound-evt (evt)
  (mod-chunk-fct (ename evt)
                 (append `(onset ,(onset evt)
                           location ,(location evt)
                           kind ,(kind evt)
                           id ,(ename evt)
                           pitch ,(pitch evt))
                         (when (finished-p evt)
                           `(offset ,(offset evt)
                             duration ,(ms->seconds (- (offset evt) (onset evt)))))
                         (mapcan (lambda (x)
                                   (when (or (eq (car x) :evt) (eq (car x) :both))
                                     (copy-list (cdr x))))
                           (other-features evt)))))
                                             
                                             
;;; FIND-SOUND      [Method]
;;; Date        : 97.08.18, delta 99.08.30
;;; Description : Parallels the Vision Module's FIND-LOCATION, this one finds
;;;             : audio events (not sounds).

(defgeneric find-sound (aud-mod spec &key stuffed)
  (:documentation  "Given a chunk-spec return a sound event which matches."))

(defmethod find-sound ((aud-mod audio-module) spec &key (stuffed nil))
  (let* ((slots (chunk-spec-slot-spec spec))
         (finished-spec (find :finished slots :key 'spec-slot-name))
         (attended-spec (find :attended slots :key 'spec-slot-name))
         (relative-slots nil)
         (event-ls (detectable-audicon aud-mod)))
    
    ;; filter by attended - spec-slot-op must be =
    (when attended-spec
      (setf event-ls (remove-if-not (lambda (x) 
                                      (eq (attended-p x) (spec-slot-value attended-spec)))
                                    event-ls)))
    ;; filter by finished - spec-slot-op must be =
    (when finished-spec
      (setf event-ls (remove-if-not (lambda (x) 
                                      (eq (finished-p x) (spec-slot-value finished-spec)))
                                    event-ls)))
    
    (dolist (x slots)
      (when (or (eq (spec-slot-value x) 'lowest)
                (eq (spec-slot-value x) 'highest))
        (setf slots (remove x slots))
        (push-last x relative-slots)))
    
    (let ((matching-chunks (find-matching-chunks (slot-specs-to-chunk-spec slots)
                                                 :chunks (mapcar 'update-sound-evt event-ls)
                                                 :variable-char #\&)))
      
      (dolist (x relative-slots)
        (let ((value nil)
              (op (spec-slot-op x))
              (slot (spec-slot-name x))
              (test (spec-slot-value x)))
          
          (dolist (y matching-chunks)
            (let ((cur-val (fast-chunk-slot-value-fct y slot)))
              (unless (numberp cur-val)
                (setf value :fail)
                (print-warning "Cannot apply ~S constraint because not all chunks have a numerical value." x)
                (return))
              (when (or (null value) 
                        (and (eq test 'lowest)
                             (< cur-val value))
                        (and (eq test 'highest)
                             (> cur-val value)))
                (setf value cur-val))))
          
          (unless (eq value :fail)
            (setf matching-chunks (find-matching-chunks (define-chunk-spec-fct (list op slot value)) :chunks matching-chunks)))))
      
      (setf (loc-failure aud-mod) nil)
      
      (if matching-chunks
          (let ((chunk (random-item matching-chunks)))
            (if (and stuffed (buffer-read 'aural-location))
                (schedule-overwrite-buffer-chunk 'aural-location chunk 0 :time-in-ms t :module :audio :requested nil :priority 10)
              (schedule-set-buffer-chunk 'aural-location chunk 0 :time-in-ms t :module :audio :requested (not stuffed) :priority 10))
            
            (when (and stuffed (unstuff-loc aud-mod))
              (let ((event (find chunk event-ls :key 'ename)))
                
                (awhen (unstuff-event aud-mod)
                       (delete-event it))
                
                (setf (unstuff-event aud-mod) 
                  (if (numberp (unstuff-loc aud-mod)) 
                      (schedule-event-relative (unstuff-loc aud-mod) 'unstuff-buffer :maintenance t :params (list 'aural-location chunk) 
                                               :destination :audio :output nil :time-in-ms t :priority :min
                                               :precondition 'check-unstuff-buffer)
                    (schedule-event (+ (offset event) (decay-time aud-mod)) 'unstuff-buffer :maintenance t :params (list 'aural-location chunk) 
                                    :destination :audio :output nil :time-in-ms t :priority :min
                                    :precondition 'check-unstuff-buffer))))))
        (schedule-event-now 'find-sound-failure :params (list stuffed) :module :audio :destination :audio :output 'medium :details "find-sound-failure")))))

  
(defun audio-event-ended (evt)
  (let ((buffer-chunk (buffer-read 'aural-location)))
    (when (and buffer-chunk (eq (chunk-slot-value-fct buffer-chunk 'id) (ename evt)))
      (set-chunk-slot-value-fct buffer-chunk 'offset (offset evt))
      (set-chunk-slot-value-fct buffer-chunk 'duration (ms->seconds (- (offset evt) (onset evt)))))))

(defun find-sound-failure (audio stuffed)
  "function to indicate a failure in the trace and set the error flag"
  (setf (loc-failure audio) t)
  (set-buffer-failure 'aural-location :ignore-if-full t :requested (not stuffed))
  nil)


;;; ATTEND-SOUND      [Method]
;;; Date        : 97.08.18
;;; Description : Parallels the Vision Module's MOVE-ATTENTION, this one
;;;             : attends an audio event, ultimately building a chunk based
;;;             : on the content of the sound.

(defgeneric attend-sound (aud-mod &key event)
  (:documentation  "Shift auditory attention to the given sound event."))

(defmethod attend-sound ((aud-mod audio-module) &key event)
  (cond ((eq (exec-s aud-mod) 'BUSY)
         (model-warning "Auditory attention shift requested at ~S while one was already in progress." (mp-time))
         (complete-request (last-aural-request aud-mod)))
         
         
        (t              ; Keep using an id slot in the audio-event to keep the connection
         (let ((s-event (find (chunk-slot-value-fct event 'id) (current-audicon aud-mod)
                              :test (lambda (x y) (eq x (ename y))))))
           
           (setf (attend-failure aud-mod) nil)
           (change-state aud-mod :exec 'busy)
           
           (if s-event ;; add in a test to make sure - could have a failure
               (progn
                 (setf (attended-p s-event) t)
                 (setf (current-marker aud-mod) s-event)
                 
                 ;; when attending to the event schedule an event to fill in the offset and
                 ;; duration for when it ends if they aren't already filled
                 
                 (unless (and (chunk-slot-value-fct (ename s-event) 'offset) (chunk-slot-value-fct (ename s-event) 'duration))
                   (schedule-event (offset s-event) 'audio-event-ended :module :audio :priority 9 :output 'high :params (list s-event)
                                   :time-in-ms t :details (format nil "~s ~s" 'audio-event-ended (ename s-event))))
                 
                 (schedule-event-relative (randomize-time-ms (recode s-event))
                                          'audio-encoding-complete
                                          :module :audio
                                          :destination :audio
                                          :params (list s-event)
                                          :time-in-ms t))
             ;; assume digit delay for a failure
             (schedule-event-relative (randomize-time-ms (digit-recode-delay aud-mod))
                                      'attend-sound-failure
                                      :time-in-ms t 
                                      :module :audio
                                      :destination :audio
                                      :output 'medium
                                      :details "attend-sound-failure"))))))
  
  
(defun attend-sound-failure (audio)
  "Flag that an error occured"
  (setf (attend-failure audio) t)
  (set-buffer-failure 'aural)
  (change-state audio :exec 'free)
  (complete-request (last-aural-request audio)))


(defgeneric audio-encoding-complete (aud-mod sevt)
  (:documentation  "Put the sound into the aural buffer."))

(defmethod audio-encoding-complete ((aud-mod audio-module) (sevt sound-event))
  (change-state aud-mod :exec 'free)
  (setf (attended-p sevt) t)
  
  (complete-request (last-aural-request aud-mod))
  (mod-chunk-fct (sname sevt)
                 (append `(content ,(content sevt)
                                   kind ,(kind sevt)
                                   event ,(ename sevt)) ;; use the ename since it's constant but won't match the "chunk" in the aural-loc buffer
                         (mapcan (lambda (x)
                                   (when (or (eq (car x) :sound) (eq (car x) :both))
                                     (copy-list (cdr x))))
                           (other-features sevt))))
  
  (schedule-set-buffer-chunk 'aural (sname sevt) 0 :time-in-ms t :module :audio)
    
  (set-attended aud-mod (sname sevt)))

(defmethod clear :after ((aud-mod audio-module))
  (setf (loc-failure aud-mod) nil)
  (setf (attend-failure aud-mod) nil))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Audio utilities
;;;; ---------------------------------------------------------------------- ;;;;


(defgeneric purge-old-sounds (aud-mod)
  (:documentation  "Removes sounds that have decayed from the audicon but can't delete corresponding event chunks"))

(defmethod purge-old-sounds ((aud-mod audio-module))
  (let ((new-audicon nil))
    (setf (audicon aud-mod)
      (dolist (e (audicon aud-mod) new-audicon)
        (if (< (+ (offset e) (decay-time aud-mod)) (mp-time-ms))
            (progn
              ;(when (chunk-p-fct (ename e))
              ;  (delete-chunk-fct (ename e)))
              (when (chunk-p-fct (sname e))
                (delete-chunk-fct (sname e))))
          (push-last e new-audicon))))))

(defgeneric current-audicon (aud-mod)
  (:documentation  "Returns the audicon, assuming all events that currently exist are in there."))

(defmethod current-audicon ((aud-mod audio-module))
  (purge-old-sounds aud-mod)
  (remove-if (lambda (x) (> (onset x) (mp-time-ms))) (audicon aud-mod)))


(defgeneric detectable-audicon (aud-mod)
  (:documentation  "Returns the audicon, but only those events that are currently detectable."))

(defmethod detectable-audicon ((aud-mod audio-module))
  (purge-old-sounds aud-mod)
  (remove-if (lambda (x) (not (detectable-p x))) (audicon aud-mod)))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; ACT6 integration stuff
;;;; ---------------------------------------------------------------------- ;;;;

(defun reset-audio-module (instance)
  
  (reset-pm-module instance)

  (chunk-type audio-event onset offset duration pitch kind location id)
  (chunk-type (set-audloc-default (:include audio-event)) (set-audloc-default t))
  (chunk-type sound kind content event)
  
  (setf (audicon instance) nil)
  (setf (loc-failure instance) nil)
  (setf (attend-failure instance) nil)
  (setf (last-aural-request instance) nil)
  
  (dolist (c '(digit speech tone word highest lowest sound find-sound set-audloc-default internal external self high middle low))
    (unless (chunk-p-fct c)
      (define-chunks-fct (list (list c 'name c)))
      (make-chunk-immutable c)))
  
  (setf (default-spec instance)
    (define-chunk-spec :attended nil)))


(defun query-audio-module (aud-mod buffer slot value)
  (case buffer
    (aural
     (if (member slot '(preparation execution processor modality))
         (generic-state-query aud-mod buffer slot value)
       (case slot
         (state
          (case value
            (busy
             (eq (mode-s aud-mod) 'busy))
            (free
             (eq (mode-s aud-mod) 'free))
            (error
                (attend-failure aud-mod))
            (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
         (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value)))))
    (aural-location
     (case slot
       (state
        (case value
          (busy nil) ;; aural-location requests are always free
          (free t)
          (error (loc-failure aud-mod))
          (t (model-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
       (attended
        (awhen (buffer-read 'aural-location)
               (let ((s-event (find (chunk-slot-value-fct it 'id) 
                                    (audicon aud-mod)
                                    :test (lambda (x y) (eq x (ename y))))))
                 (when s-event
                   (eq value (attended-p s-event))))))
       (finished
        (awhen (buffer-read 'aural-location)
               (let ((s-event (find (chunk-slot-value-fct it 'id) 
                                    (audicon aud-mod)
                                    :test (lambda (x y) (eq x (ename y))))))
                 (when s-event
                   (eq value (finished-p s-event))))))))))
                 

(defmethod pm-module-last-cmd-name ((aud-mod audio-module) buffer-name chunk-spec)
  (case buffer-name
    (aural-location (if (chunk-spec-slot-spec chunk-spec 'set-audloc-default) 
                        'set-audloc-default
                      'find-sound))
    (aural 'sound)))

(defmethod pm-module-request ((aud-mod audio-module) buffer-name chunk-spec)
  
  (case buffer-name
    (aural
     (if (test-for-clear-request chunk-spec)
         (schedule-event-now 'clear :module :audio :destination :audio :output 'medium)
       (let ((event-spec (chunk-spec-slot-spec chunk-spec 'event)))
         (cond ((and (= (length event-spec) 1)
                     (eq '= (spec-slot-op (first event-spec)))
                     (chunk-p-fct (spec-slot-value (first event-spec))))
                
                (setf (last-aural-request aud-mod) chunk-spec)

                (schedule-event-now 'attend-sound
                                 :params (list :event (spec-slot-value (first event-spec)))
                                 :module :audio  :destination :audio
                                 :details (mkstr 'attend-sound " " (spec-slot-value (first event-spec)))
                                 :output 'medium))
               (t
                (complete-request chunk-spec)
                (print-warning "Invalid command to the aural buffer does not describe a valid action"))))))
    (aural-location
     (cond ((> (length (chunk-spec-slot-spec chunk-spec :attended)) 1)
            (print-warning ":attended specification only allowed once in an aural-location request."))
           ((> (length (chunk-spec-slot-spec chunk-spec :finished)) 1)
            (print-warning ":finished specification only allowed once in an aural-location request."))
           ((chunk-spec-slot-spec chunk-spec 'set-audloc-default)
            (let* ((specs (chunk-spec-slot-spec chunk-spec 'set-audloc-default))
                   (spec (first specs)))
              (cond ((> (length specs) 1)
                     (print-warning "set-audloc-default slot can only be specified once in an aural-location request."))
                    ((not (eq '= (spec-slot-op spec)))
                     (print-warning "set-audloc-default slot must use the = test in an aural-location request."))
                    (t
                     (schedule-event-now 'set-audloc-default-fct 
                                         :params (list (apply 'append (remove-if (lambda (x) (eq (spec-slot-name x) 'set-audloc-default))
                                                                                 (chunk-spec-slot-spec chunk-spec))))
                                         :output 'medium :module :audio :details (mkstr 'set-audloc-default))))))
           (t
            (schedule-event-now 'find-sound :module :audio 
                                     :output 'medium
                                     :destination :audio 
                                     :details (mkstr 'find-sound)
                                     :params (list chunk-spec)))))))



(defun params-audio-module (aud-mod param)
  (if (consp param)
    (case (first param)
      (:digit-detect-delay
       (setf (digit-detect-delay aud-mod) (safe-seconds->ms (rest param) 'sgp))
       (rest param))
      (:digit-duration
       (setf (digit-duration aud-mod) (safe-seconds->ms (rest param) 'sgp))
       (rest param))
      (:digit-recode-delay
       (setf (digit-recode-delay aud-mod) (safe-seconds->ms (rest param) 'sgp))
       (rest param))
      (:sound-decay-time 
       (setf (decay-time aud-mod) (safe-seconds->ms (rest param) 'sgp))
       (rest param))
      (:tone-detect-delay
       (setf (tone-detect-delay aud-mod) (safe-seconds->ms (rest param) 'sgp))
       (rest param))
      (:tone-recode-delay
       (setf (tone-recode-delay aud-mod) (safe-seconds->ms (rest param) 'sgp))
       (rest param))
      (:unstuff-aural-location
       (setf (unstuff-loc aud-mod)
         (if (numberp (cdr param))
             (safe-seconds->ms (cdr param))
           (cdr param)))
       (cdr param))
      (:overstuff-aural-location
       (setf (overstuff-loc aud-mod) (cdr param))))
    (case param
      (:digit-detect-delay
       (ms->seconds (digit-detect-delay aud-mod)))
      (:digit-duration
       (ms->seconds (digit-duration aud-mod)))
      (:digit-recode-delay
       (ms->seconds (digit-recode-delay aud-mod)))
      (:sound-decay-time 
       (ms->seconds (decay-time aud-mod)))
      (:tone-detect-delay
       (ms->seconds (tone-detect-delay aud-mod)))
      (:tone-recode-delay
       (ms->seconds (tone-recode-delay aud-mod)))
      (:unstuff-aural-location
       (if (numberp (unstuff-loc aud-mod))
           (ms->seconds (unstuff-loc aud-mod))
         (unstuff-loc aud-mod)))
      (:overstuff-aural-location
       (overstuff-loc aud-mod)))))
      

(define-module-fct :audio 
    (list (define-buffer-fct 'aural-location 
            :request-params '(:attended :finished) 
            :queries '(attended finished)
            :status-fn (lambda ()
                         (command-output "  attended nil          : ~S" (query-buffer 'aural-location '(attended  nil)))
                         (command-output "  attended t            : ~S" (query-buffer 'aural-location '(attended  t)))
                         (command-output "  finished nil          : ~S" (query-buffer 'aural-location '(finished  nil)))
                         (command-output "  finished t            : ~S" (query-buffer 'aural-location '(finished  t)))))
          (define-buffer-fct 'aural 
            :queries '(modality preparation execution processor) 
            :status-fn (lambda () (print-module-status (get-module :audio)))
            :trackable t))
  (list 
   (define-parameter :digit-detect-delay
     :valid-test 'nonneg
     :default-value 0.3
     :warning "a non-negative number"
     :documentation "Lag between onset and detectability for digits")
   (define-parameter :digit-duration
     :valid-test 'nonneg
     :default-value 0.6
     :warning "a non-negative number"
     :documentation "Default duration for digit sounds.")
   (define-parameter :digit-recode-delay
     :valid-test 'nonneg
     :default-value 0.5
     :warning "a non-negative number"
     :documentation "Recoding delay for digit sound content.")
   (define-parameter :sound-decay-time
     :valid-test 'nonneg
     :default-value 3.0
     :warning "a non-negative number"
     :documentation "The amount of time after a sound has finished it takes for the sound to be deleted from the audicon")
   (define-parameter :tone-detect-delay
     :valid-test 'nonneg
     :default-value 0.05
     :warning "a non-negative number"
     :documentation "Lag between sound onset and detectability for tones")
   (define-parameter :tone-recode-delay
     :valid-test 'nonneg
     :default-value 0.285
     :warning "a non-negative number"
     :documentation "Recoding delay for tone sound content.")
   (define-parameter :unstuff-aural-location
     :valid-test (lambda (x) (or (tornil x) (numberp x)))
     :default-value nil
     :warning "T, NIL, or a number"
     :documentation "Whether chunks stuffed into the aural-location buffer should be automatically cleared by the module if unused")
   (define-parameter :overstuff-aural-location
     :valid-test 'tornil
     :default-value t
     :warning "T or nil"
     :documentation "Whether a chunk previously stuffed into the aural-location buffer can be overwritten by a new chunk to be stuffed"))
  :version "4.0"
  :documentation "A module which gives the model an auditory attentional system"
  :creation (lambda (x) (declare (ignore x))
              (make-instance 'audio-module))
  :reset #'reset-audio-module
  :query #'query-audio-module
  :request #'pm-module-request
  :params #'params-audio-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-audicon ()
  (let ((module (get-module :audio)))
    (if module
        (progn
          (command-output "Sound event    Att  Detectable  Kind           Content           location     onset     offset  Sound ID")
          (command-output "-----------    ---  ----------  -------------  ----------------  --------     -----     ------  --------")
          
          (dolist (x (sort (copy-list (current-audicon module)) #'< :key 'onset))
            (print-audio-feature x))) 
      (print-warning "No audio module found"))))

(defgeneric print-audio-feature (feat)
  (:documentation  "Print out an ASCII representation of the audicon."))

(defmethod print-audio-feature ((feat sound-event))
  (command-output "~15a~5A~12A~15A~18s~10a~8d   ~8d  ~a"
                  (ename feat)
                  (attended-p feat)
                  (detectable-p feat)
                  (kind feat)
                  (content feat)
                  (location feat)
                  (onset feat)
                  (offset feat)
                  (sname feat)))

(defmacro set-audloc-default (&rest params)
  "Macro to set the default specification for aural location buffer stuffing."
  `(set-audloc-default-fct ',params))

(defun set-audloc-default-fct (params)
  "Function to set the default specification for aural location buffer stuffing."
  (verify-current-mp
   "No current meta-process.  Cannot set audloc defaults."
   (verify-current-model 
    "No current model.  Cannot set audloc defaults."
    (aif (get-module :audio)
         (let* ((spec (funcall 'define-chunk-spec-fct params))
                (attended (when spec (chunk-spec-slot-spec spec :attended)))
                (finished (when spec (chunk-spec-slot-spec spec :finished))))
           (if (or (> (length attended) 1)
                   (> (length finished) 1))
               (progn
                 (print-warning "The :attended and :finished specification for set-audloc-default can only be specified at most once.")
                 (print-warning "Audloc defaults not changed."))
             (if spec
                 (progn (setf (default-spec it) spec) t)
               (print-warning "Invalid chunk specification.  Default audloc not changed."))))
         (print-warning "No audio module found.  Cannot set audloc defaults.")))))


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
