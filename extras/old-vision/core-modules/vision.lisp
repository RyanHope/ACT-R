;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
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
;;; Filename    : vision.lisp
;;; Version     : 2.4 
;;; 
;;; Description : Source code for the ACT-R/PM Vision Module.  
;;;
;;; Bugs        : [] Need to split object identification and chunk 
;;;             :    construction in GET-OBJECT-AT because of EMMA.
;;; 
;;; Todo        : [] Ordering of screen-x and screen-y tests.
;;;             : [] General feature synthesis.
;;;             : [] Finst marking.
;;;             : [] Tracking setting visual-location buffer.
;;;
;;;             : [] (DAN) Fix issue with encoding-complete trying to recreate
;;;                  an existing chunk (other than the current make-dme hack)
;;;             : Discuss a potential issue:
;;;                 Should encoding-complete clear the visual buffer
;;;                 when it fails to find something in a re-encoding?
;;;                 The old system did, but for now this one doesn't
;;;                 it sets the error state of the visual buffer instead.
;;;                 It probably won't have to with strict harvesting
;;;                 happening anyway, but it does differ some from ACT-R 5
;;;                 without it.  I'm just not sure which way is better...
;;; 
;;; ----- History ----- [look also at function comments]
;;;
;;; 2004.10.20 Dan [First pass an moving to ACT-R 6]
;;;             : Changed name to vision and reset version to 1.0a1
;;;             :
;;;             : Depends on changes in the general-pm file
;;;             :
;;;             : Trying to do the minimal change necessary so
;;;             ;   - not touching the internals any more than necessary
;;;             :   - not making attended a request parameter instead of a real
;;;             :     chunk slot
;;;             :
;;;             : Moved process-display and update-cursor-feat
;;;             :    here from device-interface since they are methods on the
;;;             :    vision module
;;;             :
;;;             : Changed the initialize-instance method on the vision-module
;;;             :    to eliminate the state chunk.
;;;             :
;;;             : Added the loc-failure, attend-failure, and stuffed slots to 
;;;             :    the vision-module class to record those situations for
;;;             :    responding to queries
;;;             :
;;;             : Removed these functions/methods
;;;             :    silent-events
;;;             : 
;;;             : Moved pm-print-icon here
;;;             :
;;;             : A "crazy" idea that I've had is that the vision system 
;;;             : could keep the features and objects around internally as 
;;;             : chunks now since they wouldn't be in conflict with the chunks 
;;;             : in DM and then use the provided "find chunk from chunk-spec"
;;;             : functions in find-location and maybe move-attention.  I havn't 
;;;             : worked all of that through, but I think it could be doable and 
;;;             : it would have a benefit of allowing more flexible requests for
;;;             : visual-locations (allowing things like the - operator and not 
;;;             : requiring a mapping to the within call in the pre-processing).
;;;             : It may not be feasible, but it's just a though...
;;; 04.12.17 Dan
;;;             : Some clean-up based on things I saw when running real models
;;;             :
;;;             : Fixed the visual-location buffer check in encoding-complete
;;;             :   because buffer-chunk returns the list of chunks
;;;             :
;;;             : Similar fix for visual buffer in update-tracking-mth
;;;             :
;;;             : Added ":module :vision" to the schedule-mod-buffer-chunk calls
;;;             :   in encoding-complete and update-tracking-mth
;;;             :
;;;             : Suppress the "vision sees *" and "vision found *" outputs
;;;             :   by making the default for the print-viewed slot nil 
;;;             :   since the buffer setting trace makes it redundent
;;;             :
;;;             : For now, at least, creating the chunk T in the vision 
;;;             :   reset function.  I need to fix this in the chunk checking
;;;             :   code, but this is the quick and dirty fix for now.
;;;             :
;;;             : Added a :display parameter to the scheduling of 
;;;             :   move-attention so it looks nicer
;;;             :   
;;;             : Changed the queue-command calls to schedule-event directly
;;;             :   so that the :display could be used for the encoding-
;;;             :   complete notices as well.
;;;             :
;;;             : There's an odd issue that seems to be an interaction
;;;             :   of a few things.  It manifests itself as a problem when
;;;             :   an encoding-complete attempts to define a chunk that is
;;;             :   already defined.
;;;             :   I think it happens when a re-encoding creates the initial
;;;             :   chunk and then a later move-attention goes there.  The
;;;             :   problem I think is that because the buffer creates new
;;;             :   chunks the visual-location mapping doesn't match so it
;;;             :   ends up trying to build a new one.
;;;             :   For now, I've changed the make-dme function so that it
;;;             :   doesn't "recreate" chunks and instead modifies them.
;;;             :   I think that's safe, since the buffer always gets a copy
;;;             :   modifying the old one won't affect the chunk in DM, but
;;;             :   there may be a better way to handle that.
;;;             : I'm also not sure if that works "right" for a moving object
;;;             :   because I haven't tested tracking yet to see if the buffer
;;;             :   gets updated or if it's only the original that changes.
;;; 2004.12.23 Dan
;;;             : moved verify-single-explicit-value to misc-utils
;;; 2005.01.07 mdb
;;;             : * Some reformatting to 80 columns, fixed some typos.
;;;             : * Moved some attention management slots to the 
;;;             :   ATTN-MODULE class because they apply to audio as well.
;;; 2005.01.11 mdb
;;;             : * Changed to PM-MODULE-REQUEST to handle LAST-COMMAND.
;;;             : * Added some toplevel commands.
;;;             : * Added doc strings for parameters.
;;; 2005.01.12 Dan
;;;             : * Moved the device parameters to the device module.
;;;             : * Added old-time and new-time parameters to the update-module
;;;             :   method (breaks backward compatibiliity but I think it's
;;;             :   mostly an internal method anyway).
;;;             : * Took the setting of default-spec out of the initialize-
;;;             :   instance method for the vision module because that relied
;;;             :   on the device interface being created which may or may not
;;;             :   have happened yet.  Shouldn't cause a problem however since
;;;             :   the reset-pm-module method sets it again and that will
;;;             :   always be called on initial creation as well, but after
;;;             :   all modules have been created.
;;; 2005.01.13 Dan
;;;             : * Added the test for a locked device into process-display
;;;             :   to replace the old "procedural busy" check.
;;; 2005.01.14 Dan
;;;             : * Added line as a chunk created by vision.
;;;             : * Added chunks for the colors in color-name->color-symbol.
;;; 2005.01.15 Dan
;;;             : * Changed the request function so that screen-pos for 
;;;             :   move-attention requests gets changed to the the chunk
;;;             :   it was copied from so that it maps back to the spec
;;;             :   properly.  There's probably a better way to deal with this
;;;             :   but this works for now...
;;; 2005.01.16 Dan
;;;             : * Above change broke the mapping between the chunk in the
;;;             :   visual-location buffer and the visual-object that gets
;;;             :   created so now I've re-fixed that now which requires NOT
;;;             :   modifying the attended slot of the chunk in the visual-
;;;             :   location buffer.
;;; 2005.01.17 Dan
;;;             : * Adding the scale slot to the visual-object chunk-type
;;;             :   so it can be used in +visual requests.  Probably want to
;;;             :   use a better solution long term.  Two possibilities are
;;;             :   making scale a buffer parameter which doesn't feel right
;;;             :   since it has no meaning in other +visual requests or to
;;;             :   add an new chunk-type move-attention and replace "+visual>
;;;             :   isa visual-object" with "+visual> isa move-attention".
;;;             :   While the second breaks backward compatibility I think
;;;             :   long term it fits better with the buffer command usage.
;;; 2005.01.18 Dan
;;;             : * Switched from using buffer-chunk which is really the 
;;;             :   user level printing function to buffer-read.
;;; 2005.01.20 Dan
;;;             : * Added chunk-types to the module's reset function for 
;;;             :   assign-finst, start-tracking, and clear.
;;;             : * Changed pm-attend-location to expand into 
;;;             :   attend-visual-coordinates-fct instead of 
;;;             :   attend-visual-location-fct.
;;; 2005.01.21 Dan
;;;             : * Wrapped the proclaim in an eval-when because otherwise
;;;             :   it may not actually affect the compilation.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium"  or ":output 'low" to some of the 
;;;             :   events scheduled to play friendly with the new detail level.
;;;             : * Changed the event details that used format to ones that
;;;             :   use concatenate.  It doesn't allow for the user's print
;;;             :   settings to control things now, but it is a LOT faster
;;;             :   which has an impact even with the trace off...
;;; 2005.03.25 Dan
;;;             : * Modified the visual-location chunk-type to remove attended
;;;             :   as an actual slot and instead moved it to be a request
;;;             :   parameter.
;;; 2005.03.31 Dan [2.3a2]
;;;             : * Changed the move attention request command from isa 
;;;             :   visual-object to isa move-attention.
;;;             : * Took the scale slot out of visual-objects and added the
;;;             :   move-attention chunk-type.
;;;             : * Added an attended query to visual-location which can be 
;;;             :   used to check what would be the attended slot of the chunk
;;;             :   in the visual-location buffer.
;;; 2005.04.06 Dan
;;;             : * Changed parse-xy-from-spec so that it allows 
;;;             :   {<,>} screen-* current to map to less-than-current or 
;;;             :   greater-than-current.  Which makes those special values
;;;             :   unnecessary in productions.
;;; 2005.04.22 Dan
;;;             : * Added clearing of the error flags to the clear method
;;;             :   because that seems like something which should happen.
;;; 2005.04.23 Dan
;;;             : * Stuffed slot no longer used.
;;;             : * The set-buffer-chunk calls now specify whether the chunk
;;;             :   was requested or not and it's maintained within the
;;;             :   buffer itself - stuff-visloc-buffer and update-tracking-mth
;;;             :   both indicate a non-requested chunk.
;;;             : * The query function doesn't need to respond to stuffed.
;;;             : * The buffer definition now includes the status printing.
;;; 2005.05.11 Dan
;;;             : * Modified the scheduling of find-location so that it
;;;             :   shows up in the medium trace.
;;; 2005.07.11 Dan
;;;             : * Fixed a typo in the visual-location parsing code that was
;;;             :   for the warning text.
;;;             : * Modified the visual-location request so that distance can
;;;             :   take the conditional modifiers like screen-x and screen-y.
;;; 2005.07.22 mdb
;;;             : * Worked in support for USERPROP slots in vislocs.
;;;             : * All slots in vislocs now handle either numeric or symbolic
;;;             :   tests, including negation and inequality.
;;; 2005.08.03 Dan
;;;             : * Added a find-loc-failure event to the trace when find-
;;;             :   location fails to find a location.
;;; 2005.08.10 Dan
;;;             : * Declared the old-time and new-time parameters in update-
;;;             :   module as ignored.
;;;             : * Commented out a duplicate get-phrase-at method.
;;; 2005.09.14 Dan
;;;             : * Fixed a bug in loc-to-feat that prevented it from finding
;;;             :   the appropriate feature when a copy of a visual-location
;;;             :   chunk (i.e. the one in the buffer) was passed in.  (At
;;;             :   least I think this fixes the issue...)
;;; 2005.09.19 mdb
;;;             : * Fixed bug in COPY-INSTANCE which only showed up on non-MCL
;;;             :   Lisps.  Oops.
;;; 2005.09.22 Dan
;;;             : * Adjusted slotval-match? to do an additional check for
;;;             :   numberp on the value because something like size defaults
;;;             :   to nil, but a request for a number resulted in problems.
;;;             :   Maybe the default needs to be changed, but I think this
;;;             :   could be a more general issue that had to be fixed anyway.
;;; 2005.09.23 Dan
;;;             : * More updates to slotval-match?.  Added a test for strings
;;;             :   and use string-equal (case insensitive) in that case and
;;;             :   changed the default return (when none of the cases apply)
;;;             :   to nil because that seems to make more sense than letting
;;;             :   any invalid match attempts succeed.
;;; 2005.10.27 Dan
;;;             : * Quick change to start-tracking so that it doesn't throw
;;;             :   an error on a mismatch and so that it converts the "buffer
;;;             :   chunk" to the name that vision wants.  Still has problems,
;;;             :   but this is a temporary fix - why does it need an object
;;;             :   anyway?
;;; 2005.10.28 Dan
;;;             : * Start of a serious update of tracking for 6.  The new
;;;             :   mechanism removes the object in the request to start-
;;;             :   tracking, requires that the user signal the tracked object
;;;             :   has moved by calling either proc-display or update-tracking,
;;;             :   and either continually modifies the chunks in both vision
;;;             :   buffers or stuffs the newly modified chunks in there if 
;;;             :   either is empty (will not overwrite a chunk in either
;;;             :   buffer that isn't related to the tracked item).
;;;             : * Specifically:
;;;             :   - Renamed pm-update-tracking to update-tracking and removed
;;;             :     the required parameter.
;;;             :   - Took the call to update-tracking-mth out of the update-
;;;             :     module call (mainly because update-module is going away
;;;             :     soon too since it's got a nasty performance cost).
;;;             :   - Changed the chunk-type of start-tracking to remove the
;;;             :     object slot.
;;;             :   - Modified the visual request function to handle that.
;;;             :   - Process-display now calls update-tracking-mth.
;;; 2005.10.31 Dan
;;;             : * Start of the rewrite of update-tracking-mth to handle the 
;;;             :   new buffer coordination associated with tracking.
;;; 2005.11.07 Dan [2.3a4]
;;;             : * Finished the updates to tracking.
;;;             :   Could probably use more thorough testing, but it seems to
;;;             :   work now.
;;; 2006.03.01 Dan
;;;             : * Modified tracking to try to handle the case where an object
;;;             :   just isn't visible any more.  The assumption is that if its
;;;             :   build-features-for returns nil, then it's gone.
;;;             :   For now, do the same thing that an encoding failure does -
;;;             :   clear the visual buffer and set the visual error state.
;;; 2006.03.03 Dan
;;;             : * Minor fix for update-tracking-chunks to help with the case
;;;             :   where something is no longer available. 
;;; 2006.03.18 Dan
;;;             : * OK, now tracking should work much better - the last cut at
;;;             :   it worked for the simple usage, but really failed under 
;;;             :   some circumstances...but there's still something I can't
;;;             :   figure out so it's pretty inefficient at this point because
;;;             :   it always recreates things whether there's a change or not
;;;             :   because I couldn't get to the bottom of what would cause
;;;             :   an old feature to be changed between calls to update-tracking-mth.
;;;             :   I've left lots of ugly debugging output code there (commented
;;;             :   out) for now.
;;; 2006.03.23 Dan
;;;             :  * The query-vision-module function had a problem with the
;;;             :    check for attended when tracking was on.  I think that
;;;             :    the issue might be that tracking isn't properly maintaining
;;;             :    things, but I've changed query-vision-module anyway to
;;;             :    at least not throw the error for now.
;;; 2006.04.19 Dan
;;;             : * Modified find-current-locs-with-spec and remap-spec-to-current
;;;             :   so that distance will accept lowest and highest as options.
;;;             :
;;; 2006.07.16 Dan
;;;             : * Made a change to query-vision-module to avoid a warning from
;;;             :   the updated chunk accessors.
;;; 2006.09.07 Dan
;;;             : * Moved fill-default-dimensions here from the device files
;;;             :   because it can used for all icon-features and wasn't defined
;;;             :   for virtuals (only in the acl and mcl specific devices).
;;; 2006.09.08 Dan
;;;             : * Changed some parameter tests from posnum to nonneg.
;;; 2006.11.20 Dan
;;;             : * Added a module warning function to the module definition so
;;;             :   that the module can take direct responsibility for locking/
;;;             :   unlocking proc-display during a production which makes a
;;;             :   visual request (also required adding a new slot to the 
;;;             :   module and modifying the request function).
;;; 2006.12.18 Dan
;;;             : * Added the assign-finst, start-tracking and clear chunks
;;;             :   to the reset function.
;;; 2006.12.28 Dan
;;;             : * Changed the version to 2.4 and modified the doc string.
;;;             : * Depricating the :conservative-update-visual parameter - does
;;;             :   anyone set that now.
;;;             : * Fixed a typo in the assign-finst chunk-type definition.
;;; 2007.01.03 Dan
;;;             : * Added an event to the trace when encoding-complete doesn't
;;;             :   find something to show the failure to encode (in high detail).
;;;             : * Added a set-visloc-default-fct function to go along with
;;;             :   the set-visloc-default macro that was there and made it
;;;             :   check for current model/mp to be safe.
;;; 2007.01.04 Dan
;;;             : * Fixed move-attention so that the times are randomized when
;;;             :   the :randomize-time parameter is set.
;;; 2007.04.19 Dan
;;;             : * Added a check whether pm-constant is already a chunk-type
;;;             :   in the resetting of the module.
;;; 2007.05.24 Dan
;;;             : * Took the obsolete :visual-source-activation checks out of
;;;             :   the parameter handler.
;;;             : * Performance tweaks:
;;;             :   - only sort the finsts when necessary to find the oldest
;;;             :   - removed the update-module method by having check-finsts
;;;             :     called directly by find-location, assign-finst and
;;;             :     print-visicon also calling it when the finst parameters changed
;;;             :     and
;;;             :     adding the method update-new which does the checknew tests
;;;             :     and call that at the start of find-location and print-visicon
;;;             :     (doesn't matter if it's out of date otherwise).
;;; 2007.12.11 Dan
;;;             : * Added the randomizing of times for the reencoding events.
;;;             :   That was a bug introduced in the 5->6 transition.
;;; 2007.12.13 Dan
;;;             : * Added a check to update-tracking-mth to make sure that there
;;;             :   is an object being tracked before doing any of the updating
;;;             :   otherwise there are issues with nil slots...
;;; 2007.12.13 Dan
;;;             : * If the model is tracking an object then stuff-visloc-buffer
;;;             :   shouldn't put a chunk there.  As the priorities are set now
;;;             :   the stuffed chunk overwrites the tracked chunk, and I don't
;;;             :   want to mess with the priorities so it seems best to just
;;;             :   not stuff in that situation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN 
;;; Start by making sure the dmi and general-pm files have been loaded
;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "DMI" "ACT-R6:support;dmi")
(require-compiled "GENERAL-PM" "ACT-R6:support;general-pm")


#+:allegro (eval-when (:compile-toplevel :Load-toplevel :execute)
             (setf *enable-package-locked-errors* nil))


(eval-when (:compile-toplevel :Load-toplevel :execute)
  (proclaim '(optimize (speed 3) (space 0))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Class declarations
;;;; ---------------------------------------------------------------------- ;;;;

(defclass vision-module (attn-module)
  ((visicon :accessor visicon :initarg :visicon :initform nil)
   (optimize-visual :accessor optimize-p :initarg :optimize-p :initform t)
   (print-viewed :accessor print-view-p :initarg :print-view-p :initform 
                 ;;; Dan                  
                 ;                 t)
                 nil)
   (conservative-update :accessor conservative-p :initarg :conservative-p 
                        :initform nil)
   (move-attention-latency :accessor move-attn-latency 
                           :initarg :move-attn-latency :initform 0.085)
   (tracked-object :accessor tracked-obj :initarg :tracked-obj :initform nil)
   (tracked-object-last-location :accessor tracked-obj-lastloc 
                                 :initarg :tracked-obj-lastloc :initform nil)
   (tracked-object-last-feat :accessor tracked-obj-last-feat 
                                 :initarg :tracked-obj-last-feat :initform nil)
   (tracked-object-last-obj :accessor tracked-obj-last-obj 
                                 :initarg :tracked-obj-last-obj :initform nil)
   (last-scale :accessor last-scale :initarg :last-scale :initform nil)
   (moving-attention :accessor moving-attention :initarg :moving-attention 
                     :initform nil)
   (move-allowance :accessor move-allowance :initarg :move-allowance
                   :initform 0)
   (synthd-objs :accessor synthd-objs :initarg :synthd-objs
                :initform (make-hash-table)) 
   (found-locs :accessor found-locs :initarg :found-locs
               :initform (make-hash-table))
   (feature-sets :accessor feature-sets :initarg :feature-sets 
                 :initform (all-feature-sets))
   (active-cfs :accessor active-cfs :initarg :active-cfs 
               :initform nil)
   (num-finst :accessor num-finst :initarg :num-finst :initform 4)
   (finst-lst :accessor finst-lst :initarg :finst-lst :initform nil)
   (finst-span :accessor finst-span :initarg :finst-span :initform 3.0)
   (new-span :accessor new-span :initarg :new-span :initform 0.5)
   (default-spec :accessor default-spec :initarg :default-spec 
     :initform nil)
   (current-lof :accessor current-lof :initarg :current-lof :initform #(0 0))
   (trace-attn-p :accessor trace-attn-p :initarg :trace-attn-p :initform nil)
   (attn-trace :accessor attn-trace :initarg :attn-trace :initform nil)
   (visual-lock :accessor visual-lock :initform nil)
   )
  #| mdb moved these 2005.01.07 to generic-pm.lisp
   ;;; dan 
   ;;; added this to record failures and record stuffing of visual-locations
   ;;;
   
   (loc-failure :accessor loc-failure :initform nil)
   (attend-failure :accessor attend-failure :initform nil)
   (stuffed :accessor stuffed :initform nil))
  |#  
  (:default-initargs
    :name :VISION
    :version-string "2.4"
    ))

(defclass found-spec ()
  ((spec :accessor spec :initarg :spec :initform nil)
   (feat :accessor feat :initarg :feat :initform nil)))


(defclass finst ()
  ((id :accessor id :initarg :id :initform nil)
   (tstamp :accessor tstamp :initarg :tstamp :initform 0.0)
   (synthed-from :accessor synthed-from :initarg :synthed-from :initform nil)))


;;; ICON-FEATURE      [Class]
;;; Description : Coordinates (x, y, width, height) should be in pixels to
;;;             : relate directly to the screen.  However, the SIZE attribute
;;;             : should be in degrees of visual angle (squared, of course)
;;;             : for FIND-LOCATION calls.
;;;             : Beta6 added WIDTH, HEIGHT, and SIZE attributes.

(defclass icon-feature ()
  ((screen-x :accessor screen-x :initarg :x :initform nil)
   (screen-y :accessor screen-y :initarg :y :initform nil)
   ;; need initiform for distance?
   (distance :accessor distance :initarg :distance 
             ; DAN
             ; :initform (when *mp* (viewing-distance (device-interface *mp*))))
             
             :initform (when (current-device-interface) 
                         (viewing-distance (current-device-interface))))
   
   (attended-p :accessor attended-p :initarg :attended-p :initform 'NEW)
   (kind :accessor kind :initarg :kind :initform 'visual-object)
   (val :accessor val :initarg :value :initform nil)
   (color :accessor color :initarg :color :initform 'black)
   (dmo-id :accessor dmo-id :initarg :dmo-id :initform nil)     	
   (screen-obj :accessor screen-obj :initarg :screen-obj :initform nil) 
   (height :accessor height :initarg :height :initform nil)
   (width :accessor width :initarg :width :initform nil)
   (size :accessor size :initarg :size :initform nil)
   (tstamp :accessor tstamp :initarg :tstamp :initform nil) 
   (obj-freq :accessor obj-freq :initarg :obj-freq :initform 0.01)
   (salience :accessor salience :initarg :salience :initform 0)
   (userprop1 :accessor userprop1 :initarg :userprop1 :initform nil)
   (userprop2 :accessor userprop2 :initarg :userprop2 :initform nil)
   (userprop3 :accessor userprop3 :initarg :userprop3 :initform nil)
   (userprop4 :accessor userprop4 :initarg :userprop4 :initform nil)
   ))

(defclass oval-feature (icon-feature)
  ()
  (:default-initargs
    :kind 'oval
    :value 'oval
    ))

(defclass led-line-feature (icon-feature)
  ((line-pos :accessor line-pos :initarg :line-pos :initform nil)
   (left-edge :accessor left-edge :initarg :left :initform nil)
   (right-edge :accessor right-edge :initarg :right :initform nil))
  (:default-initargs
    :kind 'text))

(defclass cursor-feature (icon-feature)
  ()
  (:default-initargs
    :kind 'cursor
    :value 'pointer))

(defclass rect-feature (icon-feature)
  ()
  (:default-initargs
    :kind 'visual-object
    :value 'box))

(defclass line-feature (icon-feature)
  ((end1-x :accessor end1-x :initarg :end1-x :initform nil)
   (end1-y :accessor end1-y :initarg :end1-y :initform nil)
   (end2-x :accessor end2-x :initarg :end2-x :initform nil)
   (end2-y :accessor end2-y :initarg :end2-y :initform nil)
   )
  (:default-initargs
    :kind 'line
    :value 'line))

(defclass text-feature (icon-feature)
  ()
  (:default-initargs
    :kind 'text))

(defclass empty-feature (icon-feature)
  ()
  (:default-initargs
    :kind 'EMPTY-SPACE
    :value nil
    :color nil))

(defclass feature-spec (icon-feature spec)
  ((nearest :accessor nearest :initarg :nearest :initform nil))
  (:default-initargs 
    :check-slots #(screen-x screen-y distance kind val color size 
                              userprop1 userprop2 userprop3 userprop4)
    :x :IGNORE
    :y :IGNORE
    :attended-p :IGNORE
    :value :IGNORE
    :color :IGNORE
    :kind :IGNORE
    :size :IGNORE
    :userprop1 :IGNORE
    :userprop2 :IGNORE
    :userprop3 :IGNORE
    :userprop4 :IGNORE
    ))

(defmethod initialize-instance :after ((feat feature-spec) &key)
  (setf (dmo-id feat) (gensym "SPEC")))

(defclass char-primitive-feature (icon-feature)
  ((true-feat :accessor true-feat :initarg :true-feat :initform nil)
   (left-edge :accessor left-edge :initarg :left :initform nil)
   (right-edge :accessor right-edge :initarg :right :initform nil))
  (:default-initargs
    :kind 'text))

;;; SYNTHD-FEAT      [Class]
;;; Date        : 99.04.02
;;; Description : When an object (word or phrase) is synthesized out of 
;;;             : primitive features, some record of that has to be kept
;;;             : around for things like mouse targeting.  Synthetic
;;;             : features are hashed by ID, and store the IDs of the
;;;             : features from which they were constructed.

(defclass synthd-feat (icon-feature)
  ((built-from :accessor built-from :initform nil :initarg :built-from)))


(defclass dummy-feature ()
  ((xy-loc :accessor xy-loc :initarg :xy-loc :initarg nil)))

(defgeneric approach-width (feat theta)
  (:documentation "Returns the width of a feature in degrees given an approach angle."))

(defmethod approach-width ((feat dummy-feature) (theta float))
  ; DAN
  ;  (default-target-width (motor-m *mp*)))
  ; not sure what to do with this one because
  ; it seems like it should go into the motor module
  ; 
  (aif (get-module :motor)
       (default-target-width it)
       1.0))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Module-level stuff stuff

(defmethod initialize-instance :after ((vis-mod vision-module) &key)
  "Initializes a vision module"
  ;;; DAN 
  ;;; state chunks are not necessary 
  ;;; 
  ;;; (setf (state-dmo vis-mod)
  ;;;      (make-dme 'vision-state 'module-state
  ;;;                '(module :vision modality free processor free preparation free
  ;;;                  execution free)
  ;;;                :where :external))
  
  ;;; DAN
  ; With the device inteface being a real module we can't guarantee that it
  ; exists when vision is created and a feature-spec (through icon-feature) 
  ; uses it to initialze the distance.  
  ; Taking this out shouldn't be a problem though because it gets set in the
  ; reset-pm-module and that's always going to be called after creation.
  
  ;(setf (default-spec vis-mod)
  ;      (make-instance 'feature-spec :attended-p 'NEW :x 'LOWEST :tstamp 0))
  ; 
  
  (set-cfs-mth vis-mod :RM-ORIG))


;;; DAN renamed method reset-pm-module

;;; RESET-PM-MODULE      [Method]
;;; Date        : 97.03.31
;;; Description : Besides the basics, clear the icon and reset the 
;;;             : current location to be nil.

(defmethod reset-pm-module :after ((vis-mod vision-module))
  "Resets the state of a Vision Module"
  (setf (visicon vis-mod) nil)
  (remove-tracking vis-mod)
  (setf (last-scale vis-mod) nil)
  (set-cfs-mth vis-mod :RM-ORIG)
  (setf (synthd-objs vis-mod) (clrhash (synthd-objs vis-mod)))
  (setf (found-locs vis-mod) (clrhash (found-locs vis-mod)))
  (setf (finst-lst vis-mod) nil)
  (setf (default-spec vis-mod)
        (make-instance 'feature-spec :attended-p 'NEW :x 'LOWEST :tstamp 0))
  (setf (attn-trace vis-mod) nil)
  (set-clof vis-mod #(0 0))
  
  ;;; DAN added this for the new slots
  
  (setf (loc-failure vis-mod) nil)
  (setf (attend-failure vis-mod) nil)
  ; not needed anymore (setf (stuffed vis-mod) nil)
  )


(defmethod clear :after ((vis-mod vision-module))
  (remove-tracking vis-mod)
  
  ;;; Dan this seems to make sense
  (setf (loc-failure vis-mod) nil)
  (setf (attend-failure vis-mod) nil)
  )



;;  (when (tracked-obj vis-mod)
;;    (update-tracking-mth vis-mod))




(defgeneric set-cfs-mth (vis-mod kwrd)
  (:documentation "Set the current character feature set."))

(defmethod set-cfs-mth ((vis-mod vision-module) kwrd)
  (setf (active-cfs vis-mod) (get-cfs-mth vis-mod kwrd)))


(defgeneric get-cfs-mth (vis-mod kwrd)
  (:documentation "Given a keyword, find a character feature set."))

(defmethod get-cfs-mth ((vis-mod vision-module) kwrd)
  (aif (member kwrd (feature-sets vis-mod) :key #'name)
    (first it)
    (pm-warning "Feature set ~S is unknown" kwrd)))


(defgeneric visicon-update (vis-mod)
  (:documentation "To be called after every time the visicon changes."))

(defmethod visicon-update ((vis-mod vision-module))
  (check-finsts vis-mod)
  (update-attended-loc vis-mod)
  (length (visicon vis-mod)))


(defgeneric add-screen-object (obj vm)
  (:documentation "Add the visual features for one screen object."))

(defmethod add-screen-object (obj (vm vision-module))
  (let ((vfeats (flatten (build-features-for obj vm)))
        ;(time (pm-time))
        ;;; DAN 
        ;;; adjusted the time function
        (time (mp-time)))
    (dolist (feat vfeats)
      (push (enter-into-visicon feat time) (visicon vm))))
  (visicon-update vm)
  )

(defgeneric delete-screen-object (objid vm)
  (:documentation "Delete all visual features for one screen object."))

(defmethod delete-screen-object (objid (vm vision-module))
  (setf (visicon vm)
        (delete objid (visicon vm) :test #'equal :key #'screen-obj))
  (visicon-update vm))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Moving attention and its immediate support functions, such
;;;;     as FOCUS-ON and the various GET- functions.
;;;; ---------------------------------------------------------------------- ;;;;

;;; MOVE-ATTENTION      [Method]
;;; Date        : 97.02.09
;;; Description : This is a toplevel command for the Vision Module, causing
;;;             : attention to move.  Latencey is 185 ms, so nothing actually
;;;             : happens right away other than setting state variables.
;;;             : A method is dispatched based on the scale that's passed.

(defgeneric move-attention (vis-mod &key location scale)
  (:documentation "Shift attention to a particular location at a particular scale."))

(defmethod move-attention ((vis-mod vision-module) &key location scale)
  (declare (symbol scale))
  (if (eq (exec-s vis-mod) 'BUSY)
    (pm-warning "Attention shift requested at ~S while one was already in progress."
                (mp-time))
    (progn
      (when (tracked-obj vis-mod) (remove-tracking vis-mod))
      ;(setf (input-q vis-mod) nil)      ; gone, right?
      (setf (moving-attention vis-mod) t)
      (clear-attended vis-mod)
      (setf (last-scale vis-mod) scale)
      
      ;;; DAN save the chunk's name for the trace 
      
      (let ((chunk-name location))
        (setf location (psdme-to-dmo location))
        
        ;;; DAN
        ;;; clear the failure indicator
        
        (setf (attend-failure vis-mod) nil)
        
        ;;; Dan
        ;(queue-command
        ; :time (move-attn-latency vis-mod) :where :VISION :command 'encoding-complete
        ; :randomize t :params `(,location ,scale))
        
        (schedule-event-relative 
         (randomize-time (move-attn-latency vis-mod))
         'encoding-complete
         :destination :vision
         :module :vision
         :params (list location scale)
         :details ;(format nil "~S ~S ~S" 'encoding-complete chunk-name scale)
         ;; For speed don't use format on the fly
         (concatenate 'string "Encoding-complete "
           (symbol-name chunk-name)
           " "
           (symbol-name scale))
         :output 'medium))
      
      
      (setf (current-marker vis-mod) location)
      (set-clof vis-mod (dmo-to-xy location))
      (change-state vis-mod :exec 'BUSY :proc 'BUSY))))


;;; ENCODING-COMPLETE      [Method]
;;; Description : Several things to do when focusing attention on a location.
;;;             : [1] Make the location attended, set state to FREE.
;;;             : [2] Make it an external source (and null the old source).
;;;             : [3] If there was nothing there, or we were non-conserving
;;;             :     get the thing at that location.
;;;             : [4] If there is something, synch it up with the location
;;;             :     chunk.
;;;             : [5] If requested, print it.

(defgeneric encoding-complete (vis-mod loc-dmo scale)
  (:documentation "When MOVE-ATTENTION completes, focus on a place with this."))

(defmethod encoding-complete ((vis-mod vision-module) (loc-dmo dmo) scale)
  (declare (symbol scale))
  
  ;; DAN
  ;; With the theory this is not going to be a chunk slot in the future
  ;; don't do it now because that simplifies things with respect to 
  ;; mapping back to the chunk from which it came i.e. the name that's
  ;; stored in the found-locs table.
  ;;
  ;(set-attributes loc-dmo '(attended t))
  
  (setf (moving-attention vis-mod) nil)
  (change-state vis-mod :exec 'free :proc 'free)
  ;(setf (current-marker vis-mod) loc-dmo)
  (let ((return-obj (get-obj-at-location vis-mod loc-dmo scale)))
    (unless return-obj
      ;(clear-loc loc-dmo)
      (clear-attended vis-mod)
      
      ;;; DAN
      ;;; record the failure
      
      (setf (attend-failure vis-mod) t)
      
      ;; note this in the trace...
      (schedule-event-relative 0 'no-visual-object-found :maintenance t :module :vision :output 'high :details "No visual-object found")
      
      (return-from encoding-complete nil))
    
    (set-attended vis-mod return-obj)
    (when (print-view-p vis-mod)
      (pm-output nil "Vision sees ~S" (id return-obj)))
    ;(format t "~% <> MOVE-ATTENTION generated DMO ~S~%" (id return-obj)))
    return-obj))

(defun no-visual-object-found ()
  "Dummy function to indicate failure to encode - someone may want to do something with this later"
  )


(defgeneric get-obj-at-location (vis-mod loc-dmo scale)
  (:documentation  "Given a location and a scale, return a DMO representing what's there."))

(defmethod get-obj-at-location ((vis-mod vision-module) (loc-dmo dmo) 
                               scale)
  (declare (symbol scale))
  ;(break)
  (let ((return-obj nil)
        (xy-loc (dmo-to-xy loc-dmo)))
    (if (conservative-p vis-mod)
      (setf return-obj (conservative-get-object vis-mod loc-dmo))
      (progn
        (setf return-obj 
              (cond ((eq scale 'PHRASE)
                     (get-phrase-at vis-mod xy-loc))
                    ((and (eq scale 'WORD) (not (optimize-p vis-mod)))
                     (get-word-at-noopt vis-mod loc-dmo))
                    (t
                     (let ((feat-lis (within-move vis-mod xy-loc)))
                       (when (eq scale 'WORD)
                         (setf feat-lis (text-feats feat-lis)))
                       (when feat-lis
                         (featlis-to-focus vis-mod loc-dmo feat-lis))))))))
    return-obj)) 



;;; CONSERVATIVE-GET-OBJECT      [Method]
;;; Date        : 99.03.29
;;; Description : When CONSERVATIVE is on, try to grab an object that's 
;;;             : present in the current location chunk.  

(defgeneric conservative-get-object (vis-mod loc-dmo)
  (:documentation "Create a DMO for the object at the given location, when :CONSERVATIVE-UPDATE-VISUAL is true."))

(defmethod conservative-get-object ((vis-mod vision-module) 
                                        (loc-dmo dmo))
  (let ((present-objects
         (remove-if-not #'(lambda (o) (object-present-p vis-mod o))
                        (get-attribute loc-dmo 'objects)))
        (best-object nil)
        (base-feat (feat (gethash (id loc-dmo) (found-locs vis-mod)))))
    (when present-objects
      (if (null base-feat)
        (setf best-object (random-item present-objects))
        (progn
          (setf best-object 
                (determine-focus-dmo vis-mod present-objects base-feat))
          (setf (attended-p base-feat) t)))
      (setf (currently-attended vis-mod) best-object)
      best-object)))


;;; SYNCH-LOCS      [Method]
;;; Date        : 98.07.26, last delta 99.06.24
;;; Description : When an object is focused on, some objects might need to
;;;             : be updated.  This should keep the location chunk in
;;;             : synch with the world and with the object chunk.

(defgeneric synch-locs (vis-mod object location)
  (:documentation "Given an object and location DM rep, synchronizes the appropriate slots."))


;;; DAN
;;;
;;; Not sure about this - it won't change chunks that went to buffers/DM so 
;;; it might not be necessary

(defmethod synch-locs ((vis-mod vision-module) (object dmo) (location dmo))
  (let ((still-around nil))
    (dolist (obj (mklist (get-attribute location 'objects)))
      (when (object-present-p vis-mod obj)
        (push obj still-around)))
    (set-attributes object `(screen-pos ,(id location)))
    (if (null still-around)
      (setf still-around (id object))
      (push (id object) still-around))
    (set-attributes location `(objects ,still-around))))


;;; WITHIN-MOVE      [Method]
;;; Date        : 99.03.29
;;; Description : Simply walk the icon and accumulate features that are within
;;;             : the movement tolerance.

(defgeneric within-move (vis-mod loc)
  (:documentation "Return a list of icon feature within the move allowance of loc."))

(defmethod within-move ((vis-mod vision-module) (loc vector))
  (if (= (move-allowance vis-mod) 0)
    (feat-match-xy (visicon vis-mod) loc)
    (let ((max (pm-angle-to-pixels (move-allowance vis-mod)))
          (accum nil))
      (dolist (feat (visicon vis-mod) accum)
        (when (>= max (dist (xy-loc feat) loc))
          (push feat accum))))))


;;; FEATLIS-TO-FOCUS      [Method]
;;; Date        : 99.03.29
;;; Description : First, determine which feature in the icon is the "best"
;;;             : feature, given the location that is the focus.  That
;;;             : location will have associate with it the feature and the
;;;             : spec used to generate it (in FIND-LOCATION).  Find the best
;;;             : feature, generate the list of DMOs at that location,
;;;             : determinine the focus, and make sure the location chunks and
;;;             : visual object chunks are synched up.

(defgeneric featlis-to-focus (vis-mod loc-dmo feat-lst)
  (:documentation  "Given the source location and a list of features, return the DMO that should be the focus."))

(defmethod featlis-to-focus ((vis-mod vision-module) (loc-dmo dmo) (feat-lis list))
  (let* ((best-feature 
          (find-best-feature feat-lis 
                             
                             ;; here's where we find the "original" name of 
                             ;; the location chunk to look up
                             
                             (gethash (aif (chunk-copied-from-fct (id loc-dmo))
                                           it
                                           (id loc-dmo))
                                      (found-locs vis-mod))))
         (dmo-lis (featlis-to-dmos vis-mod (xy-loc best-feature)
                                     (feat-match-xy feat-lis 
                                                    (xy-loc best-feature))))
         (return-dmo (determine-focus-dmo vis-mod dmo-lis best-feature)))
    (dolist (obj dmo-lis)
      (set-attributes obj `(screen-pos ,(id loc-dmo))))
    return-dmo))


;;; FEATLIS-TO-DMOS      [Method]
;;; Date        : 99.03.29
;;; Description : Actually, some of the features could be CHAR-PRIMITIVE 
;;;             : features, in which case they're part of characters.  Save 
;;;             : those out and make a character out of 'em.

(defgeneric featlis-to-dmos (vis-mod loc feat-lis)
  (:documentation  "Given a list of features, make a DMO for each."))

(defmethod featlis-to-dmos ((vis-mod vision-module) (loc vector) (feat-lis list))
  (let ((primitive-feats nil)
        (dmo-lis nil))
    (dolist (feat (feat-match-xy feat-lis loc))
      (setf (attended-p feat) t)
      ;; If it's a char primitive, push the feature, else push the feature chunk.
      (if (typep feat 'char-primitive-feature) 
        (push  feat primitive-feats)
        (push (feat-to-dmo feat) dmo-lis)))
    (when primitive-feats
      (push (synthesize-letter vis-mod primitive-feats) dmo-lis))
    dmo-lis))


;;; DETERMINE-FOCUS-DMO      [Method]
;;; Date        : 99.03.29
;;; Description : Basically, look for a DMO with the same ID as the feature.
;;;             : If none, see if the DMO was synthesized from that feature.
;;;             : If none of those, return a random one.

(defgeneric determine-focus-dmo (vis-mod dmo-lst feat)
  (:documentation  "Determine which DMO corresponds to <feat>, which should be the 'best' feature."))

(defmethod determine-focus-dmo ((vis-mod vision-module) (dmo-lis list) (feat icon-feature))
  (when (= 1 (length dmo-lis))
    (return-from determine-focus-dmo (first dmo-lis)))
  (aif (member (dmo-id feat) dmo-lis :key #'id)
    (first it)
    (dolist (dmo dmo-lis (random-item dmo-lis))
      (when (member (dmo-id feat) 
                    (synthed-to-features vis-mod (id dmo) nil))
        (return-from determine-focus-dmo dmo)))))


;;; GET-WORD-AT-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : Getting a word when optimizing is off involves calling
;;;             : SYNTHESIZE-WORD method, but we need to collect the 
;;;             : locations first, and also check the return if what was
;;;             : sent in was a DMO.

(defgeneric get-word-at-noopt (vis-mod loc-dmo)
  (:documentation  "Synthesize a word at the given location and synch it with the location."))

(defmethod get-word-at-noopt ((vis-mod vision-module) (loc-dmo dmo))
  (multiple-value-bind (locs xmin xmax) 
                       (adjoining-led-locs vis-mod (dmo-to-xy loc-dmo))
    (when locs
      (let ((rtn-dmo (synthesize-word vis-mod locs 
                                        (- xmax xmin) (dmo-to-xy loc-dmo))))
        (when rtn-dmo
          (set-attributes rtn-dmo `(screen-pos ,(id loc-dmo)))
          (values rtn-dmo xmin xmax))))))



(defmethod get-word-at-noopt ((vis-mod vision-module) (loc vector))
  (multiple-value-bind (locs xmin xmax)  (adjoining-led-locs vis-mod loc)
    (when locs
      (let ((rtn-dmo (synthesize-word vis-mod locs 
                                        (- xmax xmin) loc)))
        (when rtn-dmo
          (values rtn-dmo xmin xmax))))))


;;; GET-WORD-DMOS-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : OK, when optimizing is off and a phrase needs to be built,
;;;             : the tricky bit is figuring out which locations you need
;;;             : to grab words from, since if you hit every x location, 
;;;             : you'll generate multiple copies of each word.

(defgeneric get-word-dmos-noopt (vis-mod x-lst y)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing off."))

(defmethod get-word-dmos-noopt ((vis-mod vision-module) (x-ls list) y)
  (let ((rtn-dmos nil)
        (curr-x -1))
    (dolist (x x-ls (remove nil (nreverse rtn-dmos)))
      (when (> x curr-x)
        (multiple-value-bind (word min max)
                             (get-word-at-noopt vis-mod (vector x y))
          (declare (ignore min))
          (push word rtn-dmos)
          (setf curr-x max))))))


;;; GET-WORD-DMOS-OPT      [Method]
;;; Date        : 99.04.02
;;; Description : This is simpler when optimizing--just walk the xlist
;;;             : and accumulate words.
;;;             : Might be vestigial as of beta 6.

(defgeneric get-word-dmos-opt (vis-mod x-lst y)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing on."))

(defmethod get-word-dmos-opt ((vis-mod vision-module) (x-ls list) y)
  (let (accum)
    (dolist (x x-ls (nreverse accum))
      (dolist (feat (text-feats (feat-match-xy (visicon vis-mod) (vector x y))))
        (push (feat-to-dmo feat) accum)))))


;;; GET-ICON-OBJECT-AT      [Method]
;;; Date        : 97.06.01
;;; Description : Returns the DMO representing the feature(s) at a
;;;             : location.  If there are a whole bunch of the little line
;;;             : features that comprise letters, then do the categorization
;;;             : bit on the letters.

(defgeneric get-icon-object-at (vis-mod loc)
  (:documentation  "Return the chunk representing the icon feature(s) at the given <loc>"))

(defmethod get-icon-object-at ((vis-mod vision-module) (loc vector))
  (let ((led-feats nil)
        (dmo-lis nil)
        (return-dmo nil))
    (dolist (feat (within-move vis-mod loc))
      (setf (attended-p feat) t)
      ;; If it's an LED line, push the feature, else push the feature chunk.
      (if (eq (kind feat) 'led-line) 
        (push  feat led-feats)
        (push (feat-to-dmo feat) dmo-lis)))
    (when led-feats
      (push (synthesize-letter vis-mod led-feats) dmo-lis))
    (setf return-dmo (random-item dmo-lis))
    return-dmo))


(defun text-feats (feat-lst)
  "Given a list, return only those features which are TEXT features."
  (remove-if #'(lambda (f) (not (eq (kind f) 'text))) feat-lst))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Finst support


(defmethod print-object ((fnst finst) strm)
  (print-unreadable-object (fnst strm :type t)
    (format strm "~S ~4,2F" (id fnst) (tstamp fnst))))


(defgeneric elapsed (object)
  (:documentation  "Return the time since this object was time-stamped."))

(defmethod elapsed ((fnst finst))
  (- (mp-time) (tstamp fnst)))


(defmethod in-finst ((name symbol) (fnst finst))
  (or (eq name (id fnst))
      (member name (synthed-from fnst))))


(defgeneric feat-attended (feat vis-mod)
  (:documentation  "Return the attended status of a visicon feature object."))

(defmethod feat-attended ((feat icon-feature) (vis-mod vision-module))
  (if (eq (attended-p feat) 'NEW)
    'NEW
    (consp (member (dmo-id feat) (finst-lst vis-mod) 
                   :test #'in-finst))))


(defgeneric test-attended (spec feat)
  (:documentation  "Test a visicon feature against a spec, return T if they match on ATTENDED."))

(defmethod test-attended ((spec icon-feature) (feat icon-feature))
  (cond ((eq (attended-p spec) :IGNORE) t)
        ((and (null (attended-p spec)) (eq (attended-p feat) 'NEW)) t)
        (t (eq (attended-p spec)
               (feat-attended feat  ; DAN (vis-m *mp*))))))
                              (get-module :vision))))))

(defmethod set-attended :after ((vis-mod vision-module) obj)
  
  ;;; DAN
  ;;; put the chunk in the buffer
  
  (schedule-set-buffer-chunk 'visual (dmo-to-psdme obj) 0 :module :vision :priority 10)
                                      
  (aif (member (id obj) (finst-lst vis-mod) :key #'id)
       
       ;(setf (tstamp (first it)) (mp-time *mp*))
       ;;; DAN
       ;;; change the time reference
       (setf (tstamp (first it)) (mp-time))
       
       (push
        (make-instance 'finst :id (id obj) :tstamp (mp-time)
          :synthed-from (synthed-to-features vis-mod (id obj) nil))
        (finst-lst vis-mod)))
  
  (when (> (length (finst-lst vis-mod)) (num-finst vis-mod))
    (sort-finsts vis-mod)
    (pop (finst-lst vis-mod))))


(defgeneric sort-finsts (vis-mod)
  (:documentation  "Sort finsts according to time stamp."))

(defmethod sort-finsts ((vis-mod vision-module))
  (setf (finst-lst vis-mod) (sort (finst-lst vis-mod) #'< :key #'tstamp)))


(defgeneric check-finsts (vis-mod)
  (:documentation  "Update finsts against what's on the display and sort."))

(defmethod check-finsts ((vis-mod vision-module))
  (setf (finst-lst vis-mod)
        (delete-if #'(lambda (f) 
                       (or (not (object-present-p vis-mod (id f)))
                           (> (elapsed f)
                              (finst-span vis-mod))))
                   (finst-lst vis-mod))))

(defgeneric assign-finst (vis-mod &key object location)
  (:documentation "Assign a finst to an object or location."))

(defmethod assign-finst ((vm vision-module) &key object location)
  (if (and (null object) (null location))
    (pm-warning "ASSIGN-FINST called with two null arguments")
    (let* ((feat (if object
                    (obj-to-feat vm (psdme-to-dmo object))
                    (loc-to-feat vm (psdme-to-dmo location))))
           (finst (make-instance 'finst :id (dmo-id feat) :tstamp 
                    ;(pm-time)
                    ;;; DAN
                    ;;; change the time reference
                    (mp-time))))
      (setf (attended-p feat) t)
      (push finst (finst-lst vm))
      
      (check-finsts vm)
      
      (when (> (length (finst-lst vm)) (num-finst vm))
        (sort-finsts vm)
        (pop (finst-lst vm)))
      finst)))

(defgeneric finst-available-p (vis-mod)
  (:documentation "Are there any unused finsts?"))

(defmethod finst-available-p ((vm vision-module))
  (< (length (finst-lst vm)) (num-finst vm)))


(defgeneric finst-to-xy (fnst vis-mod)
  (:documentation "Return the XY location of a finst."))

(defmethod finst-to-xy ((f finst) (vm vision-module))
  (xy-loc (feat-named vm (id f))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Supporting NEW tags

(defmethod elapsed ((feat icon-feature))
  ;(- (mp-time *mp*) (tstamp feat))
  ;;; DAN
  ;;; change the time reference
  (- (mp-time) (tstamp feat)))


(defgeneric checknew (feat vis-mod)
  (:documentation  "Check to see if a visicon feature is still NEW."))

(defmethod checknew ((feat icon-feature) (vis-mod vision-module))
  (when (and (eq (attended-p feat) 'NEW)
             (> (elapsed feat) (new-span vis-mod)))
    (setf (attended-p feat) nil))
  feat)

(defmethod update-new ((vis-mod vision-module))
  (mapc #'(lambda (f) (checknew f vis-mod)) (visicon vis-mod)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Synthetic features (letters, words, and phrases).
;;;; ---------------------------------------------------------------------- ;;;;

(defgeneric record-synth (vis-mod the-dmo &key x y width height built-from)
  (:documentation  "Record the id of a synthesized object and its properties."))

(defmethod record-synth ((vis-mod vision-module) (the-dmo dmo) 
                           &key x y width height built-from)
  (declare (fixnum x y width height))
  (when (not (and x y width height built-from))
    (error "NIL passed to RECORD-SYNTH."))
  (setf (gethash (id the-dmo) (synthd-objs vis-mod))
        (make-instance 'synthd-feat :dmo-id (id the-dmo)
                       :built-from (mklist built-from)
                       :width width :height height
                       :x x :y y)))


;;; SYNTHED-TO-FEATURES      [Method]
;;; Description : If the ID is in the hash table of synthed IDs, then 
;;;             : recurse on those.

(defgeneric synthed-to-features (vis-mod objid accum)
  (:documentation  "Given a synthetic object, return a list of icon features comprising it."))

(defmethod synthed-to-features ((vis-mod vision-module) objid accum)
  (aif (gethash objid (synthd-objs vis-mod))
    (flatten (mapcar #'(lambda (f) (synthed-to-features vis-mod f nil)) 
                     (built-from it)))
    (cons objid accum)))


;;; FEAT-OR-SYNTH      [Method]
;;; Date        : 99.04.02
;;; Description : VISUAL-OBJECTs should correspond to either real features
;;;             : or synthetic ones.  Look in both places.

(defgeneric feat-or-synth (vis-mod id)
  (:documentation  "Return the icon feature or synthetic object associated with <id>."))

(defmethod feat-or-synth ((vis-mod vision-module) id)
  (aif (member id (visicon vis-mod) :key #'dmo-id)
    (first it)
    (awhen (gethash id (synthd-objs vis-mod))
      it)))


;;; GET-PHRASE-AT      [Method]
;;; Date        : 98.07.27
;;; Description : The only way to get a phrase is to synthesize one, there is
;;;             : no "phrase" primitive in RPM.

(defgeneric get-phrase-at (vis-mod loc)
  (:documentation  "Mostly a stub for SYNTHESIZE-PHRASE."))

;;; Dan - commenting this out because it's redefined below
;;; (defmethod get-phrase-at ((vis-mod vision-module) (loc vector))
;;;  (awhen (feat-match-y (visicon vis-mod) (py loc))
;;;    (synthesize-phrase vis-mod it (py loc))))


;;; SYNTHESIZE-LETTER      [Method]
;;; Date        : 98.07.27
;;; Description : From a list of LED-style icon features, synthesize a letter
;;;             : feature and note the synthesis.  The real worker here is
;;;             : Mike Matessa's FIND-BEST-OBJECT function, which is based on
;;;             : "rational categorization" of the features into a letter.

(defgeneric synthesize-letter (vis-mod feats)
  (:documentation  "Build a DMO representing a letter from a list of LED features."))

(defmethod synthesize-letter ((vis-mod vision-module) (feats list))
  (let ((base-feat (first feats))
        (letter (prob-best-character (active-cfs vis-mod) 
                                      (mapcar #'true-feat feats)))
        (return-dmo nil)
        )
    (dolist (feat feats)
      (setf (attended-p feat) t))
    (setf return-dmo
          (make-dme (new-name-fct "LETTER") 'visual-object
                    `(screen-pos ,(id (current-marker vis-mod))
                                 value ,letter
                                 color black)
                    :obj (screen-obj base-feat)
                    ))
    (record-synth vis-mod return-dmo :x (screen-x base-feat) 
                  :y (screen-y base-feat) :width (width base-feat) 
                  :height (height base-feat)
                  :built-from (mapcar #'dmo-id feats))
    return-dmo))


;;; SYNTHESIZE-WORD      [Method]
;;; Date        : 98.07.27
;;; Description : Given the list of contiguous locations, get the letter 
;;;             : at each location, and then build the word from the list
;;;             : of letters.

(defgeneric synthesize-word (vis-mod loc-lis width center)
  (:documentation  "Build a DMO representing a word from a location."))

(defmethod synthesize-word ((vis-mod vision-module) (loc-lis list) 
                               (width number) (center vector))
  (let ((return-dmo nil)
        (letter-dmos nil))
    (dolist (xloc loc-lis)
      (push (synthesize-letter vis-mod (feat-match-xy (visicon vis-mod) xloc)) 
            letter-dmos)
      (when (not (stringp (get-attribute (first letter-dmos) 'value)))
        (pop letter-dmos)))
    (when letter-dmos
      (setf letter-dmos (nreverse letter-dmos))
      (setf return-dmo
            (make-dme (new-name-fct "WORD") 'TEXT
                      `(screen-pos ,(id (current-marker vis-mod))
                                   value ,(word-accum
                                           (mapcar #'(lambda (x) (get-attribute x 'value))
                                                   letter-dmos)))))
      (record-synth vis-mod return-dmo :x (px center) :y (py center)
                    :width width 
                    :height 
                    (height (feat-or-synth vis-mod (id (first letter-dmos))))
                    :built-from (mapcar #'id letter-dmos))
      return-dmo)))


;;; GET-PHRASE-AT      [Method]
;;; Date        : 98.07.27
;;; Description : The only way to get a phrase is to synthesize one, there is
;;;             : no "phrase" primitive in RPM.


(defmethod get-phrase-at ((vis-mod vision-module) (loc vector))
  (awhen (feat-match-y (visicon vis-mod) (py loc))
    (synthesize-phrase vis-mod it loc)))


(defgeneric synthesize-phrase (vis-mod feature-locs loc)
  (:documentation  "Build a DMO representing a phrase."))

(defmethod synthesize-phrase ((vis-mod vision-module) (feature-locs list) 
                                 (loc vector))
  (let ((x-locs (mapcar #'screen-x feature-locs))
        (word-dmos nil) 
        (words nil)
        (return-dmo nil))
    (setf x-locs (sort (remove-duplicates x-locs) #'<))
    (if (optimize-p vis-mod)
      (setf word-dmos (get-word-dmos-opt vis-mod x-locs (py loc)))
      (setf word-dmos (get-word-dmos-noopt vis-mod x-locs (py loc))))
    ;(setf word-lis (nreverse word-lis))
    (when word-dmos
      (setf words (mapcar #'(lambda (x)
                              (get-attribute x 'value)) word-dmos))
      (setf word-dmos (mapcar #'id word-dmos))
      (setf return-dmo
            (make-dme (new-name-fct "PHRASE") 'phrase!
                      `(screen-pos 
                        ,(id (current-marker vis-mod))
                        objects ,word-dmos
                        words ,words
                        color black
                        value ,(phrase-accum words))))
      (record-synth vis-mod return-dmo :x (px loc) :y (py loc)
                    :width
                    (reduce #'+ 
                            (mapcar #'(lambda (s)
                                        (width (feat-or-synth vis-mod s)))
                                    word-dmos))
                    :height
                    (height (feat-or-synth vis-mod (first word-dmos)))
                    :built-from word-dmos)
      return-dmo)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Stuff for supporting other modules (mostly motor).
;;;; ---------------------------------------------------------------------- ;;;;

;;; UPDATE-ATTEDED-LOC      [Method]
;;; Date        : 97.06.01
;;; Description : What is at the focus of visual attention can change.
;;;             : If this might be happening, call this after PM-PROC-SCREEN
;;;             : to make sure the attended location is updated.
;;;             : UNLESS clause added 98.07.17

(defgeneric update-attended-loc (vis-mod)
  (:documentation  "If the attended location needs an update, update."))

(defmethod update-attended-loc ((vis-mod vision-module))
  ;; if we're tracking or moving around, ignore this 
  (when (or (tracked-obj vis-mod) (moving-attention vis-mod) 
            (eq 'BUSY (exec-s vis-mod)))
    (return-from update-attended-loc nil))
  ;; when do we update?
  ;; [1] when we're looking at an object and it's gone
  ;; [2] when we're looking at nothing and something appears 
  (when (or (and (currently-attended vis-mod)
                 (not (object-present-p vis-mod 
                                        (id (currently-attended vis-mod)))))
            (and (current-marker vis-mod)
                 (null (currently-attended vis-mod))
                 (within-move vis-mod (current-lof vis-mod))))
    
    ;;; Dan
    ;(queue-command
    ; :time (move-attn-latency vis-mod)
    ; :where :VISION
    ; :command 'encoding-complete
    ; :randomize t
    ; :params `(,(current-marker vis-mod) ,(last-scale vis-mod)))
    
    
    ;;; Dan
    
    (schedule-event-relative 
     (randomize-time (move-attn-latency vis-mod))
     'encoding-complete
     :destination :vision
     :module :vision
     :params `(,(current-marker vis-mod) ,(last-scale vis-mod))
     :details ;(format nil "~S ~S ~S" 'encoding-complete 
              ;  (id (current-marker vis-mod))
              ;  (last-scale vis-mod))
     (concatenate 'string "Encoding-complete "
       (symbol-name (id (current-marker vis-mod)))
       " " 
       (symbol-name (last-scale vis-mod)))
     :output 'medium)
    
    
    (change-state vis-mod :exec 'busy)))


;;; OBJECT-PRESENT-P      [Method]
;;; Description : A visual object is present if its ID is still in the icon
;;;             : or if all the features from which it was synthesized are
;;;             : still in the icon.

(defgeneric object-present-p (vis-mod obj-id)
  (:documentation  "Returns NIL if the object ID passed to it is no longer in the icon."))

(defmethod object-present-p ((vis-mod vision-module) obj-id)
  (if (member obj-id (visicon vis-mod) :key #'dmo-id)
    t
    (let ((synthed (gethash obj-id (synthd-objs vis-mod))))
      (when synthed
        (dolist (obj (built-from synthed) t)
          (unless (object-present-p vis-mod obj)
            (return-from object-present-p nil)))))))


;;; LOC-TO-FEAT      [Method]
;;; Date        : 99.04.02
;;; Description : Since each location has associated with it the feature
;;;             : and the spec used to generate that location, it's possible
;;;             : to do a "best match" here vs. the current icon.  If there
;;;             : isn't a best match, then return a dummy feature which will
;;;             : generate the default width.

(defgeneric loc-to-feat (vis-mod loc)
  (:documentation  "Given a location DMO, return the corresponding icon feature."))

(defmethod loc-to-feat ((vis-mod vision-module) (loc dmo))
  (aif (find-best-feature (visicon vis-mod) 
                          (gethash (aif (chunk-copied-from-fct (id loc))
                                        it
                                        (id loc))
                                   (found-locs vis-mod)))
    it
    (make-instance 'dummy-feature :xy-loc (dmo-to-xy loc))))


;;; OBJ-TO-FEAT      [Method]
;;; Date        : 99.04.02
;;; Description : Visual objects should be associated with either real or
;;;             : synthetic features.  If neither of those can be found, at
;;;             : least the location of the object is known, so we can try
;;;             : to find something based on that.

(defgeneric obj-to-feat (vis-mod obj)
  (:documentation  "Given a visual object DMO, return the corresponding icon feature."))

(defmethod obj-to-feat ((vis-mod vision-module) (obj dmo))
  (aif (feat-or-synth vis-mod (id obj))
    it
    (loc-to-feat vis-mod (psdme-to-dmo (get-attribute obj 'screen-pos)))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Finding locations.
;;;; ---------------------------------------------------------------------- ;;;;

;;; FIND-BEST-FEATURE      [Method]
;;; Date        : 99.03.29
;;; Description : The "best" feature is the one with the same ID as the spec. 
;;;             : The next best feature is one that matches the spec and is
;;;             : nearest to that location.  Failing any matches to the spec,
;;;             : then return the nearest feature.

(defgeneric find-best-feature (feat-lst fs)
  (:documentation  "Given a list of features and a spec, return the 'best' feature."))

(defmethod find-best-feature ((feat-lis list) (fs found-spec))
  (awhen (member (dmo-id (feat fs)) feat-lis :key #'dmo-id)
    (return-from find-best-feature (first it)))
  (let ((matches (matching-feats (spec fs) feat-lis)))
    (if matches
      (random-item (nearest-feat matches (xy-loc (feat fs))))
      (random-item (nearest-feat feat-lis (xy-loc (feat fs)))))))


;;; FIND-BEST-FEATURE      [Method]
;;; Date        : 99.03.29
;;; Description : When the spec is null, return a random feature.


(defmethod find-best-feature ((feat-lis list) (fs null))
  (declare (ignore fs))
  (random-item feat-lis))


;;; FIND-LOCATION      [Method]
;;; Date        : 98.08.11R (rewrite), delta 99.06.24
;;; Description : To find a location, we need to search the icon.  To do this,
;;;             : construct a feature spec based on the parameters passed,
;;;             : and filter the list to those that match the spec.  This 
;;;             : should be faster for large icons because it makes only
;;;             : one pass over the icon.  Regardless, some
;;;             : post-processing of that list might be necessary if LOWEST
;;;             : or HIGHEST was passed as a coordinate or if NEAREST is 
;;;             : true.
;;;             : Once we have a list, randomly pick one (that may change in
;;;             : the future) feature and build a location out of it.

(defgeneric find-location (vis-mod &key kind attended value color 
                                       size screen-x screen-y nearest)
  (:documentation  "Given a set of constraints, build a DMO for that screen location if one matches."))


(defmethod find-location ((vis-mod vision-module) 
                             &key (kind :IGNORE) (attended :IGNORE) 
                             (value :IGNORE) (color :IGNORE) (size :IGNORE)
                             (userprop1 :IGNORE) (userprop2 :IGNORE)
                             (userprop3 :IGNORE) (userprop4 :IGNORE)
                             screen-x screen-y distance nearest)
  
  (update-new vis-mod)
  (check-finsts vis-mod)
  
  (let ((spec (construct-findloc-spec 
                :kind kind :attended attended :value value :color color 
                :size size :screen-x screen-x :screen-y screen-y 
                :userprop1 userprop1 :userprop2 userprop2 :userprop3 userprop3
                :userprop4 userprop4 :distance distance :nearest nearest)))
    (awhen (find-current-locs-with-spec vis-mod spec)
      (construct-location vis-mod 
                          (random-item (objs-max-slotval it 'tstamp))
                          spec))))


(defun construct-findloc-spec (&key (kind :IGNORE) (attended :IGNORE) 
                                        (value :IGNORE) (color :IGNORE) 
                                        (size :IGNORE) (userprop1 :IGNORE) 
                                        (userprop2 :IGNORE) 
                                        (userprop3 :IGNORE) 
                                        (userprop4 :IGNORE) 
                                        screen-x screen-y 
                                        distance nearest)
  (make-instance 'feature-spec
    :attended-p attended  :value value  :kind kind  :size size
    :userprop1 userprop1 :userprop2 userprop2 :userprop3 userprop3
    :userprop4 userprop4
    :color color :nearest nearest 
    :distance (if (null distance) :IGNORE distance)
    :x (if (null screen-x) :IGNORE screen-x)
    :y (if (null screen-y) :IGNORE screen-y)))


(defmethod find-current-locs-with-spec ((vis-mod vision-module) 
                                             (base-spec feature-spec))
  (let ((feat-lst nil)
        (temp-spec (copy-instance base-spec)))
    (when (or (member (screen-x temp-spec)
                      '(greater-than-current less-than-current current lowest 
                        highest))
              (member (screen-y temp-spec)
                      '(greater-than-current less-than-current current lowest 
                                             highest))
              (member (distance temp-spec)
                      '(lowest highest)))
      (setf temp-spec (remap-spec-to-current vis-mod temp-spec)))
    (setf (nearest temp-spec) :IGNORE)
    ;; do the actual search
    (setf feat-lst (objs-match-spec (visicon vis-mod) temp-spec))
    ;; some filtering--there's an ordering issue here!
    (case (screen-x base-spec)
      (lowest (setf feat-lst (objs-min-slotval feat-lst 'screen-x)))
      (highest (setf feat-lst (objs-max-slotval feat-lst 'screen-x))))
    (case (screen-y base-spec)
      (lowest (setf feat-lst (objs-min-slotval feat-lst 'screen-y)))
      (highest (setf feat-lst (objs-max-slotval feat-lst 'screen-y))))
    (case (distance base-spec)
      (lowest (setf feat-lst (objs-min-slotval feat-lst 'distance)))
      (highest (setf feat-lst (objs-max-slotval feat-lst 'distance))))
    ;; handle the NEAREST flag
    (when (and (nearest base-spec) feat-lst)
      (setf feat-lst
            (case (nearest base-spec)
              (CURRENT (nearest-feat feat-lst (current-lof vis-mod)))
              (CURRENT-X (objs-nearest-slotval feat-lst 'screen-x 
                                               (px (current-lof vis-mod))))
              (CURRENT-Y (objs-nearest-slotval feat-lst 'screen-y
                                               (py (current-lof vis-mod))))
              (otherwise 
               (nearest-feat feat-lst
                             (dmo-to-xy 
                              (psdme-to-dmo
                               ;DAN (get-safe-wme (nearest base-spec)))))))))
                               (nearest base-spec))))))))
    #|
    (when (and (nearest base-spec) feat-lst)
      (if (eq (nearest base-spec) 'CURRENT)
        (setf feat-lst (nearest-feat feat-lst (current-lof vis-mod)))
        (setf feat-lst (nearest-feat feat-lst 
                                     (dmo-to-xy 
                                      (psdme-to-dmo
                                       (get-safe-wme (nearest base-spec))))))))
|#
    feat-lst))

(defmethod remap-spec-to-current ((vis-mod vision-module) 
                                      (spec feature-spec))
  (let ((current-loc (current-lof vis-mod)))
    (setf (screen-x spec)
          (case (screen-x spec)
            (GREATER-THAN-CURRENT (greater-than (px current-loc)))
            (LESS-THAN-CURRENT (less-than (px current-loc)))
            (CURRENT (px current-loc))
            (LOWEST :IGNORE)
            (HIGHEST :IGNORE)
            (otherwise (screen-x spec))))
    (setf (screen-y spec)
          (case (screen-y spec)
            (GREATER-THAN-CURRENT (greater-than (py current-loc)))
            (LESS-THAN-CURRENT (less-than (py current-loc)))
            (CURRENT (py current-loc))
            (LOWEST :IGNORE)
            (HIGHEST :IGNORE)
            (otherwise (screen-y spec))))
    (setf (distance spec)
          (case (distance spec)
            (LOWEST :IGNORE)
            (HIGHEST :IGNORE)
            (otherwise (distance spec))))
    spec))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Tracking stuff
;;;; ---------------------------------------------------------------------- ;;;;

;;; START-TRACKING      [Method]
;;; Date        : 97.05.15
;;; Description : Starting tracking is pretty easy, actually.  The target
;;;             : object needs to be found and installed in the relevant
;;;             : slots, and the ACT hook functions need to be set.

(defgeneric start-tracking (vis-mod)
  (:documentation  "Begin tracking currently attended object"))

(defmethod start-tracking ((vis-mod vision-module))
    (if (null (currently-attended vis-mod))
        (print-warning "Request to track object but nothing is currently being attended.")
      (progn
        (change-state vis-mod :exec 'BUSY)
        ;(setf (input-q vis-mod) nil)
        (let ((target (pm-obj (currently-attended vis-mod))))
          (setf (tracked-obj vis-mod) target)
          (setf (tracked-obj-lastloc vis-mod) (current-marker vis-mod))
          (setf (tracked-obj-last-obj vis-mod) (currently-attended vis-mod))
          (setf (tracked-obj-last-feat vis-mod) nil)
          (update-tracking-mth vis-mod)
          
          target))))


;;; When tracking it may be necessary to get the name of
;;; the chunk from the vis-loc buffer to set the internal
;;; information correctly.  This method does that, but
;;; it must be scheduled approproately to ensure the right
;;; chunk gets recorded.  
;;; It could be possible for something else to intrude and
;;; mess this up, but the default mechanisms shouldn't 
;;; result in problems.

(defmethod update-tracking-loc-chunk ((vis-mod vision-module) &key (modify nil))
  
  (let* ((vis-loc (buffer-read 'visual-location))
         (vis-loc-dmo (psdme-to-dmo vis-loc)))
    
    (when (currently-attended vis-mod)
      (set-attributes (currently-attended vis-mod) `(screen-pos ,vis-loc)))
    
    (set-clof vis-mod (dmo-to-xy vis-loc-dmo))
    
    (setf (tracked-obj-lastloc vis-mod) vis-loc-dmo)
    
    (when modify 
      (mod-buffer-chunk 'visual (list 'screen-pos vis-loc)))
    ))


(defmethod update-tracking-obj-chunk ((vis-mod vision-module))
  
  (let* ((vis-obj (buffer-read 'visual))
         (vis-obj-dmo (psdme-to-dmo vis-obj)))
    
    
    (setf (tracked-obj-last-obj vis-mod) vis-obj-dmo)))




;;; UPDATE-TRACKING-MTH      [Method]
;;; Date        : 97.05.15
;;; Description : Updating is kind of a pain.  First, if the tracked object
;;;             : hasn't moved, then do nothing.  If it has moved, however,
;;;             : there's a lot of bookkeeping to be done:
;;;             : [1] The old location need to have its object stripped and
;;;             : removed as an activation source.
;;;             : [2] The new location needs to be created, added to the 
;;;             : attention focus, and have its OBJECTS slot set.
;;;             : [3] The object chunk needs to have its location changed.


#|

Proposed new ACT-R 6 mechanism to properly deal with buffer issues:

Then, whenever there's a change to the display the buffers
will be updated as follows:

   First, if the build-features-for returns nil then assume
   that the tracked object is out of sight now.  As with an
   encoding failure, clear visual and set the error state also
   stop the tracking.


   If both buffers are empty:

   A new location is created and the object is modified
   to have that as its screen-pos.  Both chunks (the
   new loc. and the object) are stuffed into the buffers.

  If vis-loc empty, visual holds the tracked item:

   Create a new location chunk and stuff it into
   the visual-location buffer.  Modify the chunk in the
   visual buffer to have that screen-pos.

  vis-loc holds tracked item's loc. and visual is empty:

   Modify the chunk in the visual-location buffer with
   the new info and put the object back into the visual
   buffer (it shouldn't have to be modified at all since
   its loc. is in the vis-loc buffer.

  both buffers hold the appropriate chunks:

   Modify the chunk in the visual-location buffer,
   no other changes necessary.

  If either buffer holds a chunk that isn't related
  to the tracked item:

   Make the appropriate updates to the underlying chunks
   as needed (modify the chunk in the buffer if it's
   the tracked one or create/modify the internal one)
   but don't overwrite a non-tracked chunk in the
   buffer.
|#



(defgeneric update-tracking-mth (vis-mod)
  (:documentation  "Update the state of a tracked object"))

(defmethod update-tracking-mth ((vis-mod vision-module))
  
  (let ((devin (current-device-interface)))
    
    ;; Don't change anything now if there's a lock on the
    ;; device.
    
    (unless (zerop (locks devin))
      (push :tracking (pending-procs devin))
      (return-from update-tracking-mth nil))
    
    (when (tracked-obj vis-mod) ;; If there's nothing being tracked don't bother...
      
      (let* ((old-feat (tracked-obj-last-feat vis-mod))
           
           
           (nf (build-features-for (tracked-obj vis-mod) vis-mod))
           (new-feat (if (listp nf)
                      (car nf) nf))
           (old-loc-dmo (tracked-obj-lastloc vis-mod))
           (old-obj-dmo (tracked-obj-last-obj vis-mod))
           (vis-loc-chunk (buffer-read 'visual-location))
           (vis-obj-chunk (buffer-read 'visual)))
      
      #|(pprint '******************************)
        (pprint old-feat)
        (pprint new-feat)
        (format t "~%equal?: ~S~%raw-old: ~S~%raw-new: ~S~%"
          (and old-feat (feat= old-feat new-feat)) (and old-feat (rc-see (screen-obj old-feat))) (rc-see (screen-obj new-feat)))
        (pprint '******************************)
        |#
      
      
      (unless new-feat
        (schedule-clear-buffer 'visual 0
                               :module :vision :output 'high :priority 13)
        
        (change-state vis-mod :exec 'free :proc 'free)
        (clear-attended vis-mod)
        (setf (current-marker vis-mod) nil)
        (setf (attend-failure vis-mod) t)
        (setf (tracked-obj vis-mod) nil)
        (return-from update-tracking-mth nil))
      
      
      (setf (dmo-id new-feat) (new-name-fct (kind new-feat)))
      
      (setf (tracked-obj-last-feat vis-mod) new-feat)
      
      
      ; Only change it if it's the first time or different
      
      (let ((loc-dmo (current-marker vis-mod))
            (obj-dmo (currently-attended vis-mod)))
      
        ; Something really odd's happening....
        ; For some reason the old feature is getting mashed
        ; somewhere and that's causeing it to be feat=
        ; to the new one even if there's a change from
        ; the last time
        ; so I'm just going to always do this
        ;(unless (and old-feat (feat= old-feat new-feat))
          
        ;;; create new DMO's for the location and the object
        
        ;;; Taken from construct-location 
        
        (setf loc-dmo (find-loc-dmo (xy-loc new-feat)))
        (setf obj-dmo (feat-to-dmo new-feat))
        
        ;; make sure we have one
        (unless loc-dmo
            (setf loc-dmo (xy-to-dmo (xy-loc new-feat) t)))
        ;; fill in from the feature
        (setf loc-dmo (featinfo->loc vis-mod new-feat loc-dmo))
        (setf (gethash (id loc-dmo) (found-locs vis-mod))
            (make-instance 'found-spec :feat new-feat :spec nil))
        
        ;(setf (pm-obj loc-dmo) new-feat)
        
        (setf (current-marker vis-mod) loc-dmo)
        (setf (currently-attended vis-mod) obj-dmo)
        
        
        (set-attributes obj-dmo `(screen-pos ,(ps-ptr loc-dmo)))          
          
        
        
        #|
          
        (pprint '******************************)
        (pprint old-feat)
        (pprint new-feat)
        (format t "~%equal?: ~S~%raw-old: ~S~%raw-new: ~S~%"
          (and old-feat (feat= old-feat new-feat)) (and old-feat (rc-see (screen-obj old-feat))) (rc-see (screen-obj new-feat)))
        (pprint '******************************)
        (pprint-chunks-fct (list (ps-ptr loc-dmo)
                                 (ps-ptr obj-dmo)))
        (pprint '******************************)
        (format t "~%old-loc-dmo: ~S~%old-obj-dmo: ~S~%loc-dmo: ~S~%obj-dmo: ~S~%"
          old-loc-dmo old-obj-dmo loc-dmo obj-dmo)
        
        (format t "vis-loc-chunk: ~S~%vis-obj-chunk ~S~%"
          vis-loc-chunk vis-obj-chunk)
        
        (format t "case 2, 5, 8~%vis-obj-chunk: ~S~%ps-ptr old-obj-dmo: ~S~%"
          vis-obj-chunk (ps-ptr old-obj-dmo))
        
        
        (format t "case 4, 5, 6~%ps-ptr (current-marker vis-mod): ~S~%vis-loc-chunk: ~S~%"
          (ps-ptr (current-marker vis-mod)) vis-loc-chunk)
        
        (pprint '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@)
        |#
        
          (cond ((and (null vis-loc-chunk)
                      (null vis-obj-chunk))
                 
          ;       (pprint 'case-1)
                 
                 #|
                 Stuff both buffers and then update the obj with the 
                 buffer-chunk's name
                 |#
                 
                 (schedule-set-buffer-chunk 'visual-location (ps-ptr loc-dmo) 0
                                            :module :vision :output 'high :requested nil
                                            :priority 15)
                 (schedule-set-buffer-chunk 'visual (ps-ptr obj-dmo) 0
                                            :module :vision :output 'high :requested nil
                                            :priority 14)
                 (schedule-event-relative 0 #'update-tracking-obj-chunk
                                          :module :vision
                                          :destination :vision
                                          :priority 13
                                          :output nil)
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params (list :modify t)
                                          :priority 12
                                          :output nil)
                 
                 )

                ((and (null vis-loc-chunk)
                      (eq vis-obj-chunk (ps-ptr old-obj-dmo)))
                 
         ;        (pprint 'case-2)

                 #|
                 stuff a new location chunk  into
                 the visual-location buffer.  Modify the chunk in the
                 visual buffer with all the new info. and make sure
                 to sync. with the loc in the buffer.



                 Note - need to set priority of the buffer stuffing
                 so that if there's a find-location scheduled
                 but not completed this happens first, so that
                 the find-loc overwrites.  SO, the priority of this
                 needs to be set to > 10.
                 |#
                 
                 
                 (schedule-set-buffer-chunk 'visual-location (ps-ptr loc-dmo) 0
                                            :module :vision :output 'high :requested nil
                                            :priority 15)
                 (schedule-mod-buffer-chunk 'visual 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec (ps-ptr obj-dmo)))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 14)
                 
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params (list :modify t)
                                          :priority 13
                                          :output nil)

                 )

                ((null vis-loc-chunk) ;; The visual chunk is unknown
                 
          ;       (pprint 'case-3)

                 #|
                 stuff a new location chunk  into
                 the visual-location buffer.  
                 Don't touch the chunk in the visual buffer
                 
                 sync. with the loc in the buffer.
                 |#
                 
                 
                 (schedule-set-buffer-chunk 'visual-location (ps-ptr loc-dmo) 0
                                            :module :vision :output 'high :requested nil
                                            :priority 15)
                 
                 
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params (list :modify nil)
                                          :priority 13
                                          :output nil)
                 
                 )
      
                ((and (eq (ps-ptr old-loc-dmo) vis-loc-chunk)
                      (null vis-obj-chunk))
                 
                 
           ;      (pprint 'case-4)

                #|
                Modify the chunk in the visual-location buffer
                
                update the visual new visaul chunk with that loc.
                put it into the buffer
                |#
                 
                 
                 
                 (schedule-mod-buffer-chunk 'visual-location 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec (ps-ptr loc-dmo)))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 15)
                 
                 
                 (set-attributes obj-dmo `(screen-pos ,vis-loc-chunk))
                 
                 (schedule-set-buffer-chunk 'visual (ps-ptr obj-dmo) 0
                                            :module :vision :output 'high :requested nil
                                            :priority 14)
                 
                 (schedule-event-relative 0 #'update-tracking-obj-chunk
                                          :module :vision
                                          :destination :vision
                                          :priority 13
                                          :output nil)
                 
                 )
      
                ((and (eq (ps-ptr old-loc-dmo) vis-loc-chunk)
                      (eq vis-obj-chunk (ps-ptr old-obj-dmo)))
                 
            ;     (pprint 'case-5)


                 #|
                 
                 Modify both chunks after making sure the obj points to the
                 right loc just to be safe.
                |#
                 
                 (set-attributes obj-dmo `(screen-pos ,vis-loc-chunk))
                 
                 (schedule-mod-buffer-chunk 'visual-location 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec (ps-ptr loc-dmo)))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 15)
                 
                 (schedule-mod-buffer-chunk 'visual 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec (ps-ptr obj-dmo)))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 14)
                 
                 )
      
                ((eq (ps-ptr old-loc-dmo) vis-loc-chunk) 
                 ;; Don't know about the visual buffer
                 
             ;    (pprint 'case-6)

                 
                 #|
                 
                 Modify the loc and update the currently-attended one to match

                |#
                 
                 (set-attributes obj-dmo `(screen-pos ,vis-loc-chunk))
                 
                 (schedule-mod-buffer-chunk 'visual-location 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec (ps-ptr loc-dmo)))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 15)
                )
                
                
                ((null vis-obj-chunk) ;; Don't know about the vis-loc buffer
            ;     (pprint 'case-7)


                 #|
                  Just put the new object in place 
                 |#
                 
                 (schedule-set-buffer-chunk 'visual (ps-ptr obj-dmo) 0
                                            :module :vision :output 'high :requested nil
                                            :priority 14)
                 (schedule-event-relative 0 #'update-tracking-obj-chunk
                                          :module :vision
                                          :destination :vision
                                          :priority 13
                                          :output nil)
                 )
                ((eq vis-obj-chunk (ps-ptr old-obj-dmo)) ;; Don't know about vis-loc buffer
                 
             ;    (pprint 'case-8)


                 #|
                  Just modify the object chunk
                 |#
                 
                 
                 (schedule-mod-buffer-chunk 'visual 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec (ps-ptr obj-dmo)))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 14)
                 
                 )
                
                (t ;; Don't do anything 
              ;   (pprint 'case-9)


                 ))))))
  nil)

;;; REMOVE-TRACKING      [Method]
;;; Date        : 97.05.15
;;; Description : When tracking stops, the slots for tracked objects need to
;;;             : to be cleared, and the ACT hook functions need to be cleared.

(defgeneric remove-tracking (vis-mod)
  (:documentation  "Clears out all the tracking stuff"))

(defmethod remove-tracking ((vis-mod vision-module))
  (setf (tracked-obj vis-mod) nil)
  ;(awhen (tracked-obj-lastloc vis-mod)
  ;  (set-clof vis-mod it))
  (setf (tracked-obj-lastloc vis-mod) nil)
  (change-state vis-mod :exec 'FREE))


;;; UPDATE-TRACKING      [Function]
;;; Date        : 97.05.15
;;; Description : To be called by the user to indicate movement.

(defun update-tracking ()
  "Call the Vision Module's tracking update method."
  (update-tracking-mth (get-module :vision)))


(defgeneric view-loc (view)
  (:documentation  "Return the location of the center of <view> as #(x y)."))

(defmethod view-loc (x)
  (declare (ignore x))
  (error "No method defined for VIEW-LOC."))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Icon Feature class and its children
;;;; ---------------------------------------------------------------------- ;;;;

(defgeneric print-icon-feature (feat)
  (:documentation  "Print out an ASCII representation of the icon."))

(defmethod print-icon-feature ((feat icon-feature))
  (format t "~%(~3D ~3D)~11T~A~17T~A~32T~S~50T~A~66T~A"
    (screen-x feat) (screen-y feat) 
    (feat-attended feat ;(vis-m *mp*))
                   (get-module :vision))
    (kind feat)  
    (val feat)
    (color feat)
    (dmo-id feat) 
    ))
  
(defmethod print-object ((feat icon-feature) stream)
  (print-unreadable-object (feat stream :type t)
    (princ (dmo-id feat) stream)))



(defgeneric enter-into-visicon (feat time)
  (:documentation  "Enter at icon feature into the visicon at the time specified."))

(defmethod enter-into-visicon ((feat icon-feature) (time number))
  (unless (dmo-id feat)
    (setf (dmo-id feat) (new-name-fct (kind feat))))
  (setf (tstamp feat) time)
  feat)


;;; XY-LOC      [Method]
;;; Date        : 97.02.09
;;; Description : Return the location of a feature as a list.

(defgeneric xy-loc (feat)
  (:documentation  "Return the XY location of a feature as a vector."))

(defmethod xy-loc ((feat icon-feature))
  (vector (screen-x feat) (screen-y feat)))

;;; FEAT-TO-DMO      [Method]
;;; Date        : 98.05.28
;;; Description : From an icon feature, create a chunk.

(defgeneric feat-to-dmo (feat)
  (:documentation  "Build a chunk for an icon feature"))

(defmethod feat-to-dmo ((feat icon-feature))
  (setf (attended-p feat) t)
  (make-dme (dmo-id feat) (kind feat)
            `(screen-pos ,(id (xy-to-dmo (xy-loc feat) t))
                         value ,(val feat)
                         color ,(color feat)
                         height ,(height feat)
                         width ,(width feat)
                         )
            :obj (screen-obj feat)
            :where :external))

(defmethod feat-to-dmo :around ((feat cursor-feature))
  (let ((the-dmo (call-next-method)))
    (setf (pm-obj the-dmo) :CURSOR)
    the-dmo))


(defgeneric feat-named (vis-mod id)
  (:documentation  "Returns the icon feature with the given <id>, if any."))

(defmethod feat-named ((vis-mod vision-module) id)
  (first (member id (visicon vis-mod) :key #'dmo-id)))



;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Feature specifications, used to find features in the icon
;;;; ---------------------------------------------------------------------- ;;;;

;;; MATCH-SPEC-P      [Method]
;;; Date        : 98.08.11
;;; Description : Determining a match is based on testing for attribute
;;;             : matches or, in the case of coordinates, acceptable
;;;             : values.  So, check everything and return NIL on the
;;;             : first failure.  If nothing fails to match, return T.

;;;DAN
;;; defgeneric exists in general-pm
;(defgeneric match-spec-p (spec feat)
;  (:documentation  "Returns T when <spec> and <feat> match."))

#|
(defmethod match-spec-p ((spec icon-feature) (feat icon-feature))
  (cond ((not (test-attended spec feat)) nil) 
        ;((not (attr-exact-match-p spec feat 'attended-p)) nil)
        ((not (attr-exact-match-p spec feat 'kind)) nil)
        ((not (attr-exact-match-p spec feat 'val)) nil)
        ((not (attr-exact-match-p spec feat 'color)) nil)
        ((and (numberp (size spec))
              (not (attr-exact-match-p spec feat 'size))) nil)
        ((and (numberp (screen-x spec))
              (not (attr-exact-match-p spec feat 'screen-x))) nil)
        ((and (numberp (screen-y spec))
              (not (attr-exact-match-p spec feat 'screen-y))) nil)
        ((and (numberp (distance spec))
              (not (attr-exact-match-p spec feat 'distance))) nil)
        ((and (functionp (size spec))
              (not (funcall (size spec) (size feat)))) nil)
        ((and (functionp (screen-x spec))
              (not (funcall (screen-x spec) (screen-x feat)))) nil)
        ((and (functionp (screen-y spec))
              (not (funcall (screen-y spec) (screen-y feat)))) nil)
        ((and (functionp (distance spec))
              (not (funcall (distance spec) (distance feat)))) nil)
        
        ((and (or (numberp (userprop1 spec)) (symbolp (userprop1 spec)))
              (not (attr-exact-match-p spec feat 'userprop1))) nil)
        ((and (functionp (userprop1 spec)) (numberp (userprop1 feat))
              (not (funcall (userprop1 spec) (userprop1 feat)))) nil)
        
        (t t)))
|#

(defmethod match-spec-p ((spec feature-spec) (feat icon-feature))
  (when (test-attended spec feat)
    (dovector (slotname (check-slots spec) t)
      (when (not (slotval-match? (slot-value spec slotname)
                                 (slot-value feat slotname)))
        (return-from match-spec-p nil)))))


(defun slotval-match? (criterion value)
  "Does the VALUE meet the CRITERION?"
  (cond ((eql criterion :ignore) t)
        ((symbolp criterion) (eq criterion value))
        ((numberp criterion) (and (numberp value)
                                  (= criterion value)))
        ((stringp criterion) (and (stringp value)
                                  (string-equal criterion value)))
        ((functionp criterion) (funcall criterion value))
        (t nil)))



(defgeneric matching-feats (spec feat-lst)
  (:documentation  "Return a list of features matching a given feature spec."))

(defmethod matching-feats ((spec feature-spec) (feat-lis list))
  (remove-if-not #'(lambda (f) (match-spec-p spec f)) feat-lis))


;;; ATTR-EXACT-MATCH-P      [Method]
;;; Date        : 98.08.11
;;; Description : If the spec says to ignore that attribute, then return T
;;;             : becuase everything matches a don't care.  Otherwise, the
;;;             : spec and the feature values must be equal.

(defgeneric attr-exact-match-p (spec feat slotname)
  (:documentation  "Returns T when <spec> and <feat> match on attribute <slotname> (or if <spec> says :IGNORE)."))

(defmethod attr-exact-match-p ((spec icon-feature) (feat icon-feature) slotname)
  (cond ((eq (slot-value spec slotname) :IGNORE) T)
        ((equal (slot-value spec slotname) (slot-value feat slotname)))))


;;; SLOT-MATCH-P      [Method]
;;; Date        : 01.07.27
;;; Description : One should not be able to test TEXT features for their 
;;;             : value, so this specializer should short-circuit that.

(defmethod slot-match-p ((ts spec) (obj text-feature) 
                           (slotname (eql 'value)))
  t)


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Messing with features:  Doing icon checking, finding, and listing.
;;;; ---------------------------------------------------------------------- ;;;;

;;; FILL-DEFAULT-DIMENSIONS      [Method]
;;; Date        : 99.04.02
;;; Description : The base methods for most MCL views don't set the height or
;;;             : width attributes of the features they generate, nor their
;;;             : size.  Set that up if necessary.

(defgeneric fill-default-dimensions (feat)
  (:documentation  "Fill in the width, height, and size of an icon feature."))

(defmethod fill-default-dimensions ((feat icon-feature))
  (aif (simple-size feat)
    (setf (size feat) it)
    (if (null (screen-obj feat))
      (setf (size feat) 1.0)              ; should be default size, eh
      (progn
        (unless (width feat) (setf (width feat) (width (screen-obj feat))))
        (unless (height feat) (setf (height feat) (height (screen-obj feat))))
        (setf (size feat) (simple-size feat))))))


;;; APPROACH-WIDTH      [Method]
;;; Date        : 99.03.30
;;; Description : Remember in high school when someone asked in trig why we
;;;             : needed to know this crap?  Well, this is why.  I'm pretty
;;;             : sure this is all the right trig, but there could be some
;;;             : missed math in here.

(defmethod approach-width ((feat icon-feature) (theta number))
  (let* ((x (width feat))
         (y (height feat))
         (critical-theta (atan y x))
         (theta (abs theta))
         (ret-width nil))
    (when (> theta (/ pi 2))
      (setf theta (- pi theta)))
    (setf ret-width
          (cond ((= theta 0) x)
                ((= theta (/ pi 2)) y)
                ((= theta critical-theta) (sqrt (+ (* y y) (* x x))))
                ((< theta critical-theta) (/ x (cos theta)))
                (t (/ y (cos (- (/ pi 2) theta))))))
    (pm-pixels-to-angle ret-width)))


(defgeneric simple-size (feat)
  (:documentation  "If there are values for the height and width, convert to visual angle and multiply."))

(defmethod simple-size ((feat icon-feature))
  (when (and (width feat) (height feat))
    (* 0.01 (round
             (* (pm-pixels-to-angle (width feat))
                (pm-pixels-to-angle (height feat)))
             0.01))))


;;; CONSTRUCT-LOCATION      [Method]
;;; Date        : 98.08.11, last delta 99.06.18
;;; Description : Building a location is pretty easy--if one exists already,
;;;             : use that.  If not, make one.  Note the commented out part
;;;             : may get used later--the idea is to store the feature and
;;;             : the spec used to find a given location, so that if attention
;;;             : is moved to that location and the feature moves a little
;;;             : bit, that same feature can be found.  And if that feature is
;;;             : gone, we can look for a new match to the spec.

(defgeneric construct-location (vis-mod feat spec)
  (:documentation  "Find or build a DMO based on a feature and the spec used to generate that feature."))


(defmethod construct-location ((vis-mod vision-module) (feat icon-feature) 
                                  (spec icon-feature))
  (let ((loc-dmo (find-loc-dmo (xy-loc feat))))
    ;; make sure we have one
    (unless loc-dmo
      (setf loc-dmo (xy-to-dmo (xy-loc feat) nil)))
    ;; fill in from the feature
    (setf loc-dmo (featinfo->loc vis-mod feat loc-dmo))
    ;; maybe print
    (when (print-view-p vis-mod)
      (pm-output nil "Vision found ~S" (id loc-dmo)))
    ;(format t "~%Constructing location ~S" (id loc-dmo)))
    ;; record that this loc was found with this spec
    (setf (gethash (id loc-dmo) (found-locs vis-mod))
          (make-instance 'found-spec :feat feat :spec spec))
    ;; return the DMO in a format the PS can understand
    (dmo-to-psdme loc-dmo)))


(defgeneric featinfo->loc (vis-mod feat the-dmo)
  (:documentation "Given a feature and a location DMO, fill in the DMO with information from the feature."))

(defmethod featinfo->loc ((vm vision-module) (feat icon-feature)
                             (the-dmo dmo))
  (set-attributes the-dmo `(color ,(color feat) 
                                  ;  attended ,(attended-p feat)
                                  kind ,(kind feat)
                                  value ,(val feat)
                                  size ,(size feat)
                                  distance ,(distance feat)
                                  userprop1 ,(userprop1 feat)
                                  userprop2 ,(userprop2 feat)
                                  userprop3 ,(userprop3 feat)
                                  userprop4 ,(userprop4 feat)
                                  ))
  the-dmo)


(defmethod featinfo->loc ((vm vision-module) (feat text-feature)
                             (the-dmo dmo))
  (let ((the-dmo (call-next-method)))
    (set-attributes the-dmo '(value TEXT))
    the-dmo))

;;; FEAT=      [Method]
;;; Description : Two features are equal if their non-ID attributes are
;;;             : all equal.

(defgeneric feat= (feat1 feat2)
  (:documentation  "Compares two icon features to determine if they're functionally equal."))

(defmethod feat= ((feat1 icon-feature) (feat2 icon-feature))
  (declare (optimize (speed 3)))
  (cond ((neq (type-of feat1) (type-of feat2)) nil)
        ((not (= (screen-x feat1) (screen-x feat2))) nil)
        ((not (= (screen-y feat1) (screen-y feat2))) nil)
        ((not (equal (width feat1) (width feat2))) nil)
        ((not (equal (height feat1) (height feat2))) nil)
        ((neq (kind feat1) (kind feat2)) nil)
        ((not (equal (val feat1) (val feat2))) nil)
        ((neq (color feat1) (color feat2)) nil)
        (t (typed-feat= feat1 feat2))))

(defgeneric typed-feat= (feat1 feat2)
  (:documentation  "Very specific comparison of special properties of icon features.  T is returned if equal."))

;; need one for line features
(defmethod typed-feat= ((feat1 line-feature) (feat2 line-feature))
  (declare (optimize (speed 3)))
  (cond ((not (= (end1-x feat1) (end1-x feat2))) nil)
        ((not (= (end1-y feat1) (end1-y feat2))) nil)
        ((not (= (end2-y feat1) (end2-y feat2))) nil)
        ((not (= (end2-x feat1) (end2-x feat2))) nil)
        (t t)))

;; with no other info, assume they're equal
(defmethod typed-feat= ((feat1 icon-feature) (feat2 icon-feature))
  t)


(defun feature-locs (feat-lst)
  "Given a list of features, return a list of unique feature locations."
  (declare (list feat-lst))
  (remove-duplicates (mapcar #'xy-loc feat-lst) :test #'equal))


(defun feat-max-x (feat-lst)
  "Returns list of features with maximum x value"
  (declare (list feat-lst))
  (objs-max-slotval feat-lst 'screen-x))


(defun feat-min-x (feat-lst)
  "Returns list of features with minimum x value"
  (declare (list feat-lst))
  (objs-min-slotval feat-lst 'screen-x))


(defun feat-max-y (feat-lst)
  "Returns list of features with maximum y value"
  (declare (list feat-lst))
  (objs-max-slotval feat-lst 'screen-y))


(defun feat-min-y (feat-lst)
  "Returns list of features with minimum y value"
  (declare (list feat-lst))
  (objs-min-slotval feat-lst 'screen-y))


(defun feat-match-x (feat-lst x)
  "Returns features that match a given x value"
  (declare (list feat-lst) (fixnum x))
  (objs-match-slotval feat-lst 'screen-x x))


(defun feat-match-y (feat-lst y)
  "Returns features that match a given y value"
  (declare (list feat-lst) (fixnum y))
  (objs-match-slotval feat-lst 'screen-y y))


(defgeneric feat-match-xy (feat-lst loc)
  (:documentation  "Returns the the subset of the list that match the XY location."))

(defmethod feat-match-xy ((feat-lst list) (loc vector))
  (let ((outlis nil))
    (dolist (feat feat-lst outlis)
      (if (and (= (screen-x feat) (px loc)) 
               (= (screen-y feat) (py loc)))
        (push feat outlis)))))


(defun nearest-feat (feat-list loc)
  "Returns list of features nearest to a given location"
  (declare (list feat-lis))
  (unless (vectorp loc)
    (setf loc (dmo-to-xy loc)))
  (when feat-list
    (let ((best (dist (xy-loc (first feat-list)) loc))
          (outlis (list (first feat-list)))
          (the-dist nil))
      (dolist (feat (rest feat-list) outlis)
        (setf the-dist (dist (xy-loc feat) loc))
        (cond ((= the-dist best) (push feat outlis))
              ((< the-dist best) (setf best the-dist)
               (setf outlis (list feat))))))))



;;; DELETE-FEATURES      [Method]
;;; Date        : 97.02.27
;;; Description : Removes features from the icon.  If the scale is SCREEN,
;;;             : then remove all features.  If the scale is WORD, then 
;;;             : find the contiguous locations and remove all features at
;;;             : those locations.  Otherwise just remove all features at
;;;             : the specified location.

(defgeneric delete-features (vis-mod loc scale)
  (:documentation  "Removes features at a specified location"))

(defmethod delete-features ((vis-mod vision-module) loc scale)
  (declare (symbol loc))
  (case scale
    (screen (setf (visicon vis-mod) nil))
    (word 
     (let ((feat-lis (visicon vis-mod))
           (loc-lis (contiguous-locs loc (feat-match-y (visicon vis-mod) 
                                                         (py loc)))))
       (dolist (the-loc loc-lis)
         (setf feat-lis
               (remove-if #'(lambda (feat)
                              (equal the-loc (xy-loc feat)))
                          feat-lis)))
       (setf (visicon vis-mod) feat-lis)))
    (otherwise 
     (setf (visicon vis-mod)
           (remove-if #'(lambda (feat) (equal loc (xy-loc feat)))
                      (visicon vis-mod))))))


;;; BUILD-STRING-FEATS      [Method]
;;; Date        : 99.03.30
;;; Description : In order to build all the features for a string, there is a
;;;             : great deal of stuff that is needed from the interface beyond
;;;             : just the string itself.  Need to know the y coordinate, the
;;;             : starting x coordinate, the line height, the associated
;;;             : screen object, and some way of determining the width (in
;;;             : pixels) of a string.
;;;             : 
;;;             : There are two ways to do it, with and without optimizing. 
;;;             : With optimizing it's easy, just accumulate words.  When
;;;             : optimzing is off, though, have to walk the string
;;;             : character-by-character and build features for each one.

(defgeneric build-string-feats (vis-mod &key text start-x y-pos 
                                      width-fct height obj)
  (:documentation  "Build a list of ICON-FEATURES representing a string with the given geometry."))

(defmethod build-string-feats ((vis-mod vision-module) &key text start-x 
                              y-pos width-fct height obj)
  (declare (fixnum start-x y-pos)  (function width-fct) (string text)
           (number height))
  (when (not (and text start-x y-pos width-fct height))
    (error "NIL passed to BUILD-STRING-FEATS."))
  (unless (equal text "")
    (let ((curr-x start-x)
          (f-accum nil)
          (spc-wdth (funcall width-fct " "))
          (curr-width nil))
      (if (optimize-p vis-mod)
        ;; if optimizing is on, then carve the string into words (strings)
        ;; and space runs (numbers)
        (dolist (word (chop-string text) (nreverse f-accum))
          (when (stringp word)
            (setf curr-width (funcall width-fct word))
            (push (make-instance 'text-feature
                    :y y-pos :screen-obj obj :height height
                    :value (string-downcase word)
                    :width curr-width
                    :x (+ curr-x (round curr-width 2))) f-accum))
          (incf curr-x (if (stringp word) curr-width (* word spc-wdth))))
        ;; if not optimizing, then blast it character-by-character
        (let ((char nil))
          (dotimes (idx (length text) (nreverse (flatten f-accum)))
            (setf char (char text idx))
            (setf curr-width (funcall width-fct (subseq text idx (1+ idx))))
            (cond ((alphanumericp char)
                   (push (char-to-features vis-mod (char-upcase char)
                                           curr-x
                                           (- (+ curr-x curr-width) 1)
                                           y-pos
                                           height obj) f-accum))
                  ((and (graphic-char-p char)
                        (not (char-equal char #\space)))
                   (push (make-instance 'text-feature
                           :y y-pos :screen-obj obj :height height
                           :value (mkstr char)
                           :width curr-width
                           :x (+ curr-x (round curr-width 2))) f-accum))
                  (t nil))
            (incf curr-x curr-width)))))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Those wacky LED features for letters.
;;;; ---------------------------------------------------------------------- ;;;;

;;; CHAR-TO-FEATURES      [Method]
;;; Date        : 99.03.30
;;; Description : For each character, there will usually be many of those
;;;             : CHAR-PRIMITIVE features.  Grab the list of features 
;;;             : associated with a character, and build 'em.

(defgeneric char-to-features (vis-mod char left right y height obj)
  (:documentation  "Returns a list of basic icon-feature objects for a characer"))

(defmethod char-to-features ((vis-mod vision-module) (char character) 
                                (left number) (right number) (y number) 
                                (height number) obj)
  (let ((xpos (+ left (round (- right left) 2)))
        (width (1+ (- right left)))
        (features (pairlis
                     (getfeats (active-cfs vis-mod) char)
                     (get-icon-feats (active-cfs vis-mod) char)))
        (accum nil))
    (dolist (feats features accum)
      (push (make-instance 'char-primitive-feature
              :x xpos :y y :width width :height height 
              :left left :right right :screen-obj obj
              :true-feat (first feats) :value (rest feats))
            accum))))


;;; ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : To synthesize a word, we need to know which locations have
;;;             : LED-LINE features at adjoining locations.  However, 
;;;             : because letter width is variable, the icon has to be
;;;             : searched for LED-LINE locations that have features that
;;;             : are near the boundaries.  Also, because we need to know the
;;;             : width of the word, return the min and max x values, too.

(defgeneric adjoining-led-locs (vis-mod loc)
  (:documentation  "Return a list of locations adjoining <loc>, as well as the max and min x locs."))

(defmethod adjoining-led-locs ((vis-mod vision-module) (loc vector))
  (let ((feat-ls (feat-match-xy (visicon vis-mod) loc))
        (feat nil))
    (while (and (null feat) feat-ls)
      (setf feat (pop feat-ls))
      (unless (typep feat 'char-primitive-feature)
        (setf feat nil)))
    (when feat
      (setf feat-ls (feat-match-y (visicon vis-mod) (screen-y feat)))
      (multiple-value-bind 
        (lowlocs xmin) (left-adjoining-led-locs feat-ls (left-edge feat) nil)
        (multiple-value-bind
          (hilocs xmax) (right-adjoining-led-locs feat-ls (right-edge feat) nil)
          (values
           (append lowlocs (list loc) hilocs)
           xmin xmax))))))


;;; RIGHT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go right and accumulate locations that share
;;;             : the boundary.

(defgeneric right-adjoining-led-locs (feat-ls x accum)
  (:documentation  "Return a list of all the right-adjoining locs with led features and the min x."))

(defmethod right-adjoining-led-locs ((feat-ls list) x accum)
  (dolist (feat feat-ls)
    (when (typep feat 'char-primitive-feature)
      (when (> 1.5 (abs (- (left-edge feat) x)))
        (return-from right-adjoining-led-locs
          (right-adjoining-led-locs feat-ls (right-edge feat)
                                    (append accum (list (xy-loc feat))))))))
  (values accum x))


;;; LEFT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go left and accumulate locations that share
;;;             : the boundary.

(defgeneric left-adjoining-led-locs (feat-ls x accum)
  (:documentation  "Return a list of all the left-adjoining locs with led features and the max x."))

(defmethod left-adjoining-led-locs ((feat-ls list) x accum)
  (dolist (feat feat-ls)
    (when (typep feat 'char-primitive-feature)
      (when (> 1.5 (abs (- (right-edge feat) x)))
        (return-from left-adjoining-led-locs
          (left-adjoining-led-locs feat-ls (left-edge feat)
                                   (append (list (xy-loc feat)) accum))))))
  (values accum x))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Misc utils
;;;; 

(defgeneric set-clof (vis-mod new-lof)
  (:documentation "Updates vision module's current location of focus."))

(defmethod set-clof ((vis-mod vision-module) (new-lof vector))
  (setf (current-lof vis-mod) new-lof)
  (when (trace-attn-p vis-mod)
    
    ;(push (cons (pm-time) new-lof) (attn-trace vis-mod))
    ;;; DAN
    ;;; adjust the time reference function
    
    (push (cons (mp-time) new-lof) (attn-trace vis-mod))))


;;; non-MCL lisps don't define COPY-INSTANCE, so we need one just for 
;;; feature-spec types.

#-:mcl
(defmethod copy-instance ((spec feature-spec))
  (make-instance 'feature-spec
    :x (screen-x spec) :y (screen-y spec) :nearest (nearest spec) 
    :attended-p (attended-p spec) :value (val spec) :color (color spec)
    :kind (kind spec) :size (size spec) :distance (distance spec)
    :userprop1 (userprop1 spec) :userprop2 (userprop2 spec) 
    :userprop3 (userprop3 spec) :userprop4 (userprop4 spec)
    ))

;;; CHOP-STRING      [Function]
;;; Date        : 97.07.02
;;; Description : Chops up a string according to what's in the string.  
;;;             : The idea is to keep words intact, blocks of nonword characters
;;;             : intact, and know the spacing between words.  Given a string,
;;;             : this will return a list with that kind of info in it.
;;;             : Example:  (chop-string "==foo==   bar") will return:
;;;             : ("==" "foo" "==" 3 "foo").  The 3 in the list represents
;;;             : blanks, and notice how the "==foo==" was carved into three
;;;             : elements.

(defun chop-string (str)
  (declare (string str))
  (let* ((oldstate (char->state (char str 0)))
         (state nil)
         (chr nil)
         (wrd "")
         (cnt 0)
         (accum nil))
    (dotimes (i (length str))
      (setf chr (char str i))
      (setf state (char->state chr))
      (cond
       ;; if we're accumulating chars and the new char matches our state,
       ;; just concat the char
       ((or (and (eq state :WORD) (eq oldstate :WORD))
            (and (eq state :MISC) (eq oldstate :MISC)))
        (setf wrd (mkstr wrd chr))
        (setf cnt 0))
       ;; If we get a state change that finishes a word, then grab the
       ;; word.
       ((and (or (eq oldstate :WORD) (eq oldstate :MISC))
             (not (eq oldstate state)))
        (push wrd accum)
        (if (not (eq state :SPACE))
          (setf wrd (mkstr chr))
          (setf wrd "")))
       ;; when switching from spaces to words, push the counter and start
       ;; the word
       ((and (eq oldstate :SPACE) (not (eq state oldstate)))
        (push (1+ cnt) accum)
        (setf wrd (mkstr wrd chr)))
       ;; from whitespace to whitespace, inc the counter
       (t (incf cnt)))
      (setf oldstate state))
    (when (not (string= wrd ""))
      (push wrd accum))
    (setf accum (nreverse accum))
    (when (numberp (first accum))
      (decf (first accum)))
    accum))


(defun char->state (char)
  "Given a character, return :WORD, :SPACE, or :MISC"
  (declare (character char))
  (cond ((alphanumericp char) :WORD)
        ((whitespace-p char) :SPACE)
        (t :MISC)))


(defun whitespace-p (char)
  "Returns T if <char> is a whitespace character (non-printing or space)"
  (declare (character char))
  (or (not (graphic-char-p char))
      (eq char #\Space)))


(defun break-by-space (str)
  "Return <str> broken into a list of strings. Space is the separator."
  (declare (string str))
  (let ((ans nil)
        (pos nil)
        (start 0))
    (loop
      (setf pos (position #\space str :start start))
      (cond (pos 
             (push (subseq str start pos) ans)
             (setf start (1+ pos)))
            (t
             (push (subseq str start (length str)) ans)
             (return))))
    (setf ans (remove "" ans :test #'string=))
    (reverse ans)))

;;; CONTIGUOUS-LOCS      [Function]
;;; Date        : 97.02.07
;;; Description : Given a location and a feature list, return a list of all the 
;;;             : contiguous locations.  "Contiguous" only in the X direction,
;;;             : in this case.

(defun contiguous-locs (loc feat-lis)
  "Given a location and a feature list, return a list of all the contiguous locations"
  (declare (vector loc) (list feat-lis))
  (let ((workloc (copy-list loc))
        (work-lis nil))
    (while (feat-match-xy feat-lis workloc)
      (push (copy-seq workloc) work-lis)
      (incf (px workloc)))
    (setf workloc (copy-seq loc))
    (decf (px workloc))
    (while (feat-match-xy feat-lis workloc)
      (push (copy-seq workloc) work-lis)
      (decf (px workloc)))
    (sort work-lis #'< :key #'first)))


(defun phrase-accum (lis)
  "Accumulate a list of words into a phrase."
  (let ((accum (first lis)))
    (dolist (item (rest lis) accum)
      (setf accum (mkstr accum " " item)))))


(defun word-accum (lis)
  "Accumulate a list of letters into a word, and downcase it."
  (let ((accum (first lis)))
    (dolist (item (rest lis) (string-downcase accum))
      (setf accum (mkstr accum item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN
;;; Moved process-display and update-cursor-feat since they
;;; are methods on the vision-module class as well

(defmethod process-display ((devin device-interface) 
                            (vis-mod vision-module) &optional (clear nil))
  "Build a new visicon and initiate any buffer stuffing that may result"

  (let ((tmpicon nil)
        (time 
         ;(pm-time)
         ;;; DAN 
         ;;; updated time reference
         (mp-time)))
    ;; make sure a device is actually installed
    (unless (device devin)
      (pm-warning "Cannot process display--no device is installed.")
      (return-from process-display nil))
    
    ;; make sure it's safe to change things right now
    ;; and save the clear status if not for future use
    
    (unless (zerop (locks devin))
      (push clear (pending-procs devin))
      (return-from process-display nil))
    
    ;; build the temp visicon
    (setf tmpicon (flatten (build-features-for (device devin) vis-mod)))
    (when (with-cursor-p devin)
      (awhen (cursor-to-feature (device devin))
        (push  it tmpicon)))
    ;; if clear, then make the visicon the temp icon, if not we have to
    ;; check the new vs. the old icon
    (if clear
      (setf (visicon vis-mod) (mapcar #'(lambda (f)
                                          (enter-into-visicon f time))
                                      tmpicon))
      (let ((oldicon (visicon vis-mod)))
        (setf (visicon vis-mod) nil)
        (dolist (feat tmpicon)
          (aif (member feat oldicon :test #'feat=)
            (progn
              (push (checknew (first it) vis-mod) (visicon vis-mod))
              (setf oldicon (delete (first it) oldicon)))
            (push (enter-into-visicon feat time) (visicon vis-mod))))
        (setf (visicon vis-mod) (nreverse (visicon vis-mod)))))
    ;; other bookkeeping 
    
    ;; Now tracking must be explicitly updated instead of
    ;; happening in a constant polling loop.
    ;; Do it before the visicon-update so that issues with
    ;; buffer stuffing happen in the proper order i.e. tracking
    ;; has a higher priority than the default vis-loc stuffing.
    
    (when (tracked-obj vis-mod)
      (update-tracking-mth vis-mod))
    
    (synch-mouse devin)
    (visicon-update vis-mod)
    ))


(defmethod update-cursor-feat ((devin device-interface) 
                                  (vis-mod vision-module))
  (when (and (with-cursor-p devin) (device devin))
    (let ((new-pos (get-mouse-coordinates (device devin)))
          (cur-crsr (first (member 'CURSOR (visicon vis-mod) :key #'kind))))
      (if (null cur-crsr)
        (awhen (cursor-to-feature (device devin))
               ;(length (push (enter-into-visicon it (pm-time)) (visicon vis-mod)))
               ;;; DAN
               ;;; updated the time reference
               (length (push (enter-into-visicon it (mp-time)) (visicon vis-mod))))
        (when (not (vpt= new-pos (xy-loc cur-crsr)))
          (setf (attended-p cur-crsr) nil)
          (setf (screen-x cur-crsr) (px new-pos))
          (setf (screen-y cur-crsr) (py new-pos)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN 
;;; define the module interface code

;;; creation requires just making a new instance

(defun create-vision-module (model-name)
  (declare (ignore model-name))
  (make-instance 'vision-module))


;;; call the reset method and then create the chunk-types and chunks

(defun reset-vision-module (instance)
  (reset-pm-module instance)
  
  (chunk-type visual-object screen-pos value status color height width)
  (chunk-type abstract-object value line-pos bin-pos)
  (chunk-type (abstract-letter (:include abstract-object)))
  (chunk-type (abstract-number (:include abstract-object)))
  (chunk-type (text (:include visual-object)))
  (chunk-type (empty-space (:include visual-object)))
  
  (chunk-type (line (:include visual-object)) other-pos end1-x end1-y end2-x end2-y)
  (chunk-type (oval (:include visual-object)))
  (chunk-type (cursor (:include visual-object)))
  
  (chunk-type (phrase! (:include visual-object)) objects words)
  (chunk-type visual-location screen-x screen-y distance kind color 
              value size nearest
              ;;; DAN have to add the objects slot because it's used in the
              ;;; make-dme call
              objects userprop1 userprop2 userprop3 userprop4)
                                        ; mdb added a3
  (chunk-type vision-command)
  (unless (chunk-type-p pm-constant)
    (chunk-type pm-constant))
  (chunk-type color)
  
  
  (chunk-type (move-attention (:include vision-command)) screen-pos scale)
  
  (chunk-type (start-tracking (:include vision-command)))
  
  (chunk-type (assign-finst (:include vision-command)) object location)
  
  (unless (chunk-type-p clear)
    (chunk-type clear))
  
  (define-chunks 
    (lowest isa pm-constant)
    (highest isa pm-constant)
    (greater-than-current isa pm-constant)
    (less-than-current isa pm-constant)
    (current isa pm-constant)
    (external isa pm-constant)
    (internal isa pm-constant)
    (find-location isa vision-command)
    (move-attention isa vision-command)
    (assign-finst isa vision-command)
    (start-tracking isa vision-command)
    
    (black isa color)
    (red isa color)
    (blue isa color)
    (green isa color)
    (white isa color)
    (magenta isa color)
    (yellow isa color)
    (cyan isa color)
    (dark-green isa color)
    (dark-red isa color)
    (dark-cyan isa color)
    (dark-blue isa color)
    (dark-magenta isa color)
    (dark-yellow isa color)
    (light-gray isa color)
    (dark-gray isa color)
        
    (text isa chunk)
    (box isa chunk)
    (line isa chunk)
    (oval isa chunk)
    
    (new isa chunk)
    (clear isa chunk)))


;;; Return nil or non-nil based on the buffer, slot and value

(defun query-vision-module (vis-mod buffer slot value)
  (case buffer
    (visual
     (if (and (eq slot 'state) (eq value 'error))
       (attend-failure vis-mod)
       (generic-state-query vis-mod buffer slot value)))
    (visual-location
     (case slot
       (state
        (case value
          (busy nil) ;; visual-location requests are always free
          (free t)
          (error (loc-failure vis-mod))
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (attended
        (let* ((vis-loc-chunk (buffer-read 'visual-location))
               (found (and vis-loc-chunk 
                           (gethash (chunk-copied-from-fct vis-loc-chunk) 
                               (found-locs vis-mod)))))
          (when (and vis-loc-chunk found)
            (let ((feat (feat found)))
                 
                 (and feat
                      (test-attended  
                       (make-instance 'icon-feature :attended-p value)
                       feat)))))))))) 



#| most of this is now handled in GENERIC-STATE-QUERY 
     (case slot
       (state
        (case value
          (busy
           (eq (mode-s vis-mod) 'busy))
          (free
           (eq (mode-s vis-mod) 'free))
          (error
              (attend-failure vis-mod))
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (buffer
        (case value
          (stuffed nil) ;; for now don't indicate stuffing of visual, but probably should for tracking or re-encoding
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))       
       (modality
         (case value
          (busy
           (eq (mode-s vis-mod) 'busy))
          (free
           (eq (mode-s vis-mod) 'free))
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (execution
        (case value
          (busy
           (eq (exec-s vis-mod) 'busy))
          (free
           (eq (exec-s vis-mod) 'free))
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (preparation
        (case value
          (busy
           (eq (prep-s vis-mod) 'busy))
          (free
           (eq (prep-s vis-mod) 'free))
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (processor
        (case value
          (busy
           (eq (proc-s vis-mod) 'busy))
          (free
           (eq (proc-s vis-mod) 'free))
          (t (pm-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))))
|#


(defmethod warn-vision ((vis-mod vision-module) buffer-name chunk-type)
  (declare (ignore chunk-type))
  (when (and (eq buffer-name 'visual)
             (null (visual-lock vis-mod)))
    (lock-device (current-device-interface))
    (setf (visual-lock vis-mod) t)))
                

;;; Take all of the requests through the buffers and route them to the correct method/function.
;;; In general, a request should schedule its actions so that it shows in the trace
;;; and is available to the hook functions for recording. 

(defmethod pm-module-request ((vis-mod vision-module) buffer-name 
                                 chunk-spec)
  (case buffer-name
    (visual
     (when (visual-lock vis-mod)
       (setf (visual-lock vis-mod) nil)
       (schedule-event-relative 0 'unlock-device 
                                :module :vision
                                :destination :device
                                :priority :min
                                :output nil
                                :maintenance t))
       
     (case (chunk-spec-chunk-type chunk-spec)
       (clear ;; replaces the implicit clear from -visual
        (schedule-event-relative 0 'clear :module :vision :destination :vision
                                 :output 'low)
        ; could also be: (schedule-event-relative 0 'clear :module :vision :params (list vis-mod))
        )
       (start-tracking
        (schedule-event-relative 0 'start-tracking 
                                 :destination :vision
                                 :details "Start-tracking"
                                 :module :vision
                                 :output 'medium))
       (assign-finst
        (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                         (verify-single-explicit-value 
                          (chunk-spec-slot-spec chunk-spec 'object) 
                          :vision 'assign-finst 'object)
                         nil))
              (location (if (slot-in-chunk-spec-p chunk-spec 'location)
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec 'location) 
                             :vision 'assign-finst 'location)
                            nil)))
          
          (when (or object location)
            (schedule-event-relative 0 'assign-finst 
                                     :params (list vis-mod :object object 
                                                   :location location)
                                     :module :vision
                                     :output 'medium))))
       
       
       (visual-object
        (print-warning "Move attention requests are now done with an isa move-attention"))
       
       
       (move-attention
        (let ((sp (if (slot-in-chunk-spec-p chunk-spec 'screen-pos) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'screen-pos) 
                      :vision 'visual-object 'screen-pos)
                     nil))
              (scale (if (slot-in-chunk-spec-p chunk-spec 'scale)
                        (verify-single-explicit-value 
                         (chunk-spec-slot-spec chunk-spec 'scale) 
                         :vision 'visual-object 'scale)
                        nil)))
          
          (when sp
            (schedule-event-relative 0 'move-attention 
                                     :params 
                                     (list vis-mod :scale scale 
                                           :location
                                           
                                           ;; undo the chunk-copied-from here
                                           
                                           sp)
                                     
                                     :details 
                                     ;(format nil "~S ~S ~S ~S ~S" 
                                     ;  'move-attention
                                     ;  'scale scale 
                                     ;  'location sp)
                                     
                                     (concatenate 'string
                                                  "Move-attention "
                                                  (symbol-name sp)
                                                  " "
                                                  (symbol-name scale))
                                     
                                     :module :vision))))
       (t
        (print-warning "Invalid command ~a sent to the visual buffer" 
                       (chunk-spec-chunk-type chunk-spec)))
       ))
    
    (visual-location
     (case (chunk-spec-chunk-type chunk-spec)
       (visual-location 
        (let ((value (spec->criterion chunk-spec 'value :vision 
                                       'visual-location))
              (kind (spec->criterion chunk-spec 'kind :vision 
                                      'visual-location))              
              (color (spec->criterion chunk-spec 'color :vision 
                                       'visual-location))
              (size (spec->criterion chunk-spec 'size :vision 
                                      'visual-location))              
              (nearest (if (slot-in-chunk-spec-p chunk-spec 'nearest) 
                           (verify-single-explicit-value 
                            (chunk-spec-slot-spec chunk-spec 'nearest) 
                            :vision 'visual-location 'nearest)
                           nil))              
              (distance (spec->criterion chunk-spec 'distance :vision 
                                           'visual-location))              
              (screen-x (if (slot-in-chunk-spec-p chunk-spec 'screen-x) 
                            (parse-xy-from-spec 
                             (chunk-spec-slot-spec chunk-spec 'screen-x) 
                             :vision 'visual-location 'screen-x)
                            nil))              
              (screen-y (if (slot-in-chunk-spec-p chunk-spec 'screen-y) 
                            (parse-xy-from-spec 
                             (chunk-spec-slot-spec chunk-spec 'screen-y) 
                             :vision 'visual-location 'screen-y)
                            nil))
              (attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec :attended) 
                             :vision 'visual-location :attended)
                            :IGNORE))
              ;; mdb added for a3
              (userprop1 (spec->criterion chunk-spec 'userprop1 :vision 
                                            'visual-location))
              (userprop2 (spec->criterion chunk-spec 'userprop2 :vision 
                                            'visual-location))
              (userprop3 (spec->criterion chunk-spec 'userprop3 :vision 
                                            'visual-location))
              (userprop4 (spec->criterion chunk-spec 'userprop4 :vision 
                                            'visual-location))
              )           
          
          
          
          ;;; should check to make sure that there's nothing invalid, but
          ;;; for now just passing everything along
          
          ;;; technically the timeimg of this should probably be
          ;;; done in find-location, but I didn't want to change
          ;;; that at this point
          ; not necessary (setf (stuffed vis-mod) nil)
          
          (schedule-event-relative 0 'find-location :module :vision 
                                   :destination :vision 
                                   :details "Find-location" ;(format nil "~s" 'find-location)
                                   :output 'medium
                                   :params (list :kind kind :attended attended 
                                                 :value value :color color 
                                                 :distance distance
                                                 :size size :screen-x screen-x 
                                                 :screen-y screen-y 
                                                 :nearest nearest
                                                 :userprop1 userprop1
                                                 :userprop2 userprop2
                                                 :userprop3 userprop3
                                                 :userprop4 userprop4
                                                 ))
          ))
       (t
        (print-warning "Invalid command ~a sent to the visual-location buffer" 
                       (chunk-spec-chunk-type chunk-spec)))))))
  
  

;;; This isn't quite perfect since it assumes that the x or y value
;;; will be numbers, but something like current or an error in the request
;;; could result in something else

(defun parse-xy-from-spec (slot-specs module cmd slot)
  (cond ((zerop (length slot-specs))
         (print-warning "~a command to ~s module requires a value for the ~a slot." cmd module slot))
        ((> (length slot-specs) 2)
         (print-warning "the ~a slot in a ~a command to the ~s module may have at most two specifications." slot cmd module))
        ((= (length slot-specs) 1) 
#|
         (cond ((eql '- (caar slot-specs))
                (print-warning 
                 "~a slot may not have the - modifier in a ~a command to the ~s module." 
                 slot cmd module))
|#         
         ;; mdb change for a3:  why not allow negation?  we used to.
         (cond ((eql '- (caar slot-specs))
                (values (list 'not-equal (third (car slot-specs))) t))
               
               ((chunk-spec-variable-p (third (car slot-specs)))
                (print-warning 
                 "~a slot must be explict - not a variable in a ~a command to the ~s module." 
                 slot cmd module))
               
               ((eql '= (caar slot-specs))
                (values (third (car slot-specs)) t))
               
               ((eql '< (caar slot-specs))
                (if (eq (third (car slot-specs)) 'current)
                    (values 'less-than-current t)
                  (values (list 'less-than (third (car slot-specs))) t)))
               
               ((eql '<= (caar slot-specs))
                (if (eq (third (car slot-specs)) 'current)
                    (values 'less-than-current t)
                (values (list 'less-than (1+ (third (car slot-specs)))) t)))
               
               ((eql '> (caar slot-specs))
                (if (eq (third (car slot-specs)) 'current)
                    (values 'greater-than-current t)
                (values (list 'greater-than (third (car slot-specs))) t)))
               
               ((eql '>= (caar slot-specs))
                (if (eq (third (car slot-specs)) 'current)
                    (values 'greater-than-current t)
                (values (list 'greater-than (1- (third (car slot-specs)))) t)))
               
               (t
                (print-warning 
                 "invalid specification for the ~a slot in a ~a command to the ~s module." 
                 slot cmd module))))
        (t ;; length = 2
         (cond ((or (chunk-spec-variable-p (third (first slot-specs)))
                    (chunk-spec-variable-p (third (second slot-specs))))
                (print-warning "~a slot must be explict - not a variable in a ~a command to the ~s module." slot cmd module))
               
               ((and (eql '< (caar slot-specs)) 
                     (eql '> (car (second slot-specs))))
                (values (list 'within (1+ (third (second slot-specs))) 
                              (1- (third (first slot-specs)))) t))
               
               ((and (eql '< (caar slot-specs)) 
                     (eql '>= (car (second slot-specs))))
                (values (list 'within (third (second slot-specs)) 
                              (1- (third (first slot-specs)))) t))
               
               ((and (eql '> (caar slot-specs)) 
                     (eql '< (car (second slot-specs))))
                (values (list 'within (1+ (third (first slot-specs))) 
                              (1- (third (second slot-specs)))) t))
               
               ((and (eql '> (caar slot-specs)) 
                     (eql '<= (car (second slot-specs))))
                (values (list 'within (1+ (third (first slot-specs))) 
                              (third (second slot-specs)))) t)
               
               ((and (eql '<= (caar slot-specs)) 
                     (eql '> (car (second slot-specs))))
                (values (list 'within (1+ (third (second slot-specs))) 
                              (third (first slot-specs)))) t)
               
               ((and (eql '<= (caar slot-specs)) 
                     (eql '>= (car (second slot-specs))))
                (values (list 'within (third (second slot-specs)) 
                              (third (first slot-specs)))) t)
               
               ((and (eql '>= (caar slot-specs)) 
                     (eql '< (car (second slot-specs))))
                (values (list 'within (third (first slot-specs)) 
                              (1- (third (second slot-specs)))) t))
               
               ((and (eql '>= (caar slot-specs)) 
                     (eql '<= (car (second slot-specs))))
                (values (list 'within (third (first slot-specs)) 
                              (third (second slot-specs))) t))
               
               (t
                (print-warning 
                 "Invalid combination of slot ~a specifications in a ~a command to the ~s module." 
                 slot cmd module))))))
                    

(defun spec->criterion (chunk-spec slotname module cmd)
  (if (slot-in-chunk-spec-p chunk-spec slotname) 
    (parse-criterion-from-spec 
     (chunk-spec-slot-spec chunk-spec slotname) 
     module cmd slotname)
    :IGNORE))


(defun parse-criterion-from-spec (slot-specs module cmd slot)
  "Same as PARSE-XY-FROM-SPEC but doesn't do the CURRENT stuff."
  (cond ((zerop (length slot-specs))
         (print-warning "~a command to ~s module requires a value for the ~a slot." cmd module slot))
        ((> (length slot-specs) 2)
         (print-warning "the ~a slot in a ~a command to the ~s module may have at most two specifications." slot cmd module))
        ((= (length slot-specs) 1) 
         #|
        (cond ((eql '- (caar slot-specs))
                ;; wait, why not?  Seems like a negation here is OK.  --mdb
                (print-warning 
                 "~a slot may not have the - modifier in a ~a command to the ~s module." 
                 slot cmd module))

|#
         (cond ((eql '- (caar slot-specs))
                (values (list 'not-equal (third (car slot-specs))) t))
               
               ((chunk-spec-variable-p (third (car slot-specs)))
                (print-warning 
                 "~a slot must be explict - not a variable in a ~a command to the ~s module." 
                 slot cmd module))
               
               ((eql '= (caar slot-specs))
                (values (third (car slot-specs)) t))
               
               ((eql '< (caar slot-specs))
                (values (list 'less-than (third (car slot-specs))) t))
               
               ((eql '<= (caar slot-specs))
                (values (list 'less-than (1+ (third (car slot-specs)))) t))
               
               ((eql '> (caar slot-specs))
                (values (list 'greater-than (third (car slot-specs))) t))
               
               ((eql '>= (caar slot-specs))
                (values (list 'greater-than (1- (third (car slot-specs)))) t))
               
               (t
                (print-warning 
                 "invalid specification for the ~a slot in a ~a command to the ~s module." 
                 slot cmd module))))
        (t ;; length = 2
         (cond ((or (chunk-spec-variable-p (third (first slot-specs)))
                    (chunk-spec-variable-p (third (second slot-specs))))
                (print-warning "~a slot must be explict - not a variable in a ~a command to the ~s module." slot cmd module))
               
               ((and (eql '< (caar slot-specs)) 
                     (eql '> (car (second slot-specs))))
                (values (list 'within (1+ (third (second slot-specs))) 
                              (1- (third (first slot-specs)))) t))
               
               ((and (eql '< (caar slot-specs)) 
                     (eql '>= (car (second slot-specs))))
                (values (list 'within (third (second slot-specs)) 
                              (1- (third (first slot-specs)))) t))
               
               ((and (eql '> (caar slot-specs)) 
                     (eql '< (car (second slot-specs))))
                (values (list 'within (1+ (third (first slot-specs))) 
                              (1- (third (second slot-specs)))) t))
               
               ((and (eql '> (caar slot-specs)) 
                     (eql '<= (car (second slot-specs))))
                (values (list 'within (1+ (third (first slot-specs))) 
                              (third (second slot-specs)))) t)
               
               ((and (eql '<= (caar slot-specs)) 
                     (eql '> (car (second slot-specs))))
                (values (list 'within (1+ (third (second slot-specs))) 
                              (third (first slot-specs)))) t)
               
               ((and (eql '<= (caar slot-specs)) 
                     (eql '>= (car (second slot-specs))))
                (values (list 'within (third (second slot-specs)) 
                              (third (first slot-specs)))) t)
               
               ((and (eql '>= (caar slot-specs)) 
                     (eql '< (car (second slot-specs))))
                (values (list 'within (third (first slot-specs)) 
                              (1- (third (second slot-specs)))) t))
               
               ((and (eql '>= (caar slot-specs)) 
                     (eql '<= (car (second slot-specs))))
                (values (list 'within (third (first slot-specs)) 
                              (third (second slot-specs))) t))
               
               (t
                (print-warning 
                 "Invalid combination of slot ~a specifications in a ~a command to the ~s module." 
                 slot cmd module))))))



(defun params-vision-module (vis-mod param)
  (if (consp param)
    (case (car param)
      (:conservative-update-visual
       (if (cdr param)
           (print-warning ":conservative-update-visual is depricated - consult Dan if you need this parameter")
         (setf (conservative-p vis-mod) (cdr param))))
      (:optimize-visual
       (setf (optimize-p vis-mod) (cdr param))) 
      (:visual-attention-latency
       (setf (move-attn-latency vis-mod) (cdr param)))
      (:visual-finst-span
       (setf (finst-span vis-mod) (cdr param))
       (check-finsts vis-mod)
       (cdr param))   
      (:visual-movement-tolerance
       (setf (move-allowance vis-mod) (cdr param)))
      (:visual-num-finsts
       (setf (num-finst vis-mod) (cdr param))
       (check-finsts vis-mod)
       (cdr param))
      (:visual-onset-span
       (setf (new-span vis-mod) (cdr param))))
    (case param
      (:conservative-update-visual
       (conservative-p vis-mod))
      (:optimize-visual
       (optimize-p vis-mod))  
      (:visual-attention-latency
       (move-attn-latency vis-mod))
      (:visual-finst-span
       (finst-span vis-mod))   
      (:visual-movement-tolerance
       (move-allowance vis-mod))
      (:visual-num-finsts
       (num-finst vis-mod))
      (:visual-onset-span
       (new-span vis-mod)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN
;;; 
;;; Methods from the actr-interface file updated to work with buffers
;;;

(defmethod stuff-visloc-buffer ((vis-mod vision-module))
  ;(unless *visual-location*
  
  (unless (or (buffer-read 'visual-location) (tracked-obj vis-mod))
    (awhen (find-current-locs-with-spec vis-mod (default-spec vis-mod))
           ;; not needed (setf (stuffed vis-mod) t)
           
           ;(setf *visual-location*
           (schedule-set-buffer-chunk 'visual-location
                                      (construct-location vis-mod 
                                                          (random-item (objs-max-slotval it 'tstamp))
                                                          (default-spec vis-mod))
                                      0
                                      :module :vision
                                      :requested nil
                                      :priority 10))))


;;; could this be an :after method?
(defmethod find-location :around ((vis-mod vision-module) &key (kind :IGNORE)
                                     (attended :IGNORE) (value :IGNORE)
                                     (color :IGNORE) (size :IGNORE)
                                     screen-x screen-y distance nearest)
  (declare (ignore kind attended value color size screen-x screen-y distance 
                   nearest))
  (aif (call-next-method)
       ;(setf *visual-location* it)
       (progn
         (setf (loc-failure vis-mod) nil)
         (schedule-set-buffer-chunk 'visual-location it 0 :module :vision :priority 10))
       ;(setf *visual-location* (get-wme 'FAILURE))))
       (progn
         (setf (loc-failure vis-mod) t)
         (schedule-event-relative 0 'find-loc-failure :module :vision :output 'medium))))

(defun find-loc-failure ()
  "Dummy event function to signal a find-location failure in the trace"
  nil)


(defmethod visicon-update :after ((vis-mod vision-module))
  (stuff-visloc-buffer vis-mod))


                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DAN 
;;; define the module itself  -- name :vision

(define-module-fct :vision 
    (list (list 'visual-location nil '(:attended) '(attended)
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
   (define-parameter :conservative-update-visual
     :valid-test #'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Depricated...")
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
   )
  :version "2.4"
  :documentation "A module to provide a model with a visual attention system"
  :creation #'create-vision-module
  :reset #'reset-vision-module
  :query #'query-vision-module
  :request 'pm-module-request
  :buffer-mod nil  ;;; Don't accpet +visual or +visual-location chunk modifications
  :params #'params-vision-module
  :delete nil   ;;; I don't think there's any clean up necessary of the vision if a model and its vision module are deleted
  :notify-on-clear nil ;;; don't need to record the chunks that leave buffers
  ;:update #'update-module
  :warning 'warn-vision)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Various toplevel commands
;;;; 


(defmacro attend-visual-coordinates (x y)
  "Attend particular visual coordinates as X and Y."
  `(attend-visual-coordinates-fct ,(vector x y)))

(defun attend-visual-coordinates-fct (loc)
  "Tells the Vision Moddule to start with attention at a certain location."
  (setf (current-marker (get-module :vision)) (xy-to-dmo loc t))
  (setf (current-lof (get-module :vision)) loc))

(defun set-char-feature-set (setname)
  "Sets the feature set used to represent characters when optimizing is off. <setname> should be a keyword."
  (set-cfs-mth (get-module :vision) setname))

(defmacro set-visloc-default (&rest params)
  "Macro to set the default specification for visual location buffer stuffing."
  `(set-visloc-default-fct ',params))

(defun set-visloc-default-fct (params)
  "Function to set the default specification for visual location buffer stuffing."
  (verify-current-mp
   "No current meta-process.  Cannot set visloc defaults."
   (verify-current-model 
    "No current model.  Cannot set visloc defaults."
    (if (get-module :vision)
        (progn
          (setf (default-spec (get-module :vision))
            (apply #'construct-findloc-spec params))
          t)
      (print-warning "No vision module found.  Cannot set visloc defaults.")))))

(defun print-visicon ()
  "Print the Vision Module's visicon. For debugging."
  (awhen (get-module :vision)  ;; Test that there is a vision module
    (update-new it)
    (check-finsts it) 
    (format t "~%Loc        Att   Kind           Value             Color           ID")
    (format t "~%---------  ---   -------------  ----------------  --------------  -------------")
    ;;; DAN (dolist (feat (visicon (vis-m *mp*)))
    (dolist (feat (visicon it))
      (print-icon-feature feat))))

(defun remove-visual-features (loc &optional scale)
  "Removes features at the specified visual location."
  (delete-features (get-module :vision) loc scale))



;;; these are in for backward compatibility

(defmacro pm-attend-location (x y)
  "Attend a particular visual location. [left in for backward compat]"
  `(attend-visual-coordinates-fct ,(vector x y)))

(defun pm-set-char-feature-set (setname)
  "Sets the feature set used to represent characters when optimizing is off. <setname> should be a keyword."
  (set-cfs-mth (get-module :vision) setname))

(defmacro pm-set-visloc-default (&rest params)
  "Set the default specification for visual location buffer stuffing."
  `(setf (default-spec (get-module :vision))
         (apply #'construct-findloc-spec ',params)))

(defun pm-print-icon ()
  "Print the Vision Module's visicon. For debugging."
  (print-visicon))

(defun pm-add-screen-object (obj)
  "Add an object [should be an ICON-FEATURE] to the visicon."
  (add-screen-object obj (get-module :vision)))

(defun pm-delete-screen-object (obj)
  "Given an object's ID, delete it from the visicon."
  (delete-screen-object obj (get-module :vision)))

(defun pm-remove-features (loc &optional scale)
  "Removes features at the specified visual location."
  (delete-features (get-module :vision) loc scale))


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
