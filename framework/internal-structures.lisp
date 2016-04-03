;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : internal-structures.lisp
;;; Version     : 3.0
;;; 
;;; Description : All of the defstructs for the internal code.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.07 Dan
;;;             : Created.
;;; 2005.01.05 Dan
;;;             : Changed the version on the meta-process so that it indicates
;;;             : the svn revision because it's going to be used to create a
;;;             : snapshot for the website.
;;; 2005.01.10 Dan
;;;             : Same as above - added r20 and this time it's actually going
;;;             : out to the site!
;;; 2005.01.12 Dan
;;;             : Because device is becoming a module it doesn't need to be
;;;             : a slot in the model.
;;; 2005.01.15 Dan
;;;             : * Taking the r20 out of the meta-process version and uping
;;;             :   it to 1.0a2.
;;;             : * Moving to 80 charater width.
;;;             : * Adding the copied-from slot to chunks.
;;; 2005.01.16 Dan
;;;             : * Removed the print-functions for chunks and chunk-types
;;;             :   because users shouldn't be seeing those and there's no
;;;             :   need to hide the details.
;;; 2005.01.21 Dan
;;;             : * Added the merge-list slot to chunks to help speed up the
;;;             :   merging action.
;;; 2005.01.27 Dan
;;;             : * Added the filter slot to the printing module structure.
;;; 2005.01.29 Dan
;;;             : * Added r33 to the meta-process version for distribution
;;;             :   on the ACT-R website.
;;; 2005.01.31 Dan 
;;;             : * Removed the r33 from the version and updated it to 1.0a3.
;;; 2005.02.02 Dan
;;;             : * Added the detail slot to the printing module.
;;;             : * Changed the default output for break events to be low
;;;             :   for use wth the detail level.
;;; 2005.02.11 Dan
;;;             : * Changed the make-hash-tables in the chunk structure to
;;;             :   limit the size to just a little bigger than needed and
;;;             :   in the meta-process to 5 for models.
;;; 2005.03.23 Dan
;;;             : * Added the secondary-reset slot to the module structure.
;;; 2005.04.08 Dan
;;;             : * Added r67 to meta-process version for distribution on
;;;             :   the website.
;;; 2005.04.14 Dan
;;;             : * Added the suppress-cmds slot to the printing module to get
;;;             :   around a problem with no-output and trying to read the :cmdt
;;;             :   parameter...
;;; 2005.04.20 Dan
;;;             : * Took the r67 off of the meta-process version.
;;; 2005.05.11 Dan
;;;             : * Changed the version to 1.0b1 [r79]. 
;;; 2005.05.12 Dan
;;;             : * Removed the [r79] from the version.
;;; 2005.06.10 Dan
;;;             : * Changed the version to 1.0b2 [r120]. 
;;; 2005.06.11 Dan
;;;             : * Changed version to 1.0b2
;;; 2005.07.12 Dan
;;;             : * Changed the framework version to 1.0 [r130].
;;; 2005.07.13 Dan
;;;             : * Removed the r130 from the version number.
;;; 2005.08.30 Dan
;;;             : * Changed the framework version to 1.0 [r144].
;;; 2005.08.30 Dan
;;;             : * Oops, mis-encoded the file with mac line endings, so
;;;             :   now changing to [r145].
;;; 2005.09.01 Dan
;;;             : * Taking the [r145] off.
;;; 2005.09.08 Dan
;;;             : * Added the model-warnings slot to the printing module 
;;;             :   struct to support suppression of all model warnings.
;;; 2005.11.16 Dan
;;;             : * Changing framework version to 1.0 [r168].
;;; 2005.11.17 Dan
;;;             : * Changing framework version to back to 1.0.
;;; 2006.01.16 Dan
;;;             : * Changed the version to [r187] for release.
;;; 2006.01.17 Dan
;;;             : * Changing framework version to 1.1.
;;; 2006.01.18 Dan
;;;             : * Added the extended-slots slot to the chunk-type structure
;;;             :   so that one can differentiate between the original slots
;;;             :   and any that are added on the fly.
;;;             : * Added the show-all-slots slot to the printing module to hold
;;;             :   the new parameter.
;;; 2006.01.30 Dan
;;;             : * Adding the maintenance event type for use in things like
;;;             :   terminating events and periodic events.  The schedule-event-
;;;             :   after functions will have a keyword that specifies whether
;;;             :   or not to consider maintenance events that defaults to nil.
;;; 2006.02.27 Dan
;;;             : * Added slots to the meta-process to handle the configuration
;;;             :   of the real time management.
;;; 2006.03.03 Dan
;;;             : * Added the max-time-delta slot to the meta-process.
;;; 2006.03.06 Dan
;;;             : * Changed the version to [r204] for release.
;;; 2006.03.06 Dan
;;;             : * Removed the [r204] from the version.
;;; 2006.03.14 Dan
;;;             : * Changed version to [r212] for web release.
;;; 2006.03.14 Dan
;;;             : * Removed the [r212].
;;; 2006.03.15 Dan
;;;             : * Changed version to [r216] for web release.
;;; 2006.03.15 Dan
;;;             : * Removed the [r216].
;;; 2006.03.21 Dan
;;;             : * Changed version to [r219] for web release.
;;; 2006.03.21 Dan
;;;             : * Changed version to [r220] for web release.
;;; 2006.03.28 Dan
;;;             : * Changed version to [r222] for web release.
;;; 2006.03.28 Dan
;;;             : * Removed the [r222].
;;; 2006.07.10 Dan
;;;             : * Changed version to [r248] for web release.
;;; 2006.07.10 Dan
;;;             : * Removed the [r248].
;;; 2006.11.20 Dan
;;;             : * Added the warn slot to the act-r-module structure.
;;; 2007.01.15 Dan
;;;             : * Changed the version setting in the meta-process structure
;;;             :   to be the value of *actr-version-string* so that I don't
;;;             :   have to touch this file to mark the version changes.
;;; 2007.04.13 Dan
;;;             : * Added a new slot to the printing module struct to hold
;;;             :   the cbct parameter.
;;; 2008.10.20 Dan
;;;             : * Added new slots to the model structure to hold the flags
;;;             :   for normalizing.
;;; 2008.11.03 Dan
;;;             : * Updated the chunk and chunk parameter structures to work 
;;;             :   with the new array representation of the parameters.
;;; 2008.11.13 Dan
;;;             : * Added the dynamic-update-hooks slot to the model struct.
;;; 2008.12.01 Dan
;;;             : * Added the tertiary-reset slot to the module structure.
;;; 2008.12.08 Dan
;;;             : * Added the the largest-chunk-type slot to the model struct
;;;             :   to keep track of the largest possible chunk size.
;;; 2008.12.10 Dan
;;;             : * Added the copy-from-chunk slot to the chunk parameter
;;;             :   struct.
;;; 2009.02.13 Dan
;;;             : * Added the base-name slot to chunks to support the new
;;;             :   short copy names.
;;;             : * Added the short-copy-names slot to the model structure too.
;;; 2009.04.29 Dan
;;;             : * Adding a slot to the meta-process so I can detect recursive
;;;             :   calls to run and signal a warning.
;;; 2009.09.09 Dan
;;;             : * Added the multi, searchable and chunk-set slots to the buffer 
;;;             :   struct.
;;; 2009.12.03 Dan
;;;             : * Adding the dynamics, allow-dynamics, and in-slack to the meta-process 
;;;             :   to provide more flexibility with real-time when a slack-hook 
;;;             :   is used and add the dynamic tag to events.
;;; 2010.01.14 Dan
;;;             : * Adding the copy slot to the buffer struct.
;;; 2010.09.02 Dan
;;;             : * Added a slot to the meta-process to record whether or not the
;;;             :   time overflow warning has been displayed or not.
;;; 2010.11.03 Dan
;;;             : * Changed the event structure's time slot to be mstime because
;;;             :   that's what will be used elsewhere.  To go with that have
;;;             :   added an evt-time function which makes the conversion to
;;;             :   keep the API right.
;;; 2010.12.22 Dan
;;;             : * Added a run-notify slot to the module structures to support
;;;             :   the new run-notify option for modules.
;;; 2011.01.11 Dan
;;;             : * Added a run-over-notify slot to the module structures since
;;;             :   a module may want to know about both situations.
;;; 2011.03.25 Dan
;;;             : * Added a safety check to evt-time so that if there's not a
;;;             :   time it doesn't throw an error.
;;; 2011.04.01 Dan
;;;             : * Changed the initial value for the meta-process time to be 0 
;;;             :   instead of 0.0.
;;; 2012.08.08 Dan
;;;             : * Adding another slot to the meta-process so that it can 
;;;             :   record the order in which models are defined so that they
;;;             :   can be reset in that same order for consistency.
;;; 2013.01.03 Dan
;;;             : * Adding a max-time-maintenance slot to the meta-process.
;;; 2013.01.04 Dan
;;;             : * Added the cannot-define-model slot to the meta-process to
;;;             :   provide a way of avoiding a nasty situation that can occur
;;;             :   if something tries to create a model while the "current" 
;;;             :   model is being changed for other reasons.  Now, anytime there's
;;;             :   a (setf (meta-p-current-model ...)) it should be wrapped in
;;;             :   a (cannot-define-model ...) construct since that macro sets
;;;             :   and clears this slot.
;;; 2013.01.07 Dan
;;;             : * Changed cannot-define-model to be a count since t/nil doesn't
;;;             :   work since cannot-define-model macros could be nested.
;;; 2013.01.24 Dan
;;;             : * Adding a new slot to the chunk-type structure to keep track
;;;             :   of an index for slots because the production matching code
;;;             :   builds an array of bufferXslot values but chunk extension
;;;             :   mixed with subtyping can cause issues with that.
;;; 2013.01.28 Dan
;;;             : * Adding another new slot to the chunk-type structure: possible-slots.
;;;             :   That will hold the names of slots in subtypes of a type but
;;;             :   not members of the type itself.  Needed to allow for the
;;;             :   ability to specify subtype slots in conditions as constants
;;;             :   which seems to follow as a logical conclusion, but which hasn't
;;;             :   been implemented previously.
;;; 2013.05.17 Dan [1.2]
;;;             : * Added new slots to the chunk-type structure to indicate
;;;             :   and manage the new static chunk-types which create 
;;;             :   subtypes instead of growing the base type when extended.
;;; 2013.10.02 Dan
;;;             : * Took the # off of the setting for the default break event
;;;             :   action since that function isn't defined at this point.
;;; 2014.02.12 Dan
;;;             : * Added the user-defined slot to chunk-types for use with
;;;             :   printing 'cannonical' names of types based on the added
;;;             :   option of defined for the :show-static-subtype-names param.
;;; 2014.02.17 Dan
;;;             : * Added the creation-type slot to chunks to allow things like
;;;             :   this to still work with static chunks:
;;;             :    (chunk-type foo)
;;;             :    (chunk-type (bar (:include foo)) slot)
;;;             :    (define-chunks (a isa bar))
;;;             :    (set-chunk-slot-value a slot t)
;;; 2014.02.24 Dan [2.0]
;;;             : * Chunks no longer maintain a chunk-type, just a vector of filled
;;;             :   slots and now an immutable tag since changing DM chunks 
;;;             :   will really be a problem without types to organize things.
;;;             : * Chunk-types don't have a static specifier since they aren't
;;;             :   really used at run time and don't need to maintain a lot
;;;             :   of info.
;;;             : * Instead there's a structure for the model which holds the
;;;             :   chunk-type info instead of just a table of types.
;;;             : * Chunk-specs now have filled slot and empty slot vectors 
;;;             :   instead of a type.
;;; 2014.03.13 Dan
;;;             : * Chunk-specs now hold more details directly: 
;;;             :    - which slots have multiple specifications
;;;             :    - which slots have inequality tests
;;;             :    - which slots have equality tests
;;;             :    - which slots have negation tests
;;;             :    - any potential variable markers in slot tests (non alphanumericp
;;;             :      initial characters)
;;; 2014.03.14 Dan
;;;             : * Request parameters get treated just like slots in a chunk-spec
;;;             :   which means that they will need to be added as slots when 
;;;             :   defined with modules.  Keep track of them in the spec for
;;;             :   comparing a chunk-spec to a chunk-type signature.
;;; 2014.05.14 Dan
;;;             : * Maintain query info separately in the chunk-type-info struct.
;;; 2014.06.24 Dan
;;;             : * Chunk-types don't need to record subtypes info now.
;;;             : * Do want to record the specific parent types given, if any.
;;; 2014.06.25 Dan
;;;             : * Record the chunk-type names as they're created in the chunk-
;;;             :   type-info struct so that printing them out is consistent
;;;             :   among Lisps and so they don't need to be ordered by inheritance
;;;             :   when saving a model.
;;; 2014.08.14 Dan
;;;             : * Printing module no longer needs the show-all-slots slot.
;;; 2014.09.26 Dan
;;;             : * Maintain a sorted list of module names so that things like
;;;             :   resetting can always be applied in the same order unlike
;;;             :   maphash which varies from Lisp to Lisp.
;;; 2014.11.04 Dan
;;;             : * Added a slot to indicate a request-param to the slot-spec
;;;             :   structure.
;;; 2015.03.19 Dan
;;;             : * Added the flags slot to the buffer struct to handle the
;;;             :   new failure option and provide a way for implementing other
;;;             :   such markers in the future.
;;; 2015.06.01 Dan
;;;             : * Added the one-time-tags slot to the printing module struct
;;;             :   to support the new one-time-model-warning command.
;;; 2015.07.29 Dan [2.1]
;;;             : * Changed the version number set in the meta-process to be
;;;             :   the concatenation of the two version strings.
;;;             : * Added a precondition slot to events which will be used to
;;;             :   allow ignoring "unneeded" events.
;;; 2015.08.03 Dan
;;;             : * Going back to *actr-version-string* being the whole thing
;;;             :   and changing that in the version-string file.
;;; 2015.08.25 Dan
;;;             : * Adding the real-time-scale slot to the meta-process to make
;;;             :   it easier to control the scaling in real-time mode.
;;; 2015.09.09 Dan [3.0]
;;;             : * Slot added to buffers to indicate whether its module will
;;;             :   report on completion of requests i.e. trackable.
;;;             : * Slot added to model struct for tracking requests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; These are not for general use!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; NONE!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; I had some odd compiling order issues with the defstructs and defmacros 
;;; so for now the easy fix was to make sure that they are all 
;;; available from the start.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct act-r-buffer 
  "The internal structure for a buffer"
  name
  chunk  ;; holds the chunk name not struct - copy issues and such...
  flags
  module
  spread
  queries
  requests
  (requests-mask 0)
  parameter-name
  requested
  status-printing
  multi
  searchable
  chunk-set
  copy
  trackable)

(defstruct act-r-chunk-spec 
  "The internal structure of a chunk-spec"
  (filled-slots 0) 
  (empty-slots 0) 
  (request-param-slots 0)
  (duplicate-slots 0)
  (equal-slots 0)
  (negated-slots 0)
  (relative-slots 0)
  variables
  slot-vars
  dependencies
  slots)

(defstruct act-r-slot-spec 
  "The internal structure of a chunk-spec's slot specification"
  (modifier '=) name value testable variable request-param)

(defstruct act-r-chunk-type 
  "The internal structure of a chunk-type"
  name documentation super-types parents
  ;; don't need this any more: subtypes 
  slots ;; your slots (direct and inherited) including possible default values
  possible-slots ;; slot names only of all the slots of self and children
  slot-vector ;; a mask of the possible-slots
  (initial-spec (make-act-r-chunk-spec)))

(defstruct act-r-chunk-type-info 
  "The structure to hold chunk-type information for a model"
  (slot->index (make-hash-table)) 
  (index->slot (make-array (list 0) :adjustable t :fill-pointer t)) 
  (slot->mask (make-hash-table)) 
  (size 0) 
  (distinct-types (make-hash-table))
  extended-slots
  query-slots
  (table (make-hash-table))
  (types (list 'chunk)))

(defstruct act-r-chunk 
  "The internal structure of a chunk"
  name base-name
  documentation 
  (filled-slots 0)
  slot-value-lists 
  copied-from
  merged-chunks
  parameter-values
  immutable)

(defstruct act-r-chunk-parameter
  "The internal structure of a chunk parameter"
  name index default-value default-function merge copy copy-from-chunk accessor)

(defstruct (act-r-event (:conc-name evt-)) 
  "Internal ACT-R event"
  mstime priority action model mp module destination params details (output t)
  wait-condition dynamic precondition)

(defun evt-time (event)
  (when (numberp (evt-mstime event))
    (* (evt-mstime event) .001)))

(defstruct (act-r-maintenance-event (:include act-r-event (output 'low)))
  "Events for system maintenance")

(defstruct (act-r-break-event 
            (:include act-r-maintenance-event (action 'act-r-event-break-action))) 
  "The ACT-R break events"
  )

(defstruct (act-r-periodic-event (:include act-r-maintenance-event)) 
  "special event that repeatedly schedules a user's event"
  id)

(defstruct (meta-processes (:conc-name mps-))
  "The internal structure that holds meta-processes"
  (table (make-hash-table))
  (count 0)
  current)

(defstruct (meta-process (:conc-name meta-p-))
  "The internal representation of the meta-process"
  name
  (time 0)
  start-time
  start-real-time
  (models (make-hash-table :size 5))
  current-model
  (model-count 0)
  (model-name-len 0)
  events
  delayed
  dynamics
  allow-dynamics
  in-slack
  break
  pre-events
  post-events
  
  (time-function 'get-internal-real-time)
  (units-per-second internal-time-units-per-second)
  (slack-function 'real-time-slack)
  (real-time-scale 1.0)
  
  max-time-delta
  max-time-maintenance
  
  (next-hook-id 0)
  (hook-table (make-hash-table))
  (version *actr-version-string*)
  (documentation "")
  (running nil)
  time-overflow-warning
  model-order
  
  (cannot-define-model 0))

(defstruct act-r-model
  "The internal structure of a model"
  (modules-table (make-hash-table)) 
  (buffers (make-hash-table))
  (chunks-table (make-hash-table))
  (chunk-ref-table (make-hash-table))
  (chunk-types-info (make-act-r-chunk-type-info))
  name 
  code
  (chunk-update t)
  (dynamic-update t)
  delete-chunks
  dynamic-update-hooks
  short-copy-names

  tracked-requests)

(defstruct act-r-modules
  "The internal structure that holds the modules"
  (table (make-hash-table))
  (count 0)
  (name-len 0)
  notify
  update
  run-notify
  run-over-notify
  sorted-names)

(defstruct act-r-module 
  "The internal structure of a module"
  name buffers version documentation creation reset query
  request buffer-mod params delete notify-on-clear update
  secondary-reset tertiary-reset warn search offset run-notify run-over-notify)

(defstruct act-r-parameter 
  "The internal structure of a parameter"
  param-name 
  default 
  test 
  warning 
  details
  owner
  users)

(defstruct printing-module
  "The internal structure for an instance of the printing module"
  (v (make-act-r-output :stream t))
  (c (make-act-r-output :stream t))
  (suppress-cmds nil)
  (filter nil)
  (detail 'high)
  (model-warnings t)
  (cbct nil)
  (one-time-tags nil))

(defstruct act-r-output
  "The internal structure of an output stream for the printing module"
  stream file)

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
