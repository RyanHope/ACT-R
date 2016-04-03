;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2007 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : vision.lisp
;;; Version     : 5.0
;;; 
;;; Description : Source code for the ACT-R 6 Vision Module.  
;;;
;;; Bugs        : [X] Should object-present-p do a chunk match to determine if
;;;                   the object is still the "same"?  I'm doing that now, but
;;;                   may want to consider something else long term...
;;;                   UPDATE: not doing the chunk test now because it results 
;;;                   in differences where a change is not re-encoded that 
;;;                   which should be with update-attended-loc.  May still
;;;                   want to consider other options.
;;;             : [ ] Not so much a bug, but an inconsistency with the old
;;;                   version.  Find-best-feature doesn't "short circuit"
;;;                   as often with the legacy devices as the old system which
;;;                   means that it calls random-item at times where the same
;;;                   model under the old module didn't.  The behavior of this
;;;                   module is the same regardless, but the extra call to
;;;                   act-r-random will result in different "random" results
;;;                   downstream from the extra call in other modules or code.
;;;                   Only really matters if one has set the seed explicitly
;;;                   to produce a specific output (the testing traces).  Shouldn't
;;;                   cause problems otherwise.  
;;;             : [X] Current module doesn't work quite the same as the old version
;;;                   when items overlap in the display.  Particularly, a screen
;;;                   change during an attention shift for items at the same location
;;;                   in the old module you got the "old" attended item but in the
;;;                   current module you get the "new" item.  Not sure what is
;;;                   really right there.  Switched it so that now you get the
;;;                   old item as well.
;;;
;;; Todo        : [ ] Check the icon-entry code again now that the feats get
;;;                   mapped from old to new when there's no clear because it
;;;                   probably doesn't need to do the "feat match" check anymore...
;;;             : [X] Add in the support for other scales/synthesizing letters from
;;;                   primitives and re-enable the :optimize-visual parameter.
;;;             : [X] Add the tracking code in.
;;;             : [ ] Add hooks into proc-display and find-location to let users
;;;                   configure things and filter the matching (pre and post default)
;;;                   as needed.
;;;             : [ ] Consider generalizing the scale parameter so that the devices
;;;                   can take advantage of it for other uses.
;;;             : [ ] What's the right attended status for something that's no
;;;                   longer in the visicon or for overlapping new items?
;;;                   *Essentially added an "undefined" attended status for
;;;                   *items that are no longer in the visicon - it always
;;;                   *returns nil for the query regardless of the request
;;;                   *which varies from the old module because the old one would 
;;;                   *consider items no longer in the display as "attended nil".
;;;                   -- changed this because if it's a visual-location then it
;;;                   should return "attended nil" as t if it's truely not attended.
;;;                   However, this and the old module now differ because if
;;;                   attention goes back to one of these "old" locations the
;;;                   "old" location chunk reports as attended t in the current
;;;                   module, but it was attended nil in all past versions.
;;;             : [ ] Fix the add/delete/update-visicon-item code to work right
;;;                   when deleteing visicon chunks and test-feats is true.
;;;                   Pretty low priority since most people use add/delete/update
;;;                   for performance and test-feats can have a noticeable cost.
;;;             : [X] There is no feat-table now! 
;;;                   Should the feat-table be purged other than on a clear, for
;;;                   instance if an object goes away without a clear?
;;;             : [ ] Does test-feats do the right thing in enter-into-visicon
;;;                   with respect to checking the real-visual-value i.e. does
;;;                   that always get set correctly elsewhere or can that break
;;;                   with a custom device?
;;; 
;;; ----- History ----- [look also at function comments]
;;;
;;; 2007.05.23 Dan 
;;;             : * Start of a rewrite to use chunks internally for the visicon
;;;             :   and as the objects themselves.
;;;             : * Will work in conjunction with updated device interface, devices,
;;;             :   and motor (mouse cursor stuff) files.
;;; 2007.06.20 Dan
;;;             : * Works in the abstract case at this point (modeler supplied
;;;             :   device which handles everything explicitly).
;;;             : * Took the nearest slot out of visual-locations and made it
;;;             :   a request parameter - should have happened in the old system
;;;             :   long ago...
;;;             : * Also took the userprop slots out and put height and width in.
;;; 2007.07.05 Dan
;;;             : * Modified visicon-chunks to sort based on x-y coordinates to
;;;             :   provide more consistency across platforms/Lisps since the
;;;             :   visicon hash-table doesn't maintain the entry order.  It's
;;;             :   a slight performance hit, but consistency in the tutorial 
;;;             :   and elsewhere is worth it (for now at least, and it could
;;;             :   be put onto a parameter to check if people don't like it).
;;; 2007.07.06 Dan
;;;             : * Added a default case to icon-entry to just return the
;;;             :   chunks explicit chunk-visicon-entry value when the "matching"
;;;             :   process fails (fixes a bug in re-encoding of the screen when
;;;             :   a "close" but new object is the one that gets re-encoded).
;;; 2007.07.06 Dan
;;;             : * Added the text-feats function even though it's not really
;;;             :   needed yet...
;;; 2007.07.09 Dan
;;;             : * Fixed a bug when move tolerance was 0.0 - needed to add
;;;             :   a couple chunk-visicion-entry calls around chunk names in a
;;;             :   couple of places to make sure the "right" chunk was being
;;;             :   referenced.
;;; 2007.07.12 Dan
;;;             : * Fixed a bug with the move-attention requests.  It didn't
;;;             :   verify that the chunk for the screen-pos was of type visual-
;;;             :   location before sending it off for further processing.
;;; 2007.10.04 Dan
;;;             : * Added the pieces to handle the "special" processing of
;;;             :   text items into sub-features (:optimize-visual nil), and
;;;             :   the processing of scale {phrase or word} in the move-attention
;;;             :   requests for text items.
;;;             : * Updated the version to a2.
;;; 2007.11.01 Dan
;;;             : * Fixed bugs in attend-visual-coordinates and enter-into-visicon.
;;; 2007.11.16 Dan
;;;             : * Added a check of moving-atteniton to the drop-out check for
;;;             :   update-attended-loc.
;;; 2007.12.11 Dan
;;;             : * Added the randomizing of times for the reencoding events.
;;;             :   That was a bug introduced in the 5->6 transition.
;;; 2007.12.17 Dan
;;;             : * Finished first pass at adding the tracking code in.
;;;             :   It works with either the old "screen-obj" style mechanism
;;;             :   (though that's now depricated) or through the visual-location
;;;             :   feature chunk (must return the same location chunk to move
;;;             :   the item for the model).
;;; 2007.12.18 Dan
;;;             : * Clean-up of the tracking code to fix some of the cases
;;;             :   that weren't signaled or handled correctly.
;;; 2007.12.19 Dan
;;;             : * Minor fix for update-tracking-mth - make sure vis-loc-to-obj
;;;             :   called with the current device or object as needed.
;;; 2007.12.20 Dan
;;;             : * Changed object-present-p to not do the chunk testing for
;;;             :   checking for a similar object because it prevented some
;;;             :   re-encodings that should have happened.  (Also see the note
;;;             :   under bugs above.)
;;; 2007.12.21 Dan
;;;             : * Changed find-current-locs-with-spec to catch the case where
;;;             :   :nearest current is specified but there isn't yet a currently
;;;             :   attended location.  To be backward compatible it assumes 0,0
;;;             :   and prints a warning about it.
;;; 2008.01.07 Dan
;;;             : * Added merging functions for visicon-entry, visual-object, and
;;;             :   real-visual-value chunk parameters so that the new chunk's
;;;             :   values are set in the merged chunk.  Vision needs the newest
;;;             :   info available to properly align with the visicon and current
;;;             :   device...
;;; 2008.01.09 Dan
;;;             : * To maintain the determinism of things the chunks to be
;;;             :   stuffed are first sorted by name before a random one is 
;;;             :   picked.  Shouldn't be too much of a performance hit and
;;;             :   it's needed if the test models are to always produce the
;;;             :   same traces.
;;; 2008.01.16 Dan
;;;             : * Fixed a bug in icon-entry that could throw a warning if the
;;;             :   visual-location being used in an attention shift hadn't 
;;;             :   orignially come from the vision module.
;;; 2008.01.31 Dan
;;;             : * Added in a version of the add-screen-object and delete-screen-object
;;;             :   commands for those that used them in the old module.  The
;;;             :   delete method differs from the old version in that it takes
;;;             :   the same object as add does.
;;; 2008.02.14 Dan
;;;             : * Adding a new request to the visual-location buffer - set-visloc-default.
;;;             :   That allows the model to configure the buffer stuffing test as
;;;             :   would be done with the set-visloc-default command without needing
;;;             :   to resort to a !eval!.
;;;             : * Re-fixed a bug that got lost somewhere along the way!
;;;             :   find-current-locs-with-spec mis-tested :attended.
;;;             :   Was fixed on 2007.12.07 (it's in the commit logs) but that
;;;             :   change and note isn't here now so somewhere along the way
;;;             :   I modified and committed an old version...
;;;             : * Added some more saftey tests to set-visloc-default command.
;;; 2008.02.14 Dan [3.0b1]
;;;             : * Corrected an oversight in the visual-location matching.
;;;             :   The value slot wasn't being compared to the "real" visual
;;;             :   value for something like text where the visual-location
;;;             :   chunk's value isn't the same as what print-visicon shows.
;;;             :   Thus, unlike the old version of the module, requests like
;;;             :   this wouldn't work as expected prior to this fix:
;;;             :   +visual-location> isa visual-location value "a"
;;;             :   for "standard" text display items because the default operation 
;;;             :   for text items is to set the value slot to text in the 
;;;             :   visual-location chunk and that's what the comparison was using.
;;;             :   Now, when the chunk-real-visual-value parameter is set, that's
;;;             :   what gets used for the value slot tests.
;;; 2008.02.15 Dan
;;;             : * Added a remove-finst function since update-cursor-feat 
;;;             :   called it!
;;; 2008.03.13 Dan
;;;             : * Changed the priority on the set-visloc-default request
;;;             :   action that gets scheduled to have a value of 9 which puts
;;;             :   it just below the implicit buffer clearing due to the 
;;;             :   request.  That makes it very unlikely that a cooccuring
;;;             :   proc-display will sneak in between the clearing of the 
;;;             :   buffer and the changing of the defaults and thus stuff
;;;             :   an "unwanted" chunk in there.  It could avoid that by
;;;             :   scheduling the priority higher than the clear, but I don't
;;;             :   think that's a good practice in general.
;;; 2008.03.21 Dan
;;;             : * Fixed a bug in test-attended because it assumed the chunk
;;;             :   for which the attended status was being checked was still
;;;             :   a valid member of the visicon.  Brings up a question which
;;;             :   I've added under bugs - what's the right status for such
;;;             :   a chunk?  Should it just always "fail" the query?  
;;;             : * A related question the test model brought up is what 
;;;             :   about when there are "overlapping" features - should they
;;;             :   share a finst?
;;;             : * Fixed an issue with enter-into-visicon because it didn't
;;;             :   consider the "real" value of the items when eliminating
;;;             :   possible duplicates.
;;; 2008.03.31 Dan
;;;             : * Changed visicon-chunks so that it has an optional parameter
;;;             :   which indicates when to sort things.  Only using that for
;;;             :   the print-visicon command and find-location requests now to 
;;;             :   improve performance and still maintain the "cross platform"
;;;             :   consistency.
;;; 2008.04.01 Dan
;;;             : * Added add-visicon-item and delete-visicon-item as more
;;;             :   "user" level equivalents of add-screen-object and delete-...
;;;             :   because they don't require passing the vision module itself
;;;             :   as a parameter.  Otherwise they're the same commands.
;;; 2008.04.07 Dan
;;;             : * Added an automatic call to stuff-visloc-buffer when a
;;;             :   set-visloc-default request is made to trigger any possible 
;;;             :   stuffing based on the new settings. 
;;;             : * Adding a new parameter :auto-attend.
;;;             :   When it is set to t (defaults to nil) it will cause the 
;;;             :   vision module to automatically start a move-attention after
;;;             :   a successful visual-location request completes to attend
;;;             :   to that visual-location.
;;;             :   Basically it's a modeling shortcut for simple situations
;;;             :   where the result of the visual-location request aren't
;;;             :   that important and one is just going to attend it anyway.
;;;             :   Saves some productions in the model but doesn't affect the
;;;             :   timing because the request is delayed 50ms to compensate for 
;;;             :   the skipped production.
;;;             :   The module is not marked as busy during the 50ms delay.  So
;;;             :   one should be careful about using explicit move-attention
;;;             :   requests when this parameter is enabled.  The assumption is
;;;             :   that you won't be using explicit requests if you turn this
;;;             :   option on.
;;; 2008.04.08 Dan
;;;             : * Fixed an issue with re-encoding - it didn't mark the chunk
;;;             :   being set into the buffer as unrequested.  Now it does.
;;; 2008.04.10 Dan
;;;             : * Added a new parameter :test-feats which can allow for some
;;;             :   significant improvements in run time for proc-display calls
;;;             :   if it is set to nil (the default value is t).  There are 
;;;             :   limited situations where one can safely set it to nil:
;;;             :   - All proc-display calls specify :clear t
;;;             :   - If each visual item in the device returns the "same" chunk 
;;;             :     from build-vis-locs-for every time (by same it means same 
;;;             :     name though the contents could differ)
;;;             :   The next step is to update the built in devices so that 
;;;             :   they satisfy the second situation and thus work correctly
;;;             :   with the parameter set to nil.
;;; 2008.05.22 Dan
;;;             : * Added a warning to add-finst since it can be called by the
;;              :   model through an assign-finst request with a "bad" value.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list and
;;;             :   slot-specs-to-chunk-spec since the macros were removed from 
;;;             :   the chunk-spec code.
;;; 2008.07.02 Dan [3.0b2]
;;;             : * Added code to delete the "internal" visicon chunks when a
;;;             :   proc-display occurs.
;;;             :   Requires locking the device during visual-location buffer
;;;             :   settings to avoid possible deletion of a chunk that hasn't
;;;             :   yet been copied for the buffer.
;;; 2008.07.14 Dan
;;;             : * Fixed an issue when test-feats was t that overlapping text
;;;             :   could persist - the old icon entry may stick around when 
;;;             :   :clear t not given to proc-display.
;;; 2008.07.15 Dan
;;;             : * Changed the output setting for find-loc-failure so that it
;;;             :   prints in the low detail trace.
;;; 2008.07.22 Dan
;;;             : * Fixing the overlapping text issue on the 14th re-introduced
;;;             :   a bug where overlapping text items with different values
;;;             :   were "merged" into one feature.  Re-fixed that issue.
;;; 2008.07.24 Dan
;;;             : * Adding yet another parameter  - :delete-visicon-chunks.
;;;             :   Defaults to t, and when on the visicon chunks get purged
;;;             :   during proc-display.
;;; 2008.08.11 Dan [3.0b3]
;;;             : * Adding a new query to the module to allow one to detect
;;;             :   visual changes other than through the visual-location buffer
;;;             :   stuffing. 
;;;             :   
;;;             :   There's a new parameter called :scene-change-threshold
;;;             :   and if during a proc-display the computed change equals
;;;             :   or exceeds that setting the scene-change query of the visual
;;;             :   buffer will go true.
;;;             :   It will stay true for a time equal to the :visual-onset-span 
;;;             :   value.
;;;             :   It can also be explicitly cleared by using the new request
;;;             :   which takes no parameters and no time:
;;;             :     +visual>
;;;             :       isa clear-scene-change
;;;             :   It is also cleared with the existing clear request:
;;;             :     +visual>
;;;             :       isa clear 
;;;             :   The change computation is computed as a proportion of
;;;             :   items which differ between the two sets and the setting of the 
;;;             :   :scene-change-threshold must be a value in the range 0.0-1.0.
;;;             :   
;;;             :   Here's the computation used to determine the change value:
;;;             :   
;;;             :   ov = the set of items in the visicon when proc-display called.
;;;             :   o  = the number of items in ov.
;;;             :   d  = number of items from ov not in the new visicon.
;;;             :   n  = number of items in the new visicon which were not in ov.
;;;             :   
;;;             :   change = (d+n)/(o+n)
;;;             :   
;;;             :   If o and n are both 0 then the change is 0.    
;;; 2008.08.13 Dan
;;;             : * Added a new query for the visual buffer: scene-change-value.
;;;             :   It's mostly there as a debugging aid to show what the last
;;;             :   scene-change proprotion was through the buffer-status 
;;;             :   command.  However, it can be queried normally.  The value
;;;             :   given should be a number.  If the last scene-change had a
;;;             :   proportion of change greater than that value the query will
;;;             :   respond as true.  It does not time out like the scene-change
;;;             :   query does (after the onset span time).
;;; 2009.03.31 Dan [3.0]
;;;             : * Seems stable enough now to take the beta version off...
;;;             : * Added a hash-table based compare mechanism like DM uses for
;;;             :   fast-merging to the process-display code.  About the same
;;;             :   speed-wise for small (0~15) displays, but significantly
;;;             :   faster as they get larger.
;;;             : * Added a new command remove-visual-finsts since people have 
;;;             :   been using proc-display :clear t to do that even without
;;;             :   "changing" the display.
;;;             : * Fixed a bug related to "proc-display :clear t" - when the
;;;             :   same chunks were returned by the build-vis-locs-for method(s)
;;;             :   as it had previously any finsts on those items may have
;;;             :   persisted once the item was no longer "new" again.
;;; 2009.04.27 Dan
;;;             : * Added a check to see if there's a lock set in stuff-visloc-
;;;             :   buffer so it doesn't try to add more than one item at a time.
;;;             :   It was already setting and clearing a lock, but just wasn't
;;;             :   testing for it...
;;; 2009.04.29 Dan
;;;             : * Changed the phrase parsing so that the phrase! chunk gets a 
;;;             :   color based on the majority color among the individual words.
;;;             :   Also added a colors slot to hold the list of item colors
;;;             :   corresponding to the words and objects slots.
;;; 2009.08.27 Dan
;;;             : * Changed visicon-update so that it has an optional parameter
;;;             :   to determine whether or not it needs to count the visicon 
;;;             :   items for return.
;;;             : * Modified delete-screen-object (and thus delete-visicon-item)
;;;             :   so that the internal chunk gets purged if :delete-visicon-chunks
;;;             :   is true.
;;; 2009.08.28 Dan
;;;             : * Added optional update parameters to the internal add-screen-object
;;;             :   and delete-screen-object and the user commands add-visicon-item
;;;             :   and delete-visicon-item so that multiple adds & deletes can avoid
;;;             :   all having to run the updating code.  One benefit to that is that 
;;;             :   it won't always be the first item added to the screen that gets 
;;;             :   stuffed if one doesn't want that.
;;;             : * Added an update-visicon-item command to go along with the 
;;;             :   add- and delete- ones.  It has a required parameter of the object
;;;             :   an optional parameter for updating (like add and delete now have)
;;;             :   and two possible keword parameters :same-chunks and :chunks.
;;;             :   It works in one of three ways based on which if any of the 
;;;             :   keyword parameters are provided (if the parameters are invalid
;;;             :   then it just prints a warning and does nothing):
;;;             :
;;;             :   - If neither is given then it does the same thing as if the object 
;;;             :   had been deleted and then added again.
;;;             :
;;;             :   - If :chunks is non-nil and is either a symbol naming a chunk or a 
;;;             :   list of chunk names all of which correspond to the chunk(s) that
;;;             :   were originally added to the visicon for the object with add-visicon-
;;;             :   item then it will update the module's internal representation of 
;;;             :   those features.
;;;             :
;;;             :   - If :chunks is nil but :same-chunks is t then it will call 
;;;             :   build-vis-locs-for for that object and treat the list of chunks
;;;             :   that are returned the same as if the :chunks parameter had been
;;;             :   given with that list.
;;;             :
;;;             :   One note on using update-visicon-item is that you must set :test-feats
;;;             :   to nil for now otherwise it will not work.  That means if you want
;;;             :   overlapping duplicate features removed you will have to handle that
;;;             :   explicitly outside of the module.
;;; 2009.08.31 Dan
;;;             : * Added more warnings since delete-visicon-item also has a potential
;;;             :   issue with test-feats if it's set to purge chunks.
;;; 2009.11.18 Dan
;;;             : * Fixed an issue with a '(lambda ...)  in synthesize-phrase
;;;             :   because LispWorks doesn't like that construct.
;;; 2010.02.04 Dan
;;;             : * Make sure tracking keeps a finst on something tracked for 
;;;             :   longer than the finst duration time.
;;; 2010.02.12 Dan
;;;             : * Let a move-attention break tracking without having to
;;;             :   explicitly clear it first.  The issue was that tracking 
;;;             :   keeps the module busy so the move-attention would jam, but
;;;             :   now it ignores that jam if it's due to tracking.
;;; 2010.02.18 Dan
;;;             : * Fixed a bug which could cause problems with a device that
;;;             :   was modifying the chunks for its features prior to a 
;;;             :   proc-display call.  Within-move was returning the original
;;;             :   chunk and not the internal copy and featlis-to-chunks was
;;;             :   using that orignial chunk to set the icon-entry for the
;;;             :   object.  Now within-move uses the internal copy and featlis-to-
;;;             :   chunks gets the appropriate visicon-entry from that.
;;; 2010.02.19 Dan
;;;             : * Modified featlis-to-focus and determine-focus-dmo to go
;;;             :   along with the last couple of fixes to make sure everything
;;;             :   stays in sync.
;;; 2010.03.02 Dan
;;;             : * Fixed a bug with featlis-to-focus that could cause problems
;;;             :   when an attended item leaves the screen.
;;; 2010.03.25 Dan
;;;             : * Added a purge-chunk to enter-into-visicon for those which
;;;             :   already existed in the table since they wouldn't have been
;;;             :   purged previously.
;;; 2010.04.30 Dan
;;;             : * Adjusted how :auto-attend works to now mark the module as
;;;             :   busy from the time of the find-location all the way through
;;;             :   the encoding-complete.
;;;             : * Also added an event to show in the trace and make sure that
;;;             :   it is using the buffer's chunk and not the internal chunk
;;;             :   to avoid problems with deleting visicon chunks.
;;; 2010.05.03 Dan
;;;             : * Changed the :output of the "No visual-object found" event
;;;             :   from 'high to 'medium since that's how it has always printed
;;;             :   anyway because of a bug in filter-test.
;;; 2010.08.03 Dan
;;;             : * Changed update-tracking-loc-chunk to also set the current-marker
;;;             :   so that the focus ring updates as the model tracks the item.
;;;             : * Also set current-marker in update-tracking-mth because there
;;;             :   could be a model issue that prevents update-tracing-loc-chunk
;;;             :   from being called (cases 6, 7, 8, and 9).
;;;             : * Don't set the object's screen-pos slot in update-tracking-mth
;;;             :   now except in the cases where it won't get set via update-tracking-loc-chunk.
;;;             :   That prevents an issue with deleting that visicion chunk 
;;;             :   later in most cases.  May want to not set it at all in those
;;;             :   cases.
;;; 2011.01.19 Dan
;;;             : * Changed the attended querying so that chunks which aren't
;;;             :   part of the visicon now may still report that they are
;;;             :   attended nil instead of having all the attended queries fail
;;;             :   as was done previously and noted in the todo.  This differs
;;;             :   from the old vision because the old one only allowed the attended
;;;             :   tests for chunks which came from vision at some point -- thus
;;;             :   arbitrary locations would have been in the all queries fail
;;;             :   situation which the new version no longer has.  Neither however
;;;             :   addresses issue of attention "returning".
;;;             : * Depricating update-tracking now in preparation for fixing some
;;;             :   of the lingering finst/object issues in tracking.
;;;             : * Added unlock-tracking since unlock-device used to call
;;;             :   update-tracking.
;;;             : * Changed update-tracking-mth to account for the fact that things
;;;             :   should always be valid when it starts which avoids some warnings
;;;             :   and problems that used to happen when it tried to update an
;;;             :   'object' on the fly.
;;;             : * Update-attended-loc schedules it's state change so that the
;;;             :   buffer trace will always catch it.
;;; 2011.02.04 Dan [3.1]
;;;             : * A reworking of how the visicon gets maintained with respect to
;;;             :   recording features.  Instead of two tables, one for feature
;;;             :   comparison and one for the visicon, there's only a visicon
;;;             :   table and the keys are the feature lists when :test-feats is
;;;             :   set to t, otherwise the device's loc chunk is the key.
;;;             : * Fixes some issues with re-attending for an object that moves.
;;;             : * Update cursor doesn't modify the existing feature anymore
;;;             :   and instead just creates a new one.
;;;             : * Find-current-loc-with-spec now uses the real-visual-value
;;;             :   value of the "current" when 'value current' is requested if
;;;             :   there is such a value.
;;;             : * Added a new chunk parameter visual-feature-name since the
;;;             :   visicon-entry can now be a list of features instead of the
;;;             :   device's chunk name.
;;;             : * Updated the synth-feat code so that the colors get set
;;;             :   based on the majority color of the lower level when :optimize-
;;;             :   visual is nil too.
;;;             : * Fixed an issue with how the features are marked for a phrase
;;;             :   when optimize-visual is nil.
;;; 2011.03.28 Dan
;;;             : * Fixed a bug with how current-marker gets set during tracking.
;;; 2011.04.01 Dan
;;;             : * Very minor change to tracking code to avoid an unnecessary
;;;             :   copy.
;;; 2011.04.26 Dan
;;;             : * Changed finst and new/change values to use the ms timing.
;;; 2011.04.28 Dan
;;;             : * Added a declaim to avoid a compiler warning about an
;;;             :   undefined function and added some declares to ignore unused
;;;             :   variables.
;;;             : * Suppress the warnings about extending the chunks.
;;; 2011.05.16 Dan
;;;             : * Replaced pm-warning calls with model-warning.
;;; 2011.11.09 Dan
;;;             : * Commented out the *enable-package-locked-errors* line for ACL 
;;;             :   because it no longer seems to be necessary and it sets up a 
;;;             :   dangerous situation for users since that setting enables the
;;;             :   redefinition of CL functions without warning.
;;;             : * Added a new function add-word-characters which allows the 
;;;             :   user to modify how text strings get broken into "words" 
;;;             :   by specifying additional characters to include in sequences 
;;;             :   along with those that are alphanumericp.
;;; 2011.11.14 Dan
;;;             : * Added a new request parameter to visual-location requests
;;;             :   and two additional options for the :nearest specification.
;;;             :   The new :nearest options are clockwise and counterclockwise.
;;;             :   That is computed relative to a center point specified
;;;             :   by the new request parameter :center which should be either
;;;             :   a visual-location or visual-object.  If :center is not 
;;;             :   specified then the default center point is used which is
;;;             :   0,0 but can be changed with the set-visual-center-point 
;;;             :   command (note that open-exp-window will automatically set
;;;             :   the visual center point to the center of the window it opens).
;;;             : * Added current-x, current-y, clockwise, and counterclockwise
;;;             :   to the set of pm-constant chunks the module creates.
;;; 2011.12.19 Dan
;;;             : * Added a string< test to loc-sort so that when the x and y
;;;             :   coordinates are the same there's still a consistent sorting
;;;             :   to the features.
;;; 2012.01.24 Dan
;;;             : * Added a warning to process-display to note that it should
;;;             :   not be called more than once at a time because that can cause 
;;;             :   issues with the re-attending, but since people may be using
;;;             :   that as a hack to force a particular chunk to buffer stuff
;;;             :   in the vis-loc buffer it's a model warning so it can be
;;;             :   turned off.
;;; 2012.01.25 Dan
;;;             : * Changed the order in which the events are scheduled in 
;;;             :   update-attended-loc because it can affect when the next
;;;             :   conflict-resolution occurs.  This doesn't address the
;;;             :   bigger issue, but it at least fixes the only such case I
;;;             :   could find in the default modules.
;;; 2012.02.15 Dan
;;;             : * Added some code to avoid a warning about not having a current
;;;             :   location when one isn't really needed anyway.
;;; 2012.06.26 Dan
;;;             : * The currently-attended and current-marker are both cleared 
;;;             :   if the model processes a new device to avoid re-encoding
;;;             :   something from the new window that happens to be at the
;;;             :   same coordinates as the item last attended in the previous
;;;             :   device.
;;; 2012.06.29 Dan
;;;             : * Added gray as one of the default color chunks.
;;; 2012.08.09 Dan
;;;             : * The state change for update-attended-loc now happens both
;;;             :   immediately and in an event to avoid issues with multiple
;;;             :   simultaneous updates and to still allow for proper recording
;;;             :   of the state transitions.
;;; 2012.12.19 Dan
;;;             : * Added xyz-loc to get the vector of screen-x, screen-y, and
;;;             :   distance. 
;;;             : * Use xyz-loc in the nearest calculation so that distance 
;;;             :   matters too (doesn't change anything for default 2D devices).
;;; 2013.07.24 Dan
;;;             : * Added a run-time warning if a move-attention request doesn't
;;;             :   have a screen-pos value.
;;; 2013.10.02 Dan
;;;             : * Commented out the optimize proclaim since that persists and
;;;             :   may or may not be useful anyway.
;;; 2013.10.14 Dan
;;;             : * Fixed a bug with the find-best-feature function because it
;;;             :   specified a variable-character of #\$ instead of #\& in the
;;;             :   find-matching-chunks call.
;;;             : * Clear the found-locs table on reset.
;;; 2014.01.24 Dan
;;;             : * Changed build-string-feats so that it automatically handles
;;;             :   newlines so the results are consistent among the devices.
;;;             :   Has two additional keyword params now: 
;;;             :    line-height - how much to increase the y-pos for each newline
;;;             :    x-fct - an optional function which can be used to determine
;;;             :      the starting x coordinate for the current line.  If provided
;;;             :      it will be called once for each line of text being passed
;;;             :      three values: the string of the line of text, the value of 
;;;             :      the start-x parameter, and the obj parameter.  If it returns
;;;             :      a number that will override start-x.
;;; 2014.02.12 Dan
;;;             : * The request chunk-types are no longer subtypes of vision-command
;;;             :   since that wasn't actually used for anything.
;;;             : * Removed the abstract-letter and abstract-number chunk-types
;;;             :   since they aren't being used.
;;;             : * Changed the chunk-types for text, empty-space, oval, and
;;;             :   cursor to add a slot to each which is the same as the name 
;;;             :   of the type with a default value of t.  That makes them 
;;;             :   distinct types under the inheritance of the static typing 
;;;             :   mechanism.
;;; 2014.02.14 Dan [3.2]
;;;             : * The default distance for visual locations is now measured in
;;;             :   pixels to be consistent with screen-x and screen-y since
;;;             :   distance is used in nearest calculations.
;;;             : * To do that monitor pixels-per-inch as well and just set the
;;;             :   view-dist to be the right value when it or viewing-distance
;;;             :   change.
;;;             : * Test the location provided for :nearest in a visual-location
;;;             :   request to make sure it has all the coordinates needed and
;;;             :   if not substitute in 0s for x,y and view-dist for distance.
;;;             : * Allow current-distance as a possible value for :nearest.
;;; 2014.02.18 Dan
;;;             : * Fixed a bug with visual-location request's processing of the
;;;             :   :center parameter.
;;; 2014.03.17 Dan [4.0]
;;;             : * Start the change to being consistent with chunks without types.
;;;             : * Changed the module warning function to indicate that it's a
;;;             :   chunk-spec being passed in even though it's ignored.
;;;             : * Changed the query-buffer call to be consistent with the new
;;;             :   internal code.
;;; 2014.05.20 Dan
;;;             : * Start the real conversion to chunks without types.  Change 
;;;             :   the request method and initial chunk-type specifications.
;;;             : * Lots of minor cleanup along the way.
;;;             : * Added an update-new to visicon-update and removed the 
;;;             :   update-new and check-finsts from find-location.
;;; 2014.05.21 Dan
;;;             : * Within-move now considers distance when computing the angle
;;;             :   difference instead of just using an xy pixel distance.
;;;             : * Featlis-to-chunks doesn't use the loc parameter so take it
;;;             :   out.
;;; 2014.05.23 Dan
;;;             : * When creating the sub-letter features for items automatically
;;;             :   create chunks for the feature values if they are symbols.
;;; 2014.05.30 Dan
;;;             : * Use the test-for-clear-request function in pm-module-request.
;;; 2014.07.07 Dan
;;;             : * The doc string doesn't need to indicate that it uses chunks
;;;             :   internally anymore.
;;; 2014.08.13 Dan
;;;             : * The set-visloc-default-request needs to strip the SET-VISLOC-DEFAULT
;;;             :   slot from the spec.
;;; 2014.10.29 Dan
;;;             : * Changed the last-command reported for a visual-location 
;;;             :   buffer 'find' request from visual-location to find-location.
;;; 2014.10.29 Dan
;;;             : * Added safety tests to set-char-feature-set and returns t/nil
;;;             :   now instead of the char-feats object.
;;; 2014.12.08 Dan
;;;             : * Added chunks for the colors pink, light-blue, purple, and
;;;             :   brown to cover all the ones used by the AGI.
;;; 2014.12.15 Dan
;;;             : * Fixed a bug with chop-string that caused problems if there
;;;             :   was an empty line in the input string.
;;; 2015.03.18 Dan
;;;             : * Explicitly turn off the pretty printer during print-icon-
;;;             :   feature to avoid weird line breaks in the visicon output.
;;; 2015.03.20 Dan
;;;             : * Failures for both buffers now set the buffer failure flag
;;;             :   using set-buffer-failure.
;;; 2015.03.23 Dan
;;;             : * The visual buffer failures are now marked as to whether it
;;;             :   was a requested encoding or an automatic one which failed.
;;; 2015.04.21 Dan [4.1]
;;;             : * New parameter :unstuff-visual-location which indicates 
;;;             :   whether or not stuffed visual-location chunks should be cleared
;;;             :   automatically by the module.  Defaults to nil.  Can be set to
;;;             :   t which means clear it after :VISUAL-ONSET-SPAN or a number
;;;             :   which is the time to clear in seconds.
;;; 2015.04.22 Dan
;;;             : * Also allow a value of new for :unstuff-visual-location which
;;;             :   means that it can be overwritten by a new chunk to stuff.
;;; 2015.05.20 Dan
;;;             : * Move-attention requests now work better when given a location
;;;             :   chunk that wasn't returned by vision because it tests the
;;;             :   features in that chunk against the items in the visicon 
;;;             :   which are within the move distance and picks the item from
;;;             :   those which match the most features instead of just whichever
;;;             :   was closest.  For example if one makes this request now 
;;;             :   where X and Y are the coordinates of a button it will always
;;;             :   return the text of the button instead of randomly choosing
;;;             :   either the button or the text (since they were equally close):
;;;             :     (define-chunks (loc screen-x X screen-y Y kind text))
;;;             :     (module-request 'visual (define-chunk-spec isa move-attention screen-pos loc))
;;;             : * Added the vis-loc-slots slot to the vision module and the
;;;             :   method record-vis-loc-slots so that the device can tell the
;;;             :   vision module what the valid coordinate slots are after
;;;             :   calling vis-loc-coordinate-slots when a device is installed.
;;; 2015.05.29 Dan
;;;             : * Added the clear-all-finsts request to the visual buffer.
;;; 2015.06.04 Dan
;;;             : * Use safe-seconds->ms for converting the params now.
;;;             : * Use ms internally for :visual-attention-latency.
;;; 2015.06.05 Dan
;;;             : * All scheduling done in ms or with the new schedule-event-now.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;             : * Changed default of :unstuff-visual-location to t.
;;; 2015.07.29 Dan [4.2]
;;;             : * Changed the way the check-unstuff-buffer action is scheduled
;;;             :   because that's now the precondition and unstuff-buffer is the
;;;             :   actual action.
;;;             : * Splitting the unstuff- parameter into two parameters:
;;;             :   :unstuff-visual-location can be nil, t, or # and indicates
;;;             :     whether to take a stuffed chunk out of the buffer and
;;;             :     defaults to t.
;;;             :   :overstuff-visual-location determines whether or not to 
;;;             :     stuff a new chunk overtop of a previously stuffed chunk
;;;             :     and defaults to nil.
;;;             : * When an unstuff is scheduled save it so that it can be
;;;             :   deleted if another gets scheduled.
;;; 2015.08.03 Dan
;;;             : * Find best features uses the internal visaul vis-loc-slots
;;;             :   instead of calling vis-loc-coordiate-slots again.
;;; 2015.08.17 Dan
;;;             : * Changed the randomize-time calls which were used with times
;;;             :   in ms to randomize-time-ms.
;;; 2015.08.19 Dan
;;;             : * Tracking needs to use chunk-difference-to-chunk-spec instead
;;;             :   of just chunk-name-to-chunk-spec because it may need to remove
;;;             :   slots from the current buffer chunk, but those wouldn't show
;;;             :   up in the spec for the new loc/obj chunk.
;;; 2015.08.20 Dan
;;;             : * Fixed a safety check in process-display for dividing by 0,
;;;             :   which shouldn't happen anyway.
;;;             : * Only schedule clear-process-display-called if process-display
;;;             :   isn't set to avoid multiple events.
;;; 2015.09.16 Dan [5.0]
;;;             : * Changed the visual buffer to be trackable and record the
;;;             :   request so that it can be sent to complete-request.  The
;;;             :   clear action will automatically clear everything because it
;;;             :   now calls complete-all-module-requests with the module's name,
;;;             :   but all the others will need to handle that.
;;;             : * Added the last-visual-request slot to the module class to
;;;             :   hold the request for completion.
;;;             : * The start-tracking request completes eventhough it leaves
;;;             :   the module busy because that seems like the right way to
;;;             :   handle that.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General description of update
;;;
;;; The idea is to take advantage of the capabilities in ACT-R 6 to use chunks
;;; for the visicon.  Replace build-features-for with build-vis-locs-for that just 
;;; needs to return a list of chunks which can be subtypes of visual-location.  That 
;;; allows the modeler more freedom in representation.  Along with that, the "find-
;;; location" requests will now use the full power of the general chunk matcher which 
;;; means that slots can be specified any number of times and variables will
;;; be available that would allow for something like saying "find a location where
;;; the x and y coordinates are the same" or "find a location where the width
;;; is greater than the height".
;;;
;;; Along with that will be a corresponding shift from feat-to-dmo to vis-loc-to-obj 
;;; which again will shift toward returning the chunk representation directly.
;;; 
;;; It should be completely backward compatible with existing models, but not
;;; with existing devices or code that takes advantage of "hidden" features of
;;; the vision module (some undocumented capabilities like attn-trace have been
;;; removed to simplify the support code, but if people really need those they
;;; can be restored later).
;;;
;;; This does sacrifice a lot of the OO functionality for features and specs,
;;; but my observation is that many  users aren't taking advantage of that 
;;; now and in fact some don't want to bother with it to the point that they 
;;; implement a complete replacement module for vision instead of creating a new 
;;; device to use (yes, really)!  The hope is that by making it chunk based it will 
;;; be more accessible to the average user without giving up too much that the 
;;; advanced user relies on.
;;;
;;; Because it's going to be less restrictive in terms of the find-locations that
;;; it can do there is the the possibility of abusing the system in a "super-human"
;;; way, but that's an issue in other parts of the system as well and one of those
;;; things that the modeler will have to consider his/her own feelings of 
;;; what's plausible.  Basically, the idea is that the tool is very flexible
;;; and it's up to the users to determine how best to use it for their purposes.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GENERAL-PM" "ACT-R-support:general-pm")

(declaim (ftype (function (t) t) default-target-width))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass vision-module (attn-module)
  ((visicon :accessor visicon :initarg :visicon :initform (make-hash-table :test 'equalp))
   (optimize-visual :accessor optimize-p :initarg :optimize-p :initform t)
   (move-attention-latency :accessor move-attn-latency :initarg :move-attn-latency :initform 85)
   (tracked-object :accessor tracked-obj :initarg :tracked-obj :initform nil)
   (tracked-object-last-location :accessor tracked-obj-lastloc :initarg :tracked-obj-lastloc :initform nil)
   (tracked-object-last-feat :accessor tracked-obj-last-feat :initarg :tracked-obj-last-feat :initform nil)
   (tracked-object-last-obj :accessor tracked-obj-last-obj :initarg :tracked-obj-last-obj :initform nil)
   (last-scale :accessor last-scale :initarg :last-scale :initform nil)
   (moving-attention :accessor moving-attention :initarg :moving-attention :initform nil)
   (move-allowance :accessor move-allowance :initarg :move-allowance :initform 0)
   (synthd-objs :accessor synthd-objs :initarg :synthd-objs :initform (make-hash-table)) 
   (found-locs :accessor found-locs :initarg :found-locs :initform (make-hash-table))
   (feature-sets :accessor feature-sets :initarg :feature-sets :initform (all-feature-sets))
   (active-cfs :accessor active-cfs :initarg :active-cfs :initform nil)
   (num-finst :accessor num-finst :initarg :num-finst :initform 4)
   (finst-lst :accessor finst-lst :initarg :finst-lst :initform nil)
   (finst-span :accessor finst-span :initarg :finst-span :initform 3.0)
   (new-span :accessor new-span :initarg :new-span :initform 0.5)
   (default-spec :accessor default-spec :initarg :default-spec :initform nil)  
   (visual-lock :accessor visual-lock :initform nil)
   (last-obj :accessor last-obj :initform nil) ;; added for use in tracking
   (auto-attend :accessor auto-attend :initform nil)
   (test-feats :accessor test-feats :initform t) ;; runtime optimization parameter
   (purge-visicon :accessor purge-visicon :initform t)
   (scene-change :accessor scene-change :initform nil)
   (change-threshold :accessor change-threshold :initform 0.25)
   (view-dist :accessor view-dist :initform nil)
   (ppi :accessor ppi :initform nil)
   (viewing-distance :accessor viewing-distance :initform nil)
   ;(feat-table :accessor feat-table :initform (make-hash-table :test 'equal))
   (current-cursor :accessor current-cursor :initform nil)
   (other-word-chars :accessor other-word-chars :initform nil)
   (center-point :accessor center-point :initform (vector 0 0))
   (process-display-called :accessor process-display-called :initform nil)
   (vis-loc-slots :accessor vis-loc-slots :initform '(screen-x screen-y distance))
   (last-visual-request :accessor last-visual-request :initform nil))
  (:default-initargs
    :name :VISION
    :version-string "5.0"))


(defmethod vis-loc-to-obj (device vis-loc)
  (declare (ignore device))
  (let ((ct (chunk-slot-value-fct vis-loc 'kind)))
    (if (chunk-type-p-fct ct)
        (fill-default-vis-obj-slots (car (define-chunks-fct `((isa ,ct)))) vis-loc)
      (print-warning "Invalid kind slot value ~s in visual-location ~s when attempting to create a visual-object." ct vis-loc))))

(defun convert-loc-to-object (loc)
  (vis-loc-to-obj (aif (chunk-visual-object loc)
                       it
                       (current-device))
                  (chunk-visual-feature-name loc)))

(suppress-extension-warnings)

(extend-chunks synth-feat :copy-function identity)

(extend-chunks visual-new-p)

(extend-chunks visicon-entry :copy-function identity :merge-function :second)

(extend-chunks visual-feature-name :copy-function identity :merge-function :second)

(extend-chunks visual-tstamp)

(extend-chunks visual-object :copy-function identity :merge-function :second)

(extend-chunks special-visual-object :copy-function identity)

(extend-chunks real-visual-value :copy-function identity :merge-function :second)

(extend-chunks visual-approach-width-fn :copy-function identity)

(unsuppress-extension-warnings)


(defun hash-visual-chunk-contents (chunk)
  (let (res)
    (dolist (slot (chunk-filled-slots-list-fct chunk t) res)
      (push (cons slot 
                  (aif (and (eq slot 'value) (chunk-real-visual-value chunk))
                      it
                    (true-chunk-name-fct (fast-chunk-slot-value-fct chunk slot))))
            res))))


;; Since can't test the type use this function to determine if something
;; is valid as a visual-location chunk.  At this point is the only constraints
;; are that it have x and y coordinates

(defun valid-vis-loc-chunk (chunk vision-mod)
  (let ((slots (vis-loc-slots vision-mod)))
    (and (chunk-p-fct chunk)
         (fast-chunk-slot-value-fct chunk (first slots))
         (fast-chunk-slot-value-fct chunk (second slots)))))

(defmethod clear-process-display-called ((vis-mod vision-module))
  (setf (process-display-called vis-mod) nil))

(defmethod record-vis-loc-slots ((vis-m vision-module) slot-list)
  (setf (vis-loc-slots vis-m) slot-list))


(defmethod process-display ((devin device-interface) (vis-mod vision-module) &optional (clear nil))
  "Build a new visicon and initiate any buffer stuffing that may result"

  (let ((feature-list nil)
        (tempicon nil)
        (o (hash-table-count (visicon vis-mod)))
        (n 0)
        (d 0))
    ;; make sure a device is actually installed
    (unless (device devin)
      (print-warning "Cannot process display--no device is installed.")
      (return-from process-display nil))
    
    (if (process-display-called vis-mod)
        (model-warning "Proc-display should not be called more than once at the same ACT-R time.")
      (schedule-event-now 'clear-process-display-called :module :vision :destination :vision :priority :min :output nil :maintenance t))
    
    (setf (process-display-called vis-mod) t)
    
    
    ;; make sure it's safe to change things right now
    ;; and save the clear status if not for future use
    
    (unless (zerop (locks devin))
      (push clear (pending-procs devin))
      (return-from process-display nil))
    
    
    ;; If the device has changed then remove the current attended item and loc marker because
    ;; it shouldn't re-encode at that point in the new device.
    
    (when (and (last-processed-device devin) (not (eq (last-processed-device devin) (device devin))))
      (setf (currently-attended vis-mod) nil)
      (setf (current-marker vis-mod) nil))
    
    ;; record the last device processed
    (setf (last-processed-device devin) (device devin))
    
       
    ;; build the temp visicon
    (setf feature-list (flatten (build-vis-locs-for (device devin) vis-mod)))
    
    (when (with-cursor-p devin)
      
      (awhen (cursor-to-vis-loc (device devin))
             (setf (current-cursor vis-mod) it)
             (push it feature-list)))
    
    ;; Verify that they're all valid visicon items (chunks with first 2 coordinates)
    ;; and make sure that they've got size and distance values
    
    (dolist (x feature-list)
      (if (valid-vis-loc-chunk x vis-mod)
          (progn
            
            (unless (numberp (fast-chunk-slot-value-fct x (third (vis-loc-slots vis-mod))))
              (set-chunk-slot-value-fct x (third (vis-loc-slots vis-mod)) (view-dist vis-mod)))
            
            (unless (numberp (fast-chunk-slot-value-fct x 'size))
              (compute-vis-loc-size x vis-mod))
            
            (push (setf (chunk-visicon-entry x)
                    (if (test-feats vis-mod)
                        (hash-visual-chunk-contents x)
                      x))                  
                  tempicon)
            
            (setf (chunk-visual-feature-name x) x))
        
        (progn 
          (print-warning "Invalid visicon item ~s found when processing the display.  Must be a chunk with ~s and ~s values." 
                         x (first (vis-loc-slots vis-mod)) (second (vis-loc-slots vis-mod)))
          (setf feature-list (remove x feature-list)))))
    

    ;; if clear, then remove all the old entries first
    ;; do both of these steps iteratively so that the individual chunks can
    ;; be purged if desired.
    
    (when clear
      (setf d o)
      (when (purge-visicon vis-mod)
        (maphash (lambda (key value)
                   (declare (ignore key))
                   (when (chunk-p-fct value)
                     (purge-chunk-fct value)))
                 (visicon vis-mod)))
      (clrhash (visicon vis-mod))
            
      ;; just punt the finsts??
      ;; should something be able to survive a :clear ?
      
      (setf (finst-lst vis-mod) nil))
    
    (unless clear
      
      ;; take the old ones out of the visicon
      
      (maphash (lambda (key val)
                 (unless (find key tempicon :test 'equalp)
                   
                   ;; Not a repeat chunkname or a match to one of the new items
                   
                   ;; increment the number of deleted items
                   (incf d)

                   ;; If desired delete the internal chunk now
                   (when (purge-visicon vis-mod)
                     (purge-chunk-fct val))
                   
                   ;; take this out of the visicon now
                   (remhash key (visicon vis-mod))))
               (visicon vis-mod)))
        
    ;; compute how many new items there are
    
    (setf n (+ (length tempicon) d (- o)))
    
    ;; put the new ones in
    
    (mapcar (lambda (vl) (enter-into-visicon vl vis-mod)) feature-list)
  
    ;; other bookkeeping 
    
    (when (tracked-obj-last-feat vis-mod)
      (update-tracking-mth vis-mod t))
    
    ;; Compute the change value and determine if the flag should be
    ;; set at this time.
    
    (let ((change-val (if (zerop (+ o n))
                          0
                        (* 1.0 (/ (+ d n) (+ o n))))))
        (setf (scene-change vis-mod) (cons change-val (mp-time-ms))))
    
    (visicon-update vis-mod)))


(defmethod visicon-chunks ((vis-mod vision-module) &optional sorted)
  (let ((values nil))
    (maphash (lambda (key val)
               (declare (ignore key))
               (push val values))
             (visicon vis-mod))
    (if sorted
        (sort values (lambda (x y) (loc-sort x y vis-mod)))
      values)))


(defun loc-sort (i1 i2 vis-mod)
  (let* ((slots (vis-loc-slots vis-mod))
         (x1 (chunk-slot-value-fct i1 (first slots)))
         (x2 (chunk-slot-value-fct i2 (first slots)))
         (y1 (chunk-slot-value-fct i1 (second slots)))
         (y2 (chunk-slot-value-fct i2 (second slots))))
    (and (numberp x1) (numberp x2) (numberp y1) (numberp y2)
         (or
          (< x1 x2)
          (and (= x1 x2)
               (< y1 y2))
          (and (= x1 x2) (= y1 y2) (string< (symbol-name i1) (symbol-name i2)))))))

(defmethod update-cursor-feat ((devin device-interface) (vis-mod vision-module))
  (when (and (with-cursor-p devin) (device devin))
    (let ((new-pos (get-mouse-coordinates (device devin)))
          (cur-crsr (current-cursor vis-mod)))
      
      (when (and cur-crsr (not (vpt= new-pos (xy-loc cur-crsr vis-mod))))
        
        ;; if it has moved just punt the old one and get a new one instead of 
        ;; trying to modify the existing chunk values like was done previously
        
        (remove-finst vis-mod cur-crsr)
        
        (let ((entry (gethash (chunk-visicon-entry cur-crsr) (visicon vis-mod))))
          
          (when entry
            (when (purge-visicon vis-mod)
              (purge-chunk-fct entry))
            
            ;; take this out of the visicon now
            (remhash (chunk-visicon-entry cur-crsr) (visicon vis-mod))))
        
        (setf cur-crsr nil))
      
      (when (null cur-crsr)
          (awhen (cursor-to-vis-loc (device devin))
                 (if (valid-vis-loc-chunk it vis-mod)
                     (let ((d-slot (third (vis-loc-slots vis-mod))))
                       (setf (current-cursor vis-mod) it)
                       
                       (unless (numberp (chunk-slot-value-fct it d-slot))
                         (set-chunk-slot-value-fct it d-slot (view-dist vis-mod)))
                       
                       (unless (numberp (chunk-slot-value-fct it 'size))
                         (compute-vis-loc-size it vis-mod))
                       
                       (setf (chunk-visicon-entry it)
                         (if (test-feats vis-mod)
                             (hash-visual-chunk-contents it)
                           it))
                       
                       (setf (chunk-visual-feature-name it) it)
                       
                       (enter-into-visicon it vis-mod)
                       ;; Might have been looking at it so update that now (didn't do this previously)
                       (visicon-update vis-mod nil))
                   
                   (print-warning "Invalid cursor ~s found when updating the cursor feature." it)))))))

       

(defun xy-loc (chunk vis-mod)
  (let ((slots (vis-loc-slots vis-mod)))
    (vector (fast-chunk-slot-value-fct chunk (first slots))
            (fast-chunk-slot-value-fct chunk (second slots)))))
          
(defun xyz-loc (chunk vis-mod)
  (let ((slots (vis-loc-slots vis-mod)))
    (vector (fast-chunk-slot-value-fct chunk (first slots))
            (fast-chunk-slot-value-fct chunk (second slots))
            (fast-chunk-slot-value-fct chunk (third slots)))))

(defgeneric visicon-update (vis-mod &optional count)
  (:documentation "To be called after every time the visicon changes."))

(defmethod visicon-update ((vis-mod vision-module) &optional (count t))
  (update-new vis-mod)
  (check-finsts vis-mod)
  (update-attended-loc vis-mod)
  (stuff-visloc-buffer vis-mod)
  (when count
    (hash-table-count (visicon vis-mod))))


(defgeneric update-attended-loc (vis-mod)
  (:documentation  "If the attended location needs an update, update."))

(defmethod update-attended-loc ((vis-mod vision-module))
  ;; if we're tracking or moving around, ignore this 
  (when (or (tracked-obj-last-feat vis-mod) (moving-attention vis-mod)
            (eq 'BUSY (exec-s vis-mod)))
    (return-from update-attended-loc nil))
  ;; when do we update?
  ;; [1] when we're looking at an object and it's gone
  ;; [2] when we're looking at nothing and something appears 
  (when (or (and (currently-attended vis-mod)
                 (not (gethash (currently-attended vis-mod) (visicon vis-mod))))
            (and (current-marker vis-mod)
                 (null (currently-attended vis-mod))
                 (within-move vis-mod (xyz-loc (current-marker vis-mod) vis-mod))))
    
    ;; Change it now to avoid problems with simultaneous non-scheduled updates
    
    (change-state vis-mod :exec 'busy)
    
    ;; Still want to schedule the change so that it gets recorded properly
    ;; for module tracking purposes.
    
    (schedule-event-now 'change-state :params (list vis-mod :exec 'busy) :module :vision :output nil :priority :max)
    
    (schedule-event-relative 
     (randomize-time-ms (move-attn-latency vis-mod))
     'encoding-complete
     :time-in-ms t
     :destination :vision
     :module :vision
     :params `(,(current-marker vis-mod) ,(last-scale vis-mod) :requested nil)
     :details (concatenate 'string "Encoding-complete " (symbol-name (current-marker vis-mod)) " "  (symbol-name (last-scale vis-mod)))
     :output 'medium)))


(defmethod stuff-visloc-buffer ((vis-mod vision-module))
  (let ((old (buffer-read 'visual-location)))
    (when (and (or (null old)
                   (and (overstuff-loc vis-mod)
                        ;; just check unmodified and stuffed
                        ;; since can't compare to visicon because
                        ;; the old feature may be gone now
                        (multiple-value-bind (x unmodified-copy) (chunk-copied-from-fct old) (declare (ignore x)) unmodified-copy)
                        (query-buffer 'visual-location '(buffer unrequested))))
             (zerop (locks (current-device-interface)))
             (not (tracked-obj-last-feat vis-mod)))
      (awhen (find-current-locs-with-spec vis-mod (default-spec vis-mod))
             (let ((chunk (random-item (sort (objs-max-val it 'chunk-visual-tstamp) 'string< :key 'symbol-name))))
               (unless (and old (equal-chunks-fct old chunk)) ;; don't stuff the same chunk again
                 
                 (if old
                     (schedule-overwrite-buffer-chunk 'visual-location chunk 0 :time-in-ms t :module :vision :requested nil :priority 10)
                   (schedule-set-buffer-chunk 'visual-location chunk 0 :time-in-ms t :module :vision :requested nil :priority 10))
             
                 (lock-device (current-device-interface))
               
                 (schedule-event-now 'unlock-device :module :vision :destination :device
                                          :priority 9 :output nil :maintenance t)
                 
                 (when (unstuff-loc vis-mod)
                   (let ((delay (if (numberp (unstuff-loc vis-mod)) (unstuff-loc vis-mod) (new-span vis-mod))))
                     
                     (awhen (unstuff-event vis-mod)
                            (delete-event it))
                     (setf (unstuff-event vis-mod)
                       (schedule-event-relative delay 'unstuff-buffer :maintenance t :params (list 'visual-location chunk) 
                                                :destination :vision :output nil :time-in-ms t
                                                :precondition 'check-unstuff-buffer))))))))))
  

(defun test-attended (vm attended-spec chunk)
  "Assume it's a visual-location chunk, but could be either
   the key or value from the visicon or it could be an unrelated location chunk"
  
  ;; Let the unrelated chunk match to attended nil now
  ;; instead of failing all queries as was the case previously
  
  (let* ((visicon-key (chunk-visicon-entry chunk))
         (visicon-value (aif (and visicon-key (gethash visicon-key (visicon vm)))
                             it
                             chunk))
         (value (third attended-spec))
         (result nil)
         (marker (feat-attended visicon-value vm)))
    
    (cond ((eq value 'new)
           (setf result (eq marker 'new)))
          ((null value)
           (setf result (or (eq marker 'new) (eq marker nil))))
          (t
           (setf result (eq marker t))))
        
    (if (eq (car attended-spec) '-)
        (not result)
      result)))

(defun matching-attended-chunks (vm attended-spec chunks)
  (remove-if-not (lambda (x) (test-attended vm attended-spec x)) chunks))

(defun set-visual-center-point (x y)
  (aif (get-module :vision)
      (if (and (numberp x) (numberp y))
          (setf (center-point it) (vector x y))
        (print-warning "X and Y values must be numbers for set-visual-center-point."))
    (print-warning "No vision module available so cannot set center point.")))

;;; Because screen y coordinates are "upside down" the results are backwards and
;;; nearest clockwise is max angle-between and nearest counterclockwise is min angle-between
;;; where p1 is the start point and p2 is the target computed relative to center.
;;; Could compensate for that by inverting the y calculations but doesn't seem necessary.

(defun 2-points->angle (center point)
  (atan (- (py point) (py center)) (- (px point) (px center))))

(defun angle-between (p1 p2 center)
  (let ((a (- (2-points->angle center p1) (2-points->angle center p2))))
    (if (minusp a)
        (+ (* 2 pi) a)
        a)))


(defmethod find-current-locs-with-spec ((vis-mod vision-module) spec)
  "Assume that it's a valid visual-location chunk-spec with at most 1
   attended slot specification one nearest spec and one center spec"
  
  (let* ((attended (first (chunk-spec-slot-spec spec :attended)))
         (slots (remove-if 'keywordp (chunk-spec-slot-spec spec) :key 'spec-slot-name))
         (current (current-marker vis-mod))
         (current-slots (when current (chunk-filled-slots-list-fct current)))
         (nearest (spec-slot-value (first (chunk-spec-slot-spec spec :nearest))))
         (min-max-tests nil))
      
    ;; Remap all current values to the current chunk
    
    (if current
        (dolist (x slots)
          (when (eq (spec-slot-value x) 'current)
            (if (find (spec-slot-name x) current-slots)
                (if (and (eq (spec-slot-name x) 'value) (chunk-real-visual-value current)) 
                   (setf (spec-slot-value x) (chunk-real-visual-value current)) 
                  (setf (spec-slot-value x) (chunk-slot-value-fct current (spec-slot-name x))))
              (progn
                (print-warning "Current visual-location does not have a slot named ~S so it is ignored in the request." (spec-slot-name x))
                (setf slots (remove x slots))))))
      (dolist (x slots)
        (when (eq (spec-slot-value x) 'current)
          (print-warning "There is no currently attended location.  So, request specifying ~S as current is being ignored." (spec-slot-name x))
          (setf slots (remove x slots)))))
    
    ;; Remove all tests for highest and lowest for later
    
    (dolist (x slots)
      (when (or (eq (spec-slot-value x) 'lowest)
                (eq (spec-slot-value x) 'highest))
        (push-last x min-max-tests)
        (setf slots (remove x slots))))
    
    ;; update the finsts and new markers if attended is needed
    
    (when attended
      (update-new vis-mod)
      (check-finsts vis-mod))
  
    ;; find the chunks that match
    
    (let ((possible-chunks (if attended
                               (matching-attended-chunks vis-mod attended (visicon-chunks vis-mod t))
                             (visicon-chunks vis-mod t)))
          (changed nil)
          (coord-slots (vis-loc-slots vis-mod)))
      
      ;; Hack to reassign value slots as needed before testing
      
      (dolist (check possible-chunks)
        (when (chunk-real-visual-value check)
          (push (cons check (chunk-slot-value-fct check 'value)) changed)
          (fast-set-chunk-slot-value-fct check 'value (chunk-real-visual-value check))))
      
      (let ((matching-chunks (find-matching-chunks (slot-specs-to-chunk-spec slots)
                                                   :chunks possible-chunks :variable-char #\&)))
        ;; apply all of the lowest/highest constraints
        ;; in the order provided
        
        (dolist (x min-max-tests)
          (let ((value nil)
                (op (spec-slot-op x))
                (slot (spec-slot-name x))
                (test (spec-slot-value x)))
            
            ;; find the min/max value
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
        
        ;; if there's a nearest constraint then
        ;; apply that filter now
        
        (when (and nearest matching-chunks)
          (cond ((find nearest '(current-x current-y current-distance))
                 (let ((current-val (aif current 
                                         (chunk-slot-value-fct it (case nearest 
                                                                    (current-x (first coord-slots)) 
                                                                    (current-y (second coord-slots)) 
                                                                    (current-distance (third coord-slots))))
                                         (progn 
                                           (model-warning "No location has yet been attended so current is assumed to be at 0,0,~d." (view-dist vis-mod))
                                           (case nearest (current-x 0) (current-y 0) (current-distance (view-dist vis-mod)))))))
                   ;; find those matching min value
                   
                   (setf matching-chunks (objs-min-val matching-chunks 
                                                       (lambda (x) 
                                                         (abs (- current-val (chunk-slot-value-fct x
                                                                                                   (case nearest 
                                                                                                     (current-x (first coord-slots)) 
                                                                                                     (current-y (second coord-slots)) 
                                                                                                     (current-distance (third coord-slots)))))))))))
                
                ((find nearest '(clockwise counterclockwise))
                 (let* ((center-spec (spec-slot-value (first (chunk-spec-slot-spec spec :center))))
                        (center (cond ((or (null center-spec)
                                           (not (chunk-p-fct center-spec)))
                                       (center-point vis-mod))
                                      ((valid-vis-loc-chunk center-spec vis-mod)
                                       (xy-loc center-spec vis-mod))
                                      ((and (chunk-slot-value-fct center-spec 'screen-pos)
                                            (valid-vis-loc-chunk (chunk-slot-value-fct center-spec 'screen-pos) vis-mod))
                                       (xy-loc (chunk-slot-value-fct center-spec 'screen-pos) vis-mod))
                                      (t
                                       (center-point vis-mod))))
                        (current-point (aif (current-marker vis-mod) 
                                            (xy-loc it vis-mod)
                                            (progn 
                                              (model-warning "No location has yet been attended so current is assumed to be at 0,0.")
                                              (vector 0 0)))))
                    
                   (setf matching-chunks (if (eq nearest 'clockwise)
                                             (objs-max-val matching-chunks 
                                                           (lambda (x) 
                                                             (angle-between current-point (xy-loc x vis-mod) center)))
                                           (objs-min-val matching-chunks 
                                                         (lambda (x) 
                                                           (angle-between current-point (xy-loc x vis-mod) center)))))))     
                ((eq nearest 'current)
                 (let ((nearest-coords (aif (current-marker vis-mod) 
                                            (xyz-loc it vis-mod)
                                            (progn 
                                              (model-warning "No location has yet been attended so current is assumed to be at 0,0,~d." (view-dist vis-mod))
                                              (vector 0 0 (view-dist vis-mod))))))
                   
                   (setf matching-chunks (objs-min-val matching-chunks 
                                                       (lambda (x) 
                                                         (dist (xyz-loc x vis-mod) nearest-coords))))))
                
                ((valid-vis-loc-chunk nearest vis-mod)
                 (let ((nearest-coords (if (numberp (chunk-slot-value-fct nearest (third coord-slots)))
                                            (xyz-loc nearest vis-mod)
                                         (vector (chunk-slot-value-fct nearest (first coord-slots))
                                                 (chunk-slot-value-fct nearest (second coord-slots))
                                                 (view-dist vis-mod)))))
                   
                   (setf matching-chunks (objs-min-val matching-chunks 
                                                       (lambda (x) 
                                                         (dist (xyz-loc x vis-mod) nearest-coords))))))
                
                (t
                 (print-warning "Nearest test in a visual-location request must be current, current-x, current-y, clockwise, counterclockwise, or a chunk with ~s and ~s coordinates." (first coord-slots) (second coord-slots))
                 (print-warning "Ignoring nearest request for ~S." nearest))))
        
        ;; undo the value slots that were changed for matching purposes
        
        (dolist (restore changed)
          (fast-set-chunk-slot-value-fct (car restore) 'value (cdr restore)))
        
        matching-chunks))))


(defun enter-into-visicon (vis-loc vis-mod)
  
  (let* ((existing (gethash (chunk-visicon-entry vis-loc) (visicon vis-mod)))
         (tstamp (if existing (chunk-visual-tstamp existing) (mp-time-ms)))
         (new (if existing (chunk-visual-new-p existing) 'new))
         (entry (copy-chunk-fct vis-loc)))
    
    (when existing
      ;;; Removed some useless finst code that was just setting id's
      ;;; to the same value they had.
      ;;; The finsts shouldn't have to be updated since the visicon-entry
      ;;; for the new item must be the same as the old for it to have 
      ;;; matched and detected an "existing"!
      
      ;; delete the old chunk if set to do so since it shouldn't be needed now
      
      (when (purge-visicon vis-mod)
        (unless (eq existing (current-marker vis-mod))
          (purge-chunk-fct existing))))
    
    (setf (gethash (chunk-visicon-entry vis-loc) (visicon vis-mod)) entry)
    
    (setf (chunk-visual-tstamp entry) tstamp)
    (setf (chunk-visual-new-p entry) new)
    
    entry))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Supporting NEW tags


(defun tstamp-elapsed (vis-loc)
  (- (mp-time-ms) (chunk-visual-tstamp vis-loc)))

(defun checknew (vis-loc vis-mod)
  (when (and (eq (chunk-visual-new-p vis-loc) 'NEW)
             (> (tstamp-elapsed vis-loc) (new-span vis-mod)))
    (setf (chunk-visual-new-p vis-loc) nil))
  vis-loc)

(defmethod update-new ((vis-mod vision-module))
  (mapc (lambda (vl) (checknew vl vis-mod)) (visicon-chunks vis-mod)))


;;;; FInsts

(defclass finst ()
  ((id :accessor id :initarg :id :initform nil) ;; the visicon key 
   (tstamp :accessor tstamp :initarg :tstamp :initform 0.0)
   (synthed-from :accessor synthed-from :initarg :synthed-from :initform nil)))

(defgeneric check-finsts (vis-mod)
  (:documentation  "Update finsts against what's on the display."))

(defmethod check-finsts ((vis-mod vision-module))
  (setf (finst-lst vis-mod)
    (delete-if (lambda (f) 
                 (or (and (not (synthed-from f))
                          (null (gethash (id f) (visicon vis-mod))))
                     (and (synthed-from f)
                          (not (some (lambda (x) (gethash x (visicon vis-mod))) (synthed-from f))))
                     (> (- (mp-time-ms) (tstamp f))
                        (finst-span vis-mod))))
               (finst-lst vis-mod))))


(defgeneric feat-attended (feat vis-mod)
  (:documentation  "Return the attended status of a visicon feature object."))

(defmethod feat-attended (loc vis-mod)
  (if (find (chunk-visicon-entry loc) (finst-lst vis-mod) :key (lambda (x) (cons (id x) (synthed-from x))) :test (lambda (item list) (member item list :test 'equal)))
      t
    (when (gethash (chunk-visicon-entry loc) (visicon vis-mod))
      (if (eq (chunk-visual-new-p loc) 'NEW)
          'NEW
        nil))))

(defun add-finst (vis-mod loc-or-obj) 
  (let* ((feature (chunk-visicon-entry loc-or-obj))
         (current (when feature (gethash feature (visicon vis-mod)))))
    (if current
        (progn
          (aif (find feature (finst-lst vis-mod) :key 'id :test 'equal)
               (setf (tstamp it) (mp-time-ms))
               
               (push
                (make-instance 'finst :id feature :tstamp (mp-time-ms)
                  :synthed-from (chunk-synth-feat loc-or-obj))
                (finst-lst vis-mod)))
          
          (aif (chunk-synth-feat loc-or-obj)
               (dolist (x it)
                 (setf (chunk-visual-new-p (gethash x (visicon vis-mod))) nil))
               (setf (chunk-visual-new-p current) nil))
          
          (when (> (length (finst-lst vis-mod)) (num-finst vis-mod))
            (sort-finsts vis-mod)
            (pop (finst-lst vis-mod))))
      (model-warning "~S does not name an object or feature in the current visicon. No finst created." loc-or-obj))))

(defun remove-finst (vis-mod loc-or-obj) 
  (let ((name (chunk-visicon-entry loc-or-obj)))
    (setf (finst-lst vis-mod) (remove name (finst-lst vis-mod) :key 'id :test 'equal))
    (setf (finst-lst vis-mod) (remove-if (lambda (x) (find name (synthed-from x) :test 'equal)) (finst-lst vis-mod)))))


(defgeneric assign-finst (vis-mod &key object location)
  (:documentation "Assign a finst to an object or location."))

(defmethod assign-finst ((vm vision-module) &key object location)
  
  (if (and (null object) (null location))
    (print-warning "ASSIGN-FINST called without a valid object or location")
    (add-finst vm (if object object location))))
    
  
(defgeneric sort-finsts (vis-mod)
  (:documentation  "Sort finsts according to time stamp."))

(defmethod sort-finsts ((vis-mod vision-module))
  (setf (finst-lst vis-mod) (sort (finst-lst vis-mod) '< :key 'tstamp)))



;;; OBJECT-PRESENT-P      [Method]
;;; Description : A visual object is present if its ID is still in the icon
;;;             : or if all the features from which it was synthesized are
;;;             : still in the icon.

(defgeneric object-present-p (vis-mod obj-id)
  (:documentation  "Returns NIL if the object ID passed to it is no longer in the icon."))

(defmethod object-present-p ((vis-mod vision-module) obj-id)
  (aif (chunk-synth-feat obj-id)
       ;; For an object synthesized from features check all the features
       (every (lambda (x) (gethash (chunk-visicon-entry x) (visicon vis-mod))) it)
       (gethash (chunk-visicon-entry obj-id) (visicon vis-mod))))


;;; WITHIN-MOVE      [Method]
;;; Date        : 99.03.29
;;; Description : Simply walk the icon and accumulate features that are within
;;;             : the movement tolerance.

;;; 2014.05.21
;;; The assumption with how this was is that the viewing vector was perpendicular
;;; to the viewing surface because the move distance was a circle about the target
;;; point.  Adding the distance dimension into the calculation assumes the same thing --
;;; the viewing vector is along the distance-axis through the xy-loc of the item.
;;; Then the items that match are those which fall within the cone defined by the 
;;; move-allowance angle.  It's a little ugly, but avoids having to determine the
;;; head position and deal with the fact that the screen is a plane (not all items
;;; are really at the same distance) and other real 3d stuff...

(defgeneric within-move (vis-mod xyz-loc)
  (:documentation "Return a list of icon feature within the move allowance of loc."))

(defmethod within-move ((vis-mod vision-module) xyz-loc)
  (let ((accum nil)
        (coord-slots (vis-loc-slots vis-mod)))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (or (and (= (move-allowance vis-mod) 0)
                              (= (px xyz-loc) (fast-chunk-slot-value-fct value (first coord-slots))) 
                              (= (py xyz-loc) (fast-chunk-slot-value-fct value (second coord-slots)))
                              (= (pz xyz-loc) (fast-chunk-slot-value-fct value (third coord-slots))))
                         (and (> (move-allowance vis-mod) 0)
                              (>= (move-allowance vis-mod) (pm-pixels-to-angle (dist (xy-loc value vis-mod) xyz-loc) (pz xyz-loc)))))
                 (push value accum)))
             (visicon vis-mod))
    accum))

(defmethod feat-match-xyz (feat-list xyz-loc vis-mod)
  (let ((outlis nil)
        (x (px xyz-loc))
        (y (py xyz-loc))
        (z (pz xyz-loc))
        (coord-slots (vis-loc-slots vis-mod)))
    (dolist (chunk feat-list)
      (when (and (= x (fast-chunk-slot-value-fct chunk (first coord-slots))) 
                 (= y (fast-chunk-slot-value-fct chunk (second coord-slots)))
                 (= z (fast-chunk-slot-value-fct chunk (third coord-slots))))
        (push chunk outlis)))
    outlis))


;;; ENCODING-COMPLETE      [Method]
;;; Description : Several things to do when focusing attention on a location.
;;;             : [1] Make the location attended, set state to FREE.

;;;             : [3] If there was nothing there, or we were non-conserving
;;;             :     get the thing at that location.
;;;             : [4] If there is something, synch it up with the location
;;;             :     chunk.
;;;             : [5] If requested, print it.

(defgeneric encoding-complete (vis-mod loc-dmo scale &key requested)
  (:documentation "When MOVE-ATTENTION completes, focus on a place with this."))

(defmethod encoding-complete ((vis-mod vision-module) loc scale &key (requested t))
  
  (setf (moving-attention vis-mod) nil)
  (change-state vis-mod :exec 'free :proc 'free)
  (setf (current-marker vis-mod) loc)
  (complete-request (last-visual-request vis-mod))

  (let ((return-obj (get-obj-at-location vis-mod loc scale)))
    
    (if return-obj
        (progn
          (set-attended vis-mod (chunk-visicon-entry return-obj))
          (attend-to-object vis-mod return-obj :requested requested)
          
          return-obj)
      
      (progn
        (clear-attended vis-mod)
        (setf (last-obj vis-mod) nil)
        (set-buffer-failure 'visual :ignore-if-full t :requested requested)
        (setf (attend-failure vis-mod) t)
      
        (schedule-event-now 'no-visual-object-found :maintenance t :module :vision :output 'medium :details "No visual-object found")
        
        nil))))


(defun icon-entry (vis-mod visual-object)
  "Given a visual object chunk return the visicon key of the location which it came from
   or the one that's in the visicon that best matches it now, or if there isn't one 
   which matches the 'original' then just return the one associated with the object now"
  (let* ((loc-name (fast-chunk-slot-value-fct visual-object 'screen-pos))
         (original (chunk-visicon-entry loc-name)))
    
    (cond ((gethash original (visicon vis-mod))
           original)
          ((chunk-p-fct original)
           
           (aif (car (find-matching-chunks (chunk-name-to-chunk-spec original) :chunks (visicon-chunks vis-mod)))
                (chunk-visicon-entry it)
                (chunk-visicon-entry visual-object)))
          (t
           (aif (car (find-matching-chunks (chunk-name-to-chunk-spec loc-name) :chunks (visicon-chunks vis-mod)))
                (chunk-visicon-entry it)
                (chunk-visicon-entry visual-object))))))

(defun no-visual-object-found ()
  "Dummy function to indicate failure to encode - someone may want to do something with this later"
  )


(defmethod attend-to-object ((vis-mod vision-module) obj &key (requested t))
  
  ;;; put the chunk in the buffer
  
  (schedule-set-buffer-chunk 'visual obj 0 :time-in-ms t :module :vision :priority 10 :requested requested)
  
  ;; record the object for tracking purposes
  
  (setf (last-obj vis-mod) obj)
  
  ;; update the time-stamp on the finst if it's already attended or
  ;; add a new finst if it's not
  
  (aif (member (icon-entry vis-mod obj) (finst-lst vis-mod) :key 'id :test 'equal)
       
       (setf (tstamp (first it)) (mp-time-ms))
       
       (add-finst vis-mod obj)))


(defgeneric get-obj-at-location (vis-mod loc scale)
  (:documentation  "Given a location and a scale, return a chunk representing what's there."))

(defmethod get-obj-at-location ((vis-mod vision-module) loc scale)
  (let ((xyz-loc (xyz-loc loc vis-mod)))
    
    (cond ((eq scale 'PHRASE)
           (get-phrase-at vis-mod loc))
          ((and (eq scale 'WORD) (not (optimize-p vis-mod)))
           (get-word-at-noopt vis-mod loc))
          (t
           (let ((feat-lis (within-move vis-mod xyz-loc)))
             (when (eq scale 'WORD)
               (setf feat-lis (text-feats feat-lis)))
             (when feat-lis
               (featlis-to-focus vis-mod loc feat-lis)))))))

(defun text-feats (feat-lst)
  "Given a list, return only those features which are TEXT features."
  (remove-if (lambda (f) (not (eq (chunk-slot-value-fct f 'kind) 'text))) feat-lst))


;;; FEATLIS-TO-FOCUS      [Method]

(defgeneric featlis-to-focus (vis-mod loc feat-lis)
  (:documentation  "Given the source location and a list of features, return the DMO that should be the focus."))

(defmethod featlis-to-focus ((vis-mod vision-module) loc feat-lis)
  (let* ((best-feature 
          (find-best-feature vis-mod feat-lis 
                             (aif (gethash (chunk-visicon-entry loc) (visicon vis-mod))
                                  (if (chunk-p-fct it)
                                      it
                                    loc)
                                  loc)))
         (dmo-lis (featlis-to-chunks vis-mod (feat-match-xyz feat-lis (xyz-loc best-feature vis-mod) vis-mod)))
         (return-chunk (determine-focus-dmo vis-mod dmo-lis best-feature)))
    
    ;; don't mark everything just the one that's being returned   (dolist (obj dmo-lis)
    
    (when return-chunk
      (set-chunk-slot-value-fct return-chunk 'screen-pos loc) ;;;  Should it use the cannonical loc instead? (gethash best-feature (visicon vis-mod)) 
      )
    
    return-chunk))

;;; DETERMINE-FOCUS-DMO      [Method]
;;; Date        : 99.03.29
;;; Description : Basically, look for a DMO with the same ID as the feature.
;;;             : If none, see if the DMO was synthesized from that feature.
;;;             : If none of those, return a random one.

(defgeneric determine-focus-dmo (vis-mod dmo-lst feat)
  (:documentation  "Determine which DMO corresponds to <feat>, which should be the 'best' feature."))

(defmethod determine-focus-dmo ((vis-mod vision-module) (dmo-lis list) feat)
  (when (= 1 (length dmo-lis))
    (return-from determine-focus-dmo (first dmo-lis)))
  (aif (member (chunk-visicon-entry feat) dmo-lis :key 'chunk-visicon-entry :test 'equal)
       (first it)
       (print-warning "Multiple matching chunks found for attention shift.  This should not happen.  Please report the warning to Dan.")))


;;; FEATLIS-TO-chunks      [Method]
;;; Date        : 99.03.29
;;; Description : Actually, some of the features could be CHAR-PRIMITIVE 
;;;             : features, in which case they're part of characters.  Save 
;;;             : those out and make a character out of 'em.

(defgeneric featlis-to-chunks (vis-mod feat-lis)
  (:documentation  "Given a list of features, make a chunk for each."))

(defmethod featlis-to-chunks ((vis-mod vision-module) (feat-lis list))
  (let ((primitive-feats nil)
        (chunk-lis nil))
    (dolist (feat feat-lis)
      ;; If it's a char primitive, push the feature, else push the feature chunk.
      (if (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
          (push feat primitive-feats)
        (let ((obj (convert-loc-to-object feat)))
          (if (chunk-p-fct obj)
              (progn
                (setf (chunk-visicon-entry obj) (chunk-visicon-entry feat))
                (push obj chunk-lis))
            (print-warning "vis-loc-to-obj returned ~S which is not a chunk." obj)))))
    (when primitive-feats
      (push (synthesize-letter vis-mod primitive-feats) chunk-lis))
    chunk-lis))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Synthesizing 


;;; GET-WORD-AT-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : Getting a word when optimizing is off involves calling
;;;             : SYNTHESIZE-WORD method, but we need to collect the 
;;;             : locations first, and also check the return if what was
;;;             : sent in was a DMO.

(defgeneric get-word-at-noopt (vis-mod loc-dmo)
  (:documentation  "Synthesize a word at the given location and synch it with the location."))

(defmethod get-word-at-noopt ((vis-mod vision-module) (loc symbol))
  (let ((xyz-loc (xyz-loc loc vis-mod)))
    (multiple-value-bind (locs xmin xmax) (adjoining-led-locs vis-mod xyz-loc)
      (when locs
        (let ((rtn-chunk (synthesize-word vis-mod locs (- xmax xmin) xyz-loc)))
          (when rtn-chunk
            (set-chunk-slot-value-fct rtn-chunk 'screen-pos loc)
            (values rtn-chunk xmin xmax)))))))


(defmethod get-word-at-noopt ((vis-mod vision-module) (loc vector))
  (multiple-value-bind (locs xmin xmax) (adjoining-led-locs vis-mod loc)
    (when locs
      (let ((rtn-chunk (synthesize-word vis-mod locs  (- xmax xmin) loc)))
        (when rtn-chunk
          (set-chunk-slot-value-fct rtn-chunk 'screen-pos loc)
          (values rtn-chunk xmin xmax))))))


;;; GET-WORD-DMOS-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : OK, when optimizing is off and a phrase needs to be built,
;;;             : the tricky bit is figuring out which locations you need
;;;             : to grab words from, since if you hit every x location, 
;;;             : you'll generate multiple copies of each word.

(defgeneric get-word-dmos-noopt (vis-mod x-lst y z)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing off."))

(defmethod get-word-dmos-noopt ((vis-mod vision-module) (x-ls list) y z)
  (let ((rtn-dmos nil)
        (curr-x -1))
    (dolist (x x-ls (remove nil (nreverse rtn-dmos)))
      (when (> x curr-x)
        (multiple-value-bind (word min max)
                             (get-word-at-noopt vis-mod (vector x y z))
          (declare (ignore min))
          (push word rtn-dmos)
          (setf curr-x max))))))


;;; GET-WORD-DMOS-OPT      [Method]
;;; Date        : 99.04.02
;;; Description : This is simpler when optimizing--just walk the xlist
;;;             : and accumulate words.
;;;             : Might be vestigial as of beta 6.

(defgeneric get-word-dmos-opt (vis-mod x-lst y z)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing on."))

(defmethod get-word-dmos-opt ((vis-mod vision-module) (x-ls list) y z)
  (let (accum)
    (dolist (x x-ls (nreverse accum))
      (dolist (feat (text-feats (feat-match-xyz (visicon-chunks vis-mod) (vector x y z) vis-mod)))
        (setf accum (append (featlis-to-chunks vis-mod (list feat)) accum))))))


;;; SYNTHESIZE-LETTER      [Method]
;;; Date        : 98.07.27
;;; Description : From a list of LED-style icon features, synthesize a letter
;;;             : feature and note the synthesis.  The real worker here is
;;;             : Mike Matessa's FIND-BEST-OBJECT function, which is based on
;;;             : "rational categorization" of the features into a letter.

(defgeneric synthesize-letter (vis-mod feats)
  (:documentation  "Build a DMO representing a letter from a list of LED features."))

(defmethod synthesize-letter ((vis-mod vision-module) (feats list))
  (let* ((base-feat (first feats))
         (letter (prob-best-character (active-cfs vis-mod) (mapcar (lambda (x) (chunk-slot-value-fct x 'value)) feats)))
         (return-chunk nil)
         (colors (mapcar (lambda (x) (chunk-slot-value-fct x 'color)) feats))
         (color (caar (sort (mapcar (lambda (x) (cons x (count x colors))) (remove-duplicates colors)) '> :key 'cdr))))
    
    (setf return-chunk
      (car (define-chunks-fct (list (list 'isa 'text 'value letter 'color color)))))
        
    (setf (chunk-visual-feature-name return-chunk) base-feat)
    (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry base-feat))
    (setf (chunk-synth-feat return-chunk) (mapcan (lambda (x) (list (chunk-visicon-entry x))) feats))
    return-chunk))


;;; SYNTHESIZE-WORD      [Method]
;;; Date        : 98.07.27
;;; Description : Given the list of contiguous locations, get the letter 
;;;             : at each location, and then build the word from the list
;;;             : of letters.

(defgeneric synthesize-word (vis-mod loc-lis width center)
  (:documentation  "Build a DMO representing a word from a location."))

(defmethod synthesize-word ((vis-mod vision-module) (loc-lis list) (width number) (center vector))
  (let ((return-chunk nil)
        (letter-chunks nil)
        (used-feats nil))
        
    (dolist (xloc loc-lis)
      (let* ((feats (feat-match-xyz (visicon-chunks vis-mod) xloc vis-mod))
             (letter-chunk (synthesize-letter vis-mod feats)))
        
        (when (stringp (chunk-slot-value-fct letter-chunk 'value))
          (push letter-chunk letter-chunks)
          (setf used-feats (append feats used-feats)))))
    
    (when letter-chunks
      (let* ((colors (mapcar (lambda (x) (chunk-slot-value-fct x 'color)) letter-chunks))
             (color (caar (sort (mapcar (lambda (x) (cons x (count x colors))) (remove-duplicates colors)) '> :key 'cdr))))
        
        (setf letter-chunks (nreverse letter-chunks))
        (setf return-chunk
          (car (define-chunks-fct `((isa text color ,color value ,(word-accum 
                                                                   (mapcar (lambda (x) (chunk-slot-value-fct x 'value))
                                                                     letter-chunks)))))))
        
        
        (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry (car used-feats)))
        (setf (chunk-visual-feature-name return-chunk) (car used-feats))
        (setf (chunk-synth-feat return-chunk) (mapcan (lambda (x) (list (chunk-visicon-entry x))) used-feats))
        
        return-chunk))))


(defun word-accum (lis)
  "Accumulate a list of letters into a word, and downcase it."
  (let ((accum (first lis)))
    (dolist (item (rest lis) (string-downcase accum))
      (setf accum (mkstr accum item)))))

;;; GET-PHRASE-AT      [Method]
;;; Date        : 98.07.27
;;; Description : The only way to get a phrase is to synthesize one, there is
;;;             : no "phrase" primitive in RPM.


(defmethod get-phrase-at ((vis-mod vision-module) (loc symbol))
  (awhen (find-matching-chunks (define-chunk-spec-fct `(isa visual-location ,(second (vis-loc-slots vis-mod)) ,(py (xy-loc loc vis-mod))))
                               :chunks (visicon-chunks vis-mod))
         
         (synthesize-phrase vis-mod it loc)))


(defgeneric synthesize-phrase (vis-mod feature-locs loc)
  (:documentation  "Build a DMO representing a phrase."))

(defmethod synthesize-phrase ((vis-mod vision-module) (feature-locs list) (loc symbol))
  (let* ((xyz-loc (xyz-loc loc vis-mod))
         (x-slot (first (vis-loc-slots vis-mod)))
         (x-locs (mapcar (lambda (x) (chunk-slot-value-fct x x-slot)) feature-locs))
         (word-chunks nil) 
         (words nil)
         (colors nil)
         (return-chunk nil))
    (setf x-locs (sort (remove-duplicates x-locs) '<))
    
    (if (optimize-p vis-mod)
      (setf word-chunks (get-word-dmos-opt vis-mod x-locs (py xyz-loc) (pz xyz-loc)))
      (setf word-chunks (get-word-dmos-noopt vis-mod x-locs (py xyz-loc) (pz xyz-loc))))
    
    (when word-chunks
      (setf words (mapcar (lambda (x)
                            (chunk-slot-value-fct x 'value)) 
                    word-chunks))
      
      (dolist (w word-chunks)
        (let ((c (chunk-slot-value-fct w 'color)))
          
          (aif (assoc c colors) 
               (incf (cdr it))
               (push-last (cons c 1) colors))))
           
      (setf return-chunk
        (car (define-chunks-fct 
                 `((isa phrase! value ,(phrase-accum words)
                        objects ,word-chunks 
                        colors ,(mapcar (lambda (x) (chunk-slot-value-fct x 'color)) word-chunks)
                        words ,words
                        color ,(car (rassoc (apply 'max (mapcar 'cdr colors)) colors))
                        screen-pos ,loc)))))
      
      (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry loc))
      (setf (chunk-visual-feature-name return-chunk) loc) 
      
      (setf (chunk-synth-feat return-chunk) 
        (if (optimize-p vis-mod)
            (mapcan (lambda (x) (list (chunk-visicon-entry x))) word-chunks)
          (mapcan (lambda (x) (chunk-synth-feat x)) word-chunks)))
      
      return-chunk)))

(defun phrase-accum (lis)
  "Accumulate a list of words into a phrase."
  (let ((accum (first lis)))
    (dolist (item (rest lis) accum)
      (setf accum (mkstr accum " " item)))))

;;;;;;;;;;
;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Those wacky LED features for letters.
;;;; ---------------------------------------------------------------------- ;;;;


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
  (let ((feat-ls (feat-match-xyz (visicon-chunks vis-mod) loc vis-mod))
        (feat nil)
        (y-slot (second (vis-loc-slots vis-mod))))
    (while (and (null feat) feat-ls)
      (setf feat (pop feat-ls))
      (unless (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
        (setf feat nil)))
    (when feat
      (setf feat-ls (find-matching-chunks (define-chunk-spec-fct `(isa visual-location ,y-slot ,(chunk-slot-value-fct feat y-slot)))
                                          :chunks (visicon-chunks vis-mod)))
      (multiple-value-bind 
        (lowlocs xmin) (left-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'left) nil)
        (multiple-value-bind
          (hilocs xmax) (right-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'right) nil)
          (values
           (append lowlocs (list loc) hilocs)
           xmin xmax))))))


;;; RIGHT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go right and accumulate locations that share
;;;             : the boundary.

(defgeneric right-adjoining-led-locs (feat-ls vis-mod x accum)
  (:documentation  "Return a list of all the right-adjoining locs with led features and the min x."))

(defmethod right-adjoining-led-locs ((feat-ls list) (vis-mod vision-module) x accum)
  (dolist (feat feat-ls)
    (when (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
      (when (> 1.5 (abs (- (chunk-slot-value-fct feat 'left) x)))
        (return-from right-adjoining-led-locs
          (right-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'right)
                                    (append accum (list (xyz-loc feat vis-mod))))))))
  (values accum x))


;;; LEFT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go left and accumulate locations that share
;;;             : the boundary.

(defgeneric left-adjoining-led-locs (feat-ls vis-mod x accum)
  (:documentation  "Return a list of all the left-adjoining locs with led features and the max x."))

(defmethod left-adjoining-led-locs ((feat-ls list) (vis-mod vision-module) x accum)
  (dolist (feat feat-ls)
    (when (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
      (when (> 1.5 (abs (- (chunk-slot-value-fct feat 'right) x)))
        (return-from left-adjoining-led-locs
          (left-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'left)
                                   (append (list (xyz-loc feat vis-mod)) accum))))))
  (values accum x))




;;; FIND-BEST-FEATURE      [Method]
;;; Date        : 99.03.29
;;; Description : The "best" feature is the one with the same ID as the spec. 
;;;             : The next best feature is one that matches the spec and is
;;;             : nearest to that location.  Failing any matches to the spec,
;;;             : then return the nearest feature.

(defgeneric find-best-feature (vis-mod feat-lst fs)
  (:documentation  "Given a list of features and a visicon entry or visual-loc, return the 'best' feature."))

(defmethod find-best-feature (vis-mod (feat-lis list) entry)
  (aif (find entry feat-lis)
       it
       (let ((matches (aif (gethash entry (found-locs vis-mod)) 
                           (find-matching-chunks it :chunks feat-lis :variable-char #\&)
                           (let ((spec nil)
                                 (positions (vis-loc-slots vis-mod)))
                             (dolist (s (chunk-filled-slots-list-fct entry))
                               (unless (find s positions)
                                 (push (cons s (fast-chunk-slot-value-fct entry s)) spec)))
                             (objs-max-val feat-lis (lambda (x) 
                                                      (let ((c 0)) 
                                                        (dolist (s spec c)
                                                          (when (chunk-slot-equal (cdr s) (chunk-slot-value-fct x (car s)))
                                                            (incf c))))))))))
         (if matches
             (random-item (nearest-feat vis-mod matches (xyz-loc entry vis-mod)))
           (random-item (nearest-feat vis-mod feat-lis (xyz-loc entry vis-mod)))))))


(defun nearest-feat (vis-mod feat-list xyz-loc)
  "Returns list of features nearest to a given location"
  (unless (vectorp xyz-loc)
    (setf xyz-loc (xyz-loc xyz-loc vis-mod)))
  (when feat-list
    (let ((feat-chunks (mapcan (lambda (x) (aif (gethash (chunk-visicon-entry x) (visicon vis-mod)) (list it) nil)) feat-list)))
           
      (objs-min-val feat-chunks (lambda (x) (dist (xyz-loc x vis-mod) xyz-loc))))))


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

Additional ACT-R 6 mechanism to properly deal with buffer issues.

Whenever there's a change to the display the buffers will be updated as follows:

   First, if the build-features-for returns nil then assume
   that the tracked object is out of sight now.  As with an
   encoding failure, clear visual, set the error state, and
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
   its loc. is in the vis-loc buffer).

  both buffers hold the appropriate chunks:

   Modify the chunk in the visual-location buffer,
   no other changes necessary.

  If either buffer holds a chunk that isn't related
  to the tracked item:

   Make the appropriate updates to the underlying chunks
   as needed (modify the chunk in the buffer if it's
   the tracked one or create/modify the internal one)
   but don't overwrite a non-tracked chunk in a buffer.
|#

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
      (setf (tracked-obj vis-mod) (chunk-visual-object (gethash (currently-attended vis-mod) (visicon vis-mod))))
      (setf (tracked-obj-lastloc vis-mod) nil)  ;; don't assume anything about vis-loc buffer at this point
      (let ((vis-obj (buffer-read 'visual))) ;; should always be empty since the request clears it but
                                             ;; if not record it for later checking
        (if (and vis-obj (eq (chunk-copied-from-fct vis-obj) (last-obj vis-mod)))
            (setf (tracked-obj-last-obj vis-mod) vis-obj)
          (setf (tracked-obj-last-obj vis-mod) nil)))
      (setf (tracked-obj-last-feat vis-mod) (currently-attended vis-mod))
      (update-tracking-mth vis-mod)
      
      (tracked-obj vis-mod))))


;;; When tracking it may be necessary to get the name of
;;; the chunk from the vis-loc buffer to set the internal
;;; information correctly.  This method does that, but
;;; it must be scheduled approproately to ensure the right
;;; chunk gets recorded.  
;;; It could be possible for something else to intrude and
;;; mess this up, but the default mechanisms shouldn't 
;;; result in problems.

(defmethod update-tracking-loc-chunk ((vis-mod vision-module) &optional (modify nil))
  (let ((vis-loc (buffer-read 'visual-location)))
    
    (setf (tracked-obj-lastloc vis-mod) vis-loc)
    (setf (current-marker vis-mod) vis-loc)
    
    (when modify 
      (mod-buffer-chunk 'visual (list 'screen-pos vis-loc)))))

;;; Record the visual object chunk placed into the buffer
;;; for later comparisons when needed

(defmethod update-tracking-obj-chunk ((vis-mod vision-module))
  (setf (tracked-obj-last-obj vis-mod) (buffer-read 'visual)))


(defgeneric update-tracking-mth (vis-mod &optional from-proc-display)
  (:documentation  "Update the state of a tracked object"))

(defmethod update-tracking-mth ((vis-mod vision-module) &optional from-proc-display)
  
  (flet ((tracking-failed ()
                          (schedule-clear-buffer 'visual 0 :time-in-ms t :module :vision :output 'high :priority 13)
                          (change-state vis-mod :exec 'free :proc 'free)
                          (clear-attended vis-mod)
                          (setf (current-marker vis-mod) nil)
                          (set-buffer-failure 'visual :ignore-if-full t)
                          (setf (attend-failure vis-mod) t)
                          (setf (tracked-obj vis-mod) nil)
                          (setf (tracked-obj-last-feat vis-mod) nil)
                          (setf (last-obj vis-mod) nil)
                          (return-from update-tracking-mth nil)))
    (let ((devin (current-device-interface)))
      
      ;; Don't change anything now if there's a lock on the device.
      
      (unless (zerop (locks devin))
        (push :tracking (pending-procs devin))
        (return-from update-tracking-mth nil))
      
      (let* ((new-feat ;; the feature's visicon entry -- chunk name or feature list            
              (if from-proc-display
                  
                  ;; The visicon contains the updated info
                  ;; just get the appropriate one out
                  
                  (if (tracked-obj vis-mod)
                      ;; find the one with the same object
                      (let ((f (find (tracked-obj vis-mod) (visicon-chunks vis-mod) :key 'chunk-visual-object)))
                        (when f
                          (chunk-visicon-entry f)))
                    ;; If it's not object based it must have the same visual-location
                    ;; chunk of the feature originally tracked so make sure that's still
                    ;; a feature in the visicon
                    (when (gethash (tracked-obj-last-feat vis-mod) (visicon vis-mod))
                      (tracked-obj-last-feat vis-mod)))
                
                ;; proc-display wasn't called which only happens when the
                ;; tracking starts now so just use the feature that was
                ;; valid at that point.
                
                (tracked-obj-last-feat vis-mod)))
             
             (new-loc (when new-feat (gethash new-feat (visicon vis-mod)))) ;; name of the chunk
             (old-loc (tracked-obj-lastloc vis-mod))
             (old-obj (tracked-obj-last-obj vis-mod))
             (vis-loc-chunk (buffer-read 'visual-location))
             (vis-obj-chunk (buffer-read 'visual)))
        
        (unless new-loc
          (tracking-failed))
        
        (setf (tracked-obj-last-feat vis-mod) new-feat)
        (setf (current-marker vis-mod) new-loc)
        
        (let ((new-obj (convert-loc-to-object new-loc)))
          
          (unless new-obj ;; for some reason we have a location but no object
            (tracking-failed))
          
          (setf (currently-attended vis-mod) new-feat)
          (setf (last-obj vis-mod) new-obj)        
          
          ;; For the following events need to set priority of the buffer setting
          ;; so that if there's a find-location scheduled but not completed this 
          ;; happens first, so that the find-loc overwrites.  Thus the priorities > 10.

          (flet ((set-vis-loc ()
                              (schedule-set-buffer-chunk 'visual-location new-loc 0 :time-in-ms t :module :vision 
                                                         :output 'high :requested nil :priority 15)
                              ;; need to make sure the chunk being set in the buffer isn't deleted before it gets there
                              (when from-proc-display
                                (lock-device (current-device-interface))
                                (schedule-event-now 'unlock-device :module :vision
                                                         :destination :device :priority 14
                                                         :output nil :maintenance t)))
                 (mod-vis-loc ()
                              (schedule-mod-buffer-chunk 'visual-location (chunk-difference-to-chunk-spec new-loc (buffer-read 'visual-location)) 0
                                                         :time-in-ms t :module :vision :output 'high :priority 15))
                 (set-vis-obj ()
                              (schedule-set-buffer-chunk 'visual new-obj 0 :time-in-ms t :module :vision 
                                                         :output 'high :requested nil :priority 14))
                 (mod-vis-obj ()
                              (schedule-mod-buffer-chunk 'visual (chunk-difference-to-chunk-spec new-obj (buffer-read 'visual)) 0
                                                         :time-in-ms t :module :vision :output 'high :priority 14))
                 (update-loc (mod)
                             (schedule-event-now 'update-tracking-loc-chunk :module :vision
                                                 :destination :vision :params (if mod (list t) nil) :priority 13 :output nil))
                 (update-obj ()
                             (schedule-event-now 'update-tracking-obj-chunk :module :vision
                                                 :destination :vision :params nil :priority 12 :output nil)))
            
            ;;; Make sure there's still a finst on the tracked item
            
            (aif (member new-feat (finst-lst vis-mod) :key 'id :test 'equal)
                 (setf (tstamp (first it)) (mp-time-ms))
                 (add-finst vis-mod new-loc))
            
            (cond ((and (null vis-loc-chunk)
                        (null vis-obj-chunk))
                   ;; Stuff both buffers and then update the obj with the buffer-chunk's name
                   (set-vis-loc)
                   (set-vis-obj)
                   (update-loc t)
                   (update-obj))
                  
                  ((and (null vis-loc-chunk)
                        (eq vis-obj-chunk old-obj))
                   ;; stuff the new location and modify the visual buffer with the new info
                   (set-vis-loc)
                   (mod-vis-obj)
                   (update-loc t))
                  
                  ((null vis-loc-chunk) 
                   ;; stuff a new location chunk and don't touch the chunk in the visual buffer
                   (set-vis-loc)
                   (update-loc nil))
                  
                  ((and (eq vis-loc-chunk old-loc)
                        (null vis-obj-chunk))
                   ;; Modify the chunk in the visual-location buffer put new obj into visual buffer
                   (mod-vis-loc)
                   (set-vis-obj)
                   (update-loc t)
                   (update-obj))
                  
                  ((and (eq vis-loc-chunk old-loc)
                        (eq vis-obj-chunk old-obj))
                   ;; Modify both chunks and make sure the obj points to the right loc just to be safe.
                   (mod-vis-loc)
                   (mod-vis-obj)
                   (update-loc t))
                  
                  ((eq vis-loc-chunk old-loc) 
                   ;; just modify the loc and don't know about the visual buffer
                   (mod-vis-loc))
                  
                  ((null vis-obj-chunk) 
                   ;; Don't know about the vis-loc buffer just put the new object in place 
                   ;; setting the screen-pos if it isn't
                   
                   (unless (chunk-slot-value-fct new-obj 'screen-pos)
                     (set-chunk-slot-value-fct new-obj 'screen-pos new-loc))
                   
                   (set-vis-obj)
                   (update-obj))
                
                  ((eq vis-obj-chunk old-obj) 
                   ;; Just modify the object chunk and set the screen-pos if necessary
                 
                   (unless (chunk-slot-value-fct new-obj 'screen-pos)
                     (set-chunk-slot-value-fct new-obj 'screen-pos new-loc))
                   (mod-vis-obj))
                
                (t ;; Don't do anything 
                 ))))))
    nil))


;;; REMOVE-TRACKING      [Method]
;;; Date        : 97.05.15
;;; Description : When tracking stops, the slots for tracked objects need to
;;;             : to be cleared, and the ACT hook functions need to be cleared.

(defgeneric remove-tracking (vis-mod)
  (:documentation  "Clears out all the tracking stuff"))

(defmethod remove-tracking ((vis-mod vision-module))
  (setf (tracked-obj-last-feat vis-mod) nil)
  (setf (tracked-obj-lastloc vis-mod) nil)
  (setf (tracked-obj-last-obj vis-mod) nil)
  (setf (tracked-obj vis-mod) nil)
  (change-state vis-mod :exec 'FREE))


;;; UPDATE-TRACKING      [Function]
;;; Date        : 97.05.15
;;; Description : To be called by the user to indicate movement.

(defun update-tracking ()
  "Call the Vision Module's tracking update method."
  (print-warning "Update-tracking has been depricated.  All updates must be done through proc-display or the add/delete/update actions."))

;;; This is not a user command -- forcing an explicit update can
;;; lead to invalid finsts and it won't pick up any new info until
;;; that change has occured through proc-display or update-visicon-item.

(defun unlock-tracking ()
  (update-tracking-mth (get-module :vision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric set-cfs-mth (vis-mod kwrd)
  (:documentation "Set the current character feature set."))

(defmethod set-cfs-mth ((vis-mod vision-module) kwrd)
  (aif (get-cfs-mth vis-mod kwrd)
       (setf (active-cfs vis-mod) (first it))
       (model-warning "Feature set ~S is unknown" kwrd)))


(defgeneric get-cfs-mth (vis-mod kwrd)
  (:documentation "Given a keyword, find a character feature set."))

(defmethod get-cfs-mth ((vis-mod vision-module) kwrd)
  (member kwrd (feature-sets vis-mod) :key 'name))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod clear :after ((vis-mod vision-module))
  (remove-tracking vis-mod)
  (setf (last-obj vis-mod) nil)
  (setf (loc-failure vis-mod) nil)
  (setf (attend-failure vis-mod) nil)
  (setf (scene-change vis-mod) nil))


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

(defgeneric find-location (vis-mod chunk-spec)
  (:documentation  "Given a set of constraints, build a DMO for that screen location if one matches."))

(defmethod find-location ((vis-mod vision-module) chunk-spec)
  (let ((loc (awhen (find-current-locs-with-spec vis-mod chunk-spec)
                    (construct-location vis-mod (random-item (objs-max-val it 'chunk-visual-tstamp)) chunk-spec))))
    (if loc
        (progn
          (setf (loc-failure vis-mod) nil)
          (schedule-set-buffer-chunk 'visual-location loc 0 :time-in-ms t :module :vision :priority 10)
          (lock-device (current-device-interface))
          (schedule-event-now 'unlock-device 
                              :module :vision
                              :destination :device
                              :priority 9
                              :output nil
                              :maintenance t))
      (progn
        (set-buffer-failure 'visual-location)
        (setf (loc-failure vis-mod) t)
        (schedule-event-now 'find-loc-failure :module :vision :output 'low)))
    
    (when (and loc (auto-attend vis-mod))
      (schedule-event-now 'visual-auto-attend :destination :vision :output t
                               :module :vision :details (concatenate 'string "automatically attending " (symbol-name loc))))
    loc))


(defmethod visual-auto-attend ((vis-mod vision-module))
  (aif (buffer-read 'visual-location)
       (progn   
         ;; mark the module as busy between now and the attention shift
         (change-state vis-mod :exec 'BUSY :proc 'BUSY)
         
         ;; this isn't requested so may want to clear the last-visual-request
         ;; but don't really need to since the last one should already be
         ;; clear and a value of nil (if it wasn't set) would be fine too
         ;; (setf (last-visual-request vis-mod) nil)
         
         ;; schedule the attention shift to be 50ms from now - should probably use the default action time
         ;; instead but not going to do that at this point...
         
         (schedule-event-relative 50 'move-attention :time-in-ms t :params (list :location it) :destination :vision :output 'medium
                                  :module :vision :details (concatenate 'string "Move-attention " (symbol-name it)) :priority 0)
         
         ;; Hack to clear the module state just before the attention shift so as not to 
         ;; jam things.
         
         (schedule-event-relative 50 'change-state :time-in-ms t :params (list :exec 'free :proc 'free) :destination :vision :output nil
                                  :module :vision :priority 1))
       
       (model-warning "Auto-attend failed because visual-location buffer was empty.")))


(defun find-loc-failure ()
  "Dummy event function to signal a find-location failure in the trace"
  )



(defgeneric construct-location (vis-mod feat spec)
  (:documentation  "Find or build a DMO based on a feature and the spec used to generate that feature."))

(defmethod construct-location ((vis-mod vision-module) loc spec)
  
  ;; record that this loc was found with this spec
  (setf (gethash (chunk-visicon-entry loc) (found-locs vis-mod)) spec)
  loc)

;;; MOVE-ATTENTION      [Method]
;;; Date        : 97.02.09
;;; Description : This is a toplevel command for the Vision Module, causing
;;;             : attention to move.  Latencey is 185 ms, so nothing actually
;;;             : happens right away other than setting state variables.
;;;             : A method is dispatched based on the scale that's passed.

(defgeneric move-attention (vis-mod &key location scale)
  (:documentation "Shift attention to a particular location at a particular scale."))

(defmethod move-attention ((vis-mod vision-module) &key location scale)
  (cond ((and (eq (exec-s vis-mod) 'BUSY) (not (tracked-obj-last-feat vis-mod)))
         ;; just complete it now
         (complete-request (last-visual-request vis-mod))
         
         (model-warning "Attention shift requested at ~S while one was already in progress." (mp-time)))
        (t
         (when (tracked-obj-last-feat vis-mod) (remove-tracking vis-mod))
      
         (setf (moving-attention vis-mod) t)
         (clear-attended vis-mod)
         (setf (last-scale vis-mod) scale)
         
         (setf (attend-failure vis-mod) nil)
         
         (schedule-event-relative (randomize-time-ms (move-attn-latency vis-mod)) 'encoding-complete
                                  :time-in-ms t :destination :vision :module :vision :params (list location scale) :output 'medium
                                  :details (concatenate 'string "Encoding-complete " (symbol-name location) " " (symbol-name scale)))
         
         (setf (current-marker vis-mod) location)
         (change-state vis-mod :exec 'BUSY :proc 'BUSY))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-vision-module (model-name)
  (declare (ignore model-name))
  (make-instance 'vision-module))

(defun reset-vision-module (vis-mod)
  
  (reset-pm-module vis-mod)
  
  (clrhash (visicon vis-mod))
  (clrhash (found-locs vis-mod))
  
  (setf (current-cursor vis-mod) nil)
  
  (remove-tracking vis-mod)
  (setf (last-scale vis-mod) nil)
  (set-cfs-mth vis-mod :RM-ORIG)
  (setf (synthd-objs vis-mod) (clrhash (synthd-objs vis-mod)))
  (setf (finst-lst vis-mod) nil)
  
  (setf (scene-change vis-mod) nil)
  
  (setf (last-obj vis-mod) nil)
  
  (setf (loc-failure vis-mod) nil)
  (setf (attend-failure vis-mod) nil)
  
  (setf (other-word-chars vis-mod) nil)
  (setf (center-point vis-mod) (vector 0 0))
  
  (setf (process-display-called vis-mod) nil)
  
  (setf (last-visual-request vis-mod) nil)
  
  (chunk-type visual-object screen-pos value status color height width)
  
  (chunk-type (text (:include visual-object)) (text t))
  (chunk-type (empty-space (:include visual-object)) (empty-space t))
  (chunk-type (oval (:include visual-object)) (oval t))
  (chunk-type (cursor (:include visual-object)) (cursor t))
  (chunk-type (line (:include visual-object)) (line t) end1-x end1-y end2-x end2-y)
  (chunk-type (phrase! (:include visual-object)) (phrase! t) objects words colors)
  
  
  (chunk-type visual-location screen-x screen-y distance kind color value height width size)
  (chunk-type (set-visloc-default (:include visual-location)) (set-visloc-default t) type)
  (chunk-type (char-primitive (:include visual-location)) (kind char-primitive) left right)
    
  (chunk-type color color)
  
  (chunk-type vision-command cmd)
  (chunk-type (move-attention (:include vision-command)) (cmd move-attention) screen-pos scale)
  (chunk-type (start-tracking (:include vision-command)) (cmd start-tracking))
  (chunk-type (assign-finst (:include vision-command)) (cmd assign-finst) object location)
  (chunk-type (clear-scene-change (:include vision-command)) (cmd clear-scene-change))
  (chunk-type (clear-all-finsts (:include vision-command)) (cmd clear-all-finsts))
  
  (dolist (chunk '(lowest highest current current-x current-distance
                   current-y clockwise counterclockwise external
                   internal text box line oval char-primitive 
                   new find-location))
    (unless (chunk-p-fct chunk)
      (define-chunks-fct `((,chunk name ,chunk)))
      (make-chunk-immutable chunk)))

   (dolist (chunk '(black red blue green white magenta yellow
                   cyan dark-green dark-red dark-cyan dark-blue
                   dark-magenta dark-yellow light-gray dark-gray gray
                   pink light-blue purple brown))
    (unless (chunk-p-fct chunk)
      (define-chunks-fct `((,chunk color ,chunk)))
      (make-chunk-immutable chunk)))  
  
  (dolist (chunk '(move-attention assign-finst start-tracking clear-scene-change set-visloc-default clear-all-finsts))
    (unless (chunk-p-fct chunk)
      (define-chunks-fct `((,chunk isa ,chunk)))
      (make-chunk-immutable chunk)))
         
  (setf (default-spec vis-mod) (define-chunk-spec screen-x lowest :attended new)))

(defun clear-scene-change (vis-mod)
  (setf (scene-change vis-mod) nil))

(defun clear-all-finsts (vis-mod)
  (setf (finst-lst vis-mod) nil))

(defun query-vision-module (vis-mod buffer slot value)
  (case buffer
    (visual
     (cond ((and (eq slot 'state) (eq value 'error))
            (attend-failure vis-mod))
           ((eq slot 'scene-change)
            (let ((change-detect (and (numberp (car (scene-change vis-mod)))
                                      (>= (car (scene-change vis-mod)) (change-threshold vis-mod))
                                      (<= (mp-time-ms) (+ (cdr (scene-change vis-mod)) (new-span vis-mod))))))
              (or (and change-detect value)
                  (null (or change-detect value)))))
           ((eq slot 'scene-change-value)
            (and (numberp value)
                 (numberp (car (scene-change vis-mod)))
                 (>= (car (scene-change vis-mod)) value)))
           (t (generic-state-query vis-mod buffer slot value))))
    (visual-location
     (case slot
       (state
        (case value
          (busy nil) ;; visual-location requests are always free
          (free t)
          (error (loc-failure vis-mod))
          (t (print-warning "Invalid query made of the visual-location buffer with slot ~S and value ~S" slot value))))
       (attended
        (let ((vis-loc-chunk (buffer-read 'visual-location)))
          (when vis-loc-chunk
            (let* ((old-loc  (chunk-visicon-entry vis-loc-chunk))
                   (loc (if (chunk-p-fct old-loc) old-loc vis-loc-chunk)))
              (update-new vis-mod)
              (check-finsts vis-mod)
              
              (test-attended vis-mod (list '= :attended value) loc)))))))))


(defmethod warn-vision ((vis-mod vision-module) buffer-name chunk-spec)
  (declare (ignore chunk-spec))
  (when (and (eq buffer-name 'visual)
             (null (visual-lock vis-mod)))
    (lock-device (current-device-interface))
    (setf (visual-lock vis-mod) t)))


(defmethod pm-module-last-cmd-name ((vis-mod vision-module) buffer-name chunk-spec)
  (declare (ignorable vis-mod))
  (case buffer-name
    (visual-location (if (chunk-spec-slot-spec chunk-spec 'set-visloc-default)
                         'set-visloc-default
                       'find-location))
    ;; shouldn't happen since they've all got a cmd slot...
    (visual :bad-command)))

  



(defmethod pm-module-request ((vis-mod vision-module) buffer-name chunk-spec)
  (case buffer-name
    (visual
     
     (when (visual-lock vis-mod)
       (setf (visual-lock vis-mod) nil)
       (schedule-event-now 'unlock-device :module :vision :destination :device 
                                :priority :min :output nil :maintenance t))
     
     (cond ((test-for-clear-request chunk-spec)
            (schedule-event-now 'clear :module :vision :destination :vision :output 'low))
           
           ((= (length (chunk-spec-slot-spec chunk-spec 'cmd)) 1)
            
            ;; store it now since it's probably valid
            (setf (last-visual-request vis-mod) chunk-spec)
            
            (let ((cmd (spec-slot-value (first (chunk-spec-slot-spec chunk-spec 'cmd)))))
              (case cmd
                (clear-all-finsts
                 ;; just complete it now since there's no cost
                 (complete-request chunk-spec)
                 
                 (schedule-event-now 'clear-all-finsts :module :vision :destination :vision :output 'low))
                (clear-scene-change
                 ;; just complete it now since there's no cost
                 (complete-request chunk-spec)
                 
                 (schedule-event-now 'clear-scene-change :module :vision :destination :vision :output 'low))
                (start-tracking
                 ;; just complete it now since there's no cost
                 (complete-request chunk-spec)
                 
                 (schedule-event-now 'start-tracking :destination :vision :module :vision :output 'medium))
                (assign-finst
                 ;; just complete it now whether valid or not since there's no cost
                 (complete-request chunk-spec)

                 (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                                   (verify-single-explicit-value chunk-spec 'object :vision 'assign-finst)
                                 nil))
                       (location (if (slot-in-chunk-spec-p chunk-spec 'location)
                                     (verify-single-explicit-value chunk-spec 'location :vision 'assign-finst)
                                   nil)))
                   
                   (if (or object location)
                       (schedule-event-now 'assign-finst :params (list vis-mod :object object :location location)
                                                :module :vision :output 'medium)
                     (print-warning "An object or location is required for an assign-finst request"))))
                (move-attention
                 (let ((sp (verify-single-explicit-value chunk-spec 'screen-pos :vision 'move-attention))
                       (scale (if (slot-in-chunk-spec-p chunk-spec 'scale)
                                  (verify-single-explicit-value chunk-spec 'scale :vision 'move-attention)
                                nil)))
                   (if (valid-vis-loc-chunk sp vis-mod)
                       (schedule-event-now 'move-attention 
                                                :params (list vis-mod :scale scale :location sp)
                                                :details (concatenate 'string "Move-attention " (symbol-name sp)" " (symbol-name scale))
                                                :module :vision)
                     (progn
                       ;; just complete it now
                       (complete-request chunk-spec)
                       (print-warning "screen-pos value ~s in a move-attention request was not a valid chunk" sp)))))
                (t
                 ;; just complete it now
                 (complete-request chunk-spec)
                 (print-warning "Invalid command ~a sent to the visual buffer" cmd)))))
           (t
            ;; just complete it now
            (complete-request chunk-spec)
            
            (print-warning "Invalid request sent to the visual buffer~%~a" (capture-model-output (pprint-chunk-spec chunk-spec))))))
   
    (visual-location
     
     (cond ((> (length (chunk-spec-slot-spec chunk-spec :attended)) 1)
            (print-warning ":attended specification only allowed once in a visual-location request."))
           ((> (length (chunk-spec-slot-spec chunk-spec :nearest)) 1)
            (print-warning ":nearest specification only allowed once in a visual-location request."))
           ((> (length (chunk-spec-slot-spec chunk-spec :center)) 1)
            (print-warning ":center specification only allowed once in a visual-location request."))
           ((chunk-spec-slot-spec chunk-spec 'set-visloc-default)
            (let* ((specs (chunk-spec-slot-spec chunk-spec 'set-visloc-default))
                   (spec (first specs))
                   (type-specs (chunk-spec-slot-spec chunk-spec 'type))
                   (type-spec (first type-specs)))
              (cond ((> (length specs) 1)
                     (print-warning "set-visloc-default slot can only be specified once in a visual-location request."))
                    ((not (eq '= (spec-slot-op spec)))
                     (print-warning "set-visloc-default slot must use the = test in a visual-location request."))
                    ((> (length type-specs) 1)
                     (print-warning "type slot can only be specified once in a set-visloc-default request."))
                    ((and type-spec (not (eq '= (spec-slot-op type-spec))))
                     (print-warning "type slot must use the = test in a set-visloc-default request."))
                    ((and type-spec (not (chunk-type-p-fct (spec-slot-value type-spec))))
                     (print-warning "type slot must specify a valid chunk-type in a set-visloc-default request."))
                    (t
                     (schedule-event-now 'set-visloc-default-request :module :vision 
                                           :destination :vision 
                                           :details "Set-visloc-default" 
                                           :output 'medium
                                           :priority 9 ; just below the buffer clearing by the production
                                           :params (list chunk-spec))))))
           (t
            (schedule-event-now 'find-location :module :vision 
                                         :destination :vision 
                                         :details "Find-location" 
                                         :output 'medium
                                         :params (list chunk-spec)))))))



(defun params-vision-module (vis-mod param)
  (if (consp param)
    (case (car param)
      
      (:optimize-visual
       (setf (optimize-p vis-mod) (cdr param)))
      (:viewing-distance (setf (viewing-distance vis-mod) (cdr param))
                         (when (and (numberp (viewing-distance vis-mod)) (numberp (ppi vis-mod)))
                           (setf (view-dist vis-mod) (round (* (viewing-distance vis-mod) (ppi vis-mod))))))
      (:pixels-per-inch (setf (ppi vis-mod) (cdr param))
                         (when (and (numberp (viewing-distance vis-mod)) (numberp (ppi vis-mod)))
                           (setf (view-dist vis-mod) (round (* (viewing-distance vis-mod) (ppi vis-mod))))))
      (:visual-attention-latency
       (setf (move-attn-latency vis-mod) (safe-seconds->ms (cdr param) 'sgp))
       (cdr param))
      (:scene-change-threshold
       (setf (change-threshold vis-mod) (cdr param)))
      (:visual-finst-span
       (setf (finst-span vis-mod) (safe-seconds->ms (cdr param) 'sgp))
       (check-finsts vis-mod)
       (cdr param))   
      (:visual-movement-tolerance
       (setf (move-allowance vis-mod) (cdr param)))
      (:visual-num-finsts
       (setf (num-finst vis-mod) (cdr param))
       (check-finsts vis-mod)
       (cdr param))
      (:visual-onset-span
       (setf (new-span vis-mod) (safe-seconds->ms (cdr param) 'sgp))
       (cdr param))
      (:auto-attend
       (setf (auto-attend vis-mod) (cdr param)))
      (:test-feats
       (setf (test-feats vis-mod) (cdr param)))
      (:delete-visicon-chunks
       (setf (purge-visicon vis-mod) (cdr param)))
      (:unstuff-visual-location
       (setf (unstuff-loc vis-mod)
         (if (numberp (cdr param))
             (safe-seconds->ms (cdr param) 'sgp)
           (cdr param)))
       (cdr param))
      (:overstuff-visual-location
       (setf (overstuff-loc vis-mod) (cdr param))))
    
    (case param
      
      (:optimize-visual
       (optimize-p vis-mod))  
      (:scene-change-threshold
       (change-threshold vis-mod))
      (:visual-attention-latency
       (ms->seconds (move-attn-latency vis-mod)))
      (:visual-finst-span
       (ms->seconds (finst-span vis-mod)))   
      (:visual-movement-tolerance
       (move-allowance vis-mod))
      (:visual-num-finsts
       (num-finst vis-mod))
      (:visual-onset-span
       (ms->seconds (new-span vis-mod)))
      (:auto-attend
       (auto-attend vis-mod))
      (:test-feats
       (test-feats vis-mod))
      (:delete-visicon-chunks
       (purge-visicon vis-mod))
      (:unstuff-visual-location
       (if (numberp (unstuff-loc vis-mod))
           (ms->seconds (unstuff-loc vis-mod))
         (unstuff-loc vis-mod)))
      (:overstuff-visual-location
       (overstuff-loc vis-mod)))))


(define-module-fct :vision 
    (list (define-buffer-fct 'visual-location 
              :request-params (list :attended :nearest :center) 
              :queries '(attended)
              :status-fn (lambda ()
                           (command-output "  attended new          : ~S" (query-buffer 'visual-location '(attended  new)))
                           (command-output "  attended nil          : ~S" (query-buffer 'visual-location '(attended  nil)))
                           (command-output "  attended t            : ~S" (query-buffer 'visual-location '(attended  t))))) 
          (define-buffer-fct 'visual 
              :queries '(scene-change-value scene-change modality preparation execution processor last-command)
            :status-fn (lambda () 
                         (let ((v (get-module :vision)))
                           (print-module-status v)
                           (command-output "  scene-change          : ~S" (query-buffer 'visual '(scene-change t)))
                           (command-output "  scene-change-value    : ~S" (car (scene-change v)))))
            :trackable t))
  (list 
   (define-parameter :scene-change-threshold
     :valid-test (lambda (x) (and (numberp x) (<= 0.0 x 1.0))) 
     :default-value 0.25
     :warning "a number in the range [0.0,1.0]"
     :documentation "Proportion of visicon which must change to signal a scene change")
   (define-parameter :optimize-visual
     :valid-test 'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "If set to nil the default devices process text into sub-letter features")
    (define-parameter :visual-attention-latency
     :valid-test 'nonneg 
     :default-value 0.085
     :warning "a non-negative number"
     :documentation "Time for a shift of visual attention")
   (define-parameter :visual-finst-span
     :valid-test 'nonneg 
     :default-value 3.0
     :warning "a non-negative number"
     :documentation "Lifespan of a visual finst")
   (define-parameter :visual-movement-tolerance
     :valid-test 'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation 
     "How far something can move while still being seen as the same object.")
   (define-parameter :visual-num-finsts
     :valid-test 'posnum 
     :default-value 4
     :warning "a positive number"
     :documentation "Number of visual finsts.")
   (define-parameter :visual-onset-span
     :valid-test 'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation "Lifespan of new visual objects being marked as NEW")
   (define-parameter :auto-attend
     :valid-test 'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Whether visual-location requests automatically generate an attention shift")
   
   (define-parameter :test-feats
     :valid-test 'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "Whether proc-display should use the features to compare items instead of just the chunk names")
   (define-parameter :delete-visicon-chunks
     :valid-test 'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "Whether proc-display should delete and unintern the name of old chunks that were in the visicon")
   (define-parameter :unstuff-visual-location
     :valid-test (lambda (x) (or (tornil x) (numberp x)))
     :default-value t
     :warning "T, NIL, or a number"
     :documentation "Whether chunks stuffed into the visual-location buffer should be automatically cleared by the module if unused")
   (define-parameter :overstuff-visual-location
     :valid-test 'tornil
     :default-value nil
     :warning "T or NIL"
     :documentation "Whether a chunk previously stuffed into the visual-location buffer can be overwritten by a new chunk to be stuffed")
   (define-parameter :viewing-distance :owner nil)
   (define-parameter :pixels-per-inch :owner nil))
  
  :version "5.0"
  :documentation "A module to provide a model with a visual attention system"
  :creation 'create-vision-module
  :reset 'reset-vision-module
  :query 'query-vision-module
  :request 'pm-module-request
  :params 'params-vision-module
  :warning 'warn-vision)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BUILD-STRING-FEATS      [Method]
;;; Date        : 2014.01.24
;;; Description : In order to build all the features for a string, there is a
;;;             : great deal of stuff that is needed from the interface beyond
;;;             : just the string itself.  Need to know the y coordinate, the
;;;             : starting x coordinate, the text height, the line height, the 
;;;             : associated screen object, and some way of determining the width 
;;;             : (in pixels) of a string.  May also provide a function for 
;;;             : computing a different starting x coordinate for each line.
;;;             : 
;;;             : There are two ways to do it, with and without optimizing. 
;;;             : With optimizing it's easy, just accumulate words.  When
;;;             : optimzing is off, though, have to walk the string
;;;             : character-by-character and build features for each one.

(defgeneric build-string-feats (vis-mod &key text start-x y-pos width-fct height obj line-height x-fct)
  (:documentation  "Build a list of visual-locations representing a string with the given geometry."))

(defmethod build-string-feats ((vis-mod vision-module) &key text start-x y-pos width-fct height obj (line-height 0) x-fct)
  (declare (fixnum start-x y-pos)  (function width-fct) (string text) (number height) (number line-height))
  (when (not (and text start-x y-pos width-fct height line-height))
    (error "NIL passed to BUILD-STRING-FEATS."))
  (unless (equal text "")
    (let* ((f-accum nil)
           (spc-wdth (funcall width-fct " "))
           (coord-slots (vis-loc-slots vis-mod))
           (x-slot (first coord-slots))
           (y-slot (second coord-slots)))
      (dolist (line (string-to-lines text) (nreverse (flatten f-accum)))
        (let ((curr-x (if (or (functionp x-fct) (and (symbolp x-fct) (fboundp x-fct)))
                          (let ((x (funcall x-fct line start-x obj)))
                            (if (numberp x) x start-x))
                            start-x))
              (curr-width nil))
          (if (optimize-p vis-mod)
              
              ;; if optimizing is on, then carve the string into words (strings)
              ;; and space runs (numbers)
              (dolist (word (chop-string vis-mod line))
                (when (stringp word)
                  (setf curr-width (funcall width-fct word))
                  (let ((vl (car (define-chunks-fct `((isa visual-location
                                                           ,y-slot ,y-pos height ,height value text 
                                                           kind text width ,curr-width ,x-slot ,(+ curr-x (round curr-width 2))))))))
                    (push vl f-accum)
                    (setf (chunk-real-visual-value vl) (string-downcase word))))
                (incf curr-x (if (stringp word) curr-width (* word spc-wdth))))
            
            ;; if not optimizing, then blast it character-by-character
            (let ((char nil))
              (dotimes (idx (length line))
                (setf char (char line idx))
                (setf curr-width (funcall width-fct (subseq line idx (1+ idx))))
                (cond ((alphanumericp char)
                       (push (char-to-features vis-mod (char-upcase char) curr-x
                                               (- (+ curr-x curr-width) 1) y-pos
                                               height obj) 
                             f-accum))
                      ((and (graphic-char-p char)
                            (not (char-equal char #\space)))
                       
                       (let ((vl (car (define-chunks-fct `((isa visual-location
                                                                ,y-slot ,y-pos height ,height value text
                                                                kind text width ,curr-width ,x-slot ,(+ curr-x (round curr-width 2))))))))
                         (push vl f-accum)
                         (setf (chunk-real-visual-value vl) (mkstr char))))
                      (t nil))
                (incf curr-x curr-width))))
          (incf y-pos line-height))))))

;;; CHAR-TO-FEATURES      [Method]
;;; Date        : 99.03.30
;;; Description : For each character, there will usually be many of those
;;;             : CHAR-PRIMITIVE features.  Grab the list of features 
;;;             : associated with a character, and build 'em.

(defgeneric char-to-features (vis-mod char left right y height obj)
  (:documentation  "Returns a list of basic visual-location chunks for a characer"))

(defmethod char-to-features ((vis-mod vision-module) (char character) 
                             (left number) (right number) (y number) 
                             (height number) obj)
  (declare (ignore obj))
  (let* ((xpos (+ left (round (- right left) 2)))
         (width (1+ (- right left)))
         (features (pairlis
                    (getfeats (active-cfs vis-mod) char)
                    (get-icon-feats (active-cfs vis-mod) char)))
         (accum nil)
         (coord-slots (vis-loc-slots vis-mod))
         (x-slot (first coord-slots))
         (y-slot (second coord-slots)))
    (dolist (feats features accum)
      (when (and (symbolp (car feats)) (not (chunk-p-fct (car feats))))
        (define-chunks-fct `((,(car feats)))))
      (when (and (symbolp (cdr feats)) (not (chunk-p-fct (cdr feats))))
        (define-chunks-fct `((,(cdr feats)))))
      (let ((vl (car (define-chunks-fct `((isa char-primitive
                                               ,y-slot ,y height ,height value ,(cdr feats)
                                               width ,width ,x-slot ,xpos left ,left right ,right))))))
        (push vl accum)
        (setf (chunk-real-visual-value vl) (car feats))))))

(defun chop-string (vis-mod str)
  (declare (string str))
  (unless (string= str "")
    (let* ((oldstate (char->state vis-mod (char str 0)))
           (state nil)
           (chr nil)
           (wrd "")
           (cnt 0)
           (accum nil))
      (dotimes (i (length str))
        (setf chr (char str i))
        (setf state (char->state vis-mod chr))
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
      accum)))

(defun add-word-characters (&rest chars)
  (let ((vis-mod (get-module :vision)))
    (if vis-mod
        (dolist (x chars (other-word-chars vis-mod))
          (when (characterp x)
            (pushnew x (other-word-chars vis-mod))))
      (print-warning "No vision module available could not add new word characters."))))

(defun char->state (vis-mod char)
  "Given a character, return :WORD, :SPACE, or :MISC"
  (declare (character char))
  (cond ((or (alphanumericp char) (find char (other-word-chars vis-mod))) :WORD)
        ((whitespace-p char) :SPACE)
        (t :MISC)))

(defun whitespace-p (char)
  "Returns T if <char> is a whitespace character (non-printing or space)"
  (declare (character char))
  (or (not (graphic-char-p char))
      (eq char #\Space)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fill-default-vis-obj-slots (vis-obj vis-loc)
  (if (and (chunk-p-fct vis-obj) (chunk-p-fct vis-loc))
      (mod-chunk-fct vis-obj `(height ,(chunk-slot-value-fct vis-loc 'height)
                               width ,(chunk-slot-value-fct vis-loc 'width)
                               color ,(chunk-slot-value-fct vis-loc 'color)
                               value ,(aif (chunk-real-visual-value vis-loc)
                                         it
                                        (chunk-slot-value-fct vis-loc 'value))))
    (print-warning "Invalid chunk passed to fill-default-vis-obj-slots ~S not updated using ~s." vis-obj vis-loc)))

(defun compute-vis-loc-size (vis-loc vis-m)
  (set-chunk-slot-value-fct vis-loc 'size
                            (aif (simple-size vis-loc vis-m)
                                 it
                                 1.0)))
(defun simple-size (vis-loc vis-m)
  (let ((w (chunk-slot-value-fct vis-loc 'width))
        (h (chunk-slot-value-fct vis-loc 'height))
        (d (chunk-slot-value-fct vis-loc (third (vis-loc-slots vis-m)))))
    (when (and (numberp w) (numberp h) (numberp d))
      (* 0.01 (round
               (* (pm-pixels-to-angle w d)
                  (pm-pixels-to-angle h d))
               0.01)))))
  


;;; APPROACH-WIDTH      [Method]
;;; Date        : 99.03.30
;;; Description : Remember in high school when someone asked in trig why we
;;;             : needed to know this crap?  Well, this is why.  I'm pretty
;;;             : sure this is all the right trig, but there could be some
;;;             : missed math in here.

(defun approach-width (vis-loc theta vis-m)
  (aif (chunk-visual-approach-width-fn vis-loc)
       (funcall it vis-loc theta)
       (let* ((x (chunk-slot-value-fct vis-loc 'width))
              (y (chunk-slot-value-fct vis-loc 'height))
              (z (chunk-slot-value-fct vis-loc (third (vis-loc-slots vis-m)))))
         (if (and (numberp x) (numberp y))
             (let ((critical-theta (atan y x))
                   (theta (abs theta))
                   (ret-width nil))
               (when (> theta (/ pi 2))
                 (setf theta (- pi theta)))
               (setf ret-width
                 (cond ((= theta 0) x)
                       ((= theta (/ pi 2)) y)
                       ;((= theta critical-theta) (sqrt (+ (* y y) (* x x))))
                       ((<= theta critical-theta) (/ x (cos theta)))
                       (t (/ y (cos (- (/ pi 2) theta))))))
               (pm-pixels-to-angle ret-width (when (numberp z) z)))
           (aif (get-module :motor)
                (default-target-width it)
                1.0)))))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-visicon-item (obj &optional (update t))
  (add-screen-object obj (get-module :vision) update))

(defun delete-visicon-item (obj &optional (update t))
  (delete-screen-object obj (get-module :vision) update))

(defun update-visicon-item (obj &optional (update t) &key same-chunks chunks)
  (let ((module (get-module :vision)))
    (cond ((not (or same-chunks chunks)) 
           ;; simple case just delete and add the item
           (delete-screen-object obj module nil)
           (add-screen-object obj module update))
          
          (t ;; just update the chunks for the item
           
           (cond ((null chunks) ;; if none given get them from build-vis-locs-for
                  (setf chunks (flatten (build-vis-locs-for obj module))))
                 ((atom chunks) ;; if it's an atom assume that's a chunk name
                  (setf chunks (list chunks))))
                 
           
           (dolist (x chunks)
             (if (eq (chunk-special-visual-object x) obj) ;; make sure it matches the object
                 
                 (let ((old (gethash (chunk-visicon-entry x) (visicon module))))
                   (if old
                       (progn
                         
                         ;; set the visicon entry with the new features
                         
                         (setf (chunk-visicon-entry x) 
                           (if (test-feats module)
                               (hash-visual-chunk-contents x)
                             x))

                         (let ((entry (copy-chunk-fct x)))
                         
                           ;; put the new one in place and set the relevant info for it.
                           
                           (setf (gethash (chunk-visicon-entry x) (visicon module)) entry)
                           
                           (setf (chunk-visual-tstamp entry) (chunk-visual-tstamp old))
                           (setf (chunk-visual-new-p entry) (chunk-visual-new-p old))
    
                         
                         #| assuming that nobody is going to use "synthed" features
                            in their own add/update/delete code, but if needed something like 
                            this old code from enter-into-visicon will need to be made to work
                            right for the update.

                         (dolist (x (finst-lst module))
                           (when (and (synthed-from x) (find (chunk-visicon-entry existing) (synthed-from x)))
                             (setf (synthed-from x) (substitute vis-loc (chunk-visicon-entry existing) (synthed-from x)))))
                         |#
                         
                           ;; get rid of the old chunk if allowed 
                           
                           (when (purge-visicon module)
                             ;; might need to test this to make sure it's not
                             ;; currently attened and stuff...
                             
                             (purge-chunk-fct old))))
                     (print-warning "Chunk ~s is not currently in the visicon.  No update made." x)))
               (print-warning "Chunk ~s is not a feature of the object ~s.  No update made." x obj)))
           
           (when update
             (visicon-update module nil))))))
             

(defmethod add-screen-object (obj (vm vision-module) &optional (update t))
  (let ((vfeats (flatten (build-vis-locs-for obj vm))))
    (dolist (x vfeats)
      (if (valid-vis-loc-chunk x vm) 
          (progn
            (unless (numberp (chunk-slot-value-fct x (third (vis-loc-slots vm))))
              (set-chunk-slot-value-fct x (third (vis-loc-slots vm)) (no-output (car (sgp :VIEWING-DISTANCE)))))
            
            (unless (numberp (chunk-slot-value-fct x 'size))
              (compute-vis-loc-size x vm)))
        
        (progn 
          (print-warning "Invalid visicon item ~s found when processing the display.  Must be a chunk with ~s and ~s slots." 
                         x (first (vis-loc-slots vm)) (second (vis-loc-slots vm)))
          (setf vfeats (remove x vfeats)))))
    
    (mapcar (lambda (vl)
                (setf (chunk-visicon-entry vl)
                    (if (test-feats vm)
                        (hash-visual-chunk-contents vl)
                      vl))
                (setf (chunk-visual-feature-name vl) vl))
      vfeats)
    
    (mapcar (lambda (vl) 
                (setf (chunk-special-visual-object vl) obj)
                (enter-into-visicon vl vm)) 
      vfeats)
    
    (when update
      (visicon-update vm nil))))

(defmethod delete-screen-object (obj (vm vision-module) &optional (update t))
  (maphash (lambda (key val)
             (when (eq obj (chunk-special-visual-object val))
               (remhash key (visicon vm))
               (when (and (purge-visicon vm) (chunk-p-fct val))
                 (purge-chunk-fct val))))
           (visicon vm))
  (when update
    (visicon-update vm nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-char-feature-set (setname)
  "Sets the feature set used to represent characters when optimizing is off. <setname> should be a keyword."
  (verify-current-mp
   "No current meta-process.  Cannot set a char feature set."
   (verify-current-model 
    "No current model.  Cannot set a char feature set."
    (aif (get-module :vision)
         (if (set-cfs-mth it setname) t nil)
         (print-warning "No vision module found.  Cannot set a char feature set.")))))

(defmethod set-visloc-default-request ((vm vision-module) spec)
  "Assumes all the checking has been done already for validity"
  (let ((slot-specs (remove 'set-visloc-default (remove 'type (chunk-spec-slot-spec spec) :key 'spec-slot-name) :key 'spec-slot-name))
        (type (spec-slot-value (first (chunk-spec-slot-spec spec 'type)))))
    (setf (default-spec vm) 
      (define-chunk-spec-fct (if type 
                                 (append (list 'isa type) (flatten slot-specs))
                               (flatten slot-specs)))))
  (update-new vm)
  (check-finsts vm)
  (stuff-visloc-buffer vm))

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
        (let ((chunk-spec (funcall 'define-chunk-spec-fct params)))
          (if chunk-spec
              (cond ((> (length (chunk-spec-slot-spec chunk-spec :attended)) 1)
                     (print-warning ":attended specification only allowed once in set-visloc-default.")
                     (print-warning "Visloc defaults not changed."))
                    ((> (length (chunk-spec-slot-spec chunk-spec :nearest)) 1)
                     (print-warning ":nearest specification only allowed once in set-visloc-default.")
                     (print-warning "Visloc defaults not changed."))
                    (t
                     (progn (setf (default-spec (get-module :vision)) chunk-spec) t)))
            (print-warning "Invalid chunk specification.  Default not changed.")))
      (print-warning "No vision module found.  Cannot set visloc defaults.")))))

(defun print-visicon ()
  "Print the Vision Module's visicon. For debugging."
  (awhen (get-module :vision)  ;; Test that there is a vision module
         (update-new it)
         (check-finsts it) 
         (command-output "Loc        Att   Kind           Value             Color           ID")
         (command-output "---------  ---   -------------  ----------------  --------------  -------------")
         
         (mapcar (lambda (x) (print-icon-feature x it)) (visicon-chunks it t))
         nil))


(defun print-icon-feature (chunk vis-mod)
  (let* ((*print-pretty* nil)
         (coord-slots (vis-loc-slots vis-mod))
         (x-slot (first coord-slots))
         (y-slot (second coord-slots)))
    (command-output "(~3D ~3D)~11T~A~17T~A~32T~S~50T~A~66T~A"
                    (chunk-slot-value-fct chunk x-slot) 
                    (chunk-slot-value-fct chunk y-slot) 
                    (feat-attended chunk (get-module :vision))
                    (chunk-slot-value-fct chunk 'kind) 
                    (if (null (chunk-real-visual-value chunk))
                        (chunk-slot-value-fct chunk 'value) 
                      (chunk-real-visual-value chunk))
                    (chunk-slot-value-fct chunk 'color) 
                    (chunk-visual-feature-name chunk))))


(defun attend-visual-coordinates (x y &optional distance)
  "Tells the Vision Module to start with attention at a certain location."
  (aif (get-module :vision)
       (setf (current-marker it) 
         (car (define-chunks-fct `((isa visual-location
                                        ,(first (vis-loc-slots it)) ,x
                                        ,(second (vis-loc-slots it)) ,y
                                        ,(third (vis-loc-slots it)) ,(if (numberp distance) distance (view-dist it)))))))
       (print-warning "No vision module found.  Cannot set visual coordinates.")))

(defun remove-visual-finsts (&key (set-new nil) (restuff nil))
  (let ((vis-m (get-module :vision)))
    (if vis-m
        (progn
          (setf (finst-lst vis-m) nil)
          (if set-new
              (maphash (lambda (key value)
                         (declare (ignore key))
                         (setf (chunk-visual-tstamp value) (mp-time-ms))
                         (setf (chunk-visual-new-p value) 'new))
                       (visicon vis-m))
            (maphash (lambda (key value)
                       (declare (ignore key))
                       (setf (chunk-visual-new-p value) 
                         (if (<= (- (mp-time-ms) (chunk-visual-tstamp value) (new-span vis-m)))
                             'new 
                           nil)))
                     (visicon vis-m)))
          (when restuff           
            (stuff-visloc-buffer vis-m))
          nil)            
      (print-warning "No vision module found.  Cannot remove the visual finsts."))))

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
