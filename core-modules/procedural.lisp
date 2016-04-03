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
;;; Filename    : procedural.lisp
;;; Version     : 4.0
;;; 
;;; Description : Implements the procedural module (productions).
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Investigate possible problem with !eval! and variables.
;;;             : [ ] Double check that conflict set ordering is right with
;;;             :     respect to :er and when the learning is turned on.  It
;;;             :     seems right with the basic testing, but I want to really
;;;             :     investigate that to be sure when I have a chance.
;;;             : [ ] Fix the production structure accessors so that they're
;;;             :     different from the extended parameter accessors...
;;;             : [ ] Why is print-production-text not using the pre-parsed
;;;             :     production components and why isn't it in with pm-commands?
;;;             : [ ] Revisit the fix for search buffer bindings for efficiency
;;;             :     when I get some more "real world" test cases.
;;; ----- History -----
;;;
;;; 2004.11.24 : Dan
;;;              Creation.
;;; 2005.01.05 : Dan
;;;            : Fixed PRINT-PRODUCTION-OUTPUT so that it works for all cases
;;;            : correctly.
;;; 2005.01.07 : Dan
;;;            : Fixed a problem that LispWorks has with my use of
;;;            : set-parameter in parameters-fct.
;;; 2005.01.09 : Dan
;;;            : Adding two new tracing parameters :lhst and :rhst which
;;;            : control whether all the buffer actions show in the trace or
;;;            : not.
;;;            : Changed production-selection to production-selected and
;;;            : production-action to production-fired
;;; 2005.01.13 Dan
;;;            : * Put the device-lock in place when a production is going
;;;            :   to issue a +visual.  Took a little bit of trickery since
;;;            :   the production needs to actually lock things down after
;;;            :   conflict-resolution and before anything else is allowed
;;;            :   to occur.  It can't schedule that to occur because it
;;;            :   must be atomic and there wasn't a mechanism for that nor
;;;            :   anything in the production to indicate such.
;;;            : * Fixed an error in how it handles the return value from the
;;;            :   conflict-set-hook because production-selected wanted the
;;;            :   actual production and not just the name.
;;; 2005.01.15 Dan
;;;            : * Moved the user functions to a file in the tools directory.
;;; 2005.01.16 Dan
;;;            : * Added the delayed-resolution slot so that non-scheduled 
;;;            :   changes to procedural (sgp, spp, or new/changed production) 
;;;            :   can force conflict-resolution back into the event queue.
;;; 2005.01.17 Dan
;;;            : * Removed calls to format in the scheduling.
;;; 2005.01.18 Dan
;;;            : * Removed calls to get-parameters.
;;; 2005.01.19 Dan
;;;            : * Updates to conflict resolution and production struct to
;;;            :   enable production breaks and disabling.
;;;            : * Added support for the user functions.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium"  or ":output 'low" to some of the 
;;;             :   events scheduled to play friendly with the new detail level.
;;; 2005.02.09 Dan
;;;             : * Added the *-parse-table slots to make production parsing
;;;             :   faster on a reset (if it's not a reload).
;;; 2005.02.10 Dan
;;;             : * Swtched to using expt-coerced.
;;; 2005.02.13 Dan
;;;             : * Some modifications to conflict-resolution trying to speed
;;;             :   things up.  Needs to be tested and cleaned up...
;;; 2005.03.18 Dan
;;;             : * Adding the :do-not-harvest parameter to make "strict-
;;;             :   harvesting" more of a module writer/user level control 
;;;             :   instead of an architectural mandate.
;;; 2005.03.21 Dan
;;;             : * Updating the version number with the change to the
;;;             :   strict harvesting mechanism. Now 1.0b1.
;;; 2005.04.08 Dan
;;;             : * Actually make cst do something...
;;; 2005.04.14 Dan
;;;             : * Changed :conflict-set-hook so that nil actually clears it.
;;; 2005.04.19 Dan
;;;             : * Added pprint-instantiation for users/my use.
;;; 2005.04.26 Dan
;;;             : * Added the :cycle-hook parameter that gets called when
;;;             :   a production "fires".  It gets passed the name of the
;;;             :   production.
;;; 2005.05.02 Dan
;;;             : * Changed the print-production-text function so that keywords
;;;             :   for request-parameters show up correctly.
;;;             : * Added the buffer-indices and rhs-buffers slots to the 
;;;             :   production structure for compilation's use.
;;; 2005.05.03 Dan
;;;             : * Changed the utility-?-hook parameters to not report being
;;;             :   overwritten if it's with nil.
;;;             : * Fixed print-production-text to show direct requests
;;;             :   and direct assignments correctly.
;;; 2005.05.05 Dan
;;;             : * Added the standard-rep slot to productions for use in
;;;             :   production compilation.
;;;             : * Patched up a minor error introduced into print-production-
;;;             :   text dealing with numbers in rhs modification requests.
;;; 2005.05.18 Dan
;;;             : * Fixed a bug in conflict-resolution that crashed if a 
;;;             :   production had no LHS tests.
;;; 2005.06.15 Dan
;;;             : * The conflict-set passed to the conflict-set-hook function
;;;             :   wasn't ordered such that the car was the production that
;;;             :   would be fired next.  That's a problem for the stepper in
;;;             :   the environment, so I've fixed that.
;;;             : * Updated version to 1.0b2.
;;;             : * Fixed the cst so that it doesn't print if v is nil (because
;;;             :   it was using command-output it didn't get stopped on its
;;;             :   own).
;;; 2005.06.17 Dan 
;;;             : * Adjusted :ut so that it will also take nil to turn off
;;;             :   utility threshold.
;;; 2005.08.03 Dan
;;;             : * Added specific hooks for probability and cost computiations
;;;             :   when learning is enabled:
;;;             :   utility-learned-p-hook and utility-learned-c-hook
;;;             :   These are a lower priority than the p and c hooks.  So, if 
;;;             :   those are set, then these won't even get called.
;;;             :   The important note is that right now production compilation
;;;             :   uses the major p and c hooks, so if you turn on production
;;;             :   compilation essentially all of the utility hooks are off
;;;             :   limits for user's use.
;;; 2005.09.01 Dan
;;;             : * Added a new slot to the procedural struct to let me flag
;;;             :   whether or not to check the validity of an = action when
;;;             :   parsing a p*.  The reason for that is because production
;;;             :   compilation could produce a production that modifies a
;;;             :   slot which gets added by the second production that doesn't
;;;             :   exist until after the 2nd production fires.
;;;             : * To avoid problems using the flag the default is to test
;;;             :   as always and there's a with-unchecked-p* macro one needs
;;;             :   to wrap around a call to p* to not do the check.
;;; 2005.09.14 Dan
;;;             : * Big change!
;;;             :   The priorities of the RHS actions are being reordered so 
;;;             :   that modifications (=) now have a higher priority than
;;;             :   requests (+).
;;;             :   I don't expect any problems to be generated from this for
;;;             :   the core or other existing modules.
;;;             :   However, it does now mean that it isn't (easily) possible
;;;             :   for a module to "know" the contents of the buffers at the
;;;             :   time of the production matching when a request is sent.
;;;             :   The reason for the change is driven mainly by the use of
;;;             :   direct requests within a production that also makes a 
;;;             :   modification.  The old way reesulted in outcomes different
;;;             :   from what ACT-R 5 would do and didn't seem "obvious".
;;;             :   A prototypical example would be this production:
;;;             :   (p test
;;;             :      =goal>
;;;             :        isa target
;;;             :        state =next
;;;             :     ==>
;;;             :      +retrieval> =goal
;;;             :        
;;;             :      =goal>
;;;             :        state encode
;;;             :        
;;;             :      -goal>)
;;;             :   
;;;             :   Which under the old ordering was almost guaranteed to never
;;;             :   result in the the "old" goal chunk being retrieved (unless
;;;             :   =next actually was bound to encode or partial matching let
;;;             :   a match happen).
;;;             : * Updating the module version to 1.0.
;;; 2005.12.19 Dan
;;;             : * Moved the utility computations to the utility-and-reward
;;;             :   file in the modules directory.
;;; 2005.12.21 Dan
;;;             : * Started work on making productions use a flexible parameter
;;;             :   system like chunks have.
;;;             : * Removed dependence on production-effort and production-c
;;;             :   for action timeing - now it calls productions-action-time
;;;             :   to get the value which must be defined elsewhere - now
;;;             :   its in utility-and-reward with the other parameter dependent
;;;             :   code.
;;; 2006.01.03 Dan
;;;             : * Modified extend-productions like I did to extend-chunks so
;;;             :   that the explicit compile call isn't necessary to guarantee
;;;             :   that the functions get compiled.
;;; 2006.01.25 Dan
;;;             : * Slightly wild change here - the procedural module will now
;;;             :   have a buffer so that it can be queried for its state just
;;;             :   like any of the other modules.  It won't use the buffer for
;;;             :   storing chunks, but it will use it to handle the production
;;;             :   selection event (so that it can be traced more easily).
;;;             :   The motivation behind this is purely functional at this 
;;;             :   point - it allows me to implement a "buffer based" trace
;;;             :   that tracks the state of the modules (which can only be done
;;;             :   through their buffers).  However, one could imagine that
;;;             :   it may be necessary to track the module's state for meta-
;;;             :   cognitive purposes, so it's not entirely unreasonable.
;;; 2006.02.20 Dan
;;;             : * Modified extend-productions to match the fix done to extend-
;;;             :   chunks even though productions don't suffer the same issue
;;;             :   because there's no merging.
;;; 2006.03.09 Dan
;;;             : * Removed all of the utility computation code and removed the
;;;             :   corresponding parameters from the procedural module.  They
;;;             :   now belong to the utility module.  There is an API given in
;;;             :   the utility-and-reward.lisp file that shows what's needed
;;;             :   by procedural, but otherwise one can now completely change
;;;             :   utility without having to touch the procedural module.
;;; 2006.03.10 Dan
;;;             : * Changed print-production to print when a production is 
;;;             :   disabled.
;;; 2006.03.10 Dan
;;;             : * Fixed a bug in print-production-text that broke for this
;;;             :   sequence in the actions: +goal> =val !output! ("...").
;;; 2006.03.10 Dan
;;;             : * Removed print-production-condition because it wasn't being
;;;             :   used anymore by anything.
;;; 2006.03.10 Dan
;;;             : * Modified the extend-production macro so that the accessor
;;;             :   works with the production name instead of the actual
;;;             :   structure.  This makes things a bit more confusing because
;;;             :   the "extended" accessors still look just like the actual
;;;             :   structure accessors but take a different value.  
;;;             :   The to do list has been modified to attend to this.
;;; 2006.03.10 Dan
;;;             : * Calling compile-productions explicitly now from production-
;;;             :   fired instead of implicitly throught he cycle-hook (makes
;;;             :   more sense now that things are stable).
;;; 2006.06.26 Dan
;;;             : * Fixed a minor bug in print-production-text that would 
;;;             :   misprint productions that had chunk-types that end in a > or
;;;             :   a ! in it (like phrase! from the vision module). 
;;;             : * Could still be problems if someone names a chunk isa and
;;;             :   uses it in a direct request (though you can't actually do
;;;             :   that because define-chunks won't allow it).
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in get-production which caused errors if there
;;;             :   were more than one model defined and it was called with no
;;;             :   current model (not an issue for running models).
;;; 2006.11.08 Dan
;;;             : * Added the option of having production firing times be 
;;;             :   noisy (using the randomize-time command like the PM 
;;;             :   components do).  There's a new parameter :vpft (variable
;;;             :   production firing times) which defaults to nil, but if it
;;;             :   and :randomize-time are both true, then production
;;;             :   firing times will be randomized.
;;;             : * Updated the module version to 1.3.
;;;             : * Changed how the conflict-set-hook works slightly.  Now one
;;;             :   can explicitly cancel the selection of all productions with
;;;             :   the hook and have the reason output to the trace and there 
;;;             :   will be warnings printed if invalid values are returned i.e.
;;;             :   productions which weren't on the conflict set or any other
;;;             :   non-string values (which before would implicitly cancel the
;;;             :   selection process).
;;;             : * Changed un-delay-conflict-resolution to not break when there
;;;             :   is no current model and to always return nil.
;;; 2006.11.10 Dan
;;;             : * Added the dynamic slot to the production structure so that
;;;             :   there's an easy flag for determining whether a production
;;;             :   was created with p or p* (should parse the production to check
;;;             :   because a p* may not actually be dynamic but this is the
;;;             :   easy test for now).
;;;             : * Changed print-production-text so that it properly indicates
;;;             :   if it's a p or p* (based on the dynamic slot of the production)
;;;             :   and got rid of the pre-formating before sending to command-output
;;;             :   (why was it like that?).
;;;             : * DOH! I know why it was like that...and I've undone that 
;;;             :   change now - the issue for future reference is that if no-output
;;;             :   is wrapped around a call to command-output the args to command-
;;;             :   output are not evaluated.  Thus if they have important side effects
;;;             :   you need to be careful.
;;; 2006.11.15 Dan
;;;             : * Fixed a bug with un-delay-conflict-resolution which could
;;;             :   result in the model being stuck executing conflict-resolution
;;;             :   events without advancing the clock...
;;; 2006.03.29 Dan
;;;             : * Finally got around to fixing an issue that was pointed out
;;;             :   a while ago with print-produciton-text.  If there were any
;;;             :   format directives in the production itself (for instance in
;;;             :   a !eval! or !bind! call) then it would break in the 
;;;             :   command-output macro when trying to print.  
;;; 2007.06.27 Dan
;;;             : * Changed the conflict-resolution-hook suppression warning
;;;             :   to be a model-warning so that it goes away when :v nil.
;;; 2007.10.26 Dan
;;;             : * Updated the conflict resolution trace to indicate when a
;;;             :   production isn't selected because it falls below the utility
;;;             :   minimum value.
;;; 2008.03.20 Dan
;;;             : * Added a case for !mv-bind! in the production printing code.
;;; 2008.07.22 Dan
;;;             : * Changed the call to compute-utility in conflict-resolution
;;;             :   to add the optional parameter with a value of t to have it
;;;             :   save the utility value - all other computed values are not
;;;             :   to be saved (for instance pmatches/whynot calls).
;;; 2008.07.31 Dan
;;;             : * Instead of using a table in conflict-resolution for holding
;;;             :   the utilities make that an extra slot of the production 
;;;             :   struct and use the productions instead of the names for the
;;;             :   conflict-set until it needs to go out to the hooks or
;;;             :   elsewhere.
;;; 2008.08.01 Dan
;;;             : * To avoid calling sgp procedural now owns :dat and monitors
;;;             :   :v.
;;; 2008.11.03 Dan
;;;             : * Updated conflict resolution to cache the buffer chunk in an
;;;             :   array and use cr-buffer-read to get them out for LHS tests.
;;; 2008.11.25 Dan
;;;             : * Added some new functions to make changing the internals
;;;             :   easier to improve for performance:
;;;             :   - add-production and remove-production functions abstract
;;;             :   the internal storage method for productions
;;;             :   - get-production-internal is added so that some code doesn't
;;;             :   need to do two look-ups for the procedural module - not
;;;             :   sure why I took that out back in 06...
;;;             : * Added productions-list to abstract away from using procedural-
;;;             :   productions in other places (procedural-cmds in particular).
;;;             : * Added a table to store productions to improve the average
;;;             :   operation of get-production.
;;; 2008.12.05 Dan
;;;             : * Moved away from the production conditions being represented
;;;             :   with lambdas to funcall during conflict resolution.
;;;             : * Also added a table for slot values like I did for buffer
;;;             :   chunks so that conflict-resolution doesn't need to repeatedly
;;;             :   get chunk slot values (multiple hash table lookups).
;;; 2008.12.08 Dan [1.4]
;;;             : * Significant overhaul of the internal representation of 
;;;             :   productions and the mechanisms used for conflict resolution.
;;;             :   No longer create lambdas for the conditions.  Instead, it
;;;             :   creates more specific single condition tests which are used
;;;             :   instead of relying on the general chunk-spec matcher.
;;;             : * Also added code to cache the buffer and slot values tested
;;;             :   so it doesn't have to look up the buffer/chunk/slot value
;;;             :   again and again.
;;; 2008.12.12 Dan
;;;             : * Fixed a bug in test-and-perform-bindings (not the typo in
;;;             :   the name however) for !bind! conditions.
;;; 2008.12.23 Dan
;;;             : * Added a new parameter :use-tree which if enabled uses a
;;;             :   decision tree in the conflict-resolution event to speed up
;;;             :   production matching.
;;;             :   The tree gets built when the model is loaded and doesn't
;;;             :   need to be recreated at reset time unless something in the
;;;             :   model has changed.
;;;             :   Should improve performance for most models.
;;; 2009.08.13 Dan [2.0a1]
;;;             : * Added two new parameters to allow for testing out the 
;;;             :   possibility of a "new" idea in the production matching.
;;;             :   The new concept is to allow partial matching to occur on
;;;             :   the LHS in the tests of the buffers' slots.
;;;             :
;;;             :   Here is how it will work at this point:
;;;             :
;;;             :   If the :ppm parameter (procedural partial matching)
;;;             :   is set to nil then there is no change in how production
;;;             :   matching occurs.  This will be the default value and
;;;             :   thus all existing models will continue to work exactly
;;;             :   the same as they did before this was added.
;;;             :   If :ppm is set to a number, then all of the slots tested
;;;             :   for equality on the LHS of a production (those without a
;;;             :   modifier or with the explicit = modifier) will be allowed
;;;             :   to have a partial matching.  The slot value will be 
;;;             :   considered a match if:
;;;             :   
;;;             :   - it is a symbolic match as would normally occur without 
;;;             :   this addition
;;;             :   
;;;             :   OR  
;;;             :   
;;;             :   - there is a similarity value between the indicated value 
;;;             :   and the value in the slot that is > the maximum similarity 
;;;             :   difference
;;;             :   
;;;             :   
;;;             :   The similarity value used is the standard one returned by 
;;;             :   the declarative similarity function and thus the existing 
;;;             :   :similarity-hook can be used if custom similarities are 
;;;             :   needed.  The requirement that it be strictly greater than 
;;;             :   the max difference means that only things which have an 
;;;             :   explicit similarity setting can possibly be mismatched, and 
;;;             :   it provides a way to specify a "don't match these in the 
;;;             :   productions" even if a similarity value may be needed for 
;;;             :   declarative retrieval purposes.
;;;             :   
;;;             :   All slots must still be a match for the production to be 
;;;             :   considered in the conflict set.
;;;             :   
;;;             :   If all of the production's tests are perfect matches then 
;;;             :   its utility will be unaffected.
;;;             :   
;;;             :   If any slot is mismatched, then the utility of the 
;;;             :   production will be adjusted, by default, by adding the 
;;;             :   similarity of all the mismatches times a penalty factor.  
;;;             :   The penalty factor will be the setting of :ppm.  Alternatively, 
;;;             :   there is a hook :ppm-hook which can be used to override the 
;;;             :   utility penalty when slot tests are mismatched.  The hook
;;;             :   function will be passed a production name and a list of 
;;;             :   mismatch lists, one for each mismatch which occurred while
;;;             :   testing the named production. A mismatch list will be a 5 
;;;             :   element list consisting of: a buffer name, a slot name, the 
;;;             :   requested slot value, the actual value in the slot of the 
;;;             :   chunk in the buffer, and the reported similarity between those
;;;             :   two items.  If the hook returns a number that will be added to
;;;             :   the production's utility, any other return value will result in
;;;             :   the default calculation being added.
;;;             :   
;;;             :   It is important to note that this is very exploratory at this
;;;             :   time and may undergo significant changes if it persists at all.
;;; 2009.08.14 Dan
;;;             : * Changed print-instantiation so that it prints partially matched
;;;             :   information when appropriate.
;;; 2009.08.17 Dan
;;;             : * Fixed test-other-condition with respect to PPM so that it 
;;;             :   can partial match a value in a variablized slot in a p*.
;;; 2009.09.10 Dan
;;;             : * Added the require-compiled for the code that's now in production-
;;;             :   parsing-support.lisp.
;;; 2009.09.14 Dan
;;;             : * Simplified conflict-resolution by colapsing the tree/non-tree
;;;             :   branch.
;;;             : * Moved the warning about using the tree with :ppm or :crt on
;;;             :   to reset time instead of every conflict-resolution.
;;; 2009.09.18 Dan
;;;             : * Fixed a bug introduced with moving the warning from the
;;;             :   previous update.
;;;             : * Took the require-compiled of production-parsing out for now
;;;             :   to avoid an issue with when the production structure is
;;;             :   created.
;;; 2009.10.15 Dan
;;;             : * All new conflict-resolution to support the ability of a
;;;             :   production to search a multi-buffer for a chunk to match.
;;;             :   The search occurs after the constants have been tested and
;;;             :   things not dependent on search bindings have occurred (evals
;;;             :   explicit binds and so forth).  A searched buffer cannot
;;;             :   be used to name a slot for dynamic matching, but can have
;;;             :   dynamic slots that are named in standard buffers.
;;;             : * The general description is that the searches occur in parallel
;;;             :   across all the searced buffers and only one match is found
;;;             :   for each.  Then conditions among those searched buffers are
;;;             :   tested.  If there is a failure at that point the production
;;;             :   doesn't match -- it doesn't search for a set of chunks which
;;;             :   satisfy all the conditions.
;;; 2009.12.03 Dan
;;;             : * Added the reporting of utilities to the conflict-set trace.
;;; 2009.12.04 Dan
;;;             : * Flag conflict-resolution events as dynamic when scheduled so
;;;             :   that they can be moved "back" in a real-time slack hook that
;;;             :   uses dynamics.
;;; 2010.01.11 Dan
;;;             : * Updates to the buffer search code to address some problems
;;;             :   in the search code (doesn't affect non-search matching).
;;; 2010.01.12 Dan
;;;             : * Fixed a bug with the utility printing in the conflict-set
;;;             :   trace.
;;; 2010.01.12 Dan
;;;             : * Fixed some issues with the utility-offset code for searched
;;;             :   buffers.
;;; 2010.01.15 Dan
;;;             : * Fixed an issue with test-search-buffers because a module may
;;;             :   be adding the items to the buffer set in the search call.
;;; 2010.01.18 Dan
;;;             : * Added a keyword parameter to conflict-tests to allow the
;;;             :   :crt info to be suppressed during whynot and other user
;;;             :   commands. 
;;; 2010.04.23 Dan
;;;             : * Updates to remove some assumptions about tests and generally
;;;             :   be more strict about ANSI CL requirements (in particular
;;;             :   that test functions don't have to return t for true).
;;;             :   - cr-condition-equal now uses eql for the value tests to
;;;             :     make sure numerical slot conditions can be considered
;;;             :     the same (doesn't affect the matching itself).
;;;             :   - Changed any tests for chunk-slot-equal to safe-chunk-slot-equal
;;;             :     since that's now the function used.
;;; 2010.10.15 Dan
;;;             : * Updates to how the conflict set trace prints because it had
;;;             :   some "unusual" output because of the search buffer addition.
;;; 2010.11.07 Dan
;;;             : * Fixed a potential bug with the search buffer code that could
;;;             :   result in bindings in the selected production from being set
;;;             :   to values from other search results.  [There're actually two
;;;             :   potential fixes here, but one is commented out for now.  The
;;;             :   reason for that is because I don't know which is really more 
;;;             :   efficient without enough test cases, but I think the one I've
;;;             :   used should be the better one in most cases.]
;;; 2010.12.07 Dan
;;;             : * Changed print-production and print-production-text to add
;;;             :   an optional parameter that can be used to direct the output
;;;             :   to the model trace instead of the command trace.  It's just
;;;             :   a bunch of duplicated code with ifs for now...
;;; 2010.12.22 Dan
;;;             : * Adding a run-notify function to the module so that it can
;;;             :   check to see if it needs to schedule a conflict-resolution
;;;             :   action because of an abnormal situaiton (a hard break
;;;             :   during a conflict-resolution processing for example).
;;; 2011.01.11 Dan
;;;             : * Changed :run-notify to :run-start since define-module was
;;;             :   changed.
;;; 2011.04.25 Dan
;;;             : * Removed an unneeded optional parameter from test-constant-condition.
;;;             : * Realized why that optional was there and put it back.
;;; 2011.04.26 Dan
;;;             : * Use mp-time-ms for all time references instead of mp-time.
;;; 2011.04.27 Dan
;;;             : * Cache the buffer's chunk-type along with the chunk in the
;;;             :   conflict resolution table and add cr-buffer-type-read to
;;;             :   get it out.  Improves performance by more than 15% in the
;;;             :   performance test models without use-tree enabled.
;;; 2011.04.28 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;;             : * Using the warning suppression flag added to chunks in the
;;;             :   extension of productions as well.
;;; 2013.01.25 Dan
;;;             : * Changed slot-value-from-index and get-slot-index to use the
;;;             :   new index accessors for chunk-types to fix a potential issue
;;;             :   with matching extended types in a hierarchy.
;;; 2013.03.13 Dan
;;;             : * Fixed a bug with get-slot-index because it would allow a
;;;             :   non-existent slot to match if it was possible for a child or
;;;             :   other related type.
;;; 2013.03.13 Dan [2.1]
;;;             : * Because productions now allow for child slots to be specified
;;;             :   under a parent type the testing of a slot constraint now must
;;;             :   also test that the chunk in the buffer actually have such a 
;;;             :   slot for constants just as it does for variablized slots in a
;;;             :   p* (basically undoing the previous fix to make this work right
;;;             :   and replacing it with something better).
;;; 2013.05.16 Dan
;;;             : * Very subtle change, but conceptually significant.  For a 
;;;             :   slot condition to match the chunk must have a slot with the
;;;             :   value specified even when that value is nil.  This is how
;;;             :   I've always described it, but it turns out that "<slot> nil" 
;;;             :   has slipped through as successful as long as the type has such
;;;             :   a slot even if the chunk itself doesn't i.e. until now extending 
;;;             :   the type could indirectly affect how a production matches without
;;;             :   directly modifying any of the chunks in buffers that it tests.
;;; 2013.05.21 Dan
;;;             : * For static chunks the previous change is now not true --
;;;             :   "<slot> nil" tests are true if the chunk could have the slot
;;;             :   but doesn't or if it has the slot and it is empty.  It's a 
;;;             :   middle ground between the current mechanism and what I would
;;;             :   like to transition to where "slot nil" means the slot doesn't
;;;             :   exist instead of treating nil as a value for a slot to have.
;;;             :   Making that transition however would require redoing the 
;;;             :   matching code since get-slot-index would then need to return
;;;             :   something that allows for that to succeed since now a nil
;;;             :   return from that terminates the matching.
;;; 2013.06.05 Dan
;;;             : * Changed how the "slot nil" tests work for the static chunk
;;;             :   types to actually make it true when the chunk doesn't have
;;;             :   the slot at all.  The quick fix for now is to just catch
;;;             :   the nil get-slot-index situations where one might be testing
;;;             :   for nil and flag them as true in the corresponding test 
;;;             :   functions.
;;; 2013.07.25 Dan  [2.2]
;;;             : * Added some "style" warnings to the production parsing which
;;;             :   indicate situations which don't prevent the production from
;;;             :   being defined, but which may pose a problem at run time for things
;;;             :   like jamming a module, being invalid for production compilation,
;;;             :   or testing/setting things that aren't used anywhere else.
;;;             : * The :style-warnings parameter can be set to nil to disable
;;;             :   the new warnings.
;;; 2013.08.07 Dan
;;;             : * Fixed the style warning code so that it doesn't try to parse
;;;             :   direct requests for slots and looks for chunks initially in
;;;             :   the buffers as well as scheduled settings.
;;; 2013.08.15 Dan
;;;             : * Changed the style warnings so that basically any test in the
;;;             :   conditions will suppress the modification warning.
;;; 2013.10.18 Dan
;;;             : * Finally fixed the typo in test-and-perfrom.
;;; 2014.02.13 Dan
;;;             : * Updated the failure-reason-string to explicitly check the
;;;             :   slot value/existance because static chunks cache a nil for
;;;             :   non-existant slots as if they exist which was resulting in
;;;             :   inconsistent whynot info.
;;; 2014.02.17 Dan
;;;             : * Fixed a bug with finalize-procedural-reset because when it
;;;             :   cheated and looked through the event queue it didn't verify
;;;             :   that it was the current model's event that was setting the
;;;             :   chunk in a buffer.
;;; 2014.03.17 Dan [3.0]
;;;            : * Changed the query-buffer call to be consistent with the new
;;;            :   internal code.
;;; 2014.03.20 Dan
;;;            : * More serious work on the transition to the typeless chunks
;;;            :   and some general cleanup of the code because there's still
;;;            :   references to ACT-R 4/5 stuff that doesn't need to be here
;;;            :   any more.
;;; 2014.03.28 Dan
;;;            : * Printing a production now works off of the statements in
;;;            :   the production instead of the text itself which means that
;;;            :   it won't print the isa, but will print any default values
;;;            :   that come from the isa.  
;;;            : * Simplified things so that partially matched instantiations
;;;            :   are printed by the same code that does normal instantiations.
;;; 2014.04.01 Dan
;;;            : * Used slots-vector-match-signature instead of doing the bit
;;;            :   tests directly.
;;; 2014.04.04 Dan
;;;            : * Start to fix issues with the style warnings now that there 
;;;            :   aren't any chunk-types in the productions.
;;;            : * Replaced get-slot-index with slot-name->index.
;;; 2014.04.07 Dan
;;;            : * Changed failure-reason-string to not require procedural.
;;;            : * Removed the require for parsing support and moved all the
;;;            :   structure definitions here.
;;;            : * Fixed a bug with how production-statement-text handled things
;;;            :   that were partially matched.
;;; 2014.05.23 Dan
;;;            : * Fixed a bug with !mv-bind! allowing nil bindings since this
;;;            :   (find nil (list nil nil)) will return nil when it finds it...
;;;            : * Can't instantiate the chunk-specs when using production-statement-
;;;            :   text because that results in extending things "before" they
;;;            :   should be.  In particular, it means that if :cst is enabled
;;;            :   all the "extend-buffer-chunk ..." events will be prevented
;;;            :   which is not good.
;;; 2014.05.30 Dan
;;;            : * Added the in-model-definition slot to the module which gets
;;;            :   set in the initial reset function and cleared in the final one
;;;            :   so that the backward compatible production syntax hacking is
;;;            :   only applied to the initial productions.
;;; 2014.06.12 Dan
;;;            : * Adjust how variablized slots are tested to eliminate the need
;;;            :   for the slot to exist.  Changed the test-var-slot cases of the
;;;            :   test-other-condition and test-search-constants functions and
;;;            :   the corresponding failure-reason-string.
;;; 2014.06.18 Dan
;;;             : * When binding to a variablized slot make sure it exists.
;;;             : * Take advantage of valid-slot-name now returning the index
;;;             :   for true.
;;; 2014.06.26 Dan
;;;             : * When printing a production with the backward compatible 
;;;             :   switch set write an isa chunk into each request action so
;;;             :   that if it gets read back in it's not converted into a
;;;             :   modification request.
;;; 2014.08.13 Dan
;;;             : * Add another table to save conditions for style checks 
;;;             :   because a non-goal/imaginal/retrieval buffer which modifies
;;;             :   and tests its slots shouldn't throw a warning.
;;; 2014.08.18 Dan
;;;             : * Don't give the "no condtion" style warning for buffers that
;;;             :   are of the perceptual compilation type.
;;; 2014.09.22 Dan
;;;             : * Instead of excluding the perceptual buffers from the "no
;;;             :   condition" warning replace it with a warning when there's
;;;             :   only an isa that has no effect which isn't "isa chunk".  
;;;             :   That way it's fine to test that there's a chunk in the 
;;;             :   buffer with just "=buffer>" or "=buffer> isa chunk" since
;;;             :   a buffer test is required for a modification action, but
;;;             :   anything else gets flagged since it's not the same as with
;;;             :   ACT-R 6.0 and could be a problem.
;;; 2014.11.11 Dan
;;;             : * Added a check to extend-productions to make sure the
;;;             :   parameter name is a symbol and not a keyword.
;;;             : * Test the default-function passed to extend-productions to 
;;;             :   make sure it's a valid function designator.
;;; 2014.11.13 Dan
;;;             : * Removed the function test from extend-productions because of
;;;             :   the potential for issues at compile time.
;;; 2015.06.04 Dan
;;;             : * Changed all scheduling to specify :time-in-ms t.
;;;             : * Convert :dat to ms at setting.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;             : * Removed the *act-r-6.0-compatibility* hack.
;;; 2015.08.12 Dan [3.1]
;;;             : * Change how a search buffer is matched by not actually putting
;;;             :   a chunk into the buffer itself but just set the entry in the
;;;             :   lookup matrix for the chunk (since the search testing gets
;;;             :   slot values from that chunk instead of the matrix itself).
;;;             :   This fixes an issue with generating a bunch of unneeded chunks
;;;             :   for a copy buffer.  However, this has exposed a bug with both
;;;             :   the previous and current versions of matching with a serach
;;;             :   copy buffer -- the binding for the buffer variable itself is 
;;;             :   not going to match the name of the chunk in the buffer at the
;;;             :   time the production fires.  Under the new system it will 
;;;             :   however be consistent with the non-copied searchable multi-
;;;             :   buffers because it will bind to the name of the chunk in the
;;;             :   buffer set to which it was matched.
;;; 2015.08.17 Dan
;;;             : * Changed the randomize-time call to randomize-time-ms since
;;;             :   it is a millisecond based time.
;;; 2015.08.28 Dan
;;;             : * Just a note about the issue described in the 3.1 update.
;;;             :   While the buffer set chunk is used in the matching for both
;;;             :   a copy and non-copy searchable buffer the buffer varaible is
;;;             :   now rebound once a copy buffer creates the new chunk.
;;; 2015.09.10 Dan [4.0]
;;;             : * Productions now record the returned specs for the requested
;;;             :   actions they make for use with the new utility learning 
;;;             :   assignment approach that only rewards those for which the
;;;             :   actions have completed.  That's a new production parameter
;;;             :   instead of a slot because the utility module needs to be
;;;             :   able to read it.
;;;             : * Because the procedural module itself uses a request to signal
;;;             :   busy/free that will also be stored with the production's 
;;;             :   selection so that the production's firing gets handled the
;;;             :   same way as requests.
;;;             : * Moved the production and production parameter code to a
;;;             :   separate file in support which gets loaded with a require-
;;;             :   compiled.
;;; 2015.09.14 Dan
;;;             : * Make the production buffer trackable.
;;; 2015.12.16 Dan
;;;             : * Saving the tag in production-requested-actions now needs to
;;;             :   get the second return value since scheduling a request now
;;;             :   returns the event again as the first value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Important note to module writers:
;;;
;;;   The RHS actions of a production are scheduled with the following
;;;   priorities:
;;;      = priority 100
;;;      @ (buffer overwrite) 90
;;;      * priority 60
;;;      + priority 50
;;;      - priority 10
;;;
;;;   So, the module does NOT get a chance to look at the buffers before the
;;;   production has its way with them.
;;;  
;;;   The recommended thing to do when you get your request (from a +)
;;;   is to schedule your action for a priority < 10.  The production's
;;;   actions should all occur before your module starts to do anything,
;;;   you shouldn't change the state of things before the other modules 
;;;   have had a chance to look at it if they need to, and since the buffer
;;;   is cleared with priority 10 a request which sets the buffer sooner
;;;   is just going to be cleared anyway.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Unbound RHS variables are currently not useable.  Most uses were as a short-
;;; hand for "- {slot} nil" which can just be made explicit now.  The only other
;;; use (in ACT-R 5 terms at least) was in retrievals for checking that two
;;; slots had the same value.  The question becomes whether that's a desired
;;; thing or just a hold over from when retrievals were LHS tests.  For now,
;;; it's not going to be possible, but if it's wanted it can be added back in
;;; in any of a couple of different ways.
;;;
;;; I've come to realize why returning a list with the first element being 
;;; the count and the rest being the references is good for the successes, 
;;; failures and efforts production parameters...  So, that's not going
;;; to change despite my separating those pieces internally.
;;; 
;;; When setting efforts with a number and pl is a number it sets the
;;; list of efforts based on the munber of successes and failures instead
;;; of just successes as was done previously.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(declaim (ftype (function (t) t) compile-productions))
(declaim (ftype (function (t) t) learn-parameters))
(declaim (ftype (function (t) t) productions-action-time))
(declaim (ftype (function (t) t) note-production-selection))
(declaim (ftype (function (t) t) spp-fct))
(declaim (ftype (function (t &optional t) t) compute-utility))
(declaim (ftype (function (t) t) get-valid-productions))
(declaim (ftype (function () t) minimum-utility))
(declaim (ftype (function (t t) t) replace-variables-for-eval))
(declaim (ftype (function (t t) t) replace-variables))
(declaim (ftype (function (t t) t) similarity-fct))
(declaim (ftype (function (t) t) conflict-node-valid))
(declaim (ftype (function (t t) t) copy-conflict-tree))
(declaim (ftype (function (t) t) build-conflict-tree))
(declaim (ftype (function (&key) t) make-root-node))
(declaim (ftype (function (t t) t) remove-production-from-tree))
(declaim (ftype (function (t t) t) add-production-to-tree))
(declaim (ftype (function (t) t) compilation-buffer-type-fct))


(require-compiled "CENTRAL-PARAMETERS" "ACT-R-support:central-parameters")
(require-compiled "PRODUCTIONS" "ACT-R-support:productions")


;;; The structures for the module and a production parameter

(defstruct procedural productions p-table
  er crt cst v dat
  conflict-set-hook 
  cycle-hook
  bindings 
  lhst rhst
  delayed-resolution
  unharvested-buffers
  busy
  random-times
  (check-p*-mods t)
  
  buffer-indices
  buffer-lookup
  buffer-lookup-size
  slot-lookup
  
  conflict-tree
  last-conflict-tree
  delay-tree
  use-tree
  
  req-spec
  
  current-p
  ppm
  md
  ppm-hook
  
  used-search-buffers
  search-buffer-table
  search-matches-table
  temp-search
  last-cr-time
  
  style-warnings style-check cond-style-usage-table req-style-usage-table
  retrieval-cond-style-usage-table retrieval-req-style-usage-table
  mod-style-usage-table init-chunk-slots all-cond-style-usage-table
  
  (action-parse-table (make-hash-table :test 'equal))
  (condition-parse-table (make-hash-table :test 'equal))
  
  in-model-definition)


(defstruct production-statement op target definition spec)

;;; Add a production parameter for storing the requested actions
;;; which utility learning will use, but can't do it there since
;;; I want to avoid warnings about the functions and the declaim
;;; for a setf function doesn't isn't working as I expect.

(suppress-extension-warnings)
(extend-productions requested-actions)
(unsuppress-extension-warnings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions.

(defun productions-list (prod)
  (procedural-productions prod))

(defun get-production (production-name)
  (let ((procedural (get-module procedural)))
    (when procedural
      (get-production-internal production-name procedural))))

(defun get-production-internal (production-name procedural)
  (gethash production-name (procedural-p-table procedural)))

(defun add-production (production procedural)
  (push-last production (procedural-productions procedural))
  (setf (gethash (production-name production) (procedural-p-table procedural)) production)
  
  (when (and (procedural-use-tree procedural) (not (procedural-delay-tree procedural)))
    (add-production-to-tree production procedural)))

(defun remove-production (production procedural)
  (setf (procedural-productions procedural)
    (remove production (procedural-productions procedural)))
  (remhash (production-name production) (procedural-p-table procedural))
  
  (when (and (procedural-use-tree procedural) (not (procedural-delay-tree procedural)))
    (remove-production-from-tree production procedural)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procedural-params (prod param)
  (cond ((consp param)
         
         ;; Changing procedural parameters reschedules conflict resolution
         ;; if it's waiting to happen
         
         (un-delay-conflict-resolution)
         
         (case (car param)
           (:use-tree (setf (procedural-use-tree prod) (cdr param)))
           
           (:er (setf (procedural-er prod) (cdr param)))
           
           (:v (setf (procedural-v prod) (cdr param)))
           (:md (setf (procedural-md prod) (cdr param)))
           (:ppm (if (cdr param)
                     (unless (find 'ppm-offset (no-output (car (sgp :utility-offsets))))
                       (sgp :utility-offsets ppm-offset))
                   (let ((previous (car (no-output (sgp :utility-offsets)))))
                     (sgp :utility-offsets nil)
                     (dolist (x previous)
                       (unless (eq x 'ppm-offset)
                         (sgp-fct (list :utility-offsets x))))))
                 (setf (procedural-ppm prod) (cdr param)))
                 
           
           (:ppm-hook (setf (procedural-ppm-hook prod) (cdr param)))
           (:dat (setf (procedural-dat prod) (safe-seconds->ms (cdr param))) (cdr param))
           
           (:crt (setf (procedural-crt prod) (cdr param)))
           (:cst (setf (procedural-cst prod) (cdr param)))
           
           (:lhst (setf (procedural-lhst prod) (cdr param)))
           (:rhst (setf (procedural-rhst prod) (cdr param)))
           
           (:vpft (setf (procedural-random-times prod) (cdr param)))
           
           (:style-warnings (setf (procedural-style-warnings prod) (cdr param)))
           
           
           (:do-not-harvest
            (if (cdr param)
              (if (find (cdr param) (procedural-unharvested-buffers prod))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the list."
                 :do-not-harvest  (cdr param))
                (push (cdr param) (procedural-unharvested-buffers prod)))
              ;; setting to nil clears all automatically
              (setf (procedural-unharvested-buffers prod) nil)))
           
           (:cycle-hook
            (if (cdr param)
                (if (member (cdr param) (procedural-cycle-hook prod))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :cycle-hook
                     (cdr param))
                  (push (cdr param) (procedural-cycle-hook prod)))
              (setf (procedural-cycle-hook prod) nil)))
           
           (:conflict-set-hook
            (if (cdr param)
                (if (member (cdr param) (procedural-conflict-set-hook prod))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :conflict-set-hook
                     (cdr param))
                  (push (cdr param) (procedural-conflict-set-hook prod)))
              (setf (procedural-conflict-set-hook prod) nil)))))
        (t 
         (case param
           (:use-tree (procedural-use-tree prod))
           (:ppm (procedural-ppm prod))
           (:ppm-hook (procedural-ppm-hook prod))
           
           (:dat (ms->seconds (procedural-dat prod)))
           (:crt (procedural-crt prod))
           (:cst (procedural-cst prod))
           
           (:lhst (procedural-lhst prod))
           (:rhst (procedural-rhst prod))
           
           (:vpft (procedural-random-times prod))
           
           (:style-warnings (procedural-style-warnings prod))
           
           (:do-not-harvest
            (procedural-unharvested-buffers prod))
           
           (:cycle-hook (procedural-cycle-hook prod))
           (:conflict-set-hook (procedural-conflict-set-hook prod))))))


(defun reset-procedural-module (prod)
  (setf (procedural-productions prod) nil)
  (setf (procedural-p-table prod) (make-hash-table :size 31 :test 'eq))
  (setf (procedural-bindings prod) nil)
  (setf (procedural-delayed-resolution prod) nil)
  
  (setf (procedural-buffer-indices prod) nil)  
  (setf (procedural-buffer-lookup-size prod) 0) 
  
  (setf (procedural-busy prod) nil)
  
  (setf (procedural-req-spec prod) (define-chunk-spec isa chunk))
  
  (setf (procedural-delay-tree prod) t)
  
  (unless (procedural-conflict-tree prod)
    (setf (procedural-conflict-tree prod) (make-root-node)))
  
  (setf (procedural-search-buffer-table prod) (make-hash-table))
  (setf (procedural-search-matches-table prod) (make-hash-table))
  
  (setf (procedural-cond-style-usage-table prod) (make-hash-table))
  (setf (procedural-all-cond-style-usage-table prod) (make-hash-table))
  (setf (procedural-req-style-usage-table prod) (make-hash-table))
  (setf (procedural-mod-style-usage-table prod) (make-hash-table))
  (setf (procedural-retrieval-cond-style-usage-table prod) (make-hash-table))
  (setf (procedural-retrieval-req-style-usage-table prod) (make-hash-table))
  
  (setf (procedural-last-cr-time prod) nil)
  
  (setf (procedural-style-check prod) nil)
  
  (setf (procedural-init-chunk-slots prod) (make-hash-table))
  
  (setf (procedural-in-model-definition prod) t)
  
  (schedule-event-now 'conflict-resolution :module 'procedural 
                      :priority :min
                      :destination 'procedural  
                      :output 'medium))



(defvar *safe-perceptual-buffers* nil)


(defun check-production-for-style (prod p)
  (let ((queries nil))
    (dolist (c (production-lhs p))
      (let* ((def (production-statement-definition c))
             (buffer (production-statement-target c))
             (compilation-type (compilation-buffer-type-fct buffer))
             (op (production-statement-op c))
             (spec (production-statement-spec c)))
        
        ;; record which buffers are queried
        (when (eq op #\?)
          (push buffer queries))
                   
        ;; save the slots that are tested for goal, imaginal, and retrieval
        ;; style buffers 
                
        (when (and (eq op #\=)
               )
        
        (let ((table (if (eq compilation-type 'retrieval)
                         (procedural-retrieval-cond-style-usage-table prod)
                       (if (or (eq compilation-type 'goal)
                               (eq compilation-type 'imaginal)
                               (eq compilation-type 'retrieval))
                           (procedural-cond-style-usage-table prod)
                         (procedural-all-cond-style-usage-table prod)))))
          
          (let* ((slots (mapcan (lambda (x)
                                  (when (not (chunk-spec-variable-p (act-r-slot-spec-name x)))
                                    (list (act-r-slot-spec-name x))))
                          (act-r-chunk-spec-slots spec))))
            
            (setf (gethash buffer table) (remove-duplicates (append (gethash buffer table) slots))))))
    
        ;; check for any conditions with no slots tested and
        ;; which specified an isa.
    
      (when (and (eq op #\=)
                 (null (act-r-chunk-spec-slots spec))
                 (not (equalp def '(isa chunk))))
        (model-warning "Production ~s has a condition for buffer ~s with an isa that provides no tests." (production-name p) buffer))))
    
    
    (dolist (a (production-rhs p))
      (let* ((buffer (production-statement-target a))
             (compilation-type (compilation-buffer-type-fct buffer))
             (op (production-statement-op a))
             (spec (production-statement-spec a)))
        
        ;; record which slots are modified for any buffer
        ;; and those that are requested (either a normal or modification request) for 
        ;; goal, imaginal, or retrieval buffers
        
        (when (and
               (or (eq op #\=)
                   (and (or (eq op #\+) (eq op #\*))
                        (or (eq compilation-type 'goal)
                            (eq compilation-type 'imaginal)
                            (eq compilation-type 'retrieval))))
               spec) ;; it's got something specified...
        
          (let ((table (if (or (eq op #\=) (eq op #\*))
                           (procedural-mod-style-usage-table prod)
                         (if (eq compilation-type 'retrieval)
                             (procedural-retrieval-req-style-usage-table prod)
                           (procedural-req-style-usage-table prod))))
                
                (slots (mapcan (lambda (x)
                                 (let ((slot (act-r-slot-spec-name x)))
                                   (unless (or (keywordp slot) (chunk-spec-variable-p slot))
                                     (list slot))))
                         (act-r-chunk-spec-slots spec))))
            
            (setf (gethash buffer table) (remove-duplicates (append (gethash buffer table) slots)))))))
    
    (flet ((compare-actions (x y) 
                            (and (eq (production-statement-op x) (production-statement-op y))
                                 (eq (production-statement-target x) (production-statement-target y)))))
      
      (dolist (a (remove-duplicates (production-rhs p) :test #'compare-actions))
        (let* ((buffer (production-statement-target a))
               (compilation-type (compilation-buffer-type-fct buffer))
               (op (production-statement-op a)))
        
          (when (> (count a (production-rhs p) :test #'compare-actions) 1)
            (case op
              (#\* (model-warning "Production ~s makes multiple modification requests to the ~s buffer." (production-name p) buffer))
              (#\+ (model-warning "Production ~s makes multiple requests to the ~s buffer." (production-name p) buffer))
              (#\= (model-warning "Production ~s makes multiple modifications to the ~s buffer." (production-name p) buffer))
              (#\- (model-warning "Production ~s clears the ~s buffer multiple times." (production-name p) buffer))
              (#\@ (model-warning "Production ~s overwrites the ~s buffer multiple times." (production-name p) buffer))))
        
          (when (and (eq op #\+)
                     (not (find buffer queries))
                     (not (or (eq compilation-type 'goal)
                              (eq compilation-type 'retrieval)
                              (and (eq compilation-type 'perceptual)
                                   (find buffer *safe-perceptual-buffers*)))))
            (model-warning "Production ~s makes a request to buffer ~s without a query in the conditions." (production-name p) buffer)))))))


(defun check-between-production-style (prod)
  
  ;; Check the slots in the conditions to make sure that they
  ;; are set in requests, set in initial conditions, or modified in some production.
  
  (maphash (lambda (buffer slots)
             (let ((requests (gethash buffer (procedural-req-style-usage-table prod)))
                   (mods (gethash buffer (procedural-mod-style-usage-table prod)))
                   (init (gethash buffer (procedural-init-chunk-slots prod))))
               (dolist (slot slots)
                 (unless (or (find slot requests)
                             (find slot mods)
                             (find slot init))
                   (model-warning "Productions test the ~s slot in the ~s buffer which is not requested or modified in any productions." slot buffer)))))
           (procedural-cond-style-usage-table prod))
  
  #|

  This test isn't meaningful now, but do I want to generalize
  that in some way to look for patterns in chunks created or in
  the initial DM?  The thing that makes this different from the previous
  ones is that a retrieval condition is likely to use information not 
  spcified in the request or modified anywhere.  
  
  ;; Check the retrieval buffer conditions to verify that
  ;; all of the tested types are either requested or set in 
  ;; the initial model definition  -- supertypes and subtypes
  ;; are allowed since a retrieval request may retrieve any
  ;; subtype.
  
  (maphash (lambda (buffer tests)
             (let ((requests (gethash buffer (procedural-retrieval-req-style-usage-table prod))))
               (dolist (type-and-slots tests)
                 (destructuring-bind (type &rest slots) type-and-slots
                   (declare (ignore slots))
                   (unless (or
                            (find type requests :key 'car :test (lambda (x y)
                                                                  (or (chunk-type-subtype-p-fct x y) (chunk-type-subtype-p-fct y x))))
                            (find type (mapcar 'second (remove-if-not (lambda (x) (eq (car x) buffer)) (procedural-init-chunk-slots prod))) 
                                  :test (lambda (x y)
                                          (or (chunk-type-subtype-p-fct x y) (chunk-type-subtype-p-fct y x)))))
                     (model-warning "Productions test the ~s buffer for a chunk of type ~s which is not requested by any of the productions or set in the initial model."
                                    buffer type))))))
           (procedural-retrieval-cond-style-usage-table prod))
  |#
  
  ;; Check the goal and imaginal requests to make sure that the slots
  ;; that get set are tested/used elsewhere (either a condition in the same buffer,
  ;; in a request to any retrieval buffer, or a condition in any retrieval buffer.
  
  (maphash (lambda (buffer slots)
             (let ((conditions (gethash buffer (procedural-cond-style-usage-table prod)))
                   (other-conds (gethash buffer (procedural-all-cond-style-usage-table prod)))
                   (retrieval-conds (let ((res nil))
                                      (maphash (lambda (key value)
                                                 (declare (ignore key))
                                                 (setf res (append value res)))
                                               (procedural-retrieval-cond-style-usage-table prod))
                                      res))
                   (retrieval-requests (let ((res nil))
                                      (maphash (lambda (key value)
                                                 (declare (ignore key))
                                                 (setf res (append value res)))
                                               (procedural-retrieval-req-style-usage-table prod))
                                      res)))
               
               (dolist (slot slots)
                 (unless (or (find slot conditions)
                             (find slot other-conds)
                             (find slot retrieval-conds)
                             (find slot retrieval-requests))
                   (model-warning "Productions request a value for the ~s slot in a request to the ~s buffer, but that slot is not used in other productions."
                                        slot buffer)))))
           (procedural-req-style-usage-table prod))
  
  ;; Check the modifications for all buffers to make sure that the slots
  ;; that get set are tested/used elsewhere (either a condition in the same buffer,
  ;; in a request to any retrieval buffer, or a condition in any retrieval buffer.
  
  (maphash (lambda (buffer slots)
             (let ((conditions (gethash buffer (procedural-cond-style-usage-table prod)))
                   (other-conds (gethash buffer (procedural-all-cond-style-usage-table prod)))
                   (retrieval-conds (let ((res nil))
                                      (maphash (lambda (key value)
                                                 (declare (ignore key))
                                                 (setf res (append value res)))
                                               (procedural-retrieval-cond-style-usage-table prod))
                                      res))
                   (retrieval-requests (let ((res nil))
                                      (maphash (lambda (key value)
                                                 (declare (ignore key))
                                                 (setf res (append value res)))
                                               (procedural-retrieval-req-style-usage-table prod))
                                      res)))
               
               (dolist (slot slots)
                 (unless (or (find slot conditions)
                             (find slot other-conds)
                             (find slot retrieval-conds)
                             (find slot retrieval-requests))
                   (model-warning "Productions modify the ~s slot in the ~s buffer, but that slot is not used in other productions."
                                        slot buffer)))))
             
           (procedural-mod-style-usage-table prod)))

(defun finalize-procedural-reset (prod)
  
  (setf (procedural-in-model-definition prod) nil)
  
  (setf (procedural-delay-tree prod) nil)
  
  (when (procedural-style-warnings prod)
    
    (setf (procedural-style-check prod) t)
    
    ;; cheat and look through the queue for chunks being set in buffers (catches goal-focus and
    ;; possibly other initial buffer setting actions)
    
    (dolist (event (meta-p-events (current-mp)))
      (when (and (eq (evt-model event) (current-model))
                 (or (eq (evt-action event) 'set-buffer-chunk)
                     (eq (evt-action event) #'set-buffer-chunk)))
        (let ((params (evt-params event)))
          (setf (gethash (first params) (procedural-init-chunk-slots prod)) 
            (remove-duplicates (append (gethash (first params) (procedural-init-chunk-slots prod)) (chunk-filled-slots-list-fct (second params))))))))
    
    ;; also look at the buffers themselves to see if there are any chunks there
    (dolist (buffer (buffers))
      (awhen (buffer-read buffer)
             (setf (gethash buffer (procedural-init-chunk-slots prod)) 
               (remove-duplicates (append (gethash buffer (procedural-init-chunk-slots prod)) (chunk-filled-slots-list-fct it))))))
    
    (dolist (x (procedural-productions prod))
      (check-production-for-style prod x))
    
    (check-between-production-style prod))
  
  (when (procedural-use-tree prod)
    
    (when (or (procedural-crt prod)
              (procedural-ppm prod))
      (model-warning "Conflict resolution cannot use the decision tree when :crt or :ppm is enabled."))
    
    (cond ((null (procedural-last-conflict-tree prod))
           (build-conflict-tree prod)
           (setf (procedural-last-conflict-tree prod) (copy-conflict-tree (procedural-conflict-tree prod) nil)))
          ((equal (mapcar 'production-name (productions-list prod)) (conflict-node-valid (procedural-conflict-tree prod)))
           ;; assume everything still valid
           )
          ((equal (mapcar 'production-name (productions-list prod)) (conflict-node-valid (procedural-last-conflict-tree prod)))
           ;; assume saved tree is valid
           (setf (procedural-conflict-tree prod) (copy-conflict-tree (procedural-last-conflict-tree prod) nil)))
          (t ;; same as the first case
           (build-conflict-tree prod)
           (setf (procedural-last-conflict-tree prod) (copy-conflict-tree (procedural-conflict-tree prod) nil))))))



;; The conditions in a production are all converted into a conflict resolution condition
;; structure which specifies a single test/action to perform and includes indices for
;; the buffer and slot needed so the values can be pulled out of a cache instead of
;; having to get them from the buffer or chunk for each test.
;;
;; Then the process of conflict resolution is just a matter of iterating through
;; those tests and actions until either they're all true or one fails.


(defstruct cr-condition
  type buffer bi slot si value test result)

(defun cr-buffer-read (prod buffer index)
  (let ((val (aref (procedural-buffer-lookup prod) index)))
    (if (eq val :untested)
        (let* ((chunk (buffer-read buffer))
               (slots (when chunk (chunk-slots-vector chunk))))
          (setf (aref (procedural-buffer-lookup prod) index) (cons chunk slots))
          chunk)
      (car val))))

(defun cr-buffer-type-read (prod buffer index)
  (let ((val (aref (procedural-buffer-lookup prod) index)))
    (if (eq val :untested)
        (let* ((chunk (buffer-read buffer))
               (slots (when chunk (chunk-slots-vector chunk))))
          (setf (aref (procedural-buffer-lookup prod) index) (cons chunk slots))
          slots)
      (cdr val))))

(defun cr-buffer-slot-read (prod buffer bi si slot)
  (let ((val (aref (procedural-slot-lookup prod) bi si)))
    (if (eq val :untested)
        (setf (aref (procedural-slot-lookup prod) bi si) (fast-chunk-slot-value-fct (cr-buffer-read prod buffer bi) slot))
      val)))


(defun slot-value-from-index (chunk index)
  (if (chunk-p-fct chunk)
      (let ((slot-name (slot-index->name index)))
        (if slot-name
            (values (fast-chunk-slot-value-fct chunk slot-name) t)
          (values nil t)))
    (values nil nil)))

;;; Function to test if two conditions are equivalent 
;;; only valid for constant tests i.e. those that could
;;; be in a conflict-resolution tree.
;;;
;;; Changed the value tests to eql instead of eq because
;;; the value could be a number.

(defun cr-condition-equal (a b)
  (and (eq (cr-condition-type a) (cr-condition-type b))
       (eq (cr-condition-buffer a) (cr-condition-buffer b))
       (case (cr-condition-type a)
         (isa (equalp (cr-condition-value a) (cr-condition-value b)))
         (slot (= (cr-condition-si a) (cr-condition-si b)))
         (query (and (eq (cr-condition-slot a) (cr-condition-slot b))
                     (eql (cr-condition-value a) (cr-condition-value b))))
         (test-slot (and (eq (cr-condition-test a) (cr-condition-test b))
                         (= (cr-condition-si a) (cr-condition-si b))
                         (eql (cr-condition-value a) (cr-condition-value b))))
         (t nil))))

(defun test-constant-condition (prod test &optional production)
  (declare (ignore production))
  (case (cr-condition-type test)
    (isa 
     (let ((slots (cr-buffer-type-read prod (cr-condition-buffer test) (cr-condition-bi test))))
       (and slots
            (slots-vector-match-signature slots (car (cr-condition-value test)) (cdr (cr-condition-value test))))))
    (slot 
     (let ((real (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) (cr-condition-si test) (cr-condition-slot test))))
       (if (chunk-slot-equal (cr-condition-value test) real)
             t
           (when (procedural-ppm prod)
             ;;; try a partial match and save the result if it's valid
             (let ((sim (similarity-fct (cr-condition-value test) real)))
               (when (and (numberp sim) (> sim (procedural-md prod)))
                 (push (list (cr-condition-buffer test) (cr-condition-slot test) (cr-condition-value test) real sim)
                       (production-partial-matched-slots (procedural-current-p prod)))))))))
    (query 
     (eq (cr-condition-result test) (query-buffer (cr-condition-buffer test) (list (cr-condition-slot test) (cr-condition-value test)))))
    (test-slot 
     (let ((real (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) (cr-condition-si test) (cr-condition-slot test))))
       (eq (cr-condition-result test) 
           (funcall (cr-condition-test test) real (cr-condition-value test)))))))


(defun test-search-constants (prod test production)
  
  ;; don't use the cache table for slot values since they'd need to be continuously cleared.
  ;; Instead just use the accessors for the chunk from the table.
  
  (case (cr-condition-type test)
    (isa 
     (let ((slots (cr-buffer-type-read prod (cr-condition-buffer test) (cr-condition-bi test))))
       (and slots
            (slots-vector-match-signature slots (car (cr-condition-value test)) (cdr (cr-condition-value test))))))
    (slot                               ;; can't use cr-buffer-slot-read because the slots cache isn't updated for search buffers
     (multiple-value-bind (real exists) (slot-value-from-index (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) (cr-condition-si test))
       (when exists
         (if (chunk-slot-equal (cr-condition-value test) real)
             t
           (when (procedural-ppm prod)
             ;;; try a partial match and save the result if it's valid
             (let ((sim (similarity-fct (cr-condition-value test) real)))
               (when (and (numberp sim) (> sim (procedural-md prod)))
                 (push (list (cr-condition-buffer test) (cr-condition-slot test) (cr-condition-value test) real sim)
                       (production-partial-matched-slots (procedural-current-p prod))))))))))
    (test-slot                          ;; can't use cr-buffer-slot-read because the slots cache isn't updated for search buffers
     (multiple-value-bind (real exists) (slot-value-from-index (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) (cr-condition-si test))
       (when exists 
         (if (eq (cr-condition-result test) (funcall (cr-condition-test test) real (replace-variables (cr-condition-value test) (production-bindings production))))
             t
           (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
             ;;; try a partial match and save the result if it's valid
             (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                    (sim (similarity-fct desired real)))
               (when (and (numberp sim) (> sim (procedural-md prod)))
                 (push (list (cr-condition-buffer test) (cr-condition-slot test) desired real sim)
                       (production-partial-matched-slots (procedural-current-p prod))))))))))
    (test-var-slot
     (let* ((slot-name (replace-variables (cr-condition-slot test) (production-bindings production)))
            
            ;; just use the chunk-slot-value result so that nil is the value for a missing/bad slot
            (real (fast-chunk-slot-value-fct (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) slot-name)))
       (if (eq (cr-condition-result test) (funcall (cr-condition-test test) real (replace-variables (cr-condition-value test) (production-bindings production))))
           t
         (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
           ;;; try a partial match and save the result if it's valid
           (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                  (sim (similarity-fct desired real)))
             (when (and (numberp sim) (> sim (procedural-md prod)))
               (push (list (cr-condition-buffer test) (replace-variables (cr-condition-slot test) (production-bindings production)) desired real sim)
                     (production-partial-matched-slots (procedural-current-p prod)))))))))))
  

(defun test-and-perform-bindings (procedural bind production)
  (case (cr-condition-type bind)
    (bind-slot
     (bind-variable (cr-condition-value bind) (cr-buffer-slot-read procedural (cr-condition-buffer bind) (cr-condition-bi bind) (cr-condition-si bind) (cr-condition-slot bind)) production))
    (bind-buffer
     (bind-variable (cr-condition-value bind) (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) production))
    (bind-var-slot
     (let* ((slot (replace-variables (cr-condition-slot bind) (production-bindings production)))
            (index (valid-slot-name slot)))
       (when (numberp index)
         (let ((real (cr-buffer-slot-read procedural (cr-condition-buffer bind) (cr-condition-bi bind) index slot)))
           (and real
                (bind-variable (cr-condition-value bind) real production))))))
    (bind
     (let ((result (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production)))))
       (if result
           (bind-variable (cr-condition-value bind) result production)
         nil)))
    (mv-bind
     (let ((all-vars (cr-condition-value bind))
           (results (multiple-value-list (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production))))))
       (cond ((not (= (length results) (length all-vars)))
              nil)
             ((member nil results)
              nil)
             (t
              (do ((vars all-vars (cdr vars))
                   (vals results (cdr vals)))
                  ((null vars) t)
                (bind-variable (car vars) (car vals) production))))))))


(defun test-and-perform-bindings-search (procedural bind production)
  (case (cr-condition-type bind)
    (bind-slot    ;; Can't use the cached lookup for a search buffer since it may not be the "Right" chunk
                  ;; since the cache isn't overwritten for each new chunk
     (multiple-value-bind (real exists) (slot-value-from-index (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) (cr-condition-si bind))
       (when exists 
         (bind-variable (cr-condition-value bind) real production))))
    (bind-buffer
     (bind-variable (cr-condition-value bind) (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) production))
    (bind-var-slot
     (let* ((slot (replace-variables (cr-condition-slot bind) (production-bindings production)))
            (index (valid-slot-name slot)))
       (when (numberp index)
         ;; Can't use the cached lookup for a search buffer since it may not be the "Right" chunk
         ;; since the cache isn't overwritten for each new chunk
         (multiple-value-bind (real exists) (slot-value-from-index (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) index)
           (when exists 
             (bind-variable (cr-condition-value bind) real production))))))
    (bind
     (let ((result (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production)))))
       (if result
           (bind-variable (cr-condition-value bind) result production)
         nil)))
    (mv-bind
     (let ((all-vars (cr-condition-value bind))
           (results (multiple-value-list (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production))))))
       (cond ((not (= (length results) (length all-vars)))
              nil)
             ((member nil results)
              nil)
             (t
              (do ((vars all-vars (cdr vars))
                   (vals results (cdr vals)))
                  ((null vars) t)
                (bind-variable (car vars) (car vals) production))))))))

(defun test-other-condition (prod test production)
  (case (cr-condition-type test)
    (query 
     (eq (cr-condition-result test) (query-buffer (cr-condition-buffer test) (list (cr-condition-slot test) (replace-variables (cr-condition-value test) (production-bindings production))))))
    (test-slot 
     (let ((real (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) (cr-condition-si test) (cr-condition-slot test))))
       (if (eq (cr-condition-result test) (funcall (cr-condition-test test) real (replace-variables (cr-condition-value test) (production-bindings production))))
           t
         (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
           ;;; try a partial match and save the result if it's valid
           (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                    (sim (similarity-fct desired real)))
             (when (and (numberp sim) (> sim (procedural-md prod)))
               (push (list (cr-condition-buffer test) (cr-condition-slot test) desired real sim)
                     (production-partial-matched-slots (procedural-current-p prod)))))))))
    (eval
     (eval (replace-variables-for-eval (cr-condition-value test) (production-bindings production))))
    (test-var-slot
     (let* ((slot (replace-variables (cr-condition-slot test) (production-bindings production)))
            (index (valid-slot-name slot))
            (real (and index (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) index slot))))
       
       ;; just use the value of real to perform the test
       
       (if (eq (cr-condition-result test) (funcall (cr-condition-test test) real (replace-variables (cr-condition-value test) (production-bindings production))))
           t
         (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
           ;;; try a partial match and save the result if it's valid
           (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                  (sim (similarity-fct desired real)))
             (when (and (numberp sim) (> sim (procedural-md prod)))
               (push (list (cr-condition-buffer test) (replace-variables (cr-condition-slot test) (production-bindings production)) desired real sim)
                     (production-partial-matched-slots (procedural-current-p prod)))))))))))


(defun test-search-buffers (prod test production)
  (let* ((buffer (cr-condition-buffer test))
         (chunk-list (multiple-value-bind (val exists)
                         (gethash buffer (procedural-search-buffer-table prod)) 
                       (if exists
                           val
                         (multiple-value-bind (e new-val)
                             (m-buffer-search buffer)
                           (setf (gethash buffer (procedural-search-buffer-table prod)) 
                             (if e 
                                 (let ((valid (get-m-buffer-chunks buffer)))
                                   (remove-if-not (lambda (x) (member x valid)) new-val))
                               (get-m-buffer-chunks buffer))))))))
    (dolist (c chunk-list)
      
      
      ;; Don't actually put a chunk into the buffer
      ;; instead just set the table entry for it to 
      ;; avoid "garbage" chunks in the case of a copy buffer.
      ;; This works because the test-search-constants function
      ;; only reads the chunk name from the table and gets the
      ;; slot values directly from the chunk.
      
     
      (setf (aref (procedural-buffer-lookup prod) (cr-condition-bi test)) (cons c (chunk-slots-vector c)))
      
      
      (let ((crt (procedural-crt prod)))
        (setf (procedural-crt prod) nil)
        (let ((result (conflict-tests prod (cr-condition-value test) production 'test-search-constants)))
          (setf (procedural-crt prod) crt)
          (when result
            (push (cons buffer c) (procedural-temp-search prod))
            (return-from test-search-buffers t)))))
    nil))
                          

(defun failure-reason-string (condition production)
  
  (if (production-disabled production)
      "The production is disabled."
    (case (cr-condition-type condition)
      (isa (aif (buffer-read (cr-condition-buffer condition))
                (let ((slots (chunk-slots-vector it)))
                  (multiple-value-bind (match extra unfilled filled)
                      (compare-slots-vector-to-signature slots (car (cr-condition-value condition)) (cdr (cr-condition-value condition)))
                    (cond (match
                           (format nil "This should not happen -- the isa test failed but reports as a match now."))
                          (unfilled 
                           (format nil "The chunk in the ~S buffer does not have slot~p ~{~s~^, ~}." 
                             (cr-condition-buffer condition) (length unfilled) unfilled))
                          (filled
                           (format nil "The chunk in the ~S buffer has the slot~p ~{~s~^, ~}." 
                             (cr-condition-buffer condition) (length filled) filled))
                          (t
                           (format nil "This should not happen -- the isa test failed but there are neither unfilled or filled slots but extra is ~s" extra)))))
                (format nil "The ~s buffer is empty." (cr-condition-buffer condition))))
      (search (format nil "The searched multi-buffer ~s did not have a matching chunk." (cr-condition-buffer condition)))
      (slot (format nil "The ~s slot of the chunk in the ~s buffer does not have the value ~s." 
              (cr-condition-slot condition) (cr-condition-buffer condition) (cr-condition-value condition)))
      (query (format nil "The ~s ~s query of the ~s buffer failed." 
               (cr-condition-slot condition) (cr-condition-value condition) (cr-condition-buffer condition)))
      (test-slot 
       (format nil "The value in the ~s slot of the chunk in the ~s buffer does not satisfy the constraints." 
         (cr-condition-slot condition) (cr-condition-buffer condition)))
      (eval (format nil "The evaluation of the expression ~s returned nil." (cr-condition-value condition)))
      (test-var-slot (let ((slot-name (replace-variables (cr-condition-slot condition) (production-bindings production))))
                       (format nil "The value in the ~s slot (the value of the ~s variable) of the chunk of the ~s buffer does not satisfy the constraints."
                         slot-name (cr-condition-slot condition) (cr-condition-buffer condition))))
      (bind (format nil "The evaluation of the expression ~s returned nil." (cr-condition-result condition)))
      (mv-bind (format nil "The evaluation of the expression ~s either returned a nil value or too few values to bind to all of the variables." (cr-condition-result condition)))
      
      (bind-buffer (format nil "The ~s buffer is empty." (cr-condition-buffer condition))) ;; This shouldn't happen anymore since the isa will fail first
      (bind-slot (format nil "This should not happen -- a bind-slot has failed.  Please report this to Dan."))
      (bind-var-slot (let ((index (valid-slot-name (replace-variables (cr-condition-slot condition) (production-bindings production)))))
                       (if (numberp index)
                           (format nil "The chunk in the ~s buffer does not have a slot named ~s (the value of the ~s variable)." 
                                 (cr-condition-buffer condition) (replace-variables (cr-condition-slot condition) (production-bindings production)) (cr-condition-slot condition))
                         (format nil "The value of the ~s variable does not name a valid slot in the chunk in the ~s buffer." (cr-condition-slot condition) (cr-condition-buffer condition))))))))


(defun bind-variable (var value production)
  (aif (assoc var (production-bindings production))
       (setf (cdr it) value)
       (progn
         (push (cons var value) (production-bindings production))
         value)))


(defun conflict-tests (procedural test-list production tester &key (report t))
  (if (null test-list) 
      t
    (let ((outcome (do* ((tests test-list (cdr tests))
                         (result (funcall tester procedural (car tests) production)
                                 (funcall tester procedural (car tests) production)))
                        
                        ((or (null result) (null (cdr tests))) (cons result (car tests))))))
      
      (if (null (car outcome))
          (progn
            (setf (production-failure-condition production) (cdr outcome))
            (when (and (procedural-crt procedural) report); report failures..
              (model-output "Fails because: ")
              (model-output (failure-reason-string (cdr outcome) production)))
            nil)
        t))))
        

(defun conflict-resolution (procedural)
  
  (setf (procedural-delayed-resolution procedural) nil)
  
  (setf (procedural-buffer-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural)) :initial-element :untested))
  (setf (procedural-slot-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural) (largest-chunk-type-size)) :initial-element :untested))
  
  (let* ((conflict-set nil)
         (hook-set nil)
         (best nil)
         (best-ut (minimum-utility))
         (mu best-ut)
         (offsets-table (make-hash-table))
         (test-set (if (or (null (procedural-use-tree procedural))
                           (procedural-crt procedural)
                           (procedural-ppm procedural))
                       (procedural-productions procedural)
                     (mapcar (lambda (x) (get-production-internal x procedural)) (get-valid-productions procedural))))
         (saved-search-chunks (make-hash-table)))
    
    (setf (procedural-last-cr-time procedural) (mp-time-ms))
    (clrhash (procedural-search-matches-table procedural))
    
    (dolist (b (procedural-used-search-buffers procedural))
      (aif (buffer-read b)
           (setf (gethash b saved-search-chunks) it)
           (setf (gethash b saved-search-chunks) :clear)))
    
    (clrhash (procedural-search-buffer-table procedural))
    
    (dolist (production test-set)
      
      (setf (production-bindings production) nil)
      (setf (production-failure-condition production) nil)
      
      (setf (procedural-current-p procedural) production)
      (setf (production-partial-matched-slots production) nil)
      
      
      (setf (procedural-temp-search procedural) nil)
      
      
      
      (unless (production-disabled production)
        (when (procedural-crt procedural)
          (model-output "Trying production: ~s" (production-name production)))
        
        (when (and (conflict-tests procedural (production-constants production) production 'test-constant-condition)
                   (conflict-tests procedural (production-binds production) production 'test-and-perform-bindings)
                   (conflict-tests procedural (production-others production) production 'test-other-condition)
                   (conflict-tests procedural (production-searches production) production 'test-search-buffers)
                   (conflict-tests procedural (production-search-binds production) production 'test-and-perform-bindings-search)
                   (conflict-tests procedural (production-search-others production) production 'test-other-condition)
                   ) 
          
          (dolist (s (procedural-temp-search procedural))
            (pushnew (cdr s)
                     (gethash (car s) (procedural-search-matches-table procedural))))
          
          (push-last production conflict-set))))
    
    
    ;; get and save any potential chunk offsets 
         
    (maphash (lambda (buffer chunks)
               (multiple-value-bind (exists offsets)
                   (m-buffer-offset buffer chunks)
                 (when (and exists offsets (every 'numberp offsets))
                   (setf (gethash buffer offsets-table)
                     (mapcar 'cons chunks offsets)))))
             (procedural-search-matches-table procedural))
         
    
    (dolist (production conflict-set)
      
      (let ((u (compute-utility (production-name production) t)))
        
        (when (procedural-crt procedural)
          (model-output "Production ~s matches" (production-name production)))
        
        
        (dolist (s (production-searches production))
          (let* ((buf (cr-condition-buffer s))
                 (buf-var (cr-condition-test s))
                 (bound-chunk (cdr (assoc buf-var (production-bindings production))))
                 (offset (cdr (assoc bound-chunk (gethash buf offsets-table)))))
            
            (when (numberp offset)
              (incf u offset))))        
        
        
        (setf (production-conflict-val production) u)
        
        (when (and (procedural-crt procedural) mu (< u mu))
          (model-output "Fails because:~%Utility ~s is below the threshold ~s" u mu))
        
        (cond ((or (null best-ut) (> u best-ut))
               (setf best-ut u)
               (setf best (list production)))
              ((= u best-ut)
               (if (procedural-er procedural)
                   (push-last production best)
                 (setf best (list production)))))))
         
    
    (when (and (listp best) best (procedural-er procedural))
      (setf best (permute-list best)))
    
    (when (procedural-conflict-set-hook procedural)
      (let ((val nil)
            (old-val nil)
            
            (cs-names (mapcar 'production-name
                        (sort (copy-list conflict-set)
                              (lambda (x y) 
                                (sort-productions x y best))))))
        
        (dolist (hook (procedural-conflict-set-hook procedural))
          (when val
            (setf old-val val))
          (setf val (funcall hook cs-names))
          (unless (or (null val) (stringp val) (member val cs-names))
            (print-warning "Only productions in the conflict set, a string, or nil are valid return values of a conflict-set-hook function.")
            (setf val nil))
          (when (and val old-val)
            (print-warning "Multiple functions on the conflict-set-hook returned a value")))
        (setf hook-set (or val old-val))))
    
    (when (and (procedural-cst procedural) (procedural-v procedural))
      (dolist (x conflict-set)
        (print-instantiation x)
        (spp-fct (list (production-name x) :utility :u))
        (unless (= (production-conflict-val x) (no-output (caar (spp-fct (list (production-name x) :utility)))))
          (command-output "{buffer search adjusted u value is ~,3f}" (production-conflict-val x)))))
    
    ;; restore the chunks to the search buffers then any "new" search results will overwrite
    ;; those in the conflict-code
    
    (dolist (b (procedural-used-search-buffers procedural))
      (let ((val (gethash b saved-search-chunks)))
        (when val
          (if (eq val :clear)
              (erase-buffer b)
            (overwrite-buffer-chunk b val)))))
    
    (cond ((null hook-set) ;; default mechanims are used
           (let ((best-production (car best))) ; not (car conflict-set) because that's only sorted for the hook
             (if best-production 
                 (progn
                   (schedule-event-now 'production-selected 
                                       :module 'procedural
                                       :destination 'procedural
                                       :priority :max
                                       :params (list best-production)
                                       :details (concatenate 'string (symbol-name 'production-selected) " " (symbol-name (production-name best-production))))
                   
                   (awhen (production-conflict-code best-production)
                          (dolist (code it)
                            (funcall code)))
                   
                   (when (production-break best-production)
                     (schedule-event-now 'print-instantiation
                                         :module 'procedural
                                         :output nil
                                         :priority :max
                                         :params (list best-production))
                     
                     (schedule-break-relative 0 :priority :max 
                                              :time-in-ms t
                                              :details (concatenate 'string (symbol-name 'production) " " (symbol-name (production-name best-production))))))
               
               (setf (procedural-delayed-resolution procedural) 
                 (schedule-event-after-change 'conflict-resolution
                                              :module 'procedural
                                              :destination 'procedural
                                              :output 'medium
                                              :dynamic t)))))
          
          ((symbolp hook-set) ;; an over-ride production specified
           
           (schedule-event-now 'production-selected :module 'procedural
                               :destination 'procedural :priority :max
                               :params (list (get-production-internal hook-set procedural))
                               :details (concatenate 'string (symbol-name 'production-selected) " " (symbol-name hook-set)))
           
           (awhen (production-conflict-code (get-production-internal hook-set procedural))
                  (dolist (code it)
                    (funcall code)))
           
           (when (production-break (get-production-internal hook-set procedural))
             
             (schedule-event-now 'print-instantiation
                                 :module 'procedural
                                 :output nil
                                 :priority :max
                                 :params (list (get-production-internal hook-set procedural)))
             
             (schedule-break-relative 0 :priority :max 
                                      :time-in-ms t
                                      :details (concatenate 'string (symbol-name 'production) " " (symbol-name hook-set)))))
          
          ((stringp hook-set) ;; an abort selection reason provided
           (model-warning "conflict-set-hook function canceled selection because : ~a" hook-set)
           (schedule-event-relative (procedural-dat procedural) 'conflict-resolution
                                    :time-in-ms t
                                    :module 'procedural
                                    :destination 'procedural
                                    :output 'medium))
          (t ;; shouldn't happen but this is a saftey case
           (print-warning "Illegal conflict resolution situation occured. Contact Dan to let him know.")))))



(defun ppm-offset (production) ;; it's the name called through the utility-offsets
  (let ((prod (get-module procedural))
        (p (get-production production)))
    (when (and prod p (numberp (procedural-ppm prod)) (production-partial-matched-slots p))
      (let ((override (awhen (procedural-ppm-hook prod)
                             (funcall it production (production-partial-matched-slots p)))))
        (if (numberp override)
            override
          (* (procedural-ppm prod) (reduce '+ (production-partial-matched-slots p) :key 'fifth)))))))


(defun un-delay-conflict-resolution ()
  (let ((p (get-module procedural)))
    (when (and p (procedural-delayed-resolution p))
      (let ((deleted (delete-event (procedural-delayed-resolution p))))
        (setf (procedural-delayed-resolution p) nil)
        (when deleted
          (schedule-event-now 'conflict-resolution
                              :module 'procedural :destination 'procedural :output 'medium)))))
  nil)


(defmacro pprint-instantiation (production-name)
  `(pprint-instantiation-fct ',production-name))

(defun pprint-instantiation-fct (production-name)
  (let ((p (get-production production-name)))
    (if p
        (print-instantiation p)
      (model-warning "No production named ~a." p))))
                                 
                                 
(defun print-instantiation (production)
  (print-production-text production t))


(defun print-production (production &optional model-output)
  (when (production-disabled production)
    (if model-output
        (model-output ";;; Production ~s is DISABLED" (production-name production))
      (command-output ";;; Production ~s is DISABLED" (production-name production))))
  (print-production-text production nil model-output))


(defun print-production-text (p instantiate &optional model-output)
  
  (let* ((s (make-string-output-stream))
         (bindings (if instantiate (production-bindings p) nil)))
    
    (format s "(P ~a~%" (production-name p))
    (awhen (production-documentation p)
      (format s "  ~S~%" it))

    (dolist (x (production-lhs p))
      (write-string (production-statement-text x bindings (when instantiate (production-partial-matched-slots p))) s))
    
    (format s " ==>~%")
    
    (dolist (x (production-rhs p))
      (write-string (production-statement-text x bindings nil) s))
    
    (format s ")")
    
    (if model-output
        (model-output "~a" (get-output-stream-string s))
      (command-output "~a" (get-output-stream-string s)))))
    
      
    
(defun production-statement-text (statement bindings partials)
  (let ((s (make-string-output-stream))
        (op (production-statement-op statement))
        (target (production-statement-target statement))
        (spec (production-statement-spec statement))
        (definition (production-statement-definition statement)))
    
    (case op
      (#\-
       (format s "   -~a>~%" target))
      (#\?
       (format s "   ?~a>~%" target)
       (dolist (slot (act-r-chunk-spec-slots (instantiate-query-spec spec bindings)))
         (if (eq '= (act-r-slot-spec-modifier slot))
             (format s "       ~s ~s~%" (act-r-slot-spec-name slot) (act-r-slot-spec-value slot))
           (format s "    ~2a ~s ~s~%" (act-r-slot-spec-modifier slot) (act-r-slot-spec-name slot) (act-r-slot-spec-value slot)))))
      ((#\= #\*)
       (if spec
           (progn
             (format s "   ~c~a>~%" op target)
             (dolist (slot (replace-variables (chunk-spec-slot-spec spec) bindings)) 
               (let* ((name (spec-slot-name slot))
                      (val (spec-slot-value slot))
                      (partial (find (list target name val) partials :test 'equal :key (lambda (x) (subseq x 0 3)))))
               
               (if partial
                   (if (eq '= (spec-slot-op slot))
                       (format s "       ~s [~s, ~s, ~f]~%" name val (fourth partial) (fifth partial))
                     (format s "    ~2a ~s [~s, ~s, ~f]~%" (spec-slot-op slot) name val (fourth partial) (fifth partial)))                 
                 
                 (if (eq '= (spec-slot-op slot))
                     (format s "       ~s ~s~%" name val)
                   (format s "    ~2a ~s ~s~%" (spec-slot-op slot) name val))))))
         ;; there's only one thing in the definition but leave it flexible...
         (format s "   ~c~a> ~{~s~^ ~}~%" op target (replace-variables definition bindings))))
      (#\@
       (format s "   @~a> ~s~%" target (replace-variables (car definition) bindings)))
      (#\!
       (format s "   !~a! ~{~s~^ ~}~%" target (replace-variables definition bindings)))
      (#\+
       (if (= (length definition) 1)
           (format s "   +~a> ~s~%" target (first (replace-variables definition bindings)))
         (progn
           (format s "   +~a>~%" target)
           (dolist (slot (replace-variables (chunk-spec-slot-spec spec) bindings)) 
               (if (eq '= (spec-slot-op slot))
                   (format s "       ~s ~s~%" (spec-slot-name slot) (spec-slot-value slot))
                 (format s "    ~2a ~s ~s~%" (spec-slot-op slot) (spec-slot-name slot) (spec-slot-value slot))))))))
    
    (get-output-stream-string s)))
           

(defun sort-productions (p1 p2 best)
  (let ((p1-u (production-conflict-val p1))
        (p2-u (production-conflict-val p2)))
  (cond ((= p1-u p2-u)
         (< (or (position p1 best) (1+ (length best)))
            (or (position p2 best) (1+ (length best)))))
        (t (> p1-u p2-u)))))
    
    
(defun production-selected (procedural production)
  (when (procedural-lhst procedural)
    (dolist (x (production-selection-code production))
      (case (car x)
        (query-buffer 
         (schedule-query-buffer (second x) (instantiate-query-spec (third x) (production-bindings production)) 0 :time-in-ms t :module 'procedural))
        (buffer-read 
         (schedule-buffer-read (second x) 0 :time-in-ms t :module 'procedural))
        (buffer-search
         (schedule-event-now 'buffer-search :module 'procedural :params (cdr x))))))
  
  
  
  (setf (procedural-busy procedural) t)
  
  (setf (production-requested-actions (production-name production))
    (multiple-value-bind (event tag)
        (schedule-module-request 'production (procedural-req-spec procedural) 0 :time-in-ms t :module 'procedural :output nil :details (symbol-name (production-name production)) :priority :max :track t)
      (declare (ignore event))
               (list tag)))
  
  (note-production-selection (production-name production))
  
  (schedule-event-relative 
   (if (procedural-random-times procedural)
       (randomize-time-ms (productions-action-time (production-name production)))
     (productions-action-time (production-name production)))
   'production-fired :time-in-ms t :module 'procedural :destination 'procedural :params (list production)
   :details (concatenate 'string (symbol-name 'production-fired) " " (symbol-name (production-name production)))
   :output 'low))
      
      
(defun buffer-search (buffer-name)
  "dummy function to show in the trace"
  (declare (ignore buffer-name)))
      
(defun production-fired (procedural production)
  
  ;; The production itself will always be the first item and at this point
  ;; only item on the requested-actions param list
  (complete-request (first (production-requested-actions (production-name production))))
                    
  (dolist (x (production-actions production))
    (funcall x))
  
  (learn-parameters (production-name production))

  (schedule-event-now 'conflict-resolution :module 'procedural :priority :min :destination 'procedural :output 'medium)
  
  (dolist (hook (procedural-cycle-hook procedural))
    (funcall hook (production-name production)))
  
  ;; Call this explicitly now...
  (compile-productions production)
  
  (setf (procedural-busy procedural) nil))


(defun procedural-query (instance buffer-name slot value)
  (declare (ignore slot)) ; the only slot is state
  (case value
    (busy (procedural-busy instance))
    (free (not (procedural-busy instance)))
    (error nil)
    (t 
     (print-warning "Unknown state query ~S to ~S buffer" value buffer-name))))

(defun procedural-request (instance buffer-name chunk-spec)
  (declare (ignore instance buffer-name chunk-spec)))

(defun procedural-run-check (instance)
  (declare (ignore instance))
  
  ;; if there aren't any procedural events put a new
  ;; conflict-resolution out there...
  (unless (mp-modules-events 'procedural)
    (schedule-event-after-change 'conflict-resolution :module 'procedural :destination 'procedural :output 'medium :dynamic t)))
                                

(define-module-fct 'procedural (list (define-buffer production :trackable t))
  (list (define-parameter :er :owner nil)
        
        (define-parameter :v :owner nil)    
        (define-parameter :md :owner nil)
        (define-parameter :ppm :valid-test 'numornil :default-value nil
          :warning "a number or nil" :documentation "Procedural partial matching")
        
        (define-parameter :dat :valid-test 'numberp :default-value .05
          :warning "a number" :documentation "Default Action Time")
        
        (define-parameter :crt :valid-test 'tornil :default-value nil
          :warning "T or nil" :documentation "Conflict Resolution Trace")
        (define-parameter :cst :valid-test 'tornil :default-value nil
          :warning "T or nil" :documentation "Conflict Set Trace")
        
        (define-parameter :lhst :valid-test 'tornil :default-value t
          :warning "T or nil" 
          :documentation "Left Hand Side Trace")
        (define-parameter :rhst :valid-test 'tornil :default-value t
          :warning "T or nil" 
          :documentation "Right Hand Side Trace")
        
        (define-parameter :ppm-hook :valid-test 'fctornil :default-value nil
          :warning "a function or nil" :documentation "Procedural partial matching utility adjustment hook")
        
        (define-parameter :cycle-hook :valid-test 'fctornil 
          :default-value nil :warning "a function or nil" :documentation "Cycle hook")
            
        (define-parameter :vpft :valid-test 'tornil :default-value nil
          :warning "T or nil" :documentation "Variable Production Firing Time")

        (define-parameter :conflict-set-hook :valid-test 'fctornil 
          :default-value nil :warning "a function or nil" :documentation "Conflict set hook")
        
        ;;; There is another list parameter but not a hook
        ;;; it's the list of buffers not to use for strict harvesting
        
        (define-parameter :do-not-harvest :valid-test 'symbolp :default-value nil 
          :warning "a symbol" :documentation "Buffers that are not strict harvested")
        
        (define-parameter :use-tree :valid-test 'tornil :default-value nil
          :warning "T or nil" :documentation "Use a decision tree in production matching")
        
        (define-parameter :style-warnings :valid-test 'tornil :default-value t
          :warning "T or nil" :documentation "Show model warnings for production issues that don't prevent production definition"))
  
  :version "4.0" 
  :documentation 
  "The procedural module handles production definition and execution"
    
  :creation (lambda (x) (declare (ignore x)) (make-procedural))
  :query #'procedural-query
  :request #'procedural-request
  :reset '(reset-procedural-module nil finalize-procedural-reset)
  :params #'procedural-params
  :run-start #'procedural-run-check)


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
