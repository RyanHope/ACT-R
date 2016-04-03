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
;;; Filename    : chunks.lisp
;;; Version     : 2.0
;;; 
;;; Description : Definition of chunks and the function that manipulate them.
;;; 
;;; Bugs        : 
;;;
;;; To do       : * Finish the documentation.
;;;             : * This one is a big target for benchmarking and optimizing.
;;;             : * Should merge-chunks impact chunk-copied-from?
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation
;;; 2005.01.16 Dan
;;;             : * Added chunk-copied-from.
;;;             : * Reduced most things to 80 columns (I don't want to split
;;;             :   the format string because I've had problems with the ~
;;;             :   new-line breaking with "non-native" line endings).
;;;             : * Added doc strings.
;;;             : * Modified pprint-a-chunk so it can print with or without
;;;             :   the parameters.
;;;             : * Removed the print-chunk-type function since I don't want to
;;;             :   hide the structure since users shouldn't see them anyway.
;;; 2005.01.17 Dan
;;;             : * Switched to using command-output for printing.
;;;             : * Renamed pprint-chunk pprint-chunkS and took away its
;;;             :   printing of chunk parameters and added pprint-chunks-plus 
;;;             :   to display chunks with chunk parameters. 
;;; 2005.01.21 Dan
;;;             : * Updated merge-chunks-fct to work more efficiently.
;;; 2005.01.24 Dan
;;;             : * Fixed some bugs I introduced with the changes to pprint-
;;;             :   chunks and pprint-chunks-plus - I changed their return 
;;;             :   value which broke other things...
;;; 2005.02.04 Dan
;;;             : * Added the fast-* chunk accessors to eliminate the 
;;;             :   excessive calling of valid-slot-name.
;;; 2005.02.09 Dan
;;;             : * Fixed a bug that the fast-* stuff introduced with respect
;;;             :   to printing chunks.
;;; 2005.02.11 Dan
;;;             : * Some general clean up in define-chunks-fct.
;;; 2005.03.24 Dan
;;;             : * Changed the pprint-a-chunk function because it turns out
;;;             :   that some Lisps don't like using the pre-formatted format
;;;             :   string with the ~? directive.
;;; 2005.03.25 Dan
;;;             : * Changed pprint-a-chunk so that the slots print in the
;;;             :   same order as the chunk-type.
;;; 2005.04.01 Dan
;;;             : * Added true-chunk-name to help with an issue in merging
;;;             :   and may want to use it in printing and elsewhere...
;;; 2005.04.07 Dan
;;;             : * Fixed a minor issue with define-chunks and how it creates
;;;             :   the name for a chunk without one specified.
;;; 2005.05.07 Dan
;;;             : * Changed copy-chunk-fct so that instead of naming the new
;;;             :   chunk based on the chunk-type it bases it on the actual
;;;             :   name of the chunk being copied.  I think this is easier
;;;             :   to follow in the traces, but maybe it's more confusing.
;;;             :   We'll find out from experience I guess and then determine
;;;             :   which is better...
;;; 2005.06.11 Dan
;;;             : * DOH!  I remember again why I used the chunktype for the
;;;             :   name of the copy - because vision for example uses things
;;;             :   like loc1 which now when copied ends up as loc10 which
;;;             :   of course looks like "loc"+"10" instead of "loc1"+"0".
;;;             :   So, I've changed it so that it adds a - between the
;;;             :   chunk's name and the number so that would be loc1-0.             
;;; 2005.08.10 Dan
;;;             : * Minor clean-up in define-chunks to remove unused variables
;;;             :   in the let.
;;;             : * Updated version to 1.0.
;;; 2005.09.14 Dan
;;;             : * Fixed a bug in the output of a warning in define-chunks-fct
;;;             :   because invalid slot names weren't printed.
;;; 2005.11.17 Dan
;;;             : * Fixed some bugs in define-chunks-fct and pprint-a-chunk
;;;             :   related to default slot values in the chunk-type.
;;; 2006.01.03 Dan
;;;             : * Modified extend-chunks to remove the explicit compile call
;;;             :   (but still result in a compiled function at all times) to
;;;             :   hopefully get around the CMUCL issue.
;;; 2006.01.18 Dan
;;;             : * Modified the chunk printing function so that it can suppress
;;;             :   the "unfilled" extended slots of a chunk if desired.
;;; 2006.02.20 Dan
;;;             : * Fixed a bug in extend-chunks that causes problems with chunk 
;;;             :   parameters when merged when the ACT-R is both compiled and
;;;             :   loaded in the same session i.e. if one loads a previously
;;;             :   compiled version there's no problem so it shouldn't have
;;;             :   caused too many problems.
;;; 2006.07.06 Dan
;;;             : * Fixed a bug in define-chunks-fct.  When a chunk-type 
;;;             :   specified a default value for a slot which was a symbol (thus
;;;             :   interepreted as a chunk name) nothing ever created such a
;;;             :   chunk if it wasn't defined.  It doesn't make sense to do it
;;;             :   at the time of the chunk-type definition (because you may not
;;;             :   be able to create the chunk you want first) so it now happens
;;;             :   when such a slot value gets set (just like it does for any
;;;             :   non-chunk name symbols in the specified chunk slots).
;;; 2006.07.10 Dan
;;;             : * Added get-chunk-warn for use in several of the "user" functions
;;;             :   because they don't provide a warning if the chunk-name is
;;;             :   invalid, but since get-chunk is used for other purposes, 
;;;             :   I don't want to change it directly.
;;;             : * Added changed true-chunk-name to true-chunk-name-fct and
;;;             :   added a macro for true-chunk-name to make it user accessible.
;;; 2006.07.11 Dan
;;;             : * Made merge-chunks "safe" because previously it would merge
;;;             :   un-equal chunks as long as both items were really chunks.
;;;             :   Didn't cause problems since DM did the check first anyway,
;;;             :   but may be an issue if other modules were to use it.
;;; 2006.08.08 Dan
;;;             : * Put a test into define-chunks-fct so that it doesn't result
;;;             :   in errors for malformed add-dm/define-chunks calls, but just
;;;             :   prints a warning.
;;; 2006.10.10 Dan
;;;             : * Added the normalize-chunk-names command which goes through
;;;             :   all of the model's chunks and replaces any refrence to a
;;;             :   chunk name in a slot with the chunk's "true" name and then
;;;             :   optionally releases any non-true name i.e. the name that
;;;             :   was "merged away".  Generally, this probably won't see 
;;;             :   much use, but cleaning up the references may be useful at
;;;             :   times, and if a model creates so many names that the symbol
;;;             :   table becomes a memory limiter clearing those out maybe
;;;             :   necessary.
;;; 2006.10.17 Dan
;;;             : * Minor bug fix in normalize-chunk-names for the unintern 
;;;             :   clause.
;;; 2006.10.20 Dan
;;;             : * More clean-up added to normalize-chunk-names - should free
;;;             :   up more memory in the unintern case now.
;;; 2007.01.04 Dan
;;;             : * Minor tweak to chunk-copied-from-fct to make sure that the
;;;             :   "copied-from" chunk still exists - which may not be the case
;;;             :   for something like a goal or imaginal requests which delete 
;;;             :   the original.
;;; 2007.01.15 Dan
;;;             : * Bug from that last update fixed - use chunk-p-fct instead
;;;             :   of chunk-p...
;;; 2007.07.13 Dan
;;;             : * Performance enhancement for normalize-chunk-names - it
;;;             :   can skip checking the chunks for which the name change is
;;;             :   being done.  Duh!
;;; 2008.04.15 Dan
;;;             : * Performance improvement for delete-chunk-fct.  Assume that
;;;             :   the only way to get "eq" chunks in the table is through
;;;             :   merging so don't need to search the whole table to find
;;;             :   them for deletion - just use the merge-list from the chunk's
;;;             :   truename as the set of chunks to delete.
;;; 2008.04.16 Dan
;;;             : * Minor tweaks to normalize-chunk-names: if there aren't any
;;;             :   merged chunks it terminates early (unlikely situation) and
;;;             :   it now uses the fast- chunk component accessors.
;;; 2008.07.01 Dan
;;;             : * Added purge-chunk command to both delete and release the
;;;             :   name of the chunk.
;;; 2008.07.30 Dan
;;;             : * Changed an append to an nconc in merge-chunks-fct because
;;;             :   there's no need to copy the lists and performance wise it
;;;             :   makes a difference in the long run.
;;; 2008.07.31 Dan
;;;             : * Moved chunk-slot-equal from chunk-spec to here and removed
;;;             :   the equivalent equal-compare-slot-values function since
;;;             :   there don't need to be two such functions.  
;;;             : * Also improved chunk-slot-equal so that it doesn't need to
;;;             :   use eq-chunks-fct which may save 10% or more time wise for
;;;             :   models becuase it removes duplicate lookups.
;;;             : * Added the testing of val1 and val2 back into the chunk
;;;             :   case of chunk-slot-equal since a nil can short-circuit the
;;;             :   chunk lookup - slows down the chunk only cases but speeds
;;;             :   up the nil tests which is probably more common in the 
;;;             :   average model.
;;; 2008.10.08 Dan
;;;             : * Improvement to normalize-chunk-names so it doesn't have to
;;;             :   look up non-chunks.  
;;; 2008.10.20 Dan [1.1]
;;;             : * Made changes to add the option of having chunk merging 
;;;             :   work like the older ACT-R versions where it essentially 
;;;             :   normalizes as it goes. It can be enabled via the :dcnn 
;;;             :   parameter and is on by default.
;;; 2008.10.30 Dan
;;;             : * Tweaked chunk-slot-equal to make it a little more efficient.
;;; 2008.11.03 Dan [1.2]
;;;             : * Changed the internals of how chunk parameters get stored
;;;             :   from a hash-table to an array and added extra lists so that
;;;             :   copying and merging don't need to loop over all the parameters.
;;; 2008.11.04 Dan
;;;             : * Fixed a minor bug with how the parameter copy list gets
;;;             :   created because compiling and loading the chunks file would
;;;             :   lead to calling the copy functions twice.
;;; 2008.11.11 Dan
;;;             : * Fixed a bug in the chunk copy code.
;;; 2008.11.13 Dan
;;;             : * Modified chunk normalizing so that it calls the hook fn's
;;;             :   when it changes chunk slot values.
;;; 2008.12.10 Dan
;;;             : * Added the :copy-from-chunk-function keyword to extend
;;;             :   chunks because sometimes having access to the original 
;;;             :   chunk may be useful when copying a parameter.
;;; 2009.02.13 Dan
;;;             : * Modified chunk-copy-fct to better control for size since
;;;             :   the ANSI CL spec doesn't require this to be true:
;;;             :
;;;             :  (let* ((ht1 (make-hash-table))
;;;             :         (s1 (hash-table-size ht1))
;;;             :         (ht2 (make-hash-table :size s1))
;;;             :         (s2 (hash-table-size ht2)))
;;;             :    (= s1 s2))
;;;             :
;;;             :   which can result in runaway memory usage if a chunk gets
;;;             :   copied, then the copy gets copied, and so on, in a Lisp which 
;;;             :   "rounds up" the size (ACL and possibly others).
;;; 2009.02.13 Dan
;;;             : * Modified chunk-copy-fct to use the new option of "short
;;;             :   chunk copy names".  So, instead of A-0-0-0-0 one would have
;;;             :   A-3 instead.
;;; 2009.04.23 Dan
;;;             : * Fixed a bug introduced in chunk-slot-equal the last time it 
;;;             :   was updated which caused t to match any non-chunk value if
;;;             :   t was also not explicitly defined as a chunk.
;;; 2010.04.30 Dan
;;;             : * Updated delete-chunk-fct so that it doesn't print a double
;;;             :   warning for deleting a chunk which is still used and also
;;;             :   fixed an unnecessary ' in the warning.
;;; 2010.08.16 Dan
;;;             : * Fixed a bug in delete-chunk-fct that would throw an error
;;;             :   if a non-chunk were passed in.
;;; 2010.08.17 Dan
;;;             : * Changed the extend-chunk macro so that the accessor and setf
;;;             :   funtions for a parameter include the parameter in the warning
;;;             :   when there's a bad chunk name provided.
;;; 2011.04.27 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;; 2011.04.28 Dan
;;;             : * Added a mechanism for suppressing the warnings that get
;;;             :   printed when chunks are extended during compile and load.
;;; 2011.05.19 Dan
;;;             : * Added the create-chunk-alias command that allows one to
;;;             :   add new chunk names that refer to existing chunks.
;;; 2012.05.30 Dan
;;;             : * Fixed a bug in the create-chunk-alias macro.
;;; 2012.10.15 Dan
;;;             : * Changed chunk-back-links to use a hash-table instead of a
;;;             :   list of lists to improve performance.  Significant reduction
;;;             :   in time and memory usage when :ncnar is t found in test 
;;;             :   cases.
;;; 2013.03.13 Dan [1.3]
;;;             : * Changed chunk creation so that all slots which exist for
;;;             :   the chunk get set in the table, even if they are empty, 
;;;             :   because that's important for matching chunk-specs now since
;;;             :   a non-existent slot needs to be diferentiated from an empty
;;;             :   slot.
;;; 2013.04.05 Dan
;;;             : * Fixed a bug with creating chunks that have default values in
;;;             :   slots which are themselves chunks.
;;;             : * Re-fixed because I undid the previous change which is still
;;;             :   important.
;;; 2013.05.20 Dan 
;;;             : * Added the resolve-a-static-chunks-type function to handle
;;;             :   converting a chunk to the minimal type needed for holding
;;;             :   its contents.
;;; 2013.05.21 Dan
;;;             : * Fast-mod-chunk and define-chunk now resolves static chunks so
;;;             :   that they are always of the most appropriate type.
;;; 2013.05.22 Dan
;;;             : * Changed pprint-a-chunk so that static chunks show the root
;;;             :   type or current type based on the setting of the :show-static-
;;;             :   subtype-names parameter.
;;; 2013.05.23 Dan
;;;             : * Changed define-chunks to now allow one to specify any possible
;;;             :   slots when creating static chunks even if only the parent type
;;;             :   is provided, but won't allow creating as yet undefined type
;;;             :   combinations e.g. if the root type is x and you've extended
;;;             :   x with a slot1 and extended x with a slot2 but haven't extended
;;;             :   either of those (x+slot1 or x+slot2) with the other to create
;;;             :   x+slot1&slot2 a definition for a chunk like this:
;;;             :   (isa x slot1 "a" slot2 "b") will fail, but either of these: 
;;;             :   (isa x slot1 "a") (isa x slot2 "b") would be fine.
;;;             : * Fixed resolve-chunks-type so that it doesn't remove the
;;;             :   static slots of the root type from the chunks i.e. those are
;;;             :   allowed to have a value of nil and still exist.
;;; 2014.02.12 Dan
;;;             : * Changed pprint-a-chunk to use canonical-chunk-type-name 
;;;             :   instead of directly testing the type and static printing
;;;             :   state.
;;; 2014.02.17 Dan
;;;             : * Changed set-chk-slot-value so that it resolves a static
;;;             :   chunk's type after the change.
;;;             : * Set the creation-type slot when creating a chunk and check
;;;             :   that when resolving a chunk's type.  If it moves up the
;;;             :   hierarchy allow the slots of the creation type to still be
;;;             :   used in set and mod operations for verification of valid 
;;;             :   slots until it either has the same type as its creation
;;;             :   type or becomes a type which isn't a supertype of the 
;;;             :   creation type.  What this allows is the following sequence
;;;             :   of operations which are used by some of the device code to
;;;             :   create visual features 'incrementally':
;;;             :   (chunk-type foo)
;;;             :   (chunk-type (bar (:include foo)) slot)
;;;             :   (define-chunk (a isa bar)) ;; falls back to foo at this point
;;;             :   (set-chunk-slot-value a slot t) ;; want to set that slot later
;;; 2014.02.24 Dan [2.0]
;;;             : * Move to chunks not maintaining type information at runtime.
;;;             : * Instead it just keeps a bitvector of slots which have values
;;;             :   in them based on a global slot mapping.
;;;             : * Setting and modifying slot values will automatically extend
;;;             :   the chunk.  Automatic extension however will print a model
;;;             :   warning.
;;;             : * Chunks can be marked as immutable now and trying to change
;;;             :   such a chunk doesn't work and prints a warning.
;;;             : * Don't print an ISA when printing a chunk.
;;; 2014.02.25 Dan 
;;;             : * Switch slot-value-lists back to a list of cons from a hashtable.
;;;             : * Chunk-slot-value doesn't warn for missing slots, just returns nil.
;;; 2014.03.05 Dan
;;;             : * Go all out and make the isa optional in chunk definition. 
;;;             :   Print warnings for any slots that aren't already defined and
;;;             :   then extend things to compensate (basically the same as with
;;;             :   undefined chunks).
;;; 2014.03.13 Dan
;;;             : * Require chunk names to start with an alphanumeric.  That 
;;;             :   prevents a lot of potential weirdness in production compilation
;;;             :   because imagine what happens if a chunk named =goal or worse
;;;             :   =goal> were instantiated into a compiled production not to
;;;             :   mention things like just =val, +retrieval>, or ==>.
;;; 2014.03.18 Dan
;;;             : * Added the mod-chunk-with-spec-fct function.
;;; 2014.03.26 Dan
;;;             : * Added chunk-filled-slots-vector for accessing the info from
;;;             :   a chunk-name.
;;; 2014.03.27 Dan
;;;             : * Changed the warning for create-undefined-chunk to say no
;;;             :   slots instead of default type chunk.
;;; 2014.04.01 Dan
;;;             : * Already had chunk-slots-vector so no need for chunk-filled-slots-
;;;             :   vector...
;;; 2014.05.21 Dan
;;;             : * Allow the :merge-function for extend-chunks to take the value
;;;             :   :second instead of a function to just set the value based on
;;;             :   that of the second chunk instead of having to write a function
;;;             :   to do so for each parameter.
;;; 2014.06.17 Dan
;;;             : * Fixed define-chunks-fct to disambiguate what a definition 
;;;             :   like this means: (name "doc") since that could be a chunk 
;;;             :   name and documentation string or it could be an unnamed 
;;;             :   chunk with a slot named name with a value of "doc".  The
;;;             :   second interpretation is how it works now and the only way
;;;             :   to specify a doc string is to also name the chunk and specify
;;;             :   an isa.
;;;             :   Here are the valid options:
;;;             :     {slot value}+
;;;             :     <name> {slot value}+
;;;             :     isa <type> {slot value}+
;;;             :     <name> isa <type> {slot value}+
;;;             :     <name> <"doc"> isa <type> {slot value}+
;;; 2014.06.19 Dan
;;;             : * Fixed a bug that allowed a chunk to have more than one 
;;;             :   instance of a slot.
;;; 2014.06.20 Dan
;;;             : * Only create undefined chunks if the name is valid for a 
;;;             :   chunk i.e. starts with an alphanumeric.
;;;             : * Also test that when extending slots.
;;; 2014.06.24 Dan
;;;             : * Allow all chunk definitions to extend the slots, even if it's
;;;             :   got a type specified, but do a hard warning when there's a
;;;             :   type specified.
;;; 2014.10.15 Dan
;;;             : * When merging chunks need to make sure that if either is 
;;;             :   immutable then the resulting chunk is also.
;;; 2014.10.17 Dan
;;;             : * Fixed a bug with recording the documentation string for a
;;;             :   chunk that didn't have a name -- this makes it possible
;;;             :   to also specify things as: 
;;;             :     <"doc"> isa <type> {slot value}*
;;;             :   Note that for all those listed above the + should be a * as
;;;             :   well since not slot vlaue pairs are required.
;;; 2014.10.20 Dan
;;;             : * Modify mod-chunk-fct so that it returns nil for all of the
;;;             :   "bad" situations instead of letting set-chk-slot-value fail
;;;             :   and then returning the chunk-name.
;;; 2014.11.10 Dan
;;;             : * Tweak pprint-a-chunk to avoid the issue with ~ and the output
;;;             :   macros.
;;; 2014.11.11 Dan
;;;             : * Add a check to extend-chunks to make sure the parameter
;;;             :   name is a symbol and not a keyword.
;;;             : * Add tests to make sure the functions provided are actually
;;;             :   functions.
;;; 2014.11.13 Dan
;;;             : * Remove the checking of the extend-chunks functions because
;;;             :   of potential issues with compile time vs load time checking.
;;; 2014.12.17 Dan
;;;             : * Changed the declaim for some functions because they didn't
;;;             :   indicate multiple return values and some Lisps care
;;;             :   about that.
;;; 2015.03.13 Dan
;;;             : * Don't try to create a chunk for a keyword used in a slot
;;;             :   value.  Treat it like a number or string and leave it alone.
;;; 2015.04.22 Dan
;;;             : * Chunk-copied-from now has a second return value when the
;;;             :   parameter is a valid chunk name.  The second return value
;;;             :   is the act-r-chunk-copied-from value for the chunk which
;;;             :   could be the name of a chunk which is no longer equal to the
;;;             :   copy because it had been changed, a symbol which doesn't
;;;             :   name a chunk anymore because it has been deleted, or nil if
;;;             :   the chunk itself has been modified or wasn't a copy to begin
;;;             :   with.  If the second value is non-nil then that means that
;;;             :   the chunk was created as a copy and hasn't been itself
;;;             :   modified since then.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Globals and underlying chunk structures are not for general use.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(declaim (ftype (function (t) (values t t)) get-module-fct))
(declaim (ftype (function (&optional t) t) sgp-fct))
(declaim (ftype (function () t) current-model))
(declaim (ftype (function () t) use-short-copy-names))
(declaim (ftype (function (&optional t) (values t t t)) new-name-fct))
(declaim (ftype (function () t) update-chunks-on-the-fly))
(declaim (ftype (function (t) t) release-name-fct))
(declaim (ftype (function () t) notify-on-the-fly-hooks))


(defvar *chunk-parameters-count* 0)

(defvar *chunk-parameter-undefined* (gentemp "Unused-Param"))

(defvar *chunk-parameters-list* nil 
  "Internal list of parameters that have been added to chunks")

(defvar *chunk-parameters-copy-list* nil
  "Internal list of parameters that have a copy function")

(defvar *chunk-parameters-merge-list* nil
  "Internal list of parameters that have a merge function")


(defun chunk-parameter-default (param chunk-name)
  "Return a default value for a parameter in a chunk"
  (if (act-r-chunk-parameter-default-function param)
      (funcall (act-r-chunk-parameter-default-function param) chunk-name)
    (act-r-chunk-parameter-default-value param)))


(defconstant *pprint-chunk-string* (formatter "~S~:[ (~s)~;~*~]~%~@[~S~%~]~:{   ~s  ~s~%~}")
  "compiled format string for printing chunks")

(defconstant *pprint-chunk-parameters-string* (formatter "~@[  --chunk parameters--~%~:{   ~s  ~s~%~}~]~%")
  "compiled format string for printing chunk parameters")

(defun pprint-a-chunk (chunk-name &optional (w-params t))
  "Internal function for printing a chunk"
  (let ((chunk (get-chunk chunk-name)))
    (if chunk
        (progn
          (command-output "~a"
           (format nil
               *pprint-chunk-string*
             chunk-name
             (eql chunk-name (act-r-chunk-name chunk))
             (act-r-chunk-name chunk)
             (act-r-chunk-documentation chunk)
             (mapcar (lambda (slot) 
                       (list (car slot) (cdr slot)))
               (sort (copy-tree (act-r-chunk-slot-value-lists chunk)) #'< :key (lambda (x) (slot-name->index (car x)))))))
          (when w-params
            (command-output "~a"
             (format nil *pprint-chunk-parameters-string*
               (mapcar (lambda (param)
                           (list (act-r-chunk-parameter-name param)
                                 (funcall (act-r-chunk-parameter-accessor param) 
                                          chunk-name)))
                 *chunk-parameters-list*))))
          chunk-name)
      :error)))

(defmacro pprint-chunks (&rest chunk-names)
  "Print the chunks"
  `(pprint-chunks-fct ',chunk-names))

(defun pprint-chunks-fct (&optional chunk-names-list)
  "Print the chunks"
  (verify-current-mp  
   "pprint-chunks called with no current meta-process."
   (verify-current-model
    "pprint-chunks called with no current model."
    (let ((res nil))
      (dolist (chunk (if (null chunk-names-list) (chunks) chunk-names-list) res)
        (push-last (pprint-a-chunk chunk nil) res))))))


(defun chunk-back-links (chunk-name)
  (gethash chunk-name (act-r-model-chunk-ref-table (current-model-struct))))

(defun set-chunk-back-links (chunk-name val)
  (setf (gethash chunk-name (act-r-model-chunk-ref-table (current-model-struct))) val))

(defsetf chunk-back-links set-chunk-back-links)


(defmacro pprint-chunks-plus (&rest chunk-names)
  "Print the chunks and their chunk parameters"
  `(pprint-chunks-plus-fct ',chunk-names))

(defun pprint-chunks-plus-fct (&optional chunk-names-list)
  "Print the chunks and their parameters"
  (verify-current-mp  
   "pprint-chunks-plus called with no current meta-process."
   (verify-current-model
    "pprint-chunks-plus called with no current model."
    (let ((res nil))
      (dolist (chunk (if (null chunk-names-list) (chunks) chunk-names-list) res)
        (push-last (pprint-a-chunk chunk t) res))))))


(defun chunks ()
  "Returns a list of the names of all currently defined chunks"
  (verify-current-mp  
   "chunks called with no current meta-process."
   (verify-current-model
    "chunks called with no current model."
    (hash-table-keys (act-r-model-chunks-table (current-model-struct))))))

(defun get-chunk (name)
  "Internal function for getting the chunk structure from its name"
  (verify-current-mp  
   "get-chunk called with no current meta-process."
   (verify-current-model
    "get-chunk called with no current model."
    (gethash name (act-r-model-chunks-table (current-model-struct))))))


(defun get-chunk-warn (name)
  "Internal function for getting the chunk structure from its name"
  (verify-current-mp  
   "get-chunk called with no current meta-process."
   (verify-current-model
    "get-chunk called with no current model."
    (let ((c (gethash name (act-r-model-chunks-table (current-model-struct)))))
      (if c c
        (print-warning "~s does not name a chunk in the current model." name))))))

(defmacro chunk-p (chunk-name?)
  "Check a name to see if it names a chunk"
  `(chunk-p-fct ',chunk-name?))

(defun chunk-p-fct (chunk-name?)
  "Check a name to see if it names a chunk"
  (if (get-chunk chunk-name?) t nil))

(defun chunk-slots-vector (chunk-name)
  "Return the bitvector of slots with values for a chunk"
  (let ((c (get-chunk-warn chunk-name)))
    (when c (act-r-chunk-filled-slots c))))

(defmacro chunk-documentation (chunk-name)
  "Return the documentation string for a chunk"
  `(chunk-documentation-fct ',chunk-name))

(defun chunk-documentation-fct (chunk-name)
  "Return the documentation string for a chunk"
    (let ((c (get-chunk-warn chunk-name)))
    (when c
      (act-r-chunk-documentation c))))
  
(defun create-undefined-chunk (name)
  "Create a new chunk with the given name of chunk-type chunk with a warning"
  (model-warning "Creating chunk ~S with no slots" name)
  (define-chunks-fct `((,name isa chunk))))


(defmacro copy-chunk (chunk-name)
  "Create a new chunk which is a copy of the given chunk"
  `(copy-chunk-fct ',chunk-name))

(defun copy-chunk-fct (chunk-name)
  "Create a new chunk which is a copy of the given chunk"
    (let ((chunk (get-chunk-warn chunk-name)))
    (when chunk
      (when (use-short-copy-names)
        (unless (act-r-chunk-base-name chunk)
          (setf (act-r-chunk-base-name chunk) (concatenate 'string (symbol-name chunk-name) "-"))))
      
      (let* ((new-name (new-name-fct (if (use-short-copy-names)
                                         (act-r-chunk-base-name chunk)
                                       (concatenate 'string (symbol-name chunk-name) "-"))))
             (new-chunk (make-act-r-chunk 
                         :name new-name
                         :base-name (act-r-chunk-base-name chunk)
                         :merged-chunks (list new-name)
                         :filled-slots (act-r-chunk-filled-slots chunk)
                         :parameter-values (make-array *chunk-parameters-count*
                                                       :initial-element *chunk-parameter-undefined*)
                         :slot-value-lists (copy-tree (act-r-chunk-slot-value-lists chunk)))))
        
        ;; Create the back links as needed
        
        (when (update-chunks-on-the-fly)
          (dolist (slot (act-r-chunk-slot-value-lists chunk))
            (let ((slot-name (car slot))
                  (old (cdr slot)))
              (when (chunk-p-fct old)
                (let ((bl (chunk-back-links old)))
                  (if (hash-table-p bl)
                      (push slot-name (gethash new-name bl))
                    (let ((ht (make-hash-table)))
                      (setf (gethash new-name ht) (list slot-name))
                      (setf (chunk-back-links old) ht))))))))
        
        ;; update its parameters for only those that need it
        
        (dolist (param *chunk-parameters-copy-list*)
          (if (act-r-chunk-parameter-copy param)
              (let ((current (aref (act-r-chunk-parameter-values chunk) (act-r-chunk-parameter-index param))))
                (setf (aref (act-r-chunk-parameter-values new-chunk) (act-r-chunk-parameter-index param))
                  (funcall (act-r-chunk-parameter-copy param) 
                           (if (eq current *chunk-parameter-undefined*)
                               (chunk-parameter-default param chunk-name)
                             current))))
            (setf (aref (act-r-chunk-parameter-values new-chunk) (act-r-chunk-parameter-index param))
              (funcall (act-r-chunk-parameter-copy-from-chunk param) chunk-name))))
                                              
        ;; Put it into the main table
        
        (setf (gethash new-name (act-r-model-chunks-table (current-model-struct))) new-chunk)
        
        ;; note the original
        
        (setf (act-r-chunk-copied-from new-chunk) chunk-name)
        
        new-name))))

(defmacro chunk-copied-from (chunk-name)
  "Return the name of the chunk from which the provided chunk was copied"
  `(chunk-copied-from-fct ',chunk-name))

(defun chunk-copied-from-fct (chunk-name)
  "Return the name of the chunk from which the provided chunk was copied"
  (let ((chunk (get-chunk-warn chunk-name)))
    (when chunk
      (let ((copied-from (act-r-chunk-copied-from chunk)))
        (values 
         (when (and copied-from (chunk-p-fct copied-from) (equal-chunks-fct chunk-name copied-from))
           copied-from)
         copied-from)))))


(defmacro define-chunks (&rest chunk-defs)
  "Create chunks in the current model"
  `(define-chunks-fct ',chunk-defs))

(defun create-slot-value-chunk-if-needed (value)
  (when (and value
             (symbolp value) 
             (not (keywordp value))
             (not (chunk-p-fct value))
             (not (numberp value))
             (not (eq t value))
             (alphanumericp (char (symbol-name value) 0)))
    (create-undefined-chunk value)))

(defun convert-slot-value-to-true (chunk slot-name value)
  (when (and (chunk-p-fct value) (update-chunks-on-the-fly))
    (setf value (true-chunk-name-fct value))
    ;; If it's a chunk save the back link to this chunk
    (let ((bl (chunk-back-links value)))
      (if (hash-table-p bl)
          (push slot-name (gethash (act-r-chunk-name chunk) bl))
        (let ((ht (make-hash-table)))
          (setf (gethash (act-r-chunk-name chunk) ht) (list slot-name))
          (setf (chunk-back-links value) ht)))))
  value)


(defun define-chunks-fct (chunk-def-list)
  "Create chunks in the current model"
  
  ;; Do it in 2 passes like the old add-dm because there could be 
  ;; circular references which should be allowed
  
  (verify-current-mp  
   "define-chunks called with no current meta-process."
   (verify-current-model
    "define-chunks called with no current model."
    (let ((chunk-list nil))
        ;; first pass just create the chunks
        (dolist (chunk-def chunk-def-list)
          (if (listp chunk-def)
              (let (name doc type slots slots-and-values
                         (type-pos (position 'isa chunk-def))
                         (doc-pos (position-if 'stringp (subseq chunk-def 0 (min (length chunk-def) 2))))
                         (parity (if (oddp (length chunk-def)) 'odd 'even)))
                (cond ((> (count 'isa chunk-def) 1)
                       (print-warning "Invalid chunk definition: ~S has more than one ISA." chunk-def))
                      ((and type-pos (> type-pos 2))
                       (print-warning "Invalid chunk definition: ~S too many specifiers before ISA." chunk-def))
                      ((and type-pos (= (1+ type-pos) (length chunk-def)))
                       (print-warning "Invalid chunk definition: ~S no chunk-type specified after ISA." chunk-def))
                      ((and type-pos (not (get-chunk-type (nth (1+ type-pos) chunk-def))))
                       (print-warning "Invalid chunk definition: ~S chunk-type specified does not exist." chunk-def))
                      ((and type-pos (= type-pos 1) (eq parity 'even))
                       (print-warning "Invalid chunk definition: ~S odd number of items after type specification." chunk-def))
                      ((and type-pos (zerop type-pos) (eq parity 'odd))
                       (print-warning "Invalid chunk definition: ~s odd number of items after type specification." chunk-def))
                      (t
                       (when type-pos
                         (setf type (get-chunk-type (nth (1+ type-pos) chunk-def))))
                       
                       (if (or (and (eq parity 'odd) (null type-pos))
                               (and (eq parity 'odd) (null doc-pos))
                               (and type-pos (= type-pos 2) doc-pos (= doc-pos 1)))
                           (setf name (first chunk-def))
                         (setf name (new-name-fct (if type
                                                      (symbol-name (act-r-chunk-type-name type))
                                                    "CHUNK"))))
                       (when (and doc-pos type-pos)
                         (setf doc (nth doc-pos chunk-def)))
                         
                       (setf slots-and-values 
                         (cond (type-pos 
                                (subseq chunk-def (+ 2 type-pos)))
                               ((eq parity 'even)
                                chunk-def)
                               (t
                                (subseq chunk-def 1))))
                       
                       (cond ((and type-pos (null type))
                              (print-warning "Invalid chunk definition: ~S ISA does not specify a valid type." chunk-def))
                             ((or (null name) (not (symbolp name)) (keywordp name))
                              (print-warning "Invalid chunk definition: ~S chunk name is not a valid symbol." chunk-def))
                             ((not (alphanumericp (char (symbol-name name) 0)))
                              (print-warning "Invalid chunk definition: ~S chunk name must be a symbol starting with an alphanumeric character." chunk-def))
                             ((and doc (not (stringp doc)))
                              (print-warning "Invalid chunk definition: ~S documentation is not a string." chunk-def))
                             ((oddp (length slots-and-values)) ;; shouldn't happen but just be safe
                              (print-warning "Invalid chunk definition: ~S slot and values list is an odd length." chunk-def))
                             ((chunk-p-fct name)
                              (print-warning "Invalid chunk definition: ~S names a chunk which already exists." chunk-def))
                             (t
                              (do* ((s slots-and-values (cddr s))
                                    (s-name (first s) (first s))
                                    (s-val (second s) (second s)))
                                   ((null s))
                                (if (or (and type (valid-ct-slot type s-name))
                                        (and (null type) (valid-slot-name s-name) (not (keywordp s-name))))
                                    (aif (assoc s-name slots)
                                         (setf (cdr it) s-val)
                                         (push (cons s-name s-val) slots))
                                  (if (and (symbolp s-name) (not (keywordp s-name)) (alphanumericp (char (symbol-name s-name) 0)))
                                      (if type
                                          (if (valid-slot-name s-name)
                                           (progn
                                             (print-warning "Invalid slot ~s specified when creating chunk with type ~s, but creating chunk ~s anyway." s-name (act-r-chunk-type-name type) name)
                                             (aif (assoc s-name slots)
                                                  (setf (cdr it) s-val)
                                                  (push (cons s-name s-val) slots)))
                                            (progn
                                              (print-warning "Invalid slot ~s specified when creating chunk ~s with type ~s.  Extending chunks with slot named ~s." s-name name (act-r-chunk-type-name type) s-name)
                                              (extend-possible-slots s-name)
                                              (push (cons s-name s-val) slots)))
                                        (progn
                                          (model-warning "Extending chunks with slot named ~s because of chunk definition ~s" s-name chunk-def)
                                          (extend-possible-slots s-name)
                                          (push (cons s-name s-val) slots)))
                                      (progn
                                        (print-warning "Invalid chunk definition: ~S invalid slot name ~s." chunk-def s-name)
                                        (setf s nil)
                                        (setf slots :error)))))
                                  
                              (unless (eq slots :error)
                                (let ((c (make-act-r-chunk 
                                          :name name 
                                          :merged-chunks (list name)
                                          :documentation doc
                                          :parameter-values (make-array *chunk-parameters-count*
                                                                        :initial-element *chunk-parameter-undefined*)
                                          :slot-value-lists (cons slots (if type
                                                                            (act-r-chunk-type-initial-spec type)
                                                                          nil)))))
                                  (push-last c chunk-list)
                                              
                                  ;; enter it into the main chunk table
                                  (setf (gethash name (act-r-model-chunks-table (current-model-struct))) c))))))))
            (print-warning "~S is not a list in call to define-chunks-fct" chunk-def)))
      
      ;; second pass create slot-value list and define parameters
      
      (dolist (chunk chunk-list)
        
        (let* ((slots-list nil)
               (specified-slots (car (act-r-chunk-slot-value-lists chunk)))
               (default-slots (mapcar (lambda (x) 
                                        (cons (act-r-slot-spec-name x) (act-r-slot-spec-value x))) 
                                (awhen (cdr (act-r-chunk-slot-value-lists chunk)) 
                                       (act-r-chunk-spec-slots it)))))
          
          ;; add any unspecified default slots to the set to add
          (dolist (default default-slots)
            (unless (assoc (car default) specified-slots)
              (push default specified-slots)))
          
          (dolist (slot specified-slots)
            (let ((slot-name (car slot))
                  (slot-value (cdr slot)))
              (when slot-value
                
                (create-slot-value-chunk-if-needed slot-value)
                                
                ;; if updates are happening on the fly map the value to
                ;; the "true" name
                
                (setf slot-value (convert-slot-value-to-true chunk slot-name slot-value))
                
                (push (cons slot-name slot-value) slots-list)
                
                (setf (act-r-chunk-filled-slots chunk)
                  (logior (act-r-chunk-filled-slots chunk) (slot-name->mask slot-name))))))
          
          (setf (act-r-chunk-slot-value-lists chunk) slots-list)))
      
      (mapcar 'act-r-chunk-name chunk-list)))))
        

(defun chk-slot-value (chunk slot-name)
  "Internal function for getting the value of a slot in a chunk structure"
  (cdr (assoc slot-name (act-r-chunk-slot-value-lists chunk))))

(defmacro chunk-slot-value (chunk-name slot-name)
  "Return the value of a slot for the named chunk"
  `(chunk-slot-value-fct ',chunk-name ',slot-name))

(defun chunk-slot-value-fct (chunk-name slot-name)
  "Return the value of a slot for the named chunk"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (chk-slot-value c slot-name))))

(defmacro chunk-filled-slots-list (chunk-name &optional sorted)
  "Return a sorted list of slots that exist in the named chunk"
  `(chunk-filled-slots-list-fct ',chunk-name ,sorted))

(defun chunk-filled-slots-list-fct (chunk-name &optional sorted)
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (if sorted
          (mapcar 'car (sort (copy-tree (act-r-chunk-slot-value-lists c)) #'< :key (lambda (x) (slot-name->index (car x)))))
        (mapcar 'car (act-r-chunk-slot-value-lists c))))))
  

(defmacro set-chunk-slot-value (chunk-name slot-name value)
  "Set the value of a chunk's slot"
  `(set-chunk-slot-value-fct ',chunk-name ',slot-name ',value))

(defun set-chunk-slot-value-fct (chunk-name slot-name value)
  "Set the value of a chunk's slot"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (set-chk-slot-value c slot-name value))))
  
(defun make-chunk-immutable (chunk-name)
  (let ((c (get-chunk chunk-name)))
    (when c
      (setf (act-r-chunk-immutable c) t))))

(defun set-chk-slot-value (c slot-name value)
  "internal chunk slot setting function"
  
  (when (act-r-chunk-immutable c)
    (unless (chunk-slot-equal value (chk-slot-value c slot-name))
      (print-warning "Cannot change contents of chunk ~s." (act-r-chunk-name c))
      (return-from set-chk-slot-value nil)))
  (if (valid-slot-name slot-name) ;; any slot name available can be set
      (progn
        ;; changing the chunk automatically breaks it as a copy
        ;; even if the change were to set a slot to the same
        ;; value it has currently.
        
        (setf (act-r-chunk-copied-from c) nil)
        
        ;; If the value in the slot now is a chunk then
        ;; remove this chunk from the back links of that chunk
        
        (when (update-chunks-on-the-fly)
          (let ((old (chk-slot-value c slot-name)))
            (when (chunk-p-fct old)
              (let* ((bl (chunk-back-links old))
                     (new-links (remove slot-name (gethash (act-r-chunk-name c) bl))))
                (if new-links
                    (setf (gethash (act-r-chunk-name c) bl) new-links)
                  (remhash (act-r-chunk-name c) bl))))))
        
        ;; If the new value should be a chunk but isn't
        ;; create one for it
        
        (create-slot-value-chunk-if-needed value)
        
        ;; if updates are happening on the fly map the value to
        ;; the "true" name
        
        (setf value (convert-slot-value-to-true c slot-name value))
        
        ;; Set the new slot value
        (let ((mask (slot-name->mask slot-name)))
          
          (if value
              (if (logtest mask (act-r-chunk-filled-slots c))
                  ;; already there so just replace the value
                  (rplacd (assoc slot-name (act-r-chunk-slot-value-lists c)) value)
                ;; add the slot to the list and set the filled slot bit
                (progn
                  (push (cons slot-name value) (act-r-chunk-slot-value-lists c))
                  (setf (act-r-chunk-filled-slots c) (logior mask (act-r-chunk-filled-slots c)))))
            
            ;; if it's on the list remove it and clear the filled bit
            (when (logtest mask (act-r-chunk-filled-slots c))
              (setf (act-r-chunk-slot-value-lists c) (delete (assoc slot-name (act-r-chunk-slot-value-lists c)) (act-r-chunk-slot-value-lists c)))
              (setf (act-r-chunk-filled-slots c) (logandc1 mask (act-r-chunk-filled-slots c))))))
        value)
    (print-warning "~s is not a valid slot name.  You can use extend-possible-slots to add it first if needed." slot-name)))


(defmacro mod-chunk (chunk-name &rest modifications)
  "Modify the slot values of a chunk"
  `(mod-chunk-fct ',chunk-name ',modifications))

(defun mod-chunk-fct (chunk-name modifications-list)
  "Modify the slot values of a chunk"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (if (oddp (length modifications-list))
          (print-warning "Odd length modifications list in call to mod-chunk.")
        (let ((slots nil)
              (slots-and-values nil))
            (do ((s modifications-list (cddr s)))
                ((null s))
              (pushnew (car s) slots)
              (push (cons (car s) (second s)) slots-and-values))
          (cond ((not (= (length slots) (length slots-and-values)))
                 (print-warning "Slot name used more than once in modifications list."))
                ((not (every 'valid-slot-name slots))
                 (print-warning "Invalid slot name ~s specified for mod-chunk." 
                                (find-if (lambda (x) (not (valid-slot-name x))) slots)))
                ((act-r-chunk-immutable c)
                 (print-warning "Cannot modify chunk ~s because it is immutable." chunk-name))
                (t
                 (dolist (slot-value slots-and-values chunk-name)
                   (set-chk-slot-value c (car slot-value) (cdr slot-value))))))))))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to potentially speed things way up provide un-checked but fast accessors
;;; to the chunk info...


(defun fast-chunk-slot-value-fct (chunk-name slot-name)
  "Return the value of a slot for the named chunk without testing validity"
  (let ((c (get-chunk chunk-name)))
    (when c
      (chk-slot-value c slot-name))))
      
(defun fast-set-chunk-slot-value-fct (chunk-name slot-name value)
  "Set the value of a chunk's slot without testing validity"
  (let ((c (get-chunk chunk-name)))
    (when c
      (set-chk-slot-value c slot-name value))))


(defun fast-mod-chunk-fct (chunk-name modifications-list)
  "Modify the slot values of a chunk without testing validity"
  (let ((c (get-chunk chunk-name)))
    (when c
      (unless (oddp (length modifications-list))
        (loop 
          (when (null modifications-list) (return))
          (set-chk-slot-value 
           c
           (pop modifications-list)
           (pop modifications-list)))
        chunk-name))))

(defun mod-chunk-with-spec-fct (chunk-name mod-spec)
  "Modify the slot values without testing except to skip request parameters and variables"
  (let ((c (get-chunk chunk-name)))
    (when c
      (dolist (x (act-r-chunk-spec-slots mod-spec) chunk-name)
        (let ((name (act-r-slot-spec-name x)))
          (unless (or (keywordp name) (chunk-spec-variable-p name)) 
            (set-chk-slot-value c name (act-r-slot-spec-value x))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro delete-chunk (chunk-name)
  "Delete a chunk from a model"
  `(delete-chunk-fct ',chunk-name))

(defun delete-chunk-fct (chunk-name)
  "Delete a chunk from a model"
  (let ((c (get-chunk-warn chunk-name)))
    (when c
      (if (act-r-chunk-immutable c)
          (print-warning "Cannot delete chunk ~s because it is marked as immutable." chunk-name)
        (let ((tn (act-r-chunk-name c)))
        ;; If this chunk has back-links from others to it then warn because
        ;; that's likely a problem
        
        (when (update-chunks-on-the-fly) 
          (when (and (hash-table-p (chunk-back-links chunk-name)) (not (zerop (hash-table-count (chunk-back-links chunk-name)))))
            (model-warning "Chunk ~s is being deleted but it is still used as a slot value in other chunks." chunk-name))
          
          (when (and (not (eq tn chunk-name)) (hash-table-p (chunk-back-links tn)) (not (zerop (hash-table-count (chunk-back-links tn)))))
            (model-warning "Chunk ~s is being deleted but its true name ~s is still used as a slot value in other chunks." chunk-name tn))
          
          ;; Delete all of the back-links to this chunk
          
          (dolist (slots (act-r-chunk-slot-value-lists c))
            (let ((slot-name (car slots))
                  (old (cdr slots)))
              (when (chunk-p-fct old)
                (let* ((bl (chunk-back-links old))
                       (new-links (remove slot-name (gethash tn bl))))
                  (if new-links
                      (setf (gethash tn bl) new-links)
                    (remhash tn bl)))))))
        
        ;; Take all the related chunks out of the main hash-table
        
        (dolist (x (act-r-chunk-merged-chunks c))
          (remhash x (act-r-model-chunks-table (current-model-struct)))
          
          ;; Take them out of the meta-data table too
          
          (when (update-chunks-on-the-fly)
            (remhash x (act-r-model-chunk-ref-table (current-model-struct)))))
        
        chunk-name)))))

(defmacro purge-chunk (chunk-name)
  "delete a chunk and release its name"
  `(purge-chunk-fct ',chunk-name))

(defun purge-chunk-fct (chunk-name)
  (let ((name (delete-chunk-fct chunk-name)))
    (when name
      (release-name-fct name))))

(defmacro merge-chunks (chunk-name1 chunk-name2)
  "Merge two chunks into a single representation"
  `(merge-chunks-fct ',chunk-name1 ',chunk-name2))

(defun merge-chunks-fct (chunk-name1 chunk-name2)
  "Merge two chunks into a single representation"
  (let ((c1 (get-chunk-warn chunk-name1))
        (c2 (get-chunk-warn chunk-name2)))
    (when (and c1 c2)
      (unless (chunk-equal-test c1 c2)
        (return-from merge-chunks-fct nil))
      (unless (eq c1 c2)
        
        ;; update the parameters for c1
        
        (dolist (param *chunk-parameters-merge-list*)
          (setf (aref (act-r-chunk-parameter-values c1) (act-r-chunk-parameter-index param))
            (if (eq (act-r-chunk-parameter-merge param) :second)
                (aref (act-r-chunk-parameter-values c2) (act-r-chunk-parameter-index param))
              (funcall (act-r-chunk-parameter-merge param) chunk-name1 chunk-name2))))
        
        ;; If either is immutable then the result should be as well.
        ;; Since c1 will maintain its immutability need to check if c2 
        ;; is immutable and then make c1 immutable if c2 is.
        
        (when (act-r-chunk-immutable c2)
          (setf (act-r-chunk-immutable c1) t))
        
        
        ;; For any chunks which had been merged with c2 also remap them
        ;; and indicate them in c1
        
        (dolist (x (act-r-chunk-merged-chunks c2))
          (setf (gethash x (act-r-model-chunks-table (current-model-struct))) c1)
          (push x (act-r-chunk-merged-chunks c1)))
                
        
        ;; When name-remapping is on
        
        (when (update-chunks-on-the-fly)
          
          ;; delete all back-links to the c2 chunk
          
          (dolist (slots (act-r-chunk-slot-value-lists c2))
            (let ((slot-name (car slots))
                  (old (cdr slots)))
              
              (when (chunk-p-fct old)
                (let* ((bl (chunk-back-links old))
                       (new-links (remove slot-name (gethash chunk-name2 bl))))
                  (if new-links
                      (setf (gethash chunk-name2 bl) new-links)
                    (remhash chunk-name2 bl))))))
          
          ;; replace all the slot values which hold chunk-name2 with chunk-name1
          
          (when (hash-table-p (chunk-back-links chunk-name2))
            (maphash (lambda (chunk slots)
                       (dolist (x slots)
                         (fast-set-chunk-slot-value-fct chunk x chunk-name1)
                         (dolist (notify (notify-on-the-fly-hooks))
                           (funcall notify chunk))))
                     (chunk-back-links chunk-name2))
            (clrhash (chunk-back-links chunk-name2)))))
      
      chunk-name1)))



(defmacro create-chunk-alias (chunk alias)
  `(create-chunk-alias-fct ',chunk ',alias))

(defun create-chunk-alias-fct (chunk alias)
  (verify-current-mp  
   "create-chunk-alias called with no current meta-process."
   (verify-current-model
    "create-chunk-alias called with no current model."
    (cond ((not (chunk-p-fct chunk))
           (model-warning "~s is not the name of a chunk in the current model." chunk))
          ((chunk-p-fct alias)
           (model-warning "~s is already the name of a chunk in the current model and cannot be used as an alias." alias))
          ((not (symbolp alias))
           (model-warning "~s is not a symbol and thus cannot be used as a chunk alias." alias))
          (t
           (let ((c (get-chunk chunk)))
             (setf (gethash alias (act-r-model-chunks-table (current-model-struct))) c)
             (push alias (act-r-chunk-merged-chunks c))
             alias))))))


(defmacro eq-chunks (chunk-name1 chunk-name2)
  "Return t if two chunks have the same underlying representation"
  `(eq-chunks-fct ',chunk-name1 ',chunk-name2))

(defun eq-chunks-fct (chunk-name1 chunk-name2)
  "Return t if two chunks have the same underlying representation"
  (let ((c1 (get-chunk-warn chunk-name1))
        (c2 (get-chunk-warn chunk-name2)))
    (and c1 c2 (eq c1 c2))))

(defmacro true-chunk-name (chunk-name)
  "Return the prototypical name of a chunk in the event of merging"
  `(true-chunk-name-fct ',chunk-name))

(defun true-chunk-name-fct (chunk-name)
  "Return the prototypical name of a chunk in the event of merging"
  (let ((c (get-chunk chunk-name)))
    (if c
        (act-r-chunk-name c)
      chunk-name)))

(defmacro equal-chunks (chunk-name1 chunk-name2)
  "Return t if two chunks are of the same chunk-type and have equal slot values"
  `(equal-chunks-fct ',chunk-name1 ',chunk-name2))

(defun equal-chunks-fct (chunk-name1 chunk-name2)
  "Return t if two chunks are of the same chunk-type and have equal slot values"
  (let ((c1 (get-chunk-warn chunk-name1))
        (c2 (get-chunk-warn chunk-name2)))
    (chunk-equal-test c1 c2)))


(defun chunk-equal-test (c1 c2)
  "Internal function for comparing the equality of two chunks"
  (and c1 c2 (or (eq c1 c2)
                 (and (= (act-r-chunk-filled-slots c1)
                         (act-r-chunk-filled-slots c2))
                      (every (lambda (slot) 
                               (chunk-slot-equal
                                (cdr slot)
                                (chk-slot-value c2 (car slot))))
                             (act-r-chunk-slot-value-lists c1))))))


(defun chunk-slot-equal (val1 val2)
  (if (eq val1 val2)
      t
    (let (c1 c2)
      (cond ((and (setf c1 (get-chunk val1))
                  (setf c2 (get-chunk val2)))
             (eq c1 c2))
            ((stringp val1) 
             (and (stringp val2) (string-equal val1 val2)))
            (t (equalp val1 val2))))))


(defvar *suppress-extend-item-warning* nil)

(defun suppress-extension-warnings ()
  (setf *suppress-extend-item-warning* t))
  
(defun unsuppress-extension-warnings ()
  (setf *suppress-extend-item-warning* nil))


(defmacro extend-chunks (parameter-name &key (default-value nil)
                                        (default-function nil)
                                        (merge-function nil)
                                        (copy-function nil)
                                        (copy-from-chunk-function nil))
  "Add new parameters to all chunks"
  (if (or (not (symbolp parameter-name)) (keywordp parameter-name))
      (print-warning "~s is not a valid symbol for specifying a chunk parameter." parameter-name)
    
    (let ((accessor-name (intern (concatenate 'string "CHUNK-"  (string-upcase parameter-name))))
          (setf-name (intern (concatenate 'string "CHUNK-" (string-upcase parameter-name) "-SETF")))
          (index (gensym))
          (exists (gensym))
          (param (gensym)))
      (if (find parameter-name *chunk-parameters-list* :key #'act-r-chunk-parameter-name)
          (progn
            (print-warning "Parameter ~s already defined for chunks." parameter-name)
            :duplicate-parameter)
        (progn ;; can't do the function test because of potential issues with things being available at compile time 
          (if nil ;(notevery 'fctornil (list default-function merge-function copy-function copy-from-chunk-function))
              (print-warning "~{Invalid value ~s specified for the ~s function to extend chunks~^~%~}"
                             (mapcan (lambda (x y) 
                                       (unless (fctornil x)
                                         (list x y)))
                               (list default-function merge-function copy-function copy-from-chunk-function)
                               (list :default-function :merge-function :copy-function :copy-from-chunk-function)))
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               (unless *suppress-extend-item-warning*
                 (when (fboundp ',accessor-name)
                   (print-warning "Function ~s already exists and is being redefined." ',accessor-name))
                 (when (fboundp ',setf-name)
                   (print-warning "Function ~s already exists and is being redefined." ',setf-name)))
               (let* ((,index *chunk-parameters-count*)
                      (,exists (find ',parameter-name *chunk-parameters-list* :key #'act-r-chunk-parameter-name))
                      (,param (make-act-r-chunk-parameter :name ',parameter-name
                                                          :index ,index
                                                          :default-value ',default-value
                                                          :default-function ',default-function
                                                          :merge ',merge-function
                                                          :copy ',copy-function
                                                          :copy-from-chunk ',copy-from-chunk-function
                                                          :accessor ',accessor-name)))
                 (if ,exists
                     (progn
                       (setf ,index (act-r-chunk-parameter-index ,exists))
                       (setf *chunk-parameters-list* (remove ,exists *chunk-parameters-list*))
                       (setf *chunk-parameters-copy-list* (remove ,exists *chunk-parameters-copy-list*))
                       (setf *chunk-parameters-merge-list* (remove ,exists *chunk-parameters-merge-list*))
                       (setf (act-r-chunk-parameter-index ,param) (act-r-chunk-parameter-index ,exists)))
                   (incf *chunk-parameters-count*))
                 
                 (push ,param *chunk-parameters-list*)
                 
                 (if ',copy-function 
                     (push ,param *chunk-parameters-copy-list*)
                   (when ',copy-from-chunk-function
                     (push ,param *chunk-parameters-copy-list*)))
                 
                 (when ',merge-function
                   (push ,param *chunk-parameters-merge-list*))
                 
                 (defun ,accessor-name (chunk-name)
                   (let ((c (get-chunk chunk-name)))
                     (if c
                         (let ((v (aref (act-r-chunk-parameter-values c) ,index)))
                           (if (eq v *chunk-parameter-undefined*)
                               (setf (aref (act-r-chunk-parameter-values c) ,index)
                                 (chunk-parameter-default ,param chunk-name))
                             v))
                       (print-warning "Chunk ~s does not exist in attempt to access ~a." chunk-name ',accessor-name))))
                 (defun ,setf-name (chunk-name new-value)
                   (let ((c (get-chunk chunk-name)))
                     (if c
                         (setf (aref (act-r-chunk-parameter-values c) ,index) new-value)
                       (print-warning "Chunk ~s does not exist in attempt to set ~a." chunk-name ',accessor-name))))
                 (defsetf ,accessor-name ,setf-name)
                 ',accessor-name))))))))


(defun normalize-chunk-names (&optional (unintern? nil))
  (if (current-model-struct)
      (if (update-chunks-on-the-fly) 
          
          ;; Use the meta-data table to do the work
          (maphash (lambda (key value)
                     (when (not (eq key (act-r-chunk-name value))) ;; not a used chunk
                       
                       ;; Square up all names for unused chunks
                       
                       (let ((bl (chunk-back-links key)))
                         (when (hash-table-p bl)
                           (let ((tn (true-chunk-name-fct key)))
                             (maphash (lambda (c ss)
                                        (dolist (s ss)
                                          (fast-set-chunk-slot-value-fct c s tn)
                                          (dolist (notify (notify-on-the-fly-hooks))
                                            (funcall notify c))))
                                      bl))
                           (clrhash bl)))
                       
                       ;; release names of unused chunks
                       
                       (when unintern?
                         
                         ;; Take it out of the main hash-table
                         (remhash key (act-r-model-chunks-table (current-model-struct)))
                         
                         ;; Take it out of the meta-data table too
                         (remhash key (act-r-model-chunk-ref-table (current-model-struct)))
                         
                         ;; unintern the name
                         (release-name-fct key))))
                   (act-r-model-chunks-table (current-model-struct)))
        
        ;; Without the meta-data do it the hard way
        (let ((possible-removals nil))
          (maphash (lambda (key value)
                     (when (not (eq key (act-r-chunk-name value)))
                       (push key possible-removals)))
                   (act-r-model-chunks-table (current-model-struct)))
          
          ;; clean up the chunk references
          ;; this could take a while
          
          (when possible-removals
            (maphash (lambda (chunk val)
                       (when (eq chunk (act-r-chunk-name val))
                         (dolist (slot-val (act-r-chunk-slot-value-lists (get-chunk chunk)))
                           (let ((slot (car slot-val))
                                 (value (cdr slot-val)))
                             (when (and (chunk-p-fct value) (member value possible-removals))
                               (fast-set-chunk-slot-value-fct chunk slot (true-chunk-name-fct value))
                               (dolist (notify (notify-on-the-fly-hooks))
                                 (funcall notify chunk)))))))
                     (act-r-model-chunks-table (current-model-struct))))
           
           (when unintern?
             (dolist (x possible-removals)
               (remhash x (act-r-model-chunks-table (current-model-struct)))
               (release-name-fct x)))))
       
       (print-warning "No current model in which to normalize chunk names.")))

       

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
