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
;;; Filename    : chunk-types.lisp
;;; Version     : 2.0
;;; 
;;; Description : Definition of chunk-types and function that manipulate them.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider changing more of the commands to return two values
;;;             :     with the second one indicating whether the chunk-type was
;;;             :     valid as I did for chunk-type-slot-names-fct.
;;; 
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation
;;; 2005.01.16 Dan
;;;             : * Removed the print-chunk-type function since I don't want to
;;;             :   hide the structure since users shouldn't see them anyway.
;;; 2005.01.17 Dan
;;;             : * Changed pprint-chunk-type to use command-output and a
;;;             :   compiled format string.
;;; 2005.01.18 Dan
;;;             : * Made it so chunk-type returns the name and not the struct.
;;; 2005.01.21 Dan
;;;             : * Fixed a bug with maintaining the subtypes information.
;;; 2005.02.04 Dan
;;;             : * Changed member to find for speed. (?)
;;; 2005.02.24 Dan
;;;             : * Changed pprint-chunk-type becasue some Lisps don't take a
;;;             :   preformatted format string with the ~? directive.
;;; 2005.03.25 Dan
;;;             : * Changed chunk-type-fct so that when it builds a chunk-type
;;;             :   as a subtype the slot ordering is maintained. 
;;; 2005.09.01 Dan
;;;             : * Added extend-chunk-type-slots to support the experimental
;;;             :   change to p* that will allow a RHS modification to add
;;;             :   new slots to a chunk.  This should NOT be used in general
;;;             :   or by any other system/module/model at this time.
;;;             : * Had to patch chunk-type-fct to copy the slots list because
;;;             :   otherwise the macro calls inside of the existing code
;;;             :   get thumped by extend-... making the change persistent.
;;; 2006.01.18 Dan
;;;             : * Modified extend-chunk-type-slots to also record the new
;;;             :   slot names in a separate list.
;;;             : * Added the extended-slot-name-p function to allow one to see
;;;             :   whether or not a given slot name was one of the originals.
;;; 2006.03.02 Dan [1.0]
;;;             : * Fixed an issue with recording the subtype info that caused
;;;             :   problems with retrievals when there were more than 2 levels
;;;             :   of inheritance.
;;; 2008.05.01 Dan
;;;             : * Fixed a typo in one of the warning messages.
;;;             : * Changed chunk-type-slot-names-fct so that it returns two
;;;             :   values where the first is the list of slot names and the 
;;;             :   second is t or nil to indicate whether the chunk-type named
;;;             :   was valid.  This allows one to distinguish a return value
;;;             :   of nil for a chunk-type with no slots from a failure due to
;;;             :   an invalid chunk-type name.
;;; 2008.12.08 Dan
;;;             : * Added the calls to new-chunk-type-size so the model code
;;;             :   can keep track of the largest possible chunk size.
;;; 2009.11.17 Dan
;;;             : * Fixed an issue in how subtypes are created if they specify
;;;             :   slots which already exist in the parent because the current
;;;             :   procedural matching code relies on the order of the slots
;;;             :   and that's easier to address here than it is there.
;;; 2010.06.14 Dan
;;;             : * Renamed pprint-chunk-type to pprint-ct and made pprint-chunk-
;;;             :   type and pprint-chunk-type-fct user level commands that take
;;;             :   a chunk-type name.
;;; 2010.11.18 Dan
;;;             : * Explicitly prevent the creation of a chunk-type which has
;;;             :   a slot named isa to avoid any problems with production 
;;;             :   parsing.
;;; 2011.04.27 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;; 2012.02.10 Dan
;;;             : * Added some safety checks into extend-chunk-type-slots.
;;; 2012.03.21 Dan
;;;             : * Added a declaim for current-model since something in the
;;;             :   extend-chunk-type-slots checking uses it.
;;; 2013.01.24 Dan
;;;             : * Record indices for all the slots in a type and its subtypes
;;;             :   so that things can remain "in order" even when chunk extension
;;;             :   results in slot collisions between subtypes.
;;; 2013.01.25 Dan
;;;             : * Make sure that chunk-type size gets recorded as the size of
;;;             :   the index table for the type and not just the slot list.
;;;             : * Switch the indices from a hash-table to an adjustable vector
;;;             :   since the mapping needs to go both ways. 
;;;             : * Adding chunk-type-slot-name-from-index and chunk-type-slot-index
;;;             :   commands for accessing the index information.
;;; 2013.01.28 Dan
;;;             : * Maintaining an additional possible-slots list with each type.
;;;             : * Added the possible-chunk-type-slot command.
;;; 2013.04.11 Dan
;;;             : * Changed extend-chunk-type-slots to return the slot-name if
;;;             :   it successfully adds the slot to the type and also to test
;;;             :   that slot-name is a symbol.
;;; 2013.05.17 Dan [1.1]
;;;             : * Adding the "static" chunk-types which differ from normal
;;;             :   types in that they don't extend the slots of a given type
;;;             :   but instead create new subtypes when extended.
;;;             :   The new type is created by indicating (:static) when 
;;;             :   defining the type.  A subtype of a static type is also
;;;             :   static (only one of :include or :static is allowed
;;;             :   in the definition).
;;;             :   Extending a static type will also immediately change the type 
;;;             :   a chunk which is being extended which requires the additional 
;;;             :   optional parameter to extend-chunk-type-slots to indicate the
;;;             :   chunk.
;;;             :   Subtypes generated through extension of a static are not bound
;;;             :   by single inheritance and may have many different parent types,
;;;             :   but all of those will be grounded by the "root" static type.
;;;             : * Two system parameters are added with this to allow one to
;;;             :   specify whether static is the default setting, :static-default,
;;;             :   and to provide a hook function for creating the subtype names
;;;             :   :new-static-type-name.
;;; 2013.05.21 Dan
;;;             : * Changed the extend-chunk-type-slots test from valid to
;;;             :   possible since don't need to extend static chunks "again" for
;;;             :   a slot it doesn't currently have.
;;; 2013.05.22 Dan
;;;             : * Added another system parameter :show-static-subtype-names.
;;;             :   If t then it always shows the real chunk-type when printing
;;;             :   a chunk of a static type but if nil it only ever shows the
;;;             :   root type.  The default is nil.
;;; 2013.05.29 Dan
;;;             : * Fixed a bug with the static types if a subtype was made of
;;;             :   a non-root type because the new type didn't get the slots of
;;;             :   the actual parent only the root.
;;;             : * Fixed a bug introduced with the last fix which broke the
;;;             :   static subtypes of the root type not getting slots.
;;; 2013.05.30 Dan
;;;             : * Cleaned up an unused variable in chunk-type-fct.
;;; 2013.08.12 Dan
;;;             : * Added chunk-type-possible-slot-names command since that's
;;;             :   needed by the new procedural pre-checking code.
;;; 2013.10.25 Dan
;;;             : * Adjust how static chunk-types are created.  No longer create
;;;             :   the whole graph up front and instead create new static types
;;;             :   on the fly.  
;;; 2013.11.04 Dan
;;;             : * Make sure that a static gets the possible slots from its 
;;;             :   children as well as parents since it could be created in the
;;;             :   "middle" of the hierarchy now.
;;; 2014.02.12 Dan
;;;             : * Added another option for :show-static-subtype-names so that
;;;             :   it will show the type name as long as it wasn't generated
;;;             :   automatically (defined).  To support that now set the
;;;             :   user-defined slot of a chunk-type to nil when it is generated
;;;             :   automatically and added the canonical-chunk-type-name
;;;             :   function to handle determining which name to use based on the
;;;             :   setting of :show-static-subtype-names.
;;; 2014.02.13 Dan
;;;             : * Switching the default behavior for chunk-types to be static,
;;;             :   and reenabled the :static-default system parameter to do so.
;;; 2014.02.24 Dan [2.0]
;;;             : * What if chunk-types are only for preprocessing purposes?
;;;             :   Here're the changes that are being tested:
;;;             :   - Consider all chunk-types to inherit from type chunk
;;;             :   - Forget about the static specifier
;;;             :   - Allow duplicate types, but warn
;;; 2014.05.13 Dan
;;;             : * Changed valid-chunk-type-slot and valid-ct-slot to use the
;;;             :   possible slots list so that slots of subtypes can be used.
;;; 2014.05.23 Dan
;;;             : * Make sure there's a current model in all the extend-...
;;;             :   functions.
;;;             : * Added an extend-chunk-type-slots function which prints a warning
;;;             :   and then calls extend-possible-slots for backward compatability.
;;; 2014.05.29 Dan
;;;             : * Add in the hack to work with 6.0 models that adds an additional
;;;             :   slot to every chunk-type with a default value of t so they're 
;;;             :   all unique and subtypes still work out right.
;;; 2014.06.09 Dan
;;;             : * Allow the static keyword in definitions for backward 
;;;             :   compatibility, but just warn that it doesn't do anything now.
;;; 2014.06.18 Dan
;;;             : * Valid-slot-name now returns the index for true.
;;; 2014.06.20 Dan
;;;             : * Only allow slots to have names which start with an alphanumeric
;;;             :   character.
;;; 2014.06.24 Dan
;;;             : * Don't save subtype info.
;;;             : * Allow multiple parent types!
;;;             :   Any common default slots of the parents must match (not having
;;;             :   a default value for a common slot is OK) to be defined.
;;; 2014.06.25 Dan
;;;             : * Change how a chunk-type is printed to show all the parent
;;;             :   types when there's multiple inheritance.
;;; 2014.08.15 Dan
;;;             : * Fixed a bug with print-all-chunk-types.
;;; 2014.09.24 Dan
;;;             : * Changed extend-possible-slots so that if it gets passed a
;;;             :   slot name that exists, but isn't on the extended-slots list
;;;             :   adds it to the list since it may be needed for types other
;;;             :   than the one which originally created it.  Then also changed
;;;             :   chunk-type-possible-slot-names-fct to remove the duplicates
;;;             :   since it returns the combination of a chunk's slots and the
;;;             :   extended list.
;;; 2014.09.26 Dan
;;;             : * Bug in last update caused incorrect warnings to be reported.
;;; 2014.09.29 Dan
;;;             : * Changed the declaim for define-chunk-spec-fct to be 
;;;             :   consistent with other declaim of that function.
;;; 2014.10.16 Dan
;;;             : * Changed the warning for extend-chunk-type-slots since it
;;;             :   seemed to imply that it didn't do anything.
;;;             : * Valid-slot-name only returns one value now.
;;; 2014.12.17 Dan
;;;             : * Changed the declaim for some functions because they didn't 
;;;             :   indicate multiple return values and some Lisps care about that.
;;;             : * Removed declaim for new-chunk-type-size.
;;; 2015.07.28 Dan
;;;             : * Removed the *act-r-6.0-compatibility* hack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; chunk-type structure for internal use only.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(declaim (ftype (function (t) (values t t)) get-module-fct))
(declaim (ftype (function () t) current-model))
(declaim (ftype (function (t) (values t t)) get-chunk))
(declaim (ftype (function (&optional t) (values t t t)) new-name-fct))
(declaim (ftype (function (t &optional t t) (values t t)) define-chunk-spec-fct))

(defun get-chunk-type (name)
  "Internal command to get a chunk-type structure from its name"
  (verify-current-mp  
   "get-chunk-type called with no current meta-process."
   (verify-current-model
    "get-chunk-type called with no current model."
    (gethash name (act-r-chunk-type-info-table (act-r-model-chunk-types-info (current-model-struct)))))))            

(defmacro chunk-type (&rest name-and-slots)
  "The user macro to define a new chunk-type."
  `(chunk-type-fct ',name-and-slots))

(defun chunk-type-fct (name-and-slots)
  "The user function to define a new chunk-type"
  (verify-current-mp  
   "chunk-type called with no current meta-process."
   (verify-current-model
    "chunk-type called with no current model."
    (cond ((null name-and-slots)
           (print-all-chunk-types))
          ((not (listp name-and-slots))
           (print-warning "chunk-type-fct must be passed a list which defines a chunk-type."))
          (t
           (let* ((name-description (car name-and-slots))
                  (name (if (consp name-description)
                            (car name-description) 
                          name-description))
                  (modifier (if (consp name-description)
                                (cdr name-description) 
                              nil))
                  (documentation (when (stringp (second name-and-slots))
                                   (second name-and-slots)))
                  (slots (if documentation (cddr name-and-slots) 
                           (cdr name-and-slots)))
                  (super-types (list (get-chunk-type 'chunk)))
                  (parents nil)
                  (info (act-r-model-chunk-types-info (current-model-struct))))
             
             (when (get-chunk-type name)
               (return-from chunk-type-fct 
                 (print-warning "Chunk-type ~S is already defined and redefinition is not allowed." name)))
             
             ; check type hierarchy
             
             (when modifier
               (unless (every (lambda (x) (and (listp x) (= 2 (length x)) (keywordp (first x)))) modifier)
                 (return-from chunk-type-fct 
                   (print-warning "Invalid modifier list specified with the chunk-type name: ~s" modifier)))
               
               (when (find :static modifier :key 'car)
                 (setf modifier (remove :static modifier :key 'car))
                 (print-warning "Static modifier in chunk-type definitions is depricated and has no effect."))
               
               (dolist (x modifier)
                 (unless (eq (first x) :include)
                   (return-from chunk-type-fct
                     (print-warning "Invalid modifier ~s in ~s chunk-type definition." (first x) name)))
                 (aif (get-chunk-type (second x))
                      (progn
                        (if (equalp super-types (list (get-chunk-type 'chunk)))
                            (setf super-types (list it))
                          (push it super-types))
                        (push-last (second x) parents))
                      (return-from chunk-type-fct
                        (print-warning "Non-existent chunk-type ~s specified as an :include in ~s chunk-type definition." (second x) name)))))
               
             
             (dolist (slot slots)
               (unless (or (and (atom slot) (symbolp slot) (not (keywordp slot))
                                (not (eq slot 'isa)) (alphanumericp (char (symbol-name slot) 0)))
                           (and (listp slot)
                                (= (length slot) 2)
                                (not (eq (car slot) 'isa))
                                (symbolp (car slot))
                                (not (keywordp (car slot)))
                                (alphanumericp (char (symbol-name (car slot)) 0))))
                 (return-from chunk-type-fct 
                   (print-warning "Unacceptable slot specification ~S for chunk-type ~S.  Chunk-type not created." slot name))))
             
             (unless (= (length slots) (length (remove-duplicates (mapcar 'chunk-type-slot-name slots))))
               (return-from chunk-type-fct 
                 (print-warning "Duplicate slot specifications in ~S for chunk-type ~S.  Chunk-type not created." slots name)))
             
             (let ((ct (make-act-r-chunk-type 
                        :name name 
                        :documentation documentation
                        :parents parents
                        :super-types (cons name (reduce 'union super-types :key 'act-r-chunk-type-super-types)))))
               
               
               ;; verify that all the parent types are compatible -- no conflicting default values
               (let* ((unique-parent-defaults (reduce (lambda (x y) 
                                                        (union x y :test 'equalp)) 
                                                      super-types :key (lambda (x) 
                                                                         (remove-if-not 'listp (act-r-chunk-type-slots x)))))
                      (needed-defaults (remove-if (lambda (x) (find (chunk-type-slot-name x) slots :key 'chunk-type-slot-name)) unique-parent-defaults)))
                 (unless (= (length needed-defaults)
                            (length (remove-duplicates needed-defaults :key 'first)))
                   (return-from chunk-type-fct
                     (print-warning "The multiple parents specified for type ~s ~s have inconsistent default values in one or more slots.  Chunk-type not created." name parents))))
               
               
               ;; Add any new slot names to the type info data
               
               (dolist (s (mapcar 'chunk-type-slot-name slots))
                 (unless (gethash s (act-r-chunk-type-info-slot->index info))
                   
                   (let ((index (vector-push-extend s (act-r-chunk-type-info-index->slot info))))
                     (setf (gethash s (act-r-chunk-type-info-slot->index info)) index)
                     (setf (gethash s (act-r-chunk-type-info-slot->mask info)) (expt 2 index))
                     (setf (act-r-chunk-type-info-size info) (1+ index)))))
               
               ;; Build the list of slots and possible slots for this chunk-type from the parent slots 
               
               (dolist (parent-slot (reduce 'union super-types :key 'act-r-chunk-type-slots))
                 (unless (find (chunk-type-slot-name parent-slot) slots :key 'chunk-type-slot-name :test 'eq)
                   (push-last parent-slot slots)))
               
               ;; slots now holds all the slots from the specification and all the parent slots
               ;; set that as the slots for the current type
               
               (setf (act-r-chunk-type-slots ct) slots)
               
               ;; push the slot names for this type onto all of the parent's possible-slots lists 
               ;; as well as its own
                 
               (dolist (type-name (act-r-chunk-type-super-types ct))
                 (let ((type (if (eq type-name name) ct (get-chunk-type type-name))))
                   (dolist (s (mapcar 'chunk-type-slot-name slots))
                     (pushnew s (act-r-chunk-type-possible-slots type) :test 'eq))))
               
               
               ;; set the default spec info (only care about filled slots, right?)
               
               (let ((filled-specs))
                 (dolist (slot (act-r-chunk-type-slots ct))
                   (when (and (listp slot) (second slot))
                     (push-last (first slot) filled-specs)
                     (push-last (second slot) filled-specs)))
                 
                 (setf (act-r-chunk-type-initial-spec ct)
                   (define-chunk-spec-fct filled-specs)))
               
               ;; add it to the table
               
               (setf (gethash name (act-r-chunk-type-info-table info)) ct)
               
               ;; and list of types
               
               (push-last name (act-r-chunk-type-info-types info))
               
               ;; Check and store the "signature" of the chunk-type to see
               ;; if it is identical to any others, and if so warn about it.
               
               (let ((named-slots 0) (ordered-slots nil)(defaults nil))
                 (dolist (slot (act-r-chunk-type-slots ct))
                   (setf named-slots (logior (gethash (chunk-type-slot-name slot) (act-r-chunk-type-info-slot->mask info)) named-slots))
                   (push slot ordered-slots))
                 
                 (dolist (slot (sort ordered-slots #'< :key (lambda (x) (gethash (chunk-type-slot-name x) (act-r-chunk-type-info-slot->index info)))))
                   (push (if (listp slot) (second slot) nil) defaults))
                 
                 (let ((possibles (gethash named-slots (act-r-chunk-type-info-distinct-types info))))
                   (if possibles
                          (aif (find defaults possibles :test 'equalp :key 'car)
                               (progn
                                 (print-warning "Chunk-type ~s has the same specification as the chunk-type~p ~{~s~^, ~}." name (length (cdr it)) (cdr it))
                                 (setf (cdr it) (push name (cdr it))))
                               (setf (gethash named-slots (act-r-chunk-type-info-distinct-types info))
                                 (push (cons defaults (list name)) it))) 
                     (setf (gethash named-slots (act-r-chunk-type-info-distinct-types info))
                          (list (cons defaults (list name))))))
                 
                 ;; don't return the bitvector of named slots
                 
                 (setf (act-r-chunk-type-slot-vector ct) named-slots)
                 
                 ;; just return the chunk-type name
                 
                 name))))))))


(defun chunk-type-slot-name (slot)
  "Internal function for parsing chunk-types"
  (if (atom slot)
      slot
    (first slot)))

(defun print-all-chunk-types ()
  "Internal function for printing all chunk-types" 
  (let ((types (act-r-chunk-type-info-types (act-r-model-chunk-types-info (current-model-struct)))))
    (dolist (x types types)
      (pprint-chunk-type-fct x))))

(defconstant *pprint-chunk-type-string*
     (formatter "~S~@[ <- ~{~s~^, ~}~]~@[ ~S~]~%~{~{   ~s~@[ (~s)~]~%~}~}~%")
  "Internal compiled format string used to print out chunk-types")

(defun pprint-ct (chunk-type)
  "Pretty prints a chunk-type."
  (command-output  
   (format nil *pprint-chunk-type-string*
     (act-r-chunk-type-name chunk-type)
     (act-r-chunk-type-parents chunk-type)
     (act-r-chunk-type-documentation chunk-type)
     (mapcar (lambda (slot)
               (if (listp slot)
                   slot
                 (list slot nil)))
       (act-r-chunk-type-slots chunk-type))))
  (act-r-chunk-type-name chunk-type))


(defmacro pprint-chunk-type (chunk-type)
  `(pprint-chunk-type-fct ',chunk-type))

(defun pprint-chunk-type-fct (chunk-type)
  (verify-current-mp  
   "pprint-chunk-type called with no current meta-process."
   (verify-current-model
    "pprint-chunk-type called with no current model."
    (aif (get-chunk-type chunk-type)
         (pprint-ct it)
         (print-warning "~s does not name a chunk-type in the current model." chunk-type)))))

(defmacro chunk-type-p (chunk-type-name?)
  "Predicate macro for verifying that a chunk-type of a given name exists"
  `(chunk-type-p-fct ',chunk-type-name?))

(defun chunk-type-p-fct (chunk-type-name?)
  "Predicate function for verifying that a chunk-type of a given name exists"
  (if (get-chunk-type chunk-type-name?)
      t nil))


(defun valid-slot-name (slot)
  "Function to determine if a slot name is valid for any chunk"
  (values
   (let ((info (act-r-model-chunk-types-info (current-model-struct))))
     (and info (gethash slot (act-r-chunk-type-info-slot->index info))))))

(defun valid-query-name (query)
  (let ((info (act-r-model-chunk-types-info (current-model-struct))))
    (and info (find query (act-r-chunk-type-info-query-slots info)))))

(defun valid-chunk-type-slot (chunk-type-name slot)
  (let ((ct (get-chunk-type chunk-type-name)))
    (if ct
        (valid-ct-slot ct slot)
      (print-warning "Invalid chunk-type name ~s passed to valid-chunk-type-slot." chunk-type-name))))


(defun valid-ct-slot (chunk-type slot)
  (let ((info (act-r-model-chunk-types-info (current-model-struct))))
    (and chunk-type info (or (find slot (act-r-chunk-type-possible-slots chunk-type) :key 'chunk-type-slot-name)
                             (find slot (act-r-chunk-type-info-extended-slots info))))))

(defun chunk-type-possible-slot-names-fct (chunk-type-name)
  (let ((ct (get-chunk-type chunk-type-name)))
    (if ct
        (let ((info (act-r-model-chunk-types-info (current-model-struct))))
          (if info 
              (remove-duplicates (append (act-r-chunk-type-possible-slots ct)
                                         (act-r-chunk-type-info-extended-slots info)))
            (print-warning "No chunk-types info available for call to chunk-type-possible-slot-names-fct.")))
      (print-warning "Invalid chunk-type name ~s passed to chunk-type-possible-slot-names-fct." chunk-type-name))))
      
      
(defun extend-possible-slots (slot-name &optional (warn t))
  (aif (current-model-struct)
       (let ((info (act-r-model-chunk-types-info it)))
         (cond ((null info)
                (print-warning "Chunk-type info not available so the slot ~s cannot be added." slot-name))
               ((null slot-name)
                (print-warning "Nil is not a valid slot name when trying to extend slots."))
               ((not (symbolp slot-name))
                (print-warning "~s cannot be used as a slot name because it is not a symbol." slot-name))
               ((not (alphanumericp (char (symbol-name slot-name) 0)))
                (print-warning "~s cannot be used as a slot name because it does not start with an alphanumeric character." slot-name))
               ((keywordp slot-name)
                (print-warning "~s cannot be used as a slot name because it is a keyword." slot-name))
               ((gethash slot-name (act-r-chunk-type-info-slot->index info))
                (pushnew slot-name (act-r-chunk-type-info-extended-slots info))
                (when warn
                  (print-warning "~s already names a possible slot for chunks." slot-name)))
               (t
                ;; add it to the list of extended slots and create the appropriate
                ;; entries for it.
                (push slot-name (act-r-chunk-type-info-extended-slots info))    
                (let ((index (vector-push-extend slot-name (act-r-chunk-type-info-index->slot info))))
                  (setf (gethash slot-name (act-r-chunk-type-info-slot->index info)) index)
                  (setf (gethash slot-name (act-r-chunk-type-info-slot->mask info)) (expt 2 index))
                  (setf (act-r-chunk-type-info-size info) (1+ index)))
                slot-name)))
       (print-warning "Chunk-type info not available so the slot ~s cannot be added." slot-name)))

(defun extend-chunk-type-slots (chunk-type slot)
  (declare (ignore chunk-type))
  (print-warning "Extend-chunk-type-slots is depricated and extend-possible-slots should be used instead.")
  (extend-possible-slots slot))

(defun add-request-parameter (parameter-name)
  (aif (current-model-struct)
       (let ((info (act-r-model-chunk-types-info it)))
         (cond ((null info)
                (print-warning "Chunk-type info not available so the request parameter ~s cannot be added." parameter-name))
               ((null parameter-name)
                (print-warning "Nil is not a valid request parameter name."))
               ((not (keywordp parameter-name))
                (print-warning "~s cannot be used as a request parameter because it must be a keyword." parameter-name))
               ((gethash parameter-name (act-r-chunk-type-info-slot->index info))
                parameter-name)
               ; just let it go
               ; because multiple modules may want to use the same 
               ; request parameter name (print-warning "~s already names a request parameter for chunks." parameter-name))
               (t
                ;; add it to the list of extended slots and create the appropriate
                ;; entries for it.
                (push parameter-name (act-r-chunk-type-info-extended-slots info))    
                (let ((index (vector-push-extend parameter-name (act-r-chunk-type-info-index->slot info))))
                  (setf (gethash parameter-name (act-r-chunk-type-info-slot->index info)) index)
                  (setf (gethash parameter-name (act-r-chunk-type-info-slot->mask info)) (expt 2 index))
                  (setf (act-r-chunk-type-info-size info) (1+ index)))
                parameter-name)))
       (print-warning "Chunk-type info not available so the request parameter ~s cannot be added." parameter-name)))


(defun add-buffer-query (query-name)
  (aif (current-model-struct)
       (let ((info (act-r-model-chunk-types-info it)))
         (cond ((null info)
                (print-warning "Chunk-type info not available so the query ~s cannot be added." query-name))
               ((null query-name)
                (print-warning "Nil is not a valid query name."))
               ((keywordp query-name)
                (print-warning "~s cannot be used as a query parameter because it is a keyword." query-name))
               ((find query-name (act-r-chunk-type-info-query-slots info))
                query-name)
               (t
                ;; add it to the list of query slots but don't create any slot info...
                (push query-name (act-r-chunk-type-info-query-slots info))    
                ;(let ((index (vector-push-extend query-name (act-r-chunk-type-info-index->slot info))))
                ;  (setf (gethash query-name (act-r-chunk-type-info-slot->index info)) index)
                ;  (setf (gethash query-name (act-r-chunk-type-info-slot->mask info)) (expt 2 index))
                ;  (setf (act-r-chunk-type-info-size info) (1+ index)))
                query-name)))
       (print-warning "Chunk-type info not available so the query ~s cannot be added." query-name)))

(defun slot-name->mask (slot-name)
  (aif (current-model-struct)
       (let ((info (act-r-model-chunk-types-info it)))
         (if info
             (aif (gethash slot-name (act-r-chunk-type-info-slot->mask info))
                  it
                  (print-warning "~s does not name a valid slot in the current model." slot-name))
           (print-warning "Chunk-type info not available so a mask for the slot ~s cannot be found." slot-name)))
       (print-warning "Chunk-type info not available so a mask for the slot ~s cannot be found." slot-name)))

(defun slot-name->index (slot-name)
  (aif (current-model-struct)
       (let ((info (act-r-model-chunk-types-info it)))
         (if info
             (aif (gethash slot-name (act-r-chunk-type-info-slot->index info))
                  it
                  (print-warning "~s does not name a valid slot in the current model." slot-name))
           (print-warning "Chunk-type info not available so an index for the slot ~s cannot be found." slot-name)))
       (print-warning "Chunk-type info not available so an index for the slot ~s cannot be found." slot-name)))


(defun slot-index->name (slot-index)
  (aif (current-model-struct)
       (let ((info (act-r-model-chunk-types-info it)))
         (if info
             (if (< slot-index (length (act-r-chunk-type-info-index->slot info)))
                 (aref (act-r-chunk-type-info-index->slot info) slot-index)
               (print-warning "~s is not a valid slot index in the current model." slot-index))
           (print-warning "Chunk-type info not available so a name for the slot index ~s cannot be found." slot-index)))
       (print-warning "Chunk-type info not available so a name for the slot index ~s cannot be found." slot-index)))
  
(defun slot-mask->names (slot-mask)
  (aif (current-model-struct)
       (let ((info (act-r-model-chunk-types-info it)))
         (if info
             (let ((len (integer-length slot-mask))
                   res)
               (if (<= len (act-r-chunk-type-info-size info))
                   (dotimes (i len res)
                     (when (logbitp i slot-mask)
                       (push-last (aref (act-r-chunk-type-info-index->slot info) i) res)))
                 (print-warning "~s is not a valid slot mask in the current model." slot-mask)))
           (print-warning "Chunk-type info not available so slot names for the mask ~s cannot be found." slot-mask)))
       (print-warning "Chunk-type info not available so slot names for the mask ~s cannot be found." slot-mask)))


(defun all-chunk-type-names ()
  (verify-current-mp  
   "all-chunk-type-names called with no current meta-process."
   (verify-current-model
    "all-chunk-type-names called with no current model."
    (act-r-chunk-type-info-types (act-r-model-chunk-types-info (current-model-struct))))))

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
