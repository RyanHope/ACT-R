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
;;; Filename    : chunk-spec.lisp
;;; Version     : 2.0
;;; 
;;; Description : Definition of chunk specifications and corresponding functions
;;; 
;;; Bugs        : 
;;;
;;; To do       : * Finish the documentation.
;;;             : * Investigate optimizations after there's some use.
;;;             : * Add a function to check chunk-specs to make module writing
;;;             :   easier.
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation.
;;; 2004.12.29 Dan
;;;             : Realized that the comparitors are backwards with respect
;;;             : to productions in test-chunk-slots.
;;; 2005.02.03 Dan
;;;             : * Changing the internal slot-value-lists of a chunk to be a
;;;             :   hash-table instead of an alist...
;;; 2005.02.09 Dan
;;;             : * Some minor cleanup - changing member to find where possible.
;;; 2005.04.19 Dan
;;;             : * Added pprint-chunk-spec.
;;; 2005.05.16 Dan
;;;             : * Modified chunk-spec-variable-p to test that the name has
;;;             :   a length > 1 to reject the symbol itself as a variable.
;;;             :   That fixes a minor problem in production parsing and I
;;;             :   don't think it breaks anything else.
;;; 2005.09.09 Dan
;;;             : * Renamed chunk-to-chunk-spec chunk-name-to-chunk-spec to
;;;             :   clarify its use because I introduced a bug somewhere
;;;             :   along the line with its usage that didn't actually affect
;;;             :   any existing modules, but may cause problems for other
;;;             :   module writers.
;;;             : * Also fixed chunk-name-to-chunk-spec because it didn't 
;;;             :   include nil slots in the spec, but it probably should (it
;;;             :   did prior to my "fixing" it when I changed over to hash
;;;             :   tables).
;;; 2005.11.17 Dan
;;;             : * Fixed chunk-name-to-chunk-spec because it incorrectly
;;;             :   referenced the internal chunk-type slot list instead of
;;;             :   using ct-slot-names.
;;; 2006.09.11 Dan
;;;             : * Changed chunk-slot-equal so that it uses equalp instead of
;;;             :   equal when the string and chunk checks fall through because
;;;             :   numbers (which is the typical value that'd fall through)
;;;             :   don't test well with equal...
;;; 2007.06.18 Dan
;;;             : * Added slot-specs-to-chunk-spec-list and chunk-spec-to-chunk-def
;;;             :   as "official" commands now and also added slot-specs-to-chunk-spec
;;;             :   which removes the need to call define-chunk-spec after slot-specs-
;;;             :   to-chunk-spec-list.
;;; 2007.07.02 Dan
;;;             : * Changed PROCESS-SLOTS-SPECS so that it keeps the slot-spec
;;;             :   in the order provided during definition.
;;; 2007.08.07 Dan
;;;             : * Fixed pprint-chunk-spec so that it actually has the : on the
;;;             :   front of the request parameters.
;;; 2008.06.16 Dan
;;;             : * Changed test-chunk-slots so that it always fails if there
;;;             :   are unbound variables even when they're used in other 
;;;             :   tests.  Doesn't change the normal operation, just shuts
;;;             :   down the possibility of "odd" chunk-specs doing unusual
;;;             :   matches that result in a true result.
;;; 2008.06.17 Dan
;;;             : * Changed test-chunk-slots so that it ignores request parameters
;;;             :   in the chunk-spec for matching purposes.
;;;             : * Added tests for current model and mp to match-chunk-spec-p.
;;; 2008.06.18 Dan
;;;             : * Added a test to find-matching-chunks to verify that the 
;;;             :   chunks parameter is a list when it's not :all.
;;;             : * Fixed the warning in chunk-spec-slot-spec to name the right
;;;             :   function.
;;; 2008.06.20 Dan
;;;             : * Changed chunk-spec-to-chunk-def so that tests to make sure
;;;             :   the parameter is a chunk-spec.
;;;             : * Changed chunk-spec-to-chunk-def so that it also rejects a
;;;             :   spec with request parameters.
;;;             : * Removed the macros of slot-specs-to-chunk-spec-list and
;;;             :   slot-specs-to-chunk-spec and renamed the functions to not
;;;             :   have the -fct since none of the other chunk-spec manipulation
;;;             :   commands use that convention and they aren't the types of
;;;             :   things that would be entered 'directly' and need a macro
;;;             :   form.
;;; 2008.07.31 Dan
;;;             : * Took chunk-slot-equal out of here and put it into the chunks
;;;             :   file.
;;; 2008.10.30 Dan
;;;             : * Performance improvement for test-chunk-slots - fewer loops
;;;             :   over the spec/slots.
;;; 2013.01.28 Dan
;;;             : * Changed the internal storing of the chunk-type to the name
;;;             :   since that's what got used anyway and changed process-slots-specs
;;;             :   to call valid-chunk-type-slot instead (which is the command
;;;             :   it really should have been using anyway) and similarly don't
;;;             :   use the internal chunk/chunk-type accessors in chunk-name-to-chunk-spec.
;;; 2013.02.14 Dan
;;;             : * Changed strip-request-parameters-from-chunk-spec so that it
;;;             :   maintains the ordering of the slots and removed an unneeded
;;;             :   cond from chunk-spec-slot-spec.
;;; 2013.03.13 Dan [1.1]
;;;             : * Changed process-slots-specs so that it allows a parent type
;;;             :   to specify slots of child types in chunk-specs.
;;;             : * Changed test-chunk-slots so that it requires that a chunk
;;;             :   have the specified slot as well as matching its value --
;;;             :   not having the slot is not the same as having the slot with
;;;             :   a value of nil.
;;; 2013.03.18 Dan 
;;;             : * Fixed some order dependence issues in test-chunk-slots.
;;; 2013.06.07 Dan
;;;             : * Changed test-chunk-slots so that it now requires a parameter
;;;             :   indicating whether the chunk is static or not because now a
;;;             :   static chunk which doesn't have a particular slot will match
;;;             :   to a spec which says "= <that slot> nil" because for statics
;;;             :   not having a slot is the same as having a slot with an explict
;;;             :   value of nil.
;;; 2014.02.14 Dan
;;;             : * Fixed a bug in test-chunk-slots with how it tests variables
;;;             :   between slots that was broken with the 13/3/13 change.
;;; 2014.03.03 Dan [2.0]
;;;             : * Conversion to the no chunk-types at run time mechanism.  A
;;;             :   chunk-spec now holds two vectors of slot info (filled and empty)
;;;             :   instead of a type symbol.
;;;             : * Chunk-spec-chunk-type returns nil and prints a warning.
;;;             : * Chunk-spec-filled-slots and chunk-spec-empty-slots return 
;;;             :   the corresponding vectors for comparison purposes.
;;;             : * The isa is optional when defining a chunk-spec but if provided 
;;;             :   starts with the default spec for the type, but overwrites 
;;;             :   any default values with those provided in the spec.
;;; 2014.03.14 Dan
;;;             : * Treat request parameters like slots in a chunk-spec, but not
;;;             :   in chunks.
;;; 2014.03.19 Dan
;;;             : * Allow a chunk-spec to have variablized slot names, add an
;;;             :   instantiate-chunk-spec command, and require that matching a
;;;             :   chunk-spec only uses the "instantiated" slot specs.
;;;             : * Remove the warning when a chunk-spec-slot-spec gets a slot
;;;             :   that's not specified to avoid the need for pre-checking the
;;;             :   slots everywhere that's used, and do the same for the 
;;;             :   slot-in-chunk-spec-p function as well.
;;; 2014.03.20 Dan
;;;             : * Let define-chunk-spec-fct extend chunks automatically with
;;;             :   optional parameters that indicate if it can and if it should
;;;             :   print a warning when it does.
;;;             : * Now returns 2 values: the spec and a list of slots that were
;;;             :   added by extension.
;;; 2014.03.26 Dan
;;;             : * Don't allow modifiers for request parameters i.e. they must
;;;             :   have an = modifier.
;;; 2014.03.27 Dan
;;;             : * Added merge-chunk-specs command.
;;; 2014.03.28 Dan
;;;             : * Instantiate-chunk-spec now returns two values like define-
;;;             :   chunk-spec.
;;; 2014.04.01 Dan
;;;             : * Added the functions for comparing a chunk/slot-vector to
;;;             :   the filled and empty specs so I don't need to do the bit
;;;             :   tests directly everywhere.
;;; 2014.04.07 Dan
;;;             : * Moved chunk-spec-variable-p to misc-utils.
;;; 2014.05.08 Dan
;;;             : * Adding the structure spec-slot to allow for more cleanly
;;;             :   accessing the components of a slot specification.  However,
;;;             :   they are still represented as lists this just provides 
;;;             :   accessors to treat the list as a struct.
;;; 2014.05.19 Dan
;;;             : * Putting slot-specs-to-chunk-spec back in because vision and
;;;             :   audio can use such a function.
;;; 2014.06.09 Dan
;;;             : * Changed a warning so that it matches the older version.
;;; 2014.06.11 Dan
;;;             : * Fixed some logic in add-slot-spec-to-chunk-spec so that the
;;;             :   backward compatibility flag properly indicates how to deal
;;;             :   with a - slot <val> condition in a chunk-spec.
;;; 2014.06.12 Dan
;;;             : * Reevaluated the logic in how matching nil slots should work
;;;             :   under various conditions and decided that it should be the
;;;             :   same with or without the backward compat. flag because we
;;;             :   don't want to capture the "odd" behavior of 6.0 rejecting
;;;             :   a non-existant slot.
;;; 2014.06.24 Dan
;;;             : * Allow the definition of a chunk-spec which uses an isa and
;;;             :   slots which don't belong to that type, but print a hard
;;;             :   warning.
;;; 2014.09.05 Dan
;;;             : * Add extra tests to process-slots-specs when trying to extend
;;;             :   with a new slot because with allowing 'bad' slots as above
;;;             :   it has to be sure bad values are at least valid as a slot 
;;;             :   name first.
;;; 2014.11.04 Dan
;;;             : * Added model and meta-process checks to pprint-chunk-spec.
;;;             : * Starting to fix issues with the match and find code to deal
;;;             :   with the fact that variables are now allowed in the slot name
;;;             :   positions of chunk-specs.  Isn't a problem for the current
;;;             :   code that uses chunk-specs since conflict resolution doesn't 
;;;             :   rely on it and things like visicon searches which use vars
;;;             :   only do so in the value spot, but it should do the "right"
;;;             :   thing for all usage.
;;; 2014.11.05 Dan
;;;             : * Match-chunk-spec-p now works for arbitrary depth slot name
;;;             :   bindings when using the default variable character.
;;; 2014.11.06 Dan
;;;             : * Match-chunk-spec-p works fully with other variable characters
;;;             :   though it can be tough to take advantage of that since slot
;;;             :   names need to be valid during the definition of the chunk-spec.
;;;             : * Changed slot-in-chunk-spec-p to return t when it's a varaible
;;;             :   now too.
;;;             : * Added the slot-spec-* accessors to go along with spec-slot
;;;             :   since they better match the description in the manual and are
;;;             :   safer since they don't throw an error if non-list provided.
;;;             : * Fixed chunk-spec-to-chunk-def to fail if there are variablized
;;;             :   slots in the chunk-spec.
;;;             : * Safety check added to verify-single-explicit-value.
;;; 2014.12.16 Dan
;;;             : * Don't use the list structure trick to define the spec-slot-*
;;;             :   accessors since not all Lisps like that when passed nil.
;;;             :   Instead just make it functions calling first, second, and
;;;             :   third with no error checking (also was true for the struct
;;;             :   accessors) along with corresponding setf forms.
;;; 2015.08.19 Dan
;;;             : * Added the chunk-difference-to-chunk-spec function which 
;;;             :   returns a chunk-spec with the modifications necessary to
;;;             :   turn the second chunk into a copy of the first.  Needed by
;;;             :   visual tracking because with nil slots not being part of a
;;;             :   chunk now chunk-name-to-chunk-spec isn't sufficient if the
;;;             :   first chunk doesn't have a slot the second.  May be useful
;;;             :   in other places as well.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The structures are not for external use.
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
;;; Find-matching-chunks automatically ignores request parameters now so they
;;; don't have to be stripped out first...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Can't do this... (defstruct (spec-slot (:type list)) op name value)
;;; So just make them simple macros instead.

(defun spec-slot-op (spec) (first spec))
(defun set-spec-slot-op (spec value) (setf (first spec) value))
(defsetf spec-slot-op set-spec-slot-op)

(defun spec-slot-name (spec) (second spec))
(defun set-spec-slot-name (spec value) (setf (second spec) value))
(defsetf spec-slot-name set-spec-slot-name)

(defun spec-slot-value (spec) (third spec))
(defun set-spec-slot-value (spec value) (setf (third spec) value))
(defsetf spec-slot-value set-spec-slot-value)


(defun slot-spec-modifier (slot-spec-list)
  (if (and (listp slot-spec-list) (= 3 (length slot-spec-list)))
      (first slot-spec-list)
    (print-warning "Invalid slot-spec list ~s passed to slot-spec-modifier." slot-spec-list)))
  
(defun slot-spec-slot (slot-spec-list)
  (if (and (listp slot-spec-list) (= 3 (length slot-spec-list)))
      (second slot-spec-list)
    (print-warning "Invalid slot-spec list ~s passed to slot-spec-slot." slot-spec-list)))

(defun slot-spec-value (slot-spec-list)
  (if (and (listp slot-spec-list) (= 3 (length slot-spec-list)))
      (third slot-spec-list)
    (print-warning "Invalid slot-spec list ~s passed to slot-spec-value." slot-spec-list)))

(defmacro define-chunk-spec (&rest specifications)
  `(define-chunk-spec-fct ',specifications))

(defun define-chunk-spec-fct (specifications-list &optional (extend t) (warn t))
  (verify-current-mp  
   "define-chunk-spec-fct called with no current meta-process."
   (verify-current-model
    "define-chunk-spec-fct called with no current model."
    (cond ((= (length specifications-list) 1)
           (if (get-chunk (car specifications-list))
               (chunk-name-to-chunk-spec (car specifications-list))
             (print-warning "define-chunk-spec's 1 parameter doesn't name a chunk: ~S" specifications-list)))
          ((eq (car specifications-list) 'isa)
           (let ((chunk-type (get-chunk-type (second specifications-list))))
             (if chunk-type
                 (process-slots-specs chunk-type (cddr specifications-list) extend warn)
               (print-warning "Element after isa in define-chunk-spec isn't a chunk-type. ~S" specifications-list))))
          (t
           (process-slots-specs nil specifications-list extend warn))))))

(defmacro define-query-spec (&rest specifications)
  `(define-query-spec-fct ',specifications))

(defun define-query-spec-fct (specifications-list)
  (verify-current-mp  
   "define-chunk-spec-fct called with no current meta-process."
   (verify-current-model
    "define-chunk-spec-fct called with no current model."
    (cond ((= (length specifications-list) 1)
           (print-warning "define-query-spec was only given one parameter: ~S" specifications-list))
          (t
           (process-query-specs specifications-list))))))

(defun chunk-name-to-chunk-spec (chunk-name)
  (let ((spec (make-act-r-chunk-spec))
        (chunk (get-chunk chunk-name)))
    (if chunk
        (let ((slot-vector (act-r-chunk-filled-slots chunk)))
          (setf (act-r-chunk-spec-filled-slots spec) slot-vector)
          (setf (act-r-chunk-spec-equal-slots spec) slot-vector)
          (dolist (slot (act-r-chunk-slot-value-lists chunk) spec)
            (push (make-act-r-slot-spec :name (car slot) :value (cdr slot) :testable t) (act-r-chunk-spec-slots spec))))
      (print-warning "Chunk-name-to-chunk-spec called with a non-chunk ~s." chunk-name))))


(defun chunk-difference-to-chunk-spec (chunk-name1 chunk-name2)
  (let ((spec (make-act-r-chunk-spec))
        (chunk1 (get-chunk chunk-name1))
        (chunk2 (get-chunk chunk-name2)))
    (if (and chunk1 chunk2)
        (let* ((slot-vector (act-r-chunk-filled-slots chunk1))
               (slots-not-in-c1 (lognot (act-r-chunk-filled-slots chunk1)))
               (slots-in-c2 (act-r-chunk-filled-slots chunk2))
               (slots-to-remove-from-c2 (logand slots-not-in-c1 slots-in-c2)))
          (setf (act-r-chunk-spec-filled-slots spec) slot-vector)
          (setf (act-r-chunk-spec-empty-slots spec) slots-to-remove-from-c2)
          (setf (act-r-chunk-spec-equal-slots spec) (logior slot-vector slots-to-remove-from-c2))
          (dolist (slot (act-r-chunk-slot-value-lists chunk1))
            (push (make-act-r-slot-spec :name (car slot) :value (cdr slot) :testable t) (act-r-chunk-spec-slots spec)))
          (dolist (slot (slot-mask->names slots-to-remove-from-c2))
            (push (make-act-r-slot-spec :name slot :value nil :testable t) (act-r-chunk-spec-slots spec)))
          spec)
      (print-warning "Chunk-difference-to-chunk-spec called with ~s and ~s which are not both chunks." chunk-name1 chunk-name2))))


(defun chunk-spec-filled-slots (chunk-spec)
  (if (act-r-chunk-spec-p chunk-spec)
      (act-r-chunk-spec-filled-slots chunk-spec)
    (print-warning "Chunk-spec-filled-slots called with a non-chunk-spec")))

(defun chunk-spec-empty-slots (chunk-spec)
  (if (act-r-chunk-spec-p chunk-spec)
      (act-r-chunk-spec-empty-slots chunk-spec)
    (print-warning "Chunk-spec-empty-slots called with a non-chunk-spec")))

(defun chunk-spec-chunk-type (chunk-spec)
  (declare (ignore chunk-spec))
  (print-warning "Chunk-specs no longer contain a chunk-type."))

(defun chunk-spec-match-chunk-type (chunk-spec chunk-type-signature)
  ;; not sure what I need for this yet...
  (declare (ignore chunk-spec chunk-type-signature))
  )

(defun chunk-spec-slots (chunk-spec)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning "Chunk-spec-slots called with something other than a chunk-spec"))
        (t
         (remove-duplicates (mapcar 'act-r-slot-spec-name (act-r-chunk-spec-slots chunk-spec))))))
  
(defun chunk-spec-slot-spec (chunk-spec &optional slot)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning "Chunk-spec-slot-spec called with something other than a chunk-spec"))
        ((and slot (find slot (act-r-chunk-spec-slots chunk-spec) :key 'act-r-slot-spec-name))
         (mapcar 'slot-spec-to-list 
           (remove-if-not (lambda (x)
                            (eq x slot))
                          (act-r-chunk-spec-slots chunk-spec)
                          :key 'act-r-slot-spec-name)))
        (slot
         ; Don't print a warning for missing slot specs now
         ; (print-warning "Slot ~S is not specified in the chunk-spec." slot)
         nil)
        (t
         (mapcar 'slot-spec-to-list (act-r-chunk-spec-slots chunk-spec)))))

(defun slot-in-chunk-spec-p (chunk-spec slot)
  (if (act-r-chunk-spec-p chunk-spec)
      (if (chunk-spec-variable-p slot)
          (when (find slot (act-r-chunk-spec-slots chunk-spec) :key 'act-r-slot-spec-name)
            t)
        (let ((mask (slot-name->mask slot)))
          (if mask
              (or (logtest mask (act-r-chunk-spec-filled-slots chunk-spec))
                  (logtest mask (act-r-chunk-spec-empty-slots chunk-spec)))
            ;; Don't print the warning here either
            ;; (print-warning "Slot-in-chunk-spec-p called with an invalid slot ~s" slot)
            nil)))
    (print-warning "Slot-in-chunk-spec-p called with something other than a chunk-spec")))

(defun slot-spec-to-list (slot-spec)
  (list (act-r-slot-spec-modifier slot-spec)
        (act-r-slot-spec-name slot-spec)
        (act-r-slot-spec-value slot-spec)))
         
(defun process-slots-specs (chunk-type specs extend warn)
  (let ((spec (make-act-r-chunk-spec))
        (defaults nil)
        (extended? nil))
    
    (when chunk-type
      (let ((default-spec (act-r-chunk-type-initial-spec chunk-type)))
        (setf defaults (act-r-chunk-spec-slots default-spec))))
    
    (loop 
      (when (null specs)
        (dolist (x defaults)
          (let ((index (slot-name->index (act-r-slot-spec-name x))))
            ;; If the default slot is already tested for full or empty
            ;; skip the default value
            (unless (or (logbitp index (act-r-chunk-spec-filled-slots spec))
                        (logbitp index (act-r-chunk-spec-empty-slots spec)))
              (add-slot-spec-to-chunk-spec x spec))))
        (return (values spec extended?)))
      
      (let ((slot-spec (make-act-r-slot-spec)))
        
        (when (find (car specs) '(= - > < >= <=))
          (setf (act-r-slot-spec-modifier slot-spec) (pop specs)))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return nil))
        
        (let ((slot? (car specs)))
          (when (and (keywordp slot?) (not (eq (act-r-slot-spec-modifier slot-spec) '=)))
            (print-warning "Request parameters may not use a modifier other than =.")
            (return nil))
          (unless (or (and chunk-type (valid-ct-slot chunk-type slot?))
                      (and (null chunk-type) (valid-slot-name slot?))
                      (and (keywordp slot?) (valid-slot-name slot?))
                      (chunk-spec-variable-p slot?))
            (if (and extend (not (keywordp slot?)) (not (chunk-spec-variable-p slot?))
                     (symbolp slot?) (alphanumericp (char (symbol-name slot?) 0)))
                (let ((extended (extend-possible-slots slot? nil)))
                  (if extended
                      (progn
                        (push extended extended?)
                        (when warn
                          (model-warning "Chunks extended with slot ~s during a chunk-spec definition." slot?)))
                    (if chunk-type
                        (progn
                          (print-warning "Slot ~s invalid for type ~s but chunk-spec definition still created." slot? (act-r-chunk-type-name chunk-type))
                          (push (list :mismatch (act-r-chunk-type-name chunk-type) slot?) extended?))
                      (progn
                        (print-warning "Invalid slot-name ~s in call to define-chunk-spec." slot?)
                        (return nil)))))
              (progn
                (print-warning "Invalid slot-name ~S in call to define-chunk-spec." slot?)
                (return nil)))))
        
        (setf (act-r-slot-spec-name slot-spec) (pop specs))
        
        (when (chunk-spec-variable-p (act-r-slot-spec-name slot-spec))
            (setf (act-r-slot-spec-variable slot-spec) :slot))
        
        (when (keywordp (act-r-slot-spec-name slot-spec))
            (setf (act-r-slot-spec-request-param slot-spec) t))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return nil))
        
        (setf (act-r-slot-spec-value slot-spec) (pop specs))
        
        (when (chunk-spec-variable-p (act-r-slot-spec-value slot-spec))
          (if (act-r-slot-spec-variable slot-spec)
              (setf (act-r-slot-spec-variable slot-spec) :both)
            (setf (act-r-slot-spec-variable slot-spec) :value)))
        
        (unless (add-slot-spec-to-chunk-spec slot-spec spec)
          (return nil))))))


(defun process-query-specs (specs)
  (let ((spec (make-act-r-chunk-spec)))
    (loop 
      (when (null specs)
        (return (values spec nil)))
      
      (let ((slot-spec (make-act-r-slot-spec)))
        
        (when (find (car specs) '(= - > < >= <=))
          (if (find (car specs) '(> < >= <=))
              (progn
                (print-warning "Query specs only allow = or - modifiers.")
                (return nil))
            (setf (act-r-slot-spec-modifier slot-spec) (pop specs))))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-query-spec - not enough arguments")
          (return nil))
        
        (unless (valid-query-name (car specs)) 
          (print-warning "Invalid query name ~S in call to define-query-spec." (car specs))
          (return nil))
        
        (setf (act-r-slot-spec-name slot-spec) (pop specs))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-query-spec - not enough arguments")
          (return nil))
        
        (setf (act-r-slot-spec-value slot-spec) (pop specs))
        
        (when (chunk-spec-variable-p (act-r-slot-spec-value slot-spec))
          (setf (act-r-slot-spec-variable slot-spec) :value)
          (pushnew (act-r-slot-spec-value slot-spec) (act-r-chunk-spec-variables spec)))
        
        (push-last slot-spec (act-r-chunk-spec-slots spec))))))


(defun add-slot-spec-to-chunk-spec (slot-spec spec)
  
  (push-last slot-spec (act-r-chunk-spec-slots spec))
  
  (awhen (act-r-slot-spec-variable slot-spec)
         (when (or (eq it :value) (eq it :both))
           (pushnew (act-r-slot-spec-value slot-spec) (act-r-chunk-spec-variables spec)))
         (when (or (eq it :slot) (eq it :both))
           (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-variables spec))
           (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-slot-vars spec)))
         (when (eq it :both)
           (aif (assoc (act-r-slot-spec-value slot-spec) (act-r-chunk-spec-dependencies spec))
                (pushnew (act-r-slot-spec-name slot-spec) (cdr it))
                (push (list (act-r-slot-spec-value slot-spec) (act-r-slot-spec-name slot-spec)) (act-r-chunk-spec-dependencies spec)))))
  
  (if (or (eq (act-r-slot-spec-variable slot-spec) :both)
          (eq (act-r-slot-spec-variable slot-spec) :slot))
      (setf (act-r-slot-spec-testable slot-spec) t)
    
    (let* ((mod (act-r-slot-spec-modifier slot-spec))
           (slot-name (act-r-slot-spec-name slot-spec))
           (val (act-r-slot-spec-value slot-spec))
           (mask (slot-name->mask slot-name))
           (index (slot-name->index slot-name)))
      
      (when (keywordp slot-name)
        (setf (act-r-chunk-spec-request-param-slots spec) (logior mask (act-r-chunk-spec-request-param-slots spec))))
      
      (when (or (logbitp index (act-r-chunk-spec-filled-slots spec))
                (logbitp index (act-r-chunk-spec-empty-slots spec)))
        (setf (act-r-chunk-spec-duplicate-slots spec) (logior mask (act-r-chunk-spec-duplicate-slots spec))))
      
      (case mod
        (= (setf (act-r-chunk-spec-equal-slots spec) (logior mask (act-r-chunk-spec-equal-slots spec))))
        (- (setf (act-r-chunk-spec-negated-slots spec) (logior mask (act-r-chunk-spec-negated-slots spec))))
        (t (setf (act-r-chunk-spec-relative-slots spec) (logior mask (act-r-chunk-spec-relative-slots spec)))))
      
      (unless (keywordp slot-name)
        (if val
            ;; anything that's not nil means the slot filled bit gets set
            ;; and we need the test
            (progn
              (setf (act-r-slot-spec-testable slot-spec) t)
              (unless (eq mod '-)
                ;; We want "- slot <val>" to be true if the slot doesn't exist.  Thus
                ;; we only want to set the filled bit when it's not a negation test.
                ;; In ACT-R 6.0 there were actually 2 different situations: a chunk which
                ;; has the slot with a value of nil (which worked like this code does) and
                ;; not having the slot which failed all tests 
                
                (setf (act-r-chunk-spec-filled-slots spec) (logior mask (act-r-chunk-spec-filled-slots spec)))))
          ;; for nil things are a little more interesting and we don't need the tests
          (cond ((eq mod '=)
                 ;; if it's testing for nil that just means it's
                 ;; empty and all we need to do is set the empty bit
                 ;; because it doesn't need to actually test that
                 (setf (act-r-chunk-spec-empty-slots spec) (logior mask (act-r-chunk-spec-empty-slots spec))))
                ((eq mod '-)
                 ;; not nil only means it's filled
                 (setf (act-r-chunk-spec-filled-slots spec) (logior mask (act-r-chunk-spec-filled-slots spec))))
                (t
                 ;; This is actually an interesting question -- should the inequality tests
                 ;; allow testing for nil?  It doesn't seem "right", but maybe there's a
                 ;; use for it in some module description.  However, for now I'm going to
                 ;; go with it being not allowed.  If I find a legitimate use or someone 
                 ;; complains then maybe I'll change it.
                 (return-from add-slot-spec-to-chunk-spec
                   (print-warning "A ~s test for slot ~s with a value of nil not allowed in chunk-spec." mod slot-name))))))
      t)))
  
  
(defun instantiate-chunk-spec (chunk-spec bindings &optional (extend t))
  (let ((extended-slots nil))
    (cond ((not (act-r-chunk-spec-p chunk-spec))
           (print-warning "instantiate-chunk-spec called with something other than a chunk-spec: ~s" chunk-spec))
          ((not (and (listp bindings) (every (lambda (x)
                                               (and (consp x) (chunk-spec-variable-p (car x)) (cdr x)))
                                             bindings)))
           (print-warning "Instantiate-chunk-spec called with an invalid bindings alist: ~s" bindings))
          (t
           (let ((new-spec (make-act-r-chunk-spec)))
             (dolist (x (act-r-chunk-spec-slots chunk-spec))
               (aif (act-r-slot-spec-variable x)
                    (let* ((slot (if (eq it :value) 
                                     (act-r-slot-spec-name x)
                                   (aif (assoc (act-r-slot-spec-name x) bindings)
                                        (cdr it)
                                        (act-r-slot-spec-name x))))
                           (value (if (eq it :slot) 
                                      (act-r-slot-spec-value x)
                                    (replace-variables (act-r-slot-spec-value x) bindings))))
                      
                      (unless (or (chunk-spec-variable-p slot)
                                  (valid-slot-name slot))
                        (if extend
                            (let ((extended (if (keywordp slot) nil (extend-possible-slots slot))))
                              (if extended
                                  (push slot extended-slots)
                                (progn
                                  (print-warning "Invalid slot-name ~s in instantiation of chunk spec for variable ~s" slot (act-r-slot-spec-name x))
                                  (return-from instantiate-chunk-spec nil))))
                          (progn
                            (print-warning "Invalid slot-name ~s in instantiation of chunk spec for variable ~s" slot (act-r-slot-spec-name x))
                            (return-from instantiate-chunk-spec nil))))
                      
                      (add-slot-spec-to-chunk-spec (make-act-r-slot-spec :modifier (act-r-slot-spec-modifier x)
                                                                         :name slot
                                                                         :value value
                                                                         :variable (cond ((and (chunk-spec-variable-p slot)
                                                                                               (chunk-spec-variable-p value))
                                                                                          :both)
                                                                                         ((chunk-spec-variable-p slot)
                                                                                          :slot)
                                                                                         ((chunk-spec-variable-p value)
                                                                                          :value)
                                                                                         (t nil)))
                                                   new-spec))
                    ;; just add a copy of the old one
                    (add-slot-spec-to-chunk-spec (copy-act-r-slot-spec x) new-spec)))
             (values new-spec extended-slots))))))


(defun instantiate-query-spec (chunk-spec bindings)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning "instantiate-query-spec called with something other than a chunk-spec: ~s" chunk-spec))
        ((not (and (listp bindings) (every (lambda (x)
                                             (and (consp x) (chunk-spec-variable-p (car x)) (cdr x)))
                                           bindings)))
         (print-warning "Instantiate-query-spec called with an invalid bindings alist: ~s" bindings))
        (t
         (let ((new-spec (make-act-r-chunk-spec)))
           (dolist (x (act-r-chunk-spec-slots chunk-spec))
             (if (act-r-slot-spec-variable x) ;; it has to be value for a query
                 (let ((value (replace-variables (act-r-slot-spec-value x) bindings)))
                   (push-last (make-act-r-slot-spec :modifier (act-r-slot-spec-modifier x)
                                                    :name (act-r-slot-spec-name x)
                                                    :value value
                                                    :variable (if (chunk-spec-variable-p value) :value nil))
                              (act-r-chunk-spec-slots new-spec)))
                  ;; just add a copy of the old one
                  (push-last (copy-act-r-slot-spec x) (act-r-chunk-spec-slots new-spec))))
           (values new-spec nil)))))

;;; A specific value of nil may be important to some things, so that's why it
;;; returns a second value of t on success.

(defun verify-single-explicit-value (chunk-spec slot module cmd &optional (var-char #\=))
  (if (act-r-chunk-spec-p chunk-spec)
      (let ((index (slot-name->index slot)))
        (cond ((not (or (logbitp index (act-r-chunk-spec-filled-slots chunk-spec))
                        (logbitp index (act-r-chunk-spec-empty-slots chunk-spec))
                        (logbitp index (act-r-chunk-spec-request-param-slots chunk-spec))))
               (print-warning "~a command to ~s module requires a value for the ~a slot." cmd module slot))
              ((logbitp index (act-r-chunk-spec-duplicate-slots chunk-spec))
               (print-warning "~a slot may only be specified once in a ~a command to the ~s module." slot cmd module))
              ((not (logbitp index (act-r-chunk-spec-equal-slots chunk-spec)))
               (print-warning "~a slot may only have the = modifier in a ~a command to the ~s module." slot cmd module))
              (t
               (let* ((slot-spec (find slot (act-r-chunk-spec-slots chunk-spec) :key 'act-r-slot-spec-name))
                      (value (when slot-spec
                               (act-r-slot-spec-value slot-spec))))
                 (if (and value (characterp var-char) (symbolp value) (char-equal var-char (char (symbol-name value) 0)))
                     (print-warning "~a slot must be explict - not a variable in a ~a command to the ~s module." slot cmd module)
                   (values value t))))))
    (print-warning "~a is not a chunk-spec in ~a command to the ~s module." chunk-spec cmd module)))

(defun chunk-match-signature (chunk-name filled &optional empty)
  (let ((chunk (get-chunk chunk-name)))
    (cond ((not chunk)
           (print-warning "~s does not name a chunk in call to chunk-match-signature." chunk-name))
          ((not (integerp filled))
           (print-warning "~s is not an appropriate filled signature in call to chunk-match-signature." filled))
          ((not (or (null empty) (integerp empty)))
           (print-warning "~s is not an appropriate empty signature in call to chunk-match-signature." empty))
          (t
           (slots-vector-match-signature (act-r-chunk-filled-slots chunk) filled empty)))))  
  
(defun slots-vector-match-signature (slots-vector filled &optional empty)
  (and (= filled (logand filled slots-vector))
       (or (null empty)
           (= empty (logandc2 empty slots-vector)))))

(defun compare-chunk-to-signature (chunk-name filled &optional empty)
  (let ((chunk (get-chunk chunk-name)))
    (cond ((not chunk)
           (print-warning "~s does not name a chunk in call to compare-chunk-to-signature." chunk-name))
          ((not (integerp filled))
           (print-warning "~s is not an appropriate filled signature in call to compare-chunk-to-signature." filled))
          ((not (or (null empty) (integerp empty)))
           (print-warning "~s is not an appropriate empty signature in call to compare-chunk-to-signature." empty))
          (t
           (compare-slots-vector-to-signature (act-r-chunk-filled-slots chunk) filled empty)))))
  
(defun compare-slots-vector-to-signature (slots-vector filled &optional empty)
  "Returns match, extra slots with values, slots which were missing values, and slots which shouldn't have had values"
  (let ((filled? (= filled (logand filled slots-vector)))
        (empty? (or (null empty)
                    (= empty (logandc2 empty slots-vector)))))
    (values (and filled? empty?)
            (slot-mask->names (logandc2 slots-vector filled))
            (slot-mask->names (logandc1 slots-vector filled))
            (and empty (slot-mask->names (logand slots-vector empty))))))

(defun reprocess-chunk-spec (spec var-char)
  (let ((new-spec (make-act-r-chunk-spec)))
    (dolist (x (act-r-chunk-spec-slots spec))
      (let ((slot-spec (copy-act-r-slot-spec x)))
        (setf (act-r-slot-spec-variable slot-spec)
          (cond ((and (chunk-spec-variable-p (act-r-slot-spec-name slot-spec) var-char)
                      (chunk-spec-variable-p (act-r-slot-spec-value slot-spec) var-char))
                 :both)
                ((chunk-spec-variable-p (act-r-slot-spec-name slot-spec) var-char)
                 :slot)
                ((chunk-spec-variable-p (act-r-slot-spec-value slot-spec) var-char)
                 :value)
                (t nil)))
        (add-slot-spec-to-chunk-spec slot-spec new-spec)))
    new-spec))

(defun match-chunk-spec-p (chunk-name chunk-spec 
                                      &key (=test 'chunk-slot-equal) 
                                      (-test 'chunk-slot-not-equal)
                                      (>test 'safe>) (>=test 'safe>=) 
                                      (<test 'safe<) (<=test 'safe<=)
                                      (variable-char #\=))
  (verify-current-mp  
   "Match-chunk-spec-p called with no current meta-process."
   (verify-current-model
    "Match-chunk-spec-p called with no current model."
    (let ((chunk (get-chunk chunk-name)))
      (cond ((null chunk)
             (print-warning "~s does not name a chunk in call to match-chunk-spec-p." chunk-name))
            ((not (act-r-chunk-spec-p chunk-spec))
             (print-warning "~s is not a valid chunk-spec in call to match-chunk-spec-p." chunk-spec))
            (t 
             ;; check the variable character and reprocess the spec 
             ;; if it's not = since that may mean different slotnames are "real"
             
             (unless (char-equal variable-char #\=)
               (setf chunk-spec (reprocess-chunk-spec chunk-spec variable-char)))
             
             (cond
              ;; if the filled slots are filled and the empty slots aren't then test further
              ((slots-vector-match-signature (act-r-chunk-filled-slots chunk) (act-r-chunk-spec-filled-slots chunk-spec) (act-r-chunk-spec-empty-slots chunk-spec))
               
               (handler-case (test-chunk-slots (act-r-chunk-slot-value-lists chunk)
                                               chunk-spec =test -test >test >=test <test <=test)
                 (error (condition) 
                   (print-warning "Error ~S encountered in matching chunk ~s." condition chunk-name))))
              (t
               nil))))))))

(defun chunk-slot-not-equal (arg1 arg2)
 (not (chunk-slot-equal arg1 arg2)))

(defun test-chunk-slots (slots spec =test -test >test >=test <test <=test)
  (let ((bindings nil)
        (get-bindings nil)
        (others nil))
    
    (flet ((test-slot (modifier chunks-slot-value spec-value)
                      (funcall (case modifier
                                 (= =test)
                                 (- -test)
                                 (> >test)
                                 (>= >=test)
                                 (< <test)
                                 (<= <=test))
                               chunks-slot-value spec-value))
           (chunks-slot-value (slot)
                              (cdr (assoc slot slots))))
      
      ;; First pass to get all the constant bindings and test the constants
      
      (dolist (x (act-r-chunk-spec-slots spec))
        (when (act-r-slot-spec-testable x)
          
          (let ((value (act-r-slot-spec-value x))
                (modifier (act-r-slot-spec-modifier x))
                (slot (act-r-slot-spec-name x))
                (variable (act-r-slot-spec-variable x)))
            
            (cond (;; it's a constant test
                   (null variable)
                   (unless (test-slot modifier (chunks-slot-value slot) value)
                     (return-from test-chunk-slots nil)))
                  
                  (;; variables in constant slots
                   (eq (act-r-slot-spec-variable x) :value)
                   (cond (;; it has been bound and can be tested
                          (assoc value bindings)
                          (unless (test-slot modifier (chunks-slot-value slot) (cdr (assoc value bindings)))
                            (return-from test-chunk-slots nil)))
                         
                         (;; it's an equal test so create the binding
                          (eq modifier '=)
                          (push (cons value (chunks-slot-value slot)) bindings))
                         (;; some other test required so save it
                          t
                          (push x others))))
                  (;; other potential bindings save for pass 2
                   (and (eq modifier '=)
                        (eq variable :both))
                   (push x get-bindings))
                  (t ;; everything else goes at the end
                   (push x others))))))
      
      ;; Second pass if all variables bound then check things
      ;; that needed the bindings.
      
      (when get-bindings
        ;; just do this in repeated loops instead of trying to sort
        ;; things up front
        
        (do ((conds get-bindings)
             (changed t))
            ((or (null conds) (null changed)) 
             (unless (null conds)
               (return-from test-chunk-slots nil)))
          (setf changed nil)
          (dolist (x conds)
            (let ((value (act-r-slot-spec-value x))
                  (slot (act-r-slot-spec-name x)))
              
              (cond (;; slot name not bound so skip it
                       (null (assoc slot bindings)))
                    (;; value variable is bound already
                     ;; so just test it
                     (assoc value bindings)
                     (unless (test-slot '= (chunks-slot-value (cdr (assoc slot bindings))) (cdr (assoc value bindings)))
                       (return-from test-chunk-slots nil))
                     (setf conds (remove x conds))
                     (setf changed t))
                    (;; variable not bound yet so create binding if slot actually exists
                     ;; and remove this from the set
                     t
                     (let ((slot-value (chunks-slot-value (cdr (assoc slot bindings)))))
                       (unless slot-value
                         (return-from test-chunk-slots nil))
                       (push (cons value slot-value) bindings)
                       (setf conds (remove x conds))
                       (setf changed t))))))))
            
      ;; third pass - test all the others since bindings done
      
      (dolist (x others t)
        (let ((value (act-r-slot-spec-value x))
                (modifier (act-r-slot-spec-modifier x))
                (slot (act-r-slot-spec-name x))
                (variable (act-r-slot-spec-variable x)))
          
          (case variable
            (:value
             (unless (test-slot modifier (chunks-slot-value slot) (cdr (assoc value bindings)))
               (return-from test-chunk-slots nil)))
            (:slot
             (unless (test-slot modifier (chunks-slot-value (cdr (assoc slot bindings))) value)
               (return-from test-chunk-slots nil)))
            (:both
             (unless (test-slot modifier (chunks-slot-value (cdr (assoc slot bindings))) (cdr (assoc value bindings)))
               (return-from test-chunk-slots nil)))
            (t
             (unless (test-slot modifier (chunks-slot-value slot) value)
               (return-from test-chunk-slots nil)))))))))

(defun find-matching-chunks (chunk-spec 
                             &key 
                             (chunks :all) (=test 'chunk-slot-equal) 
                             (-test 'chunk-slot-not-equal)
                             (>test 'safe>) (>=test 'safe>=) 
                             (<test 'safe<) (<=test 'safe<=)
                             (variable-char #\=))
  
  (verify-current-mp  
   "Find-matching-chunks called with no current meta-process."
   (verify-current-model
    "Find-matching-chunks called with no current model."
    
    (let ((found nil))
      (cond ((not (act-r-chunk-spec-p chunk-spec))
             (print-warning "~s is not a valid chunk-spec in call to find-matching-chunks." chunk-spec))
            ((eq :all chunks)
             (maphash (lambda (name chunk)
                        (declare (ignore chunk))
                        (when (match-chunk-spec-p name chunk-spec 
                                                  :=test =test :-test -test
                                                  :>test >test :>=test >=test 
                                                  :<test <test :<=test <=test
                                                  :variable-char variable-char)
                          (push name found)))
                      (act-r-model-chunks-table (current-model-struct)))
             found)
            ((listp chunks)
             (dolist (name chunks found)
               (when (match-chunk-spec-p name chunk-spec 
                                         :=test =test :-test -test
                                         :>test >test :>=test >=test 
                                         :<test <test :<=test <=test
                                         :variable-char variable-char)
                 (push name found))))
            (t (print-warning "~S is not a valid value for the :chunks keyword parameter to find-matching-chunks." chunks)))))))
                       

(defun pprint-chunk-spec (chunk-spec)
  "Print a chunk specification in a 'production like' way to command output"
  (verify-current-mp  
   "Pprint-chunk-spec called with no current meta-process."
   (verify-current-model
    "Pprint-chunk-spec called with no current model."
    (when (act-r-chunk-spec-p chunk-spec)
      (dolist (slot (act-r-chunk-spec-slots chunk-spec))
        (if (eql '= (act-r-slot-spec-modifier slot))
            (command-output "    ~s ~s" (act-r-slot-spec-name slot) (act-r-slot-spec-value slot))
          (command-output " ~2a ~s ~s" (act-r-slot-spec-modifier slot) (act-r-slot-spec-name slot) (act-r-slot-spec-value slot))))))))


(defun slot-specs-to-chunk-spec-list (slot-specs)
  (apply 'append slot-specs))

(defun slot-specs-to-chunk-spec (slot-specs)
  (define-chunk-spec-fct (slot-specs-to-chunk-spec-list slot-specs)))



(defun chunk-spec-to-chunk-def (chunk-spec)
  "Convert a chunk-spec to a chunk definition list ignoring request parameters"
  (if (act-r-chunk-spec-p chunk-spec)
      (let ((requests (lognot (act-r-chunk-spec-request-param-slots chunk-spec)))) ;; don't consider request parameters
        (cond ((act-r-chunk-spec-slot-vars chunk-spec)
               (print-warning "Chunk-spec has variablized slots in a call to chunk-spec-to-chunk-def."))
              ((act-r-chunk-spec-variables chunk-spec)
               (print-warning "Chunk-spec has variables in the values in a call to chunk-spec-to-chunk-def."))
              ((not (zerop (logand requests (act-r-chunk-spec-duplicate-slots chunk-spec))))
               (print-warning "Chunk-spec may only specify a slot once in a call to chunk-spec-to-chunk-def."))
              ((or (not (zerop (logand requests (act-r-chunk-spec-negated-slots chunk-spec))))
                   (not (zerop (logand requests (act-r-chunk-spec-relative-slots chunk-spec)))))
               (print-warning "Chunk-spec may only use the = modifier in a call to chunk-spec-to-chunk-def."))
              (t
               (aif (mapcan (lambda (x) 
                              (unless (keywordp (act-r-slot-spec-name x))
                                (list (act-r-slot-spec-name x) (act-r-slot-spec-value x))))
                      (act-r-chunk-spec-slots chunk-spec))
                    it
                    (list 'isa 'chunk)))))
    (print-warning "Chunk-spec-to-chunk-def called with something other than a chunk-spec.")))


(defun merge-chunk-specs (spec1 spec2)
  (if (and (act-r-chunk-spec-p spec1) (act-r-chunk-spec-p spec2))
      (let ((new-spec (make-act-r-chunk-spec)))
        (dolist (slot (act-r-chunk-spec-slots spec1))
          (add-slot-spec-to-chunk-spec slot new-spec))
        (dolist (slot (act-r-chunk-spec-slots spec2))
          (add-slot-spec-to-chunk-spec slot new-spec))
        new-spec)
    (print-warning "merge-chunk-specs requires two valid chunk-specs")))
        

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
