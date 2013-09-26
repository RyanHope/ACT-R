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
;;; Version     : 1.1
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
;;; Saving both the super and sub type information in the chunk type structure
;;; for potential use in the matching or elsewhere, but may not need both when
;;; all is done.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(declaim (ftype (function (t) t) new-chunk-type-size))
(declaim (ftype (function (t) t) get-module-fct))
(declaim (ftype (function () t) current-model))
(declaim (ftype (function (t) t) get-chunk))
(declaim (ftype (function (&optional t) t) new-name-fct))


(defvar *default-static-type-setting* nil)

#|
(create-system-parameter :static-default :valid-test 'tornil :default-value nil 
                         :warning "T or nil"
                         :documentation "Whether or not all chunk-types are static by default" 
                         :handler (simple-system-param-handler *default-static-type-setting*))
|#

(defvar *static-type-naming-hook* nil)

(create-system-parameter :new-static-type-name :valid-test 'fctornil :default-value nil 
                         :warning "a function or nil"
                         :documentation "Function that is called to get the name when a static type is extended with a new slot"
                         :handler (simple-system-param-handler *static-type-naming-hook*))


(defvar *show-static-names* nil)

(create-system-parameter :show-static-subtype-names :valid-test 'tornil :default-value nil 
                         :warning "T or nil"
                         :documentation "Whether chunks of a static type show their true name or just the root name"
                         :handler (simple-system-param-handler *show-static-names*))

(defun get-chunk-type (name)
  "Internal command to get a chunk-type structure from its name"
  (verify-current-mp  
   "get-chunk-type called with no current meta-process."
   (verify-current-model
    "get-chunk-type called with no current model."
    (gethash name (act-r-model-chunk-types-table (current-model-struct))))))            

(defmacro chunk-type (&rest name-and-slots)
  "The user macro to define a new chunk-type."
  `(chunk-type-fct ',name-and-slots))


(defun list-to-power-set (list)
  (if (null list)
      (list nil)
    (let ((sub (list-to-power-set (cdr list))))
      (append (mapcar (lambda (x) (cons (car list) x)) sub)
              sub))))

(defun new-static-chunk-type-name (root-name new-slots)
  (let ((possible-name 
         (aif (and *static-type-naming-hook* (funcall *static-type-naming-hook* root-name new-slots))
              it
              (intern (format nil "~@:(~s+~{~s~^&~}~)" root-name (sort (copy-list new-slots) 'string< :key 'symbol-name))))))
    
    (if (chunk-type-p-fct possible-name)
        (new-name-fct possible-name)
      possible-name)))


(defun static-chunk-type-slot-key (new-slots root-type)
  (sort (copy-list new-slots) #'< :key (lambda (x) (position x (act-r-chunk-type-indices root-type) :test 'eq))))

(defun static-chunk-sub-type-exists (slot-list root-type)
  (let ((new-slots (set-difference slot-list (mapcar 'chunk-type-slot-name (act-r-chunk-type-slots root-type)))))
    (when (every (lambda (x) (find x (act-r-chunk-type-indices root-type))) new-slots)
      (gethash (static-chunk-type-slot-key new-slots root-type) (act-r-chunk-type-subtree root-type)))))


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
                  (super-type nil)
                  (static *default-static-type-setting*))
             
             (when (get-chunk-type name)
               (return-from chunk-type-fct 
                 (print-warning "Chunk-type ~S is already defined and redefinition is not allowed." name)))
             
             ; check type hierarchy
             (when modifier
               (if (> (length modifier) 1)
                   (return-from chunk-type-fct 
                     (print-warning "Too many options specified for chunk-type ~S. NO chunk-type created." name))
                 (case (caar modifier)
                   (:include
                    (if (and (= (length (car modifier)) 2) (get-chunk-type (cadar modifier)))
                        (progn
                          (setf super-type (get-chunk-type (cadar modifier)))
                          (setf static (act-r-chunk-type-static super-type)))
                      (return-from chunk-type-fct 
                        (print-warning "Unknown supertype ~S specified for type ~S." (cadar modifier) name))))
                   (:static
                    (if (and (= (length (car modifier)) 2) (tornil (cadar modifier)))
                        (setf static (when (cadar modifier) name))
                      (return-from chunk-type-fct 
                        (print-warning "Unknown static value ~S specified for type ~S." (cadar modifier) name))))
                   (t
                    (return-from chunk-type-fct 
                      (print-warning "Unknown option ~S specified for type ~S." (car modifier) name))))))
             
             ;; Static by default
             (when (eq t static)
               (setf static name))
             
             (dolist (slot slots)
               (unless (or (and (atom slot) (symbolp slot) (not (keywordp slot))
                                (not (eq slot 'isa)))
                           (and (listp slot)
                                (= (length slot) 2)
                                (not (eq (car slot) 'isa))
                                (symbolp (car slot))
                                (not (keywordp (car slot)))))
                 (return-from chunk-type-fct 
                   (print-warning "Unacceptable slot specification ~S for chunk-type ~S.  Chunk-type not created." slot name))))
             
             (unless (= (length slots) (length (remove-duplicates slots)))
               (return-from chunk-type-fct 
                 (print-warning "Duplicate slot specifications in ~S for chunk-type ~S.  Chunk-type not created." slots name)))
             
             (if static
                 (let ((ct (make-act-r-chunk-type 
                            :name name 
                            :documentation documentation
                            :static static
                            :subtree (when (eq static name) (make-hash-table :test 'equalp))
                            :subtypes (list name)
                            :supertypes (list name)
                            :indices (if super-type
                                         (act-r-chunk-type-indices super-type)
                                       (make-array (list 0) :adjustable t :fill-pointer t)))))
                   
                   (if (eq static name)
                       
                       ;; It's a fresh static type so basically the same as a normal type and I know
                       ;; there's no super-types so things can be created "simply"
                       (progn
                         
                         (dolist (slot slots)
                           (push-last slot (act-r-chunk-type-slots ct))

                           (let ((s (chunk-type-slot-name slot)))
                             (push s (act-r-chunk-type-possible-slots ct))
                             (vector-push-extend s (act-r-chunk-type-indices ct))))
                         
                         (new-chunk-type-size (length (act-r-chunk-type-indices ct)))
                         
                         ;; It's in the subtree with no extra slots
                         (setf (gethash nil (act-r-chunk-type-subtree ct)) name)
                         
                         (setf (gethash name (act-r-model-chunk-types-table (current-model-struct))) ct)
                         name)
                     
                     ;; It's a subtype of a static type so it needs to 
                     ;; essentially extend the parent for each of the new slots
                     ;; being added after checking to see if such a type already exists
                     
                     
                     (let* ((root-type (get-chunk-type static))
                            
                            (slot-names (remove-duplicates (mapcar 'chunk-type-slot-name (append slots (act-r-chunk-type-slots super-type)))))
                            (new-slots (set-difference slot-names (mapcar 'chunk-type-slot-name (act-r-chunk-type-slots root-type)))))
                       
                       
                       ;; If it's in the table punt                        
                       
                       (when (static-chunk-sub-type-exists slot-names root-type)
                         (return-from chunk-type-fct 
                           (print-warning "A static subtype already exists of the type ~s with the slots ~s.  Chunk-type ~s not created." static slot-names name)))
                       
                                              
                       ;; first step needs to be putting the slots on the index list
                       ;; so that the key can be created
                       
                       (dolist (s new-slots)
                         (unless (find s (act-r-chunk-type-indices ct) :test 'eq) 
                           (vector-push-extend s (act-r-chunk-type-indices ct))))
                       
                       ;; add it to the table
                       
                       (setf (gethash (static-chunk-type-slot-key new-slots root-type) (act-r-chunk-type-subtree root-type)) name)
                       
                       
                       ;; set the supertypes list based on all the possible parent types
                       ;; creating the parents as needed first
                       
                       (setf (act-r-chunk-type-supertypes ct) nil)
                       
                       (dolist (possible-parent (sort (list-to-power-set new-slots) #'< :key 'length))
                         (let ((parent (aif (static-chunk-sub-type-exists possible-parent root-type)
                                            it
                                            (chunk-type-fct `((,(new-static-chunk-type-name static possible-parent) (:include ,static)) ,@possible-parent)))))

                         (push parent (act-r-chunk-type-supertypes ct))))
                       
                       
                       ;; set the parents' subtypes
                       
                       (dolist (parent (cdr (act-r-chunk-type-supertypes ct)))
                         (push name (act-r-chunk-type-subtypes (get-chunk-type parent))))                                           
                                             
                       
                       ;; push the new slot names for this type onto all of the possible-slots lists 
               
                       (dolist (type-name (act-r-chunk-type-supertypes ct))
                         (let ((type (if (eq type-name name) ct (get-chunk-type type-name))))
                           (dolist (s slot-names)
                             (pushnew s (act-r-chunk-type-possible-slots type) :test 'eq))))
                       
                       
                       ;; make the check for new possible size
                       
                       (new-chunk-type-size (length (act-r-chunk-type-indices ct)))
               
                       ;; now add the parent slots to this type
                       ;; maintaining order for consistency
                       
                       
                       (dolist (parent-slot (act-r-chunk-type-slots (if super-type super-type root-type)))
                         (aif (find (chunk-type-slot-name parent-slot) slots :key 'chunk-type-slot-name :test 'eq)
                              (progn
                                (push-last it (act-r-chunk-type-slots ct))
                                (setf slots (remove it slots :test 'eq)))
                              (push-last parent-slot (act-r-chunk-type-slots ct))))
                       
                       ;; add the new slots
                       
                       (dolist (slot slots)
                         (push-last slot (act-r-chunk-type-slots ct)))
                       
                       (setf (gethash name (act-r-model-chunk-types-table (current-model-struct))) ct)
                       name)))
   
               ;; Normal chunk-type 
               (let ((ct (make-act-r-chunk-type 
                          :name name 
                          :documentation documentation
                          :subtypes (list name)
                          :supertypes 
                          (if super-type
                              (cons name (act-r-chunk-type-supertypes super-type))
                            (list name))
                          :indices (if super-type
                                       (act-r-chunk-type-indices super-type)
                                     (make-array (list 0) :adjustable t :fill-pointer t)))))
                 
                 ;; push the new slot names for this type onto all of the possible-slots lists 
                 
                 (dolist (type-name (act-r-chunk-type-supertypes ct))
                   (let ((type (if (eq type-name name) ct (get-chunk-type type-name))))
                     (dolist (s (mapcar 'chunk-type-slot-name slots))
                       (pushnew s (act-r-chunk-type-possible-slots type) :test 'eq))))
                 
                 
                 ;; Now add them to the index table which will affect all types in the "family"
                 
                 (dolist (s (mapcar 'chunk-type-slot-name slots))
                   (unless (find s (act-r-chunk-type-indices ct) :test 'eq) 
                     (vector-push-extend s (act-r-chunk-type-indices ct))))
                 
                 ;; pass that along to check for possible size
                 
                 (new-chunk-type-size (length (act-r-chunk-type-indices ct)))
                 
                 ;; now add the parent slots to this type
                 ;; maintaining order for consistency
                 
                 (when super-type
                   (dolist (parent-slot (act-r-chunk-type-slots super-type))
                     (aif (find (chunk-type-slot-name parent-slot) slots :key 'chunk-type-slot-name :test 'eq)
                          (progn
                            (push-last it (act-r-chunk-type-slots ct))
                            (setf slots (remove it slots :test 'eq)))
                          (progn
                            (push-last parent-slot (act-r-chunk-type-slots ct))
                            (push-last (chunk-type-slot-name parent-slot) (act-r-chunk-type-possible-slots ct))))))
                 
                 
                 ;; add the new slots
                 
                 (dolist (slot slots)
                   (push-last slot (act-r-chunk-type-slots ct)))
                 
                 
                 ;; add this type as a subtype of all the parent types
                 
                 (when super-type
                   (dolist (parent (act-r-chunk-type-supertypes super-type))
                     (push name (act-r-chunk-type-subtypes (get-chunk-type parent)))))
                 
                 (setf (gethash name (act-r-model-chunk-types-table (current-model-struct))) ct)
                 name))))))))


(defun chunk-type-slot-name (slot)
  "Internal function for parsing chunk-types"
  (if (atom slot)
      slot
    (car slot)))

(defun print-all-chunk-types ()
  "Internal function for printing all chunk-types" 
  (let ((res nil))
    (maphash #'(lambda (name chunk-type)
                 (declare (ignore name))
                 (push (pprint-ct chunk-type) res))
             (act-r-model-chunk-types-table (current-model-struct)))
    (reverse res)))

(defconstant *pprint-chunk-type-string*
     (formatter "~S~@[ <- ~s~]~@[ ~S~]~%~{~{   ~s~@[ (~s)~]~%~}~}~%")
  "Internal compiled format string used to print out chunk-types")

(defun pprint-ct (chunk-type)
  "Pretty prints a chunk-type."
  (command-output  
   (format nil *pprint-chunk-type-string*
                  (act-r-chunk-type-name chunk-type)
     (if (act-r-chunk-type-static chunk-type)
         (unless (eq (act-r-chunk-type-name chunk-type) (act-r-chunk-type-static chunk-type))
           (act-r-chunk-type-static chunk-type))
       (second (act-r-chunk-type-supertypes chunk-type)))
     (act-r-chunk-type-documentation chunk-type)
     (mapcar #'(lambda (slot)
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

(defmacro chunk-type-subtype-p (chunk-subtype? chunk-supertype)
  "Predicate macro for testing that one chunk-type isa a subtype of another"
  `(chunk-type-subtype-p-fct ',chunk-subtype? ',chunk-supertype))

(defun chunk-type-subtype-p-fct (chunk-subtype? chunk-supertype)
  "Predicate function for testing that one chunk-type isa a subtype of another"
  (let ((ct (get-chunk-type chunk-subtype?)))
    (when ct 
      (find chunk-supertype (act-r-chunk-type-supertypes ct)))))


(defmacro chunk-type-supertypes (chunk-type-name)
  "Macro to return the list of supertypes for a given chunk-type"
  `(chunk-type-supertypes-fct ',chunk-type-name))

(defun chunk-type-supertypes-fct (chunk-type-name)
  "Function to return the list of supertypes for a given chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (act-r-chunk-type-supertypes ct))))


(defmacro chunk-type-subtypes (chunk-type-name)
  "Macro to return the list of subtypes for a given chunk-type"
  `(chunk-type-subtypes-fct ',chunk-type-name))

(defun chunk-type-subtypes-fct (chunk-type-name)
  "Function to return the list of subtypes for a given chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (act-r-chunk-type-subtypes ct))))


(defmacro chunk-type-slot-names (chunk-type-name)
  "Macro to return the list of valid slot names for a given chunk-type"
  `(chunk-type-slot-names-fct ',chunk-type-name))

(defun chunk-type-slot-names-fct (chunk-type-name)
  "Function to return the list of valid slot names for a given chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (if ct 
      (values (mapcar #'chunk-type-slot-name (act-r-chunk-type-slots ct)) t)
      (values nil nil))))

(defun ct-slot-names (chunk-type)
  "Internal function for parsing chunk-type structures"
  (mapcar #'chunk-type-slot-name (act-r-chunk-type-slots chunk-type)))


(defmacro chunk-type-slot-default (chunk-type-name slot-name)
  "Macro to return the default value for a slot in a chunk-type"
  `(chunk-type-slot-default-fct ',chunk-type-name ',slot-name))

(defun chunk-type-slot-default-fct (chunk-type-name slot-name)
  "Function to return the default value for a slot in a chunk-type"
    (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (let ((slot (find slot-name (act-r-chunk-type-slots ct) 
                        :key #'chunk-type-slot-name)))
        (when (listp slot)
          (second slot))))))


(defun ct-slot-default (chunk-type slot-name)
  "Internal function for parsing chunk-type structures"
  (let ((slot (find slot-name (act-r-chunk-type-slots chunk-type) 
                    :key #'chunk-type-slot-name)))
    (when (listp slot)
      (second slot))))

(defmacro chunk-type-slot-index (chunk-type-name slot-name)
  `(chunk-type-slot-index-fct ',chunk-type-name ',slot-name))

(defun chunk-type-slot-index-fct (chunk-type-name slot-name)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (position slot-name (act-r-chunk-type-indices ct) :test 'eq))))

(defmacro chunk-type-slot-name-from-index (chunk-type-name slot-index)
  `(chunk-type-slot-name-from-index-fct ',chunk-type-name ',slot-index))

(defun chunk-type-slot-name-from-index-fct (chunk-type-name slot-index)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when (and ct (< slot-index (length (act-r-chunk-type-indices ct))))
      (aref (act-r-chunk-type-indices ct) slot-index))))

(defmacro chunk-type-documentation (chunk-type-name)
  "Macro to return the documentation string for a chunk-type"
  `(chunk-type-documentation-fct ',chunk-type-name))

(defun chunk-type-documentation-fct (chunk-type-name)
  "Function to return the documentation string for a chunk-type"
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct 
      (act-r-chunk-type-documentation ct))))

(defun valid-slot-name (slot chunk-type)
  "Internal function for testing chunk-type structures"
  (find slot (act-r-chunk-type-slots chunk-type) :key #'chunk-type-slot-name :test 'eq))

(defun valid-chunk-type-slot (chunk-type-name slot)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct
      (valid-slot-name slot ct))))

(defun possible-slot-name (slot chunk-type)
  (find slot (act-r-chunk-type-possible-slots chunk-type) :test 'eq))

(defun possible-chunk-type-slot (chunk-type-name slot)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct
      (possible-slot-name slot ct))))

(defun extend-chunk-type-slots (chunk-type-name slot-name &optional chunk-name)
  (let ((ct (get-chunk-type chunk-type-name)))
    (if ct
        (cond ((null slot-name)
               (model-warning "Nil is not a valid slot name when trying to extend chunk-type ~s" chunk-type-name))
              ((not (symbolp slot-name))
               (model-warning "~s cannot be used as a slot name because it is not a symbol." slot-name))
              ((keywordp slot-name)
               (model-warning "~s cannot be used as a slot name because it is a keyword." slot-name))
              (t
               (unless (possible-slot-name slot-name ct)
                 (if (act-r-chunk-type-static ct)
                     
                     (let* ((root (act-r-chunk-type-static ct))
                            (root-type (get-chunk-type root))
                            (slot-names (cons slot-name (mapcar 'chunk-type-slot-name (act-r-chunk-type-slots ct))))
                            (new-slots (set-difference slot-names (mapcar 'chunk-type-slot-name (act-r-chunk-type-slots root-type)))))
                                              
                       ;; Just create a new type with the additional slot because that does all the work
                       
                       (let ((new-type (chunk-type-fct `((,(new-static-chunk-type-name root new-slots) (:include ,chunk-type-name)) ,@(act-r-chunk-type-slots (get-chunk-type chunk-type-name)) ,slot-name))))
                         
                         ;; If there's a chunk to change then just smash its type
                         
                         (when chunk-name
                           (let ((chunk (get-chunk chunk-name)))
                             (setf (act-r-chunk-chunk-type chunk) (get-chunk-type new-type))))
                         
                         slot-name))
               
                   ;; normal chunk-type extension which basically
                   ;; propagates the change to all chunks of the type
                   ;; and the current chunk doesn't matter
                   
                   (progn
                     (push-last slot-name (act-r-chunk-type-slots ct))
                     (push-last slot-name (act-r-chunk-type-extended-slots ct))
                     
                     ;; push the new slot onto all of the possible-slots lists 
                     
                     (dolist (type-name (act-r-chunk-type-supertypes ct))
                       (let ((type (get-chunk-type type-name)))
                         (unless (find slot-name (act-r-chunk-type-possible-slots type) :test 'eq)
                           (push-last slot-name (act-r-chunk-type-possible-slots type)))))
                     
                     ;; Now add it to the index table which will affect all types in the "family"
                     
                     (unless (find slot-name (act-r-chunk-type-indices ct) :test 'eq) 
                       (vector-push-extend slot-name (act-r-chunk-type-indices ct))
                       
                       ;; pass that along to check for possible max size change
                       (new-chunk-type-size (length (act-r-chunk-type-indices ct))))
                     
                     ;; extend all the children as well
                     ;; results in some duplicae 
                     (dolist (sub-type (remove chunk-type-name (act-r-chunk-type-subtypes ct)))
                       (extend-chunk-type-slots sub-type slot-name))
                     slot-name)))))
      
      (model-warning "~s does not name a chunk-type so it cannot be extended." chunk-type-name))))

(defun extended-slot-name-p (slot-name chunk-type-name)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct
      (find slot-name (act-r-chunk-type-extended-slots ct)))))


(defmacro chunk-type-static-p (chunk-type-name)
  `(chunk-type-static-p-fct ',chunk-type-name))

(defun chunk-type-static-p-fct (chunk-type-name)
  (let ((ct (get-chunk-type chunk-type-name)))
    (when ct
      (act-r-chunk-type-static ct))))


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
