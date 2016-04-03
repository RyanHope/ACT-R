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
;;; Filename    : model.lisp
;;; Version     : 3.0
;;; 
;;; Description : Functions that support the abstraction of a model
;;; 
;;; Bugs        : 
;;;
;;; To do       : Finish the documentation.
;;; 
;;; ----- History -----
;;;
;;; 2004.20.08 Dan
;;;             : Creation
;;; 2005.01.12 Dan
;;;             : Don't need to special case the device because it's now an
;;;             : actual module.
;;; 2005.02.11 Dan
;;;             : * Changed some reset-model to use clrhash instead of 
;;;             :   creating new tables.
;;; 2005.02.28 Dan
;;;             : * Made the with-model macro hygienic.
;;; 2005.03.23 Dan
;;;             : * Update the model reset and creation to use the two reset 
;;;             :   functions that are now part of the module definition - one 
;;;             :   before the parameter are reset and one after.
;;; 2006.07.05 Dan
;;;             : * Fixed a bug in the delete-model-fct function in the format 
;;;             :   command for printing that there was no model.
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in delete-model which could result in the deleted
;;;             :   model being left as the current model after deletion.
;;; 2007.04.19 Dan
;;;             : * Fixed another bug in delete-model which left the current-model
;;;             :   set even when the last model currently defined was deleted.
;;; 2008.08.21 Dan
;;;             : * Added a new with-model-eval macro that sits half way between
;;;             :   the current macro and the -fct.  This macro does evaluate the
;;;             :   first parameter to get the name, but splices in the body
;;;             :   to evaluate.  Should be more convenient in many circumstances.
;;; 2008.10.22 Dan [1.0]
;;;             : * Updated the reset function to handle the new chunk-ref table
;;;             :   and setting its flags to default values.
;;;             : * Finally took the a1 off the version.
;;; 2008.12.01 Dan
;;;             : * Added the code to call a third module reset function after
;;;             :   the user code has been evaled.
;;; 2008.12.08 Dan
;;;             : * Added new-chunk-type-size and largest-chunk-type-size to
;;;             :   record and get the largest possible chunk size.
;;; 2008.12.15 Dan
;;;             : * Added the code to call the third reset function when the
;;;             :   model is initially created too.
;;; 2009.09.09 Dan
;;;             : * Updated reset-model to also clear the chunk-set from all of
;;;             :   the multi-buffers.
;;; 2009.10.08 Dan
;;;             : * Updated define-model to clear the chunk-set as well since
;;;             :   reset doesn't happen at initial definition time.
;;; 2009.12.03 Dan
;;;             : * Delete-model needs to take events out of the meta-process's 
;;;             :   dynamics list as well.
;;; 2010.08.17 Dan
;;;             : * Better cleanup in define-model when there's an error during
;;;             :   the definition code.
;;; 2010.11.16 Dan
;;;             : * Added the :mcts and :mctrt system parameters for configuring
;;;             :   the model's chunk table because those can speed things up 
;;;             :   significantly in some circumstances (for example ACL works
;;;             :   better with :mctrt of .6 and :mcts set large if there are
;;;             :   going to be a lot of chunks created).
;;; 2011.11.11 Dan
;;;             : * Added a warning about model names that could cause problems
;;;             :   for the environment.
;;; 2012.02.06 Dan
;;;             : * Added unsafe-define-model for my debugging purposes because
;;;             :   sometimes it's easier to fix things if the errors are unhandled.
;;; 2012.08.08 Dan
;;;             : * Record the models as they are defined in meta-p-model-order
;;;             :   so things like reset can occur in the same order as when
;;;             :   initially created for consistency.
;;; 2013.01.04 Dan
;;;             : * Added unwind-protect to reset- and delete- model so that the
;;;             :   current model always gets restored (define model doesn't need
;;;             :   it since it has other protection and doesn't just restore the
;;;             :   "last" model anyway).
;;;             : * Added a check to define-model to prevent it from creating a
;;;             :   new model when it's not valid to do so based on meta-p-cannot-define-model
;;;             :   and added the cannot-define-model calls to places that need
;;;             :   to be protected.
;;; 2013.01.07 Dan
;;;             : * Changed the test on cannot-define-model to be >0 instead of
;;;             :   just true.
;;; 2014.02.24 Dan [2.0]
;;;             : * Don't bother creating the chunk-type chunk since it's 
;;;             :   automatic now.
;;;             : * Largest chunk-type size is total slot count (not the most
;;;             :   efficient method, but go with it for now).
;;; 2014.05.16 Dan
;;;             : * Added more default chunk types: constant-chunk and clear.
;;;             :   constant-chunk specifies a name slot which can then be used
;;;             :   to prevent chunks from merging to avoid oddities like free
;;;             :   and busy merging!  Clear has one slot named clear with a
;;;             :   default value of t and it can be used by modules that need
;;;             :   a "clear" request without having to define it in the module.
;;;             : * All the default chunks now use the name slot to prevent 
;;;             :   possible merging.  The chunk-name is set in the slot to
;;;             :   avoid that.  It does change the fan of those items relative
;;;             :   to the older ACT-R versions if they're used in slots of DM
;;;             :   chunks and spreading activation is on, but the fan of those
;;;             :   things shouldn't have been important anyway.
;;; 2014.06.25 Dan
;;;             : * Change the definition for the type chunk since subtype info
;;;             :   isn't kept anymore.
;;;             : * Set the act-r-chunk-type-info-types list to (chunks) on a
;;;             :   reset.
;;; 2014.09.26 Dan
;;;             : * Instead of maphashing over global-modules-table dolist over
;;;             :   all-module-names because that guarantees ordering.
;;; 2014.11.07 Dan
;;;             : * Adding another default chunk-type to specify the default
;;;             :   query slots as valid: (chunk-type query-slots state buffer error).
;;; 2015.03.19 Dan
;;;             : * Resetting a model now needs to clear the buffer flags as well. 
;;;             : * Failure needs to be the name of a default chunk.
;;; 2015.09.09 Dan [3.0]
;;;             : * Model now keeps track of requests made to modules when asked
;;;             :   and can report on whether or not a request has "completed".
;;; 2015.12.16 Dan
;;;             : * All of the "completing" functions now return t as long as
;;;             :   there is a model and meta-process regardless of whether or 
;;;             :   not the params matched something previously uncompleted.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Model structure is not for use outside of the framework.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; define-model
;;; 
;;; (defmacro define-model (name &body model-code))
;;; (defun define-model-fct (name model-code-list))
;;; 
;;; name a symbol that will be the name for the new model
;;; model-code any number of forms that will be evaluated for the model
;;; model-code-list a list of forms to evaluate for the model
;;; 
;;; define-model is used to create a new model in the current meta-process.
;;; 
;;; The name must not already be used for a model in the current meta-process. If the name is not a symbol or is already used to name a model in the current meta-process a warning will be displayed and the model will not be defined (the old model with that name will remain unchanged if one existed).
;;; 
;;; When a model is first defined the following sequence of events will occur:
;;; 
;;; - Create a new model with that name
;;; - with that new model as the current model
;;;    - create the default chunk-types 
;;;    - create the default chunks
;;;    - create a new instance of each module 
;;;        - call its create function if it exists
;;;        - call its reset function if it exists
;;;    - evaluate the forms of the model in the order provided
;;;   
;;; If a model is successfully created then its name is returned otherwise define-model returns nil.
;;; 
;;; Every model will need to have a call to define-model before issuing any of the model commands because there is no default model in a meta-process.  However, if one is working with only a single model then all that is necessary is to provide a name - it is not necessary to enclose all of the model code.
;;; 
;;; current-model
;;; 
;;; (defun current-model ())
;;; 
;;; current-model returns the name of the current model in the current meta-process or nil if there is no current model or no current meta-process.
;;; 
;;; delete-model
;;; 
;;; (defmacro delete-model (&optional model-name))
;;; (defun delete-model-fct (&optional model-name))
;;; 
;;; model-name a symbol that names a model
;;; 
;;; If model-name is not provided the name of the current-model is used.  
;;; 
;;; If model-name is the name of a model in the current meta-process then the following sequence of events will occur:
;;; 
;;;  - the model with that name is set to the current model
;;;  - all events generated by that model are removed from the event queue
;;;  - each module of the model is deleted
;;;  - the model is removed from the set of models in the current meta-process
;;; 
;;; If model-name is valid then t is returned.
;;; 
;;; If model-name is not valid or there is no current meta-process then a warning is printed, nothing is done and nil is returned.
;;; 
;;; with-model
;;; 
;;; (defmacro with-model (model-name &body body))
;;; (defun with-model-fct (model-name forms-list))
;;; 
;;; model-name a symbol that names a model in the current meta-process
;;; body any number of forms to execute
;;; forms-list a list of forms to execute
;;; 
;;; If model-name is the name of a model in the current meta-process then the forms are evaluated in order with the current model set to the one named by model-name.  The value of the last form evaluated is returned.
;;; 
;;; If model-name does not name a model in the current meta-process, or there is no current meta-process then none of the forms are evaluated, a warning is printed and nil is returned.
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


(defun current-model ()
  (when (current-model-struct)
    (act-r-model-name (current-model-struct))))

(defun largest-chunk-type-size ()
  (act-r-chunk-type-info-size (act-r-model-chunk-types-info (current-model-struct))))


(defvar *model-chunk-table-size* nil)
(defvar *model-chunk-table-rehash-threshold* nil)

(create-system-parameter :mcts :valid-test 'posnumornil :default-value nil :warning "positive number or nil"
                         :documentation "initial size of a model's chunk table" 
                         :handler (simple-system-param-handler *model-chunk-table-size*))

(create-system-parameter :mctrt :valid-test (lambda (x) (typep x '(or null (real 0 1)))) :default-value nil 
                                             :warning "a real number [0-1] or nil"
                         :documentation "rehash-threshold of a model's chunk table" 
                         :handler (simple-system-param-handler *model-chunk-table-rehash-threshold*))



(defmacro define-model (name &body model-code)
  `(define-model-fct ',name ',model-code))

(defun define-model-fct (name model-code-list)
  (verify-current-mp  
   "define-model called with no current meta-process."
   (cond ((not (symbolp name))
          (print-warning "Model name must be a symbol, ~S is not valid.  No model defined." name))
         ((null name)
          (print-warning "Nil is not a valid name for a model.  No model defined."))
         ((valid-model-name name)
          (print-warning "~S is already the name of a model in the current meta-process.  Cannot be redefined." name))
         ((> (meta-p-cannot-define-model (current-mp)) 0)
          (print-warning "Cannot define a model within the context of another model."))
         (t
          (cannot-define-model
           (when (some (lambda (x) (find x (symbol-name name))) (list #\$ #\{ #\} #\[ #\]))
             (print-warning "Model names that contain any of the characters $, {, }, [, or ] may not work correctly with the ACT-R Environment."))
           
           (let ((new-model (make-act-r-model :name name))
                 (mp (current-mp)))
             
             (when (or *model-chunk-table-size* *model-chunk-table-rehash-threshold*)
               (if *model-chunk-table-size*
                   (if *model-chunk-table-rehash-threshold*
                       (progn
                         (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*))
                         (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*)))
                     (progn
                       (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size*))
                       (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size*))))
                 (progn
                   (setf (act-r-model-chunks-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*))
                   (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*)))))
             
             
             (setf (gethash name (meta-p-models mp)) new-model)
             (push-last name (meta-p-model-order mp))
             (setf (meta-p-current-model mp) new-model)
             (incf (meta-p-model-count mp))
            
             
             ;(setf (act-r-model-device new-model) 
             ;  (make-instance 'device-interface))
            
             (when (> (length (format nil "~S" name)) (meta-p-model-name-len mp))
               (setf (meta-p-model-name-len mp) (length (format nil "~S" name))))
             
             (create-model-default-chunk-types-and-chunks new-model)
             
             (dolist (module-name (all-module-names))
               (setf (gethash module-name (act-r-model-modules-table new-model))
                 (instantiate-module module-name name)))
             
             ;; instantiate the buffers
             
             (maphash (lambda (buffer-name buffer-struct)
                        (let ((buffer (copy-act-r-buffer buffer-struct)))
                          
                          (when (act-r-buffer-multi buffer)
                            (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5)))
                          
                          (dolist (x (act-r-buffer-requests buffer))
                            (add-request-parameter x)
                            (setf (act-r-buffer-requests-mask buffer) (logior (slot-name->mask x) (act-r-buffer-requests-mask buffer))))
                          
                          (dolist (x (act-r-buffer-queries buffer))
                            (add-buffer-query x))
                          
                          (setf (gethash buffer-name (act-r-model-buffers new-model)) buffer)))
                      *buffers-table*)
             
             ;; clear the tracked-requests
             
             (setf (act-r-model-tracked-requests new-model) nil)
             
             (dolist (module-name (all-module-names))
               (reset-module module-name))
             
             
             (maphash (lambda (parameter-name parameter)
                        (sgp-fct (list parameter-name (act-r-parameter-default parameter))))
                      *act-r-parameters-table*)
             
             
             (dolist (module-name (all-module-names))
               (secondary-reset-module module-name))
             
             (let ((errored nil))
               (dolist (form model-code-list)
                 (unwind-protect 
                     (handler-case (eval form)
                       (error (condition) 
                         (setf errored t)
                         (print-warning "Error encountered in model form:~%~S~%Invoking the debugger." form)
                         (print-warning "You must exit the error state to continue.")
                         (invoke-debugger condition)))
                   (when errored
                     (remhash name (meta-p-models mp))
                     (setf (meta-p-model-order mp) (remove name (meta-p-model-order mp)))
                     (print-warning "Model ~s not defined." name)
                     (decf (meta-p-model-count mp))
                     
                     ;; delete any events that may have been scheduled by modules
                     ;; or code prior to the error
                     
                     (setf (meta-p-events mp)
                       (remove name (meta-p-events mp) :key #'evt-model))
                     
                     (setf (meta-p-delayed mp)
                       (remove name (meta-p-delayed mp) :key #'evt-model))
                     
                     (setf (meta-p-dynamics mp)
                       (remove name (meta-p-dynamics mp) :key #'(lambda (x) (evt-model (car x)))))
                     
                     
                     ;; remove the modules which were created
                     
                     (dolist (module-name (all-module-names))
                       (delete-module module-name))
                     
                     (return-from define-model-fct nil)))))
             
             (setf (act-r-model-code new-model) model-code-list)
             
             (dolist (module-name (all-module-names))
               (tertiary-reset-module module-name))
             
             (unless (= 1 (meta-p-model-count mp))
               (setf (meta-p-current-model mp) nil))
             
             name))))))
  
(defmacro unsafe-define-model (name &body model-code)
  `(unsafe-define-model-fct ',name ',model-code))

(defun unsafe-define-model-fct (name model-code-list)
  (verify-current-mp  
   "define-model called with no current meta-process."
   (cond ((not (symbolp name))
          (print-warning "Model name must be a symbol, ~S is not valid.  No model defined." name))
         ((null name)
          (print-warning "Nil is not a valid name for a model.  No model defined."))
         ((valid-model-name name)
          (print-warning "~S is already the name of a model in the current meta-process.  Cannot be redefined." name))
         ((> (meta-p-cannot-define-model (current-mp)) 0)
          (print-warning "Cannot define a model within the context of another model."))
         (t
          (cannot-define-model
           (when (some (lambda (x) (find x (symbol-name name))) (list #\$ #\{ #\} #\[ #\]))
             (print-warning "Model names that contain any of the characters $, {, }, [, or ] may not work correctly with the ACT-R Environment."))
           
           (let ((new-model (make-act-r-model :name name))
                 (mp (current-mp)))
             
             (when (or *model-chunk-table-size* *model-chunk-table-rehash-threshold*)
               (if *model-chunk-table-size*
                   (if *model-chunk-table-rehash-threshold*
                       (progn
                         (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*))
                         (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*)))
                     (progn
                       (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size*))
                       (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size*))))
                 (progn
                   (setf (act-r-model-chunks-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*))
                   (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*)))))
             
             
             (setf (gethash name (meta-p-models mp)) new-model)
             (push-last name (meta-p-model-order mp))
             (setf (meta-p-current-model mp) new-model)
             (incf (meta-p-model-count mp))
             
             
             ;(setf (act-r-model-device new-model) 
             ;  (make-instance 'device-interface))
             
             (when (> (length (format nil "~S" name)) (meta-p-model-name-len mp))
               (setf (meta-p-model-name-len mp) (length (format nil "~S" name))))
             
             (create-model-default-chunk-types-and-chunks new-model)
             
             (dolist (module-name (all-module-names))
               (setf (gethash module-name (act-r-model-modules-table new-model))
                 (instantiate-module module-name name)))
             
             ;; instantiate the buffers
             
             (maphash (lambda (buffer-name buffer-struct)
                        (let ((buffer (copy-act-r-buffer buffer-struct)))
                          
                          (when (act-r-buffer-multi buffer)
                            (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5)))
                          
                          (dolist (x (act-r-buffer-requests buffer))
                            (add-request-parameter x)
                            (setf (act-r-buffer-requests-mask buffer) (logior (slot-name->mask x) (act-r-buffer-requests-mask buffer))))
                          
                          (dolist (x (act-r-buffer-queries buffer))
                            (add-buffer-query x))
                          
                          (setf (gethash buffer-name (act-r-model-buffers new-model)) buffer)))
                      *buffers-table*)
             
             ;; clear the tracked-requests
             
             (setf (act-r-model-tracked-requests new-model) nil)
             
             (dolist (module-name (all-module-names))
               (reset-module module-name))             
             
             (maphash (lambda (parameter-name parameter)
                        (sgp-fct (list parameter-name (act-r-parameter-default parameter))))
                      *act-r-parameters-table*)
             
             
             (dolist (module-name (all-module-names))
               (secondary-reset-module module-name))
             
             (dolist (form model-code-list)
               (eval form))
             
             (setf (act-r-model-code new-model) model-code-list)
             
             (dolist (module-name (all-module-names))
               (tertiary-reset-module module-name))
             
             (unless (= 1 (meta-p-model-count mp))
               (setf (meta-p-current-model mp) nil))
             
             name))))))
  
  
(defun create-model-default-chunk-types-and-chunks (model)
  (let ((info (act-r-model-chunk-types-info model))
        (c (make-act-r-chunk-type :name 'chunk :super-types (list 'chunk))))
    
    (setf (gethash 'chunk (act-r-chunk-type-info-table info)) c)
    (setf (gethash 0 (act-r-chunk-type-info-distinct-types info)) (list (cons nil (list 'chunk))))
    (setf (act-r-chunk-type-info-types info) (list 'chunk)))
  
  (chunk-type constant-chunks name)
  (chunk-type clear (clear t))
  (chunk-type query-slots state buffer error)
  
  (dolist (x '(free busy error empty full failure requested unrequested))
    (define-chunks-fct `((,x name ,x)))
    (make-chunk-immutable x))
  
  (define-chunks (clear isa clear))
  (make-chunk-immutable 'clear))


(defmacro delete-model (&optional (model-name nil provided))
  `(if ,provided
       (delete-model-fct ',model-name)
     (delete-model-fct (current-model))))
  
(defun delete-model-fct (model-name)
  (verify-current-mp  
   "delete-model called with no current meta-process.~%No model deleted."
   (let ((mp (current-mp)))
     (if model-name
         (if (gethash model-name (meta-p-models mp))
             (cannot-define-model
              (let ((model (gethash model-name (meta-p-models mp)))
                    (saved-current (meta-p-current-model mp)))
                (setf (meta-p-current-model mp) model)
                
                (setf (meta-p-events mp)
                  (remove model-name (meta-p-events mp) :key #'evt-model))
                
                (setf (meta-p-delayed mp)
                  (remove model-name (meta-p-delayed mp) :key #'evt-model))
                
                (setf (meta-p-dynamics mp)
                  (remove model-name (meta-p-dynamics mp) :key #'(lambda (x) (evt-model (car x)))))
                
                (unwind-protect 
                    (dolist (module-name (all-module-names))
                      (delete-module module-name))
                  
                  (progn
                    (decf (meta-p-model-count mp))
                    (remhash model-name (meta-p-models mp))
                    (setf (meta-p-model-order mp) (remove model-name (meta-p-model-order mp)))
                    (cond ((zerop (meta-p-model-count mp))
                           (setf (meta-p-current-model mp) nil))
                          ((= 1 (meta-p-model-count mp))
                           (setf (meta-p-current-model mp)
                             (gethash (car (hash-table-keys (meta-p-models mp))) (meta-p-models mp))))
                          (t (setf (meta-p-current-model mp) saved-current)))))
                
                t))
           (print-warning "No model named ~S in current meta-process." model-name))
       (print-warning "No current model to delete.")))))

(defmacro with-model (model-name &body body)
  (let ((mp (gensym))
        (previous-model (gensym)))
    `(let ((,mp (current-mp)))
     (if ,mp
         (if (valid-model-name ',model-name)
             (let ((,previous-model (current-model-struct)))
               (setf (meta-p-current-model (current-mp)) 
                 (gethash ',model-name (meta-p-models ,mp)))
               (cannot-define-model
                (unwind-protect (progn ,@body)
                  (setf (meta-p-current-model (current-mp)) ,previous-model))))
           (print-warning "~S does not name a model in the current meta-process" ',model-name))
       (print-warning "No actions taken in with-model because there is no current meta-process")))))

(defmacro with-model-eval (model-name &body body)
  (let ((mp (gensym))
        (previous-model (gensym))
        (model (gensym)))
    `(let ((,mp (current-mp)))
       (if ,mp
           (let ((,model ,model-name)) 
             (if (valid-model-name ,model)
                 (let ((,previous-model (current-model-struct)))
                   (setf (meta-p-current-model (current-mp)) 
                     (gethash ,model (meta-p-models ,mp)))
                   (cannot-define-model
                    (unwind-protect (progn ,@body)
                      (setf (meta-p-current-model (current-mp)) ,previous-model))))
               (print-warning "~S does not name a model in the current meta-process" ,model)))
         (print-warning "No actions taken in with-model because there is no current meta-process")))))

(defun with-model-fct (model-name forms-list)
  (let ((mp (current-mp)))
     (if mp
         (if (valid-model-name model-name)
             (let ((previous-model (current-model-struct))
                   (val nil))
               (setf (meta-p-current-model (current-mp)) 
                 (gethash model-name (meta-p-models mp)))
               (cannot-define-model
                (unwind-protect (dolist (x forms-list val)
                                 (setf val (eval x)))
                  (setf (meta-p-current-model (current-mp)) previous-model))))
           (print-warning "~S does not name a model in the current meta-process" model-name))
       (print-warning "No actions taken in with-model because there is no current meta-process"))))


(defun valid-model-name (name)
    "Returns t if name is the name of a model in the current meta-process - there must be a current mp"
  (if (gethash name (meta-p-models (current-mp)))
      t
    nil))

(defun reset-model (mp model)
 
  (let ((previous-model (meta-p-current-model mp)))
    (setf (meta-p-current-model mp) model)
    
    (cannot-define-model
     (unwind-protect
         (let ((info (act-r-model-chunk-types-info model)))
           ;; erase chunk-type info 
           (clrhash (act-r-chunk-type-info-slot->index info))
           (setf (act-r-chunk-type-info-index->slot info) (make-array (list 0) :adjustable t :fill-pointer t))
           (clrhash (act-r-chunk-type-info-slot->mask info))
           (setf (act-r-chunk-type-info-size info) 0)
           (clrhash (act-r-chunk-type-info-distinct-types info))
           (setf (act-r-chunk-type-info-extended-slots info) nil)
           (clrhash (act-r-chunk-type-info-table info))
           
           (clrhash (act-r-model-chunks-table model))
           (clrhash (act-r-model-chunk-ref-table model))
           
           (setf (act-r-model-chunk-update model) t)
           (setf (act-r-model-dynamic-update model) t)
           (setf (act-r-model-delete-chunks model) nil)
           
           (setf (act-r-model-tracked-requests model) nil)
           
           (create-model-default-chunk-types-and-chunks model)
           
           (maphash (lambda (buffer-name buffer)
                      (declare (ignore buffer-name))
                      (setf (act-r-buffer-chunk buffer) nil)
                      (when (act-r-buffer-multi buffer)
                        (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5)))
                      (dolist (x (act-r-buffer-requests buffer))
                        (add-request-parameter x)
                        (setf (act-r-buffer-requests-mask buffer) (logior (slot-name->mask x) (act-r-buffer-requests-mask buffer))))
                      (dolist (x (act-r-buffer-queries buffer))
                        (add-buffer-query x))
                      ;; clear any flags
                      (setf (act-r-buffer-flags buffer) nil))
                    (act-r-model-buffers model))
                      
           (dolist (module-name (all-module-names))
             (reset-module module-name))
           
           (maphash (lambda (parameter-name parameter)
                      (sgp-fct (list parameter-name (act-r-parameter-default parameter))))
                    *act-r-parameters-table*)
           
           
           (dolist (module-name (all-module-names))
             (secondary-reset-module module-name))
           
           (dolist (form (act-r-model-code model))
             (eval form))
           
           (dolist (module-name (all-module-names))
             (tertiary-reset-module module-name)))
       
       (setf (meta-p-current-model mp) previous-model)))))

;;; Functions for tracking and checking on requests

(defstruct request-tracking buffer module spec)

;; this is internal so ignore extra checking

(defun track-request (buffer-instance spec)
  (when (act-r-buffer-trackable buffer-instance)
    (push (make-request-tracking :buffer (act-r-buffer-name buffer-instance)
                                 :module (act-r-buffer-module buffer-instance)
                                 :spec spec)
          (act-r-model-tracked-requests (current-model-struct)))))

;; these are all available to module/user code so protect accordingly

(defun request-completed-p (spec)
  (verify-current-mp  
   "request-completed-p called with no current meta-process."
   (verify-current-model
    "request-completed-p called with no current model."
    (unless (find spec (act-r-model-tracked-requests (current-model-struct)) :key 'request-tracking-spec :test 'eq)
      t))))

(defun complete-request (spec)
  (verify-current-mp  
   "complete-request called with no current meta-process."
   (verify-current-model
    "complete-request called with no current model."
    (let ((cm (current-model-struct)))
      (setf (act-r-model-tracked-requests cm)
        (remove spec (act-r-model-tracked-requests cm) :key 'request-tracking-spec :test 'eq))
      t))))

(defun complete-all-buffer-requests (buffer-name)
  (verify-current-mp  
   "complete-all-buffer-requests called with no current meta-process."
   (verify-current-model
    "complete-all-buffer-requests called with no current model."
    (let ((cm (current-model-struct)))
      (setf (act-r-model-tracked-requests cm)
        (remove buffer-name (act-r-model-tracked-requests cm) :key 'request-tracking-buffer :test 'eq))
      t))))

(defun complete-all-module-requests (module-name)
  (verify-current-mp  
   "complete-all-module-requests called with no current meta-process."
   (verify-current-model
    "complete-all-module-requests called with no current model."
    (let ((cm (current-model-struct)))
      (setf (act-r-model-tracked-requests cm)
        (remove module-name (act-r-model-tracked-requests cm) :key 'request-tracking-module :test 'eq))
      t))))



  

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
