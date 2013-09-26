;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : blending.lisp
;;; Version     : 1.0b3
;;; 
;;; Description : Base module code to handle blended retrieval requests.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider a drop out if all potential values for a slot
;;;             :     are the same.  The questions are a) is that really
;;;             :     valid in all cases or could there be some possible use
;;;             :     of m->v + v->m that would not want that b) would it really 
;;;             :     save anything in the big picture or is the cost of the
;;;             :     comparison itself in situtations where it's not needed
;;;             :     going to outweigh the potential savings?
;;; 
;;; ----- History -----
;;; 2008.09.12 Dan
;;;             : * Initial creation.
;;; 2011.01.12 Dan
;;;             : * Fixed a bug in the trace output code.
;;; 2011.01.26 Dan [1.0a2]
;;;             : * If every chunk in a blending set has the value of nil in a
;;;             :   slot just use nil and bypass all the chunk testing code 
;;;             :   since that will result in errors otherwise.  Could
;;;             :   probably apply that sort of shortcut in situations were
;;;             :   all the slots are the same regardless of the value, but
;;;             :   for now only treat nil as special.
;;;             : * Fixed a cut-and-paste error with blending-reqeuest because
;;;             :   it still had a reference to dm when deleting an ongoing
;;;             :   request.
;;; 2011.02.01 Dan [1.0b1]
;;;             : * Added the new parameter :blend-all-slots to allow one to
;;;             :   now have the specified slots also blended.  It defaults to
;;;             :   nil which is the previous behavior -- slots in the request
;;;             :   always have the requested value.
;;; 2011.03.09 Dan
;;;             : * Added new paramters to chunks - blended-activation and blended-time.
;;;             :   They work like retrieval-activation and retrieval-time do for regular
;;;             :   retrieval requests i.e. store the computed activation values during a
;;;             :   the blending request for later use if needed.
;;; 2011.04.01 Dan
;;;             : * Took the ignore buffer-name out of the query since it does
;;;             :   actually use it.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending chunks at initial load.
;;; 2011.06.20 Dan
;;;             : * Adding a request-parameter :ignore-slots which can be
;;;             :   specified as a list of slots which will not be blended thus 
;;;             :   in the resulting chunk they will be empty.
;;; 2011.06.23 Dan
;;;             : * Adding some hooks to the module like declarative has to make
;;;             :   it more flexible and possible to create inspection tools.
;;;             : * Added the :sblt parameter and code to save the blending 
;;;             :   trace for reporting later with print-blending-activation-trace.
;;; 2011.06.24 Dan [1.0b2]
;;;             : * Actually added the code to store and print the saved blending
;;;             :   trace.  The print-blending-trace function can be called after
;;;             :   a run with a time to print the saved trace from the blended
;;;             :   retrieval that started at that time (assuming the :sblt parameter
;;;             :   is true to save the data).
;;; 2011.07.12 Dan
;;;             : * Added a safety check into one of the exp calcualtions to avoid errors
;;;             :   with over/under flow but if it does over flow the results are 
;;;             :   probably "bad" and it prints out a warning.
;;;             : * Use chunks-similarity instead of similarity-fct since it already
;;;             :   has the declarative instance and getting that every time is 
;;;             :   expensive.
;;;             : [1.0b3]
;;;             : * Added a new parameter :min-bl which if set specifies a minimum
;;;             :   base-level activation a chunk has to have to be considered in
;;;             :   the matching-set.  Note that because it is only a test of base-
;;;             :   level activation it does not take into account any context 
;;;             :   effects.  It doesn't have any theoretical basis, but may
;;;             :   be useful for performance purposes if a model generates a lot of
;;;             :   chunks over time and only the recent and/or very strong ones are
;;;             :   necessary for blending purposes.
;;; 2011.09.27 Dan
;;;             : * Changed a chunk-slot-value-fct call to a fast-... to improve
;;;             :   performance.
;;; 2011.12.19 Dan
;;;             : * Added a new request parameter :do-not-generalize which is 
;;;             :   similar to :ignore-slots in that it takes a list of slot
;;;             :   names and affects how the result for that slot is computed.
;;;             :   For a slot in the :do-not-generalize list it still uses 
;;;             :   blending to create the slot value, but it will not use method
;;;             :   c as described in the readme.
;;; 2011.12.21 Dan
;;;             : * Fixed a bug which would result in an error if all of the
;;;             :   matching chunks fell below the :min-bl value.
;;; 2012.02.22 Dan
;;;             : * Fixed an issue with the warning for setting the :blending-set-hook
;;;             :   parameter because when it was bad previously the warning said
;;;             :   :blending-result-hook.
;;; 2012.11.09 Dan 
;;;             : * Fixed a typo in the blending traces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Module to implement the blended retrieval process.  Drop this file into the
;;; modules directory to add a blending module and buffer called blending to the 
;;; system.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Requests to the blending module work like retrievals except that the chunk
;;; which gets placed into the buffer is "blended".  
;;;
;;; See the blending-read-me.txt file and the slides for details on how blended
;;; retrievals work and some background theory.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Implementing the module without touching the internals of the declarative
;;; module for now.  Does however call some of its functions to compute activation
;;; and other values.  Thus it will only work with the default declarative module
;;; or a replacement which has the same functions available.
;;; 
;;; The blending module has its own internal state and error flags for queries.
;;; Thus it is independent of the normal declarative module and it's possible to
;;; have both a retrieval request and a blending request active at the same time.
;;;
;;; Using create-new-buffer-chunk from the goal-style module codebase to handle
;;; the chunk creation/cleanup.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;; To be safe since I'm using the goal-style code's create-buffer-chunk function

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

(suppress-extension-warnings)
(extend-chunks blended-activation :default-value nil)
(extend-chunks blended-time :default-value nil)

;; If it gets a new reference unsuppress it 
(defun blending-unsuppress (c1 c2)
  (declare (ignore c1 c2))
  nil)

(extend-chunks blending-suppressed :default-value nil :merge-function blending-unsuppress)

(unsuppress-extension-warnings)

;; A structure to be the instance of the module
;; holds the busy/free flag, the error flag, the 
;; parameter values for the module and caches some
;; of the declarative module's parameters too.

(defstruct blending-module busy error tmp trace v->m m->v rt esc mp ans blend-all request-hooks result-hooks set-hooks
  trace-table sblt min-bl)


;; Structures to hold the blending trace info

(defstruct sblt-trace
  chunk-type
  no-matches
  tmp
  temperature
  activation-list
  blended-slots
  ignore
  slot-list
  new-chunk
  act
  fail)

(defstruct sblt-slot
  name
  slot-vals
  magnitudes
  condition
  mag-adjusted
  adjusted-val
  ctype
  chunks
  possible-values
  mags)

;; The function to create a new instance of the module.
;; Just return a new structure and ignore the model's name.

(defun create-blending (model)
  (declare (ignore model))
  (make-blending-module))

;; The function to reset the module.
;; Just need to clear the flags because the 
;; parameter values will be set to defaults 
;; automatically by reset.

(defun blending-reset (instance)
  (setf (blending-module-busy instance) nil)
  (setf (blending-module-error instance) nil)
  (setf (blending-module-trace-table instance) (make-hash-table :test #'equal)))

;; Set/get the parameter values for the module.

(defun blending-params (instance param)
  (if (consp param)
      (case (car param)
        
        (:min-bl (setf (blending-module-min-bl instance) (cdr param)))
        (:tmp (setf (blending-module-tmp instance) (cdr param)))
        (:blt (setf (blending-module-trace instance) (cdr param)))
        (:sblt (setf (blending-module-sblt instance) (cdr param)))
        (:value->mag (setf (blending-module-v->m instance) (cdr param)))
        (:mag->value (setf (blending-module-m->v instance) (cdr param)))
        (:rt (setf (blending-module-rt instance) (cdr param)))
        (:esc (setf (blending-module-esc instance) (cdr param)))
        (:mp (setf (blending-module-mp instance) (cdr param)))
        (:ans (setf (blending-module-ans instance) (cdr param)))
        (:blend-all-slots (setf (blending-module-blend-all instance) (cdr param)))
        (:blending-request-hook 
            (if (cdr param)
              (if (member (cdr param) (blending-module-request-hooks instance))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the hook."
                 :blending-request-hook
                 (cdr param))
                (push (cdr param) (blending-module-request-hooks instance)))
              (setf (blending-module-request-hooks instance) nil)))
        (:blending-result-hook 
            (if (cdr param)
              (if (member (cdr param) (blending-module-result-hooks instance))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the hook."
                 :blending-result-hook
                 (cdr param))
                (push (cdr param) (blending-module-result-hooks instance)))
              (setf (blending-module-result-hooks instance) nil)))
        (:blending-set-hook 
            (if (cdr param)
              (if (member (cdr param) (blending-module-set-hooks instance))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the hook."
                 :blending-set-hook
                 (cdr param))
                (push (cdr param) (blending-module-set-hooks instance)))
              (setf (blending-module-set-hooks instance) nil))))
    
    (case param
      (:min-bl (blending-module-min-bl instance))
      (:tmp (blending-module-tmp instance))
      (:blt (blending-module-trace instance))
      (:sblt (blending-module-sblt instance))
      (:value->mag (blending-module-v->m instance))
      (:mag->value (blending-module-m->v instance))
      (:blend-all-slots (blending-module-blend-all instance))
      (:blending-request-hook (blending-module-request-hooks instance))
      (:blending-result-hook (blending-module-result-hooks instance))
      (:blending-set-hook (blending-module-set-hooks instance)))))

;; The function to test the state of the module.
;; Only deal with the simple state values of busy, 
;; free, and error - nothing fancy.

(defun blending-query (instance buffer-name slot value)
  (declare (ignore slot))  ; only have 1 buffer and our only valid slot is state
  (case value
    (busy (blending-module-busy instance))
    (free (not (blending-module-busy instance)))
    (error (blending-module-error instance))
    (t (print-warning "Unknown state query ~S to ~S module" value buffer-name))))

;; The code to handle the requests.
;; They're done in a multi-step process
;; to interface well with other things.

;; The steps will be:

;; - get the request and schedule the real start for "later"
;;   to let the other buffers deal with 0-time requests
;;   for activation spreading purposes
;;
;; - process the request and then schedule either the 
;;   completion or failure event
;;
;; - Set the chunk into the buffer and clear the busy
;;   flag upon completion or set the error flag at failure time


(defun blending-request (instance buffer request)
  (declare (ignore buffer)) ;; It is always going to be blending
  
  (when (blending-module-busy instance) ;; a request is pending
    
    ;; Report a warning about that and delete the pending request
    ;; which is held in the busy slot of the module.
    
    (model-warning "A blending event has been aborted by a new request")
    (delete-event (blending-module-busy instance))
    
    ;; send a null notify for the last request
    
    (awhen (blending-module-result-hooks instance)
               (dolist (x it)
                 (funcall x nil))))
  
  ;; Clear the failure flag of the module
  
  (setf (blending-module-error instance) nil)
  
  ;; Schedule an event to start the real blending at the current time
  ;; but with a priority of -3000 saving that as the busy flag.
  
  ;; Important to ensure that any buffer modifications have had a chance
  ;; to occur so that the "correct" sources are used for activation spreading.
  
  (setf (blending-module-busy instance)
    (schedule-event-relative 0 'start-blending :destination 'blending
                             :module 'blending :details (symbol-name 'start-blending)
                             :priority -3000 :params (list request) :output 'medium)))


;; Here's the function that does most of the real work for the request.
;;

(defun start-blending (instance request)
    
  (let ((ignore nil) (dont-generalize nil)(sblt nil))
    
    (when (blending-module-sblt instance)
      (setf sblt (make-sblt-trace))
      (setf (gethash (mp-time) (blending-module-trace-table instance)) sblt))
    
    (when (member :ignore-slots (chunk-spec-slots request))
      
      (let ((ignore-slots (chunk-spec-slot-spec request :ignore-slots)))
        (cond ((> (length ignore-slots) 1)
               (print-warning "Invalid blending request.")
               (print-warning ":ignore-slots parameter used more than once.")
               (return-from start-blending))
              ((not (eq '= (caar ignore-slots)))
               (print-warning "Invalid blending request.")
               (print-warning ":ignore-slots parameter's modifier must be =.")
               (return-from start-blending))
              ((not (listp (third (car ignore-slots))))
               (print-warning "Invalid blending request.")
               (print-warning ":ignore-slots parameter's value must be a list.")
               (return-from start-blending))
              (t 
               (setf ignore (third (car ignore-slots)))))))
    
    (when (member :do-not-generalize (chunk-spec-slots request))
      
      (let ((ignore-slots (chunk-spec-slot-spec request :do-not-generalize)))
        (cond ((> (length ignore-slots) 1)
               (print-warning "Invalid blending request.")
               (print-warning ":do-not-generalize parameter used more than once.")
               (return-from start-blending))
              ((not (eq '= (caar ignore-slots)))
               (print-warning "Invalid blending request.")
               (print-warning ":do-not-generalize parameter's modifier must be =.")
               (return-from start-blending))
              ((not (listp (third (car ignore-slots))))
               (print-warning "Invalid blending request.")
               (print-warning ":do-not-generalize parameter's value must be a list.")
               (return-from start-blending))
              (t 
               (setf dont-generalize (third (car ignore-slots)))))))
    
    (setf request (strip-request-parameters-from-chunk-spec request))
    
    (let* ((dm (get-module declarative))                   ;; get that module since we're using some of its functions
           (ct (chunk-spec-chunk-type request))            ;; chunk-type of the request
           (all-slots (chunk-type-slot-names-fct ct))
           (request-details (chunk-spec-slot-spec request))
           (fixed-values (remove-if-not (lambda (x) (eq (car x) '=)) request-details))
           (fixed-slots (mapcar #'second fixed-values))
           (blended-slots (remove-if (lambda (x) 
                                       (find x ignore))
                                     (if (blending-module-blend-all instance)
                                         all-slots
                                       (set-difference all-slots fixed-slots))))
           
           ;; perform the chunk matching just like the declarative module does
           
           (chunk-list (remove-if 'chunk-blending-suppressed (no-output (sdm-fct `(isa ,ct)))))  ;; get the list of possible chunks
           (matching-chunks (cond ((or (null (blending-module-esc instance)) 
                                       (null (blending-module-mp instance)))
                                   ;; Perfect matching 
                                   (find-matching-chunks request :chunks chunk-list))
                                  (t
                                   ;; everything that fits the general pattern:
                                   (find-matching-chunks 
                                    (define-chunk-spec-fct 
                                        `(isa ,ct
                                              ,@(mapcan #'(lambda (x)
                                                            (cond ((eq (car x) '=)
                                                                   (if (third x)
                                                                       (list '- (second x) nil)
                                                                     x))
                                                                  ((eq (car x) '-)
                                                                   (unless (third x) x))
                                                                  ;;; make sure the comparison tests match
                                                                  (t x)))
                                                  (chunk-spec-slot-spec request))))
                                    :chunks chunk-list))))
           (temperature (aif (blending-module-tmp instance) 
                             it
                             (if (null (blending-module-ans instance))
                                 (progn
                                   (print-warning "Blending requires :tmp or :ans to be set - assuming default of 1.")
                                   1.0)
                               (* (sqrt 2) (blending-module-ans instance)))))
           
           ;; Have the declarative module compute the activations and record them here
           (activation-list (mapcan (lambda (chunk) 
                                      (compute-activation dm chunk request)   ;; compute the activation
                                      (setf (chunk-blended-activation chunk) (chunk-activation chunk))
                                      (setf (chunk-blended-time chunk) (mp-time))
                                      (if (and (blending-module-min-bl instance) (< (chunk-last-base-level chunk) (blending-module-min-bl instance)))
                                          (progn (setf (chunk-blending-suppressed chunk) t)
                                            nil)
                                        (list (list (chunk-activation chunk) 
                                                    (handler-case (exp (/ (chunk-activation chunk) temperature))
                                                      (floating-point-underflow () 0)
                                                      (floating-point-overflow () 
                                                                               (print-warning "Math overflow during blending chunk activation for ~s is ~s" chunk (chunk-activation chunk)) 
                                                                               (print-warning "Results of blending are not likely to be meaningful.")
                                                                               ;; just use something big and assume it's available for now...
                                                                               (exp 50)))
                                                    chunk))))
                              matching-chunks)))
      
      (when (and (blending-module-blend-all instance) (null (blending-module-mp instance)))
        (print-warning "The :blend-all-slots parameter is set to t, but the :mp parameter is nil which means only perfect matches can occur."))
      
      (when (blending-module-sblt instance)
        (setf (sblt-trace-chunk-type sblt) ct))
      
      (when (blending-module-trace instance)
        (model-output "Blending request for chunks of type ~a" ct))
      
      
      (awhen (blending-module-request-hooks instance)
             (dolist (x it)
               (funcall x request)))
      
      (awhen (blending-module-set-hooks instance)
             (dolist (x it)
               (funcall x matching-chunks)))

      
      (when (null activation-list) ;; a complete failure
        
        (when (blending-module-sblt instance)
          (setf (sblt-trace-no-matches sblt) t))
        
        (when (blending-module-trace instance)
          (model-output "No matching chunks of type ~a found." ct)
          (model-output "Blending request fails."))
        
        ;; schedule the failure event to happen and record that as the busy flag
        ;; failure time same as for declarative - based on the retrieval threshold
        
        (setf (blending-module-busy instance) 
          (schedule-event-relative (compute-activation-latency dm (blending-module-rt instance))
                                   'blending-failure :module 'blending
                                   :destination 'blending :output 'low))
        
        (awhen (blending-module-result-hooks instance)
               (dolist (x it)
                 (funcall x nil)))
        
        (return-from start-blending nil))
      
      (when (blending-module-sblt instance)
        (setf (sblt-trace-tmp sblt) (blending-module-tmp instance))
        (setf (sblt-trace-temperature sblt) temperature))
      
      (when (blending-module-trace instance)
        (if (blending-module-tmp instance)
            (model-output "Blending temperature is: ~f" temperature)
          (model-output "Blending temperature defaults to (* (sqrt 2) :ans): ~f" temperature)))
      
      (let ((sum (reduce #'+ (mapcar #'second activation-list)))
            (new-chunk (list ct 'isa))
            (blended-results (mapcar (lambda (x) (cons x nil)) blended-slots))
            sblt-slot)
        
        (mapc (lambda (x) (setf (second x) (/ (second x) sum))) activation-list)
        
        (when (blending-module-sblt instance)
          (setf (sblt-trace-activation-list sblt) activation-list)
          (setf (sblt-trace-blended-slots sblt) blended-slots)
          (setf (sblt-trace-ignore sblt) ignore))
        
        (when (blending-module-trace instance)
          (dolist (x activation-list)
            (model-output "Chunk ~S matches blending request~%  Activation ~f~%  Probability of recall ~f~%"
                          (third x) (first x) (second x)))
          (model-output "~%Slots to be blended: ~S" blended-slots)
          (when ignore 
            (model-output "Slots being explicitly ignored: ~S~%" ignore)))
        
        (dolist (slot blended-slots)
          
          (when (blending-module-sblt instance)
            (setf sblt-slot (make-sblt-slot :name slot))
            (push-last sblt-slot (sblt-trace-slot-list sblt)))
          
          (when (blending-module-trace instance)
            (model-output "Finding blended value for slot: ~s" slot))
          
          (let* ((possible-values (mapcar (lambda (x) (list (third x) (fast-chunk-slot-value-fct (third x) slot) (second x))) activation-list))
                 (slot-vals (mapcar #'second possible-values))
                 (magnitudes (mapcar (lambda (x) (list (car x) (funcall (blending-module-v->m instance) (second x)) (third x))) possible-values))
                 (mags (mapcar #'second magnitudes)))
            
            (when (blending-module-sblt instance)
              (setf (sblt-slot-slot-vals sblt-slot) slot-vals)
              (setf (sblt-slot-magnitudes sblt-slot) magnitudes))
            
            (when (blending-module-trace instance)
              (model-output "Matched chunks' slots contain: ~S" slot-vals)
              (model-output "Magnitude values for those items: ~S" mags))
            
            (cond ((every 'numberp mags)
                   
                   (when (blending-module-sblt instance)
                     (setf (sblt-slot-condition sblt-slot) :numbers))
                   
                   (let ((sum 0))
                     (when (blending-module-trace instance)
                       (model-output "With numeric magnitudes blending by weighted average"))
                     
                     (dolist (mag magnitudes)
                       
                       (incf sum (* (second mag) (third mag)))
                       
                       (when (blending-module-trace instance)
                         (model-output " Chunk ~s with probability ~f times magnitude ~f cumulative result: ~f" (first mag)
                                       (third mag) (second mag) sum)))
                     
                     (cond ((and (blending-module-m->v instance)
                                 (not (equalp slot-vals mags)))
                            (let ((result (funcall (blending-module-m->v instance) sum (common-chunk-type slot-vals))))
                              (setf (cdr (assoc slot blended-results)) result)
                              
                              (when (blending-module-sblt instance)
                                (setf (sblt-slot-mag-adjusted sblt-slot) t)
                                (setf (sblt-slot-adjusted-val sblt-slot) result))
                              
                              (when (blending-module-trace instance)
                                (model-output " Final result: ~f  Converted to value: ~s" sum result))))
                           (t 
                            (setf (cdr (assoc slot blended-results)) sum)
                            (when (blending-module-trace instance)
                              (model-output " Final result: ~f" sum))))))
                  
                  ((every 'null mags) ;; they're all nil
                   
                   (when (blending-module-sblt instance)
                     (setf (sblt-slot-condition sblt-slot) :null))
                   
                   (when (blending-module-trace instance)
                     (model-output "When all magnitudes are nil there's nothing to blend and the result is nil"))
                   
                   (setf (cdr (assoc slot blended-results)) nil))
                  
                  ((and (every (lambda (x) (or (null x) (chunk-p-fct x))) mags) (not (find slot dont-generalize)))
                   
                   (when (blending-module-sblt instance)
                     (setf (sblt-slot-condition sblt-slot) :chunks))
                   
                   (when (blending-module-trace instance)
                     (model-output "When all magnitudes are chunks or nil blending based on common chunk-types and similarities"))
                   
                   (let* ((type (common-chunk-type mags))
                          (chunks (if type
                                      (no-output (sdm-fct `(isa ,type)))
                                    (no-output (sdm)))))
                     
                     
                     (when (blending-module-sblt instance)
                       (setf (sblt-slot-ctype sblt-slot) type)
                       (setf (sblt-slot-chunks sblt-slot) chunks))
                     
                     (when (blending-module-trace instance)
                       (if type
                           (model-output "Common chunk-type for values is: ~s" type)
                         (model-output "No common chunk-type found all chunks will be tested")))
                     
                     (let ((best-val nil)
                           (best-mag nil))
                       
                       (dolist (val chunks)
                         
                         (when (blending-module-trace instance)
                           (model-output " Comparing value ~S" val))
                         
                         (let ((sum 0.0))
                           
                           (dolist (possible possible-values)
                             
                             (when (blending-module-sblt instance)
                               (push-last (cons val (list (first possible) (second possible) (third possible) (chunks-similarity dm val (second possible))))
                                         (sblt-slot-possible-values sblt-slot)))
                             
                             (incf sum (* (third possible) (expt (chunks-similarity dm val (second possible)) 2)))
                             
                             (when (blending-module-trace instance)
                               (model-output "  Chunk ~s with probability ~f slot value ~s similarity: ~f cumulative result: ~f" 
                                             (first possible) (third possible) (second possible) (chunks-similarity dm val (second possible)) sum)))
                           
                           (when (or (null best-mag)
                                     (< sum best-mag))
                             (setf best-mag sum)
                             (setf best-val val))))
                       
                       (cond ((and (blending-module-m->v instance)
                                   (not (equalp slot-vals mags)))
                              (let ((result (funcall (blending-module-m->v instance) best-val type)))
                                (setf (cdr (assoc slot blended-results)) result)
                                
                                (when (blending-module-sblt instance)
                                  (setf (sblt-slot-mag-adjusted sblt-slot) t)
                                  (setf (sblt-slot-adjusted-val sblt-slot) result))
                                
                                (when (blending-module-trace instance)
                                  (model-output " Final result: ~f  Converted to value: ~s" best-val result))))
                             (t 
                              (setf (cdr (assoc slot blended-results)) best-val)
                              (when (blending-module-trace instance)
                                (model-output " Final result: ~f" best-val)))))))
                  (t
                   
                   (when (blending-module-sblt instance)
                     (setf (sblt-slot-condition sblt-slot) :other)
                     (setf (sblt-slot-mags sblt-slot) (remove-duplicates mags)))
                   
                   (when (blending-module-trace instance)
                     (model-output "When not all magnitudes are numbers or chunks blending based on similarities using only those values"))
                   
                   (let ((best-val nil)
                         (best-mag nil))
                     
                     (dolist (val (remove-duplicates mags))
                       
                       (when (blending-module-trace instance)
                         (model-output " Comparing value ~S" val))
                       
                       (let ((sum 0.0))
                         
                         (dolist (possible possible-values)
                           
                           (when (blending-module-sblt instance)
                               (push-last (cons val (list (first possible) (second possible) (third possible) (chunks-similarity dm val (second possible))))
                                         (sblt-slot-possible-values sblt-slot)))
                           
                           (incf sum (* (third possible) (expt (chunks-similarity dm  val (second possible)) 2)))
                           
                           (when (blending-module-trace instance)
                             (model-output "  Chunk ~s with probability ~f slot value ~s similarity: ~f cumulative result: ~f" 
                                           (first possible) (third possible) (second possible) (chunks-similarity dm val (second possible)) sum)))
                         
                         (when (or (null best-mag)
                                   (< sum best-mag))
                           (setf best-mag sum)
                           (setf best-val val))))
                     
                     
                     (cond ((and (blending-module-m->v instance)
                                 (not (equalp slot-vals mags)))
                            (let ((result (funcall (blending-module-m->v instance) best-val nil)))
                              (setf (cdr (assoc slot blended-results)) result)
                              
                              (when (blending-module-sblt instance)
                                (setf (sblt-slot-mag-adjusted sblt-slot) t)
                                (setf (sblt-slot-adjusted-val sblt-slot) result))
                              
                              (when (blending-module-trace instance)
                                (model-output " Final result: ~f  Converted to value: ~s" best-val result))))
                           (t 
                            (setf (cdr (assoc slot blended-results)) best-val)
                            (when (blending-module-trace instance)
                              (model-output " Final result: ~f" best-val)))))))))
        
        ;; put the fixed values into the chunk def.
        
        (unless (blending-module-blend-all instance)
          (dolist (slot fixed-values)
            (push (second slot) new-chunk)
            (push (third slot) new-chunk)))
        
        ;; put the blended values into the chunk def.
        
        (dolist (slot blended-results)
          (push (car slot) new-chunk)
          (push (cdr slot) new-chunk))
        
        (setf new-chunk (nreverse new-chunk))
        
        (when (blending-module-sblt instance)
          (setf (sblt-trace-new-chunk sblt) new-chunk))
        
        (when (blending-module-trace instance)
          (model-output "This is the definition of the blended chunk:~%~s" new-chunk)
          (model-output "~%Computing activation and latency for the blended chunk"))
        
        (let ((act 0))
          
          (dolist (c activation-list)
            (incf act (exp (first c)))
            
            (when (blending-module-trace instance)
              (model-output " Activation of chunk ~S is ~f" (third c) (first c))))
          
          (setf act (log act))
          
          (when (blending-module-trace instance)
            (model-output "Activation for blended chunk is: ~f" act))
          
          
          (when (blending-module-sblt instance)
            (setf (sblt-trace-act sblt) act))
          
          (cond ((>= act (blending-module-rt instance))
                 (setf (blending-module-busy instance) 
                   (schedule-event-relative 
                    (compute-activation-latency dm act)
                    'blending-complete
                    :module 'blending
                    :destination 'blending
                    :params (list new-chunk)
                    :details (symbol-name 'blending-complete)
                    :output 'medium)))
                (t 
                 (when (blending-module-trace instance)
                   (model-output "Not above threshold so blending failed"))
                 
                 (when (blending-module-sblt instance)
                   (setf (sblt-trace-fail sblt) t))
                 
                 (setf (blending-module-busy instance) 
                   (schedule-event-relative 
                    (compute-activation-latency dm (blending-module-rt instance))
                    'blending-failure
                    :module 'blending
                    :destination 'blending
                    :details (symbol-name 'blending-failure)
                    :output 'medium)))))))))

    
(defun common-chunk-type (chunk-list)
  (let ((types (remove-duplicates (mapcar #'chunk-chunk-type-fct (remove nil chunk-list)))))
    (if (= (length types) 1)
        (car types)
      (let ((possible (reduce 'intersection (mapcar #'chunk-type-supertypes-fct types))))
        (cond ((= (length possible) 1)
               (car possible))
              ((null possible)
               nil)
              (t
               (car (sort possible (lambda (x y) (chunk-type-subtype-p-fct x y))))))))))
                              
    
;;; Call as an event when a chunk has been blended and is ready to be placed
;;; into the buffer.
;;;
   
(defun blending-complete (instance chunk-list)
  
  ;; Clear the busy flag
  
  (setf (blending-module-busy instance) nil)
    
  ;; Schedule an event to create that chunk in the buffer
  ;; using the goal-style module's function which handles
  ;; the scheduling and some extra cleanup.
  
  (create-new-buffer-chunk 'blending chunk-list)
  (schedule-event-after-module 'blending 'call-blending-result-hooks :maintenance t :output nil :destination 'blending :module 'blending)
  )

(defun call-blending-result-hooks (instance)
  (awhen (blending-module-result-hooks instance)
         (dolist (x it)
           (funcall x (buffer-read 'blending)))))

;;; Call as an event when a chunk fails to be created for a blending request.

(defun blending-failure (instance)
  
  ;; Clear the busy flag and set the error flag.
  
  (setf (blending-module-busy instance) nil)
  (setf (blending-module-error instance) t)
  (awhen (blending-module-result-hooks instance)
         (dolist (x it)
           (funcall x nil))))


   


(define-module-fct 'blending '((blending nil (:ignore-slots :do-not-generalize)))                 
  (list                           
   (define-parameter :blt :valid-test #'tornil 
     :default-value nil :warning "T or nil" 
     :documentation "Blending trace")
   (define-parameter :sblt :valid-test #'tornil 
     :default-value nil :warning "T or nil" 
     :documentation "Save blending trace")
   
   (define-parameter :min-bl :valid-test #'numornil
     :default-value nil :warning "A number or nil" 
     :documentation "Blending minimum base-level activation required to consider a chunk for blending")
   
   (define-parameter :tmp :valid-test #'(lambda (x) (or (null x) (nonneg x)))
     :default-value nil :warning "Non-negative number or nil" 
     :documentation "Blending temperature")
   (define-parameter :value->mag :valid-test #'(lambda (x) (and x (fctornil x)))
     :default-value 'identity :warning "function" 
     :documentation "Blending function to map a slot value to a magnitude to be blended")
   (define-parameter :mag->value :valid-test #'fctornil 
     :default-value nil :warning "a function or nil" 
     :documentation "Blending function to map a blended magnitude back into a value for the slot")
   (define-parameter :blend-all-slots :valid-test #'tornil
     :default-value nil :warning "t or nil" 
     :documentation "Whether the requested slots are also blended")
   (define-parameter :blending-request-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
     :documentation "Blending request notification hook")
   (define-parameter :blending-result-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
     :documentation "Blended result notification hook")
   (define-parameter :blending-set-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Blended chunk set notification hook")
   (define-parameter :rt :owner nil)
   (define-parameter :esc :owner nil)
   (define-parameter :mp :owner nil)
   (define-parameter :ans :owner nil))
  
  ;; Have to have version and a doc strings
  
  :version "1.0b3"
  :documentation "Module which adds a new buffer to do blended retrievals"
  
  ;; functions to handle the interfacing for the module
  
  :creation 'create-blending
  :reset 'blending-reset
  :params 'blending-params
  :query 'blending-query
  :request 'blending-request)


(defun print-blending-trace (time)
  (let ((b (get-module blending)))
    (if b
        (let ((sblt (gethash time (blending-module-trace-table b))))
          (if sblt
              (progn   
                (model-output "Blending request for chunks of type ~a" (sblt-trace-chunk-type sblt))
                (if (sblt-trace-no-matches sblt)
                    (progn
                      (model-output "No matching chunks of type ~a found." (sblt-trace-chunk-type sblt))
                      (model-output "Blending request fails."))
                  
                  (progn
                    
                    (if (sblt-trace-tmp sblt)
                        (model-output "Blending temperature is: ~f" (sblt-trace-temperature sblt))
                      (model-output "Blending temperature defaults to (* (sqrt 2) :ans): ~f" (sblt-trace-temperature sblt)))
                    
                    (dolist (x (sblt-trace-activation-list sblt))
                      (model-output "Chunk ~S matches blending request~%  Activation ~f~%  Probability of recall ~f~%"
                                    (third x) (first x) (second x)))
                    
                    (model-output "~%Slots to be blended: ~S" (sblt-trace-blended-slots sblt))
                    
                    (when (sblt-trace-ignore sblt) 
                      (model-output "Slots being explicitly ignored: ~S~%" (sblt-trace-ignore sblt)))
                    
                    (dolist (slot (sblt-trace-slot-list sblt))
                      
                      (model-output "Finding blended value for slot: ~s" (sblt-slot-name slot))
                      
                      (model-output "Matched chunks' slots contain: ~S" (sblt-slot-slot-vals slot))
                      (model-output "Magnitude values for those items: ~S" (mapcar #'second (sblt-slot-magnitudes slot)))
                      
                      (case (sblt-slot-condition slot)
                        
                        (:numbers
                         (model-output "With numeric magnitudes blending by weighted average")
                         
                         (let ((sum 0))
                           
                           (dolist (mag (sblt-slot-magnitudes slot))
                             
                             (incf sum (* (second mag) (third mag)))
                             
                             (model-output " Chunk ~s with probability ~f times magnitude ~f cumulative result: ~f" (first mag)
                                           (third mag) (second mag) sum))
                           
                           (if (sblt-slot-mag-adjusted slot)
                               (model-output " Final result: ~f  Converted to value: ~s" sum (sblt-slot-adjusted-val slot))
                             (model-output " Final result: ~f" sum))))
                        
                        (:null
                         
                         (model-output "When all magnitudes are nil there's nothing to blend and the result is nil"))
                        
                        (:chunks 
                         (model-output "When all magnitudes are chunks or nil blending based on common chunk-types and similarities")
                         
                         (if (sblt-slot-ctype slot)
                             (model-output "Common chunk-type for values is: ~s" (sblt-slot-ctype slot))
                           (model-output "No common chunk-type found all chunks will be tested"))
                         
                         (let ((best-val nil)
                               (best-mag nil))
                           
                           (dolist (val (sblt-slot-chunks slot))
                             
                             (model-output " Comparing value ~S" val)
                             
                             (let ((sum 0.0))
                               
                               (dolist (possible (mapcar 'cdr (remove-if-not (lambda (x) (eq (car x) val)) (sblt-slot-possible-values slot))))
                                 (incf sum (* (third possible) (expt (fourth possible) 2)))
                                 
                                 (model-output "  Chunk ~s with probability ~f slot value ~s similarity: ~f cumulative result: ~f" 
                                               (first possible) (third possible) (second possible) (fourth possible) sum))
                               
                               (when (or (null best-mag)
                                         (< sum best-mag))
                                 (setf best-mag sum)
                                 (setf best-val val))))
                           
                           (if (sblt-slot-mag-adjusted slot)
                               (model-output " Final result: ~f  Converted to value: ~s" best-val (sblt-slot-adjusted-val slot))
                             (model-output " Final result: ~f" best-val))))
                        
                        (:other
                         (model-output "When not all magnitudes are numbers or chunks blending based on similarities using only those values")
                         
                         (let ((best-val nil)
                               (best-mag nil))
                           
                           (dolist (val (sblt-slot-mags slot))
                             
                             (model-output " Comparing value ~S" val)
                             
                             (let ((sum 0.0))
                               
                               (dolist (possible (mapcar 'cdr (remove-if-not (lambda (x) (eq (car x) val)) (sblt-slot-possible-values slot))))
                                 
                                 (incf sum (* (third possible) (expt (fourth possible) 2)))
                                 
                                 (model-output "  Chunk ~s with probability ~f slot value ~s similarity: ~f cumulative result: ~f" 
                                               (first possible) (third possible) (second possible) (fourth possible) sum))
                               
                               (when (or (null best-mag)
                                         (< sum best-mag))
                                 (setf best-mag sum)
                                 (setf best-val val))))
                           
                           
                           (if (sblt-slot-mag-adjusted slot)
                               (model-output " Final result: ~f  Converted to value: ~s" best-val (sblt-slot-adjusted-val slot))
                             (model-output " Final result: ~f" best-val))))))
                    
                    
                    (model-output "This is the definition of the blended chunk:~%~s" (sblt-trace-new-chunk sblt))
                    (model-output "~%Computing activation and latency for the blended chunk")
                    
                    (dolist (c (sblt-trace-activation-list sblt))
                      (model-output " Activation of chunk ~S is ~f" (third c) (first c)))
                    
                    
                    (model-output "Activation for blended chunk is: ~f" (sblt-trace-act sblt))
                    
                    
                    (when (sblt-trace-fail sblt)
                      (model-output "Not above threshold so blending failed"))))) 
            
            (model-warning "No blending trace information available for time ~S" time)))
      (print-warning "No blending module available for reporting trace."))))

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
