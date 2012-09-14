;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : utility-and-reward.lisp
;;; Version     : 1.0
;;; 
;;; Description : The procedural utility computation functions and a new module
;;;             : for handling the "reward" given to a production for success
;;;             : or failure - the idea being that this isn't something tied
;;;             : explicitly to the firing of a production.
;;;             : This is the "basic" version that maintains compatibility
;;;             : with the old mechanisms and doesn't actually have a new module,
;;;             : but there are other versions planned that change thee nature 
;;;             ; of the utility computation.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2005.11.18 Dan
;;;             : * Initial creation.
;;; 2005.12.19 Dan
;;;             : * Actually put this in place.
;;; 2005.12.22 Dan
;;;             : * Moved the spp code here along with the production
;;;             :   compilation utility computing code.
;;;             : * Added the extend-productions macros to implment the
;;;             :   parameters.
;;; 2006.03.08 Dan
;;;             : * Moved ALL the utility parameters here and added a utility
;;;             :   module to coordinate it.
;;;             : * Incorporated the production compilation functions directly
;;;             :   into the probability and cost functions.
;;;             : * Implemented/documented the API that's required for utility
;;;             :   calculations from the symbolic side of the production system.
;;;             :   They now only interact through that API so replacing the
;;;             :   utility computations can be done solely through replacement
;;;             :   of this file - no need to touch any other pieces of the
;;;             :   procedural system.
;;; 2006.03.14 Dan
;;;             : * Switched from using pp to (all-productions).
;;; 2006.03.21 Dan
;;;             : * Minor bugs in parameters-fct fixed now (always assumed that 
;;;             :   :pl was t.
;;; 2006.05.23 Dan
;;;             : * Fixed a bug in the utility-hook call in compute-utility because 
;;;             :   the production name is now what gets passed to the function.
;;; 2006.08.23 Dan
;;;             : * Fixed compute-utility because it relied on a quirk of the
;;;             :   actr-random code which has been corrected (passing 0 to
;;;             :   the random function is no longer supported).
;;; 2006.09.01 Dan
;;;             : * Changed some parameter checks from posnum to nonneg.
;;; 2006.10.06 Dan
;;;             : * Changed some print-warning calls to model-warning so that
;;;             :   they turn off with the model-warning parameter.
;;; 2006.10.25 Dan
;;;             : * Fixed a performance bug - the history list was recorded
;;;             :   even when parameter learning was off,
;;; 2008.07.22 Dan
;;;             : * Changed compute-utility to have the optional parameter
;;;             :   which indicates whether or not to save the utility value.
;;; 2008.08.08 Dan
;;;             : * Changed the ownership on :dat since procedural now owns it.
;;;             : * Fixed a bug introduced in compute-utility with the previous
;;;             :   change.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes on replacemnt:
;;;
;;; If one wants to replace this with a different utility mechanism the following
;;; functions must still be defined (this is essentially the API between the
;;; procedural system and the utility system):
;;;
;;; (defun note-production-selection (production)
;;;
;;; This is called everytime a production is selected with the name of the
;;; production. 
;;;
;;; (defun learn-parameters (production)
;;;
;;; This is called after every production firing with the name of the production 
;;; that fired.
;;;
;;; (defun compute-utility (production &optional save)
;;; 
;;; This is called during conflict resolution for each production that matches
;;; current state.  It will get the name of a production and must return a
;;; number for th production's utility.  If save is true then it should store
;;; the computed value in the production-utility parameter otherwise it should
;;; leave the parameter unchanged.
;;;
;;; (defun minimum-utility ()
;;;
;;; Called during conflict resolution to determine which productions
;;; should be considered given their utilities.  Must return either a number
;;; or nil (which means no minimum value).
;;;
;;; (defun productions-action-time (production)
;;; 
;;; Called when a production is selected to determine how long before it should
;;; be fired.  It is called with a production name and must return a number.
;;;
;;; (defun initialize-utility-for-compiled-production (new-p p1 p2)
;;;
;;; Called when production compilation creates a new production for the first
;;; time.  new-p is the name of the new production and p1 and p2 are the 
;;; names of the productions that were compiled together to create it respectively.
;;; This should set all the utility parameters needed for this new production.
;;;
;;; (defun update-utility-for-compiled-production (p3 p1 p2)
;;;
;;; Called when production compilation creates a production that it has created
;;; previously.  p3 is the name of the compiled production and p1 and p2 are the 
;;; names of the productions that were compiled together to create it respectively.
;;; This should update any utility parameters needed for this new production.
;;;
;;;
;;; The spp command is also used elsewhere and users will expect it to to still 
;;; work with whatever parameters one uses for a production.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Instead of needing to set the success/failure flag on a production it is
;;; now possible to trigger the learning from an external event.
;;;
;;; The reward module works in conjunction with the procedural module to 
;;; handle the triggering of procedural learning.  
;;;
;;; Setting the success and/or failure flag on a production still works as it
;;; has in the past, but now one can also call the function trigger-reward.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; trigger-reward 
;;; (defun trigger-reward (value) ...)
;;;
;;; A new function that can be called to initiate the procedural learning.
;;; It can signal the three conditions currently handled by setting the
;;; :success and :failure flags on productions, which are:
;;;
;;; :success t and :failure nil
;;;
;;; A successful event indicated by specifing the keyword :success for the value
;;;  (trigger-reward :success)
;;;
;;; :success nil and :failure t
;;;
;;; A failure event indicated by specifing the keyword :failure for the value
;;;  (trigger-reward :failure)
;;;
;;; :success t and :failure t
;;;
;;; A null event that serves only to signal a stopping point in the assignment
;;; of credit indicated by specifing nil for the value
;;;  (trigger-reward nil)
;;; 
;;;--------------------------------------------
;;;
;;; spp ...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; By moving the utility calculations to a separate file it's easier to 
;;; consider alternatives without having to replace/change the main procedural
;;; code every time as well.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; The structure that is the utility module

(defstruct utility
  egs ut g esc er dat pl
  utility-hook utility-p-hook utility-c-hook  
  utility-learned-p-hook utility-learned-c-hook
  history
  
  ie alpha
  initial-successes
  initial-failures
  initial-efforts)

;;; Functions necessary to support the production parameters

(defun default-success-list (p)
  (declare (ignore p))
  (list 0.0))

(defun default-efforts-list (p)
  (declare (ignore p))
  (no-output (sgp :dat)))

(defun get-current-time (p)
  (declare (ignore p))
  (mp-time))

(defun get-default-action-time (p)
  (declare (ignore p))
  (car (no-output (sgp :dat))))

(defun get-g-value (p)
  (declare (ignore p))
  (car (no-output (sgp :g))))


;;; The parameters added to productions for utility purposes

(extend-productions creation-time :default-function get-current-time)
(extend-productions value :default-value 0.0)
(extend-productions effort :default-function get-default-action-time)
(extend-productions c :default-function get-default-action-time)
(extend-productions p :default-value 1.0)
(extend-productions successes :default-value 1.0)
(extend-productions failures :default-value 0.0)

(extend-productions efforts :default-function get-default-action-time)

(extend-productions pg-c)
(extend-productions failure-list)
(extend-productions success)
(extend-productions failure)

(extend-productions utility)

(extend-productions success-list :default-function default-success-list)
(extend-productions efforts-list :default-function default-efforts-list)

(extend-productions priorp :default-value 0)
(extend-productions priorc :default-function get-g-value)

;;; The functions that are required by the procedural system

(defun note-production-selection (production)
  (let ((u (get-module utility)))
    (when (and u (utility-esc u) (utility-pl u))
      (push (cons production (mp-time)) (utility-history u)))))


(defun productions-action-time (production)
  (or (production-effort production) (production-c production)))

(defun compute-utility (production &optional save)
  (let ((u (get-module utility)))
    (when u
      (let* ((over-ride (awhen (utility-utility-hook u)
                               (funcall it production)))
             (utility
              (if (numberp over-ride)
                  over-ride
                (+ (if (and (utility-esc u) (utility-egs u) (not (zerop (utility-egs u))))
                       (act-r-noise (utility-egs u))
                     0)
                   (setf (production-pg-c production)
                     (cond ((null (utility-esc u))
                            (production-value production))
                           (t
                            (let ((p (compute-p u production))
                                  (c (compute-c u production)))
                              (- (* p (utility-g u)) c)))))))))
        (if save
            (setf (production-utility production) utility)
          utility)))))



(defun minimum-utility ()
  (let ((u (get-module utility)))
    (if (utility-esc u)
        (utility-ut u)
      0)))


(defun learn-parameters (production)
  "Does the bookkeeping work for the parameters after a production fires"
  (let ((u (get-module utility)))
    (when (and u (utility-esc u) (utility-pl u))
      (cond ((and (production-success production)
                  (production-failure production)) 
             
             ;; trigger a null learning event
             (trigger-reward-internal nil u))
            
            ((production-success production)
             
             ;; call the general function to do the computation
             (trigger-reward-internal :success u))
            
            ((production-failure production)
             
             ;; call the general function to do the computation             
             (trigger-reward-internal :failure u))
            
            (t  ;; don't do anything
             )))))
    

(defun initialize-utility-for-compiled-production (new-p p1 p2)
  (let* ((p (production-p p1))
         (c (production-c p1))
         (effort1 (production-effort p1))
         (effort2 (production-effort p2))
         (effort (max effort1 effort2))
         (s1 (production-success p1))
         (s2 (production-success p2))
         (f1 (production-failure p1))
         (f2 (production-failure p2))
         (s (or s1 s2))
         (f (or f1 f2))
         (u (get-module utility))
         (pl (utility-pl u)))
                
    
    ; initialize-stats for new-prod
    
    (cond ((numberp pl)
           (print-warning "Compilation does not work with decaying utilities."))
          ((null pl)
           (setf (production-p new-p) p)
           (setf (production-c new-p) c)
           (setf (production-effort new-p) effort))
          (t ;; pl = t
           (update-priors-for-production u new-p p c)
           
           (setf (production-success new-p) s)
           (setf (production-failure new-p) f)
           (setf (production-effort new-p) effort)
           
           
           (setf (production-successes new-p)  (utility-initial-successes u))
           (setf (production-failures new-p) (utility-initial-failures u))
           (setf (production-efforts new-p) (utility-initial-efforts u))
           
           (compute-utility new-p)))))




(defun update-utility-for-compiled-production (p3 p1 p2)
  (let* ((p (production-p p1))
         (c (production-c p1))
         (effort1 (production-effort p1))
         (effort2 (production-effort p2))
         (effort (max effort1 effort2))
         (pl (no-output (sgp :pl)))
         (u (get-module utility)))
    
    (cond ((numberp pl)
           (print-warning "Compilation does not work with decaying utilities."))
          ((null pl)
           (setf (production-p p3) p)
           (setf (production-c p3) c)
           (setf (production-effort p3) effort))
          (t ;; pl = t
           (update-priors-for-production u p3 p c)
           
           (compute-utility p3)))))



;;; Support functions for computing the utility value
    
(defun compute-p (u production)
  (let ((p (awhen (utility-utility-p-hook u)
              (funcall it production))))
    (setf (production-p production)
           (if (numberp p)
               p
             (if (utility-pl u)
                 (compute-probabilities u production)
               (production-p production))))))


(defun compute-c (u production)
  (let ((c (awhen (utility-utility-c-hook u)
              (funcall it production))))
    (setf (production-c production)
      (if (numberp c)
          c
        (if (utility-pl u)
            (compute-costs u production)
          (production-c production))))))


;;; These are modified from the ACT-R 5 functions

(defun compute-probabilities (u production)
  "Computes probabilities by taking the ratio of successes to the sum of 
   successes and failures.  Computes those as decaying ratios if parameters
   learning is a number."
  
  (let ((p (awhen (utility-utility-learned-p-hook u)
                  (funcall it production))))
    (if (numberp p)
        p
      (if (and (null (production-user-created production))
               (eq t (utility-pl u)))
          ;; compute for a compiled production
          (let ((m (+ (production-successes production)
                      (production-failures production))))
            
            (/ (+ (* (utility-ie u) (production-priorp production)) 
                  (if (zerop m) 0 (* m (compute-probabilities-regular u production))))
               (if (zerop (+ (utility-ie u) m))
                   1
                 (+ (utility-ie u) m))))
        
        ;; compute for regular production
        (compute-probabilities-regular u production)))))


(defun compute-probabilities-regular (u production)
  (let ((successes 0.0)
        (failures 0.0))
    (cond ((numberp (utility-pl u))
           (let ((minus-d (- (utility-pl u))))
             (dolist (success (production-success-list production))
               (incf successes (expt-coerced (max (utility-dat u) (- (mp-time) success))
                                             minus-d)))
             (dolist (failure (production-failure-list production))
               (incf failures (expt-coerced (max (utility-dat u) (- (mp-time) failure))
                                            minus-d)))))
          (t
           (setf successes (production-successes production))
           (setf failures (production-failures production))))
    (/ successes (+ successes failures))))


(defun compute-costs (u production)
  "Computes probabilities by taking the ratio of efforts to the sum of 
   successes and failures.  Computes those as decaying ratios if parameters
   learning is a number."
  
  (let ((c (awhen (utility-utility-learned-c-hook u)
                  (funcall it production))))
    (if (numberp c)
        c
      (if (and (null (production-user-created production))
               (eq t (utility-pl u)))
          ;; compute for a compiled production
          (let ((m (+ (production-successes production)
                       (production-failures production))))
            (/ (+ (* (utility-ie u) (production-priorc production)) 
                  (if (zerop m) 0 (* m (compute-costs-regular u production))))
               (if (zerop (+ (utility-ie u) m))
                   1
                 (+ (utility-ie u) m))))
          
          ;; compute for a regular production
          (compute-costs-regular u production)))))


(defun compute-costs-regular (u production)
  (let ((s 0.0)
        (f 0.0)
        (e 0.0))
    (cond ((numberp (utility-pl u))
           (let ((minus-d (- (utility-pl u)))
                 (successes (production-success-list production))
                 (failures (production-failure-list production))
                 (efforts (production-efforts-list production))
                 (decay 0.0))
             (loop
               (when (or (and (null successes) (null failures))
                         (null efforts))
                 (return))
               (setf decay
                 (if (and successes
                          (or (null failures)
                              (> (first successes) 
                                 (first failures))))
                     (pop successes) 
                   (pop failures)))
               (setf decay (expt-coerced (max (utility-dat u)
                                              (- (mp-time) decay))
                                         minus-d))
               (incf s decay)
               (incf e (* (pop efforts) decay)))))
          (t
           (setf s (production-successes production))
           (setf f (production-failures production))
           (setf e (production-efforts production))))
    (/ e (+ s f))))
  
  
  
;;; update the values for compiled productions

(defun update-priors-for-production (u production oldp oldc)
  (let ((new-p (update-prior-parameter u (production-priorp production) oldp))
        (new-c (update-prior-parameter u (production-priorc production) oldc)))
    
    (setf (production-priorp production) new-p)
    (setf (production-priorc production) new-c)
    
    (when (car (no-output (sgp :pct)))
      (model-output "  Current values of priorP: ~9,6f  priorC: ~9,6f~%" new-p new-c))))

(defun update-prior-parameter (u prior old)
  (+ prior (* (utility-alpha u) (- old prior))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The functions that trigger a learning event 
;;; independent of the productions firing

(defun trigger-reward (value)
  (let ((utility (get-module utility)))
    (when utility
      (trigger-reward-internal value utility))))


(defun trigger-reward-internal (value u)
  "Does the bookkeeping work for the parameters independent of production firing"
  (cond ((null value)
         ;; just clear the history - no learning occurs
         
         (setf (utility-history u) nil))
        
        ((or (eq value :success)
             (eq value :failure))
         
         ;; register success or failure for all productions
         
         (dolist (production-and-time (utility-history u))
           (let* ((previous-production (car production-and-time))
                  (previous-time (cdr production-and-time))
                  (effort (- (mp-time) previous-time)))
             
             ;; always count successes, failures and efforts
             
             (if (eq value :success)
                 (incf (production-successes previous-production) 1.0)
               (incf (production-failures previous-production) 1.0))
             
             (incf (production-efforts previous-production) effort)
             
             ;; record details when learning with the decay
             
             (when (numberp (utility-pl u))
               
               (if (eq value :success)
                   (push previous-time (production-success-list previous-production))
                 (push previous-time (production-failure-list previous-production)))
               
               (push effort (production-efforts-list previous-production)))))
         
         (setf (utility-history u) nil))
        
        (t 
         (print-warning "Invalid value ~S passed to trigger-reward - no learning occurs." value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here's the actual module and associated support code

(defun utility-module-params (u param)
  (cond ((consp param)
         
         ;; Changing utility parameters may lead to
         ;; to possibly trigger conflict-resolution if it were waiting
         
         (un-delay-conflict-resolution)
         
         (when (and (> (length (all-productions)) 1)
                    (member (car param) '(:esc :g :dat :pl)))
           (model-warning "Changing procedural parameters when productions exist unsupported.")
           (model-warning "Results may not be what one expects."))
         
         (case (car param)
           (:esc (setf (utility-esc u) (cdr param)))
           
           (:g (setf (utility-g u) (cdr param)))
           (:egs (setf (utility-egs u) (cdr param)))
           (:ut (setf (utility-ut u) (cdr param)))
           (:dat (setf (utility-dat u) (cdr param)))
           (:pl (setf (utility-pl u) (cdr param)))
           
           (:ie (setf (utility-ie u) (cdr param)))
           (:alpha (setf (utility-alpha u) (cdr param)))
           (:initial-successes (setf (utility-initial-successes u) (cdr param)))
           (:initial-failures (setf (utility-initial-failures u) (cdr param)))
           (:initial-efforts (setf (utility-initial-efforts u) (cdr param)))
           
           
           (:utility-hook 
            (when (and (cdr param) (utility-utility-hook u))
              (print-warning 
               "Utility-hook was set to ~S and is being overwritten"
               (utility-utility-hook u)))
            (setf (utility-utility-hook u) (cdr param)))
            
           (:utility-p-hook 
            (when (and (cdr param) (utility-utility-p-hook u))
              (print-warning 
               "Utility-p-hook was set to ~S and is being overwritten"
               (utility-utility-p-hook u)))
            (setf (utility-utility-p-hook u) (cdr param)))
           
           (:utility-c-hook 
            (when (and (cdr param) (utility-utility-c-hook u))
              (print-warning 
               "Utility-c-hook was set to ~S and is being overwritten"
               (utility-utility-c-hook u)))
            (setf (utility-utility-c-hook u) (cdr param)))
           
           (:utility-learned-p-hook 
            (when (and (cdr param) (utility-utility-learned-p-hook u))
              (print-warning 
               "Utility-learned-p-hook was set to ~S and is being overwritten"
               (utility-utility-learned-p-hook u)))
            (setf (utility-utility-learned-p-hook u) (cdr param)))
           
           (:utility-learned-c-hook 
            (when (and (cdr param) (utility-utility-learned-c-hook u))
              (print-warning 
               "Utility-learned-c-hook was set to ~S and is being overwritten"
               (utility-utility-learned-c-hook u)))
            (setf (utility-utility-learned-c-hook u) (cdr param)))))
        (t 
         (case param
           (:g  (utility-g u))
           (:egs  (utility-egs u))
           (:ut (utility-ut u))
           (:dat (utility-dat u))
           (:pl (utility-pl u))
           
           
           (:ie (utility-ie u))
           (:alpha (utility-alpha u))
           (:initial-successes (utility-initial-successes u))
           (:initial-failures (utility-initial-failures u))
           (:initial-efforts (utility-initial-efforts u))
          
           
           (:utility-hook (utility-utility-hook u))
           (:utility-p-hook (utility-utility-p-hook u))
           (:utility-c-hook (utility-utility-c-hook u))
           
           (:utility-learned-p-hook (utility-utility-learned-p-hook u))
           (:utility-learned-c-hook (utility-utility-learned-c-hook u))))))


(defun reset-utility-module (u)
  (setf (utility-history u) nil))


(define-module-fct 'utility nil
  (list (define-parameter :esc :owner nil)
        
        (define-parameter :g :valid-test #'numberp :default-value 20.0
          :warning "a number" :documentation "G value in utility equation PG-C")
        (define-parameter :egs :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Expected Gain S")
        (define-parameter :ut :valid-test #'numornil :default-value 0.0
          :warning "a number" :documentation "Utility Threshold")
        (define-parameter :dat :owner nil)
        (define-parameter :pl :valid-test #'numorbool :default-value nil
          :warning "a number, T or nil" 
          :documentation "(Production) Parameters Learning")
        
        (define-parameter :ie :default-value 10.0
          :valid-test #'numberp :warning "a number"
          :documentation "Initial Experience")
        (define-parameter :alpha :default-value .2
          :valid-test #'numberp :warning "a number"
          :documentation "Production learning rate")
        (define-parameter :initial-successes :default-value 1.0
          :valid-test #'nonneg :warning "a positive number"
          :documentation "The number of successes for a newly learned production")
        (define-parameter :initial-failures :default-value 0.0
          :valid-test #'nonneg :warning "a positive number"
          :documentation "The number of failures for a newly learned production")
        (define-parameter :initial-efforts :default-value 0.05
          :valid-test #'nonneg :warning "a positive number"
          :documentation "The initial total efforts for a newly learned production")
        
        (define-parameter :utility-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Utility computation hook")
        
        (define-parameter :utility-p-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Utility computation p value hook")
        
        (define-parameter :utility-c-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Utility computation c value hook")
        
        (define-parameter :utility-learned-p-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Utility computation p value when learning enabled hook")
        
        (define-parameter :utility-learned-c-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Utility computation c value when learning enabled hook"))
  
  :version "1.0" 
  :documentation  "The utility module handles production utility computations"
    
  :creation (lambda (x) (declare (ignore x)) (make-utility))
  :reset #'reset-utility-module
  :params #'utility-module-params)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section is all support for spp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Taken and modified from ACT-R 5 code

(defun production-parameter-fct (production-name &optional parameters)
  "Returns the value of the production parameter(s), or print all if none given."
  (let ((esc (car (no-output (sgp :esc))))
        (pl (car (no-output (sgp :pl))))
        (value nil)
        (values nil))
    (cond (production-name
           (command-output "Parameters for production ~S:" production-name)
           
           (when pl
             
             ;; want to update values but not lose last used utility
             ;; Don't want noise computed 
             
             (let ((old-u (production-utility production-name))
                   (noise (car (no-output (sgp :egs)))))
               (no-output (sgp :egs 0.0))
               (compute-utility production-name)
               (setf (production-utility production-name) old-u)
               (no-output (sgp-fct (list :egs noise)))))
           
           (cond (parameters
                  (dolist (parameter parameters)
                    (setf value 
                      (case parameter
                        (:name production-name)
                        (:creation-time (production-creation-time production-name))
                        (:p (production-p production-name))
                        (:c (production-c production-name))
                        (:pg-c (production-pg-c production-name))
                        (:value (production-value production-name))
                        (:effort (production-effort production-name))
                        (:success (production-success production-name))
                        (:failure (production-failure production-name))
                        (:utility (production-utility production-name))
                            
                        (:successes (cons (production-successes production-name)
                                          (production-success-list production-name)))
                        
                        (:failures (cons (production-failures production-name)
                                         (production-failure-list production-name)))
                        
                        (:efforts (cons (production-efforts production-name)
                                        (production-efforts-list production-name)))
                        
                        (t (print-warning "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." parameter)
                           :error)))
                    (push-last value values)
                    (command-output " ~S ~6,3F" parameter value))
                  values)
                 (t
                  (command-output " :Effort ~6,3F" (production-effort production-name))
                  (when (and esc (numberp pl))
                    (command-output " :Creation-Time ~6,3F" (production-creation-time production-name)))
                    
                  (if esc
                      (command-output  " :P ~6,3F~% :C ~6,3F~% :PG-C ~6,3F"
                                      (production-p production-name) 
                                      (production-c production-name)
                                      (production-pg-c production-name))
                    (command-output " :Value ~6,3F" (production-value production-name)))
                  
                  
                  ;;; This should probably always print out as
                  ;;; a list regardless of conditions since 
                  ;;; that's what it always returns, but not sure
                  ;;; right now...
                  
                  (when pl
                    (if (numberp pl)
                        (command-output " :Successes ~6,3F~% :Failures ~6,3F~% :Efforts ~6,3F"
                                        (production-success-list production-name) 
                                        (production-failure-list production-name) 
                                        (production-efforts-list production-name))
                      (command-output " :Successes ~6,3F~% :Failures ~6,3F~% :Efforts ~6,3F"
                                      (production-successes production-name) 
                                      (production-failures production-name) 
                                      (production-efforts production-name)))
                    
                    (command-output " :Success ~6,3F~% :Failure ~6,3F"
                                    (production-success production-name) 
                                    (production-failure production-name)))
                  production-name)))
          (t :error))))

;;; This name is dangerously close to what I use in the sgp code, but
;;; for now I'll leave it as is.

(defmacro set-parameter (slot parameter test warning &rest housekeeping)
  "Sets parameter of production p in slot if value passes test, otherwise issue warning."
  `(cond (,test
          (setf (,slot p) value)
          ,@housekeeping
          value)
         (t
          (print-warning
           "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
           ,parameter value ,warning)
          :error)))

;;; I also use a version of these in declarative, so should
;;; merge those at some point in support, but for now I'll use different
;;; names between the two

(defun p-even-references (start end n &optional (m n))
  "Distributes m references evenly along n intervals between start and end."
  (when (plusp n)
    (let ((decrement (/ (- end start) n))
          (time end)
          (times nil))
      (dotimes (i (round m) times)
        (decf time decrement)
        (push-last time times)))))

(defun p-adapt-references (references creation-time)
  "If optimized learning is off, then erase all reference times.
   If on, then generate equidistant references since creation time.
   Generalize to a fixed number of references."
  (cond ((numberp (car (no-output (sgp :pl))))
         (p-even-references creation-time
                            (if (null (rest references)) 
                                (mp-time)
                              (first (last references)))
                            (- (first references)
                               (length (rest references)))))
        (t
         nil)))


(defun parameters-fct (p parameters)
  "Sets the parameters of the production (internal - user should use spp)."
  
  ;; Changing procedural parameters reschedules conflict resolution
  ;; if it's waiting to happen
         
  (un-delay-conflict-resolution)
         
  ;; Having the name of the production be p and the
  ;; value to be set called value are critical to
  ;; the functioning of the set-parameter macro...
  
  (let ((values nil)
        (pl (car (no-output (sgp :pl)))))
    (if p
        (loop
          
          (unless parameters 
            ;; recompute values without losing last utility
            ;; or computing new noise
            
            
            (let ((old-u (production-utility p))
                  (noise (car (no-output (sgp :egs)))))
              (no-output (sgp :egs 0.0))
              (compute-utility p)
              (setf (production-utility p) old-u)
              (no-output (sgp-fct (list :egs noise))))
            
            (return values))
        
        (let* ((parameter (pop parameters))
               (value (pop parameters)))
          
          ;; not sure about this, but I'll leave it in for now
          
          (when (and (listp value) 
                     (eq (first value) 'quote))
            (setf value (second value)))  
          
          (push-last
           (case parameter
             (:name
              (print-warning "PARAMETER NAME CANNOT BE SET.")
              :error)
             (:creation-time
              (set-parameter production-creation-time :creation-time
                             (numberp value) "a number"))
             
             (:value
              (set-parameter production-value :value
                             (numberp value)
                             "a number"))
             (:pg-c
              (print-warning 
               "PG-C CANNOT BE SET DIRECTLY: SET INDIVIDUAL PARAMETERS.")
              :error)
             (:p
              (cond (pl
                     (print-warning "P CANNOT BE SET DIRECTLY WHEN PARAMETERS LEARNING IS ON")
                     (print-warning "SET SUCCESSES AND FAILURES INSTEAD.")
                     :error)
                    (t
                     (set-parameter production-p :p
                                    (and (numberp value) 
                                         (>= value 0.0) (<= value 1.0))
                                    "a number between 0.0 and 1.0"))))
             (:c
              (cond (pl
                     (print-warning "C CANNOT BE SET DIRECTLY WHEN PARAMETERS LEARNING IS ON")
                     (print-warning "SET SUCCESSES AND FAILURES INSTEAD.")
                     :error)
                    (t
                     (set-parameter production-c :c
                                    (and (numberp value) 
                                         (>= value 0.0))
                                    "a non-negative number"))))
             (:successes
              (cond ((numberp value)
                     (setf (production-successes p) value)
                     (setf (production-success-list p)
                       (p-adapt-references (list value) (production-creation-time p)))
                     (cons (production-successes p)
                           (production-success-list p)))
                    ((and (listp value) (every #'numberp value))
                     (setf (production-successes p) (length value))
                     (setf (production-success-list p)
                       (p-adapt-references (cons (length value) value) (production-creation-time p)))
                     (cons (production-successes p)
                           (production-success-list p)))
                    (t
                     ;; CAN'T rely on set-parameter to print the warning
                     ;; in LispWorks...
                     ;;(set-parameter nil :successes nil 
                     ;;               "a number or list of numbers")
                     
                     (print-warning "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
                                    :successes value "a number or list of numbers")
                     
                     :error)))
             (:failures
              (cond ((numberp value)
                     (setf (production-failures p) value)
                     (setf (production-failure-list p)
                       (p-adapt-references (list value) (production-creation-time p)))
                     (cons (production-failures p)
                           (production-failure-list p)))
                    ((and (listp value) (every #'numberp value))
                     (setf (production-failures p) (length value))
                     (setf (production-failure-list p)
                       (p-adapt-references (cons (length value) value) (production-creation-time p)))
                     (cons (production-failures p)
                           (production-failure-list p)))
                    (t
                     ;; rely on set-parameter to print the warning
                     ;(set-parameter nil :failures nil 
                     ;               "a number or list of numbers")
                     
                     (print-warning "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
                                    :failures value "a number or list of numbers")
                     :error)))
             (:efforts
              (cond ((numberp value)
                     (setf (production-efforts p) value)
                     (setf (production-efforts-list p)
                       (when (numberp pl)
                         (let ((ratio (round 
                                       ;; Previously it was only successes
                                       ;; but I think it should be
                                       ;; both successes and failures
                                       
                                       (+ (production-successes p)
                                          (production-failures p)))))
                           (make-list ratio 
                                      :initial-element (/ value ratio)))))
                     (cons (production-efforts p)
                           (production-efforts-list p)))
                    ((and (listp value) (every #'numberp value))
                     (setf (production-efforts p) (apply #'+ value))
                     (setf (production-efforts-list p)
                       (when (numberp pl) value))
                     (cons (production-efforts p)
                           (production-efforts-list p)))
                    (t
                     ;; rely on set-parameter to print the warning
                     ;(set-parameter nil :efforts nil 
                     ;               "a number or list of numbers")
                     
                     (print-warning "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
                                    :efforts value "a number or list of numbers")
                     
                     :error)))
             
             (:effort
              (set-parameter production-effort :effort
                             (or (and (numberp value) (>= value 0.0))
                                 (null value))
                             "NIL or a non-negative number"))
             (:success
              (set-parameter production-success :success
                             (or (null value) (eq value t))
                             "T or NIL"))
             (:failure
              (set-parameter production-failure :failure
                             (or (null value) (eq value t))
                             "T or NIL"))
             (t
              (print-warning
               "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." parameter)
              :error))
           values)))
      :error)))


(defmacro spp (&rest production-parameters)
  "Inspects and sets production parameters."
  `(spp-fct ',production-parameters))


(defun spp-fct (parameters)
  "Inspects and sets production parameters."
  (let ((results nil))  
    (if (null parameters) ; print all parameters for all productions
        (dolist (production (all-productions))
          (push-last (production-parameter-fct production) results))
      (dolist (description (if (or (keywordp (first parameters))
                                   (keywordp (second parameters))
                                   (and (listp (first parameters))
                                        (null (second parameters))
                                        (not (keywordp 
                                              (second 
                                               (first parameters))))))
                               (list parameters) parameters))
        (when (atom description) (setf description (list description)))
        (if (keywordp (first description))
            (dolist (production (all-productions))
              (push-last
               (if (and (cdr description) 
                        (not (keywordp (second description))))
                   (parameters-fct production description)
                 (production-parameter-fct production description))
               results))
          
          (dolist (production (if (atom (first description))
                                  (list (first description))
                                (first description)))
            (if (get-production production)
                (push-last
                 (if (and (cddr description) 
                          (not (keywordp (third description))))
                     (parameters-fct production (rest description))
                   (production-parameter-fct production (rest description)))
                 results)
              (progn
                (model-warning "Spp cannot adjust parameters because production ~S does not exist" production)
                (push-last :error results)))))))
    results))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code from production compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This needs to be incorporated into the general utility
;;; code instead of tied in by hooks at some point.











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
