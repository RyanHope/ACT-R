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
;;; Filename    : spacing-effect.lisp
;;; Version     : 1.0
;;; 
;;; Description : A module to allow one to toggle the base-level learning
;;;             : equation from the default to the one proposed by 
;;;             : Pavlik and Anderson in Cognitive Science 29 (2005) 559-586.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2005.08.31 Dan
;;;             : * Creation for David Peebles.
;;; 2010.11.15 Dan
;;;             : * Took out the :bll setting since it's no longer necessary
;;;             :   and now generates a warning.
;;;             : * Except, it is still necessary...  So, now it's set to a 
;;;             :   large positive value instead.
;;;             : * Also added a reset function to turn off eblse before the
;;;             :   default parameters get set since declarative could go first
;;;             :   which would cause warnings from those default settings.
;;; 2011.04.25 Dan
;;;             : * Updated since DM uses millisecond times internally now.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending chunks at initial load.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; When the :eblse (enable base level spacing effect) paramter is set to t
;;; base level activation is computed using the new equation.
;;;
;;; To do so, :ol (optimized learning) must be set to nil and the model 
;;; must not be using the :bl-hook functionality (warnings will be printed if
;;; violations of either of those conditions are noticed).  In addition,
;;; the setting of :bll by the model will be ignored and it will be set to
;;; the value 91923.12 to enable the computation of base levels (since that 
;;; parameter is both the switch and the parameter value) and provide a value 
;;; that I can check against for saftey testing (since it's very unlikely that 
;;; a user will every set that specific value).
;;;
;;; The new equation for base level activation is similar to the old one:
;;;
;;;           n
;;; B[n] = ln( sum t[i]^-d[i])
;;;          i=1
;;;
;;; The only difference being that the decay exponent value is no longer
;;; a constant, but is now based on the previous base level values by this
;;; equation:
;;;
;;; d[i] = c*e^m[i-1] + a
;;;
;;; whre m[i] = B[i] + (permanant noise of the chunk)
;;;
;;; The paramters :se-intercept (spacing effect intercept) and :se-scale
;;; (spacing effect scale) control the new computation for a and c respectively.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; New parameters :eblse, :se-intercept, and :se-scale only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;;
;;; Because this was never actually implemented in ACT-R before, I asked Phil 
;;; which activation to use for m in the general case. His thoughts were that 
;;; it should only be baseed on the history of use of the chunk. That suggests 
;;; using only the base level computation plus any permanent noise present.  
;;; Thus, spreading activation, partial matching, and transient noise should 
;;; not be considered for this.
;;;
;;; It does not support changing the c and a parameters "on the fly".  Once
;;; the model has created a reference those decay values are cached and it 
;;; does not recompute them - thus changing c and a other than at the start
;;; of the model will produce "wrong" results (at least what I would consider
;;; to be wrong values because the decays wouldn't all be generated from the
;;; current settings).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defstruct spacing-effect
  enabled
  scale
  intercept)

(defun add-decay-value (chunk1 chunk2)
  "Add another d to the list based on the last m of the pre-existing chunk
   and put the initial value there if there wasn't one already."
  (declare (ignore chunk2))
  (let* ((module (get-module spacing-effect))
         (res (if (null (chunk-decays chunk1))
                  (list (spacing-effect-intercept module))
                (chunk-decays chunk1))))
    
    (aif (chunk-last-m chunk1)
         (append (list (+ (* (spacing-effect-scale module) 
                             (exp it))
                          (spacing-effect-intercept module)))
                 res)
         
         nil ; If it doesn't have a last-m then punt and
             ; rely on a complete recomputation next time it's needed
         )))

(suppress-extension-warnings)   

(extend-chunks decays :default-value nil 
               :copy-function copy-list
               :merge-function add-decay-value)

(extend-chunks last-m :default-value nil
               :copy-function identity)

(unsuppress-extension-warnings)

(defun compute-spacing-effect-activation (chunk)
  (let ((module (get-module spacing-effect))
        (ct (mp-time-ms))
        (value 0.0))
    
    (if (= (length (chunk-reference-list chunk))
           (length (chunk-decays chunk)))
        (progn
          (mapcar #'(lambda (reference decay)
                      (incf value 
                            (expt-coerced (max .05 (ms->seconds (- ct reference)))
                                          (- decay))))
            (chunk-reference-list chunk)
            (chunk-decays chunk))
          
          (setf value (log-coerced value))
          (setf (chunk-last-m chunk) (+ value (chunk-permanent-noise chunk)))
          value)
          
      ;; Compute the decay list and try again
      ;; painfully slow, but it shouldn't have to do this
      ;; since the merging should maintain the list
      ;; correctly - it's just here as a saftey net
      ;; in the event someone sets references directly.
      
      (let* ((decays (list (spacing-effect-intercept module)))
             (references (reverse (chunk-reference-list chunk)))
             (new-references (list (pop references))))
      
        (dolist (reference references)
          (let ((value 0.0)
                (ct reference)
                )
            
            (mapcar #'(lambda (reference decay)
                      (incf value 
                            (expt-coerced (max .05 (ms->seconds (- ct reference)))
                                          (- decay))))
              new-references
              decays)
            (push (+ (* (spacing-effect-scale module) 
                        (exp (+ (log-coerced value) (chunk-permanent-noise chunk))))
                     (spacing-effect-intercept module))
                  decays)
            (push reference new-references)))
      
      (setf (chunk-decays chunk) decays)
      
      (compute-spacing-effect-activation chunk)))))


(defun spacing-effect-params (module param)
  (cond ((consp param)
         
         (case (car param)
           (:ol
            (when (and (cdr param)
                       (spacing-effect-enabled module))
              (model-warning "Cannot turn on :ol when :eblse enabled")
              (no-output (sgp :ol nil))))
           (:bll 
            (when (spacing-effect-enabled module)
              (cond ((and (numberp (cdr param)) (not (= (cdr param) 91923.12)))
                     (model-warning "Changing :bll has no effect when :eblse is enabled")
                     (no-output (sgp :bll 91923.12)))
                    ((not (numberp (cdr param)))
                     (model-warning "Cannot turn off :bll while :eblse is enabled")
                     (no-output (sgp :bll 91923.12))))
              ))
           (:bl-hook 
            (when (and (spacing-effect-enabled module)
                       (not (equal (cdr param) 'compute-spacing-effect-activation)))
              (model-warning "Cannot change the :bll-hook when :eblse enabled")
              (no-output (sgp :bl-hook compute-spacing-effect-activation)))
            )
           (:eblse (setf (spacing-effect-enabled module) (cdr param))
                    (no-output 
                     (when (cdr param)
                       (sgp :bll 91923.12)
                       (sgp :ol nil)
                       (sgp :bl-hook compute-spacing-effect-activation)))
                   (cdr param))
                     
                     
                     
           (:se-intercept (setf (spacing-effect-intercept module) (cdr param)))
           (:se-scale (setf (spacing-effect-scale module) (cdr param)))))
        (t 
         (case param
           
           (:eblse (spacing-effect-enabled module))
           (:se-intercept (spacing-effect-intercept module))
           (:se-scale (spacing-effect-scale module))))))


(defun reset-spacing-effect-module (module)
  (setf (spacing-effect-enabled module) nil))

(define-module-fct 'spacing-effect 
    nil
  (list (define-parameter :eblse :owner t :default-value nil :valid-test #'tornil
          :documentation "Enable base level spacing effect - turning this on replaces the base level equation with one sensitive to spacing effects"
          :warning "T or nil")
        (define-parameter :se-intercept :owner t :default-value .5 :valid-test #'numberp
          :warning "a number"
          :documentation "Spacing effect intercept parameter (a)")
        (define-parameter :se-scale :owner t :default-value 0 :valid-test #'numberp
          :warning "a number"
          :documentation "Spacing effect scale parameter (c)")
        (define-parameter :ol :owner nil)
        (define-parameter :bl-hook :owner nil)
        (define-parameter :bll :owner nil))
  
  :creation (lambda (name) (declare (ignore name)) (make-spacing-effect))
  :reset #'reset-spacing-effect-module
  :params #'spacing-effect-params
  :version "1.0" 
  :documentation 
  "Module to add the option of the Pavlik & Anderson spacing effect equation for base level activation"
  )


(defun test-spacing-effect-1 ()
  "Test the equation against the example in the paper for a sequence of retrievals"
  (clear-all)
  (define-model foo)
  (sgp :esc t :eblse t :v t :act t :se-intercept 0.177 :se-scale .217)
  (add-dm (g isa chunk))
  (run-until-time 126)
  ;; fake a retrieval to get the times specific for the reference
  (compute-activation (get-module declarative) 'g nil)
  (set-buffer-chunk 'retrieval 'g)
  (clear-buffer 'retrieval)
  
  (run-until-time 252)
  ;; fake a retrieval to get the times specific for the reference
  (compute-activation (get-module declarative) 'g nil)
  (set-buffer-chunk 'retrieval 'g)
  (clear-buffer 'retrieval)
  
  (run-until-time 4844)
  ;; fake a retrieval to get the times specific for the reference
  (compute-activation (get-module declarative) 'g nil)
  (set-buffer-chunk 'retrieval 'g)
  (clear-buffer 'retrieval)
  
  (run-until-time 5877)
  ;; fake a retrieval to get the times specific for the reference
  (compute-activation (get-module declarative) 'g nil)
  (pprint (chunk-decays 'g))
  )




(defun test-spacing-effect-2 ()
  "Test the equation using a pre-specified reference list for a chunk"
  
  (clear-all)
  (define-model foo)
  (sgp :esc t :eblse t :v t :act t :se-intercept 0.177 :se-scale .217)
  (add-dm (g isa chunk))
  (sdp g :references (4844 252 126 0))
  (run-until-time 5877)
  (compute-activation (get-module declarative) 'g nil)
  (pprint (chunk-decays 'g))
  )


(defun test-spacing-effect-3 ()
  "Compare the default with the spacing to the original equation - should match"
  (clear-all)
  (define-model foo)
  (sgp :esc t :v t :act t :bll .5 :ol nil)
  (add-dm (g isa chunk))
  (sdp g :references (4844 252 126 0))
  (run-until-time 5877)
  (compute-activation (get-module declarative) 'g nil)
  
  (clear-all)
  (define-model foo)
  (sgp :esc t :eblse t :v t :act t)
  (add-dm (g isa chunk))
  (sdp g :references (4844 252 126 0))
  (run-until-time 5877)
  (compute-activation (get-module declarative) 'g nil)
  (pprint (chunk-decays 'g))
)
  
  
  
  
#|
CG-USER(130): (test-spacing-effect-1 )
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
   126.000   PROCEDURAL             CONFLICT-RESOLUTION 
   126.000   ------                 Stopped because time limit reached 
Computing activation for chunk G
Computing base-level
base-level hook returns: -0.8560219
Adding transient noise 0.0
Adding permanent noise 0.0
Chunk G has an activation of: -0.8560219
   252.000   PROCEDURAL             CONFLICT-RESOLUTION 
   252.000   ------                 Stopped because time limit reached 
Computing activation for chunk G
Computing base-level
base-level hook returns: -0.43415272
Adding transient noise 0.0
Adding permanent noise 0.0
Chunk G has an activation of: -0.43415272
  4844.000   PROCEDURAL             CONFLICT-RESOLUTION 
  4844.000   ------                 Stopped because time limit reached 
Computing activation for chunk G
Computing base-level
base-level hook returns: -0.9314301
Adding transient noise 0.0
Adding permanent noise 0.0
Chunk G has an activation of: -0.9314301
  5877.000   PROCEDURAL             CONFLICT-RESOLUTION 
  5877.000   ------                 Stopped because time limit reached 
Computing activation for chunk G
Computing base-level
base-level hook returns: -0.61873615
Adding transient noise 0.0
Adding permanent noise 0.0
Chunk G has an activation of: -0.61873615

(0.26249582 0.31757548 0.2691922 0.177)
CG-USER(131): (test-spacing-effect-2)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
  5877.000   PROCEDURAL             CONFLICT-RESOLUTION 
  5877.000   ------                 Stopped because time limit reached 
Computing activation for chunk G
Computing base-level
base-level hook returns: -0.61873615
Adding transient noise 0.0
Adding permanent noise 0.0
Chunk G has an activation of: -0.61873615

(0.26249582 0.31757548 0.2691922 0.177)

CG-USER(140): (test-spacing-effect-3)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
  5877.000   PROCEDURAL             CONFLICT-RESOLUTION 
  5877.000   ------                 Stopped because time limit reached 
Computing activation for chunk G
Computing base-level
Starting with blc: 0.0
Computing base-level from 4 references (4844 252 126 0)
  creation time: 0.0 decay: 0.5  Optimized-learning: NIL
base-level value: -2.649625
Total base-level: -2.649625
Adding transient noise 0.0
Adding permanent noise 0.0
Chunk G has an activation of: -2.649625
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
  5877.000   PROCEDURAL             CONFLICT-RESOLUTION 
  5877.000   ------                 Stopped because time limit reached 
Computing activation for chunk G
Computing base-level
base-level hook returns: -2.649625
Adding transient noise 0.0
Adding permanent noise 0.0
Chunk G has an activation of: -2.649625

(0.5 0.5 0.5 0.5)
|#

                         
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
