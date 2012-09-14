;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2009 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : bl-inhibition.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : A module to add the short term base-level activation inhibition 
;;;             : component described in:
;;;
;;;             Lebiere, C., & Best, B. J. (2009). Balancing Long-Term Reinforcement and
;;;             Short-Term Inhibition.  Proceedings of the 31st Annual Conference of the
;;;             Cognitive Science Society (pp. 2378-2383).  Austin, TX: Cognitive Science
;;;             Society.
;;;
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2009.08.05 Dan [1.0a1]
;;;             : * Initial creation.
;;; 2011.04.25 Dan
;;;             : * Updated because of the change to millisecond times being
;;;             :   recorded in DM parameters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; To use this extension put this file into the modules directory before loading
;;; the main ACT-R load file, or load it once after loading ACT-R and before defining
;;; any models.
;;;
;;; With this module added to the system chunk activations now have an additional
;;; term added to the equation when the :enable-inhibition parameter is set to t
;;; (its default value) and base-level learning is enabled with at least one
;;; past reference being included (either :ol nil or a number):
;;;
;;;                    - log [1 + (Tn/ts)^-ds]
;;;
;;;  Tn is the time since the most recent reference of the chunk
;;;  ds is the value of the inhibition-decay parameter
;;;  ts is the value of the inhibition-scale parameter
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; New parameters :enable-inhibition, :inhibition-scale and :inhibition-decay.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;;
;;; Relies on the new activation offsets hook to add a component to the
;;; activation calculation.
;;; 
;;; Would like to use sdp to get the reference time instead of pulling it from 
;;; the chunk parameter directly to be safe with respect to possible future changes 
;;; in declarative parameter representations, but can't because sdp forces a
;;; recomputation of the activation which would call this again and recurse
;;; infinitely (or until the stack/heap exhausts).
;;;
;;; The default values for the parameters are suggestions from Christian, but
;;; are not necessarily good values for all situations.  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defstruct bl-inhibition enabled scale decay)

(defun bl-inhibition-term (chunk)
  (let ((bl (get-module base-level-inhibition))
        (reference (car (chunk-reference-list chunk))))
    (when (and bl (numberp reference) (bl-inhibition-enabled bl))
      (- (log-coerced (+ 1.0 (expt-coerced (/ (max .05 (ms->seconds (- (mp-time-ms) reference))) (bl-inhibition-scale bl))
                                           (- (bl-inhibition-decay bl)))))))))

(defun bl-inhibition-params (module param)
  (cond ((consp param)
         
         (case (car param)
           (:enable-inhibition (setf (bl-inhibition-enabled module) (cdr param)))
           (:inhibition-scale (setf (bl-inhibition-scale module) (cdr param)))
           (:inhibition-decay (setf (bl-inhibition-decay module) (cdr param)))
           
           ;; force our function onto the offset list whenever
           ;; it's changed and we're not on it
           (:activation-offsets
            
            (unless (or (find 'bl-inhibition-term (cdr param))
                        (find 'bl-inhibition-term (no-output (sgp :activation-offsets))))
              (sgp :activation-offsets bl-inhibition-term)))))
        (t 
         (case param
           (:enable-inhibition (bl-inhibition-enabled module))
           (:inhibition-scale  (bl-inhibition-scale module))
           (:inhibition-decay  (bl-inhibition-decay module))))))
          

(define-module-fct 'base-level-inhibition
    nil
  (list (define-parameter :enable-inhibition :owner t :default-value t :valid-test #'tornil
          :documentation "Enable base-level inhibition calculation"
          :warning "T or nil")
        (define-parameter :inhibition-scale :owner t :default-value 5 :valid-test #'plusp
          :warning "a positive number"
          :documentation "Base-level inhibition scale (ts)")
        (define-parameter :inhibition-decay :owner t :default-value 1.0 :valid-test #'plusp
          :warning "a positive number"
          :documentation "Base-level inhibition decay (ds)")
        (define-parameter :activation-offsets :owner nil))
  
  :creation (lambda (name) (declare (ignore name)) (make-bl-inhibition))
  :params 'bl-inhibition-params
  :version "1.0a1" 
  :documentation 
  "Module to add the option of the Lebiere & Best base-level inhibition component to activation calculations"
  )

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
