;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2015 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : second-retrieval-buffer.lisp
;;; Version     : 0.1a
;;; 
;;; Description : A module which adds a new buffer that works like the retrieval 
;;;             : buffer to get chunks from DM using the simulate-retrieval-request
;;;             : command.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2015.07.23 Dan
;;;             : * Initial creation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This example module creates a buffer named r2 which can be used like the
;;; retrieval buffer to retrieve chunks from declarative memory in parallel with
;;; the existing retrieval buffer.  It uses the same timing and activation 
;;; parameters as the declarative memory module, but that could be changed as
;;; needed (timing being easy to change but activation parameter changes may require
;;; a little more work to either change the declarative parameter and restore
;;; on the fly or add appropriate hooks to modify the calculations).
;;;
;;; The module uses the simulate-retrieval-request command to do the real work,
;;; and sets the seed to reflect the processing which occurred.  If the :recently-
;;; retrieved request parameter is provided that will reflect the current set of
;;; finsts in the declarative module, but this request will not change that set
;;; of finsts i.e. the retrievals through this buffer will not be marked with a
;;; finst.  If a seprate finst set is needed that would be possible to add here
;;; by keeping a list of finsts in this module itself and then stripping the
;;; parameter from the request before passing it on to simulate-retrieval-request
;;; and filtering the results afterwards based on the requested result.  If
;;; however one wants there to be one set of finsts which both this and the
;;; normal retrievals access that would require changing the declarative module
;;; code as well to record the changes (not recommended). 
;;;
;;; This module has its own state flags which are independent of those of the
;;; declarative module and retrieval buffer.  It will set the buffer failure flag
;;; and state error when no chunk is retrieved.  If it is jammed it will still
;;; process the new request and it does not attempt to eliminate the previous
;;; one's result.  Thus, both chunks will end up being set in the buffer (assuming
;;; that both were successful retrievals) at the appropriate times based on their
;;; activations and that means the order of the setting depends on the activations
;;; and the second one may overwrite the first before the model can use it.  The
;;; busy flag will also clear when the first of the two is "retrieved" resulting
;;; in potentially undetectable jamming since a pending retrieval may still be
;;; scheduled after the busy flag is cleared.  Additional logic could be added to
;;; the module to adjust all that, but for simplicity of the example it doesn't
;;; do so.
;;;
;;; This module does not report the activation trace because it suppresses the
;;; output of simulate-retrieval-request, but a parameter could be added to 
;;; toggle whether that was suppressed or not.  However, it would only be the
;;; "low" trace since that's all simulate-retrieval-request outputs.
;;;
;;; Some of the places where one might want to adjust how this module works are 
;;; noted in comments, but those aren't the only changes one could make.
;;; 
;;; To use this module this file could be placed into the modules directory of
;;; the distribution before loading.
;;;
;;; This module is provided only as an example and does represent a theoretical
;;; claim that such a capability does or should exist.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Standard package checks all automatically loaded ACT-R files should have.

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; A structure to hold the module's parameter and query flags.

(defstruct second-retrieval-buffer busy error rt lf le esc)


;;; Creation function just returns a new structure.

(defun create-second-retrieval-buffer (model-name)
  (declare (ignore model-name))
  (make-second-retrieval-buffer))


;;; Reset function just clears the busy and error flags.

(defun second-retrieval-buffer-reset (instance)
  (setf (second-retrieval-buffer-busy instance) nil)
  (setf (second-retrieval-buffer-error instance) nil))

;;; The query function reports the state flags.

(defun second-retrieval-buffer-query (instance buffer query value)
  (if (eq query 'state)
      (case value
        (busy (second-retrieval-buffer-busy instance))
        (free (not (second-retrieval-buffer-busy instance)))
        (error (second-retrieval-buffer-error instance)))
    (model-warning "Invalid query ~a ~a made to the ~a buffer" query value buffer)))

;;; Function to handle the demo module's parameters which are just
;;; being passed from elsewhere since they aren't owned so they'll always
;;; be a cons and not a request for a value.  Do the second to ms
;;; conversion on :lf here.

(defun second-retrieval-buffer-params (instance param)
  (case (car param)
    (:esc (setf (second-retrieval-buffer-esc instance) (cdr param)))
    (:rt (setf (second-retrieval-buffer-rt instance) (cdr param)))
    (:lf (setf (second-retrieval-buffer-lf instance) (seconds->ms (cdr param))))
    (:le (setf (second-retrieval-buffer-le instance) (cdr param)))))

;;; The request function

(defun second-retrieval-buffer-request (instance buffer chunk-spec)
  
  (when (second-retrieval-buffer-busy instance)
    (model-warning "Second-retrieval-buffer received a request while it was still busy."))
  
  ;; set the busy flag and clear the error flag
  (setf (second-retrieval-buffer-busy instance) t)
  (setf (second-retrieval-buffer-error instance) nil)
  
  (multiple-value-bind (results seed) 
      ;; convert the chunk-spec back to a flat list and simulate the retrieval request
      (no-output (simulate-retrieval-request-plus-seed-fct (slot-specs-to-chunk-spec-list (chunk-spec-slot-spec chunk-spec))))
    ;; update the seed value to where it would be after that request happened
    (no-output (sgp-fct (list :seed seed)))
    
    ;; check that something was retrievable i.e. above the threshold
    (if (and (car results) (>= (chunk-activation (car results)) (second-retrieval-buffer-rt instance)))
        
        ;; if so schedule an event to set the chunk in the buffer and clear the busy flag
        (schedule-event-relative (if (second-retrieval-buffer-esc instance)
                                     ;; use the default latency computation from the declarative module
                                     (round (* (second-retrieval-buffer-lf instance)
                                               (exp (* -1 (second-retrieval-buffer-le instance) 
                                                       (chunk-activation (car results))))))
                                   0)
                                 'second-retrieval-buffer-retrieved-chunk
                                 :time-in-ms t 
                                 :module 'second-retrieval-buffer 
                                 :destination 'second-retrieval-buffer 
                                 :params (list (car results))
                                 
                                 :output 'medium)
      ;; otherwise schedule an event to clear the busy flag and set the failure and error flags
      (schedule-event-relative (if (dm-esc dm)
                                   (round (* (second-retrieval-buffer-lf instance)
                                               (exp (* -1 (second-retrieval-buffer-le instance) 
                                                       (second-retrieval-buffer-rt instance)))))
                                 0)
                               'second-retrieval-buffer-failure
                               :time-in-ms t 
                               :module 'second-retrieval-buffer 
                               :destination 'second-retrieval-buffer
                               :output 'low))))


(defun second-retrieval-buffer-retrieved-chunk (instance chunk)
  
  ;; Clear the busy flag
  
  (setf (second-retrieval-buffer-busy instance) nil)
  
  ;; Schedule an event to put the chunk into the buffer right now instead of
  ;; placing it there directly to comply with the guideline that buffer changes
  ;; should be scheduled.
  
  (schedule-set-buffer-chunk 'r2 chunk 0 :time-in-ms t :module 'second-retrieval-buffer :priority :max))


(defun second-retrieval-buffer-failure (instance)
  
  ;; Clear the busy flag and set the buffer failure and error flag.
  
  (setf (second-retrieval-buffer-busy instance) nil)
  (setf (second-retrieval-buffer-error instance) t)
  (set-buffer-failure 'r2))

  
  
  
  
;;; The actual module definition call specifying the components.  
  
(define-module-fct 'second-retrieval-buffer 
    ;; the module has one buffer called r2 which accepts the :recently-retrieved request parameter
    (list '(r2 nil (:recently-retrieved)))
  ;; record parameters from other modules
  (list (define-parameter :esc :owner nil)
        (define-parameter :lf :owner nil)
        (define-parameter :le :owner nil)
        (define-parameter :rt :owner nil))
  :version "0.1a"
  :documentation "Example module with a buffer that accesses DM similar to the retrieval buffer"
  :creation 'create-second-retrieval-buffer
  :reset 'second-retrieval-buffer-reset
  :query 'second-retrieval-buffer-query
  :params 'second-retrieval-buffer-params
  :request 'second-retrieval-buffer-request)  

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
