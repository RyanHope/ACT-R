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
;;; Filename    : all-components-module.lisp
;;; Version     : 1.0b2
;;; 
;;; Description : A module which uses all the options that were available in '08,
;;;               but there are more now which should be added to this at some
;;;               point like multi-buffers and the run-start/run-stop options.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2008.09.26 Dan
;;;             : * Initial creation.
;;; 2012.03.23 Dan [1.0b2]
;;;             : * Fixed bugs that someone just pointed out...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This example module will use all of the possible components
;;; for a module.  It will perform actions that are similar to
;;; the goal buffer but will also print out a trace of when the 
;;; particular functions are called and what the parameters to
;;; those functions are as the corresponding model is run.
;;;
;;; The module will be called demo.  It will have two buffers 
;;; called demo1 and demo2.  It will add a parameter called
;;; :demo-param and will also monitor the :esc parameter.
;;; It will adjust the do-not-harvest parameter so that
;;; the demo1 buffer does not get strict harvested.
;;; 
;;; The demo1 buffer will have the default spreading activation
;;; parameter and value.  It will have no specific request 
;;; parameters or additional queries.
;;;
;;; The demo2 buffer will have a spreading activation parameter
;;; called demo2-spread with a default value of 3.  It will
;;; accept a request parameter called :value.  It will respond
;;; to an additional query called detect-jam with values of t
;;; or nil, and it will print out that query's status with the 
;;; buffer-status info.
;;;
;;; Requests to the demo1 buffer will act like goal buffer
;;; requests and a new chunk will be created immediately for
;;; that buffer.
;;;
;;; Modification requests to the demo1 buffer will also act like
;;; goal buffer modification requests and immediately make the
;;; requested changes.
;;;
;;; Requests to the demo2 buffer must be of the type create-chunk.
;;; They will create a chunk of type result in the demo2 buffer
;;; after 100 ms pass.  If the value request parameter is specified
;;; with the = modifier (the default if no other modifier provided) 
;;; in the request then the chunk created will have that value in
;;; the answer slot of the chunk created. 
;;;
;;; Modification requests to the demo2 buffer are only accepted
;;; if the chunk in the demo2 buffer is of type result.  If it
;;; is, then the answer slot of that chunk will be changed to the one 
;;; provided in the modification request after the amount of time 
;;; specified by the :demo-param parameter passes.
;;; 
;;; The demo2 buffer can only process one request or modification 
;;; request at a time.  If a second request is received while
;;; it is handling one a warning will be output, the second request
;;; will be ignored and it will signal its detect-jam query.
;;;
;;; Queries to the demo1 buffer will always respond as busy nil,
;;; free t, and error nil. 
;;;
;;; Queries to the demo2 buffer will report as busy during a
;;; request or a modification request and free otherwise.  It
;;; will report an error if it receives an invalid request or modification
;;; request and that error will remain true until a valid request or 
;;; modification request is made.  It will report detect-jam as t if it
;;; receives overlapping reqests.  The detect-jam will remain 
;;; true until the current request completes.
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

(defstruct demo-module model-name busy error jammed param)


;;; Creation function just returns a new demo-module structure with
;;; the model's name recorded.

(defun create-demo-module (model-name)
  (model-output "Creating a demo module for model ~S" model-name)
  (make-demo-module :model-name model-name))

;;; Deletion function does nothing for demo module except print it was called.

(defun delete-demo-module (instance)
  (model-output "Deletion of demo module called for instance in model ~S."
                (demo-module-model-name instance)))

;;; Primary reset function used to create the chunk-types used
;;; for requests and return chunks and to clear the module's
;;; state flags.

(defun demo-primary-reset (instance)
  (model-output "Demo module's primary reset function called.")
  
  ;; create the chunk-types used by the module
  (chunk-type create-chunk)
  (chunk-type result answer)
  
  ;; clear its internal flags
  (setf (demo-module-busy instance) nil)
  (setf (demo-module-error instance) nil)
  (setf (demo-module-jammed instance) nil)
  
  ;; Don't need to set the param value because that will
  ;; get set to its default during a reset and will be
  ;; handled by the parameter function
  )

;;; Secondary reset function used to adjust the :do-not-harvest
;;; parameter.

(defun demo-secondary-reset (Instance)
  ;; Not using the instance so ignore it to suppress warnings in some Lisps
  (declare (ignore instance))
  
  (model-output "Demo module's secondary reset function called.")
  
  (sgp :do-not-harvest demo1))

;;; The query function reports the state flags as indicated
;;; above.

(defun demo-query (instance buffer query value)
  (model-output "Demo module's query function called to query the ~s buffer for ~S ~S" buffer query value)
  (if (eq buffer 'demo1)
      ;; buffer demo1 only takes the required state queries
      (if (eq query 'state)
          (case value
            (busy nil)
            (free t)
            (error nil))
        (model-warning "Invalid query specified in the demo1 buffer query"))
    
    ;; demo2 takes the state queries which check the module's flags
    (cond ((eq query 'state)
           (case value
             (busy (demo-module-busy instance))
             (free (not (demo-module-busy instance)))
             (error (demo-module-error instance))))
          ;; The detect-jam query is t if the value specified matches the
          ;; internal state of the flag
          ((eq query 'detect-jam)
           (eq value (demo-module-jammed instance)))
          ;; Otherwise print the warning and return nil
          (t
           (model-warning "Invalid query specified in the demo2 buffer query")))))

;;; Function to print the additional query info for the demo2 buffer.
;;; There are no parameters to the function and command-output should be
;;; used to handle all printing.

(defun demo-query-status ()
  ;; Space the output out like the default queries
  ;; and just print the internal flag's value.
  
  (command-output "  detect-jam            : ~S"
                  ;; Get the value by querying the module instead 
                  ;; of getting the module's instance and pulling 
                  ;; out the value directly.

                  (query-buffer 'demo2 '((detect-jam . t)))))

;;; Function to handle the demo module's parameters.

(defun demo-param-fct (instance param)
  (model-output "Demo module's parameter function called with parameter ~S" param)
  
  ;; If param is a cons it's an indication that a value is
  ;; being changed.  
  ;; The car is the parameter name and the cdr is the new
  ;; value.
  ;; Store that value for the module's parameter
  ;; and just print a notice when :esc change is detected.
  
  (if (consp param)
      (case (car param)
        (:esc (model-output "  :esc change noted"))
        (:demo-param (setf (demo-module-param instance) (cdr param))))
    
    ;; If it's not a cons then it's the name of a parameter
    ;; for which the current value is requested.  That will
    ;; only occur for the module's own parameter.
    
    (case param
      (:demo-param (demo-module-param instance)))))

;;; The request function will be called for requests of both
;;; buffers.  This function will call a buffer specific function
;;; to handle the different requests.

(defun demo-requests (instance buffer chunk-spec)
  (model-output "Request to the ~s buffer made with a request of type ~S"
                buffer
                (chunk-spec-chunk-type chunk-spec))
  (if (eq buffer 'demo1)
      (handle-demo1-request chunk-spec)
    (handle-demo2-request instance chunk-spec)))

;;; A request to the demo1 buffer just creates a chunk
;;; and then scheduled it to be put into the buffer.
;;; This could use the goal-style-request function but
;;; for demonstration purposes does that work explicitly.

(defun handle-demo1-request (chunk-spec)
  (let ((chunk-def (chunk-spec-to-chunk-def chunk-spec)))
    (if chunk-def
        ;; schedule the buffer setting for the newly created chunk
        (schedule-set-buffer-chunk 'demo1 (car (define-chunks-fct (list chunk-def))) 0
                                   :module 'demo)
      (model-warning "Invalid chunk-spec provided for a demo1 buffer request."))))

;;; A request to the demo2 buffer results in the creation
;;; of a chunk of chunk-type result after 100ms and if the
;;; request parameter :value is specified then the answer
;;; slot of that chunk will be set to that value.

(defun handle-demo2-request (instance chunk-spec)
  
  ;; check if the module is currently busy
  (if (demo-module-busy instance)
      (progn  ;; if so issue a warning and set the jammed flag
        (model-warning "Demo module's demo2 buffer can only process one request at a time.")
        (setf (demo-module-jammed instance) t))
    ;; otherwise handle the request
    (if (eq (chunk-spec-chunk-type chunk-spec) 'create-chunk)
        (let ((slot-specs (chunk-spec-slot-spec chunk-spec))
              (new-chunk (car (define-chunks (isa result)))))
          (cond ((null slot-specs) ; there is no request parameter give
                 ; set the busy flag for the module
                 (setf (demo-module-busy instance) t)
                 ; clear the error flag 
                 (setf (demo-module-error instance) nil)
                 ; schedule the buffer setting
                 (schedule-set-buffer-chunk 'demo2 new-chunk .1 :module 'demo :priority 0)
                 ; also schedule an event to call the function to clear
                 ; the busy and jammed flags with a priority 
                 ; less than the buffer setting
                 (schedule-event-relative .1 'finish-demo2-request :module 'demo :destination 'demo :priority -1))
                ;; if there is only a valid request parameter given
                ((and (= (length slot-specs) 1)
                      (eq (second (first slot-specs)) :value)
                      (eq (first (first slot-specs)) '=))
                 ; modify the chunk with that value
                 (mod-chunk-fct new-chunk (list 'answer (third (first slot-specs))))
                 ; set the flags and schedule the buffer setting as above
                 (setf (demo-module-busy instance) t)
                 (setf (demo-module-error instance) nil)
                 (schedule-set-buffer-chunk 'demo2 new-chunk .1 :module 'demo :priority 0)
                 (schedule-event-relative .1 'finish-demo2-request :module 'demo :destination 'demo :priority -1))
                (t ; something was wrong with the request
                 (setf (demo-module-error instance) t)
                 (model-warning "Invalid request to the demo2 buffer with this chunk-spec:")
                 (pprint-chunk-spec chunk-spec))))
      (progn
        (setf (demo-module-error instance) t)
        (model-warning "Invalid request to the demo2 buffer with this chunk-spec:")
        (pprint-chunk-spec chunk-spec)))))

(defun finish-demo2-request (instance)
  ;; Just clear the busy and jammed flags
  (setf (demo-module-busy instance) nil)
  (setf (demo-module-jammed instance) nil))

;;; The modification request for the module will be called
;;; for modification requests on either buffer.  It will
;;; only perform an action to modify a chunk in the demo2 buffer 
;;; if it is of type result.

(defun demo-modification-request (instance buffer modifications)
  
  (model-output "A buffer modification request was made to the ~s buffer with mods: ~s"
                buffer modifications)
  
  ;; The demo1 buffer just makes the change right away
  (if (eq buffer 'demo1)
      (schedule-mod-buffer-chunk 'demo1 modifications 0 :module 'demo)
      
    
    ;; check if the module is busy
    (if (demo-module-busy instance)
        (progn  ;; if so issue a warning and set the jammed flag
          (model-warning "Demo module's demo2 buffer can only process one request at a time.")
          (setf (demo-module-jammed instance) t))
      ;; otherwise verify and then handle the request
      (cond ((null (buffer-read 'demo2)) ; buffer is empty
             (model-warning "No chunk in the demo2 buffer to modify")
             (setf (demo-module-error instance) t))
            ((not (eq (chunk-chunk-type-fct (buffer-read 'demo2)) 'result)) ;wrong type of chunk in the buffer
             (model-warning "Chunk in the demo2 buffer is not of type result")
             (setf (demo-module-error instance) t))
            ((and (= (length modifications) 2)
                  (eq (car modifications) 'answer))
             ; set the flags 
             (setf (demo-module-busy instance) t)
             (setf (demo-module-error instance) nil)
             ; schedule the modification and finish events after the
             ; parameter time has passed
             (schedule-mod-buffer-chunk 'demo2 modifications (demo-module-param instance) :module 'demo :priority 0)
             (schedule-event-relative (demo-module-param instance) 'finish-demo2-request :module 'demo :destination 'demo :priority -1))
            (t
             (model-warning "Invalid modification request to the demo2 buffer: ~s" modifications)
             (setf (demo-module-error instance) t))))))
      


;;; This function will be called whenever any buffer clears.

(defun demo-detect-clearing (instance buffer chunk)
  (declare (ignore instance))
  (model-output "The demo module detects that the ~s buffer is clearing chunk ~s" buffer chunk))

;;; This function will be called whenever a request to the 
;;; demo module will be made by a production which has been
;;; selected.

(defun demo-detect-incoming-request (instance buffer type)
  (declare (ignore instance))
  (model-output "Demo module detects that a production will be making a request to the ~s buffer of chunk-type ~s"
                buffer type))

                            
;;; The actual module definition call specifying all the components.  
  
(define-module-fct 'demo '(demo1 (demo2 (:demo2-spread 3) (:value) (detect-jam) demo-query-status))
  (list (define-parameter :esc :owner nil)
        (define-parameter :demo-param :default-value .15
          :valid-test #'posnum 
          :warning "a positive number"
          :documentation "delay for demo2 buffer modification requests"))
  :version "1.0b2"
  :documentation "Demo module which exercises most of the module components"
  :creation 'create-demo-module
  :delete 'delete-demo-module
  :reset '(demo-primary-reset demo-secondary-reset)
  :query 'demo-query
  :params 'demo-param-fct
  :request 'demo-requests
  :buffer-mod 'demo-modification-request 
  :notify-on-clear 'demo-detect-clearing
  :warning 'demo-detect-incoming-request
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
