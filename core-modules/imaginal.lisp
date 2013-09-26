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
;;; Filename    : imaginal.lisp
;;; Version     : 1.3
;;; 
;;; Description : An actual imaginal module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2005.09.27 Dan
;;;             : Creation.
;;; 2005.11.08 Dan
;;;             : * Extended it with a second buffer to accept commands to
;;;             :   manipulate the chunk in the imaginal buffer.
;;;             : * The imaginal-action buffer right now only takes one 
;;;             :   request which is generic-action that results in a funcall
;;;             :   of the function named in the action slot.
;;; 2005.11.16 Dan
;;;             : * Replaced all occurences of imaginal-free with the actual
;;;             :   function name set-imaginal-free.
;;; 2006.09.08 Dan
;;;             : * Changed the posnum test on the delay param to nonneg. 
;;; 2006.09.25 Dan
;;;             : * The module now accepts modification requests which also
;;;             :   take the imaginal-delay time to occur.  They act just
;;;             :   like a normal buffer modification in a production i.e.
;;;             :   it just changes the slots as specified.
;;; 2006.09.25 Dan
;;;             : * Changed the behavior on a "jam".  Now, it ignores any
;;;             :   request (regular or modification) to either buffer when
;;;             :   the state of the module is busy.
;;; 2006.09.26 Dan
;;;             : * Cleaned up set-imaginal-free and set-imaginal-error so that
;;;             :   they don't throw an error when there is no current imaginal
;;;             :   module.
;;; 2006.11.07 Dan
;;;             : * Changed the mod-request cleanup event to just be set-imaginal-free
;;;             :   instead of a lambda that called it (makes the event easier to
;;;             :   read/handle).
;;;             : * Also removed the handle-imaginal-request function/event and
;;;             :   schedule the goal-style-request and set-imaginal-free events
;;;             :   directly in the request.
;;;             : * Removed the modification requests for the imaginal-action
;;;             :   buffer.
;;; 2007.01.16 Dan
;;;             : * Fixed a bug in a warning in imaginal-mod-request.
;;; 2011.05.16 Dan [1.2]
;;;             : * Added a new request to the imaginal-action buffer:
;;;             :   simple-action which works like generic-action execept that
;;;             :   it clears the imaginal buffer immediatly and the action 
;;;             :   function should return either a chunk-name or nil.
;;;             :   If a chunk is returned it is put into the imaginal buffer
;;;             :   at the current delay time and if nil is returned it sets
;;;             :   the imaginal module error state to t at that time instead.
;;;             :   In either case, the module is flagged as busy during that time.
;;; 2013.04.10 Dan
;;;             : * Allow the modification requests to now also extend the chunk-
;;;             :   type when a dynamic request specifies new slots.
;;; 2013.05.01 Dan [1.3]
;;;             : * Added a slot called slots to both the generic-action and
;;;             :   simple-action types.  That slot can be used when making a
;;;             :   request to the imaginal-action buffer to provide a list of
;;;             :   symbols.  If all of those symbols name slots of the chunk
;;;             :   in the imaginal buffer at the time of the request then 
;;;             :   they will be passed to the action function.  If they are not
;;;             :   valid slot names then the request will fail.
;;; 2013.05.07 Dan 
;;;             : * Test whether the slots slot is specified in the imaginal-action
;;;             :   requests to avoid a warning when getting its value when not
;;;             :   provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The imaginal module has two buffers one called imaginal which works just like
;;; the goal module (requests create new chunks) except there is a time delay
;;; before the new chunk shows up in the buffer.  The delay is controlled by
;;; a parameter called :imaginal-delay.
;;; The other buffer is called imaginal-action and it takes requests to change
;;; the chunk currently in the imaginal buffer or to perform other actions 
;;; that use that chunk (without having to clear the buffer).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; The new buffer imaginal is available.
;;;
;;; There is a new parameter called :imaginal-delay which must be a number.
;;; It defaults to .2 and is the time in seconds before an imaginal request
;;; completes.
;;;
;;; The imaginal-action buffer accepts two requests:
;;; 
;;; generic-action with the action slot is required and the slots slot is
;;; optional.
;;;
;;; The function named in the action slot is called at the time of the request.
;;; All timing and state maintenance must be handled by that function except
;;; that the module is marked busy at the time of the call (thus the function
;;; or an event that it schedules, needs to set it back to free using the 
;;; set-imaginal-free command).  No chunks are placed into the imaginal-action
;;; buffer by default, but the generic-action chunk-type also has a slot
;;; called result which could be a meaningful thing to use - place a
;;; chunk of type generic-action with the action and result slots set
;;; in the imaginal-action buffer in response to a request.  If the slots slot
;;; is specified with a list of symbols which name slots of the chunk in the
;;; imaginal buffer at the time of the request then those slot names will be
;;; passed to the action function in the order provided i.e. this is what
;;; will effectively happen: (apply <action> <slots list>).  If the slots
;;; list is provided but not valid then no action is taken and a warning
;;; is printed.
;;;
;;; simple-action with the action slot is required and the slots slot is
;;; optional.
;;;
;;; The function named in the action slot is called at the time of the request,
;;; the imaginal buffer is cleared, and the imaginal module is marked as busy. 
;;; The action function should return either a chunk name or nil.  If a chunk
;;; name is returned then that chunk will be put into the imaginal buffer after
;;; the current delay time for the imaginal module passes and the module will
;;; then be marked as free.  If the function returns nil then after the current
;;; imaginal delay time passes the module will be marked as free and the error
;;; state will be set to t.  If the slots slot is specified with a list of 
;;; symbols which name slots of the chunk in the imaginal buffer at the time of 
;;; the request then those slot names will be passed to the action function in 
;;; the order provided i.e. this is what will effectively happen: 
;;; (apply <action> <slots list>).  If the slots list is provided but not valid 
;;; then no action is taken and a warning is printed.

;;;
;;;
;;; Two new commands are provided for use in user defined actions:
;;;
;;; set-imaginal-free
;;; This command takes no parameters and sets the state of the imaginal 
;;; module in the current model to free.
;;;
;;; set-imaginal-error
;;; This command takes no parameters and sets the error state of the 
;;; imaginal module in the current model to t.  The only way to clear
;;; the error is through a new request or resetting the model.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; There's a little more here than the goal-style-module code would seem to
;;; indicate is necessary because there's a paramter in the module and it needs
;;; to properly signal busy/free during the delay (which the goal-style-query
;;; doesn't do).
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

(defstruct imaginal-module delay busy error randomize)

(defun create-imaginal (model)
  (declare (ignore model))
  (make-imaginal-module))

(defun imaginal-query (instance buffer-name slot value)
  (declare (ignore slot)) ; the only slot is state
  (case value
    (busy (imaginal-module-busy instance))
    (free (not (imaginal-module-busy instance)))
    (error (imaginal-module-error instance))
    (t (print-warning "Unknown state query ~S to ~S module" 
                      value buffer-name)
       nil)))


(defun set-imaginal-free ()
  (let ((im (get-module imaginal)))
    (if im
        (progn
          (setf (imaginal-module-busy im) nil)
          t)
      (print-warning "Call to set-imaginal-free failed"))))

(defun set-imaginal-error ()
  (let ((im (get-module imaginal)))
    (if im
        (setf (imaginal-module-error im) t)
      (print-warning "Call to set-imaginal-error failed"))))



(defun imaginal-request (instance buffer-name chunk-spec)
  (if (imaginal-module-busy instance)
    (model-warning "Imaginal request made to the ~S buffer while the imaginal module was busy. New request ignored." buffer-name)
  (progn
    (setf (imaginal-module-busy instance) t)
    (setf (imaginal-module-error instance) nil)
    
    (case buffer-name
      (imaginal
       (let ((delay (if (imaginal-module-randomize instance)
                        (randomize-time (imaginal-module-delay instance))
                      (imaginal-module-delay instance))))
         
         (schedule-event-relative delay
                                  'set-imaginal-free
                                  :module 'imaginal
                                  :output nil
                                  :priority -1010)
         
         (schedule-event-relative delay
                                  'goal-style-request
                                  :params (list 'imaginal chunk-spec)
                                  :destination 'imaginal
                                  :module 'imaginal
                                  :output nil)))
      (imaginal-action
       (case (chunk-spec-chunk-type chunk-spec)
         (generic-action
          (let ((action-spec (chunk-spec-slot-spec chunk-spec 'action))
                (slots-spec (when (slot-in-chunk-spec-p chunk-spec 'slots) (chunk-spec-slot-spec chunk-spec 'slots))))
            
            (cond ((null action-spec)
                   (print-warning "An imaginal-action generic-action request requires an action.")
                   (set-imaginal-free))
                  ((> (length action-spec) 1)
                   (print-warning "An imaginal-action generic-action request requires a single action.")
                   (set-imaginal-free))
                  ((not (eq '= (caar action-spec)))
                   (print-warning "An imaginal-action generic-action request requires the = specifier for the action.")
                   (set-imaginal-free))
                  ((not (or (functionp (third (car action-spec))) (fboundp (third (car action-spec)))))
                   (print-warning "An imaginal-action generic-action request requires the action to name a valid function.")
                   (set-imaginal-free))
                  (t
                   (if slots-spec
                       (cond ((> (length slots-spec) 1)
                              (print-warning "An imaginal-action generic-action request can only specify one slots list."))
                             ((not (eq '= (caar slots-spec)))
                              (print-warning "An imaginal-action generic-action request requires the = specifier for the slots."))
                             ((not (listp (third (car slots-spec))))
                              (print-warning "An imaginal-action generic-action request slots value must be a list."))
                             ((not (buffer-read 'imaginal))
                              (print-warning "An imaginal-action generic-action request which specifies slots requires a chunk in the imaginal buffer."))
                             ((let* ((chunk (buffer-read 'imaginal))
                                     (type (chunk-chunk-type-fct chunk))
                                     (slots (chunk-type-slot-names-fct type)))
                                (not (every (lambda (x) (find x slots)) (third (car slots-spec)))))
                              (print-warning "An imaginal-action generic-action request which specifies slots requires valid slot names for the current imaginal buffer chunk."))
                             (t
                              (apply (third (car action-spec)) (third (car slots-spec)))))
                       (funcall (third (car action-spec))))))))
         (simple-action
          (let ((action-spec (chunk-spec-slot-spec chunk-spec 'action))
                (slots-spec (when (slot-in-chunk-spec-p chunk-spec 'slots) (chunk-spec-slot-spec chunk-spec 'slots))))
            
            (cond ((null action-spec)
                   (print-warning "An imaginal-action simple-action request requires an action.")
                   (set-imaginal-free))
                  ((> (length action-spec) 1)
                   (print-warning "An imaginal-action simple-action request requires a single action.")
                   (set-imaginal-free))
                  ((not (eq '= (caar action-spec)))
                   (print-warning "An imaginal-action simple-action request requires the = specifier for the action.")
                   (set-imaginal-free))
                  ((not (or (functionp (third (car action-spec))) (fboundp (third (car action-spec)))))
                   (print-warning "An imaginal-action simple-action request requires the action to name a valid function.")
                   (set-imaginal-free))
                  (t
                   (when slots-spec
                     (unless (cond ((> (length slots-spec) 1)
                                    (print-warning "An imaginal-action generic-action request can only specify one slots list."))
                                   ((not (eq '= (caar slots-spec)))
                                    (print-warning "An imaginal-action generic-action request requires the = specifier for the slots."))
                                   ((not (listp (third (car slots-spec))))
                                    (print-warning "An imaginal-action generic-action request slots value must be a list."))
                                   ((not (buffer-read 'imaginal))
                                    (print-warning "An imaginal-action generic-action request which specifies slots requires a chunk in the imaginal buffer."))
                                   ((let* ((chunk (buffer-read 'imaginal))
                                           (type (chunk-chunk-type-fct chunk))
                                           (slots (chunk-type-slot-names-fct type)))
                                      (not (every (lambda (x) (find x slots)) (third (car slots-spec)))))
                                    (print-warning "An imaginal-action generic-action request which specifies slots requires valid slot names for the current imaginal buffer chunk."))
                                   (t
                                    t))
                       (set-imaginal-free)
                       (return-from imaginal-request)))
                   
                   
                   (let ((c (if slots-spec
                                (apply (third (car action-spec)) (third (car slots-spec)))
                              (funcall (third (car action-spec)))))
                         (delay (if (imaginal-module-randomize instance)
                                    (randomize-time (imaginal-module-delay instance))
                                  (imaginal-module-delay instance))))
                     
                     (schedule-clear-buffer 'imaginal 0 :module 'imaginal)
                     (cond ((null c) ;; set module free and error t
                            (schedule-event-relative delay
                                                      'set-imaginal-free
                                                      :module 'imaginal
                                                      :output nil)
                            
                            (schedule-event-relative delay
                                                     'set-imaginal-error
                                                     :module 'imaginal
                                                     :output nil))
                           
                           ((chunk-p-fct c) ;; set module free and error t
                            (schedule-set-buffer-chunk 'imaginal c delay :module 'imaginal :priority -1000)
                            
                            (schedule-event-relative delay 'set-imaginal-free :module 'imaginal :output nil :priority -1001 :maintenance t))
                           (t
                            (model-warning "Invalid result from the action of an imaginal-action simple-action function.")
                            (set-imaginal-free))))))))
                            
                            
         (t (print-warning "Invalid request ~S to the imaginal-action buffer." (chunk-spec-chunk-type chunk-spec)))))))))
  

(defun imaginal-mod-request (instance buffer mods)
  (if (imaginal-module-busy instance)
    (model-warning "Imaginal modification request made to the ~S buffer while the imaginal module was busy. New request ignored." buffer)
    (if (eq buffer 'imaginal)
        (let ((delay (if (imaginal-module-randomize instance)
                         (randomize-time (imaginal-module-delay instance))
                       (imaginal-module-delay instance))))
          (setf (imaginal-module-busy instance) t)
          
          (schedule-mod-buffer-chunk buffer mods delay :module 'imaginal :extend t)
          (schedule-event-relative delay 'set-imaginal-free :module 'imaginal :priority -1 :output nil))
      (model-warning "Modification requests not available for the imaginal-action buffer"))))

   
(defun imaginal-params (instance param)
  (if (consp param)
      (case (car param)
        (:imaginal-delay (setf (imaginal-module-delay instance) (cdr param)))
        (:vidt (setf (imaginal-module-randomize instance) (cdr param))))
    (case param
      (:imaginal-delay (imaginal-module-delay instance))
      (:vidt (imaginal-module-randomize instance)))))

(defun imaginal-reset (instance)
  (setf (imaginal-module-busy instance) nil)
  (setf (imaginal-module-error instance) nil)
  (chunk-type generic-action action result slots)
  (chunk-type simple-action action slots))

(define-module-fct 'imaginal
    '(imaginal imaginal-action)
  (list
   (define-parameter :imaginal-delay :valid-test #'nonneg 
     :default-value .2 :warning "non-negative number" 
     :documentation "Time in seconds to respond to an imaginal request")
   (define-parameter :vidt :valid-test #'tornil :default-value nil
          :warning "T or nil" 
          :documentation "Variable Imaginal Delay Time"))
   
  :version "1.3"
  :documentation "The imaginal module provides a goal style buffer with a delay and an action buffer for manipulating the imaginal chunk"
  :creation #'create-imaginal
  :query #'imaginal-query
  :request #'imaginal-request
  :buffer-mod #'imaginal-mod-request
  :params #'imaginal-params
  :reset #'imaginal-reset)


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
