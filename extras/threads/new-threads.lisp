;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2010 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : new-threads.lisp
;;; Version     : 2.0a
;;; 
;;; Description : Reimplementation of the threaded cognition module using
;;;             : the new searchable multi-buffer capability.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider making the low priority offset a parameter or
;;;             :     perhaps using some way to scale it on the fly.
;;; 
;;; ----- History -----
;;; 2010.01.13 Dan [2.0a]
;;;            : * Initial creation.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending chunks at initial load.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;   An implementation of the threaded cognition theory of Salvucci and Taatgen. 
;;;   This code uses a searchable multi-buffer in a module which is a complete 
;;;   replacement for the standard goal module.  By using a multi-buffer to
;;;   hold the potential goals and relying on the ability of the procedural
;;;   module to search over a multi-buffer's chunks this new implementation 
;;;   does not require changing any of the standard procedural or buffer based
;;;   code.  Thus, this module should not interfere with any other modules or
;;;   updates.  Another advantage of this new implementation is that it works 
;;;   with the standard tracing and testing functions (like whynot).
;;;
;;;   For the most part, this should work just like the previous implementation
;;;   of threaded cognition, but there is a limitation built into the new
;;;   procedural searching which the previous version of threaded cognition did
;;;   not have.  The limitation occurs in dynamic (p*) style productions.  The 
;;;   chunk in the searched multi-buffer cannot be used to bind a variable that 
;;;   names a slot.  Dynamically named slots can be tested in the searched buffer's 
;;;   conditions, but those variables must be bound in non-searched buffers.  That
;;;   shouldn't cause too much of an issue for modeling because problem 
;;;   representation (where those dynamic names would likely come from) should be
;;;   done in the imaginal buffer now and not the goal anyway.  The reason for
;;;   that restriciton is to avoid the need to do full NP-hard searches in the 
;;;   procedural module while matching productions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; threads-pprint
;;;
;;; Takes no parameters.  Prints out the chunks currently in the thread
;;; set in order of their current priority and returns the list of those
;;; chunks.
;;;
;;; goal-focus
;;;
;;; Works like the default module's goal focus, except that it adds all of the
;;; goals created to the set of potential goals.  The one exception to that is
;;; if the same goal is added more than once.  It will only have one instance
;;; of a particular goal chunk in the set, and the newly focused version will
;;; replace an older one.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;;  By using a searchable multi-buffer the work required for the search is now
;;;  done by the procedural system itself.  So, all that's necessary here is to
;;;  maintain the set of goal chunks in the appropriate order to pass back to
;;;  procedural when asked and to report which one to prefer if more than one
;;;  match.  Right now, that's done with a fixed offset to the utility of -999
;;;  for the low priority matches, but that may need to be adjusted.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Rely on the general functions in the goal-style-module 

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

;;; Remove the default goal module because we are going to replace it.

(undefine-module goal)


;;; Extend the structure which implements the standard goal module
;;; with some new slots.

(defstruct (threaded-goal-module (:include goal-module) (:conc-name tgm-))
  chunk-set update)


;;; A simple module creation function which just makes a new structure.

(defun create-threaded-goal-module (model-name)
  (declare (ignore model-name))
  (make-threaded-goal-module))


;;; The reset function clears all of the slots and sets the goal buffer to
;;; not be harvested.

(defun threaded-goal-reset (instance)
  (setf (tgm-delayed instance) nil)
  ; Do NOT strict harvest the goal buffer by default
  (sgp :do-not-harvest goal)
  (setf (tgm-chunk-set instance) nil)
  (setf (tgm-update instance) nil))

;;; If a chunk is cleared from the goal buffer then it needs to be removed
;;; from the set of potential goals if it is still one of them.

(defun thread-clear (instance buffer chunk)
  (when (and (eq buffer 'goal) (find chunk (tgm-chunk-set instance)))
    (setf (tgm-chunk-set instance) (remove chunk (tgm-chunk-set instance)))))

;;; The goal requests need to store the new chunk in the set of potential
;;; goals and use overwrite to put it into the buffer to avoid clearing the
;;; old one.

(defun thread-request (instance buffer-name chunk-spec &optional (delay 0))
  (let ((chunk-description (chunk-spec-to-chunk-def chunk-spec)))
    (if chunk-description
        (let ((chunk-name (car (define-chunks-fct (list chunk-description)))))
          (store-m-buffer-chunk buffer-name chunk-name)
          (push-last chunk-name (tgm-chunk-set instance))
          (schedule-event-relative delay 'create-new-thread-chunk 
                                 :module 'goal
                                 :priority -100 
                                 :details 
                                 (concatenate 'string
                                   (symbol-name 'create-new-buffer-chunk)
                                   " "
                                   (symbol-name buffer-name)
                                   " "
                                   (symbol-name (first chunk-description))
                                   " "
                                   (symbol-name (second chunk-description)))
                                 :params (list chunk-name))
          
          )
      (print-warning "Invalid request made of the ~A buffer." buffer-name))))

(defun create-new-thread-chunk (chunk)
    (schedule-overwrite-buffer-chunk 'goal chunk 0 :module 'goal :priority -1000 :requested t))

;;; These two module functions are responsible for the search interface provided
;;; by procedural now. 

;;; Thread-search will be called at the start of conflict resolution and is
;;; responsible for returning the list of chunks to search in the order which
;;; they should be searched.  The searching terminates when it finds a match.
;;; Here we just move the last used goal to the end if it's still on the
;;; list and then return that list.

(defun thread-search (instance buffer)
  (declare (ignore buffer))
  (when (tgm-update instance) 
    (when (find (tgm-update instance) (tgm-chunk-set instance))
      (setf (tgm-chunk-set instance) (append (remove (tgm-update instance) (tgm-chunk-set instance)) (list (tgm-update instance)))))
    (setf (tgm-update instance) nil))
    
  (tgm-chunk-set instance))

;;; Thread-offset is called after production matching has occurred to determine
;;; if there is a preference for which of the chunks from the multi-buffer to
;;; use.  All chunks from the multi-buffer which were found to match to some
;;; production are given in chunks and it needs to return the list of offsets
;;; to apply to the utility of the productions using those chunks (in order).
;;; Right now all it does is specify 0 (no change) for the productions which
;;; match the highest priority goal (closest to the head of the queue) and 
;;; -999 for all the others which should suppress them.  This may need to be
;;; adapted to be more flexible at some point...
;;;
;;; That "best" chunk is recorded so that it can be moved to the end of the
;;; queue for the next matching phase.

(defun thread-offset (instance buffer chunks)
  (declare (ignore buffer))
  (let* ((positions (mapcar (lambda (x) (cons (position x (tgm-chunk-set instance)) x)) chunks))
         (m (reduce 'min positions :key 'car)))
    (setf (tgm-update instance) (cdr (assoc m positions)))
    (mapcar (lambda (x) (if (= (car x) m) 0 -999)) positions)))


;;; Define the new module with the same name and buffer as the old
;;; goal module but now mark the buffer as searchable.

(define-module-fct 'goal '((goal (:ga 1.0) nil nil nil :search))
  nil
  :version "2.0a"
  :documentation "Threaded cognition version of the goal module"
  :creation 'create-threaded-goal-module
  :reset (list nil 'threaded-goal-reset)
  :query 'goal-query
  :buffer-mod 'goal-style-mod-request
  :notify-on-clear 'thread-clear
  :search 'thread-search
  :offset 'thread-offset
  :request 'thread-request
)


;;; The module's user commands.

;;; threads-pprint updates the list of chunks if necessary and then prints them out.

(defun threads-pprint ()
  (let ((module (get-module goal)))
    (pprint-chunks-fct (thread-search module 'goal))))


;;; Goal-focus works essentially the same as it does for the standard goal module,
;;; but now all the goals created are added to the set of potential goals instead
;;; of being directly set in the buffer.  To avoid creating multiple instances of 
;;; a single goal a new chunk parameter is added to keep track of goals which have 
;;; been used in the past.

(suppress-extension-warnings)

(extend-chunks parent-goal)

(unsuppress-extension-warnings)

(defun goal-focus-fct (&optional (chunk-name nil))
  "Place a chunk into the goal buffer or return either the chunk that is there
   now or the one that will be placed there by a pending goal-focus"
  (let ((g-module (get-module goal)))
    (if chunk-name
        (if (chunk-p-fct chunk-name)
            ;;; always copy the user chunks for goals
            (let ((n-chunk (copy-chunk-fct chunk-name))
                  (clear-old-chunk nil))
              
              ;; first remove an "old" instance of this same goal
              ;; if there is one
              
              (awhen (find chunk-name (tgm-chunk-set g-module) :key 'chunk-parent-goal)
                     
                     ;; if it happens to be in the buffer then we will just let it clear 
                     ;; and that will take care of the removal details
                     (if (eq it (buffer-read 'goal))
                         (setf clear-old-chunk t)
                       (progn
                         ;; otherwise just take it out of the current set of goals
                         ;; and have it replaced with the new version
                         ;; ??? should the old one get to DM ???
                         ;; I don't think so since it'd only happen in a
                         ;; threaded model, but I don't know
                         
                         (setf (tgm-chunk-set g-module) (remove it (tgm-chunk-set g-module)))
                         (remove-m-buffer-chunk 'goal it))))
              
              
              (setf (chunk-parent-goal n-chunk) chunk-name)
              
              (when (null (tgm-chunk-set g-module))
                ;; if there aren't any goals then use
                ;; set-buffer-chunk instead of overwrite
                ;; to be more consistent with existing
                ;; single threaded models
                (setf clear-old-chunk t))
              
              (store-m-buffer-chunk 'goal n-chunk)
              (setf (tgm-chunk-set g-module) (append (tgm-chunk-set g-module) (list n-chunk)))
              (if clear-old-chunk
                  (schedule-set-buffer-chunk 'goal n-chunk 0 :module 'goal :priority :max :requested nil)  
                (schedule-overwrite-buffer-chunk 'goal n-chunk 0 :module 'goal :priority :max :requested nil))
              (setf (tgm-delayed g-module) n-chunk)
              (schedule-event-after-module 'goal #'clear-delayed-goal :module 'goal 
                                           :output nil :destination 'goal :maintenance t)
              chunk-name)
          (print-warning "~S is not the name of a chunk in the current model - goal-focus failed" chunk-name))
      
      (let ((chunk (buffer-read 'goal))
            (delayed (goal-module-delayed g-module)))
        (cond ((and (null chunk) (null delayed))
               (command-output "Goal buffer is empty")
               nil)
              ((null chunk)
               (command-output "Will be ~a when the model runs" delayed)
               (pprint-chunks-fct (list delayed))
               delayed)
              ((null delayed)
               (pprint-chunks-fct (list chunk))
               chunk)
              (t
               (if (or (eq delayed chunk) ;; it shouldn't be a copy with a multi-buffer
                       (eq delayed (chunk-copied-from-fct chunk))) ;; but if it was a DM chunk it was copied
                   ;; caught it before the delayed chunk was cleared
                   (progn
                     (pprint-chunks-fct (list chunk))
                     chunk)
                 (progn
                   (command-output "Will be ~a when the model runs" delayed)
                   (command-output "Currently holds:")
                   (pprint-chunks-fct (list chunk))
                   delayed))))))))

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
