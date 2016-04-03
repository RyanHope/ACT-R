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
;;; Filename    : goal-style-module.lisp
;;; Version     : 2.0
;;; 
;;; Description : Functions that allow one to easily create a module that
;;;             : acts like the basic ACT-R goal module/buffer.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.26 Dan
;;;             : Initial creation.
;;;
;;; 2004.12.13 Dan
;;;             : Added the optional delay to the goal-style-request.
;;;             : Made sure line lengths were max 80 chars.
;;; 2005.01.09 Dan
;;;            : Moved the provide to the end.
;;; 2005.01.17 Dan
;;;            : * Removed the call to format in the scheduling.
;;; 2005.04.23 Dan
;;;            : * Updated the query function because it doesn't need to
;;;            :   respond to "buffer stuffed" anymore.
;;; 2005.08.10 Dan
;;;            : * Updated the query function to specify the ignored params.
;;;            : * Updated the version to 1.0.
;;; 2006.10.23 Dan
;;;            : * Changed the temp goal chunk "clean up" so that it's now
;;;            :   done with a maintenance event and it also uses the release-name
;;;            :   command to kill the symbol name in addition to the chunk struct.
;;; 2007.06.18 Dan
;;;            : * Removed chunk-spec-to-chunk-def because it's now an official
;;;            :   command and was moved to the chunk-spec file.
;;; 2008.09.19 Dan
;;;            : * Added the goal-style-mod-request function here and gave it
;;;            :   an optional delay time too.
;;; 2010.12.08 Dan
;;;            : * Added a priority to the goal-style-mod-request as an additional
;;;            :   optional parameter because the modification needs to take place
;;;            :   prior to the buffer being cleared (whether explicitly or implicitly).
;;;            :   The default priority is now 20 (since the clearing is 10).
;;; 2013.04.10 Dan
;;;            : * Changed the goal-style-mod-request action so that it can also
;;;            :   extend the chunk-type if there are new slots in a dynamic request.
;;; 2014.03.20 Dan [2.0]
;;;            : * Update to work with the typeless chunks.
;;;            : * Make the query function a little more careful and actually test
;;;            :   that the slot is state otherwise print a warning.
;;;            : * The mod-request doesn't need to extend the chunk now since that
;;;            :   will already have happened at the time of the chunk-spec creation.
;;; 2015.07.28 Dan
;;;            : * Changed the logical to ACT-R-support in the require-compiled examples.
;;; 2015.12.17 Dan
;;;            : * Changed the priority of the clean-up-goal-chunk event scheduled
;;;            :   by create-new-buffer-chunk to always be :min since that will
;;;            :   guarantee it happens after and doesn't throw an error for an
;;;            :   attempt to subtract from a value of :min or :max.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; With these functions one can create a new module whos buffer acts like
;;; the basic goal module/buffer i.e. the module responds to requests by 
;;; creating a new chunk and placing it into the buffer and responds to modification
;;; requests by modifying the chunk in the buffer.
;;; 
;;; Request:
;;;
;;; A request must be a unique specification of a chunk: no variables are allowed,
;;; only the = modifier, and each slot may be specified at most once.
;;;
;;; The new chunk is placed into the buffer at the same time as the request by,
;;; default, but one can provide a delay on the time it takes to put the
;;; chunk into the buffer by having the module provide its own request function 
;;; that calls the goal-style-request function with the optional delay parameter.
;;;
;;; Modification request:
;;;
;;; A modification request must be a valid buffer modification specification:
;;; no variables, only the = modifier, and each slot specified at most once.
;;;
;;; The chunk in the buffer has the specified modifications made to it at the 
;;; same time as the modification request by default, but one can provide a 
;;; delay on the time it takes to put the chunk into the buffer by having the
;;; module provide its own modification-request function that calls the 
;;; goal-style-mod-request function with the optional delay parameter.
;;;
;;; It only responds to the required queries - state {busy, free, error}.
;;;
;;; State free will always return t.
;;; State busy will always return nil.
;;; State error will always return nil.
;;;
;;;
;;; To create a basic goal style module, just place a file into the modules 
;;; directory that contains the following (where <...> is replaced
;;; with the value described within it):
#|

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GOAL-STYLE-MODULE" "ACT-R-support:goal-style-module")

(define-module <the name of your module here>
  (<the name of your buffer here which must be the same as the module name>)
  nil
  :version "Something about the version of your module"
  :documentation "Something about the details of your module"
  :query goal-style-query
  :request goal-style-request
  :buffer-mod goal-style-mod-request)

|#

;;; If one wants a delay on the time it takes to put the chunk into the
;;; buffer then the code would look like this (assuming a fixed delay 
;;; time was desired).

;;; Note this shows the use of define-module-fct instead of the macro
;;; for comparison to the previous one.  Either version can be used.
#|

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GOAL-STYLE-MODULE" "ACT-R-support:goal-style-module")

(defun my-goal-style-request (instance buffer-name chunk-spec)
  (goal-style-request instance buffer-name chunk-spec <delay time>))

(defun my-goal-style-mod-request (instance buffer-name chunk-spec)
  (goal-style-mod-request instance buffer-name chunk-spec <delay time>))


(define-module-fct '<the name of your module here>
  '(<the name of your buffer here which must be the same as the module name>)
  nil
  :version "Something about the version of your module"
  :documentation "Something about the details of your module"
  :query 'goal-style-query
  :request 'my-goal-style-request
  :buffer-mod 'my-goal-style-mod-request)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;  (defun goal-style-query (instance buffer-name slot value)
;;;
;;;  This can be used as the query function for a module that will respond to
;;;  the required queries in a static manner.  It will respond as follows:
;;;
;;;    State free will always return t.
;;;    State busy will always return nil.
;;;    State error will always return nil.
;;;
;;;  It ignores the instance and if a slot other than state is specified or a value
;;;  other than free, busy, or error is given then it prints a warning and returns nil.
;;;
;;;
;;;  (defun goal-style-request (instance buffer-name chunk-spec &optional (delay 0) (priority -100)))
;;;
;;;  This can be used as the request function of a module to allow it to
;;;  operate like the goal module i.e. create new chunks in response to a
;;;  request.  The instance is not used and it is assumed that the module
;;;  has the same name as the buffer.  
;;;  The delay and priority are used to schedule the create-new-buffer-chunk 
;;;  action.  The default priority is lower than any production action to
;;;  ensure that all of the production actions at least "start" before 
;;;  the event to create the new chunk happens.
;;;
;;;
;;;  (defun goal-style-mod-request (instance buffer chunk-spec &optional (delay 0) (priority 20))
;;;  
;;;  This can be used as a modification request function of a module to allow
;;;  it to handle modification requests like the goal module does - perform
;;;  an immediate modification of the chunk in the buffer.  It assumes that
;;;  the module and buffer have the same name and the instance of the module
;;;  is ignored.
;;;  The delay and priority are used to schedule a mod-buffer-chunk 
;;;  action.  The default priority is higher than a production's clearing action
;;;  priority (10) to ensure that all of the modification is made before a
;;;  -<buffer> action in the production clears it. 
;;; 
;;;  (defun create-new-buffer-chunk (buffer-name chunk-description &key (priority -1000)))
;;;
;;;  A function that creates a new chunk based on the chunk description in the
;;;  chunk-description value provided (a list as appropriate for passing as one of 
;;;  the lists to define-chunks-fct) and schedules that it be placed into the buffer 
;;;  called buffer-name with the specified priority at the current time.  This is
;;;  used by goal-style-request to create the chunk, but may be used on its own if
;;;  one wants to schedule the creation of a chunk in a buffer.  This function 
;;;  automatically deletes the "original" chunk created after the buffer makes its
;;;  copy through an event that is not output.
;;;  The priority is used to schedule a set-buffer-chunk action.  The default 
;;;  priority is "very low" so that other module's actions will generally take
;;;  place first, but one exception is the declarative modules retrieval requests
;;;  which have a priority of -2000 thus by default this new chunk will be a source
;;;  of spreading activation.
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


(defun goal-style-query (instance buffer-name slot value)
  (declare (ignore instance))
  ;; don't care about the buffer name and only care about slot state
  (if (eq slot 'state)
    (case value
      (busy nil)
      (free t)
      (error nil)
      (t 
       (print-warning "Unknown query state ~s to ~s buffer" value buffer-name)))
    (print-warning "Unknown query ~s ~s to the ~s buffer" slot value buffer-name)))

(defun goal-style-request (instance buffer-name chunk-spec &optional (delay 0) (priority -100))
  (declare (ignore instance))
  (let ((chunk-description (chunk-spec-to-chunk-def chunk-spec)))
    (if chunk-description
        (schedule-event-relative delay 'create-new-buffer-chunk 
                                 :module buffer-name :priority priority 
                                 :details (concatenate 'string (symbol-name 'create-new-buffer-chunk) " " (symbol-name buffer-name))
                                 :params (list buffer-name chunk-description))
      (print-warning "Invalid request made of the ~a buffer." buffer-name))))

(defun create-new-buffer-chunk (buffer-name chunk-description &key (priority -1000))
  (let ((chunk-name (car (define-chunks-fct (list chunk-description)))))
    (schedule-set-buffer-chunk buffer-name chunk-name 0 :module buffer-name :priority priority)
    ;; because the chunk is only being created to be copied into the buffer
    ;; just get rid of it after that happens to keep the chunk count down 
    (schedule-event-relative 0 'clean-up-goal-chunk :module :none :output nil 
                             :priority :min :params (list chunk-name)
                             :details "Clean-up unneeded chunk" :maintenance t)
    nil))


(defun clean-up-goal-chunk (name)
  (delete-chunk-fct name)
  (release-name-fct name))


(defun goal-style-mod-request (instance buffer chunk-spec &optional (delay 0) (priority 20))
  (declare (ignore instance))
  (schedule-mod-buffer-chunk buffer chunk-spec delay :module buffer :priority priority))

(provide "GOAL-STYLE-MODULE")

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
