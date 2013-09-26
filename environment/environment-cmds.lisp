;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : environment-cmds.lisp
;;; Version     : 3.0
;;; 
;;; Description : No system dependent code.
;;;             : Defines stuff that is needed to handle "environment"
;;;             : things.
;;; Bugs        : 
;;; 
;;; Todo        : Clean up what goes here and what's passed literally
;;;             : from the Tcl side - it's an odd mix at this point
;;;             : and I should have some consistency.
;;; 
;;; ----- History -----
;;;
;;; 05/22/2002  Dan
;;;             : File creation
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;; 01/20/2003  Dan
;;;             : Added the show-module-state-chunk function and
;;;             : hacked buffer-contents so that the module-state
;;;             : chunks shown by the environment match internal
;;;             : module states even though the "real" chunks
;;;             : don't seem to be updated anymore. WHY and WHEN?!
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; ----------------------------------------------------------------------
;;; 2005.04.13  Dan [2.0]
;;;             : * Moved to ACT-R 6.
;;; 2007.08.03 Dan
;;;             : * Moved the *stepper-open* defvar here to avoid a warning
;;;             :   at compile time.
;;;             : * Deleted the commented out in-package calls.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.05.25 Dan
;;;             : * Modified reload-model and safe-load so that warnings aren't
;;;             :   double printed in most cases.
;;;             : * Added a finishing of the output streams to those because
;;;             :   it seems to lag sometimes in ACL...
;;;             : * The open button uses a function called smart-loader which
;;;             :   isn't defined anymore, so adding that.
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;; 2012.12.18 Dan
;;;             : * Changed reload-model and safe-load so that they can signal
;;;             :   an error without opening the interactive debugger because
;;;             :   that's an issue in the environment particularly with the
;;;             :   standalones since the error would have to be cleared before
;;;             :   the environment notice can be displayed, but with CCL that's
;;;             :   very difficult becuase the background process doesn't have
;;;             :   access to the terminal...
;;; 2013.02.19 Dan
;;;             : * Haven't been able to reconstruct why the loaders "ignore"
;;;             :   unbound-variable errors, but since it can lead to very bad
;;;             :   situations for users I'm taking that out.  There are two
;;;             :   guesses so far as to where it comes from.  Christian thinks 
;;;             :   it may go all the way back to the old environment's split 
;;;             :   edit files because code may have been loaded out of order.
;;;             :   It might also have been a "fix" for a problem with the early 
;;;             :   OpenMCL versions of the standalone environment based on an 
;;;             :   error report I found in an email from 9/12/02:
;;;             :   
;;;             :   ... whenever I try to load a model (I get that far without 
;;;             :   problem), OpenMCL complains:
;;;             :   ? (start-environment)
;;;             :   ((#<TCP-STREAM (SOCKET/4) #x54A8926> #<PROCESS Environment-Connection(2) [Enabled] #x54A8DEE> ("127.0.0.1" . 2621)))
;;;             :   <try to load a model>
;;;             :   ? Error in update of Handler: HANDLER5
;;;             :   message: #<Anonymous Function #x54C3E9E>
;;;             :   
;;;             :   Error:Unbound variable: \?
;;;             :   
;;;             :   and then hangs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun stepper-open-p ()
  (environment-control-stepper-open *environment-control*))

(defun environment-busy-p ()
  (environment-control-busy-flag *environment-control*))

(defun set-environment-busy ()
  (setf (environment-control-busy-flag *environment-control*) t))

(defun set-environment-free ()
  (setf (environment-control-busy-flag *environment-control*) nil))



;;; Reset-model-env
;;; this is the function called when the reset button in the
;;; environment is pressed.  Basically, all it does print a message
;;; after it resets for reference.

(defun reset-model-env (x)
  (declare (ignore x))
  (if (or (stepper-open-p) (environment-busy-p))
      (print-warning "Cannot reset if ACT-R is running or if the stepper is open.")
    (unwind-protect 
        (progn
          (set-environment-busy)
          (reset)
          (format t "~%#|## ACT-R has been reset. ##|#~%"))
      (set-environment-free))))

;;; Reload-model
;;; this is the function called when the reload button in the
;;; environment is pressed.  It takes one parameter which specifies 
;;; whether or not to use the smart-loader instead of the simple reload
;;; function.

(defun reload-model (smart-load?)
  (let* ((save-stream (make-string-output-stream ))
         (display-stream (make-broadcast-stream *standard-output* save-stream))
         (error-stream (make-broadcast-stream *error-output* save-stream))
         (*standard-output* display-stream)
         (*error-output* error-stream)
         (*one-stream-hack* t)
         (internal-error nil))
    
    (if (or (stepper-open-p) (environment-busy-p))
        (list 0 "Cannot reload if ACT-R is running or if the stepper is open.")
      (unwind-protect
          (progn
            (set-environment-busy)   
            (multiple-value-bind (s err) 
                (ignore-errors 
                 (let ((*debugger-hook* (lambda (c o) 
                                          (declare (ignore o)) 
                                          (print-warning "Error aborted automatically by environment.") 
                                          (setf internal-error c)
                                          (error c))))
                   (if smart-load?
                     (reload t)
                   (reload))))
      
              (cond ((or internal-error
                         (subtypep (type-of err) 'condition))
                     (uni-report-error (if internal-error internal-error err) "Error during reload")
                     (list 0 (get-output-stream-string save-stream)))
                    ((eq s :none)
                     (print-warning "Cannot use reload")
                     (list 0 (get-output-stream-string save-stream)))
                    (t
                     (format t "~%#|##  Reload complete ##|#~%")
                     (list 1 (get-output-stream-string save-stream))))))
        (progn
          (finish-output *standard-output*)
          (finish-output *error-output*)
          (close save-stream)
          (set-environment-free))))))


(defun safe-load (file compile-it)
  
  (setf file (create-valid-pathname file))
  
  (let* ((save-stream (make-string-output-stream ))
         (display-stream (make-broadcast-stream *standard-output* save-stream))
         (error-stream (make-broadcast-stream *error-output* save-stream))
         (*standard-output* display-stream)
         (*error-output* error-stream)
         (*one-stream-hack* t)
         (internal-error nil))
    
    (unwind-protect
        (multiple-value-bind (s err) 
            (ignore-errors 
             (let ((*debugger-hook* (lambda (c o) 
                                      (declare (ignore o)) 
                                      (print-warning "Error aborted automatically by environment.") 
                                      (setf internal-error c)
                                      (error c))))
               
               (if compile-it
                   (compile-and-load file)
                 (load file))))
          (declare (ignore s))
          
          (cond ((or internal-error
                     (subtypep (type-of err) 'condition))
                 (uni-report-error (if internal-error internal-error err) "Error during load model")
                 (list 0 (get-output-stream-string save-stream)))
                (t
                 (format t "~%#|##  load model complete ##|#~%")
                 (list 1 (get-output-stream-string save-stream)))))
      (progn
        (finish-output *standard-output*)
        (finish-output *error-output*)
        (close save-stream)))))
  
(defun smart-loader (file)
  (safe-load file t))

(defun buffer-list (x)
  (declare (ignore x))
  (buffers))


(defun buffer-contents (buffer)
  (if (buffer-read buffer)
      (buffer-chunk-fct (list buffer))        
    (format *standard-output* "Buffer is Empty")))


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
