;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : handlers.lisp
;;; Version     : 3.0
;;; 
;;; Description : No system dependent code.
;;;             : Defines the class for the environment "handlers", 
;;;             : the function that processes the incoming messages, and
;;;             : the functions that build the Lisp->Tcl messages.
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;;
;;; 05/10/2002  Dan
;;;             : Added this header
;;; 05/20/2002  Dan
;;;             : Think I've finally got it working on Macs w/ OS < 10 and MCL.
;;; 05/22/2002  Dan
;;;             : Moved the system dependent code out of here.
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Added the simple-text-handler class.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; -------------------------------------------------------------------------
;;; 2005.04.12  Dan [2.0]
;;;             : * Moving to ACT-R 6.
;;;             :
;;; 2007.08.10  Dan
;;;             : * Adding a new class of handlers - a simple-funcall.  It
;;;             :   works like a simple handler on the Lisp side but considers
;;;             :   the target to be a procedure name that gets called on the Tcl/Tk 
;;;             :   side with the value of the update.
;;; 2007.08.13  Dan
;;;             : * Added the removal from the new reset-hook-list to the
;;;             :   delete-handler code.
;;; 2008.05.16  Dan
;;;             : * Updated delete-handler to remove the conflict-nil hooks.
;;; 2008.08.20  Dan
;;;             : * Added a new handler text-handler.  Unlike the simple-text
;;;             :   handler this one always erases the target box on the Tcl
;;;             :   side.
;;; 2009.04.13  Dan
;;;             : * Uni-send-string doesn't have an automatic newline now,
;;;             :   so need to put one in the string to be sent.
;;; 2010.09.14  Dan
;;;             : * Changed the output handler to protect things better since
;;;             :   LispWorks 6 showed some odd errors with stream setting.
;;; 2010.09.15  Dan
;;;             : * More thorough protection that the last one with all handlers
;;;             :   now locking out until completion.
;;; 2011.01.07 Dan
;;;             : * Can't lock out on the updates like that because it breaks
;;;             :   the stepper because it's update is called during the pre-
;;;             :   event hook in the ACT-R "thread" and it's waiting for the
;;;             :   go-ahead from an update that needs to happen from the 
;;;             :   background thread.
;;;             :   Need a redesign on some of this stuff, but for now killing
;;;             :   the run button and moving the lock out to the background
;;;             :   thread code only seems like it'll work.
;;; 2011.02.21  Dan 
;;;             : * Added the deletion code to handle the run-start and run-end hooks.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * A complete overhaul of the environment connection code
;;;             :   on the path to supporting multiple models.
;;;             : * Removed the delete-handler method and *env-windows*.
;;; 2011.05.24 Dan
;;;             : * Took the class definitions out and put them into handler-class.lisp.
;;; 2011.05.25 Dan
;;;             : * Changed the update-handler methods to only specify the
;;;             :   model when needed.
;;; 2011.05.26 Dan
;;;             : * When there's no model the output-handler update doesn't
;;;             :   need to set the :cmdt parameter since there isn't one...
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;; 2012.03.21 Dan
;;;             : * Use with-parameters instead of explicitly saving and 
;;;             :   restoring :cmdt in the output handler.
;;; 2012.09.21 Dan
;;;             : * Moving the let for *standard-output* in the output 
;;;             :   handler to avoid potential issues between the closeing
;;;             :   of the string stream and restoring *standard-output*.
;;; 2012.09.24 Dan
;;;             : * Adding a finish-output to the output-handler before 
;;;             :   closing the stream to be even more careful...
;;; 2012.10.25 Dan
;;;             : * Fix a problem with the output-handler when there isn't
;;;             :   a current model -- it doesn't use with-parameters now when
;;;             :   there isn't a model.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defmethod initialize-instance :after ((handler env-window-handler) &key)
  (push handler (environment-control-windows *environment-control*)))

;;; a listbox needs a special list handler

;;; safe-update-evaluation
;;; because the handlers get functions from Tcl that are
;;; then 'apply'ed to produce a result there's the potential for
;;; errors in either the generation or transmission of those expressions
;;; so this function performs the evaluation of a handlers update-form
;;; making sure to catch any errors and print a message if an error occurs.
;;; It returns 2 values on a successful eval the first is the result of that
;;; eval and the second is t.  If there is any condition generated by the
;;; eval then it returns 2 nils.

 
(defmethod safe-update-evaluation ((handler environment-handler) arg)
  (multiple-value-bind (result err)
      (ignore-errors (apply (update-form handler) (list arg)))
    (if (and (subtypep (type-of err) 'condition)
             (not (equal (type-of err) 'unbound-variable)))
        (progn
          ;(format t "~S~%" (type-of err))
          (uni-report-error err (format nil "Error in update of Handler: ~s~%For object: ~s~%message: ~s~%"
                                  (name handler) (obj-name handler) (update-form handler)))
          
          (values nil nil))
      (values result t))))


(defun valid-model (model)
  (and model (find model (mp-models))))

(defgeneric update-handler (handler arg)
  (:documentation  "The method called when an environment 
                    item needs to be updated.  The arg
                    is the argument passed to the hook function
                    if there is such an argument."))

;;; for a simple-handler just save the result and then send it

(defmethod update-handler ((handler simple-handler) arg)
  (let ((model (aif (handler-model handler) it (current-model))))
    (cond ((and (use-model handler) (valid-model model))
        
           (with-model-eval model
             (multiple-value-bind (result success)
                 (safe-update-evaluation handler arg)
               (when success
                 (setf (update-value handler) result)
                 (send-update handler)
                 result))))
          ((use-model handler)
           (print-warning "Environment trying to use model ~s which no longer exists." model))
          (t
           (multiple-value-bind (result success)
               (safe-update-evaluation handler arg)
             (when success
               (setf (update-value handler) result)
               (send-update handler)
               result))))))

;;; for a list handler verify that the result is a list, and if so
;;; set the update-value to be a string containing a Tcl list of those
;;; elements otherwise set the update value to "EMPTY_ENV_STRING"
;;; and then send the update

(defmethod update-handler ((handler list-handler) arg)
   (let ((model (aif (handler-model handler) it (current-model))))
    (cond ((and (use-model handler) (valid-model model))
        
           (with-model-eval model
             (multiple-value-bind (result success)
                 (safe-update-evaluation handler arg)
               (when success
                 (setf (update-value handler) (if (and result (listp result))
                                                  (string-downcase (format nil "~{~S ~}" result))
                                                "EMPTY_ENV_STRING"))
                 (send-update handler)
                 result))))
          ((use-model handler)
           (print-warning "Environment trying to use model ~s which no longer exists." model))
          (t
           (multiple-value-bind (result success)
               (safe-update-evaluation handler arg)
             (when success
               (setf (update-value handler) (if (and result (listp result))
                                                (string-downcase (format nil "~{~S ~}" result))
                                              "EMPTY_ENV_STRING"))
               (send-update handler)
               result))))))

;;; for the environment window handler I don't want to downcase
;;; the parameters, but other than that it's like the list-handler

(defmethod update-handler ((handler env-window-handler) arg)
  (let ((model (aif (handler-model handler) it (current-model))))
    (cond ((and (use-model handler) (valid-model model))
           (with-model-eval model
             (multiple-value-bind (result success)
                 (safe-update-evaluation handler arg)
               (when success
                 (setf (update-value handler) (if (and result (listp result))
                                                  (concatenate 'string
                                                    (string-downcase (format nil "~S " (car result)))
                                                    (format nil "~{~S ~}" (cdr result)))
                                                "EMPTY_ENV_STRING"))
                 (send-update handler)
                 result))))
          ((use-model handler)
           (print-warning "Environment trying to use model ~s which no longer exists." model))
          (t
           (multiple-value-bind (result success)
               (safe-update-evaluation handler arg)
             (when success
               (setf (update-value handler) (if (and result (listp result))
                                                (concatenate 'string
                                                  (string-downcase (format nil "~S " (car result)))
                                                  (format nil "~{~S ~}" (cdr result)))
                                              "EMPTY_ENV_STRING"))
               (send-update handler)
               result))))))


;;; for an output-handler capture the output to *standard-output* and
;;; the ACT-R *command-trace*.  If there is any output set the value to
;;; that otherwise set the update value to "EMPTY_ENV_STRING" and
;;; then send the update

(defmethod update-handler ((handler output-handler) arg)
  (let ((model (aif (handler-model handler) it (current-model))))
    (cond ((and (use-model handler) (valid-model model))
           (with-model-eval model
             (let* ((s (make-string-output-stream)))
               (unwind-protect 
                   (let ((*standard-output* s))
                     (with-parameters-fct (:cmdt s)
                       (multiple-value-bind (result success)
                           (safe-update-evaluation handler arg)
                         (when success
                           (setf (update-value handler) (get-output-stream-string s))
                           (when (zerop (length (update-value handler)))
                             (setf (update-value handler) "EMPTY_ENV_STRING"))
                           (send-update handler)
                           result))))
                 (progn
                   (finish-output s)
                   (close s))))))
          ((use-model handler)
           (print-warning "Environment trying to use model ~s which no longer exists." model))
          (t
           (let* ((s (make-string-output-stream)))
             (if (valid-model model)
                 (unwind-protect 
                     (let ((*standard-output* s))
                       (with-parameters-fct (:cmdt s)
                         (multiple-value-bind (result success)
                             (safe-update-evaluation handler arg)
                           (when success
                             (setf (update-value handler) (get-output-stream-string s))
                             (when (zerop (length (update-value handler)))
                               (setf (update-value handler) "EMPTY_ENV_STRING"))
                             (send-update handler)
                             result))))
                   (progn
                     (finish-output s)
                     (close s)))
               (unwind-protect 
                   (let ((*standard-output* s))
                     (multiple-value-bind (result success)
                         (safe-update-evaluation handler arg)
                       (when success
                         (setf (update-value handler) (get-output-stream-string s))
                         (when (zerop (length (update-value handler)))
                           (setf (update-value handler) "EMPTY_ENV_STRING"))
                         (send-update handler)
                         result)))
                 (progn
                   (finish-output s)
                   (close s)))
               
               ))))))
    

(defmethod delete-handler ((handler environment-handler) connection)
  (remhash (name handler) (environment-connection-handlers connection))
  
  (maphash (lambda (hook handlers)
             (setf (gethash hook (environment-connection-hooks connection)) (remove handler handlers)))
           (environment-connection-hooks connection))
  
  (setf (environment-control-windows *environment-control*)
    (remove handler (environment-control-windows *environment-control*)))
  
  (unintern (name handler))
  (setf (name handler) nil))

;;; send_update
;;; this method builds the string containing a Lisp -> Tcl update message
;;; as described in messages.txt for a handler and sends it to Tcl

(defmethod send-update ((handler environment-handler))
  (let ((*print-case* :downcase))
    (send-environment-message handler (format nil "update ~a ~a ~a" 
                                        (update-type handler) 
                                        (target-name handler)
                                        (update-value handler)))))
;;; send_register
;;; this method builds the string containing a Lisp -> Tcl register message
;;; as described in messages.txt for a handler and sends it to Tcl

(defmethod send-register ((handler environment-handler))
  (let ((*print-case* :downcase))
    (send-environment-message handler (format nil "register ~a ~a"
                                        (obj-name handler)
                                        (name handler)))))



;;; send-environment-message 
;;; This method takes two parameters the first is an environment-handler
;;; and the second is a string of a message to send.  That message is
;;; printed down the socket-stream associated with the handler with the
;;; <end> tag attached.

(defmethod send-environment-message ((handler environment-handler) message)
  ;(format t "sent message: ~S~%" message)
  (multiple-value-bind (value condition)
      (ignore-errors   
       (uni-send-string (socket handler) (format nil "~a<end>~%" message))) 
    (declare (ignore value))
    (when (subtypep (type-of condition) 'condition)
      (uni-report-error condition 
                        (format nil "~S Failed while sending message:~%~S~%"
                          (name handler) message)))))



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
