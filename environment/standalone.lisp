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
;;; Filename    : standalone.lisp
;;; Version     : 3.0
;;; 
;;; Description : Contains the code for connecting an application
;;;             : version of the environment to the Tcl side.
;;; Bugs        : 
;;; 
;;; Todo        : [ ] Get rid of those global pathname variables.
;;; 
;;; ----- History -----
;;;
;;; 10/01/2002  Dan
;;;             : File creation
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; 2005.05.12 Dan
;;;             : * Added the safe-load and smart-load functions so that
;;;             :   the standalone can have the open dialog.
;;; 2008.08.21 Dan
;;;             : * Added code for the ACL version of the standalone to 
;;;             :   automatically exit the error which can get generated
;;;             :   during a define-model call.
;;; 2010.06.07 Dan
;;;             : * Changed run-standalone to print out the version info.
;;; 2010.06.07 Dan
;;;             : * Not useful since that wouldn't show up in the Listener
;;;             :   window of the environment anyway...
;;; 2010.08.11 Dan
;;;             : * Changed start-listener-outputer to print the framework version
;;;             :   info at the top of the listener.  Something other than an
;;;             :   empty string needed to be sent to avoid an error being
;;;             :   logged anyway now that the error logging works better.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.07.06 Dan 
;;;             : * Fixed a problem that prevented the standalone from working
;;;             :   properly.
;;;             : * Added the package switches.
;;; 2011.09.02 Dan
;;;             : * Changed to use the new background option in connect-to-
;;;             :   environment so that the message processing itself is the
;;;             :   foreground thread in the standalone.
;;;             : * Changed how eval-command works because if it doesn't
;;;             :   spawn as a new process it blocks out other things which
;;;             :   is not good.  It now has its own lock so that only one
;;;             :   command can be evaled at a time, but if they're queued
;;;             :   up by the user there's no guarantee on ordering...
;;; 2011.09.07 Dan
;;;             : * Changed how the eval-command works so that it doesn't
;;;             :   require the 'outputer' process running constantly, but
;;;             :   instead only spawns it when there's a command being 
;;;             :   evaled.
;;;             : * Similarly, for the standalone redefine process-connection
;;;             :   so it captures output for sending to the listener window.
;;; 2011.09.08 Dan
;;;             : * A few more tweaks and it seems pretty good except for
;;;             :   an issue with running ACT-R in the background process of
;;;             :   CCL -- it stack errors quickly.
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defvar *stop-environment* nil)

(require-compiled "UNI-FILES" "ACT-R6:support;uni-files")

(defun run-standalone ()
  #+(and :allegro :actr-env-alone) (setf *debugger-hook* 'standalone-debug-exit)
  (connect-to-environment :background nil))


#+(and :allegro :actr-env-alone)
(defun standalone-debug-exit (a b) 
  (declare (ignore a b))
  (model-warning "Error has been cleared automatically") 
  (invoke-restart-interactively (second (compute-restarts))))

(defvar *eval-lock* (uni-make-lock "Evaluation lock"))
(defvar *eval-handler* nil)

#|
(defvar *record-eval-data* nil)
(defvar *eval-data* nil)

(defun start-eval-record ()
  (setf *eval-data* nil)
  (setf *record-eval-data* t))

(defun stop-eval-record ()
  (setf *record-eval-data* nil))


(defun trace-eval-dummy (line not-end start index)
  (when *record-eval-data*
    (push-last (list line not-end start index) *eval-data*)))

|#


(defun eval-command (cmd)
  (let* ((output-string (make-array '(0) :element-type 'base-char
                                    :fill-pointer 0 :adjustable t))
         (process (uni-run-process "Eval Output"
                                    #'(lambda ()
                                        (let ((index 0)
                                              (start 0)
                                              (record nil))
                                          (loop 
                                            (setf record nil)
                                            (with-input-from-string (s output-string :index index :start start)
                                              (multiple-value-bind (line not-end) (read-line s nil :actr-listener-eval-output-error)
                                                ;(trace-eval-dummy line not-end start index)
                                                (unless not-end
                                                (if (string-equal line ":actr-listener-eval-complete")
                                                    (return)
                                                  (unless (eq line :actr-listener-eval-output-error)
                                                    (setf record t)
                                                    (send-listener-output line))))))
                                            (when record
                                              (setf start index))))))))
    
    
    
    (uni-run-process "Evaluation from Listener"
                     #'(lambda ()
                         (unwind-protect 
                             (with-output-to-string (s output-string)
                               (uni-lock *eval-lock*)
                               (let ((*standard-output* s)
                                     (*error-output* s))
                                 (format t "~%> ~A~%" cmd)
                                 (multiple-value-bind (result err)
                                     (ignore-errors (read-from-string cmd))
                                   (if (and (subtypep (type-of err) 'condition)
                                            (not (equal (type-of err) 'unbound-variable)))
                                       (progn
                                         (format t "~S~%" (type-of err))
                                         (uni-report-error err (format nil "Error in command: ~s:~%" cmd)))
                                     (multiple-value-bind (res err)
                                         (ignore-errors (eval result))
                                       (if (and (subtypep (type-of err) 'condition)
                                                (not (equal (type-of err) 'unbound-variable)))
                                           (progn
                                             (format t "~S~%" (type-of err))
                                             (uni-report-error err (format nil "Error executing command: ~s:~%" cmd)))
                                         (format t "~%~S~%" res)))))
                                 
                                 (format t "~%:actr-listener-eval-complete~%")))
                         (progn
                           (uni-unlock *eval-lock*)))))
    )
  )

#|(uni-run-process "Evaluation from Listener"
                   #'(lambda ()
                       (unwind-protect 
                           (progn
                             (uni-lock *eval-lock*)
                             (format t "~%> ~A~%" cmd)
                             (multiple-value-bind (result err)
                                 (ignore-errors (read-from-string cmd))
                               (if (and (subtypep (type-of err) 'condition)
                                        (not (equal (type-of err) 'unbound-variable)))
                                   (progn
                                     (format t "~S~%" (type-of err))
                                     (uni-report-error err (format nil "Error in command: ~s:~%" cmd)))
                                 (multiple-value-bind (res err)
                                     (ignore-errors (eval result))
                                   (if (and (subtypep (type-of err) 'condition)
                                            (not (equal (type-of err) 'unbound-variable)))
                                       (progn
                                         (format t "~S~%" (type-of err))
                                         (uni-report-error err (format nil "Error executing command: ~s:~%" cmd)))
                                     (format t "~%~S~%" res))))))
                         
                         (uni-unlock *eval-lock*)))))

|#
    
(defun start-listener-outputer (handler)
  (setf *eval-handler* handler)
  (format nil ";;; ACT-R Standalone Environment version ~a~%" (meta-p-version (gethash 'default (mps-table *meta-processes*)))))


(defvar *listener-output-lock* (uni-make-lock "Listener output"))

(defun send-listener-output (string)
  (when (> (length string) 0)
    (unwind-protect 
        (progn
          (uni-lock *listener-output-lock*)
          (setf (update-value *eval-handler*) (concatenate 'string string (string #\newline)))
          (send-update *eval-handler*))
      (uni-unlock *listener-output-lock*))))

;;; Special version which sends all output to the listener dialog


(defun process-connection (connection cmd-list)
  (if *eval-handler*
      (let* ((output-string (make-string-output-stream))
             (*standard-output* output-string)
             (*error-output* output-string))
        (sub-process-connection connection cmd-list)  
        (send-listener-output (get-output-stream-string output-string))
        (close output-string))
      (sub-process-connection connection cmd-list)))

(defun sub-process-connection (connection cmd-list) 
  (let ((*local-connection* (environment-connection-local connection)))
    
    (case (car cmd-list) 
      (create ;; make a new handler instance and send a register + update back
       (if (or (= (length cmd-list) 6)(= (length cmd-list) 7))
           (let ((new-handler (make-instance (second cmd-list) 
                                :use-model (= (length cmd-list) 7)
                                :model (if (= (length cmd-list) 7) (seventh cmd-list) nil)
                                :socket (environment-connection-stream connection)
                                :object-name (third cmd-list) 
                                :target-name (fourth cmd-list)
                                :update-form (functionify (fifth cmd-list)))))
             (setf (gethash (name new-handler) (environment-connection-handlers connection)) new-handler)
             (send-register new-handler)
             (update-handler new-handler new-handler)
             (dolist (x (sixth cmd-list))
               (case x
                 ((pre post conflict conflict-nil create delete reset run-start run-end)
                  (push new-handler (gethash x (environment-connection-hooks connection))))
                 (t (model-warning "Invalid hook ~s for handler ~S" x cmd-list)))))
         (format *error-output* "Invalid create message: ~s" cmd-list)))
      (update ;; change the update form if requested and send an update back
       (cond ((= (length cmd-list) 2)
              (let ((handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                (when handler
                  (update-handler handler nil))
                ;; the when used to be an if but now it'll just
                ;; silently ignore removed handlers
                ;(format *error-output* "Warning: update for removed handler ~S~%" (second cmd-list))
                ))
             ((= (length cmd-list) 3)
              (let ((handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                (when handler
                  (setf (update-form handler) (functionify (third cmd-list)))
                  (update-handler handler nil))
                ;; the when used to be an if but now it'll just
                ;; silently ignore removed handlers
                ;(format *error-output* "Warning: update for removed handler ~S~%" (second cmd-list))
                ))
             (t
              (format *error-output* "Invalid update message: ~s" cmd-list))))
      (remove ;; take the handler off the lists and free its name
              ;; calling the optional end function if necessary
       (cond ((or (= (length cmd-list) 2) (= (length cmd-list) 3))
              (let ((handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                
                ;; This should be unnecessary now since it locks out handling 
                ;; and a create must finish before the remove could be processed
                ;(while (null handler)
                ;  (uni-process-system-events)
                ;  (setf handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                
                (when (= (length cmd-list) 3)
                  (safe-evaluation (third cmd-list) handler))
                
                (delete-handler handler connection)))
             (t (format *error-output* "Invalid remove message: ~s" cmd-list))))
      (k-a ;; don't do anything - just to make sure the socket doesn't timeout
       (ignore-errors (uni-send-string (environment-connection-stream connection) (format nil "ka nil nil<end>~%"))))
      (goodbye ;; kill the connection
       (format t "Environment Closed~%")
       (close-connection connection :kill nil))
      (t 
       (format *error-output* "Invalid command request: ~s~%" cmd-list)))))


#|
#+:ACTR-ENV-ALONE
(defun smart-loader (file)
  (declare (ignore file))
  (list 0 "You must disable the 'Compile definitions when model opened or reloaded' option to be able to open a model"))
|#


#-:ccl-5.0 

(defun create-valid-pathname (path) path)


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
