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
;;; Filename    : stepper-control.lisp
;;; Version     : 3.0
;;; 
;;; Description : No system dependent code.
;;;             : This file contains the Lisp to support the stepper window.
;;;             : 
;;; Bugs        : 
;;; 
;;; Todo        :
;;; 
;;; ----- History -----
;;;
;;; 05/24/2002  Dan
;;;             : File creation
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Modified the stepper-control-function to use the
;;;             : new uni-wait-for function.
;;; 10/14/2002  Dan
;;;             : Made the changes to implement the tutor mode
;;;             : in the instantiation window instead of the bindings
;;;             : window of the stepper.
;;; 11/11/2002  Dan
;;;             : Modified stepper-instan-info and stepper-control-function
;;;             : so that instantiation picking was possible.  It works
;;;             : off of the step buttons value and the *last-stepper-instantiation*
;;;             : variable.  Requires that the option be set in the
;;;             : environment before it's enabled.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;;             : Updated run-master-process with the RPM 2.2 version.
;;; -----------------------------------------------------------------------
;;; 2005.04.13  Dan
;;;             : * Moved to ACT-R 6.
;;;             : * LOTS of things to change - not included yet.
;;; 2005.04.20  Dan
;;;             : * Updated and added - should work with the tutorial mode.
;;; 2005.05.14 Dan
;;;             : * Fixed a typo in the stepper window - it was printing
;;;             :   +retreval> for retrieval requests...
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to declare event unused in stepper-test.
;;; 2006.03.10 Dan
;;;             : * Calls to get-production no longer need the procedural
;;;             :   module so took that out of stepper-instan-binding.
;;; 2007.07.13 Dan
;;;             : * Added the stepper-stop-button function because the
;;;             :   stop button is being put back on the stepper.
;;; 2007.08.03 Dan
;;;             : * Moved the *stepper-open* defvar to environment-cmds
;;;             :   because it's used there and that's loaded first...
;;; 2007.08.07 Dan
;;;             : * When :esc is set to t the stepper now shows the 
;;;             :   declarative or procedural parameters for the 
;;;             :   item in a new window in the lower left.
;;; 2007.08.08 Dan
;;;             : * Put the "run until" button back into the stepper and
;;;             :   added module as an option now too.
;;; 2007.08.15 Dan
;;;             : * The chunk list shown in the stepper is now sorted
;;;             :   by activation with the chunk being retrieved at the top.
;;; 2011.04.20 Dan
;;;             : * Changed the evt-time call to an evt-mstime one and
;;;             :   converted the user time to ms.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.05.25 Dan
;;;             : * Fixed a bug in the tutor-completed function.
;;; 2011.05.26 Dan 
;;;             : * Protected some code that assumed there was a model for
;;;             :   eventual use with multiple models.
;;; 2011.05.31 Dan
;;;             : * Removed a declaration from tutor-completed.
;;;             : * Changed stepper-test so that when it's "until" a production
;;;             :   that name can be from any of the defined models.
;;; 2012.03.01 Dan
;;;             : * Added the macro suppress-declarative-noise which will
;;;             :   temporarily set :ans to nil so that when sdp gets called
;;;             :   from the stepper it doesn't affect the current random
;;;             :   stream because otherwise it changes the results for a
;;;             :   fixed seed model when stepping through it and that makes
;;;             :   debugging more difficult.
;;; 2012.03.21 Dan
;;;             : * Instead of suppress-declarative-noise I can just use 
;;;             :   with-parameters to temporarily set :ans to nil.
;;; 2012.10.25 Dan
;;;             : * Fixed the stepper so it doesn't step when :v is nil, which
;;;             :   is what it's documented as doing i.e. only stepping on
;;;             :   events that show in the trace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R6:support;uni-files")

(defun init-stepper ()
  (let ((stepper (environment-control-stepper *environment-control*)))
    (setf (environment-control-stepper-open *environment-control*) t)
    (clrhash (stepper-control-handlers stepper))      ; *stepper-handlers*
    (setf (stepper-control-wait stepper) t)            ; *wait-for-step*
    (setf (stepper-control-current-event stepper) nil) ;*current-stepper-event*
    (setf (stepper-control-mode stepper) :production)    ;*stepper-viewer-mode*
    (setf (stepper-control-tutor-bindings stepper) nil)  ;*tutor-bindings*
    (setf (stepper-control-tutor-responses stepper) nil) ;*tutor-responses*
    (setf (stepper-control-skip-val stepper) nil)        ;*stepper-skip-val*
    (setf (stepper-control-skip-type stepper) nil)       ;*stepper-skip-type*
    1))

(defun remove-stepper ()
  (setf (environment-control-stepper-open *environment-control*) nil)
  (let ((stepper (environment-control-stepper *environment-control*)))
    
    (setf (stepper-control-wait stepper) nil)
    (setf (stepper-control-current-event stepper) nil)
    1))


(defun set-stepper-skip-time (vals)
  (let ((stepper (environment-control-stepper *environment-control*)))
    
    (setf (stepper-control-skip-type stepper) (car vals))
    (setf (stepper-control-skip-val stepper) (second vals))))
  

(defun stepper-test (event)
  ;; To be expanded with module/buffer based user settings
  (let ((stepper (environment-control-stepper *environment-control*)))
    
    (case (stepper-control-skip-type stepper)
      (production 
     
       (cond ((or (not (symbolp (stepper-control-skip-val stepper)))
                  (notany (lambda (model) 
                            (with-model-eval model 
                              (find (stepper-control-skip-val stepper) (all-productions))))
                          (mp-models)))
              (print-warning "Run Until Production requires a valid production name value.")
              (print-warning "Normal stepping is being used instead.")
              t)
             ((and (or (eq (evt-action event) 'production-selected)
                       (eq (evt-action event) 'production-fired))
                   (eq (stepper-control-skip-val stepper) (production-name (car (evt-params event)))))
              (setf (stepper-control-skip-val stepper) nil)
              (setf (stepper-control-skip-type stepper) nil)
              t)
             (t nil)))
      (time 
       (cond ((not (numberp (stepper-control-skip-val stepper)))
              (print-warning "Run Until Time requires a number (time in seconds) as the value.")
              (print-warning "Normal stepping is being used instead.")
              t)
             ((>= (evt-mstime event) (seconds->ms (stepper-control-skip-val stepper)))
              (setf (stepper-control-skip-val stepper) nil)
              (setf (stepper-control-skip-type stepper) nil)
              t)
             (t nil)))
      (module 
       (cond ((not (find (string (stepper-control-skip-val stepper)) (mapcar #'string (all-module-names)) :test #'string-equal) )
              (print-warning "Run Until Module requires a valid module name.")
              (print-warning "Normal stepping is being used instead.")
              t)
             ((string-equal (string (stepper-control-skip-val stepper)) (string (evt-module event)))
              (setf (stepper-control-skip-val stepper) nil)
              (setf (stepper-control-skip-type stepper) nil)
              t)
             (t nil)))
      (t t))))


(defun model-trace-enabled (event)
  (with-model-eval (if (evt-model event) (evt-model event) (first (mp-models))) ;; just use the first if there isn't one (a break event)
    (car (no-output (sgp :v)))))
    
    
(defun stepper-step (event)
  (let ((stepper (environment-control-stepper *environment-control*)))
    (when (and event (act-r-event-p event) 
               (or (and (event-displayed-p event) 
                        (model-trace-enabled event))
                   (and (eq (stepper-control-mode stepper) :tutor)
                        (eq (evt-module event) 'procedural)
                        (eq (evt-action event) 'production-selected)))
               (stepper-test event))
      
      (setf (stepper-control-current-event stepper) event)
      (setf (stepper-control-tutor-bindings stepper) nil)
      
      (dolist (x (hash-table-keys (stepper-control-handlers stepper)))
        (update-handler x event))
      
      (while (stepper-control-wait stepper)
        (uni-process-system-events))
      
      (dolist (x (hash-table-keys (stepper-control-handlers stepper)))
        (update-handler x nil))
      
      (setf (stepper-control-wait stepper) t))))

(defun add-handler-to-stepper (handler)
  (let ((stepper (environment-control-stepper *environment-control*)))
    (when (subtypep (type-of handler) 'environment-handler) 
      (unless (gethash handler (stepper-control-handlers stepper))
        (setf (gethash handler (stepper-control-handlers stepper)) t))))
  nil)

(defun stepper-step-button (x)
  (unless x
    (setf (stepper-control-wait (environment-control-stepper *environment-control*)) nil)))


(defun stepper-stop-button (x)
  (schedule-break-relative 0 :priority :max :details "Stopped by the stepper")
  (unless x
    (setf (stepper-control-wait (environment-control-stepper *environment-control*)) nil)))

(defun next-event-display (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((act-r-event-p event)
         (format-event event))
        (t " ")))

(defun stepper-list-name (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((act-r-event-p event)
         (case (evt-module event)
           (procedural (case (evt-action event)
                         (production-selected "Possible Productions")
                         (production-fired "Production")
                         (t " ")))
           (declarative (case (evt-action event)
                         (retrieved-chunk "Possible Chunks")
                         (t " ")))
           (t " ")))
        (t " ")))

(defun stepper-list-values (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         nil)
        ((and (act-r-event-p event) (current-model))
         (let ((env (get-module :environment)))
           (case (evt-module event)
             (procedural (case (evt-action event)
                           (production-selected (env-mod-conflict-set env))
                           (production-fired (subseq (env-mod-conflict-set env) 0 1))
                           (t nil)))
             (declarative (case (evt-action event)
                            (retrieved-chunk (if (env-mod-esc env)
                                                 (let ((c (copy-list (env-mod-last-dm-set env))))
                                                   (cons (car c) (sort (cdr c) #'> :key #'(lambda (x) (no-output (caar (sdp-fct (list x :last-retrieval-activation)))))))
                                                   )
                                               (env-mod-last-dm-set env)))
                            (t nil)))
             (t nil))))
        (t nil)))


(defun stepper-prod_frame-name (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((and (act-r-event-p event) (current-model))
         (case (evt-module event)
           (procedural (case (evt-action event)
                         (production-selected "Production")
                         (production-fired "Production")
                         (t " ")))
           (declarative (case (evt-action event)
                         (retrieved-chunk "Chunk")
                         (t " ")))
           (t " ")))
        (t " ")))



(defun stepper-instan-production (prod)
  (let ((stepper (environment-control-stepper *environment-control*)))
   (when prod
    (cond ((act-r-event-p (stepper-control-current-event stepper))
           (case (evt-module (stepper-control-current-event stepper))
             (procedural (case (evt-action (stepper-control-current-event stepper))
                           (production-selected (if (or (eq (stepper-control-mode stepper) :production)
                                                        (eq (stepper-control-mode stepper) :tutor))
                                                    (pp-fct (list prod))
                                                  (pprint-instantiation-fct prod)))
                           (production-fired (pprint-instantiation-fct prod))
                           (t nil)))
             (declarative (case (evt-action (stepper-control-current-event stepper))
                            (retrieved-chunk  (dm-fct (list prod)))
                            (t nil)))
             (t nil)))
          (t nil)))))

(defun stepper-parameter-frame-name (event)
  (if (and (current-model) (env-mod-esc (get-module :environment)))
      (cond ((subtypep (type-of event) 'environment-handler)
             (add-handler-to-stepper event)
             " ")
            ((act-r-event-p event)
             (case (evt-module event)
               (procedural (case (evt-action event)
                             (production-selected "Production Parameters")
                             (production-fired "Production Parameters")
                             (t " ")))
               (declarative (case (evt-action event)
                              (retrieved-chunk "Chunk Parameters")
                              (t " ")))
               (t " ")))
            (t " "))
    " "))


(defmacro suppress-declarative-noise (&body body)
  (let ((old-val (gensym)))
    `(let ((,old-val (car (no-output (sgp :ans)))))
       (if ,old-val
           (unwind-protect (progn 
                             (no-output (sgp :ans nil)) 
                             ,@body)
                     (no-output (sgp-fct (list :ans ,old-val))))
         ,@body))))


(defun stepper-parameter-frame-values (prod)
  (let ((stepper (environment-control-stepper *environment-control*)))
    (when (and prod (env-mod-esc (get-module :environment)))
      (cond ((act-r-event-p (stepper-control-current-event stepper))
             (case (evt-module (stepper-control-current-event stepper))
               (procedural (case (evt-action (stepper-control-current-event stepper))
                             (production-selected (spp-fct (list prod)))
                             (production-fired (spp-fct (list prod)))
                             (t nil)))
               (declarative (case (evt-action (stepper-control-current-event stepper))
                              (retrieved-chunk (with-parameters (:ans nil) (sdp-fct (list prod))))
                              (t nil)))
               (t nil)))
            (t nil)))))


(defun tutored-step ()
  (let ((stepper (environment-control-stepper *environment-control*)))
    (and (act-r-event-p (stepper-control-current-event stepper))
         (eq (evt-module (stepper-control-current-event stepper)) 'procedural)
         (eq (evt-action (stepper-control-current-event stepper)) 'PRODUCTION-SELECTED)
         (eq (stepper-control-mode stepper) :tutor))))
          

(defun stepper-bindings-name (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((act-r-event-p event)
         (case (evt-module event)
           (procedural (case (evt-action event)
                         (production-selected "Bindings")
                         (production-fired "Bindings")
                         (t " ")))
           (declarative (case (evt-action event)
                         (retrieved-chunk "Retrieval Request")
                         (t " ")))
           (t " ")))
        (t " ")))



(defun stepper-instan-binding (prod)
  (let ((stepper (environment-control-stepper *environment-control*)))
    (when (and prod (current-model))
      (let ((env (get-module :environment)))
        (when env
          (cond ((act-r-event-p (stepper-control-current-event stepper))
                 (case (evt-module (stepper-control-current-event stepper))
                   (procedural (case (evt-action (stepper-control-current-event stepper))
                                 (production-selected (let ((len 0))
                                                        (let ((bindings (production-bindings (get-production prod))))
                                                          (when bindings
                                                            (setf len (apply #'max (mapcar #'(lambda (x)
                                                                                               (length (symbol-name (car x)))) bindings)))
                                                            
                                                            (if (eq (stepper-control-mode stepper) :tutor)
                                                                (progn
                                                                  (when (null (stepper-control-tutor-bindings stepper))
                                                                    (setf (stepper-control-tutor-bindings stepper) (copy-tree bindings))
                                                                    (setf (stepper-control-tutor-responses stepper) (mapcar #'(lambda (x) (cons (car x) nil)) (stepper-control-tutor-bindings stepper))))
                                                                  (dolist (x (reverse (stepper-control-tutor-responses stepper)))
                                                                    (command-output "~vA : ~a" len (car x) (if (null (cdr x)) "{binding}" (cdr x)))))
                                                              
                                                              (dolist (binding (reverse bindings))
                                                                (command-output "~vA : ~s" len (car binding) (cdr binding))))))))
                                 (production-fired 
                                  (let ((len 0))
                                    (let ((bindings (production-bindings (get-production prod))))
                                      (when bindings 
                                        (setf len (apply #'max (mapcar #'(lambda (x)
                                                                           (length (symbol-name (car x)))) bindings)))
                                        
                                        (dolist (binding (reverse bindings))
                                          (command-output "~vA : ~s" len (car binding) (cdr binding)))))))
                                 (t nil)))
                   (declarative (case (evt-action (stepper-control-current-event stepper))
                                  (retrieved-chunk (command-output "+retrieval>")
                                                   (pprint-chunk-spec 
                                                    (env-mod-last-dm-request env)))
                                  (t nil)))
                   (t nil)))
                (t nil)))))))
    

(defun tutor-completed (word)
  (if (cdr (assoc word (stepper-control-tutor-responses (environment-control-stepper *environment-control*))))
      1 0))

(defun tutor-check (word binding)
  (let ((stepper (environment-control-stepper *environment-control*)))
    
    (if (equal binding (cdr (assoc word (stepper-control-tutor-bindings stepper))))
      (progn
        (setf (cdr (assoc word (stepper-control-tutor-responses stepper))) binding)
        1)
    0)))

(defun tutor-answer (word)
  (cdr (assoc word (stepper-control-tutor-bindings (environment-control-stepper *environment-control*)))))


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
