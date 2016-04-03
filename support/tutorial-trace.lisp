;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2014 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : tutorial-trace.lisp
;;; Version     : 1.0
;;; 
;;; Description : Hack to modify the model trace so it fits the page width for
;;;             : the tutorial traces.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2014.08.20 Dan 
;;;             : * Why didn't I do something like this sooner?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Call format-trace-for-tutorial providing the keyword parameters for the
;;; number of columns to use for the seconds (time-cols) and columns for the
;;; module names (module-cols).  
;;;
;;; To set things back to normal use restore-normal-trace.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Nothing public -- this is internal and use at your own risk stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Just smash the fdefinition of format-event instead of trying to redefine the
;;; methods because that makes it easier to set back.  
;;;
;;; No real safety checks in this since it's not for general use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defvar *time-cols* 1)
(defvar *module-cols* 11)
(defvar *tutorial-trace-on* nil)
(defvar *saved-format-event* nil)

(defun format-trace-for-tutorial (&key (time-cols 1) (module-cols 11))
  (setf *time-cols* time-cols)
  (setf *module-cols* module-cols)
  (unless *saved-format-event*
    (setf *saved-format-event* (fdefinition 'format-event)))
  (setf (fdefinition 'format-event) (fdefinition 'tutorial-format-event)))

(defun restore-normal-trace ()
  (when *saved-format-event*
    (setf (fdefinition 'format-event) *saved-format-event*)
    (setf *saved-format-event* nil)))


(defmethod tutorial-format-event ((event act-r-event))
  (let ((*print-pretty* nil)
        (mp (get-mp (evt-mp event))))
    (multiple-value-bind (sec ms) (when (numberp (evt-mstime event)) (truncate (evt-mstime event) 1000))
      (format nil "~:[~*~*~*~;~vd.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va ~:[~*~a~{ ~a~}~;~a~*~*~] ~:[~@[Waiting for: ~A~]~;Dynamically adjusted for: ~A~]"
        
        (evt-mstime event)
        *time-cols*
        
        sec ms
        
        (< 1 (mps-count *meta-processes*))
        (evt-mp event)
        
        (< 1 (meta-p-model-count mp))
        (meta-p-model-name-len mp)
        (evt-model event)
        
        *module-cols*
        (evt-module event)
        
        (evt-details event)
        (evt-details event)
        (evt-action event)
        (evt-params event)
        
        (evt-dynamic event)
        (evt-wait-condition event)))))


(defmethod tutorial-format-event ((event act-r-break-event))
  (let ((*print-pretty* nil)
        (mp (get-mp (evt-mp event))))
    (multiple-value-bind (sec ms) (when (numberp (evt-mstime event)) (truncate (evt-mstime event) 1000))
      (format nil "~:[~*~*~*~;~vd.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va BREAK-EVENT ~@[~a ~]~:[~@[Waiting for: ~A~]~;Dynamically adjusted for: ~A~]"
        (evt-mstime event)
        *time-cols*
        
        sec ms
        (< 1 (mps-count *meta-processes*))
        (evt-mp event)
        (< 1 (meta-p-model-count mp))
        (meta-p-model-name-len mp)
        (subseq "------" 0 (min 6 (meta-p-model-name-len mp)))
        *module-cols*
        (subseq "------" 0 (min 6 (max-module-name-length)))
        (evt-details event)
        (evt-dynamic event)
        (evt-wait-condition event)))))

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