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
;;; Filename    : sort-environment-lists.lisp
;;; Version     : 1.0
;;; 
;;; Description : Causes environment list boxes to be sorted alphabetically.
;;; 
;;; Bugs        : None?
;;;
;;; To do       : Roll this in as an option for the main environment files.
;;; 
;;; ----- History -----
;;;
;;; 2005.11.16 Dan
;;;             : * Added this to the extras for ACT-R 6 for now.
;;; 2011.06.03 Dan
;;;             : * Fixed this to work with the new multiple model environment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Move this file into the other-files directory of the ACT-R 6 distribution
;;; before loading ACT-R 6 if you would like the list boxes in the environment
;;; to be sorted alphabetically.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; None.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Quick and dirty forcing of all environment list boxes to be sorted.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defmethod update-handler ((handler list-box-handler) arg)
  (let ((model (aif (handler-model handler) it (current-model))))
    (cond ((and (use-model handler) (valid-model model))
           
           (with-model-eval model
             (multiple-value-bind (result success)
                 (safe-update-evaluation handler arg)
               (when success
                 (setf (update-value handler) (if (and result (listp result))
                                                  (string-downcase
                                                   (format nil "~{~S ~}"
                                                     (sort result #'string<
                                                           :key (lambda (x)
                                                                  (if (stringp x)
                                                                      x
                                                                    (symbol-name x))))))
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
                                                (string-downcase
                                                 (format nil "~{~S ~}"
                                                   (sort result #'string<
                                                         :key (lambda (x)
                                                                (if (stringp x)
                                                                    x
                                                                  (symbol-name x))))))
                                              "EMPTY_ENV_STRING"))
               (send-update handler)
               result))))))



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
