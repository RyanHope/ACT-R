;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2015 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : productions.lisp
;;; Version     : 1.0
;;; 
;;; Description : The structure and extension macros for productions and their
;;;             : parameters.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2015.09.11 Dan [1.0]
;;;             : * Separating this from the procedural module code so that I
;;;             :   can add parameters there and avoid some warning issues.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
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



(defstruct act-r-production-parameter
  "The internal structure of a production parameter"
  name default-value default-function accessor)


;; The internal rep. of a production is not considered public

(defstruct (production (:predicate production?))
  text name documentation 
  variables bindings conditions actions
  lhs rhs lhs-buffers rhs-buffers
  conflict-code
  break
  disabled
  buffer-indices
  standard-rep
  dynamic
  conflict-val
  (parameter-values (make-hash-table :size 23))
  constants binds others selection-code implicit
    
  failure-condition
  
  partial-matched-slots
  
  searches search-binds search-others
)


(defvar *production-parameters-list* nil)


;;; Instead of pre-specifying the paramters for a production
;;; move it to a system like chunks that allows for extending
;;; things "on the fly" so that alternative utility equations
;;; can be tried out without having to muck around with the
;;; base definitions.  All changes can then be confined to the
;;; utility-and-reward file.


(defmacro extend-productions (parameter-name &key (default-value nil)(default-function nil))
  "Add new parameters to all productions"
  (if (or (not (symbolp parameter-name)) (keywordp parameter-name))
      (print-warning "~s is not a valid symbol for specifying a production parameter." parameter-name)
    (let ((accessor-name (intern (concatenate 'string "PRODUCTION-" (string-upcase parameter-name))))
          (setf-name (intern (concatenate 'string "PRODUCTION-" (string-upcase parameter-name) "-SETF"))))
      
      (if (find parameter-name *production-parameters-list* :key 'act-r-production-parameter-name)
          (progn
            (print-warning "Parameter ~s already defined for productions" parameter-name)
            :duplicate-parameter)
        (progn ;; can't do the function test because of potential issues with things being available at compile time
          (if nil ;(not 'fctornil default-function)
              (print-warning "Invalid value ~s specified for the default-function to extend productions" default-function)
            
            `(eval-when (:compile-toplevel :load-toplevel :execute) 
               (unless *suppress-extend-item-warning*
                 (when (fboundp ',accessor-name)
                   (print-warning "Function ~s already exists and is being redefined." ',accessor-name))
                 (when (fboundp ',setf-name)
                   (print-warning "Function ~s already exists and is being redefined." ',setf-name)))
               
               (when (find ',parameter-name *production-parameters-list* :key 'act-r-production-parameter-name)
                 (setf *production-parameters-list* (remove ',parameter-name *production-parameters-list* :key 'act-r-production-parameter-name)))
               
               (push (make-act-r-production-parameter :name ',parameter-name
                                                      :default-value ',default-value
                                                      :default-function ',default-function
                                                      :accessor ',accessor-name)
                     *production-parameters-list*)
               
               (defun ,accessor-name (production-name)
                 (let ((production (get-production production-name)))
                   (if (production? production) 
                       (multiple-value-bind (value exists)
                           (gethash ',parameter-name (production-parameter-values production))
                         (if exists
                             value
                           (setf (gethash ',parameter-name (production-parameter-values production)) 
                             (production-parameter-default 
                              (find ',parameter-name *production-parameters-list* 
                                    :key 'act-r-production-parameter-name)
                              production))))
                     (print-warning "~S called with invalid production name." ',accessor-name))))
               
               (defun ,setf-name (production-name new-value)
                 (let ((production (get-production production-name)))
                   (if (production? production)
                       (setf (gethash ',parameter-name (production-parameter-values production)) new-value)
                     (print-warning "Setf of ~S called with invalid production." ',accessor-name))))
               
               (defsetf ,accessor-name ,setf-name)
               ',accessor-name)))))))

(defun production-parameter-default (param production)
  "Return a default value for a parameter in a production"
  (if (act-r-production-parameter-default-function param)
      (funcall (act-r-production-parameter-default-function param) production)
    (act-r-production-parameter-default-value param)))



(provide "PRODUCTIONS")

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
