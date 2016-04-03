;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2013 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : time-functions.lisp
;;; Version     : 1.0
;;; 
;;; Description : Macro to execute code and record the time spent in specified
;;;             : functions.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2013.10.31 Dan [1.0]
;;;            : * Initial creation.
;;; 2013.11.12 Dan
;;;            : * Added (provide "TIME-FUNCTIONS") because it's now in the
;;;            :   ACT-R support directory.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled
;;;             :   in the docs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Execute code and record the time spent in specified functions as measured by 
;;; get-internal-real-time.
;;;
;;; When being used with ACT-R, code that needs it can include this:
;;; (require-compiled "TIME-FUNCTIONS" "ACT-R-support:time-functions.lisp")
;;; to make sure it gets loaded.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; time-functions
;;;
;;; Takes a list of function names which may end with a keyword parameter
;;; :save-params that specifies whether or not to record the parameters along
;;; with the times for the recorded functions (the default is nil) and any number 
;;; of forms to evaluate. 
;;; It returns two values.  The first is an alist of the function times recorded
;;; where the function name is the car of an entry and the cdr is either a list
;;; of times if :save-params is nil or a list of lists where each sublist has 
;;; the time as the car and the parameters which were passed to the function as
;;; the cdr.  The second value returned is the result of evaluating the forms
;;; provided.
;;; Examples found in comment at end of file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Only records the total time for a recursive function i.e. the time from 
;;; the start of the first call until it returns.
;;; 
;;; The functions which are being timed are not "thread safe".  So this won't 
;;; work to time functions which are being run in multiple threads simultaneously.
;;; In particular, if one is using my parallel-* functions this can't be used to
;;; time the functions being run, but it could record the parallel-* function 
;;; itself i.e. this would be fine:
;;;
;;; (time-functions (parallel-mapcar)
;;;      (parallel-mapcar team 'compute-vals data1)
;;;      (parallel-mapcar team 'compute-vals data2)
;;;      ...)
;;;
;;; but this would not and may result in errors:
;;;
;;; (time-functions (compute-vals)
;;;      (parallel-mapcar team 'compute-vals data1)
;;;      (parallel-mapcar team 'compute-vals data2)
;;;      ...)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro create-timed-closure (f c save results)
  (let ((start (gensym)))
    `(let ((,start nil))
       (lambda (&rest rest)
         (if ,start
             (apply ,c rest)
           (unwind-protect
               (progn
                 (setf ,start (get-internal-real-time))
                 (apply ,c rest))
             (progn
               (if ,save
                   (push (cons (- (get-internal-real-time) ,start) rest) (cdr (assoc ,f ,results)))
                 (push (- (get-internal-real-time) ,start) (cdr (assoc ,f ,results))))
               (setf ,start nil))))))))

(defmacro time-functions ((&rest fns) &body body)
  (let ((results (gensym))
        (old-defs (gensym))
        (fn (gensym))
        (f (gensym))
        (c (gensym))
        (save nil))
    (when (and (>= (length fns) 2) (eq :save-params (nth (- (length fns) 2) fns)))
      (setf save (nth (1- (length fns)) fns))
      (setf fns (butlast fns 2)))
    `(let ((,results nil)
           (,old-defs (mapcan (lambda (x) (when (fboundp x) (list (cons x (fdefinition x))))) ',fns)))
       (unwind-protect 
           (progn
             (dolist (,fn ,old-defs)
               (let ((,f (car ,fn))
                     (,c (cdr ,fn)))
                 (push (cons ,f nil) ,results)
                 (setf (fdefinition ,f)
                   (create-timed-closure ,f ,c ,save ,results))
                 (compile ,f)))
             (values ,results (progn ,@body)))
         (dolist (,fn ,old-defs)
           (setf (fdefinition (car ,fn)) (cdr ,fn)))))))

(provide "TIME-FUNCTIONS")

#| Example:

CG-USER(31): (defun recursive-sleep (n)
               (unless (< n 1)
                (sleep 1)
                (recursive-sleep (1- n))))
RECURSIVE-SLEEP
CG-USER(32): (defun special-sleep (n)
               (recursive-sleep n)
               n)
SPECIAL-SLEEP
CG-USER(33): (time-functions (special-sleep recursive-sleep :save-params t)
                             (special-sleep 3)
                             (special-sleep 2))
((RECURSIVE-SLEEP (2000 2) (3000 3)) (SPECIAL-SLEEP (2000 2) (3000 3)))
2
CG-USER(34): (time-functions (special-sleep recursive-sleep)
                             (special-sleep 2)
                             (special-sleep 1))
((RECURSIVE-SLEEP 1000 2000) (SPECIAL-SLEEP 1000 2000))
1

|#

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
