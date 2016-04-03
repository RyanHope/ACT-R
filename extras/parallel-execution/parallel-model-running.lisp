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
;;; Filename    : parallel-model-running.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : Provides the ability to run independent models in separate
;;;             : processes within a single Lisp instance.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] More testing, both for performance and reliability.
;;; 
;;; ----- History -----
;;; 2015.06.15 Dan
;;;             : * Initial creation.
;;; 2015.06.17 Dan
;;;             : * Slight tweak to run with non-ide ACL.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;             : * Changed the examples to use the ACT-R logical.
;;; 2015.08.05 Dan
;;;             : * The main ACT-R load file is now called load-act-r.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This code depends on components of ACT-R therefore ACT-R must be loaded
;;; before loading this file.
;;; 
;;; One way to load this is to put it into the ACT-R support directory
;;; and then use:
;;;
;;; (require-compiled "PARALLEL-MODELS" "ACT-R-support:parallel-model-running.lisp")
;;;
;;; in any file which uses the functions defined here.
;;;
;;; The basic procedure for using this is to create a set of independent ACT-R
;;; instances, instantiate the model to be run in each, and then use those to 
;;; run multiple copies of a model in parallel and get the results of each returned
;;; in a list.
;;;
;;; Each of the ACT-R instances is created in its own package and loads the full
;;; ACT-R sources into that package.  Instantiating the model involves compiling
;;; and then loading the model file(s) into each of those separate packages.  There
;;; are some restrictions because of that:
;;;  - The model code should not call reload since there will only be a single
;;;    compiled file which will be specific to one of the packages.
;;;  - The model code should not include any package specific code, or if it 
;;;    does, then it must use the full package specifier for all references to 
;;;    that code and that code must be thread-safe.
;;;  - The model file(s) must include all the code necessary to run it.
;;;  - Any code which writes out data should be sure to use unique file names
;;;    and/or separate output streams i.e. if everything is writing to the
;;;    default *standard-output* it could be very difficult to read, or worse
;;;    could lead to errors if the Lisp's output functions aren't actually
;;;    thread safe with respect to a single stream.
;;;
;;; Since the values returned from the model runs will each be generated in a
;;; different package it is probably best to avoid returning symbols as results
;;; since they will be symbols from that package.
;;;
;;; Because this recompiles the original ACT-R sources you will need to force
;;; them to be recompiled the next time you start a Lisp and load ACT-R since
;;; the existing "fasls" will be compiled for a package which doesn't exist.
;;;
;;; How many instances to create for best performance is going to depend on
;;; a lot of factors.  Perhaps the biggest is memory availability and usage
;;; within the Lisp being used since each instance is a full copy of the ACT-R
;;; system code.  That's likely going to set the upper limit of how many you
;;; can create and actually run.  I can get about a half dozen in ACL and
;;; CCL on a Windows 7 machine with 8GB of RAM before things start to become
;;; unstable.
;;;
;;; There's an example at the bottom of this file showing it being used to run
;;; the zbrodoff model from unit 4 of the tutorial.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; create-parallel-instances (number)
;;;
;;; Number is how many independent ACT-R instances to create and must be a positive
;;; integer.
;;; 
;;; How many to create for best performance is going to require testing by the 
;;; user because it's going to depend on the Lisp and machine involved since
;;; there will be a lot of memory required to create the separate instances and
;;; each will be run in its own process/thread.  
;;;
;;; This function should only be called once to create the instances.
;;;
;;; If it is successful it returns the number specified, otherwise it will
;;; print warnings and return nil.
;;;
;;; instantiate-parallel-models (&rest files)
;;;
;;; Files should be one or more pathname designators with a type of "lisp".
;;;
;;; For each of the parallel instances created with create-parallel-instances
;;; this compiles and then loads each of the files specified in the order
;;; given into that instance.
;;;
;;; If this successfully loads the files into each instance it will return
;;; the number of instances.  If there are any problems warnings will be printed
;;; and nil returned.
;;;
;;; 
;;; destroy-parallel-instances ()
;;;
;;; This function will delete the packages created by create-parallel-instances.
;;; If the packages are deleted then create-parallel-instances would have to be
;;; called again before being able to run parallel models.
;;; 
;;; If all the instances are successfully deleted then the number of instances that
;;; were destroyed is returned.  If there are any problems then warnings will be
;;; printed and nil returned.
;;;
;;;
;;; 
;;; run-parallel-models (function &optional (time-out 60) parameters &rest more-parameters)
;;;
;;; function should be a string which contains the name of a function
;;; to call in each of the instances to run the model.  It does not
;;; have to name a function in the default/current package.
;;;
;;; time-out is a positive number in seconds which indicates how long
;;; the instances should be allowed to run before being terminated 
;;; automatically.
;;;
;;; parameters should be a list of parameters which will be passed to 
;;; the function specified.  The elements of that list will be written
;;; to a string which will then be evaluated in one of the instances.
;;;
;;; more-parameters is zero or more lists like parameters.
;;;
;;; If more-parameters are not specified then each instance which
;;; was created will effectively execute: 
;;;
;;;   (apply (read-from-string function) parameters)
;;; 
;;; with all the symbols in parameters being local to the package in which
;;; it is being run.
;;;
;;; Two values are returned.  The first is a list containing the result 
;;; of that call for each instance which was run.  A result is the return 
;;; value from the call if it completed before the time-out expired, the 
;;; keyword :time-out if the function did not complete before the time-out 
;;; period, or the keyword :error if the instance encountered an error 
;;; while running the function provided.  The second parameter is a list
;;; of the error conditions which occurred with one for each occurrence 
;;; of :error in the results list.
;;;
;;; If more-parameters contains ome or more items and fewer than the 
;;; number of instances which were created then each of the lists
;;; of parameters specified in parameters and more-parameters will
;;; be passed to a separate instance to be run and it will return
;;; two values as described above.
;;; The first return value will be effectively as if this were called:
;;;
;;; (mapcar (lambda (x) (apply function x)) (append (list parameters) more-parameters))
;;; 
;;; with each of the elements being generated in parallel and also
;;; subject to the time-out and error detection as indicated above.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Decided that since modern CPUs have multiple cores in them and most Lisps
;;; support threading that utilizes multiple cores I should try to take
;;; advantage of that for model running as well as general code execution as
;;; is done with the parallel-computation code (I don't recommend trying to
;;; use the two together).
;;;
;;; This is similar to the parallel-computation code, except this instantiates 
;;; multiple ACT-Rs in separate packages to run things (like the code I use to grade
;;; student assignments).
;;;
;;; Right now it's really bare-bones -- create the instance, load the code into
;;; them, run as needed, and kill when done. 
;;;
;;; The processor allocation is up to the Lisp to handle.  So how well it spreads
;;; things across processors/cores is going to require testing with the Lisp and
;;; machine involved, and memory use is probably also a big factor since there are
;;; multiple full instances of ACT-R loaded.
;;;
;;; Just record stuff in globals since I'm only allowing one "set" of instances
;;; to be used at a time.
;;;
;;; Thread safety is up to the user -- it assumes what is being run is safe.
;;; 
;;; To hopefully create different random state for each one the random module
;;; gets initialized with get-internal-real-time + (500 * instance-num * internal-time-units-per-second)
;;; figuring that there won't be 500 seconds between the model instantiations.
;;;
;;; A "better" system would be to spawn a fresh lisp process for each and then
;;; have that run things and send the results back.  I have a system which can
;;; do that, but it's very Lisp and OS specific since it needs to be able to
;;; run system commands to start the other jobs and that doesn't seem reasonable
;;; or general enough to make available with ACT-R, but if you want that
;;; let me know and I can send it to you (right now it works with ACL&CCL in
;;; a Linux environment that's using TORQUE for job scheduling, but could be 
;;; adapted to run in other situations).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R-support:uni-files.lisp")

(defvar *parallel-actr-package-count* 0)
(defvar *existing-parallel-packages* nil)
(defvar *parallel-packages-running* nil)
(defvar *parallel-packages-done* 0)
(defvar *parallel-packages-lock* nil)

(defun make-and-load-actr-package ()
  (let ((name (format nil "ACT-R-~d" (incf *parallel-actr-package-count*))))
    (if (find :allegro-ide *features*)
        (make-package name :use '(:cl-user :cg-user :cg :cl :excl))
      (if (find :allegro *features*)
          (make-package name :use '(:cl-user :cl :excl))
        (if (find :ccl *features*)
            (make-package name :use '(:cl-user :cl :ccl))
          (make-package name :use '(:cl-user :cl)))))
    (let ((cp (package-name *package*)))
      (eval `(in-package ,name))
      (let ((*features* (remove :act-r *features*))
            (*modules* (set-difference *modules* '("ENVIRONMENT-COLORS" "UNI-FILES" "PRODUCTION-PARSING" "GENERAL-PM" "GOAL-STYLE-MODULE" "CENTRAL-PARAMETERS") :test 'string-equal)))
        
        (pushnew :actr-recompile *features*)
        (pushnew :actr-fast *features*)
        
        (load "ACT-R:load-act-r.lisp")
        (eval (read-from-string (format nil
                                    "(defun create-random-module (ignore)
                                        (declare (ignore ignore))
                                        (let ((state (make-mersenne-twister)))
                                          (setf (mt-initial-seed state)
                                            (abs (+ (mt-start state)
                                                    (get-internal-real-time)
                                                    (* 500 internal-time-units-per-second ~d))))
    
                                          (init-random-state-from-seed state (mt-initial-seed state))
                                          (make-act-r-random-module :state state)))" 
                                  (1- *parallel-actr-package-count*))))
        (compile 'create-random-module))
      (eval `(in-package ,cp)))
    name))


(defun create-parallel-instances (number)
  (if *existing-parallel-packages*
      (print-warning "There are already ~d parallel instances available.  None created." (length *existing-parallel-packages*))
    (progn
      (setf *parallel-actr-package-count* 0)
      (dotimes (i number number)
        (push-last (make-and-load-actr-package) *existing-parallel-packages*)))))

(defun instantiate-parallel-models (&rest files)
  (if (not (every 'probe-file files))
      (print-warning "These files were not found when trying to instantiate parallel models: ~{~s~^, ~}"
                     (mapcan (lambda (x) (unless (probe-file x) (list x))) files))
    (let ((cp (package-name *package*))
          (success (length *existing-parallel-packages*)))
      (dolist (package *existing-parallel-packages*)
        (eval `(in-package ,package))
        (let ((*features* (remove :act-r *features*))
              (*modules* (set-difference *modules* '("ENVIRONMENT-COLORS" "UNI-FILES" "PRODUCTION-PARSING" "GENERAL-PM" "GOAL-STYLE-MODULE" "CENTRAL-PARAMETERS") :test 'string-equal)))
          
          (pushnew :actr-recompile *features*)
          (pushnew :actr-fast *features*)
        
          
          (dolist (x files)
            (multiple-value-bind (r err) (ignore-errors (compile-and-load x))
            (declare (ignore r))
            (when (subtypep (type-of err) 'condition)
              (eval `(in-package ,cp))
              (uni-report-error err (format nil "Error in compile and load of file ~s in package ~s~%" x package))
              (return-from instantiate-parallel-models nil))))))
      (eval `(in-package ,cp))
      success)))
  

(defun destroy-parallel-instances ()
  (if *existing-parallel-packages*
      (let ((count (length *existing-parallel-packages*)))
        (dolist (x *existing-parallel-packages*)
          (delete-package x))
        (setf *existing-parallel-packages* nil)
        count)
    (print-warning "There are no parallel instances to destroy.")))


(defun run-parallel-models (function &optional (time-out 60) parameters &rest more-parameters)
  (if *parallel-packages-running*
      (print-warning "Run-parallel-models cannot be run multiple times.  You must wait for the previous run to finish.")
    (cond ((null *existing-parallel-packages*)
           (print-warning "There are no parallel instances available to run models.  First create and instantiate some."))
          ((not (stringp function))
           (print-warning "Function parameter to run-parallel-models must be a string not ~s" function))
          ((not (and (numberp time-out) (plusp time-out)))
           (print-warning "Time-out parameter to run-parallel-models must be a positive number not ~s" time-out))
          ((and more-parameters (>= (length more-parameters) (length *existing-parallel-packages*)))
           (print-warning "Too many sets of parameters passed to run-parallel-models because there are only ~d instances available."
                          (length *existing-parallel-packages*)))
          (t
           (let* ((count (if more-parameters 
                             (1+ (length more-parameters))
                           (length *existing-parallel-packages*)))
                  (status (make-list count :initial-element :time-out))
                  (result (make-list count :initial-element nil))
                  (jobs (make-list count :initial-element nil)))
             
             (unwind-protect
                 (let ((params (if more-parameters
                                   (append (list parameters) more-parameters)
                                 (make-list count :initial-element parameters))))
                   (setf *parallel-packages-running* t)
                   (setf *parallel-packages-done* 0)
                   (setf *parallel-packages-lock* (uni-make-lock "Parallel-models"))
                   (dotimes (i count)
                     (let ((pack (nth i *existing-parallel-packages*))
                           (index i)
                           (p (format nil "~s" (nth i params))))
                       (setf (nth i jobs)
                         (uni-run-process pack
                                          (lambda ()
                                            (eval `(in-package ,pack))
                                            (multiple-value-bind (r e) (ignore-errors (apply (read-from-string function)
                                                                                             (read-from-string p)))
                                              (uni-lock *parallel-packages-lock*)
                                            
                                              (if (subtypep (type-of e) 'condition)
                                                  (setf (nth index status) :error
                                                    (nth index result) e)
                                                (setf (nth index status) :success
                                                  (nth index result) r))
                                              
                                              (setf (nth index jobs) nil)
                                              (incf *parallel-packages-done*)
                                              
                                              (uni-unlock *parallel-packages-lock*)))))))
                   
                   (let ((start (get-internal-real-time)))
                     (while (and (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) time-out)
                                 (< *parallel-packages-done* count))
                       (uni-process-system-events)
                       (sleep .1))))
               
               (unless (= count *parallel-packages-done*)
                 (dolist (x jobs)
                   (when x
                     (uni-process-kill x))))
               (setf *parallel-packages-running* nil))
             (let ((res (make-list count))
                   (errors nil))
               (dotimes (i count)
                 (setf (nth i res) (case (nth i status)
                                     (:time-out :time-out)
                                     (:error (push-last (nth i result) errors) :error)
                                     (:success (nth i result)))))
               (values res errors)))))))


(provide "PARALLEL-MODELS")

#|

Example of using it to run the zbrodoff model from unit 4 of the tutorial.
This was run in the 64bit Windows version of CCL 1.10, and the starting 
point is having loaded the ACT-R sources and this file.

? (create-parallel-instances 5)
<--clipped 5 copies of the ACT-R version output-->
5
? (instantiate-parallel-models "ACT-R:tutorial;unit4;zbrodoff.lisp")
5
? (run-parallel-models "zbrodoff-experiment" 30 (list nil nil))
(((2.2965631 2.805938 3.2856245 2.296563 2.796563 3.2996876 2.3012507 2.795 3.2949998) (64 64 64 64 64 64 64 64 64)) ((2.290313 2.795 3.3028
126 2.2965634 2.7918754 3.2934382 2.2981255 2.798125 3.2965624) (64 64 64 64 64 64 64 64 64)) ((2.2981255 2.78875 3.2934375 2.3012505 2.7840
624 3.296562 2.2950008 2.7965624 3.3028123) (64 64 64 64 64 64 64 64 64)) ((2.2950008 2.7903128 3.28875 2.2965631 2.8012505 3.3028123 2.2981
255 2.8028128 3.2887502) (64 64 64 64 64 64 64 64 64)) ((2.3012507 2.8059378 3.2903125 2.3012507 2.7918751 3.298125 2.290313 2.791875 3.2996
879) (64 64 64 64 64 64 64 64 64)))
NIL
;; Run only 3 instances with the second printing its results
? (run-parallel-models "zbrodoff-experiment" 20 (list nil nil) (list nil t) (list nil nil))

              2 (64)      3 (64)      4 (64)
Block  1  2.297 (64)  2.801 (64)  3.295 (64)
Block  2  2.289 (64)  2.801 (64)  3.315 (64)
Block  3  2.284 (64)  2.792 (64)  3.292 (64)
(((2.2950003 2.7950003 3.2856245 2.2965631 2.8012505 3.2996871 2.3059385 2.7996879 3.2918756) (64 64 64 64 64 64 64 64 64)) ((2.2965631 2.80
12505 3.2949998 2.2887506 2.8012502 3.3153129 2.2840629 2.791875 3.2918746) (64 64 64 64 64 64 64 64 64)) ((2.2981255 2.7996879 3.2965624 2.
296563 2.7887502 3.3012502 2.2950003 2.795 3.3028128) (64 64 64 64 64 64 64 64 64)))
NIL
? (time (run-parallel-models "zbrodoff-experiment" 30 (list nil nil)))
(RUN-PARALLEL-MODELS "zbrodoff-experiment" 30 (LIST NIL NIL))
took  8,503,000 microseconds ( 8.503000 seconds) to run.
      3,192,187 microseconds ( 3.192187 seconds, 37.54%) of which was spent in GC.
During that period, and with 8 available CPU cores,
     26,395,369 microseconds (26.395369 seconds) were spent in user mode
        530,403 microseconds ( 0.530403 seconds) were spent in system mode
 19,011 bytes of memory allocated.
(((2.287188 2.78875 3.30125 2.2981257 2.7950006 3.2965622 2.2981255 2.793438 3.2949998) (64 64 64 64 64 64 64 64 64)) ((2.2934384 2.8012505
3.2981246 2.2981255 2.8106253 3.2981248 2.2950006 2.8012502 3.302812) (64 64 64 64 64 64 64 64 64)) ((2.296563 2.7965627 3.2887497 2.2903132
 2.7981253 3.2871873 2.3059385 2.798125 3.2965622) (64 64 64 64 64 64 64 64 64)) ((2.2950008 2.795 3.295 2.2934382 2.791875 3.2965624 2.2965
631 2.7965627 3.298125) (64 64 64 64 64 64 64 64 64)) ((2.2903132 2.8012502 3.2949996 2.3043756 2.798125 3.3075001 2.2965631 2.7903125 3.301
2497) (64 64 64 64 64 64 64 64 64)))
NIL
? (destroy-parallel-instances)
5
? (compile-and-load "ACT-R:tutorial;unit4;zbrodoff.lisp")
#P"c:/users/db30/desktop/actr7/tutorial/unit4/zbrodoff.wx64fsl"
? (time (let (res) (dotimes (i 5 res) (push (zbrodoff-experiment nil nil) res))))
(LET (RES) (DOTIMES (I 5 RES) (PUSH (ZBRODOFF-EXPERIMENT NIL NIL) RES)))
took 19,480,000 microseconds (19.480000 seconds) to run.
      3,030,178 microseconds ( 3.030178 seconds, 15.56%) of which was spent in GC.
During that period, and with 8 available CPU cores,
     19,188,123 microseconds (19.188124 seconds) were spent in user mode
        280,801 microseconds ( 0.280801 seconds) were spent in system mode
 2,072,631,680 bytes of memory allocated.
(((2.302813 2.799688 3.2965624 2.3075008 2.7996876 3.2965617 2.3043756 2.7981253 3.2918746) (64 64 64 64 64 64 64 64 64)) ((2.293438 2.79812
5 3.30125 2.3090632 2.7950003 3.3012497 2.2809377 2.8075001 3.293437) (64 64 64 64 64 64 64 64 64)) ((2.2918754 2.7996876 3.2949996 2.295000
6 2.791875 3.293437 2.3075004 2.7887502 3.2965624) (64 64 64 64 64 64 64 64 64)) ((2.3043754 2.8090632 3.298125 2.2950006 2.798125 3.3090627
 2.2950003 2.7981253 3.2996874) (64 64 64 64 64 64 64 64 64)) ((2.2981255 2.8059382 3.296562 2.304376 2.7856252 3.2949996 2.2950003 2.791875
4 3.2918744) (64 64 64 64 64 64 64 64 64)))
?
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
