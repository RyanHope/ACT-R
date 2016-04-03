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
;;; Filename    : parallel-computation.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : Provides "mapcar like" functionality which can perform 
;;;               calculations in parallel using multiple threads using the
;;;               ACT-R uni-files functions.  The functions being used for
;;;               mapping must be arity 1.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] More testing, both for performance and reliability.
;;; 
;;; ----- History -----
;;; 2013.10.14 Dan
;;;             : * Initial creation.
;;; 2013.10.29 Dan
;;;             : * Update some of the docs after running some performance tests.
;;;             : * Replaced the separate running functions with a single one that
;;;             :   takes parameters to setup the different situations.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This code depends on components of ACT-R. 
;;; To load this the best option is to put it into the ACT-R support directory
;;; and then use:
;;;
;;; (require-compiled "PARALLEL" "ACT-R-support:parallel-computation.lisp")
;;;
;;; in any file which uses the functions defined here.
;;;
;;; The basic procedure for using this is to create a "team" of workers.  Then, that
;;; team, a function, and list of parameters can be passed to one of the parallel 
;;; running functions.  The parallel function will create a list of results based
;;; on the function and parameters provided by breaking the parameters into sublists
;;; and having the workers of the team process those sublists in parallel.  The return 
;;; values from the parallel function will then be a list of results and an indication 
;;; of whether there was any error.  When the team is no longer needed it should be 
;;; released to end the worker processes that were created.
;;;
;;; An important thing to keep in mind is that the function passed to a parallel
;;; running command must be "thread safe".  The most important aspect of that is
;;; that it can't modify non-local data without appropriate protection.  It also
;;; shouldn't call any of the parallel running code, especially with the same team.
;;; Any input or output operations are also likely going to need to be locked if
;;; the same stream is being used (for example printing to *standard-output*).  
;;;
;;; With respect to ACT-R commands, they should generally be considered not thread safe, 
;;; and only used in something called from a parallel function if one has thoroughly
;;; investigated the underlying operation.  One command which can be particularly
;;; nasty is no-output which is used internally in a lot of places and may result
;;; in all model output being lost if called from parallel running code.  
;;; 
;;; This file redefines the actr-random command to be thread safe for use with 
;;; the parallel functions by providing each worker with its own separate random state.
;;; Note however that if one uses calls to the actr-random command (or any of the 
;;; commands which use it like actr-noise or permute-list) in parallel running 
;;; functions then the determinism of the results will no longer be guaranteed by
;;; setting the :seed parameter since those calls will be drawing from different 
;;; sequences of random results.  Even if they were drawing from the same random 
;;; state, since the order in which the processes would call random is not fixed 
;;; it still wouldn't guarantee deterministic outcomes.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Not really intended for public use, but for those that are interested in
;;; giving it a try these are the functions you'll need.  There is a simple 
;;; example at the end of the file with some tests that show each function in use.
;;;
;;; create-parallel-team
;;; 
;;; defun create-parallel-team (name size)
;;; 
;;;  name must be a string which is used to create the names for the worker processes
;;;   and the locks which are used.
;;;  size must be a positive integer indicating the number of workers to create for
;;;   the team.
;;;  
;;;  If the parameters provided are appropiate it creates and returns a team of workers
;;;  which can be used for running the parallel-* functions.  That involves spawning size
;;;  processes for the workers which will be waiting until there is something for them to
;;;  perform.  Those processes will remain alive until the team is released or killed.  
;;;  If the parameters provided are not appropriate then it prints a warning and returns 
;;;  nil without creating any processes.
;;;  
;;;  When creating a team one should probably create fewer workers than there are
;;;  processors/cores in the machine since there will be the main process and likely 
;;;  other Lisp processes (gc, GUI control, repl, etc) also running.  That way
;;;  workers aren't fighting for processor time, but there may be situations where
;;;  more workers than processors may show an improvement (possibilities would be 
;;;  if memory accessing is a limiting factor or the function runs so fast that
;;;  workers can finish while others are still being started or waiting to be
;;;  collected).
;;;
;;; release-parallel-team
;;; kill-parallel-team
;;;
;;; defun release-parallel-team (team)
;;; defun kill-parallel-team (team)
;;;
;;; team must be a team that was returned from create-parallel-team.
;;;
;;; These functions end the worker threads which were created with the team
;;; provided and mark the team as unusable.  Release-parallel-team should be
;;; called first to have the team cleanly end the processes.  If the worker 
;;; processes for the team have not terminated (they still show as running
;;; processes after the team was released) then kill-parallel-team can be used
;;; to attempt to kill the processes instead.  If release-parallel-team is
;;; passed a team which has not yet been released then it will return t after 
;;; releasing the worker processes, otherwise it will print a warning and return
;;; nil.  If kill-parallel-team is passed a team which has already been released
;;; then it will return t after trying to kill the worker processes, otherwise
;;; it will print a warning and return nil.
;;; 
;;; parallel-mapcar
;;; parallel-run
;;; parallel-mapcan
;;; parallel-collect
;;; parallel-funcall
;;;
;;; defun parallel-mapcar (team function params)
;;; defun parallel-run (team function params)
;;; defun parallel-mapcan (team function params)
;;; defun parallel-collect (team function params)
;;; defun parallel-funcall (team function params)
;;;
;;; team must be a team that was returned from create-parallel-team.
;;; function must be a function or function name with an arity of one.
;;; params must be a list of things which will be passed in some way to
;;;  the function provided.
;;;
;;; The parallel-* functions are used to carry out the execution of the function 
;;; provided with the params given, distributed across the workers of the team.  
;;; How the params are passed to the function and what is returned depends on which 
;;; of the parallel-* functions is called (the differences will be described below).
;;; All of the parallel-* functions return two values.
;;;
;;; If team is a team returned from create-parallel-team which has not yet been
;;; released, function is a function or symbol which names a function, and params
;;; is a list of items then that list of items will be split into m sublists, where
;;; m=min(workers in team, length(params)), of equal length (or as close to equal as
;;; possible).  Each of those sublists will be given to a worker to process with
;;; function.  The results of the workers will be appended together and returned as
;;; the first value from a parallel-* function.
;;;
;;; The second value returned from a parallel-* function indicates whether or not
;;; there was an error either in the parameters provided or the execution of the
;;; workers.  If there is an error then a warning will be printed and the second
;;; value will be t.  If there are no errors then the second value will be nil.
;;; If the second value is t then the first value returned is likely not valid and
;;; should probably be discarded.
;;;
;;; Here are how the different parallel-* functions use the provided function
;;; and collect the results.
;;;
;;; Parallel-mapcar
;;;
;;; Each worker creates a list of results by using (mapcar function worker-params)
;;; where worker-params is the subset of params given to that worker.  Those 
;;; resulting lists are appended together such that the overall order of the resulting
;;; list matches the order of the original params list, and that list is returned
;;; as the first value.  Thus the result is the same as calling (mapcar function params).
;;;
;;; Parallel-run
;;;
;;; This is very similar to parallel-mapcar except that the order of the returned
;;; list may not match the order of the original params.  There will be as many
;;; items in the returned list as there are params, each corresponding to calling
;;; function with one of the items from params, but there are no constraints on
;;; how they are ordered.  This should be slightly faster than parallel-mapcar and may
;;; be useful if order isn't important but time is.
;;;
;;; Parallel-mapcan
;;;
;;; Each worker creates a list of results by using (mapcan function worker-params)
;;; where worker-params is the subset of params given to that worker.  Those 
;;; resulting lists are appended together such that the overall order of the resulting
;;; list matches the order of the original params list, and that list is returned
;;; as the first value.  Thus the result is the same as calling (mapcan function params).
;;;
;;; Parallel-collect
;;; 
;;; This is very similar to parallel-mapcan except that the order of the returned
;;; list may not match the order of the original params.  This should be slightly 
;;; faster than parallel-mapcan and may be useful if order isn't important but time is.
;;;
;;; Parallel-funcall
;;;
;;; Each worker creates a result using (funcall function worker-params), thus
;;; the function will be passed an entire sublist as its only parameter.  The
;;; results of those calls are appened together, in no particular order, to
;;; produce the first result returned.  Because the results are appended the
;;; function provided must always return a list otherwise the appending of the
;;; results may cause an error.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Decided that since modern CPUs have multiple cores in them and most Lisps
;;; support threading that utilizes multiple cores I should try to take
;;; advantage of that.
;;;
;;; Considered some existing libraries which do this sort of thing, but had problems
;;; with them in some Lisps and don't want to deal with issues like that.
;;;
;;; Kept things simple by using locks instead of fancier things that some Lisps
;;; provide so that it works the same in all the Lisps supported by uni-files.
;;;
;;; The workers are only spawned once and locked until needed because the tests
;;; I've run show that unlocking is cheaper than spawning threads.
;;;
;;; It also splits the list upfront so that it's divided across all of the 
;;; workers initially instead of passing off one item at a time to a worker and
;;; then having to unlock the workers multiple times for large lists.  The 
;;; assumption here is that the initial splitting will be less costly than the
;;; multiple locking/unlocking of the workers for larger lists.  This seems
;;; to hold true for the tests I've run.
;;;
;;; It just uses ignore-errors to prevent errors in the functions from breaking
;;; the workers.  If there are any errors it doesn't try to perform that calculation 
;;; again and just returns the condition to the data collector.  That doesn't stop
;;; the other calculations which are ongoing.
;;;
;;; It assumes that the threads will stay alive.  There's no error checking to
;;; make sure the workers are alive so if a worker thread dies for some reason 
;;; then the whole team is basically dead and it will freeze up if it dies 
;;; during a calculation since there's no timeout or run-time tests on the
;;; workers.
;;; 
;;; There's no protection in the workers to ensure the code they're using is
;;; itself thread safe.  That's up to the user to ensure, and things like writing 
;;; output to the REPL are certainly problematic.
;;;
;;; Because it's using locks it's important that all the calls to run the team
;;; originate in the same thread that created the workers initially since that's
;;; the thread which holds the locks.  
;;;
;;; I'm assuming that testing for null isn't a problem even when the item is being 
;;; written by a separate thread to avoid yet another layer of locking, but if
;;; that turns into an issue I'll have to consider other options.  Since only
;;; one thread will be checking a value (either the queue result in the collector
;;; or the done status in a worker) and will loop over the test it's not a 
;;; potential deadlock if it misses a setting of the value, and since the specific
;;; value doesn't matter (only whether it's nil or not) reading a partial value
;;; shouldn't matter either.
;;;
;;; Each worker gets its own random state for the ACT-R random functions since
;;; those are not thread safe and it beats trying to lock protect them.  That
;;; also requires defining a replacement for actr-random since it doesn't take
;;; a state parameter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R-support:uni-files.lisp")

(defstruct p_worker "The parallel worker structure"
  name number process lock action params result error done exit mechanism random-state)

(defstruct p_queue "A queue for workers to indicate completion"
  data lock)

(defvar *act-r-parallel-random-state* nil)

(defun act-r-random (limit)
  (if (and (numberp limit) (plusp limit))
      (let ((state (if *act-r-parallel-random-state*
                       *act-r-parallel-random-state*
                     (let ((module (get-module random-module)))
                       (if module
                           (act-r-random-module-state module)
                         *default-random-module*)))))
        (cond ((integerp limit)
               (if (< limit #xffffffff)
                   (values (floor (* limit (genrand_real2_L state))))
                 (let ((accum 0)
                       (nums (ceiling (/ (1+ (log limit 2)) 32))))
                   (dotimes (i nums)
                     (setf accum (+ (ash accum 32) (genrand_int32 state))))
                   (mod accum limit)
                   )))
              (t
               (* limit (genrand_real2 state)))))
    (print-warning "Act-r-random called with an invalid value ~s" limit)))


(defun p_worker-function (worker queue)
  (let ((*act-r-parallel-random-state* (p_worker-random-state worker)))
    (loop
      ;; Wait until unlocked to start working
      (uni-lock (p_worker-lock worker))
      
      (when (p_worker-exit worker) 
        (uni-unlock (p_worker-lock worker))        
        (return))
      
      (multiple-value-bind (value condition)
          ; simply funcall the appropriate function with the function and
          ; values provided, and detect if there's an error
          (ignore-errors (funcall (p_worker-mechanism  worker) (p_worker-action worker) (p_worker-params worker)))
        
        ; if there's a problem send the condition back
        ; otherwise return the resulting list
        (if (subtypep (type-of condition) 'condition)
            (setf (p_worker-error worker) t
              (p_worker-result worker) condition)
          (setf (p_worker-error worker) nil
            (p_worker-result worker) value)))
      
      ; note that it's not done yet.
      (setf (p_worker-done worker) nil)
      
      ; get in the queue of results ready
      
      (uni-lock (p_queue-lock queue))
      (push-last worker (p_queue-data queue))
      (uni-unlock (p_queue-lock queue))
      
      ; release my lock and wait for the
      ; signal from the team leader 
      (uni-unlock (p_worker-lock worker))
      (while (null (p_worker-done worker))
        (uni-process-system-events)))))
  
(defstruct p_team "A group of workers for parallel tasks"
  name size workers queue busy)

(defun create-parallel-team (name size)
  (if (and (stringp name) (integerp size) (plusp size))
      (let* ((queue (make-p_queue :lock (uni-make-lock name)))
             (team (make-p_team :name name :size size :queue queue :busy nil)))
        
        ;; Create and grab the locks and spawn the worker processes
        (dotimes (i size)
          (let* ((w-name (format nil "~a-~d" name i))
                 (lock (uni-make-lock w-name)))
            (uni-lock lock)
            (let* ((r-state (make-mersenne-twister))
                   (worker (make-p_worker :name w-name :number i :lock lock :random-state r-state)))
              
              ;; Initialize the random state of the worker so they're all unique
              
              (setf (mt-initial-seed r-state)
                (abs (+ (mt-start r-state)
                        (get-internal-real-time))))
              (init-random-state-from-seed r-state (mt-initial-seed r-state))
              
              ;; start the process
              
              (setf (p_worker-process worker) (uni-run-process w-name (lambda ()
                                                                        (p_worker-function worker queue))))
              (push-last worker (p_team-workers team)))))
        
        team)
    (if (stringp name)
        (print-warning "Create-parallel-team requires a positive integer for the size, but given ~s" size)
      (print-warning "Create-parallel-team requires a string for the name, but given ~s" name))))
        
  
(defun run-parallel-team (team function params mechanism order)
  ;; if the values are valid
  (if (and (p_team-p team) (not (zerop (p_team-size team))) (not (p_team-busy team))
           (or (functionp function) (fboundp function)))
      
      ;; if there's any data to split generate the split points array
      (if params
          ;; split the params into segments to pass to the workers
          (let* ((m (min (length params) (p_team-size team)))
                 (start-sizes (floor (length params) m))
                 (splits (make-array (list (1+ m)) :initial-element start-sizes))
                 (extras (rem (length params) (* m start-sizes))))
            
            ;; mark the team as busy
            (setf (p_team-busy team) t)
            
            (dotimes (i extras)
              (incf (aref splits i)))
    
            ;; set the worker functions and params and unlock them as they're set.
            
            (do* ((count 0 (1+ count))
                  (start 0 end)
                  (end (aref splits 0) (+ end (aref splits count)))
                  (workers (p_team-workers team) (cdr workers))
                  (w (car workers) (car workers)))
                 ((= count m))
              (setf (p_worker-mechanism w) mechanism
                (p_worker-action w) function
                (p_worker-params w) (subseq params start end))
              (uni-unlock (p_worker-lock w)))
            
            ;; collect the results saving them in order of worker position
            ;; if needed and report any errors that occurred leaving the corresponding
            ;; result as nil.
    
            (let ((count 0)
                  (results (if order (make-list m :initial-element nil) nil))
                  (error nil)
                  (queue (p_team-queue team)))
      
              (while (< count m)
                ;; wait for someone in the queue
                (while (null (p_queue-data (p_team-queue team)))
                  (uni-process-system-events))
        
                (incf count)
        
                ;; lock the queue and collect the first result
                (uni-lock (p_queue-lock queue))
                (let ((worker (pop (p_queue-data queue))))
                  (uni-unlock (p_queue-lock queue))
                  (if (p_worker-error worker)
                      (progn
                        (setf error t)
                        (uni-report-error 
                         (p_worker-result worker) 
                         (format nil "Error occurred in parallel operation of ~s on values ~s for thread ~a" 
                           function (p_worker-params worker) (p_worker-name worker))))
                    (if order
                        (setf (nth (p_worker-number worker) results) (p_worker-result worker))
                      (push (p_worker-result worker) results)))
          
                  ;; lock the worker again and then indicate it's done
                  (uni-lock (p_worker-lock worker))
                  (setf (p_worker-done worker) t)))
              
              ;; mark the team as not busy
              (setf (p_team-busy team) nil)
                          
              ;; return the appended results list and error state
              (values (apply 'append results) error)))
        ;; if there aren't any params to iterate over the result is nil
        (values nil nil))
    ;; invalid paramters provided so print a warning and return nil,t
    (values (cond ((not (p_team-p team))
                   (print-warning "Must provide a team to run instead of ~s" team))
                  ((zerop (p_team-size team))
                   (print-warning "Team cannot be used because it has been released."))
                  ((p_team-busy team)
                   (print-warning "Team ~s is already busy and cannot run another parallel function." (p_team-name team)))
                  (t 
                   (print-warning "~s is not a function or the name of a function." function)))
            t)))
                  

(defun parallel-mapcar (team function params)
  (run-parallel-team team function params 'mapcar t))

(defun parallel-run (team function params)
  (run-parallel-team team function params 'mapcar nil))

(defun parallel-mapcan (team function params)
  (run-parallel-team team function params 'mapcan t))

(defun parallel-collect (team function params)
  (run-parallel-team team function params 'mapcan nil))

(defun parallel-funcall (team function params)
  (run-parallel-team team function params 'funcall nil))


(defun release-parallel-team (team)
  ;; Set the exit flag and then unlock workers so they terminate by
  ;; returning from the function in the process instead of killing them.
  (if (p_team-p team)
      (if (zerop (p_team-size team))
          (print-warning "Team ~a has already been released." (p_team-name team))
        (progn
          (dolist (x (p_team-workers team))
            (setf (p_worker-exit x) t)
            (ignore-errors (uni-unlock (p_worker-lock x))))
          (setf (p_team-size team) 0)
          t))
    (print-warning "Release-parallel-team requires a team created by create-parallel-team, but was given ~s" team)))


;; If something goes wrong may need to kill the processes.

(defun kill-parallel-team (team)
  (if (p_team-p team)
      (if (zerop (p_team-size team))
          (dolist (x (p_team-workers team) t)
            (ignore-errors (uni-process-kill (p_worker-process x))))
        (print-warning "Must release team ~a before killing it." (p_team-name team)))
    (print-warning "Kill-parallel-team requires a team created by create-parallel-team, but was given ~s" team)))


;;; Some dummy code for testing it with a simple recursive fibonacci function.
;;; The default test list for n workers has 2n items where the items are 2 sets of 
;;; 42-i for i:0-(n-1).  Thus, for n=3 it's (40 41 42 40 41 42).  However, if 
;;; the optional m value is provided when setting up the test the list of params
;;; will be 2m elements long instead to allow for testing a fixed list size vs
;;; different numbers of workers.  Note that this function seems to show some 
;;; benefit to more workers than cores and also tends to show a better improvement
;;; in run time than happens with more "useful" computations.

(defun parallel-computation-fib (x)
  (if (<= x 1)
        x
      (+ (parallel-computation-fib (1- x)) (parallel-computation-fib (- x 2)))))


(defvar *parallel-test-team* nil)
(defvar *parallel-test-data* nil)

(defun setup-parallel-test (n &optional m)
  (setf *parallel-test-team* (create-parallel-team "fib" n)
    *parallel-test-data* nil)
  (dotimes (i 2)
    (dotimes (j (if m m n))
      (push (- 42 j) *parallel-test-data*))))

(defun parallel-test1 ()
  (format t "Not parallel: ~s~%" (mapcar 'parallel-computation-fib *parallel-test-data*)))

(defun parallel-test2 ()
  (format t "Parallel-mapcar: ~s~%" (parallel-mapcar *parallel-test-team* 'parallel-computation-fib *parallel-test-data*)))

(defun parallel-test3 ()
  (format t "Parallel-run: ~s~%" (parallel-run *parallel-test-team* 'parallel-computation-fib *parallel-test-data*)))

(defun parallel-test4 ()
  (format t "Parallel-mapcan: ~s~%" (parallel-mapcan *parallel-test-team* (lambda (x) (list (parallel-computation-fib x))) *parallel-test-data*)))

(defun parallel-test5 ()
  (format t "Parallel-collect: ~s~%" (parallel-collect *parallel-test-team* (lambda (x) (list (parallel-computation-fib x))) *parallel-test-data*)))

(defun parallel-test6 ()
  (format t "Parallel-funcall: ~s~%" (parallel-funcall *parallel-test-team* (lambda (x) (mapcar 'parallel-computation-fib x)) *parallel-test-data*)))

(defun finish-parallel-test ()
  (release-parallel-team *parallel-test-team*))

#| Here's the test process:
   Call parallel-test-setup to create the team of the specified size.

   Compare the time to do the tests.  Those perform a sequential run, parallel-mapcar,
   parallel-run, parallel-mapcan, parallel-collect, and parallel-funcall over the data
   respectively.  They should all result in the same values in the returned lists, but 
   only 1, 2, and 4 are guaranteed to be in the same order.
      
   When done call finish-parallel-test to release the test team.


   Here's a run with ACL 9 w/IDE (the SMP version) under Windows 7 on an i7-2600:

CG-USER(17): (setup-parallel-test 7)
NIL
CG-USER(18): (time (parallel-test1))
Not parallel: (14930352 24157817 39088169 63245986 102334155 165580141 267914296 14930352 24157817 39088169 63245986 102334155 165580141 267914296)
; cpu time (non-gc) 26.691771 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  26.691771 sec user, 0.000000 sec system
; cpu time (thread) 23.961754 sec user, 0.000000 sec system
; real time  24.104000 sec (110.7%)
; space allocation:
;  0 cons cells, 40,616 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(19): (time (parallel-test2))
Parallel-mapcar: (14930352 24157817 39088169 63245986 102334155 165580141 267914296 14930352 24157817 39088169 63245986 102334155 165580141 267914296)
; cpu time (non-gc) 40.825462 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  40.825462 sec user, 0.000000 sec system
; cpu time (thread) 9.141658 sec user, 0.000000 sec system
; real time  9.168000 sec (445.3%)
; space allocation:
;  0 cons cells, 7,880 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(20): (time (parallel-test3))
Parallel-run: (165580141 267914296 267914296 14930352 102334155 165580141 63245986 102334155 39088169 63245986 24157817 39088169 14930352 24157817)
; cpu time (non-gc) 40.061057 sec user, 0.015600 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  40.061057 sec user, 0.015600 sec system
; cpu time (thread) 8.829657 sec user, 0.015600 sec system
; real time  8.969000 sec (446.8%)
; space allocation:
;  0 cons cells, 7,400 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(21): (time (parallel-test4))
Parallel-mapcan: (14930352 24157817 39088169 63245986 102334155 165580141 267914296 14930352 24157817 39088169 63245986 102334155 165580141 267914296)
; cpu time (non-gc) 40.747461 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  40.747461 sec user, 0.000000 sec system
; cpu time (thread) 9.032458 sec user, 0.000000 sec system
; real time  9.180000 sec (443.9%)
; space allocation:
;  0 cons cells, 8,120 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(22): (time (parallel-test5))
Parallel-collect: (165580141 267914296 102334155 165580141 267914296 14930352 63245986 102334155 39088169 63245986 24157817 39088169 14930352 24157817)
; cpu time (non-gc) 40.997063 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  40.997063 sec user, 0.000000 sec system
; cpu time (thread) 9.110458 sec user, 0.000000 sec system
; real time  9.174000 sec (446.9%)
; space allocation:
;  0 cons cells, 7,640 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(23): (time (parallel-test6))
Parallel-funcall: (165580141 267914296 267914296 14930352 102334155 165580141 63245986 102334155 39088169 63245986 24157817 39088169 14930352 24157817)
; cpu time (non-gc) 40.170258 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  40.170258 sec user, 0.000000 sec system
; cpu time (thread) 8.751656 sec user, 0.000000 sec system
; real time  8.904000 sec (451.1%)
; space allocation:
;  0 cons cells, 7,400 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(24): (finish-parallel-test)
T
CG-USER(25): (setup-parallel-test 15 7)
NIL
CG-USER(26): (time (parallel-test2))
Parallel-mapcar: (14930352 24157817 39088169 63245986 102334155 165580141 267914296 14930352 24157817 39088169 63245986 102334155 165580141 267914296)
; cpu time (non-gc) 41.714667 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  41.714667 sec user, 0.000000 sec system
; cpu time (thread) 5.709637 sec user, 0.000000 sec system
; real time  6.977000 sec (597.9%)
; space allocation:
;  0 cons cells, 14,456 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(27): (finish-parallel-test)
T

|#       

(provide "PARALLEL")

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
