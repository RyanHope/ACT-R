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
;;; Filename    : parallel-chunks.lisp
;;; Version     : 1.0a2
;;; 
;;; Description : Use the parallel-computation functions to perform the find-
;;;             : matching-chunk operations, declarative retrieval actions
;;;             : (finding matches and computing activation), and blending
;;;             : computations (if the blending module is available) in parallel.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] More testing.
;;; 
;;; ----- History -----
;;; 2013.10.30 Dan
;;;             : * Initial creation.
;;; 2013.11.14 Dan
;;;             : * Added (provide "PARALLEL-CHUNKS") so this can be put into
;;;             :   support for use as needed.
;;; 2014.06.16 Dan [1.0a2]
;;;             : * Updated with the ACT-R 6.1 functions for find and retrieval.
;;; 2014.07.07 Dan 
;;;             : * Updated the blending code for 6.1.
;;; 2015.06.05 Dan
;;;             : * Schedule events in ms and compute-activation-latency is now
;;;             :   returned in ms.
;;; 2015.06.08 Dan
;;;             : * Record the saved trace at the time in ms not seconds.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Very speculative replacement functions for performing some potentially costly 
;;; operations in parallel.  
;;;
;;; To load this the best option is to put it into the ACT-R support directory
;;; and then either call this or put it into a file which will be using the
;;; provided capabilities:
;;;
;;; (require-compiled "PARALLEL-CHUNKS" "ACT-R-support:parallel-chunks.lisp")
;;;
;;; 
;;; When enabled, this will use multiple processes when searching for matching
;;; chunks with find-matching-chunks (used by the vision module for isa visual-
;;; location requests, the declarative module for retrieval requests, and the
;;; blending module for blending requests), when performing the activation 
;;; computation for chunks which match a retrieval request, and when performing
;;; the activation and blending computations in a blending request.
;;;
;;; This is not likely to improve performance for most users.
;;; 
;;; Typically the model needs to have large sets of chunks to be searched over,
;;; have a large number of chunks for which the activation needs to be computed,
;;; or have complex activation computations for the chunks to see any benefit
;;; from this to overcome the overhead of running in parallel.  For the performance 
;;; testing models it looks like somewhere around 50 chunks in the set to be 
;;; tested was the point where performance improvements started to show up, but
;;; if the model uses blending the benefits may show up even for small sets of chunks
;;; being blended if there are multiple slots receiving a blended value in the 
;;; majority of the blending requests.
;;;
;;; None of the tutorial models seem to show a benefit.  
;;;
;;; Parallel operation should not be used if the activation or blending traces are
;;; enabled or being saved.
;;;
;;; If the model has set any of the hook functions used in computing activations 
;;; then those functions must be thread safe otherwise errors or problems will 
;;; likely occur.  Many of the ACT-R commands are not thread safe, so if one uses
;;; any of those in a hook function it may not work correctly.  One command in 
;;; particular, no-output, is very bad to use in any threaded code since it may 
;;; result in the loss of all the model's command output (it is used by many 
;;; ACT-R commands internally which leads to them not being thread safe).
;;; 
;;; Because the parallel operations will not always be executed in the same order as
;;; a sequentially run model the determinism provided by setting the seed parameter
;;; doesn't exist if it's run in parallel.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; To use this, a call to run ACT-R should be wrapped in with-n-workers that
;;; indicates how many processes to use when performing the parallel operations.
;;;
;;; (with-n-workers 3
;;;   (run-experiment ...))
;;;
;;; The number of processes to use should probably be less than the number of 
;;; processors in the computer being used since there will always be other processes
;;; running both within Lisp (gc, GUI interaction, repl, etc) as well as outside Lisp
;;; (OS management, virus checkers, etc) and if the parallel ACT-R processes need
;;; to wait for each other to get processor time it's likely going to decrease 
;;; performance.
;;;
;;; In the testing I've done often it turns out that more is not better.  Using only 
;;; 3 workers seemed to provide the best performance in many situations, but that
;;; varies based on many factors including the model, the Lisp, and the computer.
;;; So, one will have to test their particular situation to find the best value
;;; to use.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Given the overhead I've noticed in testing, it generally doesn't attempt to
;;; do things in parallel if there are fewer items to process than workers but
;;; that is not optimal.  In some situations fewer workers outperform more
;;; workers.  So there're definitely tradeoffs, particularly when things aren't
;;; always "large".
;;; 
;;; Putting some sort of performance monitoring code in place so that it could
;;; adjust things to be parallel or not "on the fly" might be useful, but the 
;;; overhead on doing something like that doesn't seem worthwhile at this point.
;;; 
;;; Originally considered doing conflict-resolution in parallel as well, but
;;; there are problems with that because of how the search buffers are handled.
;;; Ignoring that, some testing with models that don't use search buffers 
;;; seemed to indicate that there need to be a lot of productions to see any
;;; benefit (a real model with 230 productions was slower with a parallel version
;;; of conflict-resolution and the dummy performance testing models needed ~500
;;; to start to show a benefit).  The :use-tree optimization shows a much better
;;; performace improvement for conflict-resolution so there doesn't seem to be a
;;; need for trying to make a parallel version of conflict-resolution at this
;;; point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(require-compiled "PARALLEL" "ACT-R-support:parallel-computation.lisp")

(defvar *actr-team* nil)
(defvar *use-actr-team* nil)

(defmacro with-n-workers (n &body body)
  `(progn
     (unless (and (p_team-p *actr-team*) (= ,n (p_team-size *actr-team*)))
       
       (when (p_team-p *actr-team*)
         (release-parallel-team *actr-team*))
       
       (setf *actr-team* (create-parallel-team "ACT-R-TEAM" ,n)))
     (let ((*use-actr-team* ,n))
       ,@body)))

(defun find-matching-chunks (chunk-spec 
                             &key 
                             (chunks :all) (=test 'chunk-slot-equal) 
                             (-test 'chunk-slot-not-equal)
                             (>test 'safe>) (>=test 'safe>=) 
                             (<test 'safe<) (<=test 'safe<=)
                             (variable-char #\=))
  
  (verify-current-mp  
   "Find-matching-chunks called with no current meta-process."
   (verify-current-model
    "Find-matching-chunks called with no current model."
    
    (let ((found nil))
      (cond ((not (act-r-chunk-spec-p chunk-spec))
             (print-warning "~s is not a valid chunk-spec in call to find-matching-chunks." chunk-spec))
            ((eq :all chunks)
             (if *use-actr-team* ;; assume that all chunks is always big enough to benefit
                 (parallel-collect *actr-team* (lambda (x)
                                                 (when (match-chunk-spec-p x chunk-spec 
                                                                           :=test =test :-test -test
                                                                           :>test >test :>=test >=test 
                                                                           :<test <test :<=test <=test
                                                                           :variable-char variable-char)
                                                   (list x)))
                                   (hash-table-keys (act-r-model-chunks-table (current-model-struct))))
               (progn
                 (maphash (lambda (name chunk)
                            (declare (ignore chunk))
                            (when (match-chunk-spec-p name chunk-spec 
                                                      :=test =test :-test -test
                                                      :>test >test :>=test >=test 
                                                      :<test <test :<=test <=test
                                                      :variable-char variable-char)
                              (push name found)))
                          (act-r-model-chunks-table (current-model-struct)))
                 found)))
            ((listp chunks)
             (if (and *use-actr-team* (<= *use-actr-team* (length chunks)))
                 (parallel-collect *actr-team* (lambda (x)
                                                 (when (match-chunk-spec-p x chunk-spec 
                                                                           :=test =test :-test -test
                                                                           :>test >test :>=test >=test 
                                                                           :<test <test :<=test <=test
                                                                           :variable-char variable-char)
                                                   (list x)))
                                   chunks)
               (dolist (name chunks found)
                 (when (match-chunk-spec-p name chunk-spec 
                                           :=test =test :-test -test
                                           :>test >test :>=test >=test 
                                           :<test <test :<=test <=test
                                           :variable-char variable-char)
                   (push name found)))))
            (t (print-warning "~S is not a valid value for the :chunks keyword parameter to find-matching-chunks." chunks)))))))


(defun retrieval-helper (dm request set)
  (let ((b nil)
        (bv nil))
    (dolist (x set)
      (compute-activation dm x request)
      
      (let ((a (setf (chunk-retrieval-activation x) (chunk-activation x))))
        
        (setf (chunk-retrieval-time x) (mp-time-ms))
        
        (cond ((null bv)
               (setf bv a)
               (push x b))
              ((= a bv)
               (push x b))
              ((> a bv)
               (setf bv a)
               (setf b (list x))))))
    (list (cons bv b))))

(defun start-retrieval (dm request)
  
  (when (dm-stuff-event dm)
    (delete-event (dm-stuff-event dm))
    (setf (dm-stuff-event dm) nil))
  
  (dolist (x (dm-retrieval-request-hook dm))
    (funcall x request))
  
  (when (dm-sact dm)
    (setf (dm-current-trace dm) (make-sact-trace :esc (dm-esc dm)))
    (setf (gethash (mp-time-ms) (dm-trace-table dm)) (dm-current-trace dm)))
  
  (let* ((filled (chunk-spec-filled-slots request))
         (empty (chunk-spec-empty-slots request))
         (chunk-list (if *use-actr-team*
                         (parallel-mapcan *actr-team* (lambda (x) 
                                                        (if (slots-vector-match-signature (car x) filled empty)
                                                             (copy-list (cdr x))
                                                          nil))
                                          (dm-chunks dm))
                       (mapcan (lambda (x) 
                                 (if (slots-vector-match-signature (car x) filled empty)
                                     (copy-list (cdr x))
                                   nil))
                         (dm-chunks dm))))
         (temp-mp nil))
    
    (when (member :recently-retrieved (chunk-spec-slots request))
      (let ((recent (chunk-spec-slot-spec request :recently-retrieved)))
        (cond ((> (length recent) 1)
               (print-warning "Invalid retrieval request.")
               (print-warning ":recently-retrieved parameter used more than once.")
               (setf (dm-busy dm) nil)
               (setf (last-request-invalid (dm-last-request dm)) :too-many)
               (return-from start-retrieval))
              ((not (or (eq '- (caar recent)) (eq '= (caar recent))))
               (print-warning "Invalid retrieval request.")
               (print-warning ":recently-retrieved parameter's modifier can only be = or -.")
               (setf (dm-busy dm) nil)
               (setf (last-request-invalid (dm-last-request dm)) :bad-modifier)
               (return-from start-retrieval))
              ((not (or (eq t (third (car recent)))
                        (eq nil (third (car recent)))
                        (and (eq 'reset (third (car recent)))
                             (eq '= (caar recent)))))
               (print-warning "Invalid retrieval request.")
               (print-warning ":recently-retrieved parameter's value can only be t, nil, or reset.")
               (setf (dm-busy dm) nil)
               (setf (last-request-invalid (dm-last-request dm)) :bad-value)
               (return-from start-retrieval))
              
              (t ;; it's a valid request
               
               ;; remove any old finsts 
               
               (remove-old-dm-finsts dm)
               
               (if (eq 'reset (third (car recent)))
                   (setf (dm-finsts dm) nil)
                 
                 (cond ((or (and (eq t (third (car recent)))   ;; = request t
                                 (eq (caar recent) '=)) 
                            (and (null (third (car recent)))   ;; - request nil
                                 (eq (caar recent) '-)))
                        
                        ;; only those chunks marked are available
                        
                        (setf chunk-list (intersection (mapcar 'car (dm-finsts dm)) chunk-list))
                        
                        ;; save that info for whynot
                        (setf (last-request-finst (dm-last-request dm)) :marked)
                        (setf (last-request-finst-chunks (dm-last-request dm)) chunk-list)
                        
                        (when (dm-sact dm)
                          (setf (sact-trace-only-recent (dm-current-trace dm)) t)
                          (setf (sact-trace-recents (dm-current-trace dm)) chunk-list))
                        
                        (when (dm-act-level (dm-act dm) 'high)
                          (model-output "Only recently retrieved chunks: ~s" chunk-list)))
                       (t
                        ;; simply remove the marked items
                        ;; may be "faster" to do this later
                        ;; once the set is trimed elsewise, but
                        ;; for now keep things simple
                        
                        (when (dm-sact dm)
                          (setf (sact-trace-remove-recent (dm-current-trace dm)) t))
                        
                        (when (dm-act-level (dm-act dm) 'high)
                          (model-output "Removing recently retrieved chunks:"))
                        
                        (setf (last-request-finst (dm-last-request dm)) :unmarked)
                        
                        
                        (setf chunk-list 
                          (remove-if (lambda (x)
                                         (when (member x (dm-finsts dm) :key 'car :test 'eq-chunks-fct)
                                           
                                           (when (dm-sact dm)
                                             (push-last x (sact-trace-recents (dm-current-trace dm))))
                                           
                                           (when (dm-act-level (dm-act dm) 'high)
                                             (model-output "~s" x))
                                           
                                           (push x (last-request-finst-chunks (dm-last-request dm)))
                                           t))
                                     chunk-list)))))))))
    
    (unwind-protect ;; want to make sure dm-mp gets set back even if there's an error and
                    ;; don't want to use with-parameters since changing :mp triggers a warning.
        (progn
          (when (member :mp-value (chunk-spec-slots request))
            (let ((mp-value (chunk-spec-slot-spec request :mp-value)))
              (cond ((> (length mp-value) 1)
                     (print-warning "Invalid retrieval request.")
                     (print-warning ":mp-value parameter used more than once.")
                     (setf (dm-busy dm) nil)
                     (setf (last-request-invalid (dm-last-request dm)) :mp-multi)
                     (return-from start-retrieval))
                    ((not (eq '= (caar mp-value)))
                     (print-warning "Invalid retrieval request.")
                     (print-warning ":mp-value parameter's modifier can only be =.")
                     (setf (dm-busy dm) nil)
                     (setf (last-request-invalid (dm-last-request dm)) :mp-modifier)
                     (return-from start-retrieval))
                    ((not (numornil (third (car mp-value))))
                     (print-warning "Invalid retrieval request.")
                     (print-warning ":mp-value parameter's value can only be nil or a number.")
                     (setf (dm-busy dm) nil)
                     (setf (last-request-invalid (dm-last-request dm)) :mp-not-num)
                     (return-from start-retrieval))
                    
                    (t ;; it's a valid request
                     (setf temp-mp (list (dm-mp dm)))
                     (setf (dm-mp dm) (third (car mp-value)))))))
          
          (let ((best-val nil)
                (best nil)
                (return-val nil)
                (chunk-set 
                 (cond ((or (null (dm-esc dm)) (null (dm-mp dm))) ;; exact matches only
                        ;; do them individually for tracing purposes
                        (if *use-actr-team* 
                            (find-matching-chunks request :chunks chunk-list)
                          (let ((found nil))
                            (dolist (name chunk-list found)
                              (if (match-chunk-spec-p name request)
                                  (progn
                                    (when (dm-sact dm)
                                      (push-last name (sact-trace-matches (dm-current-trace dm))))
                                    
                                    (when (dm-act-level (dm-act dm) 'medium)
                                      (model-output "Chunk ~s matches" name)) 
                                    (push-last name found))
                                (progn
                                  (when (dm-sact dm)
                                    (push-last name (sact-trace-no-matches (dm-current-trace dm))))
                                  
                                  (when (dm-act-level (dm-act dm) 'high)
                                    (model-output "Chunk ~s does not match" name))))))))
                       (t ;; partial matching
                        ;; everything that fits the general pattern:
                        ;; filled and empty slots (already handled)
                        ;; also test the inequalities >, <, >=, and <= 
                        
                        (let* ((extra-spec (mapcan (lambda (x)
                                                     (unless (or (eq (car x) '=) (eq (car x) '-) (keywordp (second x)))
                                                       (list x)))
                                             (chunk-spec-slot-spec request)))
                               (matches (if extra-spec
                                            (find-matching-chunks (define-chunk-spec-fct extra-spec) :chunks chunk-list)
                                          ;; reverse it to keep the ordering the same
                                          ;; relative to the older version and so that
                                          ;; things are consistent with different requests
                                          (nreverse chunk-list)))
                               (non-matches (when (or (dm-act dm) (dm-sact dm))
                                              (set-difference chunk-list matches))))
                          
                          (when (dm-act-level (dm-act dm) 'high)
                            (dolist (c non-matches)
                              (model-output "Chunk ~s does not match" c)))
                          
                          (when (dm-sact dm)
                            (setf (sact-trace-matches (dm-current-trace dm)) matches)
                            (setf (sact-trace-no-matches (dm-current-trace dm)) non-matches))
                          
                          matches)))))
            
            (setf (last-request-matches (dm-last-request dm)) chunk-set)
            
            (if (dm-esc dm)
                (if (and *use-actr-team* (<= *use-actr-team* (length chunk-set)))
                    (let ((bests (parallel-funcall *actr-team*
                                                   (lambda (x) (retrieval-helper dm request x))
                                                   chunk-set)))
                      (setf best-val (apply 'max (mapcar 'car bests)))
                      (setf best (mapcan (lambda (x) (if (= (car x) best-val) (cdr x) nil)) bests))
                      )
                  (dolist (x chunk-set)
                    (compute-activation dm x request)
                    
                    (setf (chunk-retrieval-activation x) (chunk-activation x))
                    (setf (chunk-retrieval-time x) (mp-time-ms))
                    
                    (cond ((null best-val)
                           (setf best-val (chunk-activation x))
                           (push x best)
                           (when (dm-act-level (dm-act dm) 'medium)
                             (model-output "Chunk ~s has the current best activation ~f" x best-val)))
                          ((= (chunk-activation x) best-val)
                           (push x best)
                           (when (dm-act-level (dm-act dm) 'medium)
                             (model-output "Chunk ~s matches the current best activation ~f" x best-val)))
                          ((> (chunk-activation x) best-val)
                           (setf best-val (chunk-activation x))
                           (setf best (list x))
                           (when (dm-act-level (dm-act dm) 'medium)
                             (model-output "Chunk ~s is now the current best with activation ~f" x best-val))))))
              (setf best chunk-set))
            
            (when (> (length best) 1)
              (if (dm-er dm)
                  (let ((b (random-item best)))
                    (setf best (cons b (remove b best))))
                (setf best (sort best #|(copy-list best) why was I copying this?|# 'string<))))
            
            (setf (last-request-best (dm-last-request dm)) best)
                        
            (when (car (dm-retrieval-set-hook dm))
              (let ((chunk-set-with-best (when best (cons (car best) (remove (car best) chunk-set)))))
                (dolist (x (dm-retrieval-set-hook dm))
                  (let ((val (funcall x chunk-set-with-best)))
                    (when val
                      (if return-val
                          (progn 
                            (print-warning "multiple set-hook functions returned a value - none used")
                            (setf return-val :error))
                        (setf return-val val)))))))
            
            (cond ((consp return-val)
                   (setf (dm-busy dm) (schedule-event-relative (cdr return-val) 'retrieved-chunk
                                                               :module 'declarative 
                                                               :destination 'declarative 
                                                               :params (list (car return-val))
                                                               :details (concatenate 'string
                                                                          (symbol-name 'retrieved-chunk)
                                                                          " "
                                                                          (symbol-name (car return-val)))
                                                               :output 'medium))
                   
                   (when (dm-sact dm)
                     (setf (sact-trace-result-type (dm-current-trace dm)) :force)
                     (setf (sact-trace-result (dm-current-trace dm)) (car return-val)))
                   
                   (when (dm-act-level (dm-act dm) 'low)
                     (model-output "Retrieval-set-hook function forced retrieval of" (car return-val))))
                  
                  ((numberp return-val)
                   (setf (dm-busy dm) (schedule-event-relative return-val 'retrieval-failure
                                                               :module 'declarative 
                                                               :destination 'declarative 
                                                               :output 'low))
                   
                   (when (dm-sact dm)
                     (setf (sact-trace-result-type (dm-current-trace dm)) :force-fail))
                   
                   (when (dm-act-level (dm-act dm) 'low)
                     (model-output "Retrieval-set-hook function forced retrieval failure")))
                  
                  ((or (null best) 
                       (and (dm-esc dm)
                            (< best-val (dm-rt dm))))
                   (setf (dm-busy dm) (schedule-event-relative (if (dm-esc dm)
                                                                   (compute-activation-latency dm (dm-rt dm))
                                                                 0)
                                                               'retrieval-failure
                                                               :time-in-ms t :module 'declarative 
                                                               :destination 'declarative
                                                               :output 'low))
                   
                   (when (dm-sact dm)
                     (setf (sact-trace-result-type (dm-current-trace dm)) :fail)
                     (setf (sact-trace-result (dm-current-trace dm)) (when best (dm-rt dm))))
                   
                   (when (and (dm-act-level (dm-act dm) 'low) (null best))
                     (model-output "No matching chunk found retrieval failure"))
                   
                   (when (and (dm-act-level (dm-act dm) 'low) best)
                     (model-output "No chunk above the retrieval threshold: ~f" (dm-rt dm))))
                  
                  ((= (length best) 1)
                   (setf (dm-busy dm) (schedule-event-relative (if (dm-esc dm)
                                                                   (compute-activation-latency dm (chunk-activation (car best)))
                                                                 0)
                                                               'retrieved-chunk
                                                               :time-in-ms t
                                                               :module 'declarative 
                                                               :destination 'declarative 
                                                               :params best
                                                               :details 
                                                               (concatenate 'string
                                                                 (symbol-name 'retrieved-chunk)
                                                                 " "
                                                                 (symbol-name (car best)))
                                                               :output 'medium))
                   
                   (when (dm-sact dm)
                     (setf (sact-trace-result-type (dm-current-trace dm)) :single)
                     (setf (sact-trace-result (dm-current-trace dm)) (cons (car best) (chunk-activation (car best)))))
                   
                   (when (dm-act-level (dm-act dm) 'low)
                     (model-output "Chunk ~s with activation ~f is the best" (car best) (chunk-activation (car best)))))
                  (t
                   (let ((best1 (car best)))
                     
                     (setf (dm-busy dm) (schedule-event-relative (if (dm-esc dm)
                                                                     (compute-activation-latency dm (chunk-activation best1))
                                                                   0)
                                                                 'retrieved-chunk
                                                                 :time-in-ms t
                                                                 :module 'declarative 
                                                                 :destination 'declarative 
                                                                 :params (list best1)
                                                                 :details 
                                                                 (concatenate 'string
                                                                   (symbol-name 'retrieved-chunk)
                                                                   " "
                                                                   (symbol-name best1))
                                                                 :output 'medium))
                     (when (dm-sact dm)
                       (setf (sact-trace-result-type (dm-current-trace dm)) :multi)
                       (setf (sact-trace-result (dm-current-trace dm)) (cons best1 (chunk-activation best1))))
                     
                     (when (dm-act-level (dm-act dm) 'low)
                       (model-output "Chunk ~s chosen among the chunks with activation ~f" best1 (chunk-activation best1))))))))
      (when temp-mp
        (setf (dm-mp dm) (car temp-mp)))))
  
  (when (dm-sact dm)
    (setf (dm-current-trace dm) nil)))




(defun blend-slot (dm instance activation-list dont-generalize slot)
  
  (let* ((possible-values (mapcan (lambda (x) 
                                    (awhen (fast-chunk-slot-value-fct (third x) slot)
                                           (list (make-blending-item :name (third x) :value it :prob (second x)
                                                                     :mag (funcall (blending-module-v->m instance) it)))))
                            activation-list))
         (slot-vals (mapcar 'blending-item-value possible-values))
         (mags (mapcar 'blending-item-mag possible-values))
         (true-mags (remove nil mags)))
    
    (cond ((every 'null mags) ;; they're all nil
           (cons slot nil))
          
          ((every 'numberp true-mags)
           
           (let ((sum 0))
             (dolist (mag possible-values)
               (awhen (blending-item-mag mag)
                      (let ((increment (* it (blending-item-prob mag))))
                        (incf sum increment))))
             
             (cond ((and (blending-module-m->v instance)
                         (not (equalp slot-vals mags)))
                    (let ((result (funcall (blending-module-m->v instance) sum request)))
                      (cons slot result)))
                   (t 
                    (cons slot sum)))))
                  
          (t
           (let ((which (if (every 'chunk-p-fct true-mags)
                            (if (not (find slot dont-generalize))
                                :chunks :not-generalized)
                          :other)))
             (let* ((type (when (eq which :chunks) (common-chunk-type true-mags)))
                    (chunks (if (eq which :chunks)
                                (if (zerop type) 
                                    (all-dm-chunks dm)
                                  (mapcan (lambda (x) 
                                            (if (slots-vector-match-signature (car x) type)
                                                (copy-list (cdr x))
                                              nil))
                                    (dm-chunks dm)))
                              (remove-duplicates true-mags)))
                    (best-val nil)
                    (best-mag nil))
                       
               (dolist (val chunks)
                 (let ((sum 0.0))
                   (dolist (possible possible-values)
                     (let ((sim (chunks-similarity dm val (blending-item-mag possible))))
                       (incf sum (* (blending-item-prob possible) (expt sim 2)))))
                             
                   (when (or (null best-mag)
                             (< sum best-mag))
                     (setf best-mag sum)
                     (setf best-val val))))
                       
               (cond ((and (blending-module-m->v instance)
                           (not (equalp slot-vals mags)))
                      (let ((result (funcall (blending-module-m->v instance) best-val request)))
                        (cons slot result)))
                     (t 
                      (cons slot best-val)))))))))
         
        
(defun start-blending (instance request)
    
  (let ((ignore nil) 
        (dont-generalize nil)
        (sblt nil))
    
    (when (blending-module-sblt instance)
      (setf sblt (make-sblt-trace))
      (setf (gethash (mp-time) (blending-module-trace-table instance)) sblt))
    
    (when (member :ignore-slots (chunk-spec-slots request))
      
      (let ((ignore-slots (chunk-spec-slot-spec request :ignore-slots)))
        (cond ((> (length ignore-slots) 1)
               (print-warning "Invalid blending request.")
               (print-warning ":ignore-slots parameter used more than once.")
               (return-from start-blending))
              ((not (listp (spec-slot-value (first ignore-slots))))
               (print-warning "Invalid blending request.")
               (print-warning ":ignore-slots parameter's value must be a list.")
               (return-from start-blending))
              (t 
               (setf ignore (spec-slot-value (first ignore-slots)))))))
    
    (when (member :do-not-generalize (chunk-spec-slots request))
      
      (let ((ignore-slots (chunk-spec-slot-spec request :do-not-generalize)))
        (cond ((> (length ignore-slots) 1)
               (print-warning "Invalid blending request.")
               (print-warning ":do-not-generalize parameter used more than once.")
               (return-from start-blending))
              ((not (listp (spec-slot-value (first ignore-slots))))
               (print-warning "Invalid blending request.")
               (print-warning ":do-not-generalize parameter's value must be a list.")
               (return-from start-blending))
              (t 
               (setf dont-generalize (spec-slot-value (first ignore-slots)))))))
    
    (let* ((dm (get-module declarative))                   ;; get that module since we're using some of its functions
           (request-details (chunk-spec-slot-spec request))
           (fixed-values (remove-if (lambda (x) (or (keywordp (spec-slot-name x)) (not (eq (spec-slot-op x) '=)))) request-details))
           (fixed-slots (mapcar 'spec-slot-name fixed-values))
           
           ;; perform the chunk matching just like the declarative module does
           (filled (chunk-spec-filled-slots request))
           (empty (chunk-spec-empty-slots request))
           (chunk-list (remove-if 'chunk-blending-suppressed
                                   (mapcan (lambda (x) 
                                             (if (slots-vector-match-signature (car x) filled empty)
                                                 (copy-list (cdr x))
                                               nil))
                                     (dm-chunks dm))))
           
           (matching-chunks (cond ((or (null (blending-module-esc instance)) 
                                       (null (blending-module-mp instance)))
                                   ;; Perfect matching 
                                   (find-matching-chunks request :chunks chunk-list))
                                  (t
                                   ;; everything that fits the general pattern:
                                   ;; filled and empty already done
                                   ;; so just test the inequalities
                                   
                                   (let ((extra-spec (mapcan (lambda (x)
                                                               (unless (or (eq (spec-slot-op x) '=) 
                                                                           (eq (spec-slot-op x) '-) 
                                                                           (keywordp (spec-slot-name x)))
                                                                 (list x)))
                                                       request-details)))
                                     (if extra-spec
                                         (find-matching-chunks (define-chunk-spec-fct extra-spec) :chunks chunk-list)
                                       chunk-list)))))
           
           (all-slots (slot-mask->names (reduce 'logior matching-chunks :key 'chunk-slots-vector)))
           
           (blended-slots (remove-if (lambda (x) 
                                       (find x ignore))
                                     (if (blending-module-blend-all instance)
                                         all-slots
                                       (set-difference all-slots fixed-slots))))
           
           (temperature (aif (blending-module-tmp instance) 
                             it
                             (if (null (blending-module-ans instance))
                                 (progn
                                   (print-warning "Blending requires :tmp or :ans to be set - assuming default of 1.")
                                   1.0)
                               (* (sqrt 2) (blending-module-ans instance)))))
           
           ;; Have the declarative module compute the activations and record them here
            (activation-list (if (and *use-actr-team* (<= *use-actr-team* (length matching-chunks)))
                                
                                (parallel-mapcan *actr-team* 
                                                 (lambda (chunk) 
                                                   (compute-activation dm chunk request)   ;; compute the activation
                                                   (setf (chunk-blended-activation chunk) (chunk-activation chunk))
                                                   (setf (chunk-blended-time chunk) (mp-time))
                                                   (if (and (blending-module-min-bl instance) (< (chunk-last-base-level chunk) (blending-module-min-bl instance)))
                                                       (progn (setf (chunk-blending-suppressed chunk) t)
                                                         nil)
                                                     (list (list (chunk-activation chunk) 
                                                                 (handler-case (exp (/ (chunk-activation chunk) temperature))
                                                                   (floating-point-underflow () 0)
                                                                   (floating-point-overflow () 
                                                                                            ;; can't print the warning since it's in parallel
                                                                                            ;; just use something big and assume it's available for now...
                                                                                            (exp 50)))
                                                                 chunk))))
                                                 matching-chunks)
                              (mapcan (lambda (chunk) 
                                        (compute-activation dm chunk request)   ;; compute the activation
                                        (setf (chunk-blended-activation chunk) (chunk-activation chunk))
                                        (setf (chunk-blended-time chunk) (mp-time))
                                        (if (and (blending-module-min-bl instance) (< (chunk-last-base-level chunk) (blending-module-min-bl instance)))
                                            (progn (setf (chunk-blending-suppressed chunk) t)
                                              nil)
                                          (list (list (chunk-activation chunk) 
                                                      (handler-case (exp (/ (chunk-activation chunk) temperature))
                                                        (floating-point-underflow () 0)
                                                        (floating-point-overflow () 
                                                                                 (print-warning "Math overflow during blending chunk activation for ~s is ~s" chunk (chunk-activation chunk)) 
                                                                                 (print-warning "Results of blending are not likely to be meaningful.")
                                                                                 ;; just use something big and assume it's available for now...
                                                                                 (exp 50)))
                                                      chunk))))
                                matching-chunks))))
      
      ;(format t "chunk-list: ~S matching-chunks: ~s activation-list: ~s~%" chunk-list matching-chunks activation-list)
      
      (when (and (blending-module-blend-all instance) (null (blending-module-mp instance)))
        (print-warning "The :blend-all-slots parameter is set to t, but the :mp parameter is nil which means only perfect matches can occur."))
      
      (when (blending-module-sblt instance)
        (setf (sblt-trace-chunk-type sblt) (cons filled empty)))
      
      (when (blending-module-trace instance)
        (model-output "Blending request for chunks ~@[with slots ~a~] ~@[without slots ~a~]" 
                      (slot-mask->names filled)
                      (slot-mask->names empty)))
      
      
      (awhen (blending-module-request-hooks instance)
             (dolist (x it)
               (funcall x request)))
      
      (awhen (blending-module-set-hooks instance)
             (dolist (x it)
               (funcall x matching-chunks)))

      
      (when (null activation-list) ;; a complete failure
        
        (when (blending-module-sblt instance)
          (setf (sblt-trace-no-matches sblt) t))
        
        (when (blending-module-trace instance)
          (model-output "No matching chunks found.")
          (model-output "Blending request fails."))
        
        ;; schedule the failure event to happen and record that as the busy flag
        ;; failure time same as for declarative - based on the retrieval threshold
        
        (setf (blending-module-busy instance) 
          (schedule-event-relative (compute-activation-latency dm (blending-module-rt instance))
                                   'blending-failure :time-in-ms t :module 'blending
                                   :destination 'blending :output 'low))
        
        (awhen (blending-module-result-hooks instance)
               (dolist (x it)
                 (funcall x nil)))
        
        (return-from start-blending nil))
      
      (when (blending-module-sblt instance)
        (setf (sblt-trace-tmp sblt) (blending-module-tmp instance))
        (setf (sblt-trace-temperature sblt) temperature))
      
      (when (blending-module-trace instance)
        (if (blending-module-tmp instance)
            (model-output "Blending temperature is: ~f" temperature)
          (model-output "Blending temperature defaults to (* (sqrt 2) :ans): ~f" temperature)))
      
      (let ((sum (reduce '+ (mapcar 'second activation-list)))
            (new-chunk nil)
            (blended-results (mapcar (lambda (x) (cons x nil)) blended-slots))
            (sblt-slot nil))
        
        (if (and *use-actr-team* (<= *use-actr-team* (length activation-list)))
            (parallel-run *actr-team* (lambda (x) (setf (second x) (/ (second x) sum))) activation-list)
          (mapc (lambda (x) (setf (second x) (/ (second x) sum))) activation-list))
       
        (when (blending-module-sblt instance)
          (setf (sblt-trace-activation-list sblt) activation-list)
          (setf (sblt-trace-blended-slots sblt) blended-slots)
          (setf (sblt-trace-ignore sblt) ignore))
        
        (when (blending-module-trace instance)
          (dolist (x activation-list)
            (model-output "Chunk ~S matches blending request~%  Activation ~f~%  Probability of recall ~f~%"
                          (third x) (first x) (second x)))
          (model-output "~%Slots to be blended: ~S" blended-slots)
          (when ignore 
            (model-output "Slots being explicitly ignored: ~S~%" ignore)))
        
        (if (and *use-actr-team* (second blended-slots)) ;; if there's more than one always do in parallel
            (setf blended-results
              (parallel-run *actr-team* (lambda (x) (blend-slot dm instance activation-list dont-generalize x)) blended-slots))
          
          (dolist (slot blended-slots)
            
            (when (blending-module-sblt instance)
              (setf sblt-slot (make-sblt-slot :name slot))
              (push-last sblt-slot (sblt-trace-slot-list sblt)))
            
            (when (blending-module-trace instance)
              (model-output "Finding blended value for slot: ~s" slot))
            
            (let* ((possible-values (mapcan (lambda (x) 
                                              (awhen (fast-chunk-slot-value-fct (third x) slot)
                                                     (list (make-blending-item :name (third x) :value it :prob (second x)
                                                                               :mag (funcall (blending-module-v->m instance) it)))))
                                      activation-list))
                   (slot-vals (mapcar 'blending-item-value possible-values))
                   (mags (mapcar 'blending-item-mag possible-values))
                   (true-mags (remove nil mags)))
              
              (when (blending-module-sblt instance)
                (setf (sblt-slot-slot-vals sblt-slot) slot-vals)
                (setf (sblt-slot-magnitudes sblt-slot) possible-values))
              
              (when (blending-module-trace instance)
                (model-output "Matched chunks' slots contain: ~S" slot-vals)
                (model-output "Magnitude values for those items: ~S" mags))
              
              (cond ((every 'null mags) ;; they're all nil
                     
                     (when (blending-module-sblt instance)
                       (setf (sblt-slot-condition sblt-slot) :null))
                     
                     (when (blending-module-trace instance)
                       (model-output "When all magnitudes are nil there's nothing to blend and the slot is ignored"))
                     
                     (setf blended-results (remove slot blended-results :key 'car)))
                    
                    ((every 'numberp true-mags)
                     
                     (when (blending-module-sblt instance)
                       (setf (sblt-slot-condition sblt-slot) :numbers))
                     
                     (let ((sum 0))
                       (when (blending-module-trace instance)
                         (model-output "With numeric magnitudes blending by weighted average"))
                       
                       (dolist (mag possible-values)
                         (awhen (blending-item-mag mag)
                                (let ((increment (* it (blending-item-prob mag))))
                                  (incf sum increment)
                                  (when (blending-module-trace instance)
                                    (model-output " Chunk ~s with probability ~f times magnitude ~f = ~f cumulative result: ~f" 
                                                  (blending-item-name mag) (blending-item-prob mag) it increment sum)))))
                       
                       (cond ((and (blending-module-m->v instance)
                                   (not (equalp slot-vals mags)))
                              (let ((result (funcall (blending-module-m->v instance) sum request)))
                                (setf (cdr (assoc slot blended-results)) result)
                                
                                (when (blending-module-sblt instance)
                                  (setf (sblt-slot-mag-adjusted sblt-slot) t)
                                  (setf (sblt-slot-adjusted-val sblt-slot) result))
                                
                                (when (blending-module-trace instance)
                                  (model-output " Final result: ~f  Converted to value: ~s" sum result))))
                             (t 
                              (setf (cdr (assoc slot blended-results)) sum)
                              (when (blending-module-trace instance)
                                (model-output " Final result: ~f" sum))))))
                    
                    (t
                     
                     (let ((which (if (every 'chunk-p-fct true-mags)
                                      (if (not (find slot dont-generalize))
                                          :chunks :not-generalized)
                                    :other)))
                       
                       (when (blending-module-sblt instance)
                         (setf (sblt-slot-condition sblt-slot) which))
                       
                       (when (blending-module-trace instance)
                         (case which
                           (:chunks
                            (model-output "When all magnitudes are chunks blending based on similarities to all related chunks"))
                           (:not-generalized
                            (model-output "When all magnitudes are chunks and the slot is not generalized blending based on similarities to only those chunks"))
                           (:other
                            (model-output "When not all magnitudes are numbers or chunks blending based on similarities using those values"))))
                       
                       (let* ((type (when (eq which :chunks) (common-chunk-type true-mags)))
                              (chunks (if (eq which :chunks)
                                          (if (zerop type) 
                                              (all-dm-chunks dm)
                                            (mapcan (lambda (x) 
                                                      (if (slots-vector-match-signature (car x) type)
                                                          (copy-list (cdr x))
                                                        nil))
                                              (dm-chunks dm)))
                                        (remove-duplicates true-mags))))
                         
                         (when (blending-module-sblt instance)
                           (setf (sblt-slot-ctype sblt-slot) type)
                           (setf (sblt-slot-chunks sblt-slot) chunks))
                         
                         (when (and (eq which :chunks) (blending-module-trace instance))
                           (if (not (zerop type))
                               (model-output "Intersection of slots for values is: ~s" (slot-mask->names type))
                             (model-output "No intersecting slots found all chunks will be tested")))
                         
                         (let ((best-val nil)
                               (best-mag nil))
                           
                           (dolist (val chunks)
                             
                             (when (blending-module-trace instance)
                               (model-output " Comparing value ~S" val))
                             
                             (let ((sum 0.0))
                               
                               (dolist (possible possible-values)
                                 
                                 (let ((sim (chunks-similarity dm val (blending-item-mag possible))))
                                   (when (blending-module-sblt instance)
                                     (push-last (cons val (list possible sim))
                                                (sblt-slot-possible-values sblt-slot)))
                                   
                                   (incf sum (* (blending-item-prob possible) (expt sim 2)))
                                   
                                   (when (blending-module-trace instance)
                                     (model-output "  Chunk ~s with probability ~f slot value ~s~@[ converted to magnitude ~s~] similarity: ~f cumulative result: ~f" 
                                                   (blending-item-name possible) (blending-item-prob possible)
                                                   (blending-item-value possible) 
                                                   (if (equalp (blending-item-value possible) (blending-item-mag possible))
                                                       nil
                                                     (blending-item-mag possible))
                                                   sim sum))))
                               
                               (when (or (null best-mag)
                                         (< sum best-mag))
                                 (setf best-mag sum)
                                 (setf best-val val))))
                           
                           (cond ((and (blending-module-m->v instance)
                                       (not (equalp slot-vals mags)))
                                  (let ((result (funcall (blending-module-m->v instance) best-val request)))
                                    (setf (cdr (assoc slot blended-results)) result)
                                    
                                    (when (blending-module-sblt instance)
                                      (setf (sblt-slot-mag-adjusted sblt-slot) t)
                                      (setf (sblt-slot-adjusted-val sblt-slot) result))
                                    
                                    (when (blending-module-trace instance)
                                      (model-output " Final result: ~f  Converted to value: ~s" best-val result))))
                                 (t 
                                  (setf (cdr (assoc slot blended-results)) best-val)
                                  (when (blending-module-trace instance)
                                    (model-output " Final result: ~f" best-val))))))))))))
        
        ;; put the fixed values into the chunk def.
        
        (unless (blending-module-blend-all instance)
          (dolist (slot fixed-values)
            (push (spec-slot-name slot) new-chunk)
            (push (spec-slot-value slot) new-chunk)))
        
        ;; put the blended values into the chunk def.
        
        (dolist (slot blended-results)
          (push (car slot) new-chunk)
          (push (cdr slot) new-chunk))
        
        (setf new-chunk (nreverse new-chunk))
        
        (when (blending-module-sblt instance)
          (setf (sblt-trace-new-chunk sblt) new-chunk))
        
        (when (blending-module-trace instance)
          (model-output "This is the definition of the blended chunk:~%~s" new-chunk)
          (model-output "~%Computing activation and latency for the blended chunk"))
        
        (let ((act 0))
          
          (dolist (c activation-list)
            (incf act (exp (first c)))
            
            (when (blending-module-trace instance)
              (model-output " Activation of chunk ~S is ~f" (third c) (first c))))
          
          (setf act (log act))
          
          (when (blending-module-trace instance)
            (model-output "Activation for blended chunk is: ~f" act))
          
          (when (blending-module-sblt instance)
            (setf (sblt-trace-act sblt) act))
          
          (cond ((>= act (blending-module-rt instance))
                 (setf (blending-module-busy instance) 
                   (schedule-event-relative 
                    (compute-activation-latency dm act)
                    'blending-complete
                    :time-in-ms t
                    :module 'blending
                    :destination 'blending
                    :params (list new-chunk)
                    :details (symbol-name 'blending-complete)
                    :output 'medium)))
                (t 
                 (when (blending-module-trace instance)
                   (model-output "Not above threshold so blending failed"))
                 
                 (when (blending-module-sblt instance)
                   (setf (sblt-trace-fail sblt) t))
                 
                 (setf (blending-module-busy instance) 
                   (schedule-event-relative 
                    (compute-activation-latency dm (blending-module-rt instance))
                    'blending-failure
                    :time-in-ms t
                    :module 'blending
                    :destination 'blending
                    :details (symbol-name 'blending-failure)
                    :output 'medium)))))))))


(provide "PARALLEL-CHUNKS")

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
