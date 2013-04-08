;;; ACT-R Transfer (Actransfer)
;;;
;;; Copyright 2012 Niels Taatgen
;;;
;;; 
;;; Version 0.45 Bug-fix

(defconstant *actransfer-version* "0.45")
(defconstant *actransfer-date* "17 February 2013")


(defvar *exclude-from-ul*)

(setf *exclude-from-ul* '(retrieve-instruction general-condition-does-not-match store-retrieved-instruction start-action-sequence))

(defun linear-update-utility (module production reward)
  (unless (member production *exclude-from-ul*)  ;;; Added
  (let ((old (production-u production))
        (alpha (utility-alpha module)))
    (setf (production-u production) (+ old (* alpha (- reward old)))))))

;;; Standard production compilation can misbehave in this model, because it relies on declarative
;;; finst to track instructions we have tried. So we only want to compile an instruction if all
;;; conditions are tested.

;;; Hack compile-productions to prevent compilation of retrieve-instruction unless it is with a production that clears operator
;;; Also, we don't want retrieve-instruction as a second rule in a compilation
;;;

(defun condition-nil-action (production)
  (let* ((prod (produce-standard-representation production))
         (action (second prod))
         (goal-action (second (assoc '=goal> action))))
    (or (member '(action wait-all-done) goal-action :test #'equal)
        (member '(action nil) goal-action :test #'equal)
        (member '(condition nil) goal-action :test #'equal)
        (member '(condition act) goal-action :test #'equal)
)))
         
(defun check-condition-action-bridge (p1 p2)
  (let* ((p1-action (second (assoc '=goal> (second (produce-standard-representation (get-production (first p1)))))))
         (p2-action (second (assoc '=goal> (second (produce-standard-representation p2))))))
    (and (member '(condition act) p1-action :test #'equal)
         (not (or (member '(action wait-all-done) p2-action :test #'equal)
                  (member '(action nil) p2-action :test #'equal))))))

(defun check-action-perception (p1 p2)
  (let* ((p1-action (second (assoc '+action> (second (produce-standard-representation p1)))))
         (p2-condition (second (assoc '=perception> (first (produce-standard-representation p2))))))
    (and p1-action p2-condition)))


(defun compile-productions (production)
  (let ((module (get-module production-compilation))
        (p-name (production-name production)))
    
    (when (compilation-module-epl module)
      
      (when (compilation-module-trace module)
        (model-output "Production Compilation process started for ~s" p-name))
      
      (cond  ((equal p-name 'retrieve-instruction)
             (when (compilation-module-trace module)
               (model-output "  Production ~s can never compile as second production" p-name))
             (handle-check-valid-1st-p module p-name production))  ;;; NT: added this condition
 
             ((not (valid-compilation-production p-name production))
             (when (compilation-module-trace module)
               (model-output "  Production ~s is not valid for compilation" p-name))
             (setf (compilation-module-previous module) nil))
            
            ((null (compilation-module-previous module))
             (when (compilation-module-trace module)
               (model-output "  No previous production to compose with."))
             (handle-check-valid-1st-p module p-name production))

            ((check-action-perception (get-production (first (compilation-module-previous module))) production)
              (when (compilation-module-trace module)
               (model-output "  No compilation because +action> cannot be followed by =perception>."))
             (handle-check-valid-1st-p module p-name production))  ;;; NT: added this condition


            ((and (equal (car (compilation-module-previous module)) 'retrieve-instruction)  ;;; Retrieve-instruction is the first
                  ;;; Is there a =goal> condition nil action in the production?
                  (not (condition-nil-action production)))
              (when (compilation-module-trace module)
               (model-output "  No compilation because retrieve-instruction does not combine with productions that don't finalize a condition."))
             (handle-check-valid-1st-p module p-name production))  ;;; NT: added this condition
            
            ((check-condition-action-bridge (compilation-module-previous module) production)
              (when (compilation-module-trace module)
               (model-output "  No compilation because of improper condition-action bridge."))
             (handle-check-valid-1st-p module p-name production))  ;;; NT: added this condition
 

            ((> (- (mp-time) (compilation-module-previous-time module))
                (compilation-module-tt module))
             (when (compilation-module-trace module)
               (model-output "  Cannot compile ~s and ~s because the time between them exceeds the threshold time."
                             (car (compilation-module-previous module))
                             p-name))
             (handle-check-valid-1st-p module p-name production))
            ((null (composeable-productions-p module (get-production (car (compilation-module-previous module))) production))
             (when (compilation-module-trace module)
               (model-output "  Production ~s and ~s cannot be composed." (car (compilation-module-previous module)) p-name))
             (handle-check-valid-1st-p module p-name production))
            (t
             (when (compilation-module-trace module)
               (model-output "  Production ~s and ~s are being composed." (car (compilation-module-previous module)) p-name))
             (compose-productions module production)
             (handle-check-valid-1st-p module p-name production))))))

;;; Automatically copy the name of a new fact chunk created in the imaginal to its id slot
;;;



(defun fill-in-id-slot ()
  (let ((chunk (act-r-buffer-chunk (buffer-instance 'imaginal))))
    (mod-buffer-chunk 'imaginal (list 'id chunk))))
      

(defun imaginal-request-2 (instance buffer-name chunk-spec)
  (if (imaginal-module-busy instance)
    (model-warning "Imaginal request made to the ~S buffer while the imaginal module was busy. New request ignored." buffer-name)
  (progn
    (setf (imaginal-module-busy instance) t)
    (setf (imaginal-module-error instance) nil)
    
    (case buffer-name
      (imaginal
       (let ((delay (if (imaginal-module-randomize instance)
                        (randomize-time (imaginal-module-delay instance))
                      (imaginal-module-delay instance))))
         
         (schedule-event-relative delay
                                  'set-imaginal-free
                                  :module 'imaginal
                                  :output nil
                                  :priority -1020)
         
         (schedule-event-relative delay
                                  'goal-style-request
                                  :params (list 'imaginal chunk-spec)
                                  :destination 'imaginal
                                  :module 'imaginal
                                  :output nil)
;; Added

      (when (eq (chunk-spec-chunk-type chunk-spec) 'fact)
        (schedule-event-relative delay 
                                 'fill-in-id-slot
                                 :module 'imaginal
                                 :output nil
                                 :priority -1010))))
;; End of addition

      (imaginal-action
       (case (chunk-spec-chunk-type chunk-spec)
         (generic-action
          (let ((action-spec (chunk-spec-slot-spec chunk-spec 'action)))
            
            (cond ((null action-spec)
                   (print-warning "An imaginal-action generic-action request requires an action.")
                   (set-imaginal-free))
                  ((> (length action-spec) 1)
                   (print-warning "An imaginal-action generic-action request requires a single action.")
                   (set-imaginal-free))
                  ((not (eq '= (caar action-spec)))
                   (print-warning "An imaginal-action generic-action request requires the = specifier for the action.")
                   (set-imaginal-free))
                  ((not (or (functionp (third (car action-spec))) (fboundp (third (car action-spec)))))
                   (print-warning "An imaginal-action generic-action request requires the action to name a valid function.")
                   (set-imaginal-free))
                  (t
                   (funcall (third (car action-spec)))))))
         (simple-action
          (let ((action-spec (chunk-spec-slot-spec chunk-spec 'action)))
            
            (cond ((null action-spec)
                   (print-warning "An imaginal-action simple-action request requires an action.")
                   (set-imaginal-free))
                  ((> (length action-spec) 1)
                   (print-warning "An imaginal-action simple-action request requires a single action.")
                   (set-imaginal-free))
                  ((not (eq '= (caar action-spec)))
                   (print-warning "An imaginal-action simple-action request requires the = specifier for the action.")
                   (set-imaginal-free))
                  ((not (or (functionp (third (car action-spec))) (fboundp (third (car action-spec)))))
                   (print-warning "An imaginal-action simple-action request requires the action to name a valid function.")
                   (set-imaginal-free))
                  (t
                   (let ((c (funcall (third (car action-spec))))
                         (delay (if (imaginal-module-randomize instance)
                                    (randomize-time (imaginal-module-delay instance))
                                  (imaginal-module-delay instance))))
                     
                     (schedule-clear-buffer 'imaginal 0 :module 'imaginal)
                     (cond ((null c) ;; set module free and error t
                            (schedule-event-relative delay
                                                      'set-imaginal-free
                                                      :module 'imaginal
                                                      :output nil)
                            
                            (schedule-event-relative delay
                                                     'set-imaginal-error
                                                     :module 'imaginal
                                                     :output nil))
                           
                           ((chunk-p-fct c) ;; set module free and error t
                            (schedule-set-buffer-chunk 'imaginal c delay :module 'imaginal :priority -1000)
                            
                            (schedule-event-relative delay 'set-imaginal-free :module 'imaginal :output nil :priority -1001 :maintenance t))
                           (t
                            (model-warning "Invalid result from the action of an imaginal-action simple-action function.")
                            (set-imaginal-free))))))))
                            
                            
         (t (print-warning "Invalid request ~S to the imaginal-action buffer." (chunk-spec-chunk-type chunk-spec)))))))))

;;; Write a function that adds activation to an instruction to the extent that its conditions are satisfied
;;; :spreading-hook? or :activation-offsets --> allows adding additional components to activation.
;;; - Collect the conditions in the list of conditions
;;; - Check whether the condition is satisfied. If it is, add to the spreading.
;;; :spreading-hook it is. It takes 1 parameter, the chunk
;;; - If the chunk-type is not instr, then return 0.
;;; Otherwise, follow the condition chain to collect all conditions in a list.
;;; Once all conditions are collected, check whether they are true
;;; There are three types: nil tests, unequal tests, and equal test.
;;; In order to do this effectively, we should disect the test into its three components
;;; --> make it into a string and parse the string

;;; Spreading activation if conditions are satisfied addition
;;;

(defparameter *condition-spread* 3.0) ;;; extra activation when conditions are satisfied
(defparameter *condition-penalty* 0.0) ;;; activation punishment on a mismatch

(defun char-type (c)
  (cond
   ((digit-char-p c) 'digit)
   ((both-case-p c) 'letter)
   (t 'other)))

(defun parse-one-condition (chunk)
  (let ((s (string chunk))
        (result nil)
        (part ""))
    (loop while (not (equal s "")) do
          (when (and (not (equal part ""))
                     (not (eq (char-type (char part (1- (length part))))
                              (char-type (char s 0)))))
            (push part result)
            (setf part ""))
          (setf part (concatenate 'string part (subseq s 0 1)))
          (setf s (subseq s 1)))
    (push part result)
    (reverse result)))
    
(defun find-value-of (buffer-string slot-number instr-chunk)
   (if (or (equal buffer-string "CONTROL")(equal buffer-string "TASK")(equal buffer-string "PARENT")(equal buffer-string "GWM"))
      (let ((chunk (act-r-buffer-chunk (buffer-instance 'goal))))
        (cond  ((equal buffer-string "CONTROL") (chunk-slot-value-fct chunk 'control))
               ((equal buffer-string "PARENT") (chunk-slot-value-fct chunk 'parent))
               ((equal buffer-string "GWM")(chunk-slot-value-fct chunk 'gwm))
               (t (chunk-slot-value-fct chunk 'task))))
    (let ((chunk 
           (cond
            ((equal buffer-string "CONST") instr-chunk)
            ((equal buffer-string "RT") (act-r-buffer-chunk (buffer-instance 'rharvest)))
            ((equal buffer-string "V") (act-r-buffer-chunk (buffer-instance 'perception)))
             ((equal buffer-string "PS") (act-r-buffer-chunk (buffer-instance 'imaginal)))
            (t (error "Illegal buffer-string ~S~%" buffer-string))))
          (slotname (second (assoc slot-number '(("0" id)("1" slot1)("2" slot2)("3" slot3)("4" slot4)("5" slot5)("6" slot6)) :test #'equal))))
      (when chunk (chunk-slot-value-fct chunk slotname)))
    ))

(defun test-condition (c chunk)
  (let* ((l (parse-one-condition c))
         (b1 (find-value-of (first l)(second l) chunk))
         (b2 (if (equal (fourth l) "NIL") nil 
               (find-value-of (fourth l)(fifth l) chunk)))
         (test (third l)))
    (cond ((equal test "=") (equal b1 b2))
    	  ((and (equal test "<>") (null b2)) b1)
          ((equal test "<>") (and b1 b2 (not (equal b1 b2)))))))


(defun spreading-condition (chunk)
  (if (eq (chunk-chunk-type-fct chunk) 'instr)
      (let ((condition-list nil)
            (next-condition (chunk-slot-value-fct chunk 'condition)))
        (loop while (not (null next-condition)) do
              (push (chunk-slot-value-fct next-condition 'c) condition-list)
              (setf next-condition (chunk-slot-value-fct next-condition 'cnext)))
        (let ((result t))
          (dolist (c condition-list)
            (when (and (not (eq c 'stop))(not (test-condition c chunk))) (setf result nil)))
          (if result *condition-spread* *condition-penalty*)
        ))
    0.0))


;;; Extra utility for productions that complete a whole instruction
;;;
;;; This favors a productions that matches an instruction retrieval, and checks all conditions
;;; and carries out all actions. 
;;; Why did we put this in? Hack to make Stroop work?
;;;

(defun full-instruction-interpreter (production)
  (let* ((prod (produce-standard-representation production))
  	     (condition (first prod))
         (action (second prod))
         (goal-action (second (assoc '=goal> action)))
         (retrieval-condition (second (assoc '=retrieval> condition))))
    (and (member '(isa instr) retrieval-condition :test #'equal)
    (or (member '(action wait-all-done) goal-action :test #'equal)
        (member '(action nil) goal-action :test #'equal)))))
         


(defun extra-utility (production)
  (if (full-instruction-interpreter (get-production production)) 2.0 0.0))


;;; The model uses several extra buffers

;;; It uses a perception and action buffers to simplify communication with the outside world.
;;; The results of perception are places in the percetpion buffer
;;; Actions in the external world are +action> actions that are implemented
;;; by task-specific code.
;;; There may be a pending action if the action has a later effect. 
;;; Currently restricted to one for simplicity

(defstruct pmmodule stuffed busy pending-action change)

(defun create-pmmodule (model-name)
  (declare (ignore model-name))
  (make-pmmodule))

(defun reset-pmmodule (instance)
  (format t "Resetting module pmmodule~%")
  
  (sgp :do-not-harvest perception)
  (sgp :do-not-harvest action)
 (specify-compilation-buffer-type perception imaginal)  ;;; was goal instead of imaginal
 (specify-compilation-buffer-type action imaginal)  ;;; was goal instead of imaginal
 (setf (pmmodule-busy instance) nil)
 (setf (pmmodule-pending-action instance) nil)
 (setf (pmmodule-change instance) nil)
  )


(defun query-pmmodule (instance buffer-name slot value)
;  (format t "Query made of the pm module: ~S ~S ~S~%"  buffer-name slot value)
     (case slot
       (state
        (case value
          (busy
           (pmmodule-busy instance))
          (free
           (not (pmmodule-busy instance)))
          (error
              nil)
          (t
           (print-warning "Unknown state query ~S to pm module" value)
           nil))
        )
       (buffer
        (if (eql value 'stuffed)
            (pmmodule-stuffed instance)
            (print-warning "unknown buffer query ~S to pm module" value)
          ))
       (change
         (eq value (pmmodule-change instance)))  
       
       ))

(defun pending-action-p ()
  (if (pmmodule-pending-action (get-module pmmodule)) t nil))

(defun finalize-action (instance)
  (let* ((chunk-spec (eval `(define-chunk-spec isa fact slot1 ,(first *perception*)
                                               slot2 ,(second *perception*)
                                               slot3 ,(third *perception*)
                                               slot4 ,(fourth *perception*))))
         (chunk-action (eval `(define-chunk-spec isa fact))) )
    (setf (pmmodule-busy instance) nil)
  	(goal-style-request instance 'perception chunk-spec)
        (goal-style-request instance 'action chunk-action)
))


(defun finalize-delayed-action (instance)
  (setf (pmmodule-change instance) t))

(defun schedule-delayed-action (percept latency)
  (setf *perception* '(pending))
  (setf (pmmodule-pending-action (get-module pmmodule)) percept)
  (schedule-event-relative latency #'finalize-delayed-action :params (list (get-module pmmodule)) :output 'medium))

(defun request-pmmodule (instance buffer-name chunk-spec)
     (cond
     	((eq (chunk-spec-chunk-type chunk-spec) 'fact) (goal-style-request instance buffer-name chunk-spec))
        ((eq (chunk-spec-chunk-type chunk-spec) 'update-pmmodule)  ;;; respond to a delayed state change
  ;       (format t "~% *** Setting up pending action ~A~%" (pmmodule-pending-action instance))
         (setf (pmmodule-change instance) nil)
         (setf (pmmodule-busy instance) t)
         (setf *perception*  (pmmodule-pending-action instance))
         (setf (pmmodule-pending-action instance) nil)
         (schedule-event-relative 0 #'finalize-action :params (list instance) :output 'medium ))
      ((eq buffer-name 'perception)(print-warning "Can only request perception with a fact"))
     	((eq (chunk-spec-chunk-type chunk-spec) 'external-action) 
         (let* ((action (if (slot-in-chunk-spec-p chunk-spec 'ac) 
                            (third (first 
                                    (chunk-spec-slot-spec chunk-spec 'ac))) 
                          nil))
                
                (arg1 (if (slot-in-chunk-spec-p chunk-spec 'slot1)
                          (third (first 
                                  (chunk-spec-slot-spec chunk-spec 'slot1)))
                        nil
                        ))
                
                (arg2 (if (slot-in-chunk-spec-p chunk-spec 'slot2)
                          (third (first 
                                  (chunk-spec-slot-spec chunk-spec 'slot2)))
                        nil
                        ))
                
                latency 
                )
           (cond ((eq action 'wait) ;;; Special case for wait
                  (when *verbose* (format t "*** ~8,2F: ACTION: Wait~%" (mp-time)))
                 (if (or (pmmodule-pending-action instance)(pmmodule-change instance)) (setf (pmmodule-busy instance) t)
                    (schedule-event-relative 0 #'finalize-action :params (list instance) :output 'medium ))
                  ;;; If there is a pending action, we set the state to busy so retrieve-instruction will wait for the pending action
                  ;;; If the state has already be changed, we switch state to busy as well, so that the appropriate production can pick up
                  ;;; Otherwise, wait will do schedule the current perception into the state buffer
 ;                 (format t "*** ~%Doing a wait with action ~A and Statechange ~A~%" (pmmodule-pending-action instance)(pmmodule-change instance))
                  (setf latency 0))
                 (t
                  (when *verbose*
                  (cond ((and (null arg1)(null arg2))
                         (format t "*** ~8,2F: ACTION: ~A~%" (mp-time) action))
                        ((null arg2)
                         (format t "*** ~8,2F: ACTION: ~A ~A~%" (mp-time) action arg1))
                        (t
                         (format t "*** ~8,2F: ACTION: ~A ~A ~A~%" (mp-time) action arg1 arg2))))
                  (let ((tmp *perception*))
                    (setf latency (funcall (task-pm-function *task*) action arg1 arg2))
                    (when (and *verbose* (not (equal tmp *perception*))) (format t "*** ~8,2F: PERCEPTION: ~A~%" (mp-time) *perception*)))
                  (setf (pmmodule-busy instance) t)
                  (schedule-event-relative latency #'finalize-action :params (list instance) :output 'medium )))))

)


     )
     	   
;;; Because we build up retrievals one bit at a time, we need a buffer to build up the request,
;;; the rrequest buffer. The result of a retrieval is also used one bit at a time, and therefore
;;; we need a harvest buffer, rharvest

(defstruct rrequest stuffed)

(defun create-rrequest (model-name)
  (declare (ignore model-name))
  (make-rrequest))

(defun reset-rrequest (instance)
  (format t "Resetting module rrequest~%")
  
  (sgp :do-not-harvest rrequest)
 (specify-compilation-buffer-type rrequest goal)
  )


(defun query-rrequest (instance buffer-name slot value)
;  (format t "Query made of the rrequest module: ~S ~S ~S~%"  buffer-name slot value)
     (case slot
       (state
        (case value
          (busy
           nil)
          (free
           t)
          (error
              nil)
          (t
           (print-warning "Unknown state query ~S to rrequest module" value)
           nil))
        )
       (buffer
        (if (eql value 'stuffed)
            (rrequest-stuffed instance)
            (print-warning "unknown buffer query ~S to rrequest module" value)
          ))
       
       ))

(defstruct rharvest stuffed)

(defun create-rharvest (model-name)
  (declare (ignore model-name))
  (make-rharvest))

(defun reset-rharvest (instance)
  (format t "Resetting module rharvest~%")
  
  (sgp :do-not-harvest rharvest)
 (specify-compilation-buffer-type rharvest goal)
  )


(defun query-rharvest (instance buffer-name slot value)
;  (format t "Query made of the rharvest module: ~S ~S ~S~%"  buffer-name slot value)
     (case slot
       (state
        (case value
          (busy
           nil)
          (free
           t)
          (error
              nil)
          (t
           (print-warning "Unknown state query ~S to rharvest module" value)
           nil))
        )
       (buffer
        (if (eql value 'stuffed)
            (rharvest-stuffed instance)
            (print-warning "unknown buffer query ~S to rharvest module" value)
          ))
       
       ))

;;; Code for the imaginalreq buffer

(defstruct imaginalreq stuffed)

(defun create-imaginalreq (model-name)
  (declare (ignore model-name))
  (make-imaginalreq))

(defun reset-imaginalreq (instance)
  (format t "Resetting module imaginalreq~%")
  
  (sgp :do-not-harvest imaginalreq)
 (specify-compilation-buffer-type imaginalreq imaginal)
  )


(defun query-imaginalreq (instance buffer-name slot value)
;  (format t "Query made of the imaginalreq module: ~S ~S ~S~%"  buffer-name slot value)
     (case slot
       (state
        (case value
          (busy
           nil)
          (free
           t)
          (error
              nil)
          (t
           (print-warning "Unknown state query ~S to imaginalreq module" value)
           nil))
        )
       (buffer
        (if (eql value 'stuffed)
            (imaginalreq-stuffed instance)
            (print-warning "unknown buffer query ~S to imaginalreq module" value)
          ))
       
       ))




;;; The following code parses instructions 

(defvar *action-sequences*)
(setf *action-sequences* nil)
(defvar *condition-sequences*)
(setf *condition-sequences* nil)
(defvar *pm-function*)
(defvar *perception*)
(setf *perception* nil)
(defvar *all-tasks*)
(defstruct task name pm-function init-function reward parameters)
(setf *all-tasks* nil)
(defvar *descriptions*)
(defvar *vertices*)
(defvar *edges*)


(defmacro add-instr (&body l)
  `(instr-fct ',l))

#|
(defun find-second (x l)
  (cond
   ((null l) nil)
   ((equal (second (first l)) x) (first (first l)))
   (t (find-second x (rest l)))))
|#

(defun permutep2 (l1 l2)
  (cond ((null l1) t)
        ((member (first l1) l2) (permutep2 (rest l1) l2))
        (t nil)))

(defun permutep (l1 l2)
  (if (= (length l1) (length l2)) (permutep2 l1 l2) nil))

(defun find-second (x l)
  (cond
   ((null l) nil)
   ((permutep (second (first l)) x) (first (first l)))
   (t (find-second x (rest l)))))



(defun parse-action-list (l)
 ; (push (list name l) *action-sequences*)
  (setf l (reverse l))
  (let ((l2 nil)
        (last-name 'astop)
        action)
    (loop while (not (null l)) do
        (push (first l) l2)
        (let ((new-action (find-second l2 *action-sequences*)))
          (if new-action (setf last-name new-action)
            (progn
              (setf action (new-name "AC"))
              (push (list action 'action (first l) 0) *vertices*)
              (when (not (eq last-name 'astop))(push (list action last-name 0) *edges*))
              (eval `(add-dm
                      (,action isa action name ,action a ,(first l) anext ,last-name)))
              (push (list action l2) *action-sequences*)
              (setf last-name action))))
        (setf l (rest l)))
    last-name))



(defun parse-condition-list (l)
 ; (push (list name l) *action-sequences*)
  (setf l (reverse l))
  (let ((l2 nil)
        (last-name 'cstop)
        condition)
    (loop while (not (null l)) do
        (push (first l) l2)
        (let ((new-condition (find-second l2 *condition-sequences*)))
          (if new-condition (setf last-name new-condition)
            (progn
              (setf condition (new-name "CD"))
              (push (list condition 'condition (first l) 0) *vertices*)
              (when (not (eq last-name 'cstop))(push (list condition last-name 0) *edges*))
              (eval `(add-dm
                      (,condition isa condition name ,condition c ,(first l) cnext ,last-name)))
              (push (list condition l2) *condition-sequences*)
              (setf last-name condition))))
        (setf l (rest l)))
    last-name))
    

(defun parse-instruction (task l)
  (let ((in-name (new-name "INSTR"))
        (condition nil) 
        (const1 'null)
        (const2 'null)
        (const3 'null)
        (const4 'null)
        (const5 'null)
        (const6 'null)
        (action nil))
   ; (push (list task task task) *vertices*)
    (loop while (not (null l)) do
          (cond
           ((eq (first l) ':condition)
            (pop l)
            (if (not (setf condition (find-second (first l) *condition-sequences*)))
                (setf condition (parse-condition-list (pop l)))
              (pop l)))
          ((eq (first l) ':action)
            (pop l)
            (if (not (setf action (find-second (first l) *action-sequences*)))
              (setf action (parse-action-list (pop l)))
              (pop l)))
          ((eq (first l) ':const)
           (pop l)
           (when (first (first l)) (setf const1 (first (first l))))
           (when (second (first l)) (setf const2 (second (first l))))
           (when (third (first l)) (setf const3 (third (first l))))
           (when (fourth (first l)) (setf const4 (fourth (first l))))
           (when (fifth (first l)) (setf const5 (fifth (first l))))
           (when (sixth (first l)) (setf const6 (sixth (first l))))
           (pop l))
          ((eq (first l) ':description)
           (pop l)
           (push (list in-name (pop l)) *descriptions*))

          (t (format t "Cannot parse ~A~%" (pop l)))))
    (push (list in-name task in-name 0) *vertices*)
    (push (list in-name condition 0) *edges*)
    (push (list in-name action 0) *edges*)
;;; Extra
    (push (list task in-name 1) *edges*)

    (eval `(add-dm (,in-name isa instr task ,task condition ,condition action ,action slot1 ,const1 slot2 ,const2 slot3 ,const3 slot4 ,const4 slot5 ,const5 slot6 ,const6)))))


(defun find-constants (l transtable)
	(let ((result nil))
		(dolist (x l result)
                  (when (eq x 'x) (push 'x result))
			(when (listp x)
			(when 
				(and (first x)  ;;; not nil
				     (not (member (first x) '(RT1 RT2 RT3 RT4 newWM newWM1 newWM2 newWM3 newWM4 RT PS1 PS2 PS3 PS4 EP4 AC1 AC2 AC3 control0 task0 done))) ;; not one of the reserved words
                                     (not (member (first x) result)) ;;; not already on the list
                                     (not (assoc (first x) transtable))) ;;; not already used as a variable
                          (push (first x) result))
                          (when 
                              (and (third x)  ;;; not nil
                                   (not (member (third x) '(RT1 RT2 RT3 RT4 newWM newWM1 newWM2 newWM3 newWM4 RT PS1 PS2 PS3 PS4 AC1 AC2 AC3 control0 task0 done))) ;; not one of the reserved words
                                   (not (member (third x) result)) ;;; not already on the list
                                   (not (assoc (third x) transtable))) ;;; not already used as a variable
			    (push (third x) result))))
		result))
			    

(defun translate-ca (h consts transtable)
   (if (atom h) h
  (let* ((tt (append (merge-lists consts '(CONST1 CONST2 CONST3 CONST4 CONST5 CONST6)) transtable))
  	     (buffer1 (second (assoc (first h) tt)))
  	     (buffer2 (when (third h) (second (assoc (third h) tt)))))
  	     (when (null buffer1) (setf buffer1 (first h)))
  	     (when (null buffer2) (setf buffer2 (third h)))
  	      	                      (intern (format nil "~A~A~A" buffer1 (second h) buffer2)))))

(defun find-operator (x &optional (pos 0)) 
  (cond 
   ((< (length x) 1) nil)
   ((equal (subseq x 0 1) "=") (list pos '=))
   ((< (length x) 2) nil)
   ((equal (subseq x 0 2) "<>") (list pos '<>))
   ((equal (subseq x 0 2) "->") (list pos '->))
   (t (find-operator (subseq x 1) (1+ pos)))))

(defun parse-single (x)
  "Break up symbol in subparts, if necessary"
  (if (or (listp x) (member x '(= <> ->))) (list x)
    (let* ((stringx (format nil "~A" x))
           (op (find-operator stringx))
           (endpos (when op (+ (first op) (if (eq (second op) '=) 1 2)))))
      (if op
          (let* ((item1 (when (> (first op) 0) (intern (subseq stringx 0 (first op)))))
                 (res (when (not (= (length stringx) endpos))(list (intern (subseq stringx endpos))))))
            (push (second op) res)
            (when item1 (push item1 res))
            res)
        (list x)))))

(defun parse-out-singles (l)
  (cond ((null l) nil)
        (t (append (parse-single (first l))(parse-out-singles (rest l))))))

(defun parse-three (l)
	"Group elements in groups of three, and parse out any retrieval, action and episodic lists"
          (cond 
           ((null l) nil)
            ((eq (first l) 'done) '(done))
            ((and (listp (first l)) (eq (third l) 'AC))
             (append (reverse (merge-lists (first l) '(AC1 AC2 AC3) (second l))) (parse-three (cdddr l))))
            ((and (listp (first l)) (eq (third l) 'newWM))
             (append (reverse (merge-lists (first l) '(newWM1 newWM2 newWM3 newWM4) (second l))) (parse-three (cdddr l))))
            ((and (listp (first l)) (eq (third l) 'RT))
             (append (reverse (merge-lists (first l) '(RT1 RT2 RT3 RT4) (second l))) (parse-three (cdddr l))))		 
            (t (cons (list (first l)(second l)(third l)) (parse-three (cdddr l))))))

(defun add-x-to-list (l)
  (cond ((null l) nil)
        ((eq (first l) 'x) (cons 'x l))
        (t (cons (first l)(add-x-to-list (rest l))))))

(defun parse-instruction-v (task l transtable declarative)
   (let (
        (condition nil)
        (consts nil)
        (action nil)
        (description ""))
    (loop while (not (null l)) do
          (cond
           ((eq (first l) ':condition)
            (pop l)
            (setf condition (parse-three (parse-out-singles (pop l) ))))
          ((eq (first l) ':action)
            (pop l) 
            (setf action (parse-three (parse-out-singles (pop l) ))))
          ((eq (first l) ':description)
           (pop l)
           (setf description (pop l)))
          (t (format t "~%Unknown keyword ~A~%" (first l))
             (pop l))
           ))
	(setf consts (find-constants (append (reverse action) '(x) condition) transtable)) ;;; Changed this! Potentially radical reversal of action & condition
        ;;; Now blow up the x so that the length of the list is exactly 6.
        (when (> (length consts) 7) (format t "~%Too many constants in Instruction~%"))
    (if (> (length consts) 6) (setf consts (remove 'x consts))
      (loop while (< (length consts) 6) do
           (setf consts (add-x-to-list consts))))
    (setf consts (substitute 'null 'x consts))
    (setf condition (mapcar #'(lambda (x) (translate-ca x consts transtable)) condition))
    (setf action  (mapcar #'(lambda (x) (translate-ca x consts transtable)) action))
    (parse-instruction task (list ':condition condition ':action action ':const consts ':description description))))

(defun parse-prod (l transtable)
  (let ((result nil)
        (current-buffer nil)
        (temp nil)
        (neg nil))
    (loop while (not (null l)) do
      (cond ((member (first l) '(=input> =imaginal> =goal> =action> =retrieval> +action> +retrieval>)) (setf current-buffer (pop l)))
            ((eq (first l) '-) (setf neg t)(pop l))
            ((eq (first l) '==>) (setf result temp temp nil) (pop l))
            ((assoc (first l) transtable) ;; no checking yet whether buffer is correct. Also check for the literal version, e.g. RT1
             (push (list neg (pop l) (pop l)) temp)
             (setf neg nil))
            (t (format t "Illegial expression ~A~%" (pop l)))
      
      ))
    (list result temp)))


(defun parse-classic-instruction (task l transtable declarative)
  (let* ((condition nil)
         (action nil)
         (description (pop l))
         (prod (parse-prod l transtable))
         (lhs (reverse (first prod)))
         (rhs (reverse (second prod)))
         
         )
    (format t "Parsing rule ~A~%" description)
    (loop while (not (null lhs)) do
      (let ((x (first lhs)))
        (cond ((null (third x)) ; A nil test of a slot
               (setf condition (append condition (list (second x) (if (null (first x)) '= '<>) nil)))
               (pop lhs))
              ((not (chunk-spec-variable-p (third x)))  ; Matching a constant in a slot
               (setf condition (append condition (list (second x) (if (null (first x)) '= '<>) (third x))))
               (pop lhs))
              ((null (first x)) ;; There is non-negated variable in the slot
               (let ((temp nil))  ;; There is a variable in the slot, so find all matching lhs variables
                 (dolist (y (rest lhs)) (when (eq (third x)(third y)) (push y temp)))
                 (dolist (y (reverse temp)) ;; now add all pairs to the condition. If the list is empty, no conditions are added
                   (setf condition (append condition (list (second x) (if (null (first y)) '= '<>) (second y))))
                   (setf lhs (remove y lhs :test 'equal)))
                 (pop lhs)))
              (t ;;; The only possibility left is a negated variable. We'll whine about this one
               (format t "Illegal test ~A~%" x)
               (pop lhs)))))
    (format t "  Condition: ~A~%" condition)
    (setf lhs (reverse (first prod)))
    (dolist (x rhs)  ;; action are more straightforward
      (cond ((chunk-spec-variable-p (third x))  ;; Variable
             (let ((temp nil)
                   (lhs (reverse (first prod))))
               (loop while (and lhs (null temp)) do
                 (when (and (null (first (first lhs))) (eq (third (first lhs)) (third x)))
                   (setf temp (second (first lhs))))
                 (pop lhs))
               (if temp (setf action (append action (list temp '-> (second x))))
                 (format t "Non-matching rhs ~A~%" x))))
            ((not (null (third x))) (setf action (Append action (list (third x) '-> (second x)))))
            (t (format t "Can't set a slot to nil in ~A~%" x))))
     (format t "  Action   : ~A~%" action)
   (parse-instruction-v task (list ':condition condition ':action action ':description description) transtable declarative)
  ))
    

(defun merge-lists (l1 l2 &optional cnst)
  "Multi-purpose list merger: can insert an constant in each list, and removes ?"
	(cond
		((or (null l1) (null l2)) nil)
		((eq (first l1) '?) (merge-lists (rest l1)(rest l2) cnst))
		(cnst (cons (list (first l1) cnst (first l2)) (merge-lists (rest l1)(rest l2) cnst)))
		(t (cons (list (first l1)(first l2)) (merge-lists (rest l1)(rest l2))))))
		

(defun instr-fct (l)
  (let ((task-name (pop l)) 
         (transtable '((Gtask task0)(Gcontrol control0)(Gtop gwm0)(Gparent parent0)(ACaction AC1)(ACarg1 AC2)(ACarg2 AC3)
                       (RTid RTid)(WMid PSid)
                       ))
  declarative
     (task (make-task))
  	)
  	(setf (task-name task) task-name)
        (push (list task-name task-name task-name 1) *vertices*)
  	(loop while (atom (first l)) do
              (cond
               ((eq (first l) ':input)
                (setf transtable (append (merge-lists (second l) '(V1 V2 V3 V4)) transtable))
                (pop l)(pop l))
               ((member (first l) '(:variables :working-memory))
                (setf transtable (append (merge-lists (second l) '(PS1 PS2 PS3 PS4)) transtable))
                (pop l)(pop l))

               ((eq (first l) ':declarative)
                (setf declarative (second l))
                (dolist (x declarative)
                  (setf transtable (append (merge-lists x '(RT1 RT2 RT3 RT4)) transtable)))
                (pop l)(pop l))
               ((eq (first l) ':reward)
                (pop l)
                (setf (task-reward task) (pop l)))
               ((eq (first l) ':pm-function)
                (pop l)
                (setf (task-pm-function task) (pop l)))
               ((eq (first l) ':init)
                (pop l)
                (setf (task-init-function task) (pop l)))
               ((eq (first l) ':facts) (pop l)
                (eval `(add-dm ,@(pop l))))
               ((eq (first l) ':parameters) (pop l)
                (setf (task-parameters task)(pop l)))
               (t (pop l)(pop l)))) 
  	(push task *all-tasks*)
    (dolist (x l)
      (cond
       ((eq (first x) 'in)
        (parse-instruction task-name (rest x))
       )
       ((eq (first x) 'ins)
        (parse-instruction-v task-name (rest x) transtable declarative)
        )
       ((eq (first x) 'p) 
        (parse-classic-instruction task-name (rest x) transtable declarative))
))))
        
(defun set-task (task-name)
  (let ((task (find task-name *all-tasks* :test #'(lambda (x y)(eq x (task-name y))))))
  	(setf *task* task)
  ;	(eval `(spp action-done :reward ,(task-reward task)))
  	(when (task-parameters task) (dolist (x (task-parameters task)) (eval x)))
  	))
  	
(defun init-task ()
  (clear-buffer 'goal)
  (funcall (task-init-function *task*))

  (when *perception* 
    (schedule-event-relative 0 #'finalize-action :params (list (get-module pmmodule)) :output 'medium ))


;    	(eval `(spp action-done :reward ,(task-reward *task*)))

 ; 	(when (task-parameters *task*) (dolist (x (task-parameters *task*)) (eval x)))
)

(defun issue-reward ()
  (trigger-reward  (task-reward *task*))
)

(defvar *verbose* nil)

(defun trace-output (s)
  (when *verbose*
  (format t "~A~%" s)))


(defun trace-retrieved-instructions (chunk)

(when (and *verbose* chunk (eq (chunk-chunk-type-fct chunk) 'instr))
  
  (format t "*** ~8,2F: ~A: ~A~%" (mp-time) chunk (second (assoc chunk *descriptions*))))
(when (and (eq *verbose* 'full) chunk (eq (chunk-chunk-type-fct chunk) 'condition))
  (let ((value (chunk-slot-value-fct chunk 'c)))
    (if (eq value 'stop)
        (format t "        ~8,2F: All conditions matched~%" (mp-time))
      (format t "        ~8,2F: Testing condition ~A: ~A~%" (mp-time) chunk value))))
(when (and (eq *verbose* 'full) chunk (eq (chunk-chunk-type-fct chunk) 'action))
  (let ((value (chunk-slot-value-fct chunk 'a)))
    (if (eq value 'stop)
        (format t "        ~8,2F: All actions done~%" (mp-time))
      (format t "        ~8,2F: Carrying out action ~A: ~A~%" (mp-time) chunk value))))
(when (and *verbose* chunk (eq (chunk-chunk-type-fct chunk) 'fact))
      (format t "*** ~8,2F: Retrieving fact ~A: ~A ~A ~A ~A~%" (mp-time) chunk (chunk-slot-value-fct chunk 'slot1)
              (chunk-slot-value-fct chunk 'slot2)(chunk-slot-value-fct chunk 'slot3)(chunk-slot-value-fct chunk 'slot4)))
  
)



;;; Print out nodes and vertices in order to plot a network of instruction DM
;;; Used in combination with an R script
;;;

(defun print-net ()
  ; Vertices first
  (with-open-file (f "~/vertices.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (format f "name task label task-node~%")
  
  (dolist (x *vertices*) (format f "~10S ~10S ~10S ~10S~%" (first x)(second x)(third x)(fourth x))))
  (with-open-file (f "~/edges.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create)

  (format f "from to task-node~%")
  (dolist (x *edges*)(format f "~10S ~10S ~10S~%" (first x)(second x)(third x)))))

;;; Now define a wrapper macro to define a new model

(defmacro define-model-transfer (&body model-code)
`(progn
(clear-all)

;(defmethod device-speak-string ((win rpm-window) text)

;(format t "~S~%" text)

;)
(setf *vertices* nil *edges* nil)
(setf *descriptions* nil)


(undefine-module imaginal)

(define-module-fct 'imaginal
    '(imaginal imaginal-action)
  (list
   (define-parameter :imaginal-delay :valid-test #'nonneg 
     :default-value .2 :warning "non-negative number" 
     :documentation "Time in seconds to respond to an imaginal request")
   (define-parameter :vidt :valid-test #'tornil :default-value nil
          :warning "T or nil" 
          :documentation "Variable Imaginal Delay Time"))
   
  :version "1.2"
  :documentation "The imaginal module provides a goal style buffer with a delay and an action buffer for manipulating the imaginal chunk"
  :creation #'create-imaginal
  :query #'imaginal-query
  :request #'imaginal-request-2
  :buffer-mod #'imaginal-mod-request
  :params #'imaginal-params
  :reset #'imaginal-reset)



;;; Add the new modules to ACT-R
(undefine-module pmmodule)          
(define-module-fct 'pmmodule '((perception nil nil (change)) action) nil
  :version "1.0a1"
  :documentation "pm module"
  :creation #'create-pmmodule
  :reset (list nil #'reset-pmmodule)
  :query #'query-pmmodule
  :request #'request-pmmodule)

(undefine-module rrequest)          
(define-module-fct 'rrequest '(rrequest) nil
  :version "1.0a1"
  :documentation "rrequest module"
  :creation #'create-rrequest
  :reset (list nil #'reset-rrequest)
  :query #'query-rrequest
  :request #'goal-style-request)

(undefine-module rharvest)
(define-module-fct 'rharvest '(rharvest) nil
  :version "1.0a1"
  :documentation "rharvest module"
  :creation #'create-rharvest
  :reset (list nil #'reset-rharvest)
  :query #'query-rharvest
  :request #'goal-style-request)

(undefine-module imaginalreq)
(define-module-fct 'imaginalreq '(imaginalreq) nil
  :version "1.0a1"
  :documentation "imaginalreq module"
  :creation #'create-imaginalreq
  :reset (list nil #'reset-imaginalreq)
  :query #'query-imaginalreq
  :request #'goal-style-request)



(define-model actransfer
(sgp :do-not-harvest imaginal :save-buffer-trace t :esc t :rt -0.5 :lf 0.5 :egs .03 :ul t :epl t :DECLARATIVE-NUM-FINSTS 300
     :DECLARATIVE-FINST-SPAN 3000.0 :alpha 0.1 :ut -10000 :pct t :er t :iu 10.0 :ans 0.1 :bll 0.5 :v nil
     :utility-offsets extra-utility) 
(setf *standard-motor-time* 0.5)
(setf *standard-visual-time* 0.5)
(setf *condition-spread* 0.3)  
(setf *condition-penalty* -0.3)

(sgp :activation-offsets spreading-condition)
(sgp :retrieved-chunk-hook trace-retrieved-instructions)

(chunk-type goal task condition action slot1 slot2 slot3 slot4 slot5 slot6 retrieval-request action-request episodial-request control parent gwm)
(chunk-type instr task condition action slot1 slot2 slot3 slot4 slot5 slot6)
(chunk-type condition name c cnext slot1)
(chunk-type action name a anext slot1)
(chunk-type fact id slot1 slot2 slot3 slot4)
(chunk-type external-action ac slot1 slot2)
(chunk-type update-pmmodule)
(setf *action-sequences* nil)
(setf *condition-sequences* nil)
(setf *verbose* t)

(add-dm 

 (cstop isa condition name cstop c stop)
 (astop isa action name astop a stop)
)

,@model-code

(when (and (no-output (sgp :bll)) (< (caar (no-output (sdp cstop :reference-count))) 2))
(sdp :reference-count 1000 :creation-time -1000000)) ;; 11.5 days, only set if bll is on and it has not yet been set by the model
	  ;;; This will set the activation to approximately 0.7


(p start-a-goal
   ?goal>
     buffer empty
   ?imaginal>
     state free
   ?perception>
     buffer empty
==>
  !bind! =task (task-name *task*)
  +goal>
     isa goal
     task =task
;     condition pending
  +perception>
     isa fact
  +imaginal>
     isa fact
;     id pending
   +rharvest>
     isa fact
   +rrequest>
     isa fact
   +action>
     isa fact
   +imaginalreq>
     isa fact
  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) ;;; Set it up for the next cycle   
  !eval! (setf (dm-failed (get-module declarative)) nil)
)

(p start-a-goal-with-initial-perception
   ?goal>
     buffer empty
   ?imaginal>
     state free
   ?perception>
     - buffer empty
==>
  !bind! =task (task-name *task*)
  +goal>
     isa goal
     task =task
;     condition pending
  +imaginal>
     isa fact
;     id pending
   +rharvest>
     isa fact
   +rrequest>
     isa fact
   +action>
     isa fact
   +imaginalreq>
     isa fact

  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) ;;; Set it up for the next cycle   
  !eval! (setf (dm-failed (get-module declarative)) nil)
)
#|
(p fill-in-imaginal
  "Bit of a nasty bugger, should be in the module. Only necessary at the start."
   =goal>
      isa goal
      condition pending
   =imaginal>
      isa fact
      id pending
==>
   =goal>
     condition nil
   =imaginal>
      id =imaginal)
|#
(p retrieve-instruction
   =goal>
     isa goal
     task =task
     condition nil
     action nil
   ?retrieval>
     state free
     buffer empty
   ?imaginal>
     state free
   !safe-eval! (and (not (pmmodule-change (get-module pmmodule)))(not (pmmodule-busy (get-module pmmodule)))) 
;; same as ?perception> state free, but that was dropped during compilation
   !safe-eval! (not (dm-failed (get-module declarative)))
==>
   +retrieval>
     isa instr
     task =task
     :recently-retrieved nil
   )

(spp retrieve-instruction :u 0.5)

(p retry-on-pmmodule-change
   =goal>
     isa goal
     condition nil
     action nil
   ?retrieval>
     state free
     buffer empty
   ?imaginal>
     state free
   ?perception>
     change t
==>
   +perception>
     isa update-pmmodule
  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) ;;; Set it up for the next cycle   
)


(p retry-on-failure
   =goal>
     isa goal
     action nil
     condition nil
   ?retrieval>
      state error
==>
   !eval! (setf (dm-failed (get-module declarative)) nil)
  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) ;;; Set it up for the next cycle   
  !safe-eval!  (trace-output (format nil "*** ~6,2F: Failed to retrieve an instruction, retrying." (mp-time))))



(p store-retrieved-instruction
   =goal>
     isa goal
     action nil
   =retrieval>
     isa instr
     condition =cond
     action =act
     slot1 =const1
     slot2 =const2
     slot3 =const3
     slot4 =const4
     slot5 =const5
     slot6 =const6

==>
  =goal>
    condition =cond
    action =act
     slot1 =const1
     slot2 =const2
     slot3 =const3
     slot4 =const4
     slot5 =const5
     slot6 =const6
  +retrieval>
    isa condition
    name =cond
;  !safe-eval!  (trace-output (format nil "*** ~6,2F: ~A: ~S" (mp-time) (chunk-copied-from-fct =retrieval) (second (assoc  (chunk-copied-from-fct =retrieval) *descriptions*))))
)

;;; Loop through the nil checks

(defvar *problem-states*)
(defvar *retrieval-request-slots*)
(defvar *retrieval-harvest-slots*)
(defvar *const-slots*)
(defvar *visual-slots*)
(defvar *motor-slots*)
(defvar *imaginalreq-slots*)
(defvar *goal-slots*)

(setf *problem-states* '((PSid id imaginal)(PS1 slot1 imaginal) (PS2 slot2 imaginal) (PS3 slot3 imaginal) (PS4 slot4 imaginal)))
(setf *retrieval-request-slots* '((RTid id rrequest)(RT1 slot1 rrequest) (RT2 slot2 rrequest) (RT3 slot3 rrequest) (RT4 slot4 rrequest)))
(setf *retrieval-harvest-slots* '((RTid id rharvest)(RT1 slot1 rharvest) (RT2 slot2 rharvest) (RT3 slot3 rharvest) (RT4 slot4 rharvest)))
(setf *const-slots* '((CONST1 slot1 goal)(CONST2 slot2 goal)(CONST3 slot3 goal)(CONST4 slot4 goal)(CONST5 slot5 goal)(CONST6 slot6 goal)))
(setf *visual-slots* '((V1 slot1 perception)(V2 slot2 perception)(V3 slot3 perception)(V4 slot4 perception)))
(setf *motor-slots* '((AC1 slot1 action)(AC2 slot2 action)(AC3 slot3 action)))  ;; removed (VOC string vocal)
(setf *imaginalreq-slots* '((newWM1 slot1 imaginalreq)(newWM2 slot2 imaginalreq)(newWM3 slot3 imaginalreq)(newWM4 slot4 imaginalreq)))
(setf *goal-slots* '((task0 task goal2)(control0 control goal2)(parent0 parent goal2)(gwm0 gwm goal2)))

(defvar *all-lhs-slots*)
(defvar *all-rhs-slots*)
(setf *all-lhs-slots* (append *problem-states* *const-slots* *visual-slots* *retrieval-harvest-slots* *goal-slots*))
(setf *all-rhs-slots* (append *problem-states* *retrieval-request-slots* *motor-slots* *imaginalreq-slots* *goal-slots*))

  (dolist (x *all-lhs-slots*)
    (let ((test (intern (format nil "~A=NIL" (first x))))
          (goal-test (if (eq (third x) 'goal) (list (second x) 'null)
          	(when (eq (third x) 'goal2) (list (second x) nil))))
          (imaginal-test (when (eq (third x) 'imaginal) (list '=imaginal> 'isa 'fact (second x) nil)))
          (retrieval-test (when (eq (third x) 'rharvest) (list '=rharvest> 'isa 'fact (second x) nil)))
          (pmmodule-test (when (eq (third x) 'perception)(list '=perception> 'isa 'fact (second x) nil))))
      (eval `(add-dm (,test isa chunk)))
    (eval
`(p ,(new-name-fct (format nil "check-~A=nil-" (first x)))
    =goal>
      isa goal
      ,@goal-test
      ,@retrieval-test
   =retrieval>
     isa condition
     c ,test
     cnext =c2
   ,@imaginal-test
   ,@pmmodule-test
==> 
   +retrieval>
    isa condition
    name      =c2))))


  (dolist (x *all-lhs-slots*)
    (let ((test (intern (format nil "~A<>NIL" (first x))))
          (goal-test (if (eq (third x) 'goal) (list (second x) '=value '!safe-eval! '(not (equal =value 'null)))
          	(when (eq (third x) 'goal2) (list (second x) '=value))))
          (imaginal-test (when (eq (third x) 'imaginal) (list '=imaginal> 'isa 'fact (second x) '=value)))
          (retrieval-test (when (eq (third x) 'rharvest) (list '=rharvest> 'isa 'fact (second x) '=value)))
          (pmmodule-test (when (eq (third x) 'perception)(list '=perception> 'isa 'fact (second x) '=value))))
      (eval `(add-dm (,test isa chunk)))
    (eval
`(p ,(new-name-fct (format nil "check-~A<>nil-" (first x)))
    =goal>
      isa goal
      ,@goal-test
      ,@retrieval-test
   =retrieval>
     isa condition
     c ,test
     cnext =c2
   ,@imaginal-test
   ,@pmmodule-test
==> 
   +retrieval>
    isa condition
    name      =c2))))

;;; Check-not-equal and check-equal
;;; Needs to loop through the following slots:
;;; PS1-4, RT1-4, CONST1-2, and later possibly others (visual, etc.)
;;;

(dolist (x *all-lhs-slots*)
      (let (
          (goal-test-1 (when (member (third x) '(goal goal2)) (list (second x) '=value1)))
          (imaginal-test-1 (when (eq (third x) 'imaginal) (list '=imaginal> 'isa 'fact (second x) '=value1)))
          (retrieval-test-1 (when (eq (third x) 'rharvest) (list '=rharvest> 'isa 'fact (second x) '=value1)))
          (pmmodule-test-1 (when (eq (third x) 'perception)(list '=perception> 'isa 'fact (second x) '=value1))))
        (dolist (y *all-lhs-slots*)
          (when (not (equal x y))
      (let ((test (intern (format nil "~A=~A" (first x)(first y))))
            (test-n (intern (format nil "~A<>~A" (first x)(first y))))
          (goal-test-2 (when (member (third y) '(goal goal2)) (list (second y) '=value2)))
          (imaginal-test-2 (when (eq (third y) 'imaginal) 
                             (if (null imaginal-test-1) (list '=imaginal> 'isa 'fact (second y) '=value2)
                                       (list (second y) '=value2))))
          (pmmodule-test-2 (when (eq (third y) 'perception) 
                             (if (null pmmodule-test-1) (list '=perception> 'isa 'fact (second y) '=value2)
                                       (list (second y) '=value2))))
          (retrieval-test-2 (when (eq (third y) 'rharvest) 
                             (if (null retrieval-test-1) (list '=rharvest> 'isa 'fact (second y) '=value2)
                                       (list (second y) '=value2)))))      
        
      (eval `(add-dm (,test isa chunk)(,test-n isa chunk)))
    (eval
`(p ,(new-name-fct (format nil "check-~A=~A-" (first x)(first y)))
    =goal>
      isa goal
      ,@goal-test-1
      ,@goal-test-2
      ,@retrieval-test-1
      ,@retrieval-test-2
      ,@pmmodule-test-1
      ,@pmmodule-test-2
   =retrieval>
     isa condition
     c ,test
     cnext =c2
   ,@imaginal-test-1
   ,@imaginal-test-2
   !safe-eval! (equal =value1 =value2)
==>
   +retrieval>
    isa condition
    name      =c2))
   (eval
`(p ,(new-name-fct (format nil "check-~A<>~A-" (first x)(first y)))
    =goal>
      isa goal
      ,@goal-test-1
      ,@goal-test-2
      ,@retrieval-test-1
      ,@retrieval-test-2
      ,@pmmodule-test-1
      ,@pmmodule-test-2
   =retrieval>
     isa condition
     c ,test-n
     cnext =c2
   ,@imaginal-test-1
   ,@imaginal-test-2
   !safe-eval! (not (equal =value1 =value2))
==>
   +retrieval>
    isa condition
    name      =c2))

)))))

(p general-condition-does-not-match
   =goal>
     isa goal
   =retrieval>
     isa condition
   !eval! t ;; compilation blocker
==> 
  !safe-eval! (trace-output (format nil "*** ~8,2F: Mismatch on condition ~A" (mp-time) =retrieval))

  =goal>
     condition nil
     action nil
     slot1 nil)

;(spp general-condition-does-not-match :reward 0)

;;;

(p check-condition-done
   =goal>
     isa goal
   =retrieval>
     isa condition
     c stop
==>
   =goal>
     condition act
)  

(p start-action-sequence
   =goal>
     isa goal
     condition act
     action =a1
   ?retrieval>
     state free
     buffer empty
   !safe-eval! (not (dm-failed (get-module declarative)))
==>
  =goal>
    condition nil
   +retrieval>
    isa action
    name      =a1

)  

(spp start-action-sequence :u 2.0)

;(spp check-done :reward 2.0)

;;; Action productions all move something from one slot to another
;;; Some slots are destination only, mainly motor actions

(dolist (x *all-lhs-slots*)
      (let (
          (goal-test (when (member (third x) '(goal goal2)) (list (second x) '=value)))
          (imaginal-test-1 (when (eq (third x) 'imaginal) (list '=imaginal> 'isa 'fact (second x) '=value)))
          (retrieval-test (when (eq (third x) 'rharvest) (list '=rharvest> 'isa 'fact (second x) '=value)))
          (visual-test (when (eq (third x) 'perception)(list '=perception> 'isa 'fact (second x) '=value))))
        (dolist (y *all-rhs-slots*)
          (when (not (equal x y))
      (let ((test (intern (format nil "~A->~A" (first x)(first y))))
          (goal-action (cond ((member (third y) '(goal goal2)) (list '=goal> (second y) '=value))
          					 ((eq (third y) 'rrequest) '(=goal> retrieval-request t))
                                                 ((eq (third y) 'imaginalreq) '(=goal> episodial-request t))
          					 ((eq (third y) 'action) '(=goal> action-request t))))
          (imaginal-test-2 (when (and (null imaginal-test-1) (eq (third y) 'imaginal)) '(=imaginal> isa fact)))
          (imaginal-action (when (eq (third y) 'imaginal) 
                              (list '=imaginal> (second y) '=value)))
          (imaginalreq-test (when (eq (third y) 'imaginalreq) '(=imaginalreq> isa fact)))
          (imaginalreq-action (when (eq (third y) 'imaginalreq) (list '=imaginalreq> (second y) '=value)))
          (external-action-test (when  (eq (third y) 'action) '(=action> isa fact)))
          (external-action (when (eq (third y) 'action) (list '=action> (second y) '=value)))
          (retrieval-match (when (eq (third y) 'rrequest) '(=rrequest> isa fact)))                
          (retrieval-action (when (eq (third y) 'rrequest) (list '=rrequest> (second y) '=value))))
      (eval `(add-dm (,test isa chunk)))
    (eval
`(p ,(new-name-fct (format nil "action-~A->~A-" (first x)(first y)))
    =goal>
      isa goal
      ,@goal-test
      ,@retrieval-test
      ,@visual-test
   =retrieval>
     isa action
     a ,test
     anext =anext
   ,@imaginal-test-1
   ,@imaginal-test-2
   ,@imaginalreq-test
;   ,@motor-test
   ,@external-action-test
   ,@retrieval-match
==>
   ,@goal-action
   ,@retrieval-action
   ,@imaginalreq-action
   ,@imaginal-action
;   ,@motor-action
   ,@external-action
   +retrieval>
    isa action
    name      =anext))

)))))



(p action-done
   =goal>
     isa goal
   =retrieval>
     isa action
     a done
   ?action>
     state free
==>
   -goal>
   -imaginal>
   -perception>
  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) ;;; Set it up for the next cycle   
   !stop!
)

(p action-goal-finish
   =goal>
      isa goal
      task finish
      condition nil
      action nil
==>
   -goal>
   -imaginal>
   -perception>
  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) ;;; Set it up for the next cycle   
   !stop!
)


(p action-stop-init-no-retrieval-request
   =goal>
     isa goal
     retrieval-request nil
   =retrieval>
     isa action
     a stop
==>
  +rharvest>
    isa fact
  =goal>
    action wait-all-done)

(p action-stop-init-retrieval-request
   =goal>
     isa goal
     retrieval-request t
   =retrieval>
     isa action
     a stop
==>
  =goal>
    action wait-all-done)


(p action-stop-done
   "Last action and there is no retrieval, action or anything else left"
   =goal>
     isa goal
     action wait-all-done
     retrieval-request nil
     action-request nil
     episodial-request nil
   ?retrieval>
     state free
     buffer empty
;   =imaginal>
;     isa fact
;     id =value
   !safe-eval! (not (dm-failed (get-module declarative)))
==>
  =goal>
    action nil

  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) 
)
#|
(p action-stop-done-fill-imaginal
   "Last action and there is no retrieval, action or anything else left"
   =goal>
     isa goal
     action wait-all-done
     retrieval-request nil
     action-request nil
     episodial-request nil
   ?retrieval>
     state free
     buffer empty
   =imaginal>
     isa fact
     id nil
   !safe-eval! (not (dm-failed (get-module declarative)))
==>
  =goal>
    action nil
  =imaginal>
    id =imaginal
  !safe-eval! (setf (dm-finsts (get-module declarative)) nil) 
)
|#
;;; Perform an external action

(p action-do-action-stop-1
   =goal>
     isa goal
     action wait-all-done
     action-request t
   =action>
     isa fact
     slot1 =act
     slot2 nil
     slot3 nil
   ?action>
     state free
==>
   =goal>
     action-request nil
   +action>
     isa external-action
     ac =act
)


(p action-do-action-stop-2
   =goal>
     isa goal
     action wait-all-done
     action-request t
   =action>
     isa fact
     slot1 =act
     slot2 =arg1
     slot3 nil
   ?action>
     state free
==>
   =goal>
     action-request nil
   +action>
     isa external-action
     ac =act
     slot1 =arg1
)

(p action-do-action-stop-3
   =goal>
     isa goal
     action wait-all-done
     action-request t
   =action>
     isa fact
     slot1 =act
     slot2 =arg1
     slot3 =arg2
   ?action>
     state free
==>
   =goal>
     action-request nil
   +action>
     isa external-action
     ac =act
     slot1 =arg1
     slot2 =arg2
)

;;; When we are done with all the actions we are going to handle any retrievals
;;; Retrieval requests and harvests can have any combination of variables and nils, so we need a separate rule for each of them
(dolist (id '(t nil))
(dolist (slot1 '(t nil))
  (dolist (slot2 '(t nil))
    (dolist (slot3 '(t nil))
      (dolist (slot4 '(t nil))
        (eval
`(p ,(new-name-fct "check-done-start-retrieval")
   =goal>
     isa goal
     action wait-all-done
     retrieval-request t
   =rrequest>
     isa fact
     id ,(if id '=r0 nil)
     slot1 ,(if slot1 '=r1 nil)
     slot2 ,(if slot2 '=r2 nil)
     slot3 ,(if slot3 '=r3 nil)
     slot4 ,(if slot4 '=r4 nil)
   ?retrieval>
     state free
     buffer empty
   !safe-eval! (not (dm-failed (get-module declarative)))
==>
  =goal>
    retrieval-request nil
 +retrieval>
     isa fact
     ,@(if id '(id =r0) nil)
     ,@(if slot1 '(slot1 =r1) nil)
     ,@(if slot2 '(slot2 =r2) nil)
     ,@(if slot3 '(slot3 =r3) nil)
     ,@(if slot4 '(slot4 =r4) nil)
))
        (eval
`(p ,(new-name-fct "harvest-retrieval")
   =goal>
     isa goal
     action wait-all-done
   =retrieval>
     isa fact
     id ,(if id '=r0 nil)
     slot1 ,(if slot1 '=r1 nil)
     slot2 ,(if slot2 '=r2 nil)
     slot3 ,(if slot3 '=r3 nil)
     slot4 ,(if slot4 '=r4 nil)
   =rrequest>
     isa fact
   !eval! t
==>
  +rharvest>
    isa fact
     id ,(if id '=r0 nil)
     slot1 ,(if slot1 '=r1 'null)
     slot2 ,(if slot2 '=r2 'null)
     slot3 ,(if slot3 '=r3 'null)
     slot4 ,(if slot4 '=r4 'null)
  =rrequest>
     id ,(if id '=r0 nil)
     slot1 ,(if slot1 '=r1 'null)
     slot2 ,(if slot2 '=r2 'null)
     slot3 ,(if slot3 '=r3 'null)
     slot4 ,(if slot4 '=r4 'null)
  +rrequest>
    isa fact
))


)))))


(p retrieval-failure
   =goal>
     isa goal
     action wait-all-done
   ?retrieval>
     state error
   =rrequest>
     isa fact

==>
  +rharvest>
    isa fact
    slot1 error
  =rrequest>
    slot1 error
    slot2 nil
    slot3 nil
    slot4 nil
  +rrequest>
    isa fact
   !eval! (setf (dm-failed (get-module declarative)) nil)
)


;;; Imaginalreq actions
;; Need versions for different numbers of slots

(dolist (slot1 '(t nil))
  (dolist (slot2 '(t nil))
    (dolist (slot3 '(t nil))
      (dolist (slot4 '(t nil))
        (eval
`(p ,(new-name-fct "new-imaginal")
   =goal>
     isa goal
     action wait-all-done
     episodial-request t
   =imaginalreq>
     isa fact
     slot1 ,(if slot1 '=r1 nil)
     slot2 ,(if slot2 '=r2 nil)
     slot3 ,(if slot3 '=r3 nil)
     slot4 ,(if slot4 '=r4 nil)
==>
  =goal>
    episodial-request nil
  =imaginalreq>
    id nil
    slot1 nil
    slot2 nil
    slot3 nil
    slot4 nil
  +imaginal>
     isa fact
     ,@(if slot1 '(slot1 =r1) nil)
     ,@(if slot2 '(slot2 =r2) nil)
     ,@(if slot3 '(slot3 =r3) nil)
     ,@(if slot4 '(slot4 =r4) nil)
))
 

))))




(spp general-condition-does-not-match :u -20.0 )

)))

(format t "~%Loaded Actransfer version ~A from ~A~%" *actransfer-version* *actransfer-date*)