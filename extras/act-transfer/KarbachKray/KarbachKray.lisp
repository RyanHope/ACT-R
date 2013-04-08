;;; Task Switching, Working memory capacity and Stroop model
;;; 
;;; Copyright 2012 Niels Taatgen
;;;
;;; Model of the Karbach and Kray (2009) experiment
;;;

;;; Run the full model with (do-karbach n) where n is the desired number of subjects
;;; This will generate three data files: stroop.txt, digitspan.txt and task-switching.txt

;;; The individual models can be tested with:
;;; (test-count-span) : runs a single trial of countspan
;;; (test-stroop): run a block of Stroop
;;; (do-task-switch): runs a block of task-switching
;;; (do-single-task): runs a block of single task


;;; Code to implement the Countspan task
;;;

(defstruct countspantask nums numbers trials responses)

(defvar *cstask*)
(defvar *outputdir* "~/")

(defun get-next-symbol ()
   (case (pop (countspantask-numbers *cstask*))
                              (dbc '(yes blue circle))
                              (lgc '(yes green circle))
                              (dbs '(yes blue square))) 
)

(defun set-new-count-span-trial ()
  (let ((darkbluecircle (pop (countspantask-nums *cstask*)))
        (lightgreencircle (1+ (random 5)))
        (darkbluesquares (nth (random 5) '(1 3 5 7 9)))
        stims)
    (dotimes (i darkbluecircle) (push 'dbc stims))
    (dotimes (i lightgreencircle) (push 'lgc stims))
    (dotimes (i darkbluesquares) (push 'dbs stims))
    (setf (countspantask-numbers *cstask*) (permute-list stims)))
  (schedule-delayed-action (get-next-symbol) 0.5)  
  )

(defun test-count-span ()
  (set-task 'count-span)
  (init-task)
  (setf (countspantask-nums *cstask*) '(3 4 5))
  (set-new-count-span-trial)
  (sgp :v nil :save-buffer-trace t)
  (setf *verbose* t)
  (run 100)
)

(defun init-count-span ()
  (setf *cstask* (make-countspantask))
  (setf *results* nil)
      (let ((trials nil))
        (dotimes (i 5)
          (dotimes (j 3)
            (let ((trial nil))
              (dotimes (k (+ 2 i))
                (push (+ 3 (random 7)) trial))
              (push trial trials))))
    (setf (countspantask-trials *cstask*) (permute-list trials)))
 )

;;; Countspan has the following actions:
;;;
;;; say number: used to say the number of blue circles
;;; report number: used to type in all the numbers seen
;;; attend-next: look at the next visual stimulus on the screen and encode it
;;; suppress-pending: ignore the fixation cross on the screen


(defun count-span-action (action &optional h1 h2)
  (let ((latency 0.00)) ;; set default to 0 because most actions are in parallel with cognition
    (when (member action '(say report)) (trigger-reward (task-reward *task*))) 
    (if (and (eq action 'say)(null (countspantask-nums *cstask*))) ;; we just said the last number, so now we have to report
        (setf *perception* '(report)) ;; we just said the last number, so now we have to report
        
      (progn
        (when (eq action 'say)   ;;; Otherwise, when waiting or saying start the next set of stimuli
           (set-new-count-span-trial)
           )
        (when   ;;; Also, if there is a field of stimuli, we look at the next one
            (eq action 'attend-next)
          (setf latency 0.15)
          (if (null (countspantask-numbers *cstask*))
              (setf *perception* '(last))
            (setf *perception* (get-next-symbol))))
        (when (eq action 'suppress-pending) (when (equal *perception* '(pending)) (setf *perception* '(waiting)))
          (setf latency 0.0))
        (when (eq action 'report)    ;;; If we report, than put the reported number in the responses variable
;      (format t "~%Reported number ~A~%" h1)
          (push h1 (countspantask-responses *cstask*))
          (setf latency 0.3))
        ))
     latency))

(defun do-count-span (&optional (nolearn nil))
  (set-task 'count-span)
  (init-task)
  (sgp :v nil :save-buffer-trace nil)
  (when nolearn (sgp :alpha 0))
  (dolist (x (countspantask-trials *cstask*))
    (let ((l (length x)))
      (setf (countspantask-nums *cstask*) x)
      (set-new-count-span-trial)
      (run 1000)
      (clear-buffer 'goal)
      (push (list l x (length (countspantask-responses *cstask*)) (reverse (countspantask-responses *cstask*))) *results*)
    (setf (countspantask-responses *cstask*) nil))))

(defun score-lists (l1 l2)
  (cond ((null l1) 0)
        ((eq (first l1)(first l2))(1+ (score-lists (rest l1)(rest l2))))
        (t (score-lists (Rest l1)(rest l2)))))

(defun report-count-span (&optional (day 1)(condition 'NONE))
  (dolist (x (Reverse *results*))
    (let ((score (score-lists (fourth x)(mapcar #'(lambda (x) (nth x '(zero one two three four five six seven eight nine))) (second x)))))
    (format t "~%~D ~9A ~D ~D ~D ~6,3F" day condition (first x) (third x) score (/ score (first x)))))
  (with-open-file (f (concatenate 'string *outputdir* "digitspan.txt") :direction :output :if-exists :append :if-does-not-exist :create)
  (dolist (x (Reverse *results*))
    (let ((score (score-lists (fourth x)(mapcar #'(lambda (x) (nth x '(zero one two three four five six seven eight nine))) (second x)))))
    (format f "~D ~9A ~D ~D ~D ~6,3F~%" day condition (first x) (third x) score (/ score (first x)))))))

(defun count-span-quick-report ()
  (let ((result1 0)(count 0))
    (dolist (x *results*)
      (setf result1 (+ result1 (third x)))
      (setf count (+ count (first x))))
    (format t "~%~6,3F~%" (/ result1 count))))

 

;;; Run code for the Stroop task
;;;

(defstruct strooptask task-id starttime count numtrials type answer)

(defvar *sttask*)

(setf *vertices* nil)
(setf *edges* nil)

(defun stroop-init ()
  (setf *sttask* (make-strooptask))
  (setf (strooptask-count *sttask*) 0)
  (setf (strooptask-numtrials *sttask*) 24) ;;; default
  (setf *results* nil)
  (setf (strooptask-starttime *sttask*) (+ (mp-time) 1.0)) ;;; start at t=1
  (schedule-delayed-action '(yes) 1.0) ;; Schedule first stimulus
  )

;;; The Stroop actions are the following:
;;; get-property. Can be called with color-property, in which case it only gives the color of the ink, otherwise it gives both word and color
;;; say. Say the answer

(defun stroop-pm (action &optional h1 h2)
	(let ((latency 0.05))
	(case action
	  (get-property 
           (if (= (mod (strooptask-count *sttask*) 2) 0)
               (setf (strooptask-type *sttask*) 'neutral
                     (strooptask-answer *sttask*) 'red-concept
                     *perception* '(rtrain red-color train-word))
             (setf (strooptask-type *sttask*) 'incongruent
                   (strooptask-answer *sttask*) 'red-concept
                   *perception* '(rblue red-color blue-word)))
           (when (eq h1 'color-property) (setf *perception* (list 'rblue (second *perception*))))  ;;; If we ask for color specifically leave out the distractor
           (setf latency 0.2))  
	  (say (setf latency 0.2)
              (let ((correct (equal h1 (strooptask-answer *sttask*))))
                (trigger-reward (if correct (task-reward *task*) 0))
                (push (list (1+ (strooptask-count *sttask*))(+ (mp-time) 0.2 (- (strooptask-starttime *sttask*))) (strooptask-type *sttask*) correct) *results*))
	    (let ((new-percept (if (> (incf (strooptask-count *sttask*)) (strooptask-numtrials *sttask*)) '(last)
                                 (progn (setf (strooptask-starttime *sttask*) (+ (mp-time) 1.2))
                                   '(yes)))))
              (schedule-delayed-action new-percept 1.2)

;	    (format t "~%*** Answer ~A for trial ~A at time ~6,3F~%" h1 (strooptask-type *sttask*) (+ (mp-time) 0.2 (- (strooptask-starttime *sttask*))))
	    )))

     latency))

(defun do-stroop (&optional (day 1)(condition 'NONE)(nolearn nil))
  (set-task 'stroop)
  (init-task)
  (dotimes (i 4)
     (init-task)
     (when nolearn (sgp :alpha 0))
     (run 10000)
     (with-open-file (f (concatenate 'string *outputdir* "stroop.txt") :direction :output :if-exists :append :if-does-not-exist :create)
       (dolist (x (reverse *results*))
         (format t "~%STROOP ~A ~D ~D ~D ~A ~D ~6,3F" condition (1+ i) day (first x) (third x)(if (fourth x) 1 0)(second x))
         (format f "STROOP ~A ~D ~D ~D ~A ~D ~6,3F~%" condition (1+ i) day (first x) (third x)(if (fourth x) 1 0)(second x))
         ))))


(defun test-stroop ()
  (set-task 'stroop)
  (init-task)
   (setf (strooptask-numtrials *sttask*) 4) ;;; default
  ;;; Practice
  (sgp :v nil :save-buffer-trace t) (setf *verbose* t)
  (run 10000))

;;; Run code for task switching
;;;

(defstruct swtask task-id starttime count numtrials)
(defvar *swtask*)

(defun init-single-task-AB ()
  (setf *swtask* (make-swtask))
  (setf (swtask-count *swtask*) 0)
  (setf (swtask-numtrials *swtask*) 17) ;;; default
  (setf *results* nil)
  (setf (swtask-starttime *swtask*) (+ (mp-time) 1.425)) ;; We start after 1.425 seconds
  (schedule-delayed-action '(yes) 1.425) ;; Schedule the first stimulus
  )
  
(defun set-trial-count (n)
  (setf (swtask-numtrials *swtask*) n))

;;; Perceptual motor functions. Both functions are used for the single task as well as the task switching condition

;;; Actions are:

;;; get-property property. Get the desired property of the attented object. property can be food-property, size-property, transport-property or number-property
;;; press-key key. Give a response

(defun single-task-AB (action &optional h1 h2)
"Do the p/m parts of task A and B. It should implement get-property food/size, press-key key and wait"
	(let ((latency 0.05))
	(case action
	  (get-property (cond
                         ((eq h1 'food-property) (setf *perception* (list nil (nth (random 2) '(fruit vegetable)))))
                         ((eq h1 'size-property) (setf *perception* (list nil nil (nth (random 2) '(small large))))))
                        (setf latency 0.13))
	  (press-key (setf latency 0.2)   
	    (push (list (1+ (swtask-count *swtask*))(+ (mp-time) 0.2 (- (swtask-starttime *swtask*)))) *results*)
            (let ((new-percept (if (> (incf (swtask-count *swtask*)) (swtask-numtrials *swtask*))  '(last)
                                 (progn
                                   (setf (swtask-starttime *swtask*) (+ (mp-time) 1.425))  
                                   '(yes)))))
              (schedule-delayed-action new-percept 1.625) ;;; This action is now asynchronous, triggers next stimulus at current time plus 1625 ms (1425 after key press)
              ;; schedule-delayed-action sets percept to pending, and ?state> changing to t until the time has elapsed, after which it will revert to nil and the change is made.
              (trigger-reward (task-reward *task*))
;	    (format t "~%*** Key ~S at time ~6,3F~%" h1 (+ (mp-time) 0.2 (- (swtask-starttime *swtask*))))
	    )))
     latency))
     
(defun single-task-CD (action &optional h1 h2)
"Do the p/m parts of task C and D. It should implement get-property food/size, press-key key and wait"
	(let ((latency 0.05))
	(case action
	  (get-property (cond
                         ((eq h1 'transport-property) (setf *perception* (list nil (nth (random 2) '(plane car)))))
                         ((eq h1 'number-property) (setf *perception* (list nil nil (nth (random 2) '(one two))))))
                        (setf latency 0.13))
	  (press-key (setf latency 0.2)   
                     (push (list (1+ (swtask-count *swtask*))(+ (mp-time) 0.2 (- (swtask-starttime *swtask*)))) *results*)
                     (let ((new-percept (if (> (incf (swtask-count *swtask*)) (swtask-numtrials *swtask*))  '(last)
                                          (progn
                                            (setf (swtask-starttime *swtask*) (+ (mp-time) 1.425))  
                                   '(yes)))))
                       (schedule-delayed-action new-percept 1.625) ;;; This action is now asynchronous, triggers next stimulus at current time plus 1625 ms (1425 after key press)
                       ;; schedule-delayed-action sets percept to pending, and ?state> changing to t until the time has elapsed, after which it will revert to nil and the change is made.
                       (trigger-reward (task-reward *task*))
;	    (format t "~%*** Key ~S at time ~6,3F~%" h1 (+ (mp-time) 0.2 (- (swtask-starttime *swtask*))))
                       )))
        latency))

;;; Testing code

(defun do-task-switch ()
  (set-task 'task-switching-AB)
  (init-task)
  (sgp :v nil :save-buffer-trace t)
  (setf *verbose* t)
  (run 10000)
  (dolist (x (reverse *results*))(format t "~%A ~6,3F" x)))

(defun do-single-task ()
  (set-task 'single-task-A)
  (init-task)
   (sgp :v nil :save-buffer-trace t)
  (setf *verbose* t)
  (run 10000)
  (dolist (x (reverse *results*))(format t "~%B ~6,3F" x)))


;;; The experiment is as follows
;;; Blocks are always 17 trials
;;; Day 1, tasks A and B:
;;; Two single task blocks for training that are discarded
;;; 2 single - 2 mixed - 2 single - 2 mixed - single - 2 mixed - single - 2 mixed - single - 2 mixed - single - 2 mixed
;;;
;;; Days 2-5 (4 days), tasks C and D
;;; Both conditions:
;;; Two practice blocks followed by 24 experimental blocks
;;;
;;; Day 6: same as day 1

  
(defun do-karbach-full (&optional (conditions '(single switch)))
 (dolist (cnd conditions)
  (reset)
  (sgp :v nil :save-buffer-trace nil)
  (setf *verbose* nil)
  ;;; Day 1
  (do-count-span)
  (report-count-span 1 cnd)
  (reset)
  (sgp :v nil :save-buffer-trace nil)
  (setf *verbose* nil)
  (do-stroop 1 cnd) 
  (reset)
  (sgp :v nil :save-buffer-trace nil)
  (setf *verbose* nil)
 (let ((schedule '(A B A B AB AB A B AB AB A AB AB B AB AB A AB AB B AB AB))
        (alist '((A single-task-a)(B single-task-b)(AB task-switching-AB))))
    (dolist (x schedule)
       (set-task (second (assoc x alist)))
       (init-task)
       (run 500)
     (with-open-file (f (concatenate 'string *outputdir* "task-switching.txt") :direction :output :if-exists :append :if-does-not-exist :create)

       (dolist (y (reverse *Results*))
       	(let ((tp (cond ((and (eq x 'AB)(= (mod (first y) 2) 0)) 'repeat)
       				     ((eq x 'AB) 'switch)
       				     (t 'single))))
         (Format t "~%~A 1 ~A ~A ~D ~6,3F" cnd x tp (first y)(second y))
         (Format f "~A 1 ~A ~A ~D ~6,3F~%" cnd x tp (first y)(second y))
         )))))
  (reset)
  (sgp :v nil :save-buffer-trace nil)
  (setf *verbose* nil)
   ;;; Day 2-5
   (dotimes (day 4)
  (let ((schedule (if (eq cnd 'single)
    			      '(C D C D C D C D C D C D C D C D C D C D C D C D C D)
    			      '(CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD CD)))
        (alist '((C single-task-c)(D single-task-d)(CD task-switching-CD))))
    (dolist (x schedule)
       (set-task (second (assoc x alist)))
       (init-task)
       (run 500)
      (clear-buffer 'goal)
       (dolist (y (reverse *Results*))
       	(let ((tp (cond ((and (eq x 'CD)(= (mod (first y) 2) 0)) 'repeat)
       				     ((eq x 'CD) 'switch)
       				     (t 'single))))
         (Format t "~%~A ~D ~A ~A ~D ~6,3F" cnd (+ day 2) x tp (first y)(second y)))))))
    ;;; Day 6
   (do-stroop 6 cnd)
   (do-count-span)
   (report-count-span 6 cnd)
   (let ((schedule '(A B A B AB AB A B AB AB A AB AB B AB AB A AB AB B AB AB))
        (alist '((A single-task-a)(B single-task-b)(AB task-switching-AB))))
    (dolist (x schedule)
       (set-task (second (assoc x alist)))
       (init-task)
       (run 500)
     (with-open-file (f (concatenate 'string *outputdir* "task-switching.txt") :direction :output :if-exists :append :if-does-not-exist :create)
       (dolist (y (reverse *Results*))
       	(let ((tp (cond ((and (eq x 'AB)(= (mod (first y) 2) 0)) 'repeat)
       				     ((eq x 'AB) 'switch)
       				     (t 'single))))
         (Format t "~%~A 6 ~A ~A ~D ~6,3F" cnd x tp (first y)(second y))
         (Format f "~A 6 ~A ~A ~D ~6,3F~%" cnd x tp (first y)(second y))

         ))))) 

         ))
 
(defun do-karbach (n)
  (dotimes (i n) (format t "~%~%***** Run number ~D ****~%~%~%" (1+ i))
    (do-karbach-full)))

(defun run-experiment (n)
  (do-karbach n))

(defun run-sample (&optional x)
  (case x
    (1 (print "Running Stroop") (test-stroop))
    (2 (print "Running Countspan") (test-count-span))
    (3 (print "Running Task Switch") (do-task-switch))
    (4 (print "Running Single Task") (do-single-task))
    (otherwise (print "1 - Stroop  2 - Countspan  3 - Task Switch 4 - Single Task"))))

(clear-all)

(define-model-transfer

(add-dm

;;; count-facts
 (count0 isa fact slot1 count-fact slot2 zero slot3 one)
 (count1 isa fact slot1 count-fact slot2 one slot3 two)
 (count2 isa fact slot1 count-fact slot2 two slot3 three)
 (count3 isa fact slot1 count-fact slot2 three slot3 four)
 (count4 isa fact slot1 count-fact slot2 four slot3 five)
 (count5 isa fact slot1 count-fact slot2 five slot3 six)
 (count6 isa fact slot1 count-fact slot2 six slot3 seven)
 (count7 isa fact slot1 count-fact slot2 seven slot3 eight)
 (count8 isa fact slot1 count-fact slot2 eight slot3 nine)
 (count9 isa fact slot1 count-fact slot2 nine slot3 ten)
 (count10 isa fact slot1 count-fact slot2 ten slot3 eleven)
 (count11 isa fact slot1 count-fact slot2 eleven slot3 twelve)
 (count12 isa fact slot1 count-fact slot2 twelve slot3 thirteen)
 (count13 isa fact slot1 count-fact slot2 thirteen slot3 fourteen)
 (count14 isa fact slot1 count-fact slot2 fourteen slot3 fifteen)
 (count15 isa fact slot1 count-fact slot2 fifteen slot3 sixteen)
 (count16 isa fact slot1 count-fact slot2 sixteen slot3 seventeen)
 (count17 isa fact slot1 count-fact slot2 seventeen slot3 eighteen)
 (count18 isa fact slot1 count-fact slot2 eighteen slot3 nineteen)
 (count19 isa fact slot1 count-fact slot2 nineteen slot3 twenty)


)

;;; Instructions for the count span task

(add-instr count-span :input (Vobject Vcolor Vshape) :variables (WMcount WMprev)  :declarative ((RTcount RTfirst RTsecond)(RTcount RTprev))
:pm-function count-span-action
:init init-count-span
:reward 10.0 ;; might take long
:parameters ((sgp :lf 0.05 :egs 0.3 :ans 0.1 :rt -0.7 :perception-activation 0.0 :imaginal-activation 1.0 :mas nil :alpha 0.03)(setf *condition-spread* 1.0)   
             (spp retrieve-instruction :u 2.0)
          (setf *verbose* nil)
)  ;; :ans was 0.2
(ins :condition (WMcount = nil) :action (count-squares -> Gcontrol zero -> WMcount WMid -> Gtop (wait) -> AC) :description "Set count to zero and wait for first screen")
(ins :condition (Vobject = yes  Vcolor <> blue  Gcontrol = count-squares) :action ((attend-next) -> AC) :description "Green thing, so ignore")
(ins :condition (Vobject = yes  Vshape <> circle  Gcontrol = count-squares) :action ((attend-next) -> AC) :description "Square, so ignore")
(ins :condition (Vcolor = blue  Vshape = circle  RTsecond = nil  Gcontrol = count-squares) :action ( (count-fact WMcount) -> RT) :description "Blue circle, retrieve next count")
(ins :condition (RTsecond <> nil  Gcontrol = count-squares) :action (RTsecond -> WMcount (attend-next) -> AC) :description "Update count, attend next object")
(ins :condition (Vobject = last Gcontrol = count-squares) :action ((zero WMid) -> newWM pre-rehearse -> Gcontrol (say WMcount) -> AC) :description "No more object, remember count")
;(ins :condition (Vobject = last  WMtop = nil Gcontrol = count-squares) :action (  (top WMcount) -> EP EPself -> WMtop EPself -> WMcurrent pre-rehearse -> Gcontrol  (say WMcount) -> AC) :description "No more objects, remember count as first item")
;(ins :condition (Vobject = last  WMtop <> nil  Gcontrol = count-squares) :action ( (WMcurrent WMcount) -> EP EPself -> WMcurrent pre-rehearse -> Gcontrol (say WMcount) -> AC) :description "No more objects, remember count as next item on the list")
(ins :condition (Vobject = pending) :action ((wait) -> AC) :description "If there is nothing to see, just wait")
(ins :condition (Vobject = pending  Gcontrol = pre-rehearse) :action (Gtop -> RTid (suppress-pending) -> AC rehearse -> Gcontrol) :description "If there is nothing to see, start rehearsing!")
(ins :condition (Gcontrol = rehearse RT1 <> error) :action (RTid -> RTprev rehearse -> Gcontrol) :description "Rehearse the next item")
(ins :condition (Vobject = yes Gcontrol = pre-rehearse) :action (count-squares -> Gcontrol) :description "If a new screen is up, start a new count")
(ins :condition (RT1 = error  Gcontrol = rehearse) :action (pre-rehearse -> Gcontrol) :description "If we're done with the list, start again!")
(ins :condition (Vobject = report  Gcontrol = pre-rehearse) :action (Gtop -> RTid  report -> Gcontrol ) :description "Report is asked for, retrieve first item")

(ins :condition (RT1 <> error Gcontrol = report) :action (RTid -> RTprev  (report  RTcount) -> AC) :description "Report item and retrieve next")
(ins :condition (RT1 = error  Gcontrol = report) :action (finish -> Gtask) :description "No more items: done")
)



;;; What is the sequence?
;;; Initial: PS empty, V empty
;;; Experimental code puts something in V1: V1 has an object
;;; First instruction puts something in PS1: PS1 has the task
;;; Second instruction gets the property of the object in V1: PS1 task, V1 is empty again, V2 has the property
;;; Retrieval gets the key corresponding to V2: PS1 still has the task, V1 is empty V2 has the property and V3 has the key
;;; After key has been pressed, PS1 has the task, and V1-V3 should be empty (action should clear them)

(add-instr single-task-A :input (Vobject Vfood Vsize)  :declarative ((RTmapping RTcategory RTkey))
:pm-function single-task-AB
:init init-single-task-AB
:reward 10.0
:facts ((isa fact slot1 mapping slot2 vegetable slot3 "s")(isa fact slot1 mapping slot2 fruit slot3 "k"))
:parameters ((sgp :lf 0.05 :egs 0.04 :rt 0.0 :perception-activation 0.0 :mas nil :alpha 0.03 :imaginal-activation 0.0 )             (spp retrieve-instruction :u 2.0)
(setf *condition-spread* 1.0)             (setf *verbose* nil)
)
;;; Initialize the first trial
(ins :condition (Vobject = pending) :action ((wait) -> AC) :description "Wait for next stimulus")
(ins :condition (Vobject = yes) :action ((get-property food-property) -> AC) :description "Stimulus detected, get food-property")
(ins :condition (Vfood <> nil RTkey = nil) :action ((mapping Vfood) -> RT) :description "Retrieve key related to food concept")
(ins :condition (RTkey <> nil) :action ((press-key RTkey) -> AC) :description "Press appropriate key")
(ins :condition (Vobject = last) :action (finish -> Gtask) :description "Done with the block")

)

(add-instr single-task-B :input (Vobject Vfood Vsize)  :declarative ((RTmapping RTcategory RTkey))
:pm-function single-task-AB
:init init-single-task-AB
:reward 10.0
:facts ((isa fact slot1 mapping slot2 small slot3 "s")(isa fact slot1 mapping slot2 large slot3 "k"))
:parameters ((sgp :lf 0.05 :egs 0.04 :rt 0.0 :perception-activation 0.0 :mas nil :alpha 0.03 :imaginal-activation 0.0 )             (spp retrieve-instruction :u 2.0)
(setf *condition-spread* 1.0)             (setf *verbose* nil)
)
;;; Initialize the first trial
(ins :condition (Vobject = pending) :action ((wait) -> AC))
(ins :condition (Vobject = yes) :action ((get-property size-property) -> AC))
(ins :condition (Vsize <> nil RTkey = nil) :action ((mapping Vsize) -> RT))
(ins :condition (RTkey <> nil) :action ((press-key RTkey) -> AC))
(ins :condition (Vobject = last) :action (finish -> Gtask))

;;; Also: the simulation now has to wait for the next stimulus
)

(add-instr task-switching-AB :input (Vobject Vfood Vsize) :variables (WMcur-task WMcount) :declarative ((RTmapping RTcategory RTkey)(RTother RTfirst RTsecond))
:pm-function single-task-AB
:init init-single-task-AB
:reward 10.0
:facts ((isa fact slot1 other-task slot2 food-task slot3 size-task)(isa fact slot1 other-task slot2 size-task slot3 food-task))
:parameters ((sgp :lf 0.05 :egs 0.04 :rt 0.0 :perception-activation 0.0 :mas nil :alpha 0.03 :imaginal-activation 0.0 )             (spp retrieve-instruction :u 2.0)
(setf *condition-spread* 1.0)             (setf *verbose* nil)
)

(ins :condition (WMcur-task = nil) :action (food-task -> WMcur-task  one -> WMcount do-task -> Gcontrol (wait) -> AC) :description "Initialize task, set task to food, wait for first stimulus")
(ins :condition (Gcontrol = do-task Vobject = yes WMcur-task = food-task ) :action ((get-property food-property) -> AC) :description "Task is food so get food property")
(ins :condition (Gcontrol = do-task Vobject = yes WMcur-task = size-task) :action  ((get-property size-property) -> AC) :description "Task is size so get size property")
(ins :condition (Vfood <> nil RTkey = nil) :action ((mapping Vfood) -> RT) :description "Get food - key mapping")
(ins :condition (Vsize <> nil RTkey = nil) :action ((mapping Vsize) -> RT) :description "Get Size - key mapping")
(ins :condition (Gcontrol = do-task RTkey <> nil) :action ((press-key RTkey) -> AC) :description "Press appropriate key")
(ins :condition (Vobject = pending  Gcontrol = do-task) :action (choose-task -> Gcontrol) :description "While waiting determine next task")
(ins :condition (Vobject = pending  Gcontrol <> choose-task) :action (choose-task -> Gcontrol (wait) -> AC) :description "Not preparing but waiting")
(ins :condition (WMcount = one Gcontrol = choose-task) :action (do-task -> Gcontrol two -> WMcount (wait) -> AC) :description "If we have done the task once increase count and wait")
(ins :condition (RTsecond = nil WMcount = two Gcontrol = choose-task ) :action ((other-task WMcur-task) -> RT) :description "If we have done it twice retrieve the other task")
(ins :condition (RTsecond <> nil Gcontrol = choose-task ) :action (do-task -> Gcontrol one -> WMcount RTsecond -> WMcur-task (wait) -> AC) :description "Set the task to the retrieved task, counter to one, and wait")
(ins :condition (Vobject = last) :action (finish -> Gtask) :description "Done with the block")
)


;;; Now tasks C and D.

(add-instr single-task-C :input (Vobject Vtransport Vnumber)  :declarative ((RTmapping RTcategory RTkey))
:pm-function single-task-CD
:init init-single-task-AB
:reward 10.0
:facts ((isa fact slot1 mapping slot2 plane slot3 "s")(isa fact slot1 mapping slot2 car slot3 "k"))
:parameters ((sgp :lf 0.05 :egs 0.04 :rt 0.0 :perception-activation 0.0 :mas nil :alpha 0.03 :imaginal-activation 0.0 )             (spp retrieve-instruction :u 2.0)
(setf *condition-spread* 1.0)             (setf *verbose* nil)
)
;;; Initialize the first trial
(ins :condition (Vobject = pending) :action ((wait) -> AC))
(ins :condition (Vobject = yes) :action ((get-property transport-property) -> AC))
(ins :condition (Vtransport <> nil RTkey = nil) :action ((mapping Vtransport) -> RT))
(ins :condition (RTkey <> nil) :action ((press-key RTkey) -> AC))
(ins :condition (Vobject = last) :action (finish -> Gtask))

)


(add-instr single-task-D :input (Vobject Vtransport Vnumber)  :declarative ((RTmapping RTcategory RTkey))
:pm-function single-task-CD
:init init-single-task-AB
:reward 10.0
:facts ((isa fact slot1 mapping slot2 one slot3 "s")(isa fact slot1 mapping slot2 two slot3 "k"))
:parameters ((sgp :lf 0.05 :egs 0.04 :rt 0.0 :perception-activation 0.0 :mas nil :alpha 0.03 :imaginal-activation 0.0 )             (spp retrieve-instruction :u 2.0)
(setf *condition-spread* 1.0)             (setf *verbose* nil)
)
;;; Initialize the first trial
(ins :condition (Vobject = pending) :action ((wait) -> AC))
(ins :condition (Vobject = yes) :action ((get-property number-property) -> AC))
(ins :condition (Vnumber <> nil RTkey = nil) :action ((mapping Vnumber) -> RT))
(ins :condition (RTkey <> nil) :action ((press-key RTkey) -> AC))
(ins :condition (Vobject = last) :action (finish -> Gtask))
)

(add-instr task-switching-CD :input (Vobject Vtransport Vnumber) :variables (WMcur-task WMcount) :declarative ((RTmapping RTcategory RTkey)(RTother RTfirst RTsecond))
:pm-function single-task-CD
:init init-single-task-AB
:reward 10.0
:facts ((isa fact slot1 other-task slot2 transport-task slot3 number-task)(isa fact slot1 other-task slot2 number-task slot3 transport-task))
:parameters ((sgp :lf 0.05 :egs 0.04 :rt 0.0 :perception-activation 0.0 :mas nil :alpha 0.03 :imaginal-activation 0.0 )             (spp retrieve-instruction :u 2.0)
(setf *condition-spread* 1.0)             (setf *verbose* nil)
)

(ins :condition (WMcur-task = nil) :action (transport-task -> WMcur-task  one -> WMcount do-task -> Gcontrol (wait) -> AC))
(ins :condition (Gcontrol = do-task Vobject = yes WMcur-task = transport-task ) :action ((get-property transport-property) -> AC))
(ins :condition (Gcontrol = do-task Vobject = yes WMcur-task = number-task) :action  ((get-property number-property) -> AC))
(ins :condition (Vtransport <> nil RTkey = nil) :action ((mapping Vtransport) -> RT))
(ins :condition (Vnumber <> nil RTkey = nil) :action ((mapping Vnumber) -> RT))
(ins :condition (Gcontrol = do-task RTkey <> nil) :action ((press-key RTkey) -> AC))
(ins :condition (Vobject = pending  Gcontrol = do-task) :action (choose-task -> Gcontrol))  ;; 32
;(ins :condition (Vobject = pending  Gcontrol <> choose-task) :action (choose-task -> Gcontrol (wait) -> AC) :description "Not preparing but waiting")
(ins :condition (WMcount = one Gcontrol = choose-task) :action (do-task -> Gcontrol two -> WMcount (wait) -> AC))
(ins :condition (RTsecond = nil WMcount = two Gcontrol = choose-task ) :action ((other-task WMcur-task) -> RT))
(ins :condition (RTsecond <> nil Gcontrol = choose-task ) :action (do-task -> Gcontrol one -> WMcount RTsecond -> WMcur-task (wait) -> AC))
(ins :condition (Vobject = last) :action (finish -> Gtask))
)


(add-instr stroop :input (Vobject Vcolor Vword)  :declarative ((RTmapping RTstimulus RTconcept RTstim-type))
:pm-function stroop-pm
:init stroop-init
:reward 13.0
:facts ((red-word-assoc isa fact slot1 s-mapping slot2 red-word slot3 red-concept slot4 word-task)
        (blue-word-assoc isa fact slot1 s-mapping slot2 blue-word slot3 blue-concept slot4 word-task)
        (red-color-assoc isa fact slot1 s-mapping slot2 red-color slot3 red-concept slot4 color-task)
        (blue-color-assoc isa fact slot1 s-mapping slot2 blue-color slot3 blue-concept slot4 color-task)
        (red-color isa chunk)(red-word isa chunk)(blue-color isa chunk)(blue-word isa chunk)(train-word isa chunk))
:parameters ((sgp :lf 0.05 :egs 0.3 :rt -3.0 :perception-activation 2.0 :mas 3.0 :alpha 0.02 :imaginal-activation 0.0 )             (spp retrieve-instruction :u 1.0)
(setf *condition-spread* 1.0) 
             (sdp (red-color-assoc blue-color-assoc red-word-assoc blue-word-assoc) :references 1000 :creation-time -40000000)
             (add-sji (red-word red-word-assoc 1.5)(red-word blue-word-assoc -1.5)
             		  (red-color blue-color-assoc -1.5)(red-color red-color-assoc 1.5)
             		  (blue-word red-word-assoc -1.5)(blue-word blue-word-assoc 1.5)
                          (blue-word red-color-assoc -1.5)(blue-word blue-color-assoc 1.5)
                          (red-word blue-color-assoc -1.5)(red-word red-color-assoc 1.5)
             		  (blue-color blue-color-assoc 1.5)(blue-color red-color-assoc -1.5))
             (setf *verbose* nil)
             
             )

(ins :condition (Gcontrol = nil) :action (prepare -> Gcontrol  (wait) -> AC) :description "Start Stroop task, wait for stimulus")
(ins :condition (Vobject = yes Gcontrol = prepare) :action ((get-property color-property) -> AC) :description "Object seen, focus on just color") 
(ins :condition (Vobject = yes) :action ((get-property both) -> AC) :description "Object seen, focus on all")
(ins :condition (Vcolor <> nil RTconcept = nil) :action ( (s-mapping Vcolor) -> RT) :description "Retrieve color concept of the ink color")
(ins :condition (RTconcept <> nil) :action ((say RTconcept) -> AC neutral -> Gcontrol) :description "Say the answer") 
(ins :condition (Vobject = pending  Gcontrol = neutral) :action (wait -> AC1 prepare -> Gcontrol) :description "Prepare to focus on the color of the next stimulus")  
(ins :condition (Vobject = pending) :action ((wait) -> AC) :description "Wait for the next stimulus without preparation")
(ins :condition (Vobject = last) :action (finish -> Gtask) :description "Done with this block")


)

(sdp :reference-count 3000 :creation-time -1000000)

)