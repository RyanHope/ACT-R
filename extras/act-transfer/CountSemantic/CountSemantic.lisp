;;; Count and Semantic model
;;; 
;;; Copyright 2012 Niels Taatgen
;;;
;;; This is a demonstration model to show how the model operates. It has two simple models,
;;; one that can count from a particular starting number to a goal number
;;; and a semantic reasoning model that determines whether a canary is an animal
;;;

(clear-all)


;;; The initialization functions are very simple: they just set a fixed input
;;; These values can be changed, or a loop function can be added to set them 
;;; to different values

(defun init-count ()
  (setf *perception* '(two five))
)

(defvar *trials* nil)
(defun init-semantic ()
  (when (null *trials*)   (setf *trials* '((canary animal)(shark bird))))
  (setf *perception* (pop *trials*)))


;;; The action function, which is used for both tasks, just prints the action, and specifies that it takes 0.3 seconds to carry it out
;;; Also give a reward on each action

(defun do-action (action &optional h1 h2)
  (when (eq action 'answer) (issue-reward)) ;; give a reward
  0.3)

;;; The model can be run once by giving the following instructions:

;;; (set-task 'count)
;;; or
;;; (set-task 'semantic)
;;; and then (init-task)
;;; followed by (run 50)
;;;

;;; The following two functions will run either count or semantic a number of times
;; Usage: (do-count 10) or (do-semantic 10)

(defun do-count (n)
  (let (res)
  (sgp :v nil :save-buffer-trace nil)
  (set-task 'count)
  (setf *verbose* t)
  (dotimes (i n)(init-task)(push (run 30.0) res))
  (dolist (x (reverse res))(format t "~%~6,3F" x))))

(defun do-semantic (n)
  (let (res)
   (sgp :v nil :save-buffer-trace nil)
  (set-task 'semantic)
  (setf *verbose* t)
  (dotimes (i n)(init-task)(push (run 30.0) res))
  (dolist (x (reverse res))(format t "~%~6,3F" x))))

(defun test-count ()
  (set-task 'count)
  (init-task)
  (sgp :v nil)
  (setf *verbose* 'full)
  (run 10))

(defun test-semantic ()
  (set-task 'semantic)
  (init-task)
  (sgp :v nil)
  (setf *verbose* 'full)
  (run 10))
;;; This is a simple loop function that runs the count model n times, then runs the episodic model n times,
;;; resets the model, and then runs the episodic model another n times to see what the transfer is.
;;; Run it by (do-it 10), or with whatever number you like


(defun do-it (n)
  (let ((res1 nil)(res2 nil)(res3 nil))
    (reset)
    (sgp :v nil :save-buffer-trace nil)
    (set-task 'count)
    (dotimes (i n)
      (init-task)
      (push (run 30.0) res1))
    (set-task 'semantic)
    (setf *trials* nil)
    (dotimes (i n)
      (init-task)
      (push (run 30.0) res2))
    (reset)
    (sgp :v nil :save-buffer-trace nil)
    (set-task 'semantic)
    (setf *trials* nil)
    (dotimes (i n)
      (init-task)
      (push (run 30.0) res3))
    (format t "~%Trial Count   Sem-transfer Sem-control")
    (dotimes (i n)
    (format t "~% ~3D   ~5,1F    ~5,1F        ~5,1F" (1+ i) (nth (- n i 1) res1)(nth (- n i 1) res2)(nth (- n i 1) res3)))))
  
(defun run-experiment (n)
  (do-it n))

(defun run-sample (&optional x)
  (case x
    (1 (print "Running count") (format t "~%") (test-count))
    (2 (print "Running semantic") (format t "~%") (test-semantic))
    (otherwise (print "1 - Count  2 - Semantic"))))
   

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
;;; Facts for semantic.

(p1 isa fact slot1 property slot2 canary slot3 bird)
(p2 isa fact slot1 property slot2 shark slot3 fish)
(p3 isa fact slot1 property slot2 bird slot3 animal)
(p4 isa fact slot1 property slot2 fish slot3 animal))


(add-instr count :input (Vstart Vend) :working-memory (WMcount) :declarative ((RTcount-fact RTfirst RTsecond))
:pm-function do-action
:init init-count
:reward 10.0
:parameters ((sgp :lf 0.15 :egs 0.2 :ans 0.1 :rt -0.5  :alpha 0.2))  

  (ins :condition (Vstart<>nil WMcount=nil) :action (Vstart->WMcount  (say WMcount)->AC  (count-fact WMcount)->RT) :description "Initialize Count")
  (ins :condition (Vend<>RTsecond  WMcount=RTfirst) :action (RTsecond->WMcount  (say WMcount)->AC (count-fact WMcount)->RT) :description "Counting Step")
  (ins :condition (Vend=RTsecond) :action ((answer RTsecond)->AC finish->Gtask) :description "Finalize count"))

(add-instr semantic :input (Vanimal Vcategory) :working-memory (WMcurrent) :declarative ((RTprop RTitem RTmember-of))
:pm-function do-action
:init init-semantic
:reward 10.0
:parameters ((sgp :lf 0.15 :egs 0.2 :ans 0.1 :rt -0.5  :alpha 0.2))  

  (ins :condition (Vanimal<>nil WMcurrent = nil) :action (Vanimal->WMcurrent (say WMcurrent)->AC  (property WMcurrent)->RT) :description "Retrieve first category")
  (ins :condition (Vcategory<>RTmember-of  WMcurrent = RTitem) :action (RTmember-of->WMcurrent  (say WMcurrent)->AC (property WMcurrent)->RT) :description "Chain up")
  (ins :condition (Vcategory = RTmember-of) :action ((answer yes)->AC finish->Gtask) :description "Match found")
  (ins :condition (RTprop = error) :action ((answer no)->AC finish->Gtask) :description "No Match"))


)

