(defvar *response* nil)
(defvar *response-time* nil)

(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

(defconstant *paired-latencies* '(0.0 2.158 1.967 1.762 1.680 1.552 1.467 1.402))
(defconstant *paired-probability* '(0.000 .526 .667 .798 .887 .924 .958 .954))

(defun do-experiment (size trials)
  (if *actr-enabled-p*
      (do-experiment-model size trials)
    (do-experiment-person size trials)))

(defun do-experiment-model (size trials)
  (let ((result nil)
        (window (open-exp-window "Paired-Associate Experiment" :visible nil)))
    
    (reset) 
    
    (install-device window)
    
    (dotimes (i trials) 
      (let ((score 0.0)
            (time 0.0)
            (start-time))
        (dolist (x (permute-list (subseq *pairs* (- 20 size)))) 
          
          (clear-exp-window)
          (add-text-to-exp-window :text (car x) :x 150 :y 150)
        
          (setf *response* nil)                   
          (setf *response-time* nil)
          (setf start-time (get-time))
          
          (proc-display)
          (run-full-time 5)
          
          (when (equal (second x) *response*)      
            (incf score 1.0)    
            (incf time (- *response-time* start-time))) 
        
          (clear-exp-window)
          (add-text-to-exp-window :text (second x) :x 150 :y 150)
          
          (proc-display)
          (run-full-time 5))
        
        (push (list (/ score size) (and (> score 0) (/ time (* score 1000.0)))) result)))
    
    (reverse result)))
  
(defun do-experiment-person (size trials)
  (let ((result nil)
        (window (open-exp-window "Paired-Associate Experiment" :visible t)))
    
    (dotimes (i trials) 
      (let ((score 0.0)
            (time 0.0)
            (start-time))
        (dolist (x (permute-list (subseq *pairs* (- 20 size)))) 
          
          (clear-exp-window)
          (add-text-to-exp-window :text (car x) :x 150 :y 150 :width 50)
          (setf *response* nil)                   
          (setf *response-time* nil)
          
          (setf start-time (get-time)) 
          (while (< (- (get-time) start-time) 5000)
                 (allow-event-manager window)) 
          
          (when (equal (second x) *response*)
            (incf score 1.0) 
            (incf time (/ (- *response-time* start-time) 1000.0))) 
        
          (clear-exp-window)
          (add-text-to-exp-window :text (second x) :x 150 :y 150)
          (sleep 5.0))
        
        (push (list (/ score size) (and (> score 0) (/ time score))) result)))
    
    ;; return the list of scores 
    (reverse result)))

(defun collect-data (n)
  (do ((count 1 (1+ count))
       (results (do-experiment 20 8)
                (mapcar #'(lambda (lis1 lis2)
                            (list (+ (first lis1) (first lis2))
                                  (+ (or (second lis1) 0) (or (second lis2) 0))))
                  results (do-experiment 20 8))))
      ((equal count n) 
       (output-data results n))))

(defun output-data (data n)
   (let ((probability (mapcar #'(lambda (x) (/ (first x) n)) data))
        (latency (mapcar #'(lambda (x) (/ (or (second x) 0) n)) data)))
    (print-results latency *paired-latencies* "Latency")
     (print-results probability *paired-probability* "Accuracy")))
    
(defun print-results (predicted data label)
 (format t "~%~%~A:~%" label)
  (correlation predicted data)
  (mean-deviation predicted data)
  (format t "Trial    1       2       3       4       5       6       7       8~%")
  (format t "     ~{~8,3f~}~%" predicted))


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response* (string-upcase (string key)))
  (setf *response-time* (get-time)))


(clear-all)

(define-model paired
    
(sgp :esc t :v nil :pct t :pl t :epl t :rt -1.7 :lf 0.4 :ans 0.5 :egs 0.1 :bll 0.5 :trace-detail low :alpha .2) 

(chunk-type operator pre action arg1 arg2 post)
(chunk-type task state step arg1 arg2)

(add-dm
 (goal isa task state start step ready)
 (op1 isa operator pre start action read arg1 fill post stimulus-read)
 (op2 isa operator pre stimulus-read action associate arg1 filled arg2 fill post recalled)
 (op3 isa operator pre recalled action test-arg2 arg1 respond arg2 wait)
 (op4 isa operator pre respond action type arg2 response post wait)
 (op5 isa operator pre wait action read arg2 fill post new-trial)
 (op6 isa operator pre new-trial action complete-task post start))

(set-all-base-levels 1000)


(p retrieve-operator
   =goal>
       isa task 
       state =state
       step ready
==>
   +retrieval>
       isa operator
       pre =state
    =goal>
       step retrieving-operator)

(p read-arg1
   =goal>
       isa task
       step retrieving-operator
   =retrieval>
       isa operator
       action read
       arg1 fill
       post =state
    =visual-location>
      isa      visual-location
   ?visual>
      state     free
==>
    +visual>               
      isa      move-attention
      screen-pos =visual-location
    =goal>
       step    attending
       arg1 fill
       state =state)

(p encode-arg1
    =goal>
      isa      task
      step    attending
      arg1     fill
    =visual>
      isa      text
      value    =val
==>
    =goal>
      arg1     =val
      step     ready
)

(p retireve-associate
   =goal>
       isa task
       step retrieving-operator
       arg1 =stimulus
   =retrieval>
       isa operator
       action associate
       arg1 filled
       arg2 fill
       post =state
==>
    +retrieval>               
      isa      task
      arg1     =stimulus
    =goal>
       step    retrieving-response
       arg2 fill
       state =state)

(p retrieval-arg2-unsuccessful
    =goal>
      isa      task
      step     retrieving-response
      arg2     fill
   ?retrieval>
      state   error
==>
    =goal>
      arg2     nil
      step     ready
)

(p retrieval-arg2-successful
    =goal>
      isa      task
      step     retrieving-response
      arg2     fill
   =retrieval>
      isa task
      arg2 =val
==>
    =goal>
     arg2     =val
      step     ready
)

(p respond-to-retrieval-failure
   =goal>
       isa task
       step retrieving-operator
       arg2 nil
   =retrieval>
       isa operator
       action test-arg2
       arg2 =state
==>
    =goal>
      state    =state
      step     ready
)

(p respond-to-retrieval-success
   =goal>
       isa task
       step retrieving-operator
       arg2 =val
   =retrieval>
       isa operator
       action test-arg2
       arg1 =state
==>
    =goal>
      state    =state
      step     ready
)

(p type-arg2
   =goal>
       isa task
       step retrieving-operator
       arg2 =val
   =retrieval>
       isa operator
       action type
       arg2 response
       post =state
   ?manual>   
      state    free
==>
    +manual>              
      isa      press-key     
      key      =val
    =goal>
      state    =state
      step     ready
)

(p read-arg2
   =goal>
       isa task
       step retrieving-operator
   =retrieval>
       isa operator
       action read
       arg2 fill
       post =state
    =visual-location>
      isa      visual-location
   ?visual>
      state     free
==>
    +visual>               
      isa      move-attention
      screen-pos =visual-location
    =goal>
       step    attending
       arg2 fill
       state =state)

(p encode-arg2
    =goal>
      isa      task
      step    attending
      arg2     fill
    =visual>
      isa      text
      value    =val
==>
    =goal>
      arg2     =val
      step     ready
)

(p complete-task
   =goal>
       isa task
       step retrieving-operator
   =retrieval>
       isa operator
       action complete-task
       post =state
==>
    +goal>
      isa task
      state    =state
      step     ready
)

(spp :successes 1000000 :failures 0 :efforts 10000000)
(spp complete-task :success t)
(setf *actr-enabled-p* t)

(goal-focus goal)
)