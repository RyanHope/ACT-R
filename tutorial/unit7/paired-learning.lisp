(defvar *response* nil)
(defvar *response-time* nil)
(defvar *model-doing-task* nil)

(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

(defvar *paired-latencies* '(0.0 2.158 1.967 1.762 1.680 1.552 1.467 1.402))
(defvar *paired-probability* '(0.000 .526 .667 .798 .887 .924 .958 .954))

(defun paired-task (size trials &optional who)
  
  (if (eq who 'human)
      (setf *model-doing-task* nil)
    (setf *model-doing-task* t))
  
  (if (not (eq who 'human))
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
          
          (setf start-time (get-time nil)) 
          (while (< (- (get-time nil) start-time) 5000)
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

(defun paired-experiment (n)
  (do ((count 1 (1+ count))
       (results (paired-task 20 8)
                (mapcar #'(lambda (lis1 lis2)
                            (list (+ (first lis1) (first lis2))
                                  (+ (or (second lis1) 0) (or (second lis2) 0))))
                  results (paired-task 20 8))))
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
  (setf *response-time* (get-time *model-doing-task*)))


(clear-all)

(define-model paired-learning
    
(sgp :esc t :v nil :pct nil :ul t :epl t :rt -1.7 :lf 0.4 :ans 0.5 :egs 0.1 :bll 0.5 :trace-detail low :iu 10 :alpha 0.2 :ncnar nil)

(chunk-type operator pre action arg1 arg2 post)
(chunk-type task state step)
(chunk-type args arg1 arg2)

(add-dm
  (goal isa task state start step ready)
  (op1 isa operator pre start action read arg1 create post stimulus-read)
  (op2 isa operator pre stimulus-read action associate arg1 filled arg2 fill post recalled)
  (op3 isa operator pre recalled action test-arg2 arg1 respond arg2 wait)
  (op4 isa operator pre respond action type arg2 response post wait)
  (op5 isa operator pre wait action read arg2 fill post new-trial)
  (op6 isa operator pre new-trial action complete-task post start))

(set-all-base-levels 1000)

(p retrieve-operator
    =goal>           
       isa   task
       state =state
       step  ready
==>
    +retrieval>
       isa   operator
       pre   =state
    =goal>
       step  retrieving-operator
)

(p read-arg1
    =goal>
        isa       task
        step      retrieving-operator
    =retrieval>
        isa       operator
        action    read
        arg1      create
        post      =state
     =visual-location>
        isa       visual-location
    ?visual>
        state     free
==>
    +imaginal>
        isa       args
        arg1      fill
    +visual>
        isa       move-attention
        screen-pos =visual-location
    =goal>
        step     attending
        state    =state
)

(p encode-arg1
     =goal>
       isa        task
       step       attending
     =visual>
       isa        text
       value      =val
     =imaginal>
       isa        args
       arg1       fill
  ==>
     =imaginal>
       arg1       =val
     =goal>
       step       ready
)

(p retireve-associate
    =goal>
       isa        task
       step       retrieving-operator
    =imaginal>
       isa        args
       arg1       =stimulus
    =retrieval>
       isa        operator
       action     associate
       arg1       filled
       arg2       fill
       post       =state
==>
    +retrieval>
       isa      args
       arg1     =stimulus
    =imaginal>
       arg2     fill
    =goal>
       step     retrieving-response
       state    =state
   )

(p retrieval-arg2-unsuccessful
    =goal>
       isa      task
       step     retrieving-response
    =imaginal>
       isa args
       arg2     fill
    ?retrieval>
       state    error
==>
    =imaginal>
       arg2     nil

    =goal>
       step     ready
)

(p retrieval-arg2-successful
    =goal>
       isa      task
       step     retrieving-response
    =imaginal>
       isa      args
       arg2     fill
    =retrieval>
       isa      args
       arg2     =val
==>
    =imaginal>
       arg2     =val
    =goal>
       step     ready
)

(p respond-to-retrieval-failure
    =goal>
       isa      task
       step     retrieving-operator
    =imaginal>
       isa      args
       arg2     nil
    =retrieval>
       isa      operator
       action   test-arg2
       arg2     =state
==>
    =imaginal>
    =goal>
       state    =state
       step     ready
)

(p respond-to-retrieval-success
    =goal>
        isa     task
        step    retrieving-operator
    =imaginal>
        isa     args
        arg2    =val
    =retrieval>
        isa     operator
        action  test-arg2
        arg1    =state
==>
    =imaginal>
    =goal>
       state    =state
       step     ready
)

(p type-arg2
    =goal>
        isa     task
        step    retrieving-operator
    =imaginal>
        isa     args
        arg2    =val
    =retrieval>
        isa     operator
        action  type
        arg2    response
        post    =state
    ?manual>
       state    free
==>
    =imaginal>
    +manual>
       isa      press-key
       key      =val
    =goal>
       state    =state
       step     ready
)

(p read-arg2
    =goal>
       isa     task
       step    retrieving-operator
    =imaginal>
       isa     args
    =retrieval>
       isa     operator
       action  read
       arg2    fill
       post    =state
    =visual-location>
       isa     visual-location
    ?visual>
       state   free
==>
    +visual>
       isa     move-attention
       screen-pos =visual-location
    =imaginal>
       arg2    fill
    =goal>
       step    attending
       state   =state 
)

(p encode-arg2
     =goal>
       isa      task
       step    attending
     =imaginal>
       isa args
       arg2     fill
     =visual>
       isa      text
       value    =val
==>
     =imaginal>
       arg2     =val
     =goal>
       step     ready
)

(p complete-task
    =goal>
       isa      task
       step     retrieving-operator
    =retrieval>
       isa      operator
       action   complete-task
       post     =state
==>
    +goal>
       isa task
       state    =state
       step     ready
)


(spp complete-task :reward 20)

(goal-focus goal)
)
