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
  (if (not (eq who 'human))
      (do-experiment-model size trials)
    (do-experiment-person size trials)))

(defun do-experiment-model (size trials)
  (let ((result nil)
        (window (open-exp-window "Paired-Associate Experiment" :visible nil)))
    
    (setf *model-doing-task* t)
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
    
    (setf *model-doing-task* nil)
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
                (mapcar (lambda (lis1 lis2)
                          (list (+ (first lis1) (first lis2))
                                (+ (or (second lis1) 0) (or (second lis2) 0))))
                  results (paired-task 20 8))))
      ((equal count n) 
       (output-data results n))))

(defun output-data (data n)
  (let ((probability (mapcar (lambda (x) (/ (first x) n)) data))
        (latency (mapcar (lambda (x) (/ (or (second x) 0) n)) data)))
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

(define-model paired-learning-dynamic
    
(sgp :esc t :v nil :pct t :ul t :epl t :rt -1.7 :lf 0.2 :ans 0.4 :egs 0.1 :bll 0.5 :trace-detail low :iu 5 :alpha 0.2)
(sgp :do-not-harvest imaginal)

(chunk-type operator pre action arg1 arg2 post label required slot success failure)
(chunk-type task state step context)

(add-dm
 (op1 isa operator pre start         action read                           label   word    post stimulus-read)
 (op2 isa operator pre stimulus-read action retrieve      required word    label   number  post recalled)
 (op3 isa operator pre recalled      slot   number        success  respond failure wait)
 (op4 isa operator pre respond       action type          required number                  post wait)
 (op5 isa operator pre wait          action read                           label   number  post new-trial)
 (op6 isa operator pre new-trial     action complete-task                                  post start))

(set-all-base-levels 1000)

(goal-focus-fct (car (define-chunks (isa task state start step ready))))

(p retrieve-operator
   =goal>           
     isa     task
     state   =state
     step    ready
  ==>
   +retrieval>
     isa     operator
     pre     =state
   =goal>
     step    retrieving-operator
     context nil)

(p read-and-create
   =goal>
     isa       task
     step      retrieving-operator
   =retrieval>
     isa       operator
     action    read
     label     =destination
     post      =state
   =visual-location>
     isa       visual-location
   ?visual>
     state     free
   ?imaginal>
     buffer    empty
     state     free
  ==>
   +imaginal>
   +visual>
     isa        move-attention
     screen-pos =visual-location
   =goal>
    context    =destination
    step       process
    state      =state)

(p read
   =goal>
     isa        task
     step       retrieving-operator
   =retrieval>
     isa        operator
     action     read
     label      =destination
     post       =state
   =visual-location>
     isa        visual-location
   ?visual>
     state      free
   ?imaginal>
     buffer     full
  ==>
   +visual>
     isa        move-attention
     screen-pos =visual-location
   =goal>
     context    =destination
     step       process
     state      =state)

(p encode
   =goal>
     isa         task
     step        process
     context     =which-slot
   =visual>
     isa         text
     value       =val
   =imaginal>
   ?imaginal>
     state       free
  ==>
   *imaginal>
     =which-slot =val
   =goal>
     step        ready)

(p retireve-associate
   =goal>
     isa      task
     step     retrieving-operator
   =imaginal>
     =target  =stimulus
   =retrieval>
     isa      operator
     action   retrieve
     required =target
     label    =other
     post     =state
  ==>
   +retrieval>
     =target  =stimulus
   =goal>
     step     retrieving-result
     context  =other
     state    =state)

(p retrieval-unsuccessful
   =goal>
     isa      task
     step     retrieving-result
     context  =dest
   =imaginal>
   ?retrieval>
     buffer   failure
   ?imaginal>
     state    free
  ==>
   *imaginal>
     =dest    nil
   =goal>
     step     ready)

(p retrieval-successful
   =goal>
     isa      task
     step     retrieving-result
     context  =dest
   =imaginal>
   =retrieval>
     =dest    =val
   ?imaginal>
     state    free
  ==>
   *imaginal>
     =dest    =val
   =goal>
     step     ready)

(p unsuccessful-test
   =goal>
     isa     task
     step    retrieving-operator
   =imaginal>
     =dest   nil
   =retrieval>
     isa     operator
     slot    =dest
     failure =state
   ?imaginal>
     state   free
  ==>
   =goal>
     state   =state
     step    ready)

(p successful-test
   =goal>
     isa     task
     step    retrieving-operator
   =imaginal>
     =dest   =val
   =retrieval>
     isa     operator
     slot    =dest
     success =state
   ?imaginal>
     state   free
  ==>
   =goal>
     state   =state
     step    ready)

(p type
   =goal>
     isa      task
     step     retrieving-operator
   =imaginal>
     =slot    =val
   =retrieval>
     isa      operator
     action   type
     required =slot
     post     =state
   ?manual>
     state    free
  ==>
   +manual>
     cmd      press-key
     key      =val
   =goal>
     state    =state
     step     ready)

(p complete-task
   =goal>
     isa    task
     step   retrieving-operator
   =retrieval>
     isa    operator
     action complete-task
     post   =state
   ?imaginal>
     state  free
  ==>
   -imaginal>
   +goal>
     isa    task
     state  =state
     step   ready)

(spp complete-task :reward 20)
)
