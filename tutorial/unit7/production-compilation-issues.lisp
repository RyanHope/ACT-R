(clear-all)

(defvar *val1*)
(defvar *val2*)
(defvar *responses*)
(defvar *start-time*)
(defvar *times*)
(defvar *exp-length*)
(defvar *task-over*)

(defparameter *model* t)
(defparameter *result-matrix* (make-array '(4 4 4) :initial-contents '((("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose"))
                                                                       (("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose"))
                                                                       (("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose"))
                                                                       (("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose")))))

(defun convert-key-to-index (key)
  (case key
    ((#\s #\S) 0)
    ((#\d #\D) 1)
    ((#\f #\F) 2)
    (t 3)))

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (unless *task-over*
    (if (eq key #\space)
        (if (= *exp-length* (length *responses*))
            (setf *task-over* t)
          (present-next-trial))
      (let* ((response-index (convert-key-to-index key))
             (result (aref *result-matrix* *val1* *val2* response-index)))
        (push result *responses*)
        (push (- (get-time *model*) *start-time*) *times*)
        (present-feedback result)))))

(defun present-feedback (result)
  (clear-exp-window)
  (add-text-to-exp-window :x 100 :y 100 :text result)
  (when *model* 
    (schedule-event-relative 0 'proc-display)))
        
(defun present-next-trial ()
  (clear-exp-window)
  (setf *val1* (act-r-random 4))
  (setf *val2* (act-r-random 4))
  (add-text-to-exp-window :x 35 :y 50 :text (princ-to-string *val1*) :width 30)
  (add-text-to-exp-window :x 70 :y 50 :text (princ-to-string *val2*) :width 30)
  (when *model* 
    (schedule-event-relative 0 'proc-display))
  (setf *start-time* (get-time *model*)))


(defun choice-game-trials (&key (n 200) (reset t) (output t))
  (when reset (reset))
  (let ((window (open-exp-window "Compilation task" :visible (not *model*))))
    (setf *times* nil)
    (setf *responses* nil)
    (setf *task-over* nil)
    (setf *exp-length* n)
    (present-next-trial)
    (if *model*
        (progn
          (install-device window)
          (run-until-condition (lambda () *task-over*)))
      (while (not *task-over*)
        (allow-event-manager window)))
    
    (analyze-results output)))

(defun choice-game-experiment (n &optional show-games)
  (let ((scores (make-list 20 :initial-element 0))
        (times (make-list 20 :initial-element 0)))
    (dotimes (i n)
      (let ((result (choice-game-trials :output show-games)))
        (setf scores (mapcar '+ scores (first result)))
        (setf times (mapcar '+ times (second result)))))
      (format t "~%Average Score of ~d trials~%" n)
      (format t "~{~8,3f ~}" (mapcar (lambda (x) (/ x n)) scores))
      (format t "~%Average Response times~%")
      (format t "~{~8,3f ~}" (mapcar (lambda (x) (ms-round (ms->seconds (/ x n)))) times))))



  
(defun analyze-results (output)
  (let ((data (list nil nil)))
  (format output "Score~%")
  
  (setf *responses* (reverse *responses*))
  (while *responses*
    (let ((sum 0)
          (count 0))
      (dotimes (i (min (length *responses*) 10))
        (let ((result (pop *responses*)))
          (when (string-equal result "win") (incf sum))
          (when (string-equal result "lose") (decf sum)))
        (incf count))
      (format output "  ~2d(~2d)" sum count)
      (push-last sum (first data))))
  (format output "~%Average response times~%")
  (setf *times* (reverse *times*))
  (while *times*
    (let ((sum 0)
          (count 0))
      (dotimes (i (min (length *times*) 10))
        (incf sum (pop *times*))
        (incf count))
      (format output "~8,3f" (ms-round (ms->seconds (/ sum count))))
      (push-last (/ sum count) (second data))))
    (format output "~%")
    data))


(define-model compilation-test
   (sgp :esc t :lf .5 :bll .5 :mp 18 :rt -3 :ans .25)
  
  (chunk-type task state)
  (chunk-type number visual-rep)
  (chunk-type response key)
  (chunk-type trial num1 num2 result response)
         
  (define-chunks (win isa chunk)
    (draw isa chunk)
    (lose isa chunk)
    (attend-num-1 isa chunk)
    (encode-num-1 isa chunk)
    (find-num-2 isa chunk)
    (attend-num-2 isa chunk)
    (encode-num-2 isa chunk)
    (num2 isa chunk)
    (retrieve-past-trial isa chunk)
    (process-past-trial isa chunk)
    (respond isa chunk)
    (guess-other isa chunk)
    (detect-feedback isa chunk)
    (encode-feedback isa chunk))
  
  (add-dm (zero isa number visual-rep "0")
          (one isa number visual-rep "1")
          (two isa number visual-rep "2")
          (three isa number visual-rep "3"))
          
  (add-dm (response-1 isa response key "s")
          (response-2 isa response key "d")
          (response-3 isa response key "f"))

  
  (set-similarities (win draw -.2) (win lose -.15)
                    (zero one -.1) (one two -.1) (two three -.1) 
                    (zero two -.2) (one three -.2) 
                    (zero three -.3))
  
  (set-all-base-levels 1000 -100)
  
  (goal-focus-fct (car (define-chunks (isa task))))
  
  (p detect-trial-start
     =goal>
       isa task
       state nil
     =visual-location>
       isa visual-location
     ?visual>
       state free
     ==>
     =goal>
       state attend-num-1
     +imaginal>
       isa trial
     +visual>
       isa move-attention
       screen-pos =visual-location)
  
  (p attend-num-1
     =goal>
       isa task
       state attend-num-1
     =visual>
       isa visual-object
       value =val
     ==>
     =goal>
       state encode-num-1
     +retrieval>
       isa number 
       visual-rep =val)
  
  (p encode-num-1
     =goal>
       isa task
       state encode-num-1
     =retrieval>
       isa number
     =imaginal>
       isa trial
     ==>
     =imaginal>
       num1 =retrieval
     =goal>
       state find-num-2
     +visual-location>
       isa visual-location
       > screen-x current
       :attended nil)
  
  (p find-num-2
     =goal>
       isa task
       state find-num-2
     =visual-location>
       isa visual-location
     ==>
     =goal>
       state attend-num-2
     +visual>
       isa move-attention
       screen-pos =visual-location)
  
  (p attend-num-2
     =goal>
       isa task
       state attend-num-2
     =visual>
       isa visual-object
       value =val
     ==>
     =goal>
       state encode-num-2
     +retrieval>
       isa number 
       visual-rep =val)
  
  (p encode-num-2
     =goal>
       isa task
       state encode-num-2
     =retrieval>
       isa number
     =imaginal>
       isa trial
     ==>
     =imaginal>
       num2 =retrieval
     =goal>
       state retrieve-past-trial)
  
  
  (p retrieve-past-trial
     =goal>
       isa task
       state retrieve-past-trial
     =imaginal>
       isa trial
       num1 =n1
       num2 =n2
     ==>
     =imaginal>
     +retrieval>
       isa trial
       num1 =n1
       num2 =n2
       result win
     =goal>
       state process-past-trial)
  
  (p no-past-trial
     =goal>
       isa task 
       state process-past-trial
     ?retrieval>
       state error
     ==>
     +retrieval>
       isa response
     =goal>
       state respond)
  
  (p retrieved-a-win
     =goal>
       isa task
       state process-past-trial
     =retrieval>
       isa trial
       result win
       response =response
     ==>
     +retrieval> =response
     =goal>
       state respond)
  
  (p retrieved-a-non-win
     =goal>
       isa task
       state process-past-trial
     =retrieval>
       isa trial
      - result win
       response =response
     ==>
     +retrieval> =response
     =goal>
       state guess-other)
  
  (p guess-other
     =goal>
       isa task
       state guess-other
     =retrieval>
       isa response
       key =key
     ==>
     +retrieval>
       isa response
      - key =key
     =goal>
       state respond)
  
  (p respond
     =goal>
       isa task
       state respond
     =retrieval>
       isa response
       key =key
     ?manual>
       state free
     =imaginal>
       isa trial     
     ==>
     =imaginal>
       response =retrieval
     +manual>
       isa press-key
       key =key
     =goal>
       state detect-feedback)
  
  (p respond-when-response-failure
     =goal>
       isa task
       state respond
     ?retrieval>
       state error
     ?manual>
       state free
     =imaginal>
       isa trial
     ==>
     =imaginal>
       response response-2
     +manual>
       isa press-key
       key "d"
     =goal>
       state detect-feedback)
  
  (p detect-feedback
     =goal>
       isa task
       state detect-feedback
     =visual-location>
       isa visual-location
     ?visual>
       state free
     ==>
     +visual>
       isa move-attention
       screen-pos =visual-location
     =goal>
       state encode-feedback)
  
  (p encode-feedback-win
     =goal>
       isa task 
       state encode-feedback
     =imaginal>
       isa trial
     =visual>
       isa visual-object
       value "win"
     ?manual>
       state free
     ==>
     =imaginal>
       result win
     +manual>
       isa press-key
       key space
     +goal>
       isa task)
     
  (p encode-feedback-lose
     =goal>
       isa task 
       state encode-feedback
     =imaginal>
       isa trial
     =visual>
       isa visual-object
       value "lose"
     ?manual>
       state free
     ==>
     =imaginal>
       result lose
     +manual>
       isa press-key
       key space
     +goal>
       isa task)    
  
    (p encode-feedback-draw
     =goal>
       isa task 
       state encode-feedback
     =imaginal>
       isa trial
     =visual>
       isa visual-object
       value "draw"
     ?manual>
       state free
     ==>
     =imaginal>
       result draw
     +manual>
       isa press-key
       key space
     +goal>
       isa task)
)
