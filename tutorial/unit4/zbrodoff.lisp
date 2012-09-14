(defvar *trials*)
(defvar *results*)
(defvar *start-time*)
(defvar *block*)

(defvar *zbrodoff-control-data* '(1.84 2.46 2.82 1.21 1.45 1.42 1.14 1.21 1.17))

(defparameter *run-model* t)

(defstruct trial block addend1 addend2 sum answer visible)
(defstruct response block addend correct time)

(defun present-trial (trial &optional (new-window t))
  (let ((window (if new-window
                    (open-exp-window "Alpha-arithmetic Experiment" :visible (trial-visible trial))
                  nil)))
    
    (unless new-window
      (clear-exp-window))
    
    (add-text-to-exp-window :text (trial-addend1 trial) :x 100 :y 150 :width 25)
    (add-text-to-exp-window :text "+" :x 125 :y 150 :width 25)
    (add-text-to-exp-window :text (trial-addend2 trial) :x 150 :y 150 :width 25)
    (add-text-to-exp-window :text "=" :x 175 :y 150 :width 25)
    (add-text-to-exp-window :text (trial-sum trial) :x 200 :y 150 :width 25)
    
    (when new-window
      (install-device window))
    
    (proc-display :clear t)    
    
    (setf *start-time* (get-time *run-model*))
    
    window))


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (let ((trial (pop *trials*)))
    (push (make-response :block (trial-block trial)
                         :addend (trial-addend2 trial)
                         :time (/ (- (get-time *run-model*) *start-time*) 1000.0)
                         :correct (string-equal (trial-answer trial) (string key)))
          *results*)
    (when *trials*
      (present-trial (first *trials*) nil))))


(defun collect-responses (trial-count)
  (setf *results* nil)
  (let ((window (present-trial (first *trials*))))
    (if *run-model* 
        (run (* 10 trial-count))        
      (while (< (length *results*) trial-count)
        (allow-event-manager window)))))


(defun zbrodoff-trial (addend1 addend2 sum answer &optional (visible (not *run-model*)))
  
  (setf *block* 1)
  (setf *trials* (list (construct-trial (list addend1 addend2 sum answer) visible)))
  (collect-responses 1)
  (analyze-results))


(defun zbrodoff-set (&optional (visible (not *run-model*)))
  
  (setf *block* 1)
  (setf *trials* (create-set visible))
  (collect-responses 24)
  (analyze-results))


(defun zbrodoff-block (&optional (visible (not *run-model*)))
  (setf *block* 1)
  (setf *trials* nil)
  (dotimes (i 8)
    (setf *trials* (append *trials* (create-set visible))))
  (collect-responses 192)
  (analyze-results))

(defun zbrodoff-experiment (&optional (visible (not *run-model*)) (show-results t))
  (reset)
  (setf *trials* nil)
  (dotimes (j 3)
    (setf *block* (+ j 1))
    (dotimes (i 8)
      (setf *trials* (append *trials* (create-set visible)))))
  (collect-responses 576)
  (analyze-results show-results))

(defun zbrodoff (n)
  (let ((results nil))
    (dotimes (i n)
      (push (zbrodoff-experiment nil nil) results))
    
    (let ((rts (mapcar #'(lambda (x) (/ x (length results)))
                 (apply #'mapcar #'+ (mapcar #'first results))))
          (counts (mapcar #'(lambda (x) (truncate x (length results)))
                    (apply #'mapcar #'+ (mapcar #'second results)))))
      
      (correlation rts *zbrodoff-control-data*)
      (mean-deviation rts *zbrodoff-control-data*)
      
      (print-analysis rts counts '(1 2 3) '("2" "3" "4") '(64 64 64)))))

        
(defun analyze-results (&optional (display t))
  (let ((blocks (sort (remove-duplicates (mapcar #'response-block *results*)) #'<))
        (addends (sort (remove-duplicates (mapcar #'response-addend *results*) :test #'string-equal) #'string<))
        (counts nil)
        (rts nil)
        (total-counts nil))
    
    (setf total-counts (mapcar #'(lambda (x) 
                                   (/ (count x *results* 
                                             :key #'response-addend 
                                             :test #'string=)
                                      (length blocks)))
                         addends))
    
    (dolist (x blocks)
      (dolist (y addends)
        (let ((data (mapcar #'response-time
                      (remove-if-not #'(lambda (z)
                                         (and (response-correct z)
                                              (string= y (response-addend z))
                                              (= x (response-block z))))
                                     *results*))))
          (push (length data) counts)
          (push (/ (apply #'+ data) (max 1 (length data))) rts))))
    
    
    (when display
      (print-analysis (reverse rts) (reverse counts) blocks addends total-counts))
      
    (list (reverse rts) (reverse counts))))

    
(defun print-analysis (rts counts blocks addends total-counts)
  (format t "~%        ")
  (dotimes (addend (length addends))
    (format t " ~6@a (~2d)" (nth addend addends) (nth addend total-counts)))
  (dotimes (block (length blocks))
    (format t "~%Block ~2d" (nth block blocks))
    (dotimes (addend (length addends))
      (format t " ~6,3f (~2d)" (nth (+ addend (* block (length addends))) rts)
        (nth (+ addend (* block (length addends))) counts))))
  (terpri))
        
  
(defun create-set (visible)
  (permute-list (mapcar (lambda (x) (construct-trial x visible)) 
                  '(("a" "2" "c" "k")("d" "2" "f" "k")
                    ("b" "3" "e" "k")("e" "3" "h" "k")
                    ("c" "4" "g" "k")("f" "4" "j" "k")
                    ("a" "2" "d" "d")("d" "2" "g" "d")
                    ("b" "3" "f" "d")("e" "3" "i" "d")
                    ("c" "4" "h" "d")("f" "4" "k" "d")
                    ("a" "2" "c" "k")("d" "2" "f" "k")
                    ("b" "3" "e" "k")("e" "3" "h" "k")
                    ("c" "4" "g" "k")("f" "4" "j" "k")
                    ("a" "2" "d" "d")("d" "2" "g" "d")
                    ("b" "3" "f" "d")("e" "3" "i" "d")
                    ("c" "4" "h" "d")("f" "4" "k" "d")))))

(defun construct-trial (trial visible)
  (make-trial :block *block* 
              :addend1 (first trial)
              :addend2 (second trial)
              :sum (third trial)
              :answer (fourth trial)
              :visible visible))
  

(clear-all)

(define-model zbrodoff
    
(sgp :v nil :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)

(sgp :show-focus t)

(chunk-type problem arg1 arg2 result)
(chunk-type goal state count target)
(chunk-type sequence identity next)

(add-dm
 (zero  ISA sequence identity "0" next "1")
 (one   ISA sequence identity "1" next "2")
 (two   ISA sequence identity "2" next "3")
 (three ISA sequence identity "3" next "4")
 (four  ISA sequence identity "4" next "5")
 (a ISA sequence identity "a" next "b")
 (b ISA sequence identity "b" next "c")
 (c ISA sequence identity "c" next "d")
 (d ISA sequence identity "d" next "e")
 (e ISA sequence identity "e" next "f")
 (f ISA sequence identity "f" next "g")
 (g ISA sequence identity "g" next "h")
 (h ISA sequence identity "h" next "i")
 (i ISA sequence identity "i" next "j")
 (j ISA sequence identity "j" next "k")
 (goal isa goal))

(P attend
   =goal>
      ISA         goal
      state       nil
   =visual-location>
      ISA         visual-location
   ?visual>
       state      free
==>
   =goal>
      state       attending
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
)

(P read-first
   =goal>
     ISA         goal
     state       attending
   =visual>
     ISA         text
     value       =char
     status      nil
   ?vocal>
     state      free
   ?imaginal>
     buffer     empty
     state      free   
==>
   +vocal>
     isa         subvocalize
     string      =char
   +imaginal>
     isa         problem 
     arg1        =char
   =goal>
     state       nil
   +visual-location>
     ISA         visual-location
   > screen-x    150 
   < screen-x    160
)


(P read-second
   =goal>
     ISA         goal
     state       attending
   =visual>
     ISA         text
     value       =char
     status      nil
   =imaginal>
     isa         problem
     arg2        nil
   ?vocal>
       state      free
==>
   +vocal>
     isa         subvocalize
     string      =char
   =imaginal>
     arg2       =char
   =goal>
     state       nil
   +visual-location>
     ISA         visual-location
   > screen-x    200 
   < screen-x    210
)


(P read-third
   =goal>
     ISA         goal
     state       attending
   =imaginal>
     isa         problem
     arg1        =arg1
     arg2        =arg2
   =visual>
     ISA         text
     value       =char
     status      nil
   ?vocal>
       state      free
==>
   =imaginal>
   +vocal>
     isa         subvocalize
     string      =char
   =goal>
     target      =char
     state       count
   +visual>
     isa         clear
)


(P start-counting
   =goal>
     ISA         goal
     state       count
   
   =imaginal>
     isa         problem
     arg1        =a
     arg2        =val

   ?vocal>
     state      free
==>
   +vocal>
     isa         subvocalize
     string      =a
   =imaginal>
     result      =a
   =goal>
     count       "0"
     state       counting
   +retrieval>
     ISA         sequence
     identity    =a
)

(P update-result
   =goal>
     ISA         goal
     count       =val
   =imaginal>
     isa         problem
     result      =let
   - arg2        =val
   =retrieval>
     ISA         sequence
     identity    =let
     next        =new
   ?vocal>
      state      free
==>
   +vocal>
     isa         subvocalize
     string      =new
   =imaginal>
     result      =new
   +retrieval>
     ISA         sequence
     identity    =val
)

(P update-count
   =goal>
     ISA         goal
     count       =val
   =imaginal>
     isa         problem
     result      =ans
   =retrieval>
     ISA         sequence
     identity    =val
     next        =new
   ?vocal>
     state       free
==>
   +vocal>
     isa         subvocalize
     string      =new
   =imaginal>
   =goal>
     count       =new
   +retrieval>
     ISA         sequence
     identity    =ans
)


(P final-answer-yes
   =goal>
     ISA         goal
     target      =let
     count       =val
   =imaginal>
     isa         problem
     result      =let
     arg2        =val
   ?vocal>
     state       free
   
   ?manual>
     state       free
   ==>
   +goal>
     isa goal
   +manual>
     ISA         press-key
     key         "k"
   
)

(P final-answer-no
    =goal>
     ISA         goal
     count       =val
     target      =let
   =imaginal>
     isa         problem
   - result      =let
     arg2        =val
   ?vocal>
     state       free
   
   ?manual>
     state       free
   ==>
   +goal>
     isa goal
   +manual>
     ISA         press-key
     key         "d"
   
)

(set-all-base-levels 100000 -1000)
(goal-focus goal)
)
