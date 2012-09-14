(defvar *stick-a*)
(defvar *stick-b*)
(defvar *stick-c*)
(defvar *target*)
(defvar *current-stick*)
(defvar *current-line*)
(defvar *done*)
(defvar *choice*)
(defvar *experiment-window* nil)

(defconstant *bst-exp-data* '(20.0 67.0 20.0 47.0 87.0 20.0 80.0 93.0 
                              83.0 13.0 29.0 27.0 80.0 73.0 53.0))

(defparameter *bst-stimuli* '((15  250  55  125)(10  155  22  101)
                              (14  200  37  112)(22  200  32  114)
                              (10  243  37  159)(22  175  40  73)
                              (15  250  49  137)(10  179  32  105)
                              (20  213  42  104)(14  237  51  116)
                              (12  149  30  72)
                              (14  237  51  121)(22  200  32  114)
                              (14  200  37  112)(15  250  55  125)))

(defun build-display (a b c target)
   (setf *experiment-window* (open-exp-window "Building Sticks Task"                                      
                                              :visible nil
                                              :width 600
                                              :height 400))
  
  (setf *stick-a* a)
  (setf *stick-b* b)
  (setf *stick-c* c)
  (setf *target* target)
  (setf *current-stick* 0)
  (setf *done* nil)
  (setf *choice* nil)
  (setf *current-line* nil)
  
  (add-button-to-exp-window :x 10 :y 25  :height 20 :width 20 :text "A"     :action #'button-a-pressed)
  (add-button-to-exp-window :x 10 :y 50  :height 20 :width 20 :text "B"     :action #'button-b-pressed)
  (add-button-to-exp-window :x 10 :y 75  :height 20 :width 20 :text "C"     :action #'button-c-pressed)
  (add-button-to-exp-window :x 10 :y 125 :height 20 :width 38 :text "Reset" :action #'reset-display)
  
  (add-line-to-exp-window (list 50 35)  (list (+ a 50) 35) :color 'black)
  (add-line-to-exp-window (list 50 60)  (list (+ b 50) 60) :color 'black)
  (add-line-to-exp-window (list 50 85)  (list (+ c 50) 85) :color 'black)
  (add-line-to-exp-window (list 50 110) (list (+ target 50) 110) :color 'green)
  
  (allow-event-manager *experiment-window*))


(defun button-a-pressed (button)
  (declare (ignore button))
  
  (unless *choice* (setf *choice* 'under))
  
  (unless *done*
    (if (> *current-stick* *target*)
        (setf *current-stick* (- *current-stick* *stick-a*))
      (setf *current-stick* (+ *current-stick* *stick-a*)))
    (update-current-line)))

(defun button-b-pressed (button)
  (declare (ignore button))
  
  (unless *choice* (setf *choice* 'over))
  
  (unless *done*
    (if (> *current-stick* *target*)
        (setf *current-stick* (- *current-stick* *stick-b*))
      (setf *current-stick* (+ *current-stick* *stick-b*)))
    (update-current-line)))

(defun button-c-pressed (button)
  (declare (ignore button))
  
  (unless *choice* (setf *choice* 'under))
  
  (unless *done*
    (if (> *current-stick* *target*)
        (setf *current-stick* (- *current-stick* *stick-c*))
      (setf *current-stick* (+ *current-stick* *stick-c*)))
    (update-current-line)))

(defun reset-display (button)
  (declare (ignore button))
  
  (unless *done*
    (setf *current-stick* 0)
  (update-current-line)))

(defun update-current-line ()

  (when *current-line* 
    (remove-items-from-exp-window *current-line*))
  
  (if (= *current-stick* *target*)
      (progn
        (setf *done* t)
        (setf *current-line* (add-line-to-exp-window (list 50 135) (list  (+ *target* 50) 135) :color 'blue))
        (add-text-to-exp-window :x 180 :y 200 :width 50 :text "Done"))
    (if (zerop *current-stick*)
          (setf *current-line* nil)
        (setf *current-line* (add-line-to-exp-window (list 50 135) (list (+ *current-stick* 50) 135) :color 'blue))))
  
  (allow-event-manager *experiment-window*)
  
  (when *actr-enabled-p* 
    (proc-display)))

(defun do-experiment (sticks)
   
  (apply #'build-display sticks)
  
  (if *actr-enabled-p* 
      (do-experiment-model)
    (do-experiment-person)))

(defun do-experiment-model ()

  (install-device *experiment-window*)
  (proc-display :clear t)
  
  (run 60))

(defun do-experiment-person ()   
  (while (not *done*) 
         (sleep .25))
  (sleep 1))

(defun do-set ()
  (let ((result nil))
    (when *actr-enabled-p* 
      (reset))  
    (dolist (stim *bst-stimuli*)
      (do-experiment stim)
      (push *choice* result))
    (reverse result)))

(defun collect-data (n)
  (let ((result (make-list (length *bst-stimuli*) :initial-element 0))
        (p-values (list '(decide-over 0) '(decide-under 0) '(force-over 0) '(force-under 0))))
    (dotimes (i n result)
      (setf result (mapcar #'+ result (mapcar #'(lambda (x) (if (equal x 'over) 1 0)) (do-set))))
      (setf p-values (mapcar #'(lambda (x) (list (car x) (+ (second x) (no-output (production-p-value (car x))))))
                       p-values)))
    
    (setf result (mapcar #'(lambda (x) (* 100.0 (/ x n))) result))
    
    (when (= (length result) (length *bst-exp-data*))
      (correlation result *bst-exp-data*)
      (mean-deviation result *bst-exp-data*))
    
    (format t "~%Trial ")
    
    (dotimes (i (length result))
      (format t "~8s" (1+ i)))
    
    (format t "~%  ~{~8,2f~}~%~%" result)
        
    (dolist (x p-values)
      (format t "~12s: ~6,4f~%" (car x) (/ (second x) n)))))

(defun production-p-value (prod)
  (caar (no-output (spp-fct (list prod :p)))))

(clear-all)

(define-model bst-learn
    
(sgp :v nil :esc t :pl t :egs 3 :ut -100
     :show-focus t :trace-detail low)
  
(chunk-type try-strategy strategy a-loc b-loc c-loc goal-loc length state over under)
  
(add-dm (goal isa try-strategy state start))
  
(p find-first-line
   =goal>
     isa      try-strategy
     state    start
   =visual-location>
     isa      visual-location
==>
   +visual-location>
     isa      visual-location
     :attended nil
     kind     line
     screen-y lowest
   =goal>
     state looking)  

   
(p attend-line
   =goal>
     isa      try-strategy
     state    looking
   =visual-location>
     isa      visual-location
   ?visual>
      state    free
   ==>
   =visual-location>
   
   =goal>
     state    attending
   +visual>               
      isa move-attention
      screen-pos =visual-location)

(p encode-line-a
   =goal>
     isa      try-strategy
     state    attending
     a-loc    nil
   =visual>
     isa      line
     screen-pos =pos
==>
   =goal>
     a-loc    =pos
     state    start)

(p encode-line-b
   =goal>
     isa      try-strategy
     state    attending
     a-loc    =a
     b-loc    nil
   =visual>
     isa      line
     screen-pos =pos
==>
   =goal>
     b-loc    =pos
     state    start)

(p encode-line-c
   =goal>
     isa      try-strategy
     state    attending
     b-loc    =b
     c-loc    nil
   =visual>
     isa      line
     screen-pos =pos
==>
   =goal>
     c-loc    =pos
     state    start)

(p encode-line-goal
   =goal>
     isa      try-strategy
     state    attending
     c-loc    =c
     goal-loc nil
   =visual>
     isa      line
     screen-pos =pos
     width    =length
   ?visual>
      state    free
   ==>
   =goal>
     goal-loc =pos
     length   =length
     state    encode-under
   +visual>
     isa      move-attention
     screen-pos =c)

(p encode-under
   =goal>
     isa      try-strategy
     state    encode-under
     b-loc    =b
     length   =goal-len
   =visual>
     isa      line
     width    =c-len
   ?visual>
      state    free
   ==>
   !bind! =val (- =goal-len =c-len)
   
   =goal>
     under =val
     state    encode-over
   +visual>
     isa      move-attention
     screen-pos =b)

(p encode-over
   =goal>
     isa      try-strategy
     state    encode-over
     length   =goal-len
   =visual>
     isa      line
     width    =b-len
==>
   !bind! =val (- =b-len =goal-len)
   
   =goal>
     over =val
     state    choose-strategy)

(p encode-line-current
   =goal>
     isa      try-strategy
     state    attending
     goal-loc =goal-loc
   =visual>
     isa      line
     width    =current-len
   ?visual>
      state    free
   ==>
   =goal>
     length   =current-len
     state    calculate-difference
   +visual> 
     isa      move-attention
     screen-pos =goal-loc)

(p calculate-difference
   =goal>
     isa      try-strategy
     state    calculate-difference
     length   =current-len
   =visual>
     isa      line
     width    =goal-len
   ==>
   !bind! =val (abs (- =current-len =goal-len))
   =goal>
     length =val
     state    consider-next)

(p check-for-done
   =goal>
     isa      try-strategy
     state    consider-next
     length   0
==>
   =goal>
     state    check-for-done
   +visual-location>
     isa      visual-location
    > screen-y 200)

(p find-done
   =goal>
     isa      try-strategy
     state    check-for-done
   =visual-location>
     isa      visual-location
   
   ?visual>
      state    free
   ==>
   +visual>
     isa      move-attention
     screen-pos =visual-location
   =goal>
     state    read-done)

(p read-done
   =goal>
     isa      try-strategy
     state    read-done
   =visual>
     isa      text
     value    "done"
==>
   +goal>
     isa      try-strategy
     state    start)

(p consider-c
   =goal>
     isa      try-strategy
     state    consider-next
     c-loc    =c-loc
   > length   0
   
   ?visual>
      state free
   ==>
   =goal>
     state    evaluate-c
   +visual> 
     isa     move-attention
     screen-pos =c-loc)
 
(p choose-c
   =goal>
     isa      try-strategy
     state    evaluate-c
     length   =difference
   =visual>
     isa      line
   <= width   =difference
==>
   =goal>
     state    prepare-mouse
   +visual-location>              
     isa      visual-location
     kind     oval
     screen-y 85)

(p consider-a
   =goal>
     isa      try-strategy
     a-loc    =a-loc
     state    evaluate-c
     length   =difference
   =visual>
     isa      line
   > width    =difference
   ?visual>
     state free
   ==>
   =goal>
     state    evaluate-a
   +visual> 
     isa      move-attention
     screen-pos =a-loc)

(p choose-a
   =goal>
     isa      try-strategy
     state    evaluate-a
     length   =difference
   =visual>
     isa      line
   <= width   =difference
==>
   =goal>
     state    prepare-mouse
   +visual-location>              
     isa      visual-location
     kind     oval
     screen-y 35)

(p reset
   =goal>
     isa      try-strategy
     state    evaluate-a
     length   =difference
   =visual>
     isa      line
   > width    =difference
==>
   =goal>
     state    prepare-mouse
   +visual-location>              
     isa      visual-location
     kind     oval
     screen-y 135)

(p decide-over
   =goal>
     isa      try-strategy
     state    choose-strategy
     strategy nil
     under    =under
     over     =over
   
   !eval! (< =over (- =under 25))
   
==>
   =goal>
     state    prepare-mouse
     strategy over
   +visual-location>              
     isa      visual-location
     kind     oval
     screen-y 60)

(p force-over
   =goal>
     isa      try-strategy
     state    choose-strategy
   - strategy over
==>
   =goal>
     state    prepare-mouse
     strategy over
   +visual-location>              
     isa      visual-location
     kind     oval
     screen-y 60)

(p decide-under
   =goal>
     isa      try-strategy
     state    choose-strategy
     strategy nil
     over     =over
     under    =under
   
   !eval! (< =under (- =over 25))
==>
   =goal>
     state    prepare-mouse
     strategy under
   +visual-location>              
     isa      visual-location
     kind     oval
     screen-y 85)

(p force-under
   =goal>
     isa      try-strategy
     state    choose-strategy
   - strategy under
==>
   =goal>
     state    prepare-mouse
     strategy under
   +visual-location>              
     isa      visual-location
     kind     oval
     screen-y 85)

(p move-mouse
   =goal>
     isa      try-strategy
     state    prepare-mouse
   =visual-location>
     isa      visual-location
   
   ?visual>
      state   free
   ?manual>
      state   free
   ==>
   =visual-location>
   
   +visual>
     isa      move-attention
     screen-pos =visual-location
   =goal>
     state    move-mouse
   +manual>              
     isa      move-cursor       
     loc      =visual-location
   )

(p click-mouse
   =goal>
     isa      try-strategy
     state    move-mouse
   ?manual>
      state    free
   
   ==>
   =goal>
     state    wait-for-click
   +manual>              
     isa      click-mouse)

(p look-for-current
   =goal>
     isa      try-strategy
     state    wait-for-click
   ?manual>
      state    free
   
   =visual-location> 
     isa      visual-location
   < screen-y 100
==>
   +visual-location>
     isa      visual-location
     :attended nil
     kind     line
     screen-y highest
   =goal>
     state    looking)  

(p pick-another-strategy
   =goal>
     isa      try-strategy
     state    wait-for-click
   ?manual>
      state free
   =visual-location> 
     isa      visual-location
   > screen-y 100
==>
   =goal>
      state choose-strategy)

(setf *actr-enabled-p* t)

(start-hand-at-mouse)

(goal-focus goal)

(spp :efforts 500 :successes 100)
(spp decide-over :failures 7 :successes 13 :efforts 100)
(spp decide-under :failures 7 :successes 13 :efforts 100)
(spp force-over :failures 10 :successes 10 :efforts 100)
(spp force-under :failures 10 :successes 10 :efforts 100)

(spp read-done :success t)
(spp pick-another-strategy :failure t)
)
