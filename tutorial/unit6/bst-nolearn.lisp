(defvar *stick-a*)
(defvar *stick-b*)
(defvar *stick-c*)
(defvar *target*)
(defvar *current-stick*)
(defvar *current-line*)
(defvar *done*)
(defvar *choice*)
(defvar *experiment-window* nil)
(defvar *visible* nil)

(defparameter *bst-stimuli* '((15  200  41 103)(10  200 29 132)))

(defun build-display (a b c target)
    (setf *experiment-window* (open-exp-window "Building Sticks Task"
                                               :visible *visible*
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

   (add-button-to-exp-window :x 5 :y 23  :height 24 :width 40 :text "A"     :action #'button-a-pressed)
   (add-button-to-exp-window :x 5 :y 48  :height 24 :width 40 :text "B"     :action #'button-b-pressed)
   (add-button-to-exp-window :x 5 :y 73  :height 24 :width 40 :text "C"     :action #'button-c-pressed)
   (add-button-to-exp-window :x 5 :y 123 :height 24 :width 65 :text "Reset" :action #'reset-display)

   (add-line-to-exp-window (list 75 35)  (list (+ a 75) 35) :color 'black)
   (add-line-to-exp-window (list 75 60)  (list (+ b 75) 60) :color 'black)
   (add-line-to-exp-window (list 75 85)  (list (+ c 75) 85) :color 'black)
   (add-line-to-exp-window (list 75 110) (list (+ target 75) 110) :color 'green)

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
         (setf *current-line* (add-line-to-exp-window (list 75 135) (list  (+ *target* 75) 135) :color 'blue))
         (add-text-to-exp-window :x 180 :y 200 :width 50 :text "Done"))
     (if (zerop *current-stick*)
           (setf *current-line* nil)
         (setf *current-line* (add-line-to-exp-window (list 75 135) (list (+ *current-stick* 75) 135) :color 'blue))))

   (allow-event-manager *experiment-window*)

   (proc-display))

(defun do-experiment (sticks who)

  (if (eq who 'human)
      (setf *visible* t)
    (setf *visible* nil))
  
  (apply #'build-display sticks)
  (install-device *experiment-window*)
  
  (if (eq who 'human)
      (wait-for-human)
    (progn
      (proc-display :clear t)
      (run 60))))

(defun wait-for-human ()
  (while (not *done*)
    (allow-event-manager *experiment-window*))
  (sleep 1))

(defun bst-set (&optional who)
   (let ((result nil))
     (reset)
     
     (dolist (stim *bst-stimuli*)
       (do-experiment stim who)
       (push *choice* result))
     (reverse result)))

(defun bst-task (n &optional who)
   (let ((result (make-list (length *bst-stimuli*) :initial-element 0)))
     (dotimes (i n result)
       (setf result (mapcar #'+ result (mapcar #'(lambda (x) (if (equal x 'over) 1 0)) (bst-set who)))))))

(clear-all)

(define-model bst-nolearn

(sgp :v nil :esc t :egs 3 :show-focus t :ul nil)

(chunk-type try-strategy strategy state)
(chunk-type encoding a-loc b-loc c-loc goal-loc length over under)

(add-dm (goal isa try-strategy state start))


(p start-trial
    =goal>
      isa      try-strategy
      state    start
    ?visual-location>
      buffer unrequested
   ==>
    =goal>
      state find-line)


(p find-next-line
    =goal>
      isa      try-strategy
      state    find-line
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
   
    =goal>
      state    attending
    +visual>
      isa move-attention
      screen-pos =visual-location)

(p encode-line-a
    =goal>
      isa      try-strategy
      state    attending
    =visual>
      isa      line
      screen-pos =pos
    ?imaginal>
      buffer empty
      state  free
   ==>
    +imaginal>
      isa encoding
      a-loc    =pos
    =goal>
      state    find-line)

(p encode-line-b
    =goal>
      isa      try-strategy
      state    attending
    =imaginal>
      isa encoding
      a-loc    =a
      b-loc    nil
    =visual>
      isa      line
      screen-pos =pos
==>
    =imaginal>
      b-loc    =pos
    =goal>
      state    find-line)

(p encode-line-c
    =goal>
      isa      try-strategy
      state    attending
    =imaginal>
      isa encoding
      b-loc    =b
      c-loc    nil
    =visual>
      isa      line
      screen-pos =pos
==>
    =imaginal>
      c-loc    =pos
    =goal>
      state    find-line)

(p encode-line-goal
    =goal>
      isa      try-strategy
      state    attending
    =imaginal>
      isa encoding
      c-loc    =c
      goal-loc nil
    =visual>
      isa      line
      screen-pos =pos
      width    =length
    ?visual>
      state    free
==>
    =imaginal>
      goal-loc =pos
      length   =length

    =goal>
      state    encode-under
    +visual>
      isa      move-attention
      screen-pos =c)

(p encode-under
    =goal>
      isa      try-strategy
      state    encode-under
    =imaginal>
      isa encoding
      b-loc    =b
      length   =goal-len
    =visual>
      isa      line
      width    =c-len
    ?visual>
      state    free
==>
    !bind! =val (- =goal-len =c-len)

    =imaginal>
      under =val
    =goal>
      state    encode-over
    +visual>
      isa      move-attention
      screen-pos =b)

(p encode-over
    =goal>
      isa      try-strategy
      state    encode-over
    =imaginal>
      isa encoding
      length   =goal-len
    =visual>
      isa      line
      width    =b-len
==>
    !bind! =val (- =b-len =goal-len)
    =imaginal>
      over =val

    =goal>
      state    choose-strategy)

(p encode-line-current
    =goal>
      isa      try-strategy
      state    attending
    =imaginal>
      isa encoding
      goal-loc =goal-loc
    =visual>
      isa      line
      width    =current-len
    ?visual>
      state    free
==>
    =imaginal>
      length   =current-len
    =goal>
      state    calculate-difference
    +visual>
      isa      move-attention
      screen-pos =goal-loc)

(p calculate-difference
    =goal>
      isa      try-strategy
      state    calculate-difference
    =imaginal>
      isa encoding
      length   =current-len
    =visual>
      isa      line
      width    =goal-len
==>
    !bind! =val (abs (- =current-len =goal-len))
   
    =imaginal>
      length =val
   
   =goal>
      state    consider-next)

(p check-for-done
    =goal>
      isa      try-strategy
      state    consider-next
   
    =imaginal>
      isa encoding
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
    =imaginal>
      isa encoding
      c-loc    =c-loc
    > length   0

    ?visual>
       state free
==>
    =imaginal>
    =goal>
      state    evaluate-c
    +visual>
      isa     move-attention
      screen-pos =c-loc)

(p choose-c
    =goal>
      isa      try-strategy
      state    evaluate-c
    =imaginal>
      isa encoding
      length   =difference
    =visual>
      isa      line
    <= width   =difference
==>
    =imaginal>
    =goal>
      state    prepare-mouse
    +visual-location>
      isa      visual-location
      kind     oval
      screen-y 85)

(p consider-a
    =goal>
      isa      try-strategy
      state    evaluate-c
    =imaginal>
      isa encoding
      a-loc    =a-loc
      length   =difference
    =visual>
      isa      line
    > width    =difference
    ?visual>
      state free
==>
    =imaginal>
    =goal>
      state    evaluate-a
    +visual>
      isa      move-attention
      screen-pos =a-loc)

(p choose-a
    =goal>
      isa      try-strategy
      state    evaluate-a
    =imaginal>
      isa encoding
      length   =difference
    =visual>
      isa      line
    <= width   =difference
==>
    =imaginal>
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
    =imaginal>
      isa encoding
      length   =difference
    =visual>
      isa      line
    > width    =difference
==>
    =imaginal>
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
    =imaginal>
      isa encoding
      under    =under
      over     =over

    !eval! (< =over (- =under 25))

==>
    =imaginal>
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
    =imaginal>
      isa encoding
      over     =over
      under    =under

    !eval! (< =under (- =over 25))
==>
    =imaginal>
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


(goal-focus goal)

(start-hand-at-mouse)

(spp decide-over :u 13)
(spp decide-under :u 13)
(spp force-over :u 10)
(spp force-under :u 10))
