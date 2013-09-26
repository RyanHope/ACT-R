(clear-all)

(defvar *deck1* 'regular-deck)
(defvar *deck2* 'regular-deck)

(defun play-hands (hands &optional (print-game nil))
  (let ((scores (list 0 0 0 0)))
   (dotimes (i hands)
      (let* ((mcards (deal *deck1*))
             (ocards (deal *deck2*))
             (mchoice (show-model-cards (butlast mcards) (first ocards)))
             (ochoice (show-opponent-cards (butlast ocards) (first mcards))))
        
        (unless (string-equal "h" mchoice) (setf mcards (butlast mcards)))
        (unless (string-equal "h" ochoice) (setf ocards (butlast ocards)))
          
        (let* ((mtot (score-cards mcards))
               (otot (score-cards ocards))
               (mres (compute-outcome mcards ocards))
               (ores (compute-outcome ocards mcards)))
          
          (show-model-results mcards ocards mres ores)
          
          (when print-game
            (format t "Model: ~{~2d ~} -> ~2d (~4s)   Opponent: ~{~2d ~}-> ~2d (~4s)~%"
              mcards mtot mres ocards otot ores))
          
          (setf scores (mapcar #'+ scores
                         (list (if (eq mres 'win) 1 0)
                               (if (eq ores 'win) 1 0)
                               (if (and (eq mres 'bust) (eq ores 'bust)) 1 0)
                               (if (and (= mtot otot) (not (eq mres 'bust)) (not (eq ores 'bust))) 1 0)))))))
          scores))

(defun run-blocks (blocks block-size) 
  (let (res)    
    (dotimes (i blocks (reverse res))
      (push (play-hands block-size) res))))

(defun show-learning (n &optional (graph t) (game 'game0))
  (let ((data nil))
    (dotimes (i n)
      (reset)
      (funcall game)
      (if (null data)
          (setf data (run-blocks 20 5))
        (setf data (mapcar (lambda (x y) (mapcar #'+ x y)) data (run-blocks 20 5)))))
    (let ((percentages (mapcar (lambda (x) (/ (car x) (* n 5.0))) data)))
      (when graph
        (draw-graph percentages))
      (list (list (/ (apply #'+ (subseq percentages 0 5)) 5)
                  (/ (apply #'+ (subseq percentages 5 10)) 5)
                  (/ (apply #'+ (subseq percentages 10 15)) 5)
                  (/ (apply #'+ (subseq percentages 15 20)) 5))
                  percentages))))

(defun draw-graph (points)
  (open-exp-window "Data" :width 550 :height 430 :visible t)
  (add-line-to-exp-window '(50 0) '(50 400) :color 'white :window "Data")
  (dotimes (i 5)
    (add-text-to-exp-window :x 0 :y (- 380 (* i 80)) :width 40 :text (format nil "~2,1f" (* (+ i 2) .1)) :window "Data")
    (add-line-to-exp-window (list 50 (- 390 (* i 80))) (list 550 (- 390 (* i 80))) :color 'white :window "Data"))
  
  (let ((x 50))
    (mapcar (lambda (a b) (add-line-to-exp-window (list x (floor (- 550 (* 800 a))))
                                                  (list (incf x 25) (floor (- 550 (* 800 b))))
                                                  :color 'blue :window "Data"))
      (butlast points) (cdr points))))

(defun deal (deck)
  (list (funcall deck)
        (funcall deck)
        (funcall deck)))

(defun score-cards (list &optional (bust 21))
  (if (find 1 list)
      (special-score list bust)
    (apply #'+ list)))

(defun special-score (list bust)
  (let ((possible (list (apply #'+ list))))
    (dotimes (i (count 1 list))
      (push (+ (* 10 (1+ i)) (apply #'+ list)) possible))
    (apply 'max (remove-if (lambda (x) (> x bust)) possible))))
  
(defun compute-outcome (p1cards p2cards &optional (bust 21))
  (let ((p1tot (score-cards p1cards))
        (p2tot (score-cards p2cards)))
    (if (> p1tot bust) 
        'bust 
      (if (or (> p2tot bust) (> p1tot p2tot)) 
          'win 
        'lose))))
  
  
(defun show-model-cards (mcards ocard)
  (if (buffer-read 'goal)
      (mod-focus-fct `(mc1 ,(first mcards) mc2 ,(second mcards) mc3 nil mtot nil mstart ,(score-cards mcards)
                           mresult nil oc1 ,ocard oc2 nil oc3 nil otot nil ostart ,(score-cards (list ocard))
                           oresult nil state start))
    (goal-focus-fct (car (define-chunks-fct 
                             `((isa game-state mc1 ,(first mcards) mc2 ,(second mcards) mc3 nil
                                    mtot nil mstart ,(score-cards mcards) mresult nil oc1 ,ocard
                                    oc2 nil oc3 nil otot nil ostart ,(score-cards (list ocard))
                                    oresult nil state start))))))
  (setf *model-action* nil)
  (run-full-time 10)
  *model-action*
  )

(defvar *model-action* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *model-action* (string key)))

(defun show-model-results (mcards ocards mres ores)
  (if (buffer-read 'goal)
      (mod-focus-fct `(mc1 ,(first mcards)  mc2 ,(second mcards) mc3 ,(third mcards) mtot ,(score-cards mcards)
                           mstart ,(score-cards (subseq mcards 0 2)) mresult ,mres
                           oc1 ,(first ocards) oc2 ,(second ocards) oc3 ,(third ocards) otot ,(score-cards ocards)
                           ostart ,(score-cards (list (first ocards))) oresult ,ores
                           state results))
    (goal-focus-fct (car (define-chunks-fct 
                             `((isa game-state mc1 ,(first mcards)  mc2 ,(second mcards) mc3 ,(third mcards) 
                                    mtot ,(score-cards mcards) mstart ,(score-cards (subseq mcards 0 2)) mresult ,mres
                                    oc1 ,(first ocards) oc2 ,(second ocards) oc3 ,(third ocards) otot ,(score-cards ocards)
                                    ostart ,(score-cards (list (first ocards))) oresult ,ores
                                    state results))))))
  (run-full-time 10))


(defun regular-deck ()
  (min 10 (1+ (act-r-random 13))))

(defvar *opponent-rule* 'fixed-threshold)

(defun show-opponent-cards (cards mc1)
  (funcall *opponent-rule* cards mc1))

(defvar *opponent-threshold* 15)

(defun fixed-threshold (cards mc1)
  (if (< (score-cards cards) *opponent-threshold*) "h" "s"))


 

(defun game0 ()
  (setf *deck1* 'regular-deck)
  (setf *deck2* 'regular-deck)
  (setf *opponent-threshold* 15)
  (setf *opponent-rule* 'fixed-threshold))


(defvar *card-list* nil)

(defun game1 ()
  (setf *card-list* nil)
  (setf *deck1* 'stacked-deck)
  (setf *deck2* 'stacked-deck)
  (setf *opponent-rule* 'always-hit))


(defun load-stacked-deck ()
  (let* ((card1 (+ 5 (act-r-random 6)))
         (card2 (+ 5 (act-r-random 6)))
         (card4 (if (> (act-r-random 1.0) .5) 2 8))
         (card3 (if (= card4 2) 10 (- 21 (+ card1 card2))))
         (card5 10)
         (card6 (if  (= card4 2) 10 2)))
    (list card1 card2 card3 card4 card5 card6)))

(defun stacked-deck ()
  (cond (*card-list* (pop *card-list*))
        (t (setf *card-list* (load-stacked-deck)) 
           (pop *card-list*))))

(defun always-hit (cards mc1) 
  "h")


(defun number-sims (a b)
  (when (and (numberp a) (numberp b))
    (- (/ (abs (- a b)) (max a b)))))

(define-model 1-hit-model 
    
    ;; do not change these parameters
    (sgp :esc t :bll .5 :ol t :sim-hook number-sims :er t :ncnar nil :lf 0 :rt -60)
  
  ;; adjust these as needed
  (sgp :v nil :ans .2 :mp 10.0)
  
  ;; create a device for the model to interact with
  
  (install-device (open-exp-window "" :visible nil))
  
  ;; This type holds all the game info 
  
  (chunk-type game-state mc1 mc2 mc3 mstart mtot mresult oc1 oc2 oc3 ostart otot oresult state bust)
  
  ;; This chunk-type should be modified to contain the information needed
  ;; for your model to learn
  
  (chunk-type learned-info mc1 action)
  
   
  (define-chunks (win isa chunk) (lose isa chunk) (bust isa chunk) 
    (done isa chunk) (retrieving isa chunk) (start isa chunk) (results isa chunk))
    
      
  (p start
    =goal>
       isa game-state
       state start
       MC1 =c
   ==>
    =goal>
       state retrieving
    +retrieval>
       isa learned-info
       MC1 =c
     )

  (p cant-remember-game
    =goal>
       isa game-state
       state retrieving
    ?retrieval>
       state   error
    ?manual>
       state free
   ==>
    =goal>
       state done
    +imaginal>
       isa learned-info
       action "s"
    +manual>
       isa press-key
       key "s"
     )
  
  (p remember-game
    =goal>
       isa game-state
       state retrieving
    =retrieval>
       isa learned-info
       action =act
   ==>
    =goal>
       state done
    +imaginal>
       isa learned-info
       action =act
     
    +manual>
       isa press-key
       key =act
     
    =retrieval>
       MC1 nil
       action nil
    -retrieval>
    )

  (p results-should-hit
    =goal>
       isa game-state
       state results
       mresult =outcome
       MC1 =c
    =imaginal>
       isa learned-info
   ==>
    !output! (I =outcome)

    =imaginal>
       MC1 =c 
       action "h"
    -imaginal>
    )

  (spp results-should-hit :u 10)

  
  (p results-should-stay
    =goal>
       isa game-state
       state results
       mresult =outcome
       MC1 =c
    =imaginal>
       isa learned-info
   ==>
    !output! (I =outcome)
     
    =imaginal>
       MC1 =c 
       action "s"
    -imaginal>
    ) 
  )
