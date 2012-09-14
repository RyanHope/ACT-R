(defvar *report*)
(defvar *number*)
(defvar *word*)
(defvar *repcount*)
(defvar *trial*)

(defun make-triples (l)
   (when l
   (cons (list (first l)(second l)(third l)
               (fourth l)
               (if (eq (second l) 'I) 'blank 'ed))
         (make-triples (nthcdr 4 l)))))

(defparameter *word-list* 
  (make-triples '(have      I              12458 had
                  do        I               4367 did
                  make      I               2312 made
                  get       I               1486 got
                  use       R               1016 use
                  look      R                910 look
                  seem      R                831 seem
                  tell      I                759 told
                  show      R                640 show
                  want      R                631 want
                  call      R                627 call
                  ask       R                612 ask
                  turn      R                566 turn
                  follow    R                540 follow
                  work      R                496 work
                  live      R                472 live
                  try       R                472 try
                  stand     I                468 stood
                  move      R                447 move
                  need      R                413 need
                  start     R                386 start
                  lose      I                274 lost)))


(defparameter *total-count* (apply #'+ (mapcar #'third *word-list*)))

;;; Select a random word from the vocabulary, but based on its frequency

(defun random-word ()
  (let
    ((num (act-r-random *total-count*)))
    (dolist (i *word-list*)
      (if (< num (third i))
        (return i)
        (setf num (- num (third i)))))))

;;; Set the goal to do one past tense

(defun make-one-goal ()
   (let*
     ((wordpair (random-word))
      (word (first wordpair))
      (word-no (second wordpair)))
     
     (setf *number* word-no *word* word)
     (set-buffer-chunk 'imaginal (car (define-chunks-fct 
                                          (list (list 'isa 'past-tense 'verb word)))))
     (goal-focus starting-goal)))

;;; This function simulates "hearing" a past tense: it adds a correct 
;;; past tense to memory by placing it into a buffer and then clearing
;;; it so that it can be merged with others

(defun add-past-tense-to-memory ()
   (let*
     ((wordpair (random-word))
      (word (first wordpair))
      (stem (fourth wordpair))
      (suffix (fifth wordpair)))
     (set-buffer-chunk 'imaginal (car (define-chunks-fct 
                                          (list (list 'isa 'past-tense 'verb word 
                                                      'stem stem 'suffix suffix)))))
     (clear-buffer 'imaginal)))

;;; The following function reports how often an irregular word gets an irregular 
;;; (correct), regular past tense or just the stem as past tense (None). It 
;;; shows how this developes in time.

(defun report-irreg (&optional (graph nil) (trials 1000))
   (format t "~% Irreg    Reg   None   Overreg~%")
   (let ((data (mapcar #'fourth (rep-f-i (reverse *report*) trials))))
     (when graph
       (graph-it data)))
  nil)

(defun graph-it (data)
  (let ((win (open-exp-window "Irregular Verbs correct" :width 400 :height 350)))
    (clear-exp-window)
    (add-text-to-exp-window :x 5 :y 5 :text "1.0" :width 22)
    (add-text-to-exp-window :x 5 :y 300 :text "0.7" :width 22)
    (add-text-to-exp-window :x 150 :y 320 :text "Trials" :width 100)
    (add-line-to-exp-window '(30 10) '(30 310) :color 'black)
    (add-line-to-exp-window '(380 310) '(30 310) :color 'black)
    (add-line-to-exp-window '(25 10) '(35 10) :color 'black)
    (add-line-to-exp-window '(25 110) '(35 110) :color 'black)
    (add-line-to-exp-window '(25 210) '(35 210) :color 'black)
      
    (do* ((increment (max 1.0 (floor (/ 350.0 (length data)))))
          (p1 (butlast data) (cdr p1))
          (p2 (cdr data) (cdr p2))
          (last-x 30 this-x)
          (last-y (+ 10 (floor (* (- 1.0 (car p1)) 1000)))
                  (+ 10 (floor (* (- 1.0 (car p1)) 1000))))
          (this-x (+ last-x increment)
                  (+ last-x increment))
          (this-y (+ 10 (floor (* (- 1.0 (car p2)) 1000)))
                  (+ 10 (floor (* (- 1.0 (car p2)) 1000)))))
         ((null (cdr p1)) (add-line-to-exp-window 
                           (list last-x last-y) (list this-x this-y) :color 'red))
      (add-line-to-exp-window (list last-x last-y) 
                              (list this-x this-y) 
                              :color 'red))))


(defun rep-f-i (l n)
   (if l
     (let ((x (if (> (length l) n) (subseq l 0 n) l))
           (y (if (> (length l) n) (subseq l n) nil))
           (irreg 0)
           (reg 0)
           (none 0)
           (data nil))
       (dolist (i x)
         (cond ((eq (first i) 'R) nil)
               ((eq (second i) 'reg) (setf reg (1+ reg)))
               ((eq (second i) 'irreg) (setf irreg (1+ irreg)))
               (t (setf none (1+ none)))))
       (if (> (+ irreg reg none) 0)
         (setf data (list (/ irreg (+ irreg reg none))
                 (/ reg (+ irreg reg none))(/ none (+ irreg reg none))
                 (if (> (+ irreg reg) 0) (/ irreg (+ irreg reg)) 0)))
         (setf data (list 0 0 0 0)))
       (format t "~{~6,3F~}~%" data)
       (cons data (rep-f-i y n)))
    nil))

(defun add-to-report (the-chunk)
   (let* ((stem (chunk-slot-value-fct the-chunk 'stem))
          (word (chunk-slot-value-fct the-chunk 'verb))
          (suffix (chunk-slot-value-fct the-chunk 'suffix)))
     (cond
      ((eq stem word) (push (list *number* 'reg *word*) *report*))
      ((eq suffix nil) (push (list *number* 'none *word*) *report*))
      (t (push (list *number* 'irreg *word*) *report*)))))


;;; This function will run the experiment for n trials.
;;; The keyword parameters are:
;;;   cont - continue, when set to true the experiment continues instead of 
;;;          starting anew.
;;;   repfreq - determines how often results are reported during a run.
;;;   v - the setting for the :v parameter of the model i.e. whether or not
;;;       to display the model's trace.

(defun past-tense (n &key (cont nil)(repfreq 100)(v nil))
  (unless cont
   
    (reset)
    (format t "~%")
    (setf *report* nil)
    (setf *trial* 0 *repcount* 0))
  
  (sgp-fct (list :v v))
  
  (dotimes (i n)
    (add-past-tense-to-memory)
    (add-past-tense-to-memory)
    (make-one-goal)
    (run 200) 
    (add-to-report (buffer-read 'imaginal))
    (clear-buffer 'imaginal)
    (incf *repcount*)
    (when (>= *repcount* repfreq)
      (format t "Trial ~6D : " (1+ *trial*))
      (rep-f-i (subseq *report* 0 repfreq) repfreq)
      (setf *repcount* 0))
    
    (run-full-time 200)
    (incf *trial*)))

(clear-all)

(define-model past-tense
    (sgp
     :V t
     :esc T
     :ul t 
     :OL 6   
     :BLL 0.5  
     :ANs 0.1   
     :EGs 0.2   
     :mas   3.5 
     :imaginal-activation 1.0
     :LF 0.5
     :RT 0.5
     :epl t
     :alpha 0.1 
     :pct t
     :iu 5    
     :ncnar nil
     :do-not-harvest imaginal)
  
  (chunk-type past-tense verb stem suffix)
  (chunk-type goal state)
  
  (define-chunks (starting-goal isa goal state start))
  

;;; When there is no suffix we have an irregular

(p find-past-tense-no-suffix
    =goal>
   isa goal
   state done
   =imaginal>
      isa past-tense
      verb =word
      suffix blank
      
==>
   =goal> state nil
   )

(spp find-past-tense-no-suffix :reward 5)

;;; When there is a suffix

(p find-past-tense-regular
   =goal>
   isa goal
   state done
   =imaginal>
   isa past-tense
      verb =stem
      stem =stem
      suffix =suffix
   !safe-eval! (not (eq =suffix 'blank)) 
   ==>
   =goal> state nil
   )

(spp find-past-tense-regular :reward 4.2)

;;; a failed goal

(p find-past-tense-equal
    =goal>
   isa goal
   state done
   =imaginal>
   isa past-tense
      stem nil
      suffix nil
==>
   =goal> state nil
   )

(spp find-past-tense-equal :reward 3.9) 
)
