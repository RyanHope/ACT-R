(defvar *report*)
(defvar *word*)
(defvar *repcount*)
(defvar *trial*)

(defun make-word-freq-list (l &optional (start 0))
  (when l
    (let ((count (third l)))
      (cons (list (+ start count) (first l) (fourth l)
                  (if (eq (second l) 'I) 'blank 'ed))
            (make-word-freq-list (nthcdr 4 l) (+ start count))))))

(defparameter *word-list* 
  (make-word-freq-list '(have      I              12458 had
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


(defparameter *total-count* (caar (last *word-list*)))

(defun random-word ()
  (let ((num (act-r-random *total-count*)))
    (cdr (find-if (lambda (x) (< num (first x))) *word-list*))))

(defun make-one-goal ()
  (setf *word* (random-word))
  
  (set-buffer-chunk 'imaginal 
                    (car (define-chunks-fct 
                             (list (list 'verb (first *word*))))))
  (goal-focus starting-goal))

(defun add-past-tense-to-memory ()
  (let ((word (random-word)))
    (set-buffer-chunk 'imaginal 
                      (car (define-chunks-fct 
                               (list (mapcan (lambda (x y) (list x y))
                                       '(verb stem suffix) word)))))
    (clear-buffer 'imaginal)))


(defun report-irreg (&optional (graph nil) (trials 1000))
  (format t "~% Irreg   Reg   None  Overreg~%")
  (let ((data (mapcar 'fourth (rep-f-i *report* trials))))
    (when (and graph data)
      (graph-it data)))
  nil)

(defun graph-it (data)
  (let* ((win (open-exp-window "Irregular Verbs correct" :width 500 :height 475))
         (low (apply 'min data))
         (zoom (min .9 (/ (floor low .1) 10))))
    (allow-event-manager win)
    (clear-exp-window)
    (add-text-to-exp-window :x 5 :y 5 :text "1.0" :width 22)
    (add-text-to-exp-window :x 5 :y 400 :text (format nil "~0,2f" zoom) :width 22)
    (add-text-to-exp-window :x 5 :y 200 :text (format nil "~0,2f" (+ zoom (/ (- 1.0 zoom) 2))) :width 22)
    
    (add-text-to-exp-window :x 200 :y 420 :text "Trials" :width 100)
    (add-line-to-exp-window '(30 10) '(30 410) :color 'black)
    (add-line-to-exp-window '(450 410) '(25 410) :color 'black)
    (dotimes (i 10)
      (add-line-to-exp-window (list 25 (+ (* i 40) 10)) (list 35 (+ (* i 40) 10)) :color 'black))
    
    (when (= (length data) 1)
      (push (first data) data))
    (do* ((increment (max 1.0 (floor (/ 450.0 (length data)))))
          (range (floor 400 (- 1.0 zoom)))
          (intercept (+ range 10))
          (p1 (butlast data) (cdr p1))
          (p2 (cdr data) (cdr p2))
          (last-x 30 this-x)
          (last-y (- intercept (floor (* range (car p1))))
                  (- intercept (floor (* range (car p1)))))
          (this-x (+ last-x increment)
                  (+ last-x increment))
          (this-y (- intercept (floor (* range (car p2))))
                  (- intercept (floor (* range (car p2))))))
         ((null (cdr p1)) (add-line-to-exp-window 
                           (list last-x last-y) (list this-x this-y) :color 'red))
      (add-line-to-exp-window (list last-x last-y) 
                              (list this-x this-y) 
                              :color 'red))
    (allow-event-manager win)))


(defun rep-f-i (l n)
   (when l
     (let ((x (if (> (length l) n) (subseq l 0 n) l))
           (y (if (> (length l) n) (subseq l n) nil))
           (irreg 0)
           (reg 0)
           (none 0)
           (error 0))
       (dolist (i x)
         (when (first i)
           (case (second i)
             (reg
              (incf reg))
             (irreg 
              (incf irreg))
             (none
              (incf none))
             (t
              (incf error)))))
       
       (let* ((total (+ irreg reg none))
              (data
               (if (zerop total)
                   (list 0 0 0 0)
                 (list (/ irreg total) (/ reg total)
                       (/ none total) (if (> (+ irreg reg) 0) (/ irreg (+ irreg reg)) 0)))))
         (format t "~{~6,3F~^ ~}~%" data)
         (cons data (rep-f-i y n))))))

(defun add-to-report (the-chunk)
  (let ((stem (chunk-slot-value-fct the-chunk 'stem))
        (word (chunk-slot-value-fct the-chunk 'verb))
        (suffix (chunk-slot-value-fct the-chunk 'suffix))
        (irreg (eq (third *word*) 'blank)))
    
    (if (eq (first *word*) word)
        (cond ((and (eq stem word) (eq suffix 'ed))
               (push-last (list irreg 'reg) *report*))
              ((and (null suffix) (null stem))
               (push-last (list irreg 'none) *report*))
              ((and (eq stem (second *word*)) (eq suffix 'blank))
               (push-last (list irreg 'irreg) *report*))
              (t
               (print-warning "Incorrectly formed verb.  Presented ~s and produced ~{~s~^ ~}." 
                              (first *word*) 
                              (mapcan (lambda (x y) (list x y))
                                '(verb stem suffix) (list word stem suffix)))
               (push-last (list irreg 'error) *report*)))
      (progn
        (print-warning "Incorrectly formed verb.  Presented ~s and produced ~{~s~^ ~}." 
                       (first *word*) 
                       (mapcan (lambda (x y) (list x y))
                         '(verb stem suffix) (list word stem suffix)))
        (push-last (list irreg 'error) *report*)))))


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
    (incf *trial*)
    (when (>= *repcount* repfreq)
      (format t "Trial ~6D : " *trial*)
      (rep-f-i (subseq *report* (- *trial* repfreq)) repfreq)
      (setf *repcount* 0))
    
    (run-full-time 200)))

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
  
  (define-chunks 
      (starting-goal isa goal state start)
      (start isa chunk) (done isa chunk))
  
  ;; Make sure all the words needed are already chunks
  (dolist (x *word-list*)
    (unless (chunk-p-fct (second x)) (define-chunks-fct (list (list (second x)))))
    (unless (chunk-p-fct (third x)) (define-chunks-fct (list (list (third x)))))
    (unless (chunk-p-fct (fourth x)) (define-chunks-fct (list (list (fourth x))))))

(declare-buffer-usage goal goal state)
  (declare-buffer-usage imaginal past-tense :all)

;;; When there is a stem and no suffix we have an irregular

(p past-tense-irregular
   =goal>
     isa    goal
     state  done
   =imaginal>
     isa    past-tense
     verb   =word
     stem   =past
     suffix blank
  ==>
   =goal> 
     state  nil)

(spp past-tense-irregular :reward 5)

;;; When the stem matches the verb and the suffix is not "blank" consider it regular

(p past-tense-regular
   =goal>
    isa     goal
    state   done
   =imaginal>
     isa    past-tense
     verb   =stem
     stem   =stem
     suffix =suffix
   !safe-eval! (not (eq =suffix 'blank)) 
  ==>
   =goal> 
     state  nil)

(spp past-tense-regular :reward 4.2)

;;; when there's no stem and no suffix that's a "give up" result

(p no-past-tense-formed
   =goal>
     isa    goal
     state  done
   =imaginal>
     isa    past-tense
     stem   nil
     suffix nil
  ==>
   =goal> 
     state  nil)

(spp no-past-tense-formed :reward 3.9) 
)
