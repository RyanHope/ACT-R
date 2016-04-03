(defvar *key-time* nil)
(defvar *speak-time* nil)
(defvar *answer* nil)
(defvar *correct* nil)

(defvar *subitizing-exp-data*  
    '(.6 .65 .7 .86 1.12 1.5 1.79 2.13 2.15 2.58))


(defun subitize-trial (n &optional who)
  (let ((points (generate-points n))
        (window (open-exp-window "Subitizing Experiment"
                                 :visible t
                                 :width 300
                                 :height 300
                                 :x 300
                                 :y 300))
        (rt))
    
    (dolist (point points) 
      (add-text-to-exp-window :text "x"
                              :width 10
                              :x (first point)
                              :y (second point)))
    
    (setf *correct* nil)
    (setf *speak-time* nil)
    (setf *key-time* nil)
    
    (reset)
    
    (if (not (eq who 'human)) 
        (progn
          (setf *answer* (format nil "~r" n))
          (install-device window)
          (proc-display)
          (run 30 :real-time t)
          (setf rt (if (numberp *speak-time*) 
                       *speak-time* 
                     30000)))
      (progn
        (let ((start-time (get-time nil))) 
          (setf *answer* (format nil "~d" (if (= n 10) 0 n)))
          (while (null *key-time*)
            (allow-event-manager window))
          (setf rt (- *key-time* start-time)))))
    (list (/ rt 1000.0) *correct*)))

(defun subitize (&optional who)
  (let (result)
    (dolist (items (permute-list '(10 9 8 7 6 5 4 3 2 1)))
      (push (list items (subitize-trial items who)) result))
    (report-data (mapcar 'second (sort result '< :key 'car)))))

(defun report-data (data)
  (let ((rts (mapcar 'first data)))
    (correlation rts *subitizing-exp-data*)
    (mean-deviation rts *subitizing-exp-data*)
    (print-results data)))

(defun print-results (predictions)
  (format t "Items    Current Participant   Original Experiment~%")
  (dotimes (i (length predictions))
    (format t "~3d        ~5,2f  (~3s)              ~5,2f~%" 
      (1+ i) (car (nth i predictions)) (second (nth i predictions))
      (nth i *subitizing-exp-data*))))
  
(defun generate-points (n)
   (let ((points nil))
    (dotimes (i n points)
      (push (new-distinct-point points) points))))

(defun new-distinct-point (points)
  (do ((new-point (list (+ (act-r-random 240) 20) (+ (act-r-random 240) 20))
                  (list (+ (act-r-random 240) 20) (+ (act-r-random 240) 20))))
      ((not (too-close new-point points)) new-point)))

(defun too-close (new-point points)
  (some (lambda (a) (and (< (abs (- (car new-point) (car a))) 40)
                         (< (abs (- (cadr new-point) (cadr a))) 40))) 
        points))                 

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *correct* (string-equal (string key) *answer*))
  (setf *key-time* (get-time nil)))

(defmethod device-speak-string ((win rpm-window) text)
  (setf *correct* (string-equal text *answer*))
  (setf *speak-time* (get-time)))

(clear-all)

(define-model subitize

(sgp :v t)

(sgp :show-focus t 
     :visual-num-finsts 10 
     :visual-finst-span 10)

(chunk-type count count state)
(chunk-type number-fact identity next value)

(add-dm (one isa chunk)(two isa chunk)
        (three isa chunk)(four isa chunk)
        (five isa chunk)(six isa chunk)
        (seven isa chunk)(eight isa chunk)
        (nine isa chunk)(ten isa chunk)
        (zero isa chunk) (eleven isa chunk)
        (start isa chunk)
        (n0 isa number-fact identity zero next one value "zero")
        (n1 isa number-fact identity one next two value "one")
        (n2 isa number-fact identity two next three value "two")
        (n3 isa number-fact identity three next four value "three")
        (n4 isa number-fact identity four next five value "four")
        (n5 isa number-fact identity five next six value "five")
        (n6 isa number-fact identity six next seven value "six")
        (n7 isa number-fact identity seven next eight value "seven")
        (n8 isa number-fact identity eight next nine value "eight")
        (n9 isa number-fact identity nine next ten value "nine")
        (n10 isa number-fact identity ten next eleven value "ten")
        (goal isa count state start)) 


(goal-focus goal)

)
