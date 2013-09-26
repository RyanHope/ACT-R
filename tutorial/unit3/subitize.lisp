(defvar *response* nil)
(defvar *response-time* nil)
(defvar *who*)

(defvar *subitizing-exp-data*  
    '(.6 .65 .7 .86 1.12 1.5 1.79 2.13 2.15 2.58))


(defun subitize-trial (n &optional who)
  (let ((points (generate-points n))
        (window (open-exp-window "Subitizing Experiment"                                      
                                 :visible t
                                 :width 300
                                 :height 300
                                 :x 300
                                 :y 300)))
    
    (dolist (point points) 
      (add-text-to-exp-window :text "x"
                              :width 10
                              :x (first point)
                              :y (second point)))
    (setf *response* nil)
    (setf *response-time* nil)
    
    
    (if (not (eq who 'human)) 
        (progn
          (setf who 'model)
          (reset)
          (install-device window)
          (proc-display )
          (run 30 :real-time t))
      (progn
        (let ((start-time (get-time nil))) 
          (while (null *response*)
            (allow-event-manager window))
          (setf *response-time* (- *response-time* start-time)))))
        
    (let ((response (if (and (eq who *who*) *response*) (read-from-string *response*) -1)))
      (list (if (or (not (eq who *who*)) (null *response-time*)) 30.0 (/ *response-time* 1000.0)) 
            (or (= response n) 
                (and (= n 10) (= response 0)))))))

(defun subitize (&optional who)
  (let (result)
    (dolist (items (permute-list '(10 9 8 7 6 5 4 3 2 1)))
      (push (list items (subitize-trial items who)) result))
    (report-data (mapcar 'second (sort result '< :key 'car)))))


(defun report-data (data)
  (let ((rts (mapcar #'first data)))
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
  (some #'(lambda (a) (and (< (abs (- (car new-point) (car a))) 40)
                           (< (abs (- (cadr new-point) (cadr a))) 40))) 
        points))                 

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *who* 'human)
  (setf *response-time* (get-time nil))
  (setf *response* (string key)))

(defmethod device-speak-string ((win rpm-window) text)
  (setf *who* 'model)
  (setf *response-time* (get-time))
  (setf *response* text))


(clear-all)

(define-model subitize

(sgp :v t)

(sgp :show-focus t 
     :visual-num-finsts 10 
     :visual-finst-span 10 )

(chunk-type count count state)
(chunk-type number-fact identity next value)

(add-dm (one isa chunk)(two isa chunk)
        (three isa chunk)(four isa chunk)
        (five isa chunk)(six isa chunk)
        (seven isa chunk)(eight isa chunk)
        (nine isa chunk)(ten isa chunk)
        (zero isa chunk) (eleven isa chunk)
        (start isa chunk)
        (n0 isa number-fact identity zero next one value "0")
        (n1 isa number-fact identity one next two value "1")
        (n2 isa number-fact identity two next three value "2")
        (n3 isa number-fact identity three next four value "3")
        (n4 isa number-fact identity four next five value "4")
        (n5 isa number-fact identity five next six value "5")
        (n6 isa number-fact identity six next seven value "6")
        (n7 isa number-fact identity seven next eight value "7")
        (n8 isa number-fact identity eight next nine value "8")
        (n9 isa number-fact identity nine next ten value "9")
        (n10 isa number-fact identity ten next eleven value "0")
        (goal isa count state start)) 


(goal-focus goal)

)
