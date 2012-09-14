(defvar *responses* nil)
(defvar *show-responses* nil)
(defvar *done* nil)

(defconstant *sperling-exp-data* '(3.03 2.40 2.03 1.50))

(defun do-trial (onset-time)
  
  (reset)
  
  (let* ((lis (permute-list '("B" "C" "D" "F" "G" "H" "J" 
                              "K" "L" "M" "N" "P" "Q" "R" 
                              "S" "T" "V" "W" "X" "Y" "Z")))
         (answers nil)   
         (tone (act-r-random 3))
         (window (open-exp-window "Sperling Experiment"                                      
                                  :visible t
                                  :width 300
                                  :height 300)))
    
    (dotimes (i 3)
      (dotimes (j 4)        
        (let ((txt (nth (+ j (* i 4)) lis)))
          (when (= i tone)
            (push txt answers))
          (add-text-to-exp-window :text txt
                                  :width 40
                                  :x (+ 75 (* j 50))
                                  :y (+ 101 (* i 50))))))
 

    (if *actr-enabled-p* 
        (progn
          
          (install-device window)
          (new-tone-sound (case tone (0 2000) (1 1000) (2 500)) .5 onset-time)
          (schedule-event-relative (+ .9 (act-r-random .2)) 'clear-screen)
    
          (proc-display)
          (setf *responses* nil)
          (run 30))
      (progn
        (sleep onset-time) 
        
        (if (fboundp 'beep) 
            (dotimes (i (1+ tone)) (beep)) 
          (format t "Cannot generate sounds.~%Recall row ~S~%" (1+ tone)))
        
        (sleep (- 1.0 onset-time))
        
        (setf *done* nil)
        (setf *responses* nil)
        (clear-exp-window)
    
        (while (null *done*)
          (allow-event-manager window))))
    
    (when *show-responses*
      (format t "~%~%answers: ~S~%responses: ~S~%" answers *responses*))
    
    (compute-score answers)))


(defun compute-score (answers)
  (let ((score 0))
    (dolist (x answers score)
      (when (member x *responses* :test #'string-equal)
        (incf score)))))
    
  
(defun clear-screen () 
  (clear-exp-window)
  (proc-display))
   


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (if (string= key " ")
      (setf *done* t)
    (push (string key) *responses*)))



(defun report-data (data)
  (correlation data *sperling-exp-data*)
  (mean-deviation data *sperling-exp-data*)
  (print-results data))

(defun print-results (data)
  (format t "~%Condition    Current Participant   Original Experiment~%")
  (do ((condition '(0.00 0.15 0.30 1.00) (cdr condition))
       (temp1 data (cdr temp1))
       (temp2 *sperling-exp-data* (cdr temp2)))
      ((null temp1))
    (format t " ~4,2F sec.          ~6,2F                ~6,2F~%" 
              (car condition) (car temp1) (car temp2))))


(defun run-block ()
  (let ((times (permute-list '(0.0 .15 .30 1.0)))
        (result nil))
    (dolist (x times)
      (push (cons x (do-trial x)) result))
    (sort result #'< :key #'car)))


(defun repeat-experiment (n)
  (let ((results (list 0 0 0 0)))
    (dotimes (i n)
      (setf results (mapcar #'+ results (mapcar #'cdr (run-block)))))
    (report-data (mapcar #'(lambda (x) (/ x n)) results))))

(clear-all)

(define-model sperling

(sgp :v t :esc t :declarative-finst-span 10)

(sgp :show-focus t :needs-mouse nil :trace-detail medium)

(sgp :seed (100 0))

(chunk-type read-letters location tone state upper-y lower-y)
(chunk-type report-row row)

(add-dm
 (attending isa chunk) (low isa chunk)
 (medium isa chunk) (high isa chunk)
 (find isa chunk) (encode isa chunk)
 (goal isa read-letters state attending upper-y 0 lower-y 300))

(p detected-sound
   =aural-location>
     isa      audio-event
     attended nil
  
   ?aural>
      state    free
   
   ==>
   +aural>
     isa      sound
     event    =aural-location)

(p sound-respond-low
   =goal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  500
==>
   =goal>
     tone     low
     upper-y  205
     lower-y  215)

(p sound-respond-medium
   =goal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  1000
==>
   =goal>
     tone     medium
     upper-y  155
     lower-y  165)

(p sound-respond-high
   =goal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  2000
==>
   =goal>
     tone     high
     upper-y  105
     lower-y  115)

(p find-random-letter
   =goal>
     isa      read-letters
     state    find
     tone     nil
==>
   +visual-location>
     isa      visual-location
     :attended nil
   =goal>
     state    attending)

(p attend-low
   =goal>
     isa      read-letters
     state    attending
   =visual-location>
     isa      visual-location
   > screen-y 204
   < screen-y 216
   
   ?visual>
      state    free
   
   ==>
   =goal>
     location low
     state    encode
   +visual>
     isa      move-attention
     screen-pos =visual-location)

(p attend-medium
   =goal>
     isa      read-letters
     state    attending
   =visual-location>
     isa      visual-location
   > screen-y 154
   < screen-y 166
   
   ?visual>
      state    free
==>
   =goal>
     location medium
     state    encode
   +visual>
     isa      move-attention
     screen-pos =visual-location)


(p attend-high
   =goal>
     isa      read-letters
     state    attending
   =visual-location>
     isa      visual-location
   > screen-y 104
   < screen-y 116
   
   ?visual>
      state    free
==>
   =goal>
     location high
     state    encode
   +visual>
     isa      move-attention
     screen-pos =visual-location)

(p encode-row-and-find
   =goal>
     isa      read-letters
     location =pos
     upper-y  =uy
     lower-y  =ly
   =visual>
     isa      text
     status   nil
==>
   =visual>
     status   =pos
   
   -visual>
   
   =goal>
     location nil
     state    attending
   +visual-location>
     isa      visual-location
     :attended nil
   > screen-y =uy 
   < screen-y =ly)

(P start-report
   =goal>
     isa      read-letters
     tone     =tone
   
   ?visual>
      state   free
   ==>
   +goal>
     isa      report-row
     row      =tone
   +retrieval>
     isa      text
     status   =tone)

(P do-report
   =goal>
     isa      report-row
     row      =tone
   =retrieval>
     isa      text
     status   =tone
     value    =val
   
   ?manual>
      state    free
   ==>
   
   +manual>              
     isa      press-key     
     key      =val
   +retrieval>
     isa      text
     status   =tone
     :recently-retrieved nil
)

(p stop-report 
   =goal>
     isa      report-row
     row      =row
   
   ?retrieval>
      state   error
   
   ?manual>
      state    free
==>
   +manual>              
     isa      press-key       
     key      space
   -goal>)

(setf *actr-enabled-p* t)

(setf *show-responses* t)

(goal-focus goal)

(spp start-report :c 2)
(spp detected-sound  :c 0)
(spp sound-respond-low :c 0)
(spp sound-respond-medium :c 0)
(spp sound-respond-high :c 0)
)