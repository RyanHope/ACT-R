(clear-all)

(defvar *response* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response* (string key)))

(defun do-unit2 (&optional who)
  
  (reset)
  
  (let* ((letters (permute-list '("B" "C" "D" "F" "G" "H" "J" "K"  
                                  "L" "M" "N" "P" "Q" "R" "S" "T"  
                                  "V" "W" "X" "Y" "Z")))
         (target (first letters))
         (foil (second letters))
         (window (open-exp-window "Letter difference"))
         (text1 foil)
         (text2 foil)
         (text3 foil))       
    
    
    (case (act-r-random 3)
      (0 (setf text1 target))
      (1 (setf text2 target))
      (2 (setf text3 target)))
    
    (add-text-to-exp-window :text text1 :x 125 :y 75)
    (add-text-to-exp-window :text text2 :x 75 :y 175)
    (add-text-to-exp-window :text text3 :x 175 :y 175)
    
    (setf *response* nil)
    
    (if (not (eq who 'human)) 
        (progn
          (install-device window)
          (proc-display)
          (run 10 :real-time t))
      (while (null *response*)
        (allow-event-manager window)))
    
    (if (string-equal *response* target)
        'correct
      nil)))



(define-model unit2
    
(sgp :v t :show-focus t :needs-mouse nil)


(chunk-type read-letters state)
(chunk-type array letter1 letter2 letter3)

(add-dm 
 (start isa chunk)
 (goal isa read-letters state start))

 
(goal-focus goal)
)
