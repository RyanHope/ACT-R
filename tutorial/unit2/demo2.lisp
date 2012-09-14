(defvar *response* nil)
(defvar *model* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response* (string key))
  (clear-exp-window)
  (when *model* 
    (proc-display)))

(defun do-demo2 (&optional who)
  
  (reset)
  
  (if (eq who 'human)
      (setf *model* nil)
    (setf *model* t))
  
  (let* ((lis (permute-list '("B" "C" "D" "F" "G" "H" 
                              "J" "K" "L" "M" "N" "P" 
                              "Q" "R" "S" "T" "V" "W" 
                              "X" "Y" "Z")))
         (text1 (first lis))
         (window (open-exp-window "Letter recognition")))
    
    (add-text-to-exp-window :text text1 :x 125 :y 150)
    
    (setf *response* nil) 
         
    (if *model*
        (progn
          (install-device window)
          (proc-display)
          (run 10 :real-time t))
      
      (while (null *response*)
        (allow-event-manager window)))
    
    *response*))



(clear-all)

(define-model demo2

(sgp :seed (123456 0))
(sgp :v t :needs-mouse nil :show-focus t :trace-detail high)
  
(chunk-type read-letters state)
(chunk-type array letter)

(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk)
 (goal isa read-letters state start))

(P find-unattended-letter
   =goal>
      ISA         read-letters
      state       start
 ==>
   +visual-location>
      ISA         visual-location
      :attended    nil
   =goal>
      state       find-location
)

(P attend-letter
   =goal>
      ISA         read-letters
      state       find-location
   =visual-location>
      ISA         visual-location
   
   ?visual>
      state       free
   
==>
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
   =goal>
      state       attend
)

(P encode-letter
   =goal>
      ISA         read-letters
      state       attend
   =visual>
      ISA         text
      value       =letter
==>
   =goal>
      state       respond
   +imaginal>
      isa         array
      letter      =letter
)


(P respond
   =goal>
      ISA         read-letters
      state       respond
   =imaginal>
      isa         array
      letter      =letter
   ?manual>   
      state       free
==>
   =goal>
      state       done
   +manual>
      ISA         press-key
      key         =letter
)

(goal-focus goal)

)
