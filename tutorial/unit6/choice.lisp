(defvar *unit8-choice-data* '(0.66 0.78 0.82 0.84))

(defvar *response*)

(defun do-choice-person ()
  (let ((window (open-exp-window "Choice Experiment" :visible t)))
    
    (add-text-to-exp-window :text "choose" :x 50 :y 100)
    
    (setf *response* nil)
    
    (while (null *response*)        
           (allow-event-manager window))
    
    (clear-exp-window)
    
    (add-text-to-exp-window :text (if (< (act-r-random 1.0) .9) "heads" "tails")
                            :x 50 :y 100)
    
    (sleep 1.0)
    *response*))
  
 

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response* (string-upcase (string key))))

(clear-all)

(define-model choice 

 
)
