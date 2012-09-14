;;; Test model for multiple window AGI use.
;;; This test uses two windows for one model.
;;; The model will first read the letter from
;;; one window then the other window will be 
;;; installed as the current device and the 
;;; model will read the letter from it.
;;;
;;; To run the example call the run-it function and
;;; provide the optional parameter as t to show
;;; the task in real windows.

(clear-all)

(defun run-it (&optional (visible nil))
  (reset)
  
  ;;; Open two windows 
  (let ((w1 (open-exp-window 'a :visible visible))
        (w2 (open-exp-window 'b :x 0 :visible visible)))
    
    ;;; add a letter to each window
    
    (add-text-to-exp-window :window 'a :text "a" :x 40 :y 40)
    (add-text-to-exp-window :window 'b :text "b" :x 20 :y 20)
    
    ;; install the first window as the current device
    (install-device w1)
    (proc-display)
    
    ;; run the model
    (run 1 :real-time visible)
    
    ;; now install the second window as the current device
    (install-device w2)
    (proc-display)
    
    ;; run the model again
    (run 1 :real-time visible)))

(define-model test1
  (sgp :v t :show-focus t :needs-mouse nil)
  
  (p attend
     =visual-location>
       isa visual-location
     ?visual>
       state free
    ==>
     +visual>
       isa move-attention
       screen-pos =visual-location)
  
  (p report
     =visual>
       isa text
       value =x
     ==>
    
     !output! (the value is =x)))
