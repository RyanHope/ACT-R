;;; Simple example of multiple models viewing the same
;;; AGI window.
;;; This file defines two models and each one 
;;; will be interacting with the same window.  

;;; To run the example call the function run-it
;;; providing the optional parameter as t will
;;; make the window visible (if possible).


(clear-all)

(defun run-it (&optional (visible nil))
  (reset)
  (let ((window nil))
    
    ;; AGI windows must be created in the context of a model
    ;; so use model-1 to create the window.
    (with-model model-1
      (setf window (open-exp-window 'test :visible visible))
      (add-text-to-exp-window :window 'test :text "A" :x 40 :y 40 :width 15)
      (add-text-to-exp-window :window 'test :text "B" :x 60 :y 40 :width 15)
      (install-device window)
      (proc-display))
    
    (with-model model-2
      ;; install the same device for model-2
      (install-device window)
      (proc-display))
    
    (run 1 :real-time visible)
    (close-exp-window window)))


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (model-output "Model ~s pressed key ~c" (current-model) key))

    
(define-model model-1
  (sgp :v t :show-focus t)
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
       value =value
     ?manual>
       state free
     ==>
     +manual>
       isa press-key
       key =value))                      

(define-model model-2
  (sgp :v t :show-focus t)
  
  (set-visloc-default isa visual-location screen-x highest)
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
       value =value
     ?manual>
       state free
     ==>
     +manual>
       isa press-key
       key =value))  
