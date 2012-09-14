;;; Simple example of multiple window AGI use.
;;; This file defines two models and each one 
;;; will be interacting with its own window
;;; titled test.  

;;; To run the example call the function run-it
;;; providing the optional parameter as t to use
;;; visible windows.


(clear-all)

(defvar *count*)

(defun run-it (&optional (visible nil))
  (reset)
  (setf *count* 0)
  (set-up 'model-1 visible)
  (set-up 'model-2 visible)
  (run 1 :real-time visible))

(defun set-up (model visible)
  ;; in the context of the named mode
  (with-model-eval model 
    ;; Open a window titled test
    (let ((w1 (open-exp-window 'test :visible visible))) 
      ;; Add the text of a number to that window.
      ;; Notice that within the model the window can be referenced by title
      ;; even though there may be multiple windows with that title in different
      ;; models.
      (add-text-to-exp-window :window 'test :text (princ-to-string (incf *count*)) :x 40 :y 40)
      ;; make that the current device for this model
      (install-device w1)
      (proc-display))))

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (model-output "Model ~s pressed key ~c" (current-model) key))

    
(define-model model-1
  (sgp :v t)
  (sgp :show-focus t)
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
  (sgp :v t)
  (sgp :show-focus t)
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
