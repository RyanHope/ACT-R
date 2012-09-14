;;; This is a simple example model to show the new options of
;;; clockwise and counterclockwise which can be used with the
;;; :nearest request parameter in a visual-location request.
;;; 
;;; The display is a clock face in an experiment window.  The
;;; model finds the top most number (12), attends to it, and
;;; then finds and attends to items clockwise until it returns
;;; to a number it has seen before.  Then, it switches to finding
;;; and attending to items counterclockwise until it again 
;;; returns to a previously seen number.

(clear-all)

(defun run-test ()
  ;; open a visible experiment window and make that the current device
  ;; the window has the default width and height of 300 pixels each
  (install-device (open-exp-window "Test"))
  
  ;; draw the clock face
  (add-text-to-exp-window :text "12" :x 150 :y 50 :width 25)
  (add-text-to-exp-window :text "1" :x 200 :y 63  :width 25)
  (add-text-to-exp-window :text "2" :x 237 :y 100  :width 25)
  (add-text-to-exp-window :text "3" :x 250 :y 150  :width 25)
  (add-text-to-exp-window :text "4" :x 237 :y 200 :width 25)
  (add-text-to-exp-window :text "5" :x 200 :y 237 :width 25)
  (add-text-to-exp-window :text "6" :x 150 :y 250 :width 25)
  (add-text-to-exp-window :text "7" :x 100 :y 237 :width 25)
  (add-text-to-exp-window :text "8" :x 63 :y 200 :width 25)
  (add-text-to-exp-window :text "9" :x 50 :y 150 :width 25)
  (add-text-to-exp-window :text "10" :x 63 :y 100 :width 25)
  (add-text-to-exp-window :text "11" :x 100 :y 63 :width 25)       
  
  (proc-display)
  
  ;; We do not need to set the center point because open-exp-window
  ;; sets it to the middle of the window automatically, but for a
  ;; device not created by open-exp-window one will have to explicitly
  ;; set a center if needed or have the model specify its own center.
  ;;
  ;;(set-visual-center-point 150 150)
  
  (run 10 :real-time t))


(define-model test-rotation
    
    (sgp :show-focus t)
    
  (chunk-type goal start dir current)
  
  (p start
     ?goal>
       buffer empty
     ==>
     +visual-location>
       isa visual-location
       screen-y lowest
     +goal>
       isa goal
       dir clockwise)
    
  (p attend
     =goal>
       isa goal
     =visual-location>
       isa visual-location
     ?visual>
       state free
       buffer empty
     ==>
     +visual>
       isa move-attention
       screen-pos =visual-location)
  
  (p encode-and-record
     =goal>
       isa goal
       start nil
       dir =dir
     =visual>
       isa text
       value =val
     ==>
     =goal>
       start =val
       current =val)
  
  (p encode
     =goal>
       isa goal
      - start =val
      - start nil
       dir =dir
     =visual>
       isa text
       value =val
     ==>
     =goal>
       current =val)

  
  (p encode-and-switch
     =goal>
       isa goal
       start =val
       dir clockwise
     =visual>
       isa text
       value =val
     ==>
     =goal>
       dir counterclockwise
       current =val)
  
  (p encode-and-stop
     =goal>
       isa goal
       dir counterclockwise
       start =val
     =visual>
       isa text
       value =val
     ==>
     =goal>
       dir nil
       current =val)
  
  
  (p find-next
     =goal>
       isa goal
       dir =dir
       current =val
     ?visual-location>
       buffer empty
     ?visual>
       buffer empty
       state free
     ==>
     +visual-location>
       isa visual-location
      - value =val
       :nearest =dir)
  )