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
;;;
;;; The difference between this model and rotational-vision-test-1
;;; is that this model indicates the object around which it will
;;; rotate explicitly using the :center request parameter instead
;;; of using the point set with set-visual-center-point.

(clear-all)

(defun run-test ()
  ;; open a visible experiment window and make that the current device
  ;; the window has the default width and height of 300 pixels each
  (install-device (open-exp-window "Test"))
  
  ;; draw the clock face off to one side with an "X" at the center
  (add-text-to-exp-window :text "X" :x 100 :y 100 :width 25)
  (add-text-to-exp-window :text "12" :x 100 :y 30 :width 25)
  (add-text-to-exp-window :text "1" :x 135 :y 39  :width 25)
  (add-text-to-exp-window :text "2" :x 161 :y 65  :width 25)
  (add-text-to-exp-window :text "3" :x 170 :y 100  :width 25)
  (add-text-to-exp-window :text "4" :x 161 :y 135 :width 25)
  (add-text-to-exp-window :text "5" :x 135 :y 161 :width 25)
  (add-text-to-exp-window :text "6" :x 100 :y 170 :width 25)
  (add-text-to-exp-window :text "7" :x 65 :y 161 :width 25)
  (add-text-to-exp-window :text "8" :x 39 :y 135 :width 25)
  (add-text-to-exp-window :text "9" :x 30 :y 100 :width 25)
  (add-text-to-exp-window :text "10" :x 39 :y 65 :width 25)
  (add-text-to-exp-window :text "11" :x 65 :y 39 :width 25)       
  
  (proc-display)
  
  ;; We could set the center point for the model, but instead 
  ;; the model will use the location of the X that it finds as
  ;; the center.
  ;;(set-visual-center-point 100 100)
  
  (run 10 :real-time t))


(define-model test-rotation
    
    (sgp :show-focus t)
    
  (chunk-type goal start dir center-loc center-val current)
  
  (p start
     ?goal>
       buffer empty
     ==>
     +visual-location>
       isa visual-location
       value "X"
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
  
  (p encode-center
     =goal>
       isa goal
       start nil
       center-loc nil
       dir =dir
     =visual>
       isa text
       value =val
       screen-pos =loc
     ==>
     =goal>
       center-val =val
       ; we could use either =loc or =visual here because
       ; either a location or object can be used as the center
       center-loc =loc
     +visual-location>
       isa visual-location
       screen-y lowest)
  
  (p encode-and-record
     =goal>
       isa goal
       start nil
       center-loc =loc
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
       center-val =c-val
       center-loc =c-loc
     ?visual-location>
       buffer empty
     ?visual>
       buffer empty
       state free
     ==>
     +visual-location>
       isa visual-location
       - value =val
       - value =c-val
       :nearest =dir
       :center =c-loc)
  )