;;; This model demonstrates a new request available through the
;;; visual-location buffer.
;;;
;;; The model can now make a request to change the default specification
;;; for buffer stuffing as can be done in code with the set-visloc-default
;;; command.
;;;
;;; This model is basically the same as the new-vision-test example in terms of
;;; the visicon constructed for the model to see, but in this task
;;; the model changes the default specification and relies on proc-display
;;; and buffer stuffing to set the chunk into the visual-location buffer
;;; instead of directly requesting it.  It doesn't attend to any of the
;;; items nor does it use as many different specifications as the
;;; original new-vision-test did just to keep things simpler.

(clear-all)

(defmethod device-move-cursor-to ((device list) loc)
  ;; ignore model's mouse move requests
  nil)

(defmethod get-mouse-coordinates ((device list))
  ;; always return the same location for the mouse
  (vector 0 0))

(defmethod device-handle-click ((device list))
  ;; ignore a mouse click
 nil)

(defmethod device-handle-keypress ((device list) key)
  ;; ignore key presses
  nil)

(defmethod device-speak-string ((device list) string)
  ;; ignore model's speech output
  nil)


(defmethod cursor-to-vis-loc ((device list))
  nil)


(defmethod build-vis-locs-for ((device list) vis-mod)
  ;; just return the cars from all the sublists
  (mapcar 'car device))

(defmethod vis-loc-to-obj ((device list) vis-loc)
  ;; here we're just returning the pregenerated object from the list
  (cdr (assoc vis-loc device)))



(defun do-experiment ()
  
  (reset)
  
  (let* ((visual-location-chunks (define-chunks 
                                     (isa visual-location screen-x 12 screen-y 20 kind oval value oval height 10 width 40 color blue)
                                     (isa polygon-feature screen-x 50 screen-y 50 kind polygon value polygon height 50 width 40 color blue regular nil)
                                     (isa polygon-feature screen-x 10 screen-y 50 kind square value square height 30 width 30 color red regular t)
                                     (isa polygon-feature screen-x 5 screen-y 70 kind polygon value polygon height 50 width 45 color green regular nil)))
         
         
         (visual-object-chunks (define-chunks
                                   (isa oval value "The oval" height 10 width 40 color blue)
                                   (isa polygon value "Poly 1" height 20 width 40 color blue sides 7)
                                   (isa square value "The square" height 30 width 30 color red)
                                   (isa polygon value "Poly 2" height 50 width 45 color green sides 5)))
         
         (the-device (pairlis visual-location-chunks visual-object-chunks)))
    
    (install-device the-device)
    
    (proc-display)
    
    (print-visicon)
    
    ;; We will have proc-display called periodically to ensure that
    ;; buffer stuffing will occur - essentially simulating a dynamic 
    ;; screen where things could be changing.
    
    (schedule-periodic-event 1.0 (lambda () (proc-display) (print-visicon)) :initial-delay .75)
    
    (run 10)))

;;; The model is very simple in that it just repeatedly changes the
;;; defaults for buffer stuffing and then prints out the chunk that
;;; was stuffed into the buffer after a proc-display occurs.
;;;

(define-model set-visloc-default-request-test
    
    (sgp :v t :trace-detail medium :save-buffer-trace t :bold-inc .1)
  
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  (chunk-type (square (:include polygon)) (sides 4))
  (chunk-type goal step)
  (chunk-type wait step)
  
  ;; Define a subtype of the set-visloc-default chunk-type that
  ;; adds the slot regular so that it can be used in setting the
  ;; default specification too.
  
  (chunk-type (polygon-visloc-spec (:include set-visloc-default)) regular)
  
  ;; Just do this to avoid the warning when the visual-locations are created
  (define-chunks (square isa chunk) (polygon isa chunk))
  
  
  (p print-and-advance
     "Just print out the chunk in the
      visual-location buffer and advance the step"
     =goal>
       isa wait
       step =step
     =visual-location>
       isa visual-location
   ==>
     !bind! =next (1+ =step)
     !output! (Here is the chunk in the visual-location buffer)
     !eval! (pprint-chunks-fct (list =visual-location))
     +goal>
       isa goal
       step =next)
  
  (p start
     ?goal>
       buffer empty
     ==>
     +goal>
       isa wait
       step 0
     !output! (The default spec is leftmost attended new))
  
     
  (p find-oval
     =goal>
       isa goal
       step 1
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 1
     +visual-location>
       isa set-visloc-default
       kind oval
     !output! (The specification is now kind oval))
  
  (p find-regular
     =goal>
       isa goal
       step 2
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 2
     
     ;; This shows the use of the subtype to specify
     ;; new slots in the subtypes of visual-location.
     ;; Note: a valid subtype of visual-location must
     ;; be specified in the type slot which has all
     ;; the new slots specified.
     
     +visual-location>
       isa polygon-visloc-spec
       type polygon-feature
       regular t
     
     !output! (The specification is now for a regular polygon feature))

  
    (p find-on-diagonal
     =goal>
       isa goal
       step 3
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 3
     
       ;; This shows that the variables allowed in the
       ;; isa visual-location requests can also be used
       ;; in specifying the defaults.
     
     +visual-location>
       isa set-visloc-default
       screen-x &x
       screen-y &x
     !output! (The specification is now that the x and y coords are equal))
  
  (p restore-default
     =goal>
      isa goal
      step 4
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 4
     +visual-location>
       isa set-visloc-default
       screen-x lowest
       :attended new
         
     !output! (The specification is now back to default - screen-x lowest :attended new which will fail since nothing is new anymore))
  )

  
  
