(clear-all)

(defvar *mouse-pos* (vector 0 0))

(defmethod device-move-cursor-to ((device list) loc)
  (model-output "Model moved mouse to ~A" loc)
  (setf *mouse-pos* loc))

(defmethod get-mouse-coordinates ((device list))
  *mouse-pos*)

(defmethod device-handle-click ((device list))
  (model-output "Model clicked the mouse"))

(defmethod device-handle-keypress ((device list) key)
  (model-output "Model pressed key ~c" key))

(defmethod device-speak-string ((device list) string)
  (model-output "Model said ~s" string))

(defmethod cursor-to-vis-loc ((device list))
  nil)

(defmethod build-vis-locs-for ((device list) vis-mod)
  (mapcar 'car device))

(defmethod vis-loc-to-obj ((device list) vis-loc)
  (cdr (assoc vis-loc device)))


(defun do-experiment ()
  
  (reset)
  
  (let* ((visual-location-chunks (define-chunks 
                                     (isa visual-location screen-x 10 screen-y 20 kind oval value oval height 10 width 40 color blue)
                                     (isa polygon-feature screen-x 50 screen-y 50 kind polygon value polygon height 50 width 40 color blue regular nil)
                                     (isa polygon-feature screen-x 10 screen-y 50 kind square value square height 30 width 30 color red regular t)
                                     (isa polygon-feature screen-x 90 screen-y 70 kind polygon value polygon height 50 width 45 color green regular nil)))
         
         
         (visual-object-chunks (define-chunks
                                   (isa oval value "The oval" height 10 width 40 color blue)
                                   (isa polygon value "Poly 1" height 20 width 40 color blue sides 7)
                                   (isa square value "The square" height 30 width 30 color red)
                                   (isa polygon value "Poly 2" height 50 width 45 color green sides 5)))
         
         (the-device (pairlis visual-location-chunks visual-object-chunks)))
    
    
    (install-device the-device)
    
    (proc-display)
    (print-visicon)
    
    (run 10)))

 
(define-model device-test
    
    (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  (chunk-type (square (:include polygon)) (sides 4))

    (start-hand-at-mouse)
    
    (p start
       ?goal>
       buffer empty
       ==>
       +visual-location>
       isa visual-location 
       kind oval
       +manual>
       isa press-key
       key "a"
       +goal>
       isa chunk)
  (p found-oval
     =visual-location>
     isa visual-location
     kind oval
     ?visual>
     state free
     ==>
     +visual>
     isa move-attention
     screen-pos =visual-location)
  (p attended-oval
     =visual>
     isa oval
     ?manual>
     state free
     ==>
     +manual>
     isa move-cursor
     object =visual
     +visual-location>
     isa polygon-feature)
  (p found-poly
     =goal>
      isa chunk
     =visual-location>
     isa polygon-feature
     ?manual>
     state free
     ==>
     +visual>
     isa move-attention
     screen-pos =visual-location
     +manual>
     isa click-mouse
     )
  (p attending-poly
     =visual>
     isa polygon
     ?vocal>
     state free
     ==>
     +vocal>
     isa speak
     string "Done")
  )
  