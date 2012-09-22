#-:ACT-R-6.0 (load "~/workspace/actr6/load-act-r-6.lisp")
  
(define-model jni-test
  
  (chunk-type (visual-location-ext (:include visual-location)) quad)
  (chunk-type (fixation-cross (:include visual-object)))
  (chunk-type (letterobj (:include visual-object)) quad)
  (chunk-type (background (:include visual-object)))
  (chunk-type color-opposite color opposite)
  (chunk-type find-color found quad)
  (chunk-type find-bg)
  (chunk-type get-direction)
  (chunk-type search-direction color direction)
  (chunk-type oddball)
  (chunk-type respond start-quad start-color bg-color direction letter)
  (chunk-type quad quad clockwise counter-clockwise)
  
  (add-dm
   (o-blue isa color-opposite color blue opposite red)
   (o-red isa color-opposite color red opposite blue)
   (d-white isa search-direction color white direction counter-clockwise)
   (d-back isa search-direction color black direction clockwise)
   (quad1 isa quad quad 1 clockwise 2 counter-clockwise 4)
   (quad2 isa quad quad 2 clockwise 3 counter-clockwise 1)
   (quad3 isa quad quad 3 clockwise 4 counter-clockwise 2)
   (quad4 isa quad quad 4 clockwise 1 counter-clockwise 3))
  
  (sgp :v t) 
  (sgp :needs-mouse nil :process-cursor t)
  ;(start-hand-at-mouse)
  
  (p start-trial
     ?goal>
     buffer empty
     ?imaginal>
     buffer empty
     ==>
     +goal>
     isa fixation-cross
     )
  
  (p find-fixation-cross
     =goal>
     isa fixation-cross
     screen-pos nil
     ?visual-location>
     buffer empty
     ==>
     +visual-location>
     isa visual-location
     kind fixation-cross
     )  
  
  (p found-fixation-cross
     =goal>
     isa fixation-cross
     screen-pos nil
     =visual-location>
     isa visual-location
     kind fixation-cross
     ?visual>
     state free
     ==>
     =goal>
     screen-pos =visual-location
     +visual>
     isa move-attention
     screen-pos =visual-location
     )
  
  (p attending-fixation-cross
     =goal>
     isa fixation-cross
     =visual>
     isa fixation-cross
     ?manual>
     state free
     ==>
     +manual>
     ISA press-key
     key "space"
     +imaginal>
     isa respond
     +goal>
     isa chunk
     )
  
  (p find-bg
     =goal>
     isa chunk
     =imaginal>
     isa respond
     bg-color nil
     direction nil
     ==>
     =imaginal>
     +goal>
     isa find-bg
     +visual-location>
     isa visual-location
     kind background
     )
  
  (p found-bg
     =goal>
     isa find-bg
     =visual-location>
     isa visual-location
     kind background
     color =bg-color
     =imaginal>
     isa respond
     bg-color nil
     ==>
     =imaginal>
     bg-color =bg-color
     +goal>
     isa chunk
     )
  
  (p find-color
     =goal>
     isa chunk
     =imaginal>
     isa respond
     start-color nil
     ?visual-location>
     buffer empty
     - state error
     ==>
     =imaginal>
     +visual-location>
     isa visual-location-ext
     kind letterobj
     )
  
  (p found-color
     =goal>
     isa chunk
     =visual-location>
     isa visual-location-ext
     kind letterobj
     color =color
     quad =quad
     =imaginal>
     isa respond
     start-color nil
     ==>
     =imaginal>
     +goal>
     isa find-color
     found =color
     quad =quad
     )
  
  (p find-same-color
     =goal>
     isa find-color
     found =color
     quad =quad
     =imaginal>
     isa respond
     start-color nil
     ==>
     =imaginal>
     +visual-location>
     isa visual-location-ext
     kind letterobj
     color =color
     - quad =quad
     )
  
  (p found-same-color
     =visual-location>
     isa visual-location-ext
     kind letterobj
     color =color
     =goal>
     isa find-color
     found =color
     =imaginal>
     isa respond
     start-color nil
     ==>
     =imaginal>
     +retrieval>
     isa color-opposite
     color =color
     +goal>
     isa oddball
     )
  
  (p found-no-same-color
     =goal>
     isa find-color
     found =color
     quad =quad
     =imaginal>
     isa respond
     start-color nil
     ?visual-location>
     state error
     ==>
     =imaginal>
     start-color =color
     +goal>
     isa chunk
     )
  
  (p found-oddball
     =goal>
     isa oddball
     =retrieval>
     isa color-opposite
     opposite =color
     =imaginal>
     isa respond
     start-color nil
     ==>
     =imaginal>
     start-color =color
     +goal>
     isa chunk
     )
  
  (p get-direction
     =goal>
     isa chunk
     =imaginal>
     isa respond
     bg-color =bg-color
     direction nil
     ?retrieval>
     buffer empty
     ==>
     =imaginal>
     +retrieval>
     isa search-direction
     color =bg-color
     +goal>
     isa get-direction
     )
  
  (p set-direction
     =goal>
     isa get-direction
     =imaginal>
     isa respond
     bg-color =bg-color
     direction nil
     =retrieval>
     isa search-direction
     direction =direction
     ==>
     =imaginal>
     direction =direction
     +goal>
     isa chunk
     )
  
  (p start-responding
     =goal>
     isa chunk
     =imaginal>
     isa respond
     - direction nil
     direction =direction
     - start-color nil
     start-color =start-color
     start-quad nil
     ==>
     =imaginal>
     +visual-location>
     isa visual-location-ext
     kind letterobj
     color =start-color
     )
  
  (p set-start-quad
     =goal>
     isa chunk
     =imaginal>
     isa respond
     - direction nil
     direction =direction
     - start-color nil
     start-color =start-color
     start-quad nil
     =visual-location>
     isa visual-location-ext
     kind letterobj
     color =start-color
     quad =quad
     ==>
     =visual-location>
     +goal>
     isa respond
     direction =direction
     start-color =start-color
     start-quad =quad
     )
  
  (p found-response-letter
     =goal>
     isa respond
     - start-quad nil
     =visual-location>
     isa visual-location-ext
     kind letterobj
     ==>
     +visual>
     isa move-attention
     screen-pos =visual-location
     )
  
  (p* respond-letter
      =goal>
      isa respond
      start-quad =start-quad
      letter nil
      =visual>
      isa letterobj
      value =letter
      quad =quad
      ?manual>
      preparation free 
      ==>
      +retrieval>
      isa quad
      quad =quad
      +manual>
      ISA press-key
      key =letter
      =goal>
      letter =letter
      )
  
  (p* get-next-response
      =goal>
      isa respond
      - letter nil
      direction =direction
      start-quad =start-quad
      =retrieval>
      isa quad
      =direction =next-quad
      - =direction =start-quad
      ==>
      +visual-location>
      isa visual-location-ext
      kind letterobj
      quad =next-quad
      =goal>
      letter nil
      )
  
  (p restart
     ?aural-location>
     buffer full
     ?manual>
     preparation free
     ==>
     +manual>
     ISA press-key
     key "space"
     -goal>
     -imaginal>
     -aural-location>
     )
  
  )

(defun test-jni (host port)
  (if (install-device (jni-device host port))
      (run 10 :real-time T)))

(defun restart-test ()
  (if (current-model)
      (progn
        (reset)
      	(install-device (get-module jni))
        (run 10 :real-time T))))