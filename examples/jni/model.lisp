(handler-case
    (asdf:load-system 'actr6)
  (error () #-:ACT-R-6.0 (load "~/workspace/actr6/load-act-r-6.lisp")))

(define-model json-test
  
  (chunk-type (visual-location-ext (:include visual-location)) quad)
  (chunk-type (fixation-cross (:include visual-object)))
  (chunk-type (letterobj (:include visual-object)) quad)
  (chunk-type (background (:include visual-object)))
  (chunk-type color-opposite color opposite)
  (chunk-type find-color found quad)
  (chunk-type get-direction)
  (chunk-type search-direction color direction)
  (chunk-type oddball)
  (chunk-type respond start-quad start-color bg-color direction letter)
  (chunk-type quad quad clockwise counter-clockwise)
  (chunk-type intro)
  (chunk-type trial)
  
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
  (start-hand-at-mouse)
  
  (p find-x
     ?goal>
     buffer empty
     =visual-location>
     isa visual-location
     - kind letterobj
     - color red
     ==>
     +visual-location>
     isa visual-location
     kind letterobj
     color red
     )
  
  (p found-x
     ?goal>
     buffer empty
     =visual-location>
     isa visual-location
     kind letterobj
     color red
     ?manual>
     preparation free
     ==>
     +goal>
     isa intro
     +manual>
     isa move-cursor
     loc =visual-location
     )
  
  (p click-x
     =goal>
     isa intro
     ?manual>
     preparation free
     ==>
     +manual>
     isa click-mouse
     +goal>
     isa trial
     -imaginal>
     )
  
  (p start-trial
     =goal>
     isa trial
     ?imaginal>
     buffer empty
     ?manual>
     preparation free
     ==>
     +goal>
     isa fixation-cross
     +manual>
     isa hand-to-home
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
     preparation free
     ==>
     +manual>
     isa press-key
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
     +visual-location>
     isa visual-location
     kind background
     )
  
  (p found-bg
     =goal>
     isa chunk
     =visual-location>
     isa visual-location
     kind background
     color =bg-color
     =imaginal>
     isa respond
     bg-color nil
     direction nil
     ==>
     =imaginal>
     bg-color =bg-color
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
      - direction nil
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
     +goal>
     isa trial
     -imaginal>
     -aural-location>
     )
  
  )

(defun test-ji ()
  (if (install-device (json-interface "localhost" 5555 :sync .1))
      (let ((start (get-internal-real-time)))
	(run-full-time 30 :real-time nil)
	(format nil "~f" (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))