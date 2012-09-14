(defconstant *person-location-data* '(1.11 1.17 1.22
                                       1.17 1.20 1.22
                                       1.15 1.23 1.36
                                       1.20 1.22 1.26
                                       1.25 1.36 1.29
                                       1.26 1.47 1.47))

(defvar *response*)
(defvar *response-time*)

(defun test-sentence-model (person location target term)
  (let ((window (open-exp-window "Sentence Experiment" 
                                 :visible nil
                                 :width 600 
                                 :height 300))
        (x 25))
    
    (reset)
   
    (install-device window)
    
    (case term 
      (person (spp retrieve-from-person :c 0))
      (location (spp retrieve-from-location :c 0)))
  
   (dolist (text (list "The" person "is" "in" "the" location))
     (add-text-to-exp-window :text text :x x :y 150 :width 75)
     (incf x 75))
    
    (setf *response* nil)
    (setf *response-time* nil)
    
    
    (proc-display)
    
    (run 30)
    
    (if (null *response*)
        (list 30.0 nil)
      (list (/ *response-time* 1000.0)
            (or (and target (string-equal *response* "k"))
                (and (null target) (string-equal *response* "d")))))))



#| This version of the test-sentence-model function
   runs the model without using the visual interface.
   It is provided as an example of showing how one could
   bypass the interface when it isn't really necessary.
   The difference is explained in the experiment description
   text.
|#
#|
(defun test-sentence-model (person location target term)
  
  (reset)
  
  (case term 
    (person (spp retrieve-from-person :c 0))
    (location (spp retrieve-from-location :c 0)))
  
  (mod-chunk-fct 'goal (list 'arg1 person 'arg2 location 'state 'test))
  
  (setf *response-time* (run 30.0))
  
  (setf *response* (chunk-slot-value-fct (buffer-read 'goal) 'state))
  
  (list *response-time*
        (or (and target (string-equal *response* "k"))
            (and (null target) (string-equal *response* "d")))))

|#

(defun test-sentence-person (person location target term)
  (let ((window (open-exp-window "Sentence Experiment" :width 600 :height 300))
        (x 25)
        (start-time))
    
   (dolist (text (list "The" person "is" "in" "the" location))
     (add-text-to-exp-window :text text :x x :y 150)
     (incf x 75))
        
    (setf *response* nil)
    (setf *response-time* nil)
    
    (setf start-time (get-time))
    
    (while (null *response*) ;; wait for a key to be pressed by a person
           (allow-event-manager window))
    
    (list (/ (- *response-time* start-time) 1000.0)
          (or (and target (string-equal *response* "k"))
              (and (null target) (string-equal *response* "d"))))))


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response-time* (get-time))
  
  (setf *response* (string key)))


(defun do-person-location (term &optional (in-order *actr-enabled-p*)) 
  (let ((test-set '(("lawyer" "store" t)("captain" "cave" t)("hippie" "church" t)
                      ("debutante" "bank" t)("earl" "castle" t)("hippie" "bank" t)
                      ("fireman" "park" t)("captain" "park" t)("hippie" "park" t)
                      ("fireman" "store" nil)("captain" "store" nil)("giant" "store" nil)
                      ("fireman" "bank" nil)("captain" "bank" nil)("giant" "bank" nil)
                      ("lawyer" "park" nil)("earl" "park" nil)("giant" "park" nil)))
        (results nil))
    
    (dolist (sentence (if in-order test-set (permute-list test-set)))
      (push (list sentence 
                  (apply (if *actr-enabled-p* #'test-sentence-model #'test-sentence-person) 
                         (append sentence (list term))))
            results))
     (mapcar #'second (sort results #'< :key #'(lambda (x) (position (car x) test-set))))))

(defun average-person-location ()
  (output-person-location (mapcar #'(lambda (x y) 
                                      (list (/ (+ (car x) (car y)) 2.0) 
                                            (and (cadr x) (cadr y))))
                            (do-person-location 'person) 
                            (do-person-location 'location))))

(defun output-person-location (data)
  (let ((rts (mapcar 'first data)))
    (correlation rts *person-location-data*)
    (mean-deviation rts *person-location-data*)
    (format t "~%TARGETS:~%                         Person fan~%")
    (format t  "  Location      1             2             3~%")
    (format t "    fan")
    
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~{~8,3F (~3s)~}" (nth (+ j (* i 3)) data))))
    
    (format t "~%~%FOILS:")
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~{~8,3F (~3s)~}" (nth (+ j (* (+ i 3) 3)) data))))))

(clear-all)

(define-model fan

(sgp :v nil :esc t :lf 0.63 :mas 1.6 :act nil) 

(chunk-type comprehend-sentence relation arg1 arg2 state)
(chunk-type meaning word)

(add-dm
    (p1 ISA comprehend-sentence relation in arg1 hippie arg2 park)
    (p2 ISA comprehend-sentence relation in arg1 hippie arg2 church)
    (p3 ISA comprehend-sentence relation in arg1 hippie arg2 bank)
    (p4 ISA comprehend-sentence relation in arg1 captain arg2 park)
    (p5 ISA comprehend-sentence relation in arg1 captain arg2 cave)
    (p6 ISA comprehend-sentence relation in arg1 debutante arg2 bank)
    (p7 ISA comprehend-sentence relation in arg1 fireman arg2 park)
    (p8 ISA comprehend-sentence relation in arg1 giant arg2 beach)
    (p9 ISA comprehend-sentence relation in arg1 giant arg2 castle)
    (p10 ISA comprehend-sentence relation in arg1 giant arg2 dungeon)
    (p11 ISA comprehend-sentence relation in arg1 earl arg2 castle)
    (p12 ISA comprehend-sentence relation in arg1 earl arg2 forest)
    (p13 ISA comprehend-sentence relation in arg1 lawyer arg2 store)
    (guard ISA meaning word "guard")
    (beach ISA meaning word "beach")
    (castle ISA meaning word "castle")
    (dungeon ISA meaning word "dungeon")
    (earl ISA meaning word "earl")
    (forest ISA meaning word "forest")
    (giant ISA meaning word "giant")
    (hippie ISA meaning word "hippie")
    (park ISA meaning word "park")
    (church ISA meaning word "church")
    (captain ISA meaning word "captain")
    (cave ISA meaning word "cave")
    (debutante ISA meaning word "debutante")
    (bank ISA meaning word "bank")
    (fireman ISA meaning word "fireman")
    (lawyer ISA meaning word "lawyer")
    (store ISA meaning word "store")
    (in ISA meaning word "in")
    (goal ISA comprehend-sentence))

;; This is the visual/motor solution
;;#|

(P find-person
     =goal>
       ISA         comprehend-sentence
     ?visual-location>
       buffer      unrequested
   ==>
    +visual-location>
       ISA         visual-location
       > screen-x    105 
       < screen-x    135
)

(P attend-person
    =goal>
       ISA         comprehend-sentence
       arg1        nil
   =visual-location>
       ISA         visual-location
   ?visual-location>
       buffer      requested
   ?visual>
       state       free
   ==>
    +visual>
       ISA         move-attention
       screen-pos  =visual-location
)

(P retrieve-person
    =goal>
       ISA         comprehend-sentence
       arg1        nil
    =visual>
       ISA         text
       value       =word
==>
    +retrieval>
       ISA         meaning
       word        =word
)

(P encode-person
    =goal>
       ISA         comprehend-sentence
       arg1        nil
    =retrieval>
       ISA         meaning
==>
    =goal>
       arg1        =retrieval
    +visual-location>
       ISA         visual-location
       > screen-x    400
       < screen-x    430
)

(P attend-location
    =goal>
       ISA         comprehend-sentence
       arg1        =person
    =visual-location>
       ISA         visual-location
   ?visual>
       state   free
==>
    +visual>
       ISA         move-attention
       screen-pos  =visual-location
)

(P retrieve-location
    =goal>
       ISA         comprehend-sentence
       arg1        =person
   =visual>
       ISA         text
       value       =word
==>
    +retrieval>
       ISA         meaning
       word        =word
)


(P encode-location
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       
   =retrieval>
       ISA         meaning
==>
    =goal>
       arg2        =retrieval
)

(P retrieve-from-person
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
   ?retrieval>
       state       free
       buffer      empty 
==>
    +retrieval>
       ISA         comprehend-sentence
       arg1        =person
)

(P retrieve-from-location
    =goal>
       ISA         comprehend-sentence
       arg2        =location
       arg1        =person
   ?retrieval>
       state       free
       buffer      empty 
==>
    +retrieval>
       ISA         comprehend-sentence
       arg2        =location
)


(P yes
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
    =retrieval>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
    ?manual>   
     state free   
==>
    -goal>
    +manual>
       ISA         press-key
       key         "k"
   
   )

(P mismatch-person
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
    =retrieval>
       ISA         comprehend-sentence
    -  arg1        =person
    ?manual>   
       state free   
==>
    -goal>
    +manual>
       ISA         press-key
       key         "d"

)

(P mismatch-location
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
    =retrieval>
       ISA         comprehend-sentence
    -  arg2        =location
    ?manual>   
       state free   
==>
    -goal>
    +manual>
       ISA         press-key
       key         "d"
)

;;|#

(setf *actr-enabled-p* t)

(set-base-levels
 (guard 10) (beach 10) (castle 10) (dungeon 10) (earl 10) 
 (forest 10) (hippie 10) (park 10) (church 10) (bank 10) 
 (captain 10) (cave 10) (giant 10) (debutante 10) (fireman 10)
 (lawyer 10) (store 10) (in 10))

(goal-focus goal)




#| The solution commented out here works with the
   alternate test-sentence-model that does not use the
   visual interface for input or the motor module for
   a response and produces the same results at the 
   cost of estimating the parameters for the efforts 
   of the productions for encoding and responding.

   The benefits are a simpler model and a faster running
   time (approximately half the time to run the above
   version with a virtual interface).
|#
#|
(P start
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       state       test   
==>
    =goal>
       state       harvest-person
    +retrieval>
       ISA         meaning
       word        =person
)

(P harvest-person
    =goal>
       ISA         comprehend-sentence
       arg2        =location
       state       harvest-person
    =retrieval>
       ISA         meaning
==>
    =goal>
       arg1        =retrieval
       state       harvest-location
    +retrieval>
       ISA         meaning
       word        =location
)

(p harvest-location
   =goal>
       ISA         comprehend-sentence
       state       harvest-location
   =retrieval>
       ISA         meaning
==>
   =goal>
       arg2        =retrieval
       state       get-retrieval
)


(P retrieve-from-person
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       state       get-retrieval
==>
    =goal>
       state       nil
    +retrieval>
       ISA         comprehend-sentence
       arg1        =person
)

(P retrieve-from-location
    =goal>
       ISA         comprehend-sentence
       arg2        =location
       state       get-retrieval
==>
    =goal>
       state       nil
    +retrieval>
       ISA         comprehend-sentence
       arg2        =location
)

(P respond-yes
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
       state       nil
    =retrieval>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
==>
    =goal>
       state       "k"
       
)

(P mismatch-person-no
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
       state       nil
    =retrieval>
       ISA         comprehend-sentence
    -  arg1        =person
==>
    =goal>
       state       "d"
)

(P mismatch-location-no
    =goal>
       ISA         comprehend-sentence
       arg1        =person
       arg2        =location
       state       nil
    =retrieval>
       ISA         comprehend-sentence
    -  arg2        =location
==>
    =goal>
       state       "d"
)


(spp mismatch-location-no :effort .21)
(spp mismatch-person-no :effort .21)
(spp respond-yes :effort .21)
(spp start :effort .26)
(spp harvest-person :effort .26)

|#


)

