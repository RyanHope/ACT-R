;;; This file creates three models in the same meta-process
;;; which are able to hear each other talking through the use
;;; of a custom device.
;;;
;;; To run the model just call the test-it function.  That
;;; will create the instance of the device, install it for
;;; each model and then run them.

(clear-all)

;;; Creating a custom device to allow all models to hear
;;; each other when they talk. 

(defstruct conference-call)

;;; Define the motor and visual methods, but don't do anything
;;; with them i.e. a model's motor actions don't cause any
;;; effects and it can't see anything.  They need to be defined
;;; to prevent possible errors, but most can just return nil to 
;;; be ignored.

(defmethod device-move-cursor-to ((device conference-call) loc))
(defmethod device-handle-click ((device conference-call)))
(defmethod device-handle-keypress ((device conference-call) key))
(defmethod cursor-to-vis-loc ((device conference-call)))
(defmethod build-vis-locs-for ((device conference-call) vis-mod))
(defmethod vis-loc-to-obj ((device conference-call) vis-loc))

;;; This one always needs to return a vector however so just
;;; return 0,0 everytime.

(defmethod get-mouse-coordinates ((device conference-call))
  (vector 0 0))

;;; This is the key to them hearing each other.
;;; When anyone speaks all other models hear that with the
;;; location being the name of the model which spoke.
;;; The audio/speech modules take care of a model hearing
;;; itself automatically so don't want to double that up.

(defmethod device-speak-string ((device conference-call) string)
  (let ((originator (current-model))) ; record which model spoke
    (dolist (model (mp-models))
      (unless (eq model (current-model)) ; for everyone but the originator
        
        (with-model-eval model ; make the other model the current-model
          
          ;; Create the originating model's name as a simple chunk if
          ;; it isn't one already to avoid a warning of it being
          ;; created by default when the sound is generated.
          
          (unless (chunk-p-fct originator)
            (define-chunks-fct `((,originator isa chunk))))
          
          ;; Create the sound as a word with location indicating
          ;; the speaker.
          
          (new-word-sound string (mp-time) originator))))))
  

(defun test-it ()
  ;; Make one device for the models.
  
  (let ((d (make-conference-call)))
    (reset)
    
    ;; Install that device for each model.
    ;; They don't have to share the device for this to work 
    ;; since separate devices would also do the same thing
    ;; since it's the same method that would be called, but 
    ;; there's also no advantage to separate devices since
    ;; it doesn't do anything else.
    
    (dolist (x (mp-models))
      (with-model-eval x
        (install-device d)))
    
    (run 10)))


;;; Each model will output what it hears and from whom in 
;;; the trace.  M2 will start things off by saying "hello" 
;;; and M1 will respond to anything which it hears by 
;;; saying "back at you".  M3 doesn't do anything other 
;;; than listen.

;;; One note about the models is that the current audio
;;; system is not as flexible as it probably should be.
;;; In particular, it isn't possible to put a negation 
;;; into the audloc search spec (buffer stuffing control)
;;; or in the aural-location requests.  So it can't be set up 
;;; to only stuff or find sounds where "location is not self".
;;; That would be fine if there was some other slot where the 
;;; speaker could be placed into the audio event so that the 
;;; location slot could be left as "external", but the current 
;;; audio generation commands don't have that option. I could
;;; create my own sound-event objects which put things into
;;; other slots and make those available to the audio module, 
;;; but it's easier for this test to just have a model
;;; explictily ignore itself.


(define-model m1
    
  (chunk-type goal speaker)
  
  (goal-focus-fct (car (define-chunks (isa goal))))

  (p ignore-self
     =goal>
       isa      goal
     =aural-location>
       isa      audio-event
       location self
     ?aural>
       state    free
   ==>
     =goal>
       speaker  nil
     +aural>
       isa      sound
       event    =aural-location)
  

  (p detected-sound
     =goal>
       isa      goal
     =aural-location>
       isa      audio-event
       location =who
     - location self
     ?aural>
       state    free
   ==>
     =goal>
       speaker  =who
     +aural>
       isa      sound
       event    =aural-location)
  
  (p hear
     =goal>
       isa     goal
       speaker =who
     =aural>
       isa     sound
       content =x
     ?vocal>
       state   free
   ==>
     +vocal>
       isa     speak
       string  "Back at you"
     !output! (I heard =who say =x))
)


(define-model m2
    
  (chunk-type goal speaker)

  (p speak
     ?goal>
       buffer empty
     ?vocal>
       state  free
   ==>
     +goal>
       isa    goal
     +vocal>
       isa    speak
       string "Hello")
  
  (p ignore-self
     =goal>
       isa      goal
     =aural-location>
       isa      audio-event
       location self
   ==>
     =goal>
       speaker  nil
     +aural>
       isa      sound
       event    =aural-location)
  

  (p detected-sound
     =goal>
       isa      goal
     =aural-location>
       isa      audio-event
       location =who
     - location self
     ?aural>
       state    free
   ==>
     =goal>
       speaker  =who
     +aural>
       isa      sound
       event    =aural-location)
  
  (p hear
     =goal>
       isa     goal
       speaker =who
     =aural>
       isa     sound
       content =x
   ==>
     !output!  (I heard =who say =x))
)

  
(define-model m3
   
  (chunk-type goal speaker)
  
  (goal-focus-fct (car (define-chunks (isa goal))))

  (p ignore-self
     =goal>
       isa      goal
     =aural-location>
       isa      audio-event
       location self
   ==>
     =goal>
       speaker  nil
     +aural>
       isa      sound
       event    =aural-location)
  
  (p detected-sound
     =goal>
       isa      goal
     =aural-location>
       isa      audio-event
       location =who
     - location self
     ?aural>
       state    free
   ==>
     =goal>
       speaker  =who
     +aural>
       isa      sound
       event    =aural-location)
  
  (p hear
     =goal>
       isa     goal
       speaker =who
     =aural>
       isa     sound
       content =x
   ==>
     !output!  (I heard =who say =x))
)