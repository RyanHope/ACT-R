;;; This model demonstrates how to create an abstract device using the
;;; new chunk-based vision module's device interface.  
;;;
;;; It demonstrates three primary components of the new system:
;;;
;;; - That any sub-type of visual-location chunks compose the visicon
;;;   which allows the user to extend things as needed.
;;;   Note: the visual-location chunk-type has been slightly redefined
;;;   to remove the nearest slot (which is now a request parameter),
;;;   to add in the height and width slots, and to remove the userprop
;;;   slots (since the user can now extend it with meaningful slot names
;;;   as desired).
;;;
;;; - That the requests of the visual-location buffer are now more 
;;;   flexible and even allow for variablized matching and the
;;;   nearest test is now a request parameter instead of an actual slot.
;;;
;;; - That the modeler has direct control of how the visual object chunk
;;;   is created when attention is shifted.
;;;
;;;
;;; The interface created here is one that has a fixed set of features
;;; which are presented to the model and those items each have a fixed
;;; object representation which is known in advance.  That is a bit of
;;; a contrived example, but is generally how typical static trials are
;;; presented in an experiment for a model.
;;; 
;;; For our device we will be using a very simple representation - a list.
;;; The device for the model will simply be a list of visual-location  
;;; visual-object pairs which constitute what the model can see.


(clear-all)

;;;
;;; To start we will define the general motor methods needed for the device
;;; even though we will not be using them.  Because of that there will be
;;; no actions performed by them - if the model needed them then we would
;;; need to put the appropriate code in there.


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

;;; Now we deal with the visual processing

;;; Since we don't have a mouse for our device we will also set the
;;; method that creates a feature for the cursor to do nothing -
;;; generate no visual feature for it.

(defmethod cursor-to-vis-loc ((device list))
  nil)


;;; This method gets called during proc-display to generate
;;; the features available to the model.  It must return a 
;;; list (or list of lists) of visual-location (or sub-type of 
;;; visual-location) chunk names.  

(defmethod build-vis-locs-for ((device list) vis-mod)
  ;; just return the cars from all the sublists
  (mapcar 'car device))

;;; This method gets called when the model moves attention
;;; to one of the features in the visicon.  It is passed
;;; the device and the chunk name of the feature attended
;;; to (which will always be one of the ones that was 
;;; origninally provided by build-vis-locs-for).

(defmethod vis-loc-to-obj ((device list) vis-loc)
  ;; here we're just returning the pregenerated object from the list
  (cdr (assoc vis-loc device)))


;;; Here is a function that creates a device for the model,
;;; installs it, and then runs the model.  This could be done
;;; directly in the model definition code, but it's often easier
;;; to see when separated out like this.


(defun do-experiment ()
  
  ;; Start by resetting the model.
  
  (reset)
  
  ;; First create the visual-location chunks
  ;; using define-chunks and not add-dm because we don't
  ;; want the model to already have them in DM.
  ;; We also don't care about the names, so we let the
  ;; system generate them automatically.
  ;; We also aren't specifying the distance and size slots
  ;; because they will be filled automatically by proc-display
  ;; if not provided.
  ;; Note that the polygon-feature chunk-type is defined in the model
  ;; as a sub-type of visual-location.
  
  (let* ((visual-location-chunks (define-chunks 
                                     (isa visual-location screen-x 10 screen-y 20 kind oval value oval height 10 width 40 color blue)
                                     (isa polygon-feature screen-x 50 screen-y 50 kind polygon value polygon height 50 width 40 color blue regular nil)
                                     (isa polygon-feature screen-x 10 screen-y 50 kind square value square height 30 width 30 color red regular t)
                                     (isa polygon-feature screen-x 90 screen-y 70 kind polygon value polygon height 50 width 45 color green regular nil)))
         
         ;;; Now we define the visual-object chunks that
         ;;; correspond to the visual-locations.
         ;;; Again we don't care about the names.
         ;;; Here we're specifying all the slot information.  Example 2
         ;;; will show how that can be handled more automatically.
         ;;; Also note that we don't include the screen-pos slot in the
         ;;; definition because that will be filled in automatically by
         ;;; the vision module when the object is attended.
         
         
         (visual-object-chunks (define-chunks
                                   (isa oval value "The oval" height 10 width 40 color blue)
                                   (isa polygon value "Poly 1" height 20 width 40 color blue sides 7)
                                   (isa square value "The square" height 30 width 30 color red)
                                   (isa polygon value "Poly 2" height 50 width 45 color green sides 5)))
         
         (the-device (pairlis visual-location-chunks visual-object-chunks)))
    
    ;; Now we make that the current device for the model
    
    (install-device the-device)
    
    ;; process the display 
    
    (proc-display)
    
    ;; Check the features just to be sure
    
    (print-visicon)
    
    ;; run the model
    
    (run 10)))

;;; The model is very simple in that it just repeatedly finds
;;; a location and then attends to the item there printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes.
;;;
;;; The visual-location requests are specified such that 
;;; there will only be one visual-location which matches
;;; the request.  
;;;
;;; The productions which make the visual-location requests
;;; describe the new features being used to perform the 
;;; request.

(define-model new-visual-test-1
    
    (sgp :v t :trace-detail low :save-buffer-trace t :bold-inc .1)
  
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  (chunk-type (square (:include polygon)) (sides 4))
  (chunk-type goal step)
  
  ;; Just do this to avoid the warning when the visual-locations are created
  (define-chunks (square isa chunk) (polygon isa chunk))
  
  ;; Define this chunk for use in a nearest request
  
  (define-chunks (loc-100x100 isa visual-location screen-x 100 screen-y 100))
  
  (p find-oval
     ?goal>
     buffer empty
     ==>
     +goal>
     isa goal
     step 0
     
     ;; This visual-location request is 
     ;; just like the old vision module
     ;; allowed - nothing new going on here.
     
     +visual-location>
     isa visual-location
     kind oval
     )
  
  
     (p find-regular
        =goal>
        isa goal
        step 1
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This request shows that one can use
        ;;; any sub-type of visual-location as the
        ;;; chunk-type for the request, and then
        ;;; any valid slot for that chunk-type
        ;;; is valid for the request.
        
        +visual-location>
        isa polygon-feature
        regular t
        )
    
     (p find-on-diagonal
        =goal>
        isa goal
        step 2
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This request shows the use of a request variable.
        ;;; The visual-location requets consider any value 
        ;;; which starts with an & to be a variable in the
        ;;; same way that a production uses = in the LHS matching.
        ;;; Thus, this tests that the screen-x and screen-y values
        ;;; are the same.
        ;;;
        ;;; It's important to note that this is only the case
        ;;; for +visual-location requests - not a general feature
        ;;; of RHS requests.  Other modules could use other variable
        ;;; indicators if they wanted (or none at all as all the
        ;;; other buffers' do currently do currently).
        
        +visual-location>
        isa visual-location
        screen-x &x
        screen-y &x
        )
  
     (p find-wider-than-current
        =goal>
        isa goal
        step 3
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This request shows the special marker current
        ;;; which can be used in any slot, and is compatible with the
        ;;; slot modifiers.  It substitues the corresponding value of the
        ;;; currently attended item (the most recently attended) into
        ;;; to do the matching.
        
        +visual-location>
        isa visual-location
        > width current
        )
  
  
       (p find-thinnest
        =goal>
        isa goal
        step 4
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This request shows the special marker lowest which
          ;;; can be used in any slot specified in the request.
          ;;; If there is a feature chunk which doesn't have a numerical
          ;;; value for the test then that test is ignored.
        
        +visual-location>
        isa visual-location
         width lowest
        )
  
  (p find-left-most-of-the-tallest-items
     =goal>
        isa goal
        step 5
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This request shows the special marker highest which
      ;;; can be used in any slot specified in the request
     ;;; and indicates that the ordering when multiple such 
     ;;; constraints is given is as specified by
      ;;; the request (see the next one for the other example).
     ;;; Thus, in this case first it will find the chunks which
     ;;; have the highest height and then from those find the
     ;;; one with the lowest screen-x coordinate.
     
     
        +visual-location>
        isa visual-location
     height highest
     screen-x lowest
        )
  
    (p find-tallest-of-the-left-most-items
     =goal>
        isa goal
        step 6
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
       ;;; The same request as the last one, but with the opposite
       ;;; ordering.
       ;;; So, here first it will find the locations with the lowest
       ;;; screen-x coordinate and then find the one with the highest
       ;;; height among those.
        
        +visual-location>
        isa visual-location
     screen-x lowest
     height highest
        )
  
  
     (p find-not-red-or-blue
        =goal>
        isa goal
        step 7
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        
        ;;; This request shows that now the same slot may be
        ;;; specified more than once in the request (any number
        ;;; of occurences are allowed).
        
        +visual-location>
        isa visual-location
        - color red
        - color blue
        )
  
  
     (p find-largest-x-less-than-current-x
        =goal>
        isa goal
        step 8
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This shows that not only can a slot be specifed
        ;;; more than once, but that you can use multiple special
        ;;; markers in a given slot.
        
        +visual-location>
        isa visual-location
        < screen-x current
        screen-x highest
        )
  
  
     (p find-wider-than-tall
        =goal>
        isa goal
        step 9
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This shows the use of variables in the request again,
        ;;; this time showing that they don't have to be used only
        ;;; for = tests.
        
        +visual-location>
        isa visual-location
        height &height
        > width &height
        )
  
       (p find-closest-to-100-100
        =goal>
        isa goal
        step 10
        ?visual-location>
        buffer empty
        ?visual>
        buffer empty
        state free
        ==>
        
        ;;; This shows that nearest is now a requst parameter.
          ;;; In this case taking a specific location chunk to use.
          
        +visual-location>
        isa visual-location
          :nearest loc-100x100
        )

  
  
  (p shift-attention
     =goal>
     isa goal
     =visual-location>
     isa visual-location
     ==>
     +visual>
     isa move-attention
     screen-pos =visual-location
     !output! (Here is the chunk in the visual-location buffer)
     !eval! (pprint-chunks-fct (list =visual-location)))
  
  (p attend-item
     =goal>
     isa goal
     step =step
     =visual>
     isa visual-object
     ==>
     !output! (Here is the chunk in the visual buffer)
     !eval! (pprint-chunks-fct (list =visual))
     !bind! =next-step (1+ =step)
     =goal>
     step =next-step
  )   
  )

  
  