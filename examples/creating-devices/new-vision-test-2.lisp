;;; This model extends on the previous one showing new components
;;; available in creating a device with respect to how the object
;;; is constructed from the visual-location feature.
;;;
;;; The two primary changes from the last example are:
;;;
;;; - using the fill-default-vis-obj-slots command to set the 
;;;   primary slots of the visual-object chunks.
;;;
;;; - Using a chunk's real-visual-value parameter to have
;;;   the value used in the visual-location be different
;;;   from the one used in the visual-object (which is
;;;   handled by the fill-default-dimensions command automatically).
;;; 
;;;
;;; This example will also show the use of the set-visloc-default 
;;; command for specifying which (if any) chunk will get stuffed into the 
;;; visual-location buffer when the screen changes.
;;;
;;;
;;; For our device we will be using a very simple representation - a list.
;;; The device for the model will simply be a list of visual-locations and
;;; then we will construct the visual-object chunks "on the fly".
;;;

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
  ;; We want to modify the chunk's real-visual-value
  ;; parameter and the actual contents of the value slot
  ;; if we haven't already done so.
  ;; 
  ;; We want to save the value slot's content as the "real"
  ;; value to use in the object and then replace the value
  ;; slot in the visual-location with the value of the kind
  ;; slot.
  
  (dolist (x device)
    (when (null (chunk-real-visual-value x))
      (setf (chunk-real-visual-value x) (chunk-slot-value-fct x 'value))
      (set-chunk-slot-value-fct x 'value (chunk-slot-value-fct x 'kind))))
    
  device)


;;; This method gets called when the model moves attention
;;; to one of the features in the visicon.  It is passed
;;; the device and the chunk name of the feature attended
;;; to (which will always be one of the ones that was 
;;; origninally provided by build-vis-locs-for).


;;; Note that this operation is the same thing that
;;; would happen if one were to skip defining the vis-loc-to-obj
;;; method - this is exactly what it will do.

(defmethod vis-loc-to-obj ((device list) vis-loc)
  ;; Given the visual-location attended to we want to create
  ;; a visual-object chunk with a chunk-type matching the
  ;; kind from the visual-location:
  
  (let ((new-object (car (define-chunks-fct `((isa ,(chunk-slot-value-fct vis-loc 'kind)))))))
    
    ;; Now, we use the fill-default-dimensions command to
    ;; set the value, color, height, and width slots of
    ;; that chunk based on what's in the visual-location chunk
    ;; which takes into account the chunk's real-visual-value
    ;; setting.  That command returns the chunk-name of
    ;; the object so we just return that from vis-loc-to-obj
    ;; because we don't need to make any other changes to it.
    
    (fill-default-vis-obj-slots new-object vis-loc)))

  
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
                                     (isa visual-location screen-x 10 screen-y 20 kind text value "ok" height 10 width 40 color blue)
                                     (isa polygon-feature screen-x 10 screen-y 50 kind square value red-square-1 height 30 width 30 color red regular t))))
    
         
    
    ;; Now just make that list the current device for the model
    
    (install-device visual-location-chunks)
    
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

(define-model new-visual-test-1
    
    (sgp :v t :trace-detail low)
  
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  (chunk-type (square (:include polygon)) (sides 4))
  (chunk-type goal step)
  
  ;; Here we set the default specification to use in finding
  ;; the chunk to stuff into the visual-location buffer upon a
  ;; screen change.  
  ;; The syntax of the specification is just as one would make the
  ;; request for a visual-location on the RHS of a production except
  ;; without the +visual-location> part (including the & variable indicator
  ;; which is now available).
  
  ;; In this case we want that to be any polygon-feature which is new to
  ;; the screen, has the same height as width and is not colored green:
  
  (set-visloc-default isa polygon-feature :attended new height &height width &height - color green)
  
  
  
  ;; Just do this to avoid the warning when the visual-locations are created
  (define-chunks (square isa chunk))
     
  (p find-oval
     ?goal>
     buffer empty
     ==>
     +goal>
     isa goal
     step 0
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
        
        +visual-location>
        isa visual-location
        kind text
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
     !output! (Note the difference in the values between the visual-location and visual-object chunks)
     !eval! (pprint-chunks-fct (list =visual))
     !bind! =next-step (1+ =step)
     =goal>
     step =next-step
  )   
  )

  
  