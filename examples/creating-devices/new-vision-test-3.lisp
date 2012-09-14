;;; This model version shows how one can have vis-loc-to-obj
;;; methods defined on classes other than the whole device
;;; (for example how the default device works by specializing
;;; on the window "component" classes).
;;;
;;; It also shows that the approach-width method from the
;;; old version has been replaced with a hook function that
;;; one sets on the visual-location (visicon feature) chunk instead.
;;; 
;;;
;;; This time the device will be a CLOS object which represents a 
;;; window and it will contain "subviews" for which we will have
;;; the features actually defined.  
;;; This is essentially how the current device interfaces work for
;;; the virtual windows and the ACL/MCL native windows.


(clear-all)

;; A variable to help record movement times

(defvar *start-time* 0)


;; The classes for the device and the "subviews" it contains

(defclass fake-window ()
  ((subviews :accessor subs :initform nil)
   (mouse-pos :accessor mouse-pos :initform (vector 0 0))))

(defclass fake-window-item ()
  ((x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   (color :accessor color :initform 0 :initarg :color)
   (height :accessor height :initform 0 :initarg :height)
   (width :accessor width :initform 0 :initarg :width)
   (kind :accessor kind :initarg :kind)))

(defclass fake-text (fake-window-item)
  ((text :accessor text :initform "" :initarg :text))
  (:default-initargs
      :kind 'text))

(defclass fake-box (fake-window-item)
  ((sides :accessor sides :initform 0 :initarg :sides)
   (special :accessor special :initform nil :initarg :special))
  (:default-initargs
      :kind 'polygon))


;;;
;;; To start we will define the general motor methods needed for the device.
;;; In this case we will be moving the mouse so set the position methods
;;; appropriately.

(defmethod device-move-cursor-to ((device fake-window) loc)
  (setf (mouse-pos device) loc))

(defmethod get-mouse-coordinates ((device fake-window))
  (mouse-pos device))

(defmethod device-handle-click ((device fake-window))
  ;; ignore a mouse click
 nil)

(defmethod device-handle-keypress ((device fake-window) key)
  ;; ignore key presses
  nil)

(defmethod device-speak-string ((device fake-window) string)
  ;; ignore model's speech output
  nil)

;;; Now we deal with the visual processing

;;; Still ignoring the mouse cursor for visual processing.

(defmethod cursor-to-vis-loc ((device fake-window))
  nil)

;;; This method gets called during proc-display to generate
;;; the features available to the model.  It must return a 
;;; list (or list of lists) of visual-location (or sub-type of 
;;; visual-location) chunk names.  

;;; Here, we're calling specific build-vis-locs-for that
;;; are defined on the specific subviews and then just
;;; collecting those into a list to return.

(defmethod build-vis-locs-for ((device fake-window) vis-mod)
  (let ((feats nil))
    (dolist (x (subs device) feats)
      (push (build-vis-locs-for x vis-mod) feats))))


     
(defmethod build-vis-locs-for ((view fake-text) vis-mod)
  (let ((vl (car (define-chunks-fct `((isa visual-location
                                      color ,(color view)
                                      screen-x ,(x view)
                                      screen-y ,(y view)
                                      height ,(height view)
                                      width ,(width view)
                                      kind ,(kind view)
                                      value ,(kind view)))))))
    
    ;; Like in the last example set the real value separately for the object
    
    (setf (chunk-real-visual-value vl) (text view))
    
    ;; Now, we're also setting the visual-object parameter of the chunk
    ;; so that it will be used for the vis-loc-to-obj call
    
    (setf (chunk-visual-object vl) view)
    
    ;; return the location
    
    vl))


(defmethod build-vis-locs-for ((view fake-box) vis-mod)
  (let ((vl (car (define-chunks-fct `((isa visual-location
                                      color ,(color view)
                                      screen-x ,(x view)
                                      screen-y ,(y view)
                                      height ,(height view)
                                      width ,(width view)
                                      kind ,(kind view)
                                      value ,(kind view)))))))
    
     
    ;; Now, we're also setting the visual-object parameter of the chunk
    ;; so that it will be used for the vis-loc-to-obj call
    
    (setf (chunk-visual-object vl) view)
    
    ;; In addition for the box, if it's marked as special then we
    ;; want to give it an approach-width function other than the 
    ;; default.
    ;; The approach-width function will be called with the visual-location
    ;; chunk and the theta value for the approach and must return a number
    ;; representing the perceived width of the item along that approach
    ;; angle.
    
    (when (special view)
      (setf (chunk-visual-approach-width-fn vl) (lambda (vis-loc theta) 20)))
    
    ;; return the location
    
    vl))



;;; These methods get called when the model moves attention
;;; to one of the features in the visicon.  Instead of being
;;; defined on the device they are defined on the classes that
;;; are used for the chunk-visual-object settings i.e. the specific
;;; subview classes.

#| We don't need to define this because the default will do the
same thing.

(defmethod vis-loc-to-obj ((view fake-text) vis-loc)
  
  (let ((new-object (car (define-chunks-fct `((isa ,(chunk-slot-value-fct vis-loc 'kind)))))))
    (fill-default-vis-obj-slots new-object vis-loc)))

|#


;; This could also be done using call-next-method to get the
;; default values filled, but it's being done directly for 
;; demonstration.

(defmethod vis-loc-to-obj ((view fake-box) vis-loc)
  
  (let ((new-object (car (define-chunks-fct `((isa ,(chunk-slot-value-fct vis-loc 'kind)))))))
    (fill-default-vis-obj-slots new-object vis-loc)
    (set-chunk-slot-value-fct new-object 'sides (sides view))
    new-object))


  
;;; Here is a function that creates a device for the model,
;;; installs it, and then runs the model.  
;;;
;;; The optional parameter determines whether or not the box at 150,50 is
;;; marked as special.  If it is marked as special, then it will have
;;; an effective approach width much larger than the default which 
;;; will make the mouse movement to that item faster.

(defun do-experiment (&optional special)
  ;; Start by resetting the model.
  
  (reset)
  
  ;; First create the device which is a fake-window
  
  (let ((device (make-instance 'fake-window)))
    
    ;; Now, we're going to explictly put some subview items into that fake-window
    
    (push (make-instance 'fake-text
            :x 10 :y 20 :color 'black :width 20 :height 10 :text "Ok")
          (subs device))
    
    (push (make-instance 'fake-box
            :x 50 :y 50 :color 'black :width 4 :height 4 :special nil :sides 4)
          (subs device))
    
    ;; Use the special parameter to mark this box 
    
    (push (make-instance 'fake-box
            :x 150 :y 50 :color 'black :width 4 :height 4 :special special :sides 4)
          (subs device))
    
    
    ;; Now make that the current device for the model
        
    (install-device device)
    
    ;; process the display 
    
    (proc-display)
    
    ;; Check the features just to be sure
    
    (print-visicon)
    
    ;; run the model
    
    (run 10)))
  
;;; The model is very simple in that it just repeatedly finds
;;; a location, attends to the item there printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes, and then moves
;;; the mouse to the corresponding item and back to the center
;;; of the screen printing out the time it took to go from
;;; the center to the item.
;;;
;;; The visual-location requests are specified such that 
;;; the model will attend to items left to right across
;;; the screen.

(define-model new-visual-test-3
    
    (start-hand-at-mouse)
  
    (sgp :v t :trace-detail high)
  
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  (chunk-type goal step)
  
  ;; Just do this to avoid the warning when the visual-locations are created
  (define-chunks (polygon isa chunk))
  
  ;; Create a chunk representing a point equidistant from the two boxes
  (define-chunks (loc-100x100 isa visual-location screen-x 100 screen-y 100))
  
  
  (p start
     "Move the cursor to a point between the the 2 boxes"
     ?goal>
       buffer empty
     ?manual>
       state free
     ?visual>
       buffer empty
     =visual-location>
       isa visual-location
     ==>
     +goal>
       isa goal
       step nil
     +manual>
       isa move-cursor
       loc loc-100x100
     )

 
  (p find-next
     "Find the next leftmost item"
     =goal>
       isa goal
       step nil
     ?visual-location>
       buffer empty
       error nil
     ?visual>
       buffer empty
       state free
     ==>
     =goal>
       step 1
     +visual-location>
       isa visual-location
      > screen-x current
       screen-x lowest)
  
  
  (p shift-attention
     "Request an attention shift to that item and clear the motor module to remove any prepared features"
     =goal>
       isa goal
     =visual-location>
       isa visual-location
     ?manual>
       state free
     ==>
     +manual>
       isa clear
     +visual>
       isa move-attention
       screen-pos =visual-location)
  
  (p attend-item
     "Move the mouse to the attended object"
     =goal>
       isa goal
       step =val
     =visual>
       isa visual-object
     ?manual>
       state free
     ==>
     !eval! (setf *start-time* (mp-time))
     +manual>
       isa move-cursor
       object =visual
     -goal>)   
  
  (p return-to-center
     "Report how long it took to make the last move and return the mouse to the center spot"
     ?goal>
       buffer empty
     ?manual>
       state free
     ?visual>
       buffer empty
     ?visual-location>
       buffer empty
     !bind! =move-time (- (mp-time) *start-time*)
     ==>
     !output! (The time to move to the object was =move-time)
     +goal>
       isa goal
       step nil
     +manual>
       isa move-cursor
       loc loc-100x100
     )
  )
 