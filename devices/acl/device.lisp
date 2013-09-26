;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell (plus code from Tech support at Franz Inc.)
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2000-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : device.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : ACL-specific functions for RPM.  This consists primarily
;;;             : of stuff for vision (parsing the screen), and output
;;;             : stuff for motor.
;;;             : I'm unsure if this file should stay in the main distribution -
;;;             : I think it should be dropped down to the extras.
;;; 
;;; Bugs        : 
;;;
;;; Todo        : Document this file better...
;;;             : There's an issue with human pressing of "punctuation" keys
;;;             : because ACL sends the vk- code instead of the ASCII for
;;;             : things like comma and semicolon so it differs from what the
;;;             : model does.  Don't know if it's worth fixing, but is somthing
;;;             : to be aware of if using real ACL windows and not visible-
;;;             : virtuals.
;;;             : * Make this work with a packaged ACT-R - the problem is 
;;;             :   that some of the cg functions are't exported from cg-user
;;;             :   so just having that on the uses of the :act-r package
;;;             :   isn't sufficient...
;;; 
;;; --- History ---
;;; 00.01.25 Dan Bothell
;;;             : First version.
;;;             : function comments copied from mcl-interface.
;;; 00.06.08 mdb
;;;             : Moved POPULATE-LOC-TO-KEY-ARRAY method here.
;;; 00.09.05 Dan
;;;             : Fixed all of the text feature building to correctly
;;;             : use ascent and descent parameters for the font instead of
;;;             : only using the font-height.
;;;             : Made the changes to bring it up to speed with 2.0b3
;;;             : Specifically:
;;;             :    Added a focus ring.
;;;             :    Changed from xy lists to vectors.
;;;             :    Fixed the build-features-for for buttons.
;;;             :    Added the approach-width method for text features
;;;             :    so that if it's on a button it's computed correctly.
;;; 01.04.12 Dan
;;;             : Added a hack to get around the keypad not returning
;;;             :    numbers.
;;;             : Changed the focus ring so that it didn't 'eat' the
;;;             :    the keypresses.
;;; 02.01.15 Dan
;;;             : Added some feature checks to better handle ACL versions
;;;             : other than 5.0.1.  It's still not perfect however because
;;;             : in ACL 6 or newer the focus ring doesn't erase 
;;;             : and I haven't figured out how to fix that yet.
;;; 02.06.07 Dan
;;;             : Finally got to the bottom of the focus ring stuff because
;;;             : I needed it to work in ACL 6.1 and it had a couple
;;;             : of issues - it stole mouse clicks and kept being recreated,
;;;             : but it's all better now.
;;; 02.06.21 Dan [b7]
;;;             : Changed rpm-window class to rpm-real-window class
;;;             : and updated all the methods accordingly.
;;; 02.06.30 Dan
;;;             : Removed the UWI code from this file.
;;;             : Moved the view-line and color mapping code into this file.
;;; 02.11.25 Dan
;;;             : Changed the model's pressing of the escape key from #\esc to
;;;             : vk-escape because that's what ACL uses for "real" escape 
;;;             : presses so this makes the model act the same i.e. the same
;;;             : key handling code works for both a model and human.
;;;             : Might impact some existing models.
;;; 02.12.23 Dan 
;;;             : Added the around method for the build-features-for of text
;;;             : items to handle color.
;;; 03.01.24 Dan
;;;             : Redefine the attr-exact-match-p method because of the issue
;;;             : raised by adding the colors - ACL defines red, blue, green,
;;;             : etc as special symbols that cause problems in RHS requests
;;;             : so those need to be caught and handled differently.  I don't 
;;;             : think there are any LHS issues to resolve, but we'll see...
;;;
;;; 04.04.13 Dan [2.2] (both previous changes are also "new" in 2.2)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : changed the name to device to be placed in a folder called acl
;;;             : following the MCL exaple, got rid of the do-update method
;;;             :   (which is necessary anyway) and replaced it with the 
;;;             :   device-update :after method to do basically the same thing
;;; 2005.02.17 Dan
;;;             : * Added the use-package to deal with the packaging stuff.
;;; 2005.08.10 Dan
;;;             : * Minor clean-up of compiler warnings.  Wrapped an eval-when
;;;             :   around the use-package and added some ignores.
;;;             : * Fixed build-features-for button items with multiple lines
;;;             :   of text (I think).
;;;             : * Noted that it doesn't work with the packaged ACT-R quite
;;;             :   right (virtuals do, but reals don't).
;;; 2005.09.16 Dan
;;;             : * Preliminary work for converting to ACL 7 - for now, just
;;;             :   check to see if the do-xxxxxxx (key, mouse) stuff is
;;;             :   needed.  The release notes don't show anything else that
;;;             :   should be a problem, but we'll find out...
;;;             : * Of course they fail to document that they've reoganized
;;;             :   the class hierarchy and there is no longer a class called
;;;             :   window.  Using the class basic-pane instead for ACL 7.
;;; 2006.09.07 Dan
;;;             : * Removed the fill-default-dimensions method because it's
;;;             :   now defined in the vision file.
;;; 2008.07.12 Dan
;;;             : * Added a call to process-pending-events in the device-handle-
;;;             :   keypress method because in ACL 8+ that seems to cause
;;;             :   a timing issue if it's not there - when the keypress is
;;;             :   handled varies which didn't seem to happen for the previous
;;;             :   versions of ACL.
;;; 2012.06.26 Dan
;;;             : * Eliminating *attn-tracker* and instead using the visual-
;;;             :   fixation-marker slot to hold the item so each model has
;;;             :   its own and respond to a nil xyloc by removing the current
;;;             :   ring.
;;; 2013.01.03 Dan
;;;             : * Clipped the rpm-view-line function since it isn't needed
;;;             :   and contains outdated code to avoid any confusion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cg-user))

(defvar *simulated-key* nil)


(defmethod window-select ((w #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane))
  (select-window w))

(defmethod device-move-cursor-to ((device #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) (xyloc vector))
  (setf (cg:cursor-position device) (cg:make-position (px xyloc) (py xyloc))))

(defmethod device-speak-string ((device #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) string)
  (declare (ignore string)))

(defmethod get-mouse-coordinates ((device #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane))
  (let ((cur-pos (cg:cursor-position device)))
    (vector (cg:position-x cur-pos) (cg:position-y cur-pos))))

(defmethod device-handle-click ((device #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane))
  #+(version>= 6)
  (let (x y)
    (when (and (show-focus-p (current-device-interface))
               (equal (type-of (visual-fixation-marker)) 'focal-view))
      (setf x (left (visual-fixation-marker)))
      (setf y (top (visual-fixation-marker)))
      
      (close (visual-fixation-marker))
      (cg:process-pending-events))
    (do-click nil :preview-seconds nil :down-seconds .0001)
    (cg:process-pending-events)
    (when (and (show-focus-p (current-device-interface))
               (equal (type-of (visual-fixation-marker)) 'focal-view))
      (setf (visual-fixation-marker) (cg:make-window :focus-ring :device 'focal-view :parent device
                                      :left x 
                                      :top y )))
      )
    
  #-(version>= 6)
  (do-click nil :preview-seconds nil :down-seconds .0001))


(defmethod device-handle-keypress ((device #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) key)
  (do-keypress device key :preview-seconds nil :down-seconds nil)
  (cg:process-pending-events))


;;; Similar to what Mike did for the MCL version
;;; but since process-pending-events is faster than
;;; event-dispatch in MCL (basically negligible) I don't 
;;; need to do the testing and only fire it periodically.
;;; 

(defmethod device-update :after ((wind #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) time)
  (declare (ignore time))
  (cg:process-pending-events)
  )

#|
(defmethod do-update :after ((mstr-proc master-process) current-time 
                               &key (real-wait nil))
  (declare (ignore current-time real-wait))
  (cg:process-pending-events))
|#




(defun loc-avg (x y)
  (declare (fixnum x) (fixnum y))
  (floor (/ (+ x y) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; ACL screen-to-vis-loc interface
;;;; ---------------------------------------------------------------------- ;;;;;;;


(defmethod build-vis-locs-for ((self #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) (vis-mod vision-module))
  (let ((base-ls (flatten
                    (mapcar #'(lambda (obj) (build-vis-locs-for obj vis-mod))
                            (get-sub-objects self)))))
    ;(dolist (feat base-ls)
    ;  (fill-default-dimensions feat))
    base-ls))

(defmethod vis-loc-to-obj ((self #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) loc)
  (case (chunk-slot-value-fct loc 'kind)
    (cursor
       (fill-default-vis-obj-slots (car (define-chunks (isa cursor))) loc))))

(defmethod get-sub-objects ((v #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane))
  "Grabbing the sub-objects of a window by default returns the dialog-items."
  (cg:dialog-items v))


(defmethod build-vis-locs-for ((self cg:dialog-item) (vis-mgr vision-module))
  (let ((c (car (define-chunks-fct `((isa visual-location 
                                          screen-x ,(px (view-loc self))
                                          screen-y ,(py (view-loc self))
                                          kind visual-object
                                          value unknown
                                          height ,(height self)
                                          width ,(width self)
                                          color black))))))
    (setf (chunk-visual-object c) self)
    c))





(defmethod build-vis-locs-for ((self cg:editable-text) (vis-mod vision-module))
  (let* ((font-spec (cg:nfontmetrics (cg:window self) (cg:make-fontmetrics)))
         (ascent (cg:font-ascent font-spec))
         (descent (cg:font-descent font-spec))
         (text (cg:value self))
         (feats (cons
                 (car (define-chunks-fct `((isa visual-location
                                                kind visual-object
                                                value box
                                                screen-x ,(px (view-loc self))
                                                screen-y ,(py (view-loc self))
                                                height ,(height self)
                                                width ,(width self)))))
                 (unless (equal text "")
                   (build-string-feats vis-mod :text text
                                       :start-x (1+ (cg:box-left (cg:box self)))
                                       :y-pos (+ (cg:box-top (cg:box self))
                                                 descent (round ascent 2))
                                       :width-fct #'(lambda (str)
                                                      (string-width str self))
                                       :height ascent :obj self)))))
    (dolist (x feats)
      (setf (chunk-visual-object x) self))
    feats))
    


(defmethod build-vis-locs-for ((self cg:button) (vis-mod vision-module))
  (let* ((btn-width (width self))
         (btn-height (height self))
         (text (title self))
         (feats
    
          (cons
           (car (define-chunks-fct `((isa visual-location
                                          kind oval
                                          value oval
                                          screen-x ,(px (view-loc self)) 
                                          screen-y ,(py (view-loc self))
                                          width ,btn-width 
                                          height ,btn-height
                                          color ,(system-color->symbol (cg:foreground-color self))))))
           (unless (equal text "")
             (let* ((font-spec (cg:nfontmetrics (cg:window self) (cg:make-fontmetrics)))
                    (ascent (cg:font-ascent font-spec))
                    (descent (cg:font-descent font-spec))
                    (textlines (string-to-lines text))
                    (start-y (+ (cg:box-top (cg:box self))
                                (round (- btn-height (* (length textlines)
                                                        (+ ascent descent))) 2)))
                    (accum nil)
                    (width-fct #'(lambda (str) (string-width str self))))
               (dolist (item textlines (flatten (nreverse accum)))
                 (push
                  (build-string-feats vis-mod :text item
                                      :start-x 
                                      (+ (cg:box-left (cg:box self))
                                         (round 
                                          (- btn-width (funcall width-fct item))
                                          2))
                                      :y-pos (+ start-y (round (+ ascent descent) 2))
                                      :width-fct width-fct :height (min ascent btn-height) 
                                      :obj self)
                  accum)
                 (incf start-y (+ ascent descent))))))))
    
    (let ((fun (lambda (x y) (declare (ignore x)) (approach-width (car feats) y))))
      (dolist (x (cdr feats))
        (setf (chunk-visual-approach-width-fn x) fun)
        (set-chunk-slot-value-fct x 'color 'black)))
    
    (dolist (x feats)
      (setf (chunk-visual-object x) self))
    
    feats))
    
(defmethod build-vis-locs-for ((self cg:static-text)
                                  (vis-mod vision-module))
  (let* ((font-spec (cg:nfontmetrics (cg:window self) (cg:make-fontmetrics)))
         (ascent (cg:font-ascent font-spec))
         (descent (cg:font-descent font-spec))
         (text (cg:value self)))                        
    (unless (equal text "")
      (let ((feats (build-string-feats vis-mod :text text
                                       :start-x (1+ (cg:box-left (cg:box self)))
                                       :y-pos (+ (cg:box-top (cg:box self))
                                                 descent (round ascent 2))
                                       :width-fct #'(lambda (str)
                                                      (string-width str self))
                                       :height ascent :obj self))
            (color (system-color->symbol (aif (cg:foreground-color self)
                                              it
                                              cg:black))))
        (dolist (x feats)
          (set-chunk-slot-value-fct x 'color color)
          (setf (chunk-visual-object x) self))
        feats))))
     

(defun string-width (str item)
  (cg:stream-string-width (cg:window item) str))



;;; CURSOR-TO-FEATURE      [Function]
;;; Date        : 97.06.16
;;; Description : Returns a feature representing the current state and shape
;;;             : of the cursor.

(defmethod cursor-to-vis-loc ((the-window #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane))
  "Returns a feature corresponding to the current cursor."
  (let ((pos (cg:cursor-position the-window))
        (shape (cursor the-window)))
    (when (cursor-in-window-p the-window)
      (car (define-chunks-fct `((isa visual-location
                                     kind cursor
                                     screen-x ,(cg:position-x pos)
                                     screen-y ,(cg:position-y pos)
                                     value ,(case (name shape)
                                              (:line-cursor 'i-beam)
                                              (:cross-cursor 'crosshair)
                                              (:waiting-cursor 'watch)
                                              (otherwise 'pointer))
                                     color black)))))))
               

(defmethod cursor-in-window-p (tw)
  "Returns T if the cursor is over the input window, NIL otherwise."
  (or (equal (cg:window-under-mouse) tw)
      (equal (cg:parent (cg:window-under-mouse)) tw)))



(defmethod view-loc ((self cg:dialog-item))
  "Return the center point of a view in (X Y) format."
  (let ((b (cg:box self))
        )
    (vector (+ (cg:box-left b) (round (/ (- (cg:box-right b) (cg:box-left b)) 2)))
            (+ (cg:box-top b) (round (/ (- (cg:box-bottom b) (cg:box-top b)) 2))))))



(defmethod view-loc ((self symbol))
  "Hacked VIEW-LOC method for the cursor--returns the cursor location as (X Y)."
  (if (eq self :cursor)
    (get-mouse-coordinates (current-device))
    (error "!! Can't find location of ~S" self)))



(defmethod populate-loc-to-key-array ((ar array))
  "Sets all the keys in the array that need to be set"
  ;; function key row
  (setf (aref ar 0 0) cg:vk-escape)
  (setf (aref ar 2 0) cg:vk-f1)
  (setf (aref ar 3 0) cg:vk-f2)
  (setf (aref ar 4 0) cg:vk-f3)
  (setf (aref ar 5 0) cg:vk-f4)
  (setf (aref ar 7 0) cg:vk-f5)
  (setf (aref ar 8 0) cg:vk-f6)
  (setf (aref ar 9 0) cg:vk-f7)
  (setf (aref ar 10 0) cg:vk-f8)
  (setf (aref ar 12 0) cg:vk-f9)
  (setf (aref ar 13 0) cg:vk-f10)
  (setf (aref ar 14 0) cg:vk-f11)
  (setf (aref ar 15 0) cg:vk-f12)
  (setf (aref ar 17 0) cg:vk-f13)
  (setf (aref ar 18 0) cg:vk-f14)
  (setf (aref ar 19 0) cg:vk-f15)
  ;; numeric key row
  (setf (aref ar 0 2) cg:vk-backquote)
  (setf (aref ar 1 2) #\1)
  (setf (aref ar 2 2) #\2)
  (setf (aref ar 3 2) #\3)
  (setf (aref ar 4 2) #\4)
  (setf (aref ar 5 2) #\5)
  (setf (aref ar 6 2) #\6)
  (setf (aref ar 7 2) #\7)
  (setf (aref ar 8 2) #\8)
  (setf (aref ar 9 2) #\9)
  (setf (aref ar 10 2) #\0)
  (setf (aref ar 11 2) #\-)
  (setf (aref ar 12 2) #\=)
  (setf (aref ar 13 2) cg:vk-backspace)
  (setf (aref ar 15 2) cg:vk-insert)
  (setf (aref ar 16 2) cg:vk-home)
  (setf (aref ar 17 2) cg:vk-pageup)
  (setf (aref ar 19 2) cg:vk-numlock)
  (setf (aref ar 20 2) #\=)
  (setf (aref ar 21 2) #\/)
  (setf (aref ar 22 2) #\*)
  ;; qwerty row
  (setf (aref ar 0 3) #\tab)
  (setf (aref ar 1 3) #\q)
  (setf (aref ar 2 3) #\w)
  (setf (aref ar 3 3) #\e)
  (setf (aref ar 4 3) #\r)
  (setf (aref ar 5 3) #\t)
  (setf (aref ar 6 3) #\y)
  (setf (aref ar 7 3) #\u)
  (setf (aref ar 8 3) #\i)
  (setf (aref ar 9 3) #\o)
  (setf (aref ar 10 3) #\p)
  (setf (aref ar 11 3) #\[)
  (setf (aref ar 12 3) #\])
  (setf (aref ar 13 3) #\\)
  (setf (aref ar 15 3) cg:vk-delete)
  (setf (aref ar 16 3) cg:vk-end)
  (setf (aref ar 17 3) cg:vk-pagedown)
  (setf (aref ar 19 3) #\7)
  (setf (aref ar 20 3) #\8)
  (setf (aref ar 21 3) #\9)
  (setf (aref ar 22 3) #\-)
  ;; ASDF row
  (setf (aref ar 0 4) cg:vk-capslock)
  (setf (aref ar 1 4) #\a)
  (setf (aref ar 2 4) #\s)
  (setf (aref ar 3 4) #\d)
  (setf (aref ar 4 4) #\f)
  (setf (aref ar 5 4) #\g)
  (setf (aref ar 6 4) #\h)
  (setf (aref ar 7 4) #\j)
  (setf (aref ar 8 4) #\k)
  (setf (aref ar 9 4) #\l)
  (setf (aref ar 10 4) #\;)
  (setf (aref ar 11 4) cg:vk-quote)
  (setf (aref ar 12 4) #\return)
  (setf (aref ar 13 4) #\return)
  (setf (aref ar 19 4) #\4)
  (setf (aref ar 20 4) #\5)
  (setf (aref ar 21 4) #\6)
  (setf (aref ar 22 4) #\+)
  ;; Z row
  (setf (aref ar 0 5) cg:vk-shift)
  (setf (aref ar 1 5) #\z)
  (setf (aref ar 2 5) #\x)
  (setf (aref ar 3 5) #\c)
  (setf (aref ar 4 5) #\v)
  (setf (aref ar 5 5) #\b)
  (setf (aref ar 6 5) #\n)
  (setf (aref ar 7 5) #\m)
  (setf (aref ar 8 5) #\,)
  (setf (aref ar 9 5) #\.)
  (setf (aref ar 10 5) #\/)
  (setf (aref ar 11 5) cg:vk-shift)
  (setf (aref ar 12 5) cg:vk-shift)
  (setf (aref ar 16 5) cg:vk-up)
  (setf (aref ar 19 5) #\1)
  (setf (aref ar 20 5) #\2)
  (setf (aref ar 21 5) #\3)
  (setf (aref ar 22 5) cg:vk-enter)
  ;; space bar row
  (setf (aref ar 0 6) cg:vk-control)
  (setf (aref ar 1 6) 'option)
  (setf (aref ar 2 6) cg:vk-alt)
  (setf (aref ar 3 6) #\space)
  (setf (aref ar 4 6) #\space)
  (setf (aref ar 5 6) #\space)
  (setf (aref ar 6 6) #\space)
  (setf (aref ar 7 6) #\space)
  (setf (aref ar 8 6) #\space)
  (setf (aref ar 9 6) #\space)
  (setf (aref ar 10 6) #\space)
  (setf (aref ar 11 6) cg:vk-alt)
  (setf (aref ar 12 6) 'option)
  (setf (aref ar 13 6) cg:vk-control)
  (setf (aref ar 15 6) cg:vk-left)
  (setf (aref ar 16 6) cg:vk-down)
  (setf (aref ar 17 6) cg:vk-right)
  (setf (aref ar 19 6) #\0)
  (setf (aref ar 20 6) #\0)
  (setf (aref ar 21 6) #\.)
  (setf (aref ar 22 6) cg:vk-enter)
  ar)



;;; DEVICE-UPDATE-ATTENDED-LOC      [Method]
;;; Date        : 00.09.05
;;; Description : When the attended location is updated, update the focus
;;;             : ring.  Create a new one and add it to the window if the
;;;             : previous one was closed, exists in a different window, or 
;;;             : if it hasn't been created yet.
;;;             : Differs from MCL's in that it doesn't exist outside of a
;;;             : device window.

(defmethod device-update-attended-loc ((wind #-(version>= 7) cg:window #+(version>= 7) cg:basic-pane) xyloc)
  
  ;;; Make sure it's a focal-view or nil
  ;;; and if not just clear it which could be an issue if
  ;;; someone creates different types of windows each of which
  ;;; will be setting incompatible fixtion markers, but that's
  ;;; unlikely to occur.
  
  (unless (or (null (visual-fixation-marker))
              (equal (type-of (visual-fixation-marker)) 'focal-view)
              #+:allegro-v5.0.1 (equal (type-of (visual-fixation-marker)) 'closed-stream))
    (setf (visual-fixation-marker) nil))

  
  ;;; Close it if it's open and the xyloc is nil or the parent doesn't match
  
  (when (and (visual-fixation-marker)
             #+:allegro-v5.0.1 (not (equal (type-of (visual-fixation-marker)) 'closed-stream))
             #+(version>= 6) (cg:handle (visual-fixation-marker))
             
             (or (null xyloc)
                 (not (equal wind (cg:parent (visual-fixation-marker))))))
    (close (visual-fixation-marker)))
  
  ;;;  If there's a location and it doesn't exist or is closed create a new one 
  
  (when (and xyloc
             (or (null (visual-fixation-marker))
                 #+:allegro-v5.0.1 (equal (type-of (visual-fixation-marker)) 'closed-stream)
                 #+(version>= 6) (null (cg:handle (visual-fixation-marker)))))
    (setf (visual-fixation-marker) (cg:make-window :focus-ring :device 'focal-view :parent wind
                                     :left (- (px xyloc) 11) 
                                     :top (- (py xyloc) 11))))
  
  ;; only update it if there's a location
  
  (when xyloc
    (update-me (visual-fixation-marker) wind xyloc)))

;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; The view based line drawing classes and methods
;;;; ---------------------------------------------------------------------- ;;;;;;;

;;; The base class for the dialog based lines.  All it adds is a color slot.

(defclass liner (cg:drawable)
  ((color :accessor color :initarg :color :initform 'black)
   (start-pt :accessor start-pt :initarg :start-pt :initform (list 0 0))
   (end-pt :accessor end-pt :initarg :end-pt :initform (list 0 0)))
  (:default-initargs
    :width 1
    :height 1
    :left -2
    :top 0
    :on-redisplay 'draw-view-line))


(defun draw-view-line (di stream)
  (declare (ignore stream))
  (let* ((real-stream (cg:parent di)))
    (with-foreground-color (real-stream (color di))
      (draw-line real-stream (cg:make-position (first (start-pt di)) (second (start-pt di)))
                 (cg:make-position (first (end-pt di)) (second (end-pt di)))))))


(defmethod build-vis-locs-for ((lnr liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let ((f (car (define-chunks-fct `((isa visual-location
                                          color ,(system-color->symbol (color lnr))
                                          value line
                                          kind line
                                          screen-x ,(loc-avg (first (start-pt lnr)) (first (end-pt lnr)))
                                          screen-y ,(loc-avg (second (start-pt lnr)) (second (end-pt lnr)))
                                          width ,(abs (- (first (start-pt lnr)) (first (end-pt lnr))))
                                          height ,(abs (- (second (start-pt lnr)) (second (end-pt lnr))))))))))
    (setf (chunk-visual-object f) lnr)
    f))

         

(defmethod vis-loc-to-obj ((lnr liner) loc)
  (let ((v-o (fill-default-vis-obj-slots (car (define-chunks (isa line))) loc)))
    (set-chunk-slot-value-fct v-o 'end1-x (first (start-pt lnr)))
    (set-chunk-slot-value-fct v-o 'end1-y (second (start-pt lnr)))
    (set-chunk-slot-value-fct v-o 'end2-x (first (end-pt lnr)))
    (set-chunk-slot-value-fct v-o 'end2-y (second (end-pt lnr)))
    v-o))
    





;;;; ---------------------------------------------------------------------- ;;;;
;;;; Focus ring stuff


(defclass focal-view (cg:transparent-pane)
  ()
  (:default-initargs
    :foreground-color cg:red
    :width 23
    :height 23))


;;; REDISPLAY-WINDOW    [Method]
;;; Date        : 00.09.05
;;; Description : Draws a red ring centered in the center of the transparent  
;;;             : pane, which should be at the focus of attention.

(defmethod redisplay-window ((self focal-view) &optional box)
  (declare (ignore box))
  (setf (line-width self) 3)
  (draw-circle self (cg:make-position 11 11) 10))



;;; UPDATE-ME      [Method]
;;; Date        : 00.09.05
;;; Description : Udating the focus ring means changing its location.
;;;             : It's hidden before moving to prevent disruption of
;;;             : other screen objects (problem with ACL transparent pane).

(defmethod update-me ((foc-ring focal-view) window xyloc)
  (declare (ignore window))
  (setf (cg:state foc-ring) :shrunk)
  (setf (cg:left foc-ring) (- (px xyloc) 11))
  (setf (cg:top foc-ring) (- (py xyloc) 11))
  (setf (cg:state foc-ring) :normal))


;;; hack so that when the focus-ring is present it passes the key presses on to the
;;; window for handling - oh yeah it actually works 

(defmethod virtual-key-down :before ((focus-ring focal-view) buttons key-code)
  (virtual-key-down (cg:parent focus-ring) buttons key-code))

;;;; --------------------------------------------------------------------- ;;;;
;;;; The color mapping functions

;;; because of how ACL treats the color names as symbols I need to do some
;;; extra trickery so that one can use names like red in a production request

;(defmethod attr-exact-match-p ((spec icon-feature) (feat icon-feature) slotname)
;  (cond ((eq (slot-value spec slotname) :IGNORE) T)
;        ((and (equal slotname 'color) (rgb-p (slot-value spec slotname))) 
;         (equal (slot-value feat slotname) (color-name->color-symbol (slot-value spec slotname))))
;        ((equal (slot-value spec slotname) (slot-value feat slotname)))))

;;; This function returns the symbol name of one of the ACL RGB special 
;;; symbols.  It's similar to system-color->symbol but doesn't do the funny
;;; mapping for things like magenta -> pink because that isn't the problem.
;;; It's an issue because a RHS request like this:
;;;
;;; +visual-location>
;;;    isa visual-location
;;;    color red
;;;
;;; ends up being parsed into passing the special RGB red to the find-location
;;; instead of the symbol red.
;;; I think this is enough to fix things, but maybe there are other issues
;;; I haven't seen yet where those special symbols cause problems (perhaps
;;; some LHS issues need to be resolved).

(defun color-name->color-symbol (color)
  (cond ((cg:rgb-equal color cg:red) 'red)
        ((cg:rgb-equal color cg:blue) 'blue)
        ((cg:rgb-equal color cg:green) 'green)
        ((cg:rgb-equal color cg:black) 'black)
        ((cg:rgb-equal color cg:white) 'white)
        ((cg:rgb-equal color cg:magenta) 'magenta)
        ((cg:rgb-equal color cg:yellow) 'yellow)
        ((cg:rgb-equal color cg:cyan) 'cyan)
        ((cg:rgb-equal color cg:dark-green) 'dark-green)
        ((cg:rgb-equal color cg:dark-red) 'dark-red)
        ((cg:rgb-equal color cg:dark-cyan) 'dark-cyan)
        ((cg:rgb-equal color cg:dark-blue) 'dark-blue)
        ((cg:rgb-equal color cg:dark-magenta) 'dark-magenta)
        ((cg:rgb-equal color cg:dark-yellow) 'dark-yellow)
        ((cg:rgb-equal color cg:light-gray) 'light-gray)
        ((cg:rgb-equal color cg:gray) 'gray)
        ((cg:rgb-equal color cg:dark-gray) 'dark-gray)))


(defun system-color->symbol (color)
  "Return a symbol that names the color for the 'recognized' colors.   
   Any other color gets mapped to a symbol color-RRRRR-GGGGG-BBBBB where the 
   R's, G's, and B's are the red, green, and blue components of the 
   color left padded with zeros to 5 digits."
  (if (null color)
    'black
    (cond ((cg:rgb-equal color cg:red) 'red)
          ((cg:rgb-equal color cg:blue) 'light-blue)
          ((cg:rgb-equal color cg:green) 'green)
          ((cg:rgb-equal color cg:black) 'black)
          ((cg:rgb-equal color cg:white) 'white)
          ((cg:rgb-equal color cg:magenta) 'pink)
          ((cg:rgb-equal color cg:yellow) 'yellow)
          ((cg:rgb-equal color cg:dark-green) 'dark-green)
          ((cg:rgb-equal color cg:dark-blue) 'blue)
          ((cg:rgb-equal color cg:dark-magenta) 'purple)
          ((cg:rgb-equal color cg:dark-yellow) 'brown)
          ((cg:rgb-equal color cg:light-gray) 'light-gray)
          ((cg:rgb-equal color cg:gray) 'gray)
          ((cg:rgb-equal color cg:dark-gray) 'dark-gray)
          (t (intern (format nil "COLOR-~5,'0d-~5,'0d-~5,'0d" 
                             (cg:rgb-red color) 
                             (cg:rgb-green color) 
                             (cg:rgb-blue color)))))))

(defun color-symbol->system-color (color)
  "this may look like it should do the inverse of the above, but right now
   it doesn't exactly.  If the color isn't one of the default ones then
   the black color is returned.  It's only being used by the UWI right now,
   so it's simplified for that purpose."
  (cond ((equal color 'red) cg:red)
        ((equal color 'blue) cg:dark-blue)
        ((equal color 'green) cg:green)
        ((equal color 'black) cg:black)
        ((equal color 'white) cg:white)
        ((equal color 'pink)  cg:magenta)
        ((equal color 'yellow) cg:yellow)
        ((equal color 'dark-green) cg:dark-green)
        ((equal color 'light-blue) cg:blue)
        ((equal color 'purple) cg:dark-magenta)
        ((equal color 'brown) cg:dark-yellow)
        ((equal color 'light-gray) cg:light-gray)
        ((equal color 'gray) cg:gray)
        ((equal color 'dark-gray) cg:dark-gray)
        (t cg:black)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The following code was given to me by a developer at Franz to simulate the
;;;  mouse clicking and the button pressing in a window so that all of the
;;;  correct ACL 'actions' occur (methods get called, window is selected, visual
;;;  action occurs, etc). 



;;; As of ACL 7 these exact functions have been made part of the system!!!!
;;; So, I need to not load them when using ACL 7.

#-(version>= 7)
(progn 

#| 
Our developer most familiar with Common Graphics has found a way to
accomplish what you were trying to do. He has written some code
(attached below my signature) which when loaded into your lisp will
allow you to run some examples showing a number of ways to simulate keypresses
 
and button clicks.
 
Using the following definition of a window (which contains 2 widgets)


    (defun make-form1
    (&key (parent (development-main-window cg:*system*))
     (exterior (make-box 256 167 944 534)) (name :form1)
     (title "Form1") form-p)
  (let ((parent
         (cg:make-window name
           :parent parent
           :device 'dialog
           :exterior exterior
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :widgets
               (list 
                  (setf w3 (make-instance 'button
                   :font
                   (cg:make-font-ex nil "MS Sans Serif" 13 nil)
                             :left 176
                   :on-change 'foo         
                   :name :button4
                   :top 24))
                 (setf w4 (make-instance 'editable-text
                   :font
                   (cg:make-font-ex nil "MS Sans Serif" 13 nil)
                   :left 106
                   :name :editable-text-1
                   :template-string nil
                   :top 208
                   :up-down-control nil
                            :value "EDITABLE-TEXT")))                
                   :form-state :normal
                 :maximize-button t
           :minimize-button t
           :name :form1
           :package-name :common-graphics-user
           :pop-up nil
           :resizable t
           :scrollbars nil
           :state :normal
           :status-bar nil
           :system-menu t
           :title title
           :title-bar t
           :toolbar nil
           :form-p form-p
           :path #p"C:\\Program Files\\acl50195pf\\form1.bil"
           :help-string nil
           :package-name :common-graphics-user)))
    parent))
 
(defun foo(widget new-value old-value)
  (princ "hello"))
 

You can try running the following examples using the widgets w3 and w4.
For example: 
 
       (do-click w3)
 
will  call the button to be pressed and the on-change function foo to run.
Foo simply prints "hello" to the debug window.
 



Examples (where "it" is either a window or widget):
 
;; Mouse clicks
 
;; Left-click in the center of its scrollable page.
(do-click it)
 
;; Left-click near the upper-right corner of its scrollable page.
(do-click it :position (cg:make-position (- (interior-width (window it))
                                         8)
                                      12))
 
;; Left-click it with no pause for people to watch the action.
(do-click it :preview-seconds nil :down-seconds nil)
 
;; Right-click in the center of its client area.
(do-click it :button :right)
 
;; Left-click wherever the mouse is now.
(do-click nil)
 
;; Left-click an arbitary position on the screen.
(do-click nil :position (cg:make-position 100 200))
 
 
;; Left-click the screen over the center of it, but without first
;; exposing it.  So if another window covers it, then the click
;; will go to that window instead of it.  Pre-expose defaults to
;; t to ensure that the click goes to the specified window, but
;; it may be useful to pass it as nil as in this example if you
;; are testing that that the window is exposed when it should be.
(do-click it :pre-expose nil)
 
;; Keypresses
 
;; Type a "j" into it.
(do-keypress it #\j)
 
;; Type a semicolon into whatever window has the focus already.
(do-keypress nil vk-semicolon)
 
;; Give it the focus and press down the shift key
;; without releasing it.  WARNING:  Doing this without
;; a subsequent up-click of the same key leaves the OS thinking
;; that the shift key is still down, and a further keystroke
;; will believe it is shifted.  This can be fixed interactively
;; by simply pressing and releasing the left shift key.
(do-keypress it vk-shift :up nil)
 
;; Type control-J into it.
(do-keypress it #\j :control t)
 
;; Type a whole string of characters into it.
(do-keypresses it "How about that.")
 
;; Print an arbitrary object into it.
(do-keypresses it (list :one "Foo"))
 
|#
 
;; ------------------------------------------------------------
;; mouse events
 
(defconstant win::mouseeventf_move        #x0001)
(defconstant win::mouseeventf_leftdown    #x0002)
(defconstant win::mouseeventf_leftup      #x0004)
(defconstant win::mouseeventf_rightdown   #x0008)
(defconstant win::mouseeventf_rightup     #x0010)
(defconstant win::mouseeventf_middledown  #x0020)
(defconstant win::mouseeventf_middleup    #x0040)
(defconstant win::mouseeventf_wheel       #x0800)
(defconstant win::mouseeventf_absolute    #x8000)
 
(ff:def-foreign-call (win::mouse_event "mouse_event")
    ((win::dwflags win::dword)
     (win::dx win::dword)
     (win::dy win::dword)
     (win::dwdata win::dword)
     (win::dwextrainfo win::dword))) ;; officially ulong_ptr
     
(defgeneric do-click (window-or-widget-or-nil
                        &key (position (and window-or-widget-or nil :center))
                        (button :left)(pre-expose t)
                        (preview-seconds 0.5)(down-seconds 0.5))
  (:documentation
   "Simulates clicking a mouse button at some position in a window."))
 
(defmethod do-click ((widget cg:dialog-item)
                       &key (position (and widget :center))
                       (button :left)(pre-expose t)
                       (preview-seconds 0.5)(down-seconds 0.5))
  (let* ((window (cg:window widget)))
    (when (cg:windowp window)
      (do-click window :position position
                :button button :pre-expose pre-expose
                :preview-seconds preview-seconds
                :down-seconds down-seconds))))
 
(defmethod do-click ((window t)
                     &key (position (and window :center))
                     (button :left)(pre-expose t)
                     (preview-seconds 0.5)(down-seconds 0.5)
                     (down t)(up t))
  (unless window
    (setq window (cg:screen cg:*system*)))
  (let* ((win window)
         (down-event (and down
                          (case button
                            (:left win::mouseeventf_leftdown)
                            (:middle win::mouseeventf_middledown)
                            (:right win::mouseeventf_rightdown))))
         (up-event (and up
                        (case button
                          (:left win::mouseeventf_leftup)
                          (:middle win::mouseeventf_middleup)
                          (:right win::mouseeventf_rightup))))
         (stream-pos (case position 
                       (:center (cg:box-center (visible-box window)))
                       (t position)))
         #+not-used
         (screen-pos (and stream-pos
                          (window-to-screen-units window
                            (stream-to-window-units window
                              (copy-position stream-pos))))))
    (when window
      (when pre-expose
        (loop (unless (cg:windowp win)(return))
              (unless (eq win (cg:selected-window (cg:parent win)))
                (select-window win))
              (setq win (cg:parent win))))
      
      ;; Move the mouse over the window.
      (when stream-pos
        (setf (cg:cursor-position window) stream-pos))
      
      #+no ;; These units apparently would need to be normalized
      ;; where 0 to 65k covers the screen, so use (setf cursor-position)
      ;; instead in order to use pixel units.
      (when screen-pos
        (win::mouse_event (logior win::mouseeventf_move
                                  win::mouseeventf_absolute)
                          (cg:position-x screen-pos)
                          (cg:position-y screen-pos)
                          0 0)))
    
    ;; Wait a bit for the user to see the window before the click is done.
    (when preview-seconds (sleep preview-seconds))
    
    ;; Send the click down and up messages, pausing a bit in between
    ;; so that a human can see that the button was clicked.
    (when down
      (win::mouse_event down-event 0 0 0 0)
      
      #+no ;; The coordinates don't matter when doing the click,
      ;; though the MSDN doesn't make this clear.
      (win::mouse_event (logior down-event
                                (if screen-pos
                                    win::mouseeventf_absolute
                                  0))
                        (if screen-pos (cg:position-x screen-pos) 0)
                        (if screen-pos (cg:position-y screen-pos) 0)
                        0 0))
    #+old ;; This works, but mouse_event may be more robust.
    (win:sendmessage (cg:handle window) win:wm_lbuttondown 
      win:mk_lbutton (win:makelong (cg:position-x window-pos)
                                   (cg:position-y window-pos)))
    (when down-seconds (sleep down-seconds))
    (when up
      (win::mouse_event up-event 0 40000 0 0))
    ))
 
;; ------------------------------------------------------------
;; key presses
 
(defconstant win::keyeventf_extendedkey 1)
(defconstant win::keyeventf_keyup       2)
 
(ff:def-foreign-call (win::keybd_event "keybd_event")
    ((win::bvk byte)
     (win::bscan byte)
     (win::dwflags win::dword)
     (win::dwextrainfo win::dword))
  :convention :stdcall
  :release-heap :when-ok
  :arg-checking nil
  :returning :void)
 
(defgeneric do-keypress (window-or-widget-or-nil
                         keynum-or-character
                         &key (preview-seconds 0.5)
                         shift control alt
                         (down-seconds 0.5)(down t)(up t))
  (:documentation
   #.(format nil "Simulates pressing and/or releasing a key ~
       on the keyboard while some window has the focus.")))
 
(defmethod do-keypress ((widget cg:dialog-item)(keynum-or-char t)
                        &key (preview-seconds 0.5)(down-seconds 0.5)
                        shift control alt
                        (down t)(up t))
  (let* ((window (cg:window widget)))
    (when (cg:windowp window)
      (do-keypress window keynum-or-char
        :down-seconds down-seconds
        :preview-seconds preview-seconds
        :shift shift :control control :alt alt
        :down down :up up))))
 
(defmethod do-keypress ((window t)(keynum integer)
                        &key (preview-seconds 0.5)(down-seconds 0.5)
                        shift control alt
                        (down t)(up t))
  (declare (ignore shift control alt))
  
  ;; Expose the window and its parents all the way up
  (when window
    (let* ((win window))
      (loop (unless (cg:windowp win)(return))
            (unless (eq win (cg:selected-window (cg:parent win)))
              (select-window win))
            (setq win (cg:parent win))))
      
    ;; Make sure the window has the keyboard focus.
    (unless (eq window (cg:get-focus (cg:screen cg:*system*)))
      (win:setfocus (cg:handle window))))
    
  ;; Wait a bit for the user to see the window before the click is done.
  (when preview-seconds (sleep preview-seconds))
  
  ;; Send the click down and up messages, pausing a bit in between
  ;; so that a human can see any effect of the key being down.
  (when down
    (win::keybd_event keynum 0 0 0)
    #+old ;; this doesn't seem to work
    (win:sendmessage (cg:handle window) win:wm_keydown keynum
      1)) ;; "repeat" the keypress one time
  (when down-seconds (sleep down-seconds))
  (when up
    (win::keybd_event keynum 0 win::keyeventf_keyup 0)
    #+old
    (win:sendmessage (cg:handle window) win:wm_keyup keynum
      (logior (expt 2 31) ;; transition flag
              (expt 2 30) ;; flag that key was down
              1)) ;; "repeat" the keypress one time
    ))
 
(defmethod do-keypress ((window t)(char character)
                        &key (preview-seconds 0.5)(down-seconds 0.5)
                        shift control alt
                        (down t)(up t))
  (let* ((vk (win:VkKeyScan (char-int char)))
         (key-number (cg::lobyte vk))
         (shift-keys (cg::hibyte vk))
         (upper-case? (logbitp 0 shift-keys)))
    (when preview-seconds
      (sleep preview-seconds))
    (when alt
      (do-keypress window cg:vk-alt :up nil
        :preview-seconds nil :down-seconds nil))
    (when control
      (do-keypress window cg:vk-control :up nil
        :preview-seconds nil :down-seconds nil))
    (when (or shift upper-case?)
      (do-keypress window cg:vk-shift :up nil
        :preview-seconds nil :down-seconds nil))
    (do-keypress window key-number
      :preview-seconds nil
      :down-seconds down-seconds
      :down down :up up)
    (when (or shift upper-case?)
      (do-keypress window cg:vk-shift :down nil
        :preview-seconds nil :down-seconds nil))
    (when control
      (do-keypress window cg:vk-control :down nil
        :preview-seconds nil :down-seconds nil))
    (when alt
      (do-keypress window cg:vk-alt :down nil
        :preview-seconds nil :down-seconds nil))
    ))
         
(defmethod do-keypresses ((window t)(object string)
                          &key (preview-seconds 0.5)(down-seconds 0.5))
  (let* ((length (length object)))
    (dotimes (j length)
      (do-keypress
        window (aref object j) 
        :preview-seconds (if (eq j 0) preview-seconds nil)
        :down-seconds (if (eq j (1- length)) down-seconds)))))
 
(defmethod do-keypresses ((window t)(object symbol)
                          &key (preview-seconds 0.5)(down-seconds 0.5))
  (do-keypresses window (symbol-name object)
    :preview-seconds preview-seconds
    :down-seconds down-seconds))
 
(defmethod do-keypresses ((window t)(object t)
                          &key (preview-seconds 0.5)(down-seconds 0.5))
  (do-keypresses window (princ-to-string object)
    :preview-seconds preview-seconds
    :down-seconds down-seconds))

)
#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
