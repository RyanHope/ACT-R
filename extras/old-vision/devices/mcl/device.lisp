;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2004 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : device.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : MCL-specific functions for RPM.  This consists primarily
;;;             : of stuff for vision (parsing the screen), and output
;;;             : stuff for motor.
;;; 
;;; Bugs        : 
;;; 
;;; --- History ---
;;; 01.09.21 mdb [b2]
;;;             : Fixed an infinte recursion bug in APPROACH-WIDTH.
;;; 2002.04.16 mdb [b6]
;;;             : * Rolled in color text stuff.
;;;             : * Added BUILD-FEATURES-FOR methods for radio buttons and
;;;             : check boxes.
;;; 2002.04.18  mdb
;;;             : Fixed minor glitch created by color text stuff--if the part
;;;             : color was not set, that passed NIL to the color parser.  No.
;;; 2002.05.17 mdb
;;;             : Moved COLOR-SYMBOL->MAC-COLOR here.
;;; 2002.06.05 mdb
;;;             : Grr, fixed what is hopefully the last vector bug issue.
;;; 
;;; 2002.06.21 Dan [b7]
;;;             : Changed the rpm-window class to rpm-real-window and
;;;             : updated the methods accordingly.
;;; 2002.06.30 Dan
;;;             : Changed the COLOR-SYMBOL->MAC-COLOR and MAC-COLOR->SYMBOL
;;;             : function names by replacing MAC with SYSTEM to be a little
;;;             : more consistent (that way there aren't as many 'different'
;;;             : function names floating around in these files).
;;;             : Moved the view-line stuff in here from the separate file and
;;;             : documented it better.
;;;             : Removed all of the UWI code from this file.
;;; 2002.07.03 mdb
;;;             : Makes sure that SPEECH-AVAILABLE-P is defined.
;;; 2002.11.25 mdb [2.1f1]
;;;             : Added DEVICE-MOVE-CURSOR-TO for MCL5.0 on OSX. 
;;; 2003.03.11 mdb [2.1.2]
;;;             : Per DB's suggestion, cut back on EVENT-DISPATCHing. 
;;; 2003.06.18 mdb
;;;             : Turns out static text dialog items support multiple kinds
;;;             : of justifications, though it's hard to get at it.  Now
;;;             : handled properly. 
;;; 2003.06.23 mdb [2.1.3]
;;;             : Under-the-hood addition of RPM-OVERLAY class. 
;;; 2004.03.11 mdb [2.2]
;;;             : Added a VIEW-KEY-EVENT-HANDLER method for editable text dialog
;;;             : items, which used to break.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : changed the name to device to be placed in a folder called mcl
;;;             : removed references to *mp* and other minor
;;;             : ACT-R 6 updates
;;; 2006.09.07 Dan
;;;             : * Removed the fill-default-dimensions method because it's
;;;             :   now defined in the vision file.
;;; 2007.07.03 mdb
;;;             : * Fixed handling of multi-line static text items.
;;;             : * Check boxes now draw a blue box if checked under OS X.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defparameter *crosshair-cursor* 
  (#_GetCursor #$crossCursor) "Crosshair cursor")

(defvar *attn-tracker* nil "Holds the view for the focus ring.")
(defparameter *last-update* (get-internal-real-time))

;;;; RPM-LINE-TO is meant to be called in the VIEW-DRAW-CONTENTS method for
;;;; window, which should be called after PM-PROC-DISPLAY.  I know this is
;;;; is stupid, but I'm having trouble getting this to work right with views.

;;; RPM-LINE-TO      [Function]
;;; Date        : 97.03.31
;;; Description : For use in building interfaces.  If there is to be a lot of
;;;             : drawing, then there will probably be a lot of LINE-TO 
;;;             : commands.  After making the window in question the current
;;;             : RPM window (via RPM-INSTALL-WINDOW), then call this function
;;;             : instead.  Note that the point should be in the coordinate
;;;             : system of the window.

(defun rpm-line-to (the-view h &optional v)
  "Adds a line to the icon and draws in in the default window. Designed to be called by the VIEW-DRAW-CONTENTS method for the window."
  (let* (;DAN (the-wind (device (device-interface *mp*)))
         (the-wind (current-device))
         (start-point (pen-position the-wind))
         (end-point (if v (make-point h v) h))
         (the-offset (view-position the-view)))
    (line-to the-view end-point)
    (rpm-build-line (p2vpt (add-points start-point the-offset))
                    (p2vpt (add-points the-offset end-point)))
    end-point))


;;; RPM-BUILD-LINE      [Function]
;;; Date        : 97.03.31
;;; Description : Called by RPM-LINE-TO, this creates an icon feature based 
;;;             : on the beginning and end points.

(defun rpm-build-line (start-pt end-pt)
  "Build a line feature in the visual interface"
  ;  (declare (point start-pt) (point end-pt))
  (push
   (make-instance 'line-feature
     :end1-x (px start-pt) :end1-y (py start-pt)
     :end2-x (px end-pt) :end2-y (py end-pt)
     :x (loc-avg (px start-pt) (py end-pt))
     :y (loc-avg (px start-pt) (py end-pt))
     :width (abs (- (px start-pt) (py end-pt)))
     :height (abs (- (px start-pt) (py end-pt))))
   ; DAN (visicon (vis-m *mp*))))
   (visicon (get-module :vision ))))

(defun loc-avg (x y)
  "Return the 'location' (integer) average of <x> and <y>."
  (declare (fixnum x) (fixnum y))
  (floor (/ (+ x y) 2)))


;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; MCL screen-to-icon interface
;;;; ---------------------------------------------------------------------- ;;;;;;;


;;; BUILD-FEATURES-FOR      [Method]
;;; Description : For an MCL window, just walk the sub-objects (which by
;;;             : default are subviews) and build features.

(defmethod build-features-for ((self window) (vis-mod vision-module))
  (let ((base-ls (flatten
                    (mapcar #'(lambda (obj) (build-features-for obj vis-mod))
                            (get-sub-objects self)))))
    (dolist (feat base-ls)
      (fill-default-dimensions feat))
    base-ls))


(defgeneric get-sub-objects (view)
  (:documentation  "Grabbing the sub-objects of a view by default returns the subviews."))

(defmethod get-sub-objects ((v view))
  (subviews v))


;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 97.04.02
;;; Description : Builder of features for a generic view.  Doesn't do anything
;;;             : based on the view itself, but it will call the 
;;;             : feature-builder on all subviews.

(defmethod build-features-for ((self view) (vis-mod vision-module))
  (let ((subs (get-sub-objects self))
        (outlis nil))
    (dolist (sub subs outlis)
      (push (build-features-for sub vis-mod) outlis))))



;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 97.01.27
;;; Description : The basic method for building features, returns an object
;;;             : of the ICON-FEATURE class with the location and ISA fields
;;;             : set.

(defmethod build-features-for ((self dialog-item) (vis-mod vision-module))
  (declare (ignore vis-mod))
  (make-instance 'icon-feature
    :x (px (view-loc self))
    :y (py (view-loc self))
    :kind 'visual-object
    :value 'unknown
    :screen-obj self))


;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 97.01.28
;;; Description : Same as for EDIT-BOXes.

(defmethod build-features-for ((self editable-text-dialog-item)
                                  (vis-mod vision-module))
  (let ((font-spec (view-font self))
        (text (dialog-item-text self)))
    (cons
     (make-instance 'rect-feature 
       :x (px (view-loc self))
       :y (py (view-loc self))
       :screen-obj self)
     (unless (equal text "")
       (multiple-value-bind (ascent descent)
                            (font-info font-spec)
         (build-string-feats vis-mod :text text
                             :start-x (1+ (point-h (view-position self)))
                             :y-pos (+ (point-v (view-position self))
                                       descent (round ascent 2))
                             :width-fct #'(lambda (str)
                                            (string-width str font-spec))
                             :height ascent :obj self))))))


;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 99.04.02, delta 99.07.02
;;; Description : A button dialog item is a lot like a static text item,
;;;             : except there's an oval associated with it and the text
;;;             : is centered both horizontally and vertically.

(defmethod build-features-for ((self button-dialog-item)
                                  (vis-mod vision-module))
  (let ((btn-width (point-h (view-size self)))
        (btn-height (point-v (view-size self)))
        (text (dialog-item-text self)))
    (cons
     (make-instance 'oval-feature 
       :x (px (view-loc self)) :y (py (view-loc self))
       :width btn-width :height btn-height :color 'light-gray
       :screen-obj self) 
     (unless (equal text "")
       (let* ((font-spec (view-font self))
              (start-y nil)
              (accum nil)
              (textlines (string-to-lines text))
              (width-fct #'(lambda (str) (string-width str font-spec))))
         (multiple-value-bind (ascent descent) (font-info font-spec)
           (setf start-y (+ (point-v (view-position self))
                            (round (- btn-height (* (length textlines)
                                                    (+ ascent descent))) 2)))
           (dolist (item textlines (nreverse accum))
             (push
              (build-string-feats vis-mod :text item
                                  :start-x 
                                  (+ (point-h (view-position self))
                                     (round 
                                      (- btn-width (funcall width-fct item))
                                      2))
                                  :y-pos 
                                  (+ start-y (round (+ ascent descent) 2))
                                  :width-fct width-fct 
                                  :height (min ascent btn-height)
                                  :obj self)
              accum)
             (incf start-y (+ ascent descent)))))))))



;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 02.04.16
;;; Description : A radio button is like a regular button, except that the
;;;             : oval is small and might be gray [unselected] or gray
;;;             : [selected].  Text is also not horizontally centered.

(defmethod build-features-for ((self radio-button-dialog-item)
                                  (vis-mod vision-module))
  (let* ((btn-height (point-v (view-size self)))
         (text (dialog-item-text self)))
    (cons
     (make-instance 'oval-feature 
       :x (+ 7 (point-h (view-position self))) :y (py (view-loc self))
       :width 11 :height 11 :screen-obj self
       :color (if (radio-button-pushed-p self)
                'black
                'light-gray))
     (unless (equal text "")
       (let* ((font-spec (view-font self))
              (start-y nil)
              (accum nil)
              (textlines (string-to-lines text))
              (width-fct #'(lambda (str) (string-width str font-spec))))
         (multiple-value-bind (ascent descent) (font-info font-spec)
           (setf start-y (+ (point-v (view-position self))
                            (round (- btn-height (* (length textlines)
                                                    (+ ascent descent))) 2)))
           (dolist (item textlines (nreverse accum))
             (push
              (build-string-feats vis-mod :text item
                                  :start-x 
                                  (+ (point-h (view-position self))
                                     17)
                                  :y-pos 
                                  (+ start-y (round (+ ascent descent) 2))
                                  :width-fct width-fct 
                                  :height (min ascent btn-height)
                                  :obj self)
              accum)
             (incf start-y (+ ascent descent)))))))))


;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 02.04.16
;;; Description : Very much like radio buttons, but if checked add an 
;;;             : "X" to the output.

(defmethod build-features-for ((self check-box-dialog-item)
                                  (vis-mod vision-module))
  (let ((btn-height (point-v (view-size self)))
        (text (dialog-item-text self))
        (feats nil))
    (setf feats
          (cons
           (make-instance 'rect-feature 
             :x (+ 8 (point-h (view-position self))) :y (py (view-loc self))
             :width 11 :height 11 :color (if (and (check-box-checked-p self)
                                                  (osx-p))
                                           'blue
                                           'light-gray)
             :screen-obj self)
           (unless (equal text "")
             (let* ((font-spec (view-font self))
                    (start-y nil)
                    (accum nil)
                    (textlines (string-to-lines text))
                    (width-fct #'(lambda (str) (string-width str font-spec))))
               (multiple-value-bind (ascent descent) (font-info font-spec)
                 (setf start-y (+ (point-v (view-position self))
                                  (round (- btn-height (* (length textlines)
                                                          (+ ascent descent))) 2)))
                 (dolist (item textlines (nreverse accum))
                   (push
                    (build-string-feats vis-mod :text item
                                        :start-x 
                                        (+ (point-h (view-position self))
                                           17)
                                        :y-pos 
                                        (+ start-y (round (+ ascent descent) 2))
                                        :width-fct width-fct 
                                        :height (min ascent btn-height)
                                        :obj self)
                    accum)
                   (incf start-y (+ ascent descent))))))))
    (when (check-box-checked-p self)
      (setf feats
            (cons
             (make-instance 'icon-feature
               :x (+ 8 (point-h (view-position self)))
               :y (py (view-loc self))
               :kind 'visual-object
               :value 'check
               :screen-obj self
               :height 11
               :width 11)               
             
             feats)))
    feats
    ))


(defmethod approach-width ((feat text-feature) (theta number))
  (let ((screen-obj (screen-obj feat)))
    (if (button-p screen-obj)
      (let ((new-feat 
             (find-if #'(lambda (f) (and (eq (screen-obj f) screen-obj)
                                          (eq (kind f) 'oval)))
                      ;DAN (visicon (vis-m *mp*)))))
                      (visicon (get-module :vision)))))
        (if new-feat
          (approach-width new-feat theta)
          (call-next-method)))
      (call-next-method))))


(defmethod button-p (obj)
  (declare (ignore obj))
  nil)

(defmethod button-p ((obj button-dialog-item))
  (declare (ignore obj))
  t)

;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 99.04.02
;;; Description : A static text dialog item is really just text, so just
;;;             : build the string features for it.

(defmethod build-features-for ((self static-text-dialog-item)
                                  (vis-mod vision-module))
  (let ((text (dialog-item-text self)))
    (unless (equal text "")
      (let* ((font-spec (view-font self))
             (start-y nil)
             (accum nil)
             (textlines (string-to-lines text))
             (width-fct #'(lambda (str) (string-width str font-spec))))
        (multiple-value-bind (ascent descent) (font-info font-spec)
          (setf start-y (point-v (view-position self)))
          (dolist (item textlines (nreverse accum))
            (push
             (build-string-feats vis-mod :text item
                                 :start-x (xstart self)                               
                                 :y-pos 
                                 (+ start-y (round (+ ascent descent) 2))
                                 :width-fct width-fct 
                                 :height ascent
                                 :obj self)
             accum)
            (incf start-y (+ ascent descent))))))
    ))




#| the old BUILD-FEATURES-FOR for static text dialog items
    (unless (equal text "")
      (multiple-value-bind (ascent descent)
                           (font-info font-spec)
        (build-string-feats vis-mod :text text
                            :start-x (xstart self)
                            :y-pos (+ (point-v (view-position self))
                                      descent (round ascent 2))
                            :width-fct #'(lambda (str)
                                           (string-width str font-spec))
                            :height ascent :obj self)))
))
|#

(defmethod xstart ((self static-text-dialog-item))
  (let ((left-x (point-h (view-position self)))
        (text-width (string-width (dialog-item-text self)
                                  (view-font self)))
        (text-justification
         (or (cdr (assq (slot-value self 'ccl::text-justification)
                        '((:left . #.#$tejustleft)
                          (:center . #.#$tejustcenter)
                          (:right . #.#$tejustright))))
             (require-type (slot-value self 'ccl::text-justification) 'fixnum)))
        )
    (ecase text-justification
      (#.#$teJustLeft (1+ left-x))
      (#.#$teJustCenter (+ 1 left-x (round (/ (- (width self) text-width) 2))))
      (#.#$teJustRight (+ 1 left-x (- (width self) text-width))))))
 


;;; CURSOR-TO-FEATURE      [Function]
;;; Date        : 97.06.16
;;; Description : Returns a feature representing the current state and shape
;;;             : of the cursor.

(defmethod cursor-to-feature ((the-window window))
  (let ((pos (view-mouse-position the-window))
        (shape (window-cursor the-window)))
    (when (cursor-in-window-p the-window)
      (make-instance 'cursor-feature
        :x (point-h pos) :y (point-v pos)
        :value (case shape
                 (*i-beam-cursor* 'I-BEAM)
                 (*crosshair-cursor* 'CROSSHAIR)
                 (*watch-cursor* 'WATCH)
                 (otherwise 'POINTER))))))


(defgeneric cursor-in-window-p (wind)
  (:documentation  "Returns T if the cursor is over <wind>, NIL otherwise."))

(defmethod cursor-in-window-p ((tw window))
  (when (window-shown-p tw)
    (rlet ((the-rect rect))
      (points-to-rect (view-position tw)
                      (add-points (view-position tw) (view-size tw))
                      the-rect)
      (point-in-rect-p the-rect 
                       (local-to-global tw (view-mouse-position tw))))))



(defmethod view-loc ((self view))
  (let ((pos (view-position self))
        (size (view-size self)))
    (vector (round (+ (point-h pos) (/ (point-h size) 2)))
            (round (+ (point-v pos) (/ (point-v size) 2))))))


(defmethod view-loc ((self simple-view))
  (let ((pos (view-position self))
        (size (view-size self)))
    (vector (round (+ (point-h pos) (/ (point-h size) 2)))
            (round (+ (point-v pos) (/ (point-v size) 2))))))


(defmethod view-loc ((self symbol))
  (if (eq self :cursor)
    ;DAN (get-mouse-coordinates (device (device-interface *mp*)))
    (get-mouse-coordinates (current-device))
    (error "!! Can't find location of ~S" self)))


(defmethod width ((self simple-view))
  (point-h (view-size self)))


(defmethod height ((self simple-view))
  (point-v (view-size self)))

#| Moved to vision

;;; FILL-DEFAULT-DIMENSIONS      [Method]
;;; Date        : 99.04.02
;;; Description : The base methods for most MCL views don't set the height or
;;;             : width attributes of the features they generate, nor their
;;;             : size.  Set that up if necessary.

(defgeneric fill-default-dimensions (feat)
  (:documentation  "Fill in the width, height, and size of an icon feature."))

(defmethod fill-default-dimensions ((feat icon-feature))
  (aif (simple-size feat)
    (setf (size feat) it)
    (if (null (screen-obj feat))
      (setf (size feat) 1.0)              ; should be default size, eh
      (progn
        (unless (width feat) (setf (width feat) (width (screen-obj feat))))
        (unless (height feat) (setf (height feat) (height (screen-obj feat))))
        (setf (size feat) (simple-size feat))))))

|#
;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; The view based line drawing classes and methods
;;;; ---------------------------------------------------------------------- ;;;;;;;

;;; LINER      [Class]
;;; Description : The base class for the view based lines.  
;;;             : All it adds to a simple-view is a color slot that defaults
;;;             : to black.

(defclass liner (simple-view)
  ((color :accessor color :initarg :color :initform *black-color*)))

;;; POINT-IN-CLICK-REGION-P      [Method]
;;; Description : Override this method so that lines don't handle mouse clicks.

(defmethod point-in-click-region-p ((self liner) where)
  (declare (ignore where))
  nil)


;;; TD-LINER      [Class]
;;; Description : A view that represents a line which is drawn top-down 
;;;             : i.e. from the view-position (upper-left) to the 
;;;             : [view-size - (1,1)] (lower-right) in the container window

(defclass td-liner (liner)
  ())

;;;  A view that represents a line which is drawn bottom-up i.e. from the
;;;  view's lower-left to the view's upper-right in the container window.

;;; BU-LINER      [Class]
;;; Description : A view that represents a line which is drawn bottom-up 
;;;             : i.e. from the view's lower-left to the view's upper-rignt
;;;             : in the container window

(defclass bu-liner (liner)
  ())

;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : Draw a top-down line on it's container window.

(defmethod view-draw-contents ((lnr td-liner))
  "Draws the line on the view-container window using the color specified
   and restoring the previous draw color and pen position"
  (let* ((parent (view-container lnr))
         (old-point (pen-position parent))
         (old-color (get-fore-color parent))
         (other-end (add-points (view-size lnr) (view-position lnr))))
    (set-fore-color parent (color lnr))
    (move-to parent (view-position lnr))
    (line-to parent (make-point (1- (point-h other-end))
                                (1- (point-v other-end))))
    (set-fore-color parent old-color)
    (move-to parent old-point)))

;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : Draw a bottom-up line on it's container window.

(defmethod view-draw-contents ((lnr bu-liner))
  "Draws the line on the view-container window using the color specified
   and restoring the previous draw color and pen position"
  (let* ((parent (view-container lnr))
         (old-point (pen-position parent))
         (old-color (get-fore-color parent)))
    (set-fore-color parent (color lnr))
    (move-to parent (make-point (point-h (view-position lnr))
                                (1- (point-v (add-points (view-position lnr) (view-size lnr))))))
    (line-to parent (make-point (1- (point-h (add-points (view-size lnr) (view-position lnr))))
                                (point-v (view-position lnr))))
    (set-fore-color parent old-color)
    (move-to parent old-point)))


;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : A td-liner is just a line-feature located "at" it's mid-point.

(defmethod build-features-for ((lnr td-liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let ((start-pt (view-position lnr))
        (end-pt (subtract-points (add-points (view-position lnr) (view-size lnr)) 
                                  (make-point 1 1))))
    (make-instance 'line-feature
      :color (system-color->symbol (color lnr))
      :end1-x (point-h start-pt) 
      :end1-y (point-v start-pt)
      :end2-x (point-h end-pt) 
      :end2-y (point-v end-pt)
      :x (loc-avg (point-h start-pt) (point-h end-pt))
      :y (loc-avg (point-v start-pt) (point-v end-pt))
      :width (abs (- (point-h start-pt) (point-h end-pt)))
      :height (abs (- (point-v start-pt) (point-v end-pt))))))


;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : A bu-liner is just a line-feature located "at" it's mid-point.

(defmethod build-features-for ((lnr bu-liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let ((start-pt (add-points (view-position lnr)
                                (make-point 0 (1- (point-v (view-size lnr))))))
        
        
        (end-pt (add-points (view-position lnr) 
                             (make-point (1- (point-h (view-size lnr))) 0))))
    (make-instance 'line-feature
      :color (system-color->symbol (color lnr))
      :end1-x (point-h start-pt) 
      :end1-y (point-v start-pt)
      :end2-x (point-h end-pt) 
      :end2-y (point-v end-pt)
      
      :x (loc-avg (point-h start-pt) (point-h end-pt))
      :y (loc-avg (point-v start-pt) (point-v end-pt))
      :width (abs (- (point-h start-pt) (point-h end-pt)))
      :height (abs (- (point-v start-pt) (point-v end-pt))))))

;;; RPM-VIEW-LINE [Function]
;;; Description : Add a view to the window that displays a line defined by
;;;             : the start and end points in the color supplied (an MCL
;;;             : system style color).

(defun rpm-view-line (wind start-pt end-pt &optional (color *black-color*))
  "Adds a view in the specified window which draws a line from the start-pt to the end-pt
   using the optional color specified (defaulting to black).  This view will add features 
   to the icon on PM-PROC-DISPLAY."
  (let* ((gx (> (point-h end-pt) (point-h start-pt)))
         (gy (> (point-v end-pt) (point-v start-pt)))
         (vs (subtract-points start-pt end-pt)))
    (setf vs (make-point (+ 1 (abs (point-h vs)))
                         (+ 1 (abs (point-v vs)))))
    (add-subviews wind (cond ((and gx gy)
                              (make-instance 'td-liner
                                :color color
                                :view-position start-pt 
                                :view-size vs))
                             ((and (not gx) (not gy))
                              (make-instance 'td-liner
                                :color color
                                :view-position end-pt 
                                :view-size vs))
                             ((and gx (not gy))
                              (make-instance 'bu-liner
                                :color color
                                :view-position (make-point (point-h start-pt) (point-v end-pt))
                                :view-size vs))
                             (t
                              (make-instance 'bu-liner
                                :color color
                                :view-position (make-point (point-h end-pt) (point-v start-pt))
                                :view-size vs))))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Utilities
;;;; ---------------------------------------------------------------------- ;;;;

;;; XY->POINT      [Function]
;;; Description : Converts an (X Y) list into an MCL/Quickdraw point.

(defun xy->point (xy)
  "(x y) to point converstion function. Deprecated, use vpt2p instead."
  (declare (list xy))
  (make-point (first xy) (second xy)))


;;; P2XY      [Function]
;;; Description : Takes an MCL/Quickdraw point and returns an XY list

(defun p2xy (p)
  "Coverts an MCL/Quickdraw point to an XY list.  Deprecated, use p2vpt instead."
  ;  (declare (point p))
  (list (point-h p) (point-v p)))


(defun p2vpt (p)
  "Convert an MCL/Quickdraw point to #(x y) format."
  (declare (inline p2vpt))
  (vector (point-h p) (point-v p)))


(defun vpt2p (mpt)
  "Convert an #(x y) format point to MCL/Quickdraw format."
  (declare (vector mpt) (inline vpt2p))
  (make-point (px mpt) (py mpt)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; RPM device methods.
;;;; ---------------------------------------------------------------------- ;;;;

;;; DEVICE-HANDLE-KEYPRESS      [Method]
;;; Description : Just call VIEW-KEY-EVENT-HANDLER and make sure that the 
;;;             : event gets dealt with.

(defmethod device-handle-keypress ((device window) key)
  (view-key-event-handler device key)
  (event-dispatch)) 


;;; VIEW-KEY-EVENT-HANDLER      [Method]
;;; Description : ACT-R couldn't actually type into editiable text dialog
;;;             : items because the default method required that 
;;;             : *current-event* be bound, which of course it wouldn't be.
;;;             : So hack around it.

(defmethod view-key-event-handler ((view editable-text-dialog-item) 
                                       (char character))
  (if (not *actr-enabled-p*)
    (call-next-method)
    (progn
      (cond ((graphic-char-p char) (ed-insert-char view char))
            ((char= char #\backspace) (ed-rubout-char view))
            )
      (view-draw-contents view)))
  )


;;; DEVICE-HANDLE-CLICK      [Method]
;;; Description : Again, just call the base MCL method and dispatch.

(defmethod device-handle-click ((device window))
  (view-click-event-handler device (view-mouse-position device))
  (event-dispatch))


;;; DEVICE-MOVE-CURSOR-TO      [Method]
;;; Date        : 97.02.18 [revised 98.10.29]
;;; Description : Since moving the mouse is considered a Bad Thing by 
;;;             : Apple's HI police, you can't just make a simple call
;;;             : to do it.  First, there's moving the cursor, which
;;;             : involves blasting into low memory.  Then, if the cursor
;;;             : is being tracked by the system, we have to make sure that 
;;;             : the cursor move has really been registered (#$CrsrNew 
;;;             : changes from -1 to 255 when this happens) by the OS.  Then 
;;;             : make sure it's been registered by MCL with UPDATE-CURSOR.

#-ccl-4.3.1
(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (let ((absloc (local-to-global device (px xyloc) (py xyloc))))
    (without-interrupts
     (ccl::%put-point (%int-to-ptr #$MTemp) absloc)
     (ccl::%put-point (%int-to-ptr #$RawMouse) absloc)
     (%put-word (%int-to-ptr #$CrsrNew) -1))
    (while (eql (%get-signed-word (%int-to-ptr #$CrsrNew)) -1))
    (update-cursor)
    (while (not (vpt= xyloc (p2vpt (view-mouse-position device))))
      (event-dispatch))))

#+ccl-4.3.1
(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (let ((absloc (local-to-global device (px xyloc) (py xyloc))))
    (without-interrupts
     ;(ccl::%put-point (%int-to-ptr #$MTemp) absloc)
     (#_LMSetMouseTemp absloc)
     ;(ccl::%put-point (%int-to-ptr #$RawMouse) absloc)
     (#_LMSetRawMouseLocation absloc)
     ;(%put-word (%int-to-ptr #$CrsrNew) -1)
     (#_LMSetCursorNew -1)
     )
    ;(while (eql (%get-signed-word (%int-to-ptr #$CrsrNew)) -1))
    (while (eql (#_LMGetCursorNew) -1))
    (update-cursor)
    (while (not (vpt= xyloc (p2vpt (view-mouse-position device))))
      (event-dispatch))))


(unless (fboundp 'speech-available-p)
  (defun speech-available-p () nil))



;;; DEVICE-SPEAK-STRING      [Method]
;;; Description : If the Mac Speech Manager is installed, actually speak the
;;;             : string.

(defmethod device-speak-string ((device window) string)
  (when (speech-available-p)
    (speak-string string)
    ))


;;; GET-MOUSE-COORDINATES      [Method]
;;; Description : Return the current mouse loc in #(x y) format.

(defmethod get-mouse-coordinates ((device window))
  (p2vpt (view-mouse-position device)))


;;; DEVICE-UPDATE      [Method]
;;; Date        : 03.03.11
;;; Description : Rather than calling EVENT-DISPATCH on every cycle, call it
;;;             : only at about 10Hz.

(defmethod device-update :after ((wind window) time)
  (declare (ignore wind time))
  (when (< 100 (- (get-internal-real-time) *last-update*))
    (event-dispatch)
    (setf *last-update* (get-internal-real-time)))
  )


#|
(defmethod do-update :after ((mstr-proc master-process) current-time 
                               &key (real-wait nil))
  (declare (ignore current-time real-wait))
  (event-dispatch))
|#




(defmethod populate-loc-to-key-array ((ar array))
  "Sets all the keys in the array that need to be set"
  ;; function key row
  (setf (aref ar 0 0) #\ESC)
  (setf (aref ar 2 0) #\2061)
  (setf (aref ar 3 0) #\2062)
  (setf (aref ar 4 0) #\2063)
  (setf (aref ar 5 0) #\2064)
  (setf (aref ar 7 0) #\2065)
  (setf (aref ar 8 0) #\2066)
  (setf (aref ar 9 0) #\2067)
  (setf (aref ar 10 0) #\2070)
  (setf (aref ar 12 0) #\2071)
  (setf (aref ar 13 0) #\2101)
  (setf (aref ar 14 0) #\2102)
  (setf (aref ar 15 0) #\2103)
  (setf (aref ar 17 0) #\2014)
  (setf (aref ar 18 0) #\2015)
  (setf (aref ar 19 0) #\2016)
  ;; numeric key row
  (setf (aref ar 0 2) #\tab)
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
  (setf (aref ar 13 2) #\Delete)
  (setf (aref ar 15 2) #\help)
  (setf (aref ar 16 2) #\home)
  (setf (aref ar 17 2) #\pageup)
  (setf (aref ar 19 2) #\ESC)
  (setf (aref ar 20 2) #\=)
  (setf (aref ar 21 2) #\/)
  (setf (aref ar 22 2) #\*)
  ;; qwerty row
  (setf (aref ar 0 3) #\Tab)
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
  (setf (aref ar 15 3) #\DEL)
  (setf (aref ar 16 3) #\End)
  (setf (aref ar 17 3) #\Page)
  (setf (aref ar 19 3) #\7)
  (setf (aref ar 20 3) #\8)
  (setf (aref ar 21 3) #\9)
  (setf (aref ar 22 3) #\-)
  ;; ASDF row
  (setf (aref ar 0 4) 'caps-lock)
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
  (setf (aref ar 11 4) #\')
  (setf (aref ar 12 4) #\Newline)
  (setf (aref ar 13 4) #\Newline)
  (setf (aref ar 19 4) #\4)
  (setf (aref ar 20 4) #\5)
  (setf (aref ar 21 4) #\6)
  (setf (aref ar 22 4) #\+)
  ;; Z row
  (setf (aref ar 0 5) 'shift)
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
  (setf (aref ar 11 5) 'shift)
  (setf (aref ar 12 5) 'shift)
  (setf (aref ar 16 5) #\UpArrow)
  (setf (aref ar 19 5) #\1)
  (setf (aref ar 20 5) #\2)
  (setf (aref ar 21 5) #\3)
  (setf (aref ar 22 5) #\Enter)
  ;; space bar row
  (setf (aref ar 0 6) 'control)
  (setf (aref ar 1 6) 'option)
  (setf (aref ar 2 6) 'command)
  (setf (aref ar 3 6) #\Space)
  (setf (aref ar 4 6) #\Space)
  (setf (aref ar 5 6) #\Space)
  (setf (aref ar 6 6) #\Space)
  (setf (aref ar 7 6) #\Space)
  (setf (aref ar 8 6) #\Space)
  (setf (aref ar 9 6) #\Space)
  (setf (aref ar 10 6) #\Space)
  (setf (aref ar 11 6) 'command)
  (setf (aref ar 12 6) 'option)
  (setf (aref ar 13 6) 'control)
  (setf (aref ar 15 6) #\BackArrow)
  (setf (aref ar 16 6) #\DownArrow)
  (setf (aref ar 17 6) #\ForwardArrow)
  (setf (aref ar 19 6) #\0)
  (setf (aref ar 20 6) #\0)
  (setf (aref ar 21 6) #\.)
  (setf (aref ar 22 6) #\Enter)
  ar)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; RPM overlay and Focus ring stuff


;;; RPM-OVERLAY      [Class]
;;; Description : If you want a view to be superimposed on a window, but not
;;;             : be visible to RPM, use this class.  The focus ring in RPM
;;;             : is a subclass.
;;;
;;;             : The OFFSET slot is for the difference between the center of
;;;             : the view and the upper-left corner, as a QuickDraw point.
;;;             : For example, for the focus ring its #@(-10 -10).

(defclass rpm-overlay (simple-view)
  ((offset :accessor offset :initarg :offset :initform nil)))


(defgeneric update-me (olay wind xyloc)
  (:documentation "Call this to move the overlay to a specific location within a window."))

(defmethod update-me ((olay rpm-overlay) (wind window) (xyloc vector))
  (set-view-position olay (add-points (offset olay) (vpt2p xyloc)))
  (unless (equal (view-window olay) wind) (add-subviews wind olay))
  (event-dispatch)
  (when (wptr (view-window olay)) (view-draw-contents olay)))


;;; BUILD-FEATURES-FOR      [Method]
;;; Description : We don't want icon features for the focus ring, and since 
;;;             : it'll be a subview a null BUILD-FEATURES-FOR method is 
;;;             : necessary.

(defmethod build-features-for ((olay rpm-overlay) (vm vision-module))
  (declare (ignore olay vm))
  nil)

;;; POINT-IN-CLICK-REGION-P      [Method]
;;; Description : The focus ring will generally be the "front" view, but 
;;;             : having it receive clicks is a Bad Thing (tm) so it's 
;;;             : necessary to override the POINT-IN-CLICK-REGION-P method
;;;             : for this view class.

(defmethod point-in-click-region-p ((olay rpm-overlay) where)
  (declare (ignore olay where))
  nil)


;;; here's the actual focus ring itself

(defclass focus-ring (rpm-overlay)
  ((color :accessor color :initarg :color :initform *red-color*))
  (:default-initargs 
    :view-size #@(19 19)
    :offset #@(-10 -10)))


(defmethod view-draw-contents ((self focus-ring))
  (let ((oldmode (pen-mode self))
        (oldpat (pen-pattern self))
        (oldsize (pen-size self)))
    (set-pen-mode self :pator)
    (set-pen-pattern self *light-gray-pattern*)
    (set-pen-size self 4 4)
    (with-focused-view self
      (with-fore-color (color self)
        (frame-oval self #@(0 0) (view-size self))))
    (set-pen-mode self oldmode)
    (set-pen-pattern self oldpat)
    (set-pen-size self (point-h oldsize) (point-v oldsize))
    ))


;;; DEVICE-UPDATE-ATTENDED-LOC      [Method]
;;; Date        : 00.07.11
;;; Description : When the attended location is updated, update the focus
;;;             : ring.

(defmethod device-update-attended-loc ((wind window) xyloc)
  (update-me *attn-tracker* wind xyloc))


;;; make the fous ring

(eval-when (load eval)
  (setf *attn-tracker* (make-instance 'focus-ring)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; color text stuff

(defmethod build-features-for :around ((self static-text-dialog-item)
                                          (vis-mod vision-module))
  (let ((feats (flatten (call-next-method)))
        (color (system-color->symbol (aif (part-color self :text)
                                        it
                                        *black-color*))))
    (mapcar #'(lambda (f)
                (setf (color f) color)
                f)
            feats)))


(defun system-color->symbol (color)
  "Given an MCL color code, return a symbol representing that color.  Unknown colors get mapped to COLOR-RRRRR-GGGGG-BBBBB."
  (if (null color)
    'black
    (case color
      (#.*black-color* 'black)
      (#.*green-color* 'green)
      (#.*red-color* 'red)
      (#.*blue-color* 'blue)
      (#.*brown-color* 'brown)
      (#.*purple-color* 'purple)
      (#.*pink-color* 'pink)
      (#.*orange-color* 'orange)
      (#.*dark-gray-color* 'dark-gray)
      (#.*light-blue-color* 'light-blue)
      (#.*white-color* 'white)
      (#.*light-gray-color* 'light-gray)
      (#.*dark-green-color* 'dark-green)
      (#.*tan-color* 'tan)
      (#.*yellow-color* 'yellow)
      (otherwise (intern (format nil "COLOR-~5,'0d-~5,'0d-~5,'0d" 
                                 (color-red color) 
                                 (color-green color) 
                                 (color-blue color)))))))

(defun color-symbol->system-color (color)
  "this may look like it should do the inverse of the above, but right now
   it doesn't exactly.  If the color isn't one of the default ones then
   the black color is returned.  It's only being used by the UWI right now,
   so it's simplified for that purpose.  Only colors that the systems have
   in 'common' are used - with the Mac names being the default, in keeping
   with the usual bias :) "
  (cond ((equal color 'red) *red-color*)
        ((equal color 'blue) *blue-color*)
        ((equal color 'green) *green-color*)
        ((equal color 'black) *black-color*)
        ((equal color 'white) *white-color*)
        ((equal color 'pink)  *pink-color*)
        ((equal color 'yellow) *yellow-color*)
        ((equal color 'dark-green) *dark-green-color*)
        ((equal color 'light-blue) *light-blue-color*)
        ((equal color 'purple) *purple-color*)
        ((equal color 'brown) *brown-color*)
        ((equal color 'light-gray) *light-gray-color*)
        ((equal color 'gray) *gray-color*)
        ((equal color 'dark-gray) *dark-gray-color*)
        (t *black-color*)))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; handling mouse movement under MCL 5.0 and OS X.

#+:ccl-5.0
(when (osx-p)
(progn

(defparameter *warp* (lookup-function-in-framework 
                       "CGWarpMouseCursorPosition"))

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (when (and device (wptr device)) (window-select device))
  (setf xyloc (local-to-global device (vpt2p xyloc)))
  (ccl::ppc-ff-call *warp* 
                    :single-float (coerce (point-h xyloc) 'short-float)
                    :single-float (coerce (point-v xyloc) 'short-float)
                    :unsigned-fullword)
  )))

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