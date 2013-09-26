; ----------------------------------------------------------------------
; Begin file: actr6/devices/ccl/device.lisp
; ----------------------------------------------------------------------


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
;;;             : Fixed an infinite recursion bug in APPROACH-WIDTH.
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
;;;             : changed the name to device to be placed in a folder called MCL 
;;;             : removed references to *mp* and other minor
;;;             : ACT-R 6 updates
;;; 2006.09.07 Dan
;;;             : * Removed the fill-default-dimensions method because it's
;;;             :   now defined in the vision file.
;;; 2007.07.02 Dan
;;;             : * Converted things over for the new vision module.
;;; 2007.07.05 Dan
;;;             : * Rolled in the multi-line fix Mike made to the old MCL device.
;;; 2010.03.11 mdb
;;;             : Fixed DEVICE-MOVE-CURSOR-TO for (R)MCL 5.2 under OS X.
;;; 2010.06.03 mdb
;;;             : Fixed XSTART for (R)MCL 5.2 under OS X, which uses NIL for 
;;;             : left-justified text as a default (rather than :left).
;;; 2011.11.21 Dan
;;;             : * Using model-generated-action instead of *actr-enabled-p*
;;;             :   in view-key-event-handler  for editable-text-dialog-items
;;;             :   to determine when to hack the output.
;;; 2012.08.07 cts
;;;             : Tweaked original MCL device.lisp code, and used it to build a
;;;               device for CCL that leverages ccl-simple-view.lisp.
;;;               Note that for any code that is left in this file and is
;;;               commented out, I did not fully understand exactly what is
;;;               was meant for. But all tests are passing without adding the
;;;               code back in. So I'm keeping it commented out. If someone
;;;               fully understands how these pieces should
;;;               work, and sees that the code isn't needed, feel free to
;;;               remove. Or if it is needed, please add it back in.
;;; 2012.08.27 Dan
;;;             : * The device-handle-keypress method now selects the window
;;;             :   before generating the events so that it goes to the right
;;;             :   window.
;;;             : * In the device-handle-keypress method it now waits on a 
;;;             :   semaphore to be set by the view-key-event-handler method
;;;             :   before returning to guarantee the press gets processed.
;;;             :   It doesn't need to delay in the keypress action because of
;;;             :   that so it passes nil for the delay.
;;; 2012.08.29 Dan
;;;             : * Added a timeout to device-handle-keypress so that it doesn't
;;;             :   hang if the semaphore never gets set.  If it's not set in
;;;             :   500ms it prints a warning and just gives up.
;;; 2012.08.30 cts
;;;             : * Added the semaphore method for mouse clicks. Wrapped the 
;;;             :   timeout code into a function and using it for key presses
;;;             :   and mouse clicks. 
;;;             : * Calling event-dispatch one final time after semaphore is triggered,
;;;             :   so that any events created during the keypress/mouseclick
;;;             :   that were queued to run on the NSRunLoop are run before the
;;;             :   keypress/mouseclick method returns.
;;; 2013.01.03 Dan
;;;             : * Clipped the rpm-view-line function (which was already commented out)
;;;             :   to avoid confusion since it isn't needed and contained outdated code.
;;; 2013.02.01 Clayton
;;;             : * Streamlined the device-move-cursor-to call.
;;;             :   The function no longer pauses for a set time to ensure that the move
;;;             :   is registered. Instead, it polls and checks that the move is registered
;;;             :   and exits immediately after success.
;;; 2013.04.10 cts
;;;             : * Feature was added in ccl device to change text color for buttons.
;;;                 Visicon representation for buttons now shows text in appropriate color
;;; 2013.04.20 cts
;;;           : Removed stray commented out code that is no longer necessary.
;;;           : Now spell checking comments and strings in the code.
;;; 2013.04.27 cts
;;;           : Removed unecessary code duplication when building visual objects for lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :allegro-ide) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :allegro-ide) (in-package :cl-user)

(require-compiled "CCL-SIMPLE-VIEW" "ACT-R6:support;ccl-simple-view")

(defun loc-avg (x y)
  "Return the 'location' (integer) average of <x> and <y>."
  (declare (fixnum x) (fixnum y))
  (floor (/ (+ x y) 2)))


;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; MCL screen-to-icon interface
;;;; ---------------------------------------------------------------------- ;;;;;;;



(defmethod build-vis-locs-for ((self window) (vis-mod vision-module))
  (let ((base-ls (flatten
                   (mapcar #'(lambda (obj) (build-vis-locs-for obj vis-mod))
                           (get-sub-objects self)))))
    base-ls))

(defmethod vis-loc-to-obj ((device window) loc)
  (case (chunk-slot-value-fct loc 'kind)
    (cursor
      (fill-default-vis-obj-slots (car (define-chunks (isa cursor))) loc))))

(defgeneric get-sub-objects (view)
  (:documentation  "Grabbing the sub-objects of a view by default returns the subviews."))

(defmethod get-sub-objects ((v simple-view))
  (subviews v))



(defmethod build-vis-locs-for ((self simple-view) (vis-mod vision-module))
  (let ((subs (get-sub-objects self))
        (outlis nil))
    (dolist (sub subs outlis)
      (push (build-vis-locs-for sub vis-mod) outlis))))



(defmethod button-p (obj)
  (declare (ignore obj))
  nil)

(defmethod button-p ((obj button-dialog-item))
  (declare (ignore obj))
  t)

(defmethod build-vis-locs-for ((self static-text-dialog-item)
                               (vis-mod vision-module))
  (let ((text (dialog-item-text self)))
    (unless (equal text "")
      (let* ((font-spec (view-font self))
             (start-y nil)
             (accum nil)
             (textlines (string-to-lines text))
             (width-fct #'(lambda (str) (string-width str font-spec)))
             (color (system-color->symbol (aif (part-color self :text)
                                            it
                                            *black-color*))))
        (multiple-value-bind (ascent descent) (font-info font-spec)
          (setf start-y (point-v (view-position self)))
          (dolist (item textlines)
            (push
              (build-string-feats vis-mod :text item
                                  :start-x (xstart self)                               
                                  :y-pos 
                                  (+ start-y (round (+ ascent descent) 2))
                                  :width-fct width-fct 
                                  :height ascent
                                  :obj self)
              accum)
            (incf start-y (+ ascent descent))))

        (setf accum (flatten (nreverse accum)))
        (dolist (x accum accum)
          (set-chunk-slot-value-fct x 'color color)
          (setf (chunk-visual-object x) self))))))

(defmethod xstart ((self static-text-dialog-item))
  (let ((left-x (point-h (view-position self)))
        (text-width (string-width (dialog-item-text self)
                                  (view-font self)))
        (text-justification (text-just self))
        )
    (ecase text-justification
      (#.#$tejustleft (1+ left-x))
      (#.#$tejustcenter (+ 1 left-x (round (/ (- (width self) text-width) 2))))
      (#.#$tejustright (+ 1 left-x (- (width self) text-width))))))

(defmethod cursor-to-vis-loc ((the-window window))
  (let ((pos (view-mouse-position the-window))
        (shape (window-cursor the-window)))
    (when (cursor-in-window-p the-window)
      (car (define-chunks-fct `((isa visual-location kind cursor 
                                     screen-x ,(round (point-h pos))
                                     screen-y ,(round (point-v pos))
                                     color ,(system-color->symbol (color shape))
                                     value ,(cond ((eq shape *i-beam-cursor*) 'i-beam)
                                                  ((eq shape *crosshair-cursor*) 'crosshair)
                                                  (t 'pointer)))))))))

(defgeneric cursor-in-window-p (wind)
  (:documentation  "Returns T if the cursor is over <wind>, NIL otherwise."))

(defmethod cursor-in-window-p ((tw window))
  (when (window-shown-p tw)
    (let ((size (view-size tw))
          (cpos (view-mouse-position tw)))
      (and (>= (point-h cpos) 0)
           (>= (point-v cpos) 0)
           (<= (point-h cpos) (point-h size))
           (<= (point-v cpos) (point-v size))))))

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


;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : A liner is just a line feature located at its mid-point

(defun 1-not<0 (val)
  (guard ((>= val 0) "subtracting one from an already negative value"))
  (max (1- val) 0))

(defun subtract-point-1-not<0 (point)
  (make-point
    (1-not<0 (point-h point))
    (1-not<0 (point-v point))))

(defmethod build-vis-locs-for ((lnr liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let ((start-pt (local-to-global lnr (subtract-point-1-not<0 (get-start lnr))))
        (end-pt (local-to-global lnr (subtract-point-1-not<0 (get-end lnr)))))
    (let ((f (car (define-chunks-fct
                    `((isa visual-location
                           color ,(system-color->symbol (color lnr))
                           value line
                           kind line
                           screen-x ,(loc-avg (point-h start-pt) (point-h end-pt))
                           screen-y ,(loc-avg (point-v start-pt) (point-v end-pt))
                           width ,(abs (- (point-h start-pt) (point-h end-pt)))
                           height ,(abs (- (point-v start-pt) (point-v end-pt)))))))))
      (setf (chunk-visual-object f) lnr)
      f)))

(defmethod vis-loc-to-obj ((lnr liner) loc)
  (let ((start-pt (local-to-global lnr (subtract-point-1-not<0 (get-start lnr))))
        (end-pt (local-to-global lnr (subtract-point-1-not<0 (get-end lnr))))
        (v-o (fill-default-vis-obj-slots (car (define-chunks (isa line))) loc)))
    (set-chunk-slot-value-fct v-o 'end1-x (point-h start-pt))
    (set-chunk-slot-value-fct v-o 'end1-y (point-v start-pt))
    (set-chunk-slot-value-fct v-o 'end2-x (point-h end-pt))
    (set-chunk-slot-value-fct v-o 'end2-y (point-v end-pt))
    v-o))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Utilities
;;;; ---------------------------------------------------------------------- ;;;;

;;; XY->POINT      [Function]
;;; Description : Converts an (X Y) list into an MCL/Quickdraw point.

(defun xy->point (xy)
  "(x y) to point conversion function. Deprecated, use vpt2p instead."
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

;;; To guarantee that a keypress by the model gets processed before returning
;;; from the device-handle-keypress method use a semaphore that gets cleared
;;; when the view-key-event-handler method gets called.  Since this is a complete
;;; lock-out (never have two keypresses pending at the same time regardless of
;;; whether they're from different models or not) only need the single semaphore
;;; instead of something fancy like a semaphore per model or window.

(defvar *keypress-wait* (make-semaphore))

(defun wait-n-times (fun n delay-secs)
  (let ((count -1)
        (max-count n))
    (while (and (null (funcall fun))
                (< (incf count) max-count))
      (event-dispatch)
      (awhen delay-secs 
        (spin-for-fct (* 1000 it))))
    (not (= count max-count))))

(defun wait-n-times-on-semaphore (sema n delay-secs)
  (wait-n-times
    (lambda () (timed-wait-on-semaphore sema delay-secs))
    n
    nil))

;;; DEVICE-HANDLE-KEYPRESS      [Method]
;;; Description : Generate a real keypress and then wait for VIEW-KEY-EVENT-HANDLER 
;;;             : to deal with it.
;;;             : To make sure the event is dealt with wait for the semaphore
;;;             : to be set and call event-dispatch periodically (every 50ms)
;;;             : while the semaphore is still clear.

(defmethod device-handle-keypress ((device window) key)
  (window-select device)
  (sv-log-n 1 "starting device-handle-keypress")
  (keypress key nil)
  (unless (wait-n-times-on-semaphore *keypress-wait* 10 .05)
    (print-warning "Model keypress event was not handled correctly within 500ms."))
  (event-dispatch)
  (sv-log-n 1 "ending device-handle-keypress"))

(defvar *mouseclick-wait* (make-semaphore))

;;; DEVICE-HANDLE-CLICK      [Method]
;;; Description : Again, just call the base MCL method and dispatch.

(defmethod device-handle-click ((device window))
  (window-select device)
  (sv-log-n 1 "starting device-handle-click")
  (left-mouse-click
    (local-to-global device (view-mouse-position device))
    nil)
  (unless (wait-n-times-on-semaphore *mouseclick-wait* 10 .05)
    (print-warning "Model mouse click was not handled correctly within 500ms."))
  (event-dispatch)
  (sv-log-n 1 "ending device-handle-click"))

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

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (window-select device)
  (sv-log-n 1 "moving cursor to ~a" xyloc)
  (easygui::running-on-main-thread ()
    (let ((xyloc (local-to-global device (vpt2p xyloc))))
      (#_CGWarpMouseCursorPosition
       (ns:make-ns-point (point-h xyloc) (point-v xyloc)))))
  (unless (wait-n-times (lambda () (vpt= xyloc (p2vpt (view-mouse-position device)))) 10 .05)
    (print-warning "Model cursor movement was not handled correctly within 500ms."))
  (event-dispatch)
  (sv-log-n 1 "ending move cursor"))

;;; DEVICE-SPEAK-STRING      [Method]
;;; Description : If the Mac Speech Manager is installed, actually speak the
;;;             : string.

#+:digitool
(defmethod device-speak-string ((device window) string)
  (when (speech-available-p)
    (speak-string string)
    ))

; Haven't ported MCL's speech code yet.

#+:clozure
(defmethod device-speak-string ((device window) string)
  #-:sv-dev (declare (ignore string))
  nil)


;;; GET-MOUSE-COORDINATES      [Method]
;;; Description : Return the current mouse location in #(x y) format.

(defmethod get-mouse-coordinates ((device window))
  (p2vpt (view-mouse-position device)))

#|
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


|#

#|
(defmethod do-update :after ((mstr-proc master-process) current-time 
                                                        &key (real-wait nil))
  (declare (ignore current-time real-wait))
  (event-dispatch))
|#


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

(defclass rpm-overlay (simple-overlay-view)
  ((offset :accessor offset :initarg :offset :initform nil)))


(defgeneric update-me (olay wind xyloc)
  (:documentation "Call this to move the overlay to a specific location within a window."))

(defmethod update-me ((olay rpm-overlay) (wind window) (xyloc vector))
  (set-view-position olay (add-points (offset olay) (vpt2p xyloc)))
  (unless (equal (view-window olay) wind) (add-subviews wind olay))
  (when (wptr (view-window olay)) (view-draw-contents olay)))

(defmethod update-me ((olay rpm-overlay) (wind window) (xyloc (eql nil)))
  (sv-log "calling update-me with nil xyloc"))

;;; BUILD-FEATURES-FOR      [Method]
;;; Description : We don't want icon features for the focus ring, and since 
;;;             : it'll be a subview a null BUILD-FEATURES-FOR method is 
;;;             : necessary.

(defmethod build-vis-locs-for ((olay rpm-overlay) (vm vision-module))
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
  ((easygui::foreground :reader color :initform *red-color*))
  (:default-initargs 
    :view-size #@(19 19)
    :offset #@(-10 -10)))

(defmethod rpm-overlay-p ((view rpm-overlay))
  t)

(defmethod rpm-overlay-p ((view simple-view))
  nil)

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
  (unless (aand (visual-fixation-marker) (eq (type-of it) 'focus-ring)
                (view-window (visual-fixation-marker)) (eq it wind))
    (setf (visual-fixation-marker) (make-instance 'focus-ring)))
  (when (wptr wind)
    (update-me (visual-fixation-marker) wind xyloc)))

; When called with nil xyloc, remove the current visual-fixation-marker.
; Note that this is called in cases where the current marker is not a subview in wind.
; For instance, at the start of a model run after a previous model has been run,
; the vis location marker might still be non-nil, but there is not a focus ring
; on the current window (window for new model). In this case, just set the vis
; location marker to nil.

(defmethod device-update-attended-loc ((wind window) (xyloc (eql nil)))
  (when (aand (visual-fixation-marker) (eq (type-of it) 'focus-ring)
              (view-window (visual-fixation-marker)) (eq it wind) (wptr wind))
    (remove-subviews wind (visual-fixation-marker)))
  (setf (visual-fixation-marker) nil))

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



; ----------------------------------------------------------------------
; End file: actr6/devices/ccl/device.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: bincarbon/device-patches-mcl.lisp
; ----------------------------------------------------------------------


(defun set-color-of-feats (color feats)
  (dolist (feat feats feats)
    (set-chunk-slot-value-fct feat 'color color)))

(defmethod build-vis-locs-for ((self dialog-item) (vis-mod vision-module))
  (declare (ignore vis-mod))
  (let ((f (car (define-chunks-fct `((isa visual-location
                                 screen-x ,(px (view-loc self))
                                 screen-y ,(py (view-loc self))
                                 kind visual-object
                                 color ,(system-color->symbol (part-color self :text))
                                 value unknown))))))
    (setf (chunk-visual-object f) self)
    f))

(defmethod build-vis-locs-for ((self editable-text-dialog-item) (vis-mod vision-module))
  (let* ((font-spec (view-font self))
         (text (dialog-item-text self))
         (feats 
           (cons
             (car (define-chunks-fct `((isa visual-location
                                            screen-x ,(px (view-loc self))
                                            screen-y ,(py (view-loc self))
                                            kind visual-object
                                            value box
                                            height ,(point-v (view-size self))
                                            width ,(point-h (view-size self))))))
             (unless (equal text "")
               (multiple-value-bind (ascent descent) (font-info font-spec)
                 (set-color-of-feats (system-color->symbol (part-color self :text))
                                     (build-string-feats vis-mod :text text
                                                         :start-x (1+ (point-h (view-position self)))
                                                         :y-pos (+ (point-v (view-position self))
                                                                   descent (round ascent 2))
                                                         :width-fct #'(lambda (str)
                                                                        (string-width str font-spec))
                                                         :height ascent :obj self)))))))
    (dolist (x feats)
      (setf (chunk-visual-object x) self))
    feats))

(defmethod build-vis-locs-for ((self button-dialog-item) (vis-mod vision-module))
  (let* ((btn-width (point-h (view-size self)))
         (btn-height (point-v (view-size self)))
         (text (dialog-item-text self))
         (feats (cons
                  (car (define-chunks-fct `((isa visual-location
                                                 screen-x ,(px (view-loc self))
                                                 screen-y ,(py (view-loc self))
                                                 kind oval
                                                 value oval
                                                 height ,(point-v (view-size self))
                                                 width ,(point-h (view-size self))
                                                 color light-gray))))
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
                        (dolist (item textlines (flatten (nreverse accum)))
                          (push
                            (set-color-of-feats (system-color->symbol (part-color self :text))
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
                                                                    :obj self))
                            accum)
                          (incf start-y (+ ascent descent)))))))))
    (let ((fun (lambda (x y) (declare (ignore x)) (approach-width (car feats) y))))
      (dolist (x (cdr feats))
        (setf (chunk-visual-approach-width-fn x) fun)))
  (dolist (x feats)
    (setf (chunk-visual-object x) self))
  feats))



; ----------------------------------------------------------------------
; End file: bincarbon/device-patches-mcl.lisp
; ----------------------------------------------------------------------
