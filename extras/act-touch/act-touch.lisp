;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2012-4 Cogscent, LLC
;;; Availability: GNU LGPL, see LGPL.txt
;;; Address     : Cogscent, LLC
;;; 		: PMB 7431
;;;		: 2711 Centerville Rd, Ste 120
;;;		: Wilmington DE, USA 19808-1676
;;;		: frank.tamborello@cogscent.com
;;;
;;; Disclaimer	:     This library is free software; you can redistribute it and/or
;;;		: modify it under the terms of the GNU Lesser General Public
;;;		: License as published by the Free Software Foundation; either
;;;		: version 2.1 of the License, or (at your option) any later version.
;;;		:     This library is distributed in the hope that it will be useful,
;;;		: but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;		: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;		: Lesser General Public License for more details.
;;;		:     You should have received a copy of the GNU Lesser General Public
;;;		: License along with this library; if not, write to the Free Software
;;;		: Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;		: 
;;; 
;;; Acknowledgements
;;;		: This research is sponsored by Measurement Science and 
;;;		Engineering grant 60NANB12D134 from the 
;;;		National Institute of Standards and Technology (NIST).
;;;		Special acknowledgements are due to Dr. Ross Micheals and 
;;;		Dr. Kristen K. Greene of NIST's Information Technology 
;;;		Laboratory.
;;;		Thanks also to Dr. Michael D. Byrne, upon whose experiment 
;;;		library code I based the device code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : act-touch.lisp
;;; Revision    : 13
;;; 
;;; Description : This code extends the ACT-R 6.1 motor module to implement
;;;		several movement styles commonly used with multi-touch handheld 
;;;		computers as well as defines a device with which to perform those 
;;;		movement styles.
;;;
;;; Usage	: Place in ACT-R folder "User Loads." This file will load
;;;		automatically after ACT-R loads.
;;; 
;;; Bugs        : None known
;;;
;;; To do       : 
;;; 
;;;
;;; Issues	:
;;; 2014.10.05 fpt
;;;		: 1. Long-term, it'd probably make more sense to add a z-dimension to hand locations.
;;;		For now movement time methods use "z-index", a slot of the device object.
;;;
;;; 2012.06.14 fpt
;;; 		: 1. What's the target of a swipe? Is it the entire side of the
;;;		device?
;;; 2012.06.14 fpt
;;;		: 2. There's something funny about using Fitts' Law to compute
;;;		pinch execution time. How wide should the target be? That's sort
;;;		of a nonsensical question to ask in the context of a pinch, but
;;;		Fitts' Law requires a width and on the surface the pinch movement
;;;		seems fairly corrected-ballistic like other pointing movements.
;;;		For now let's assume that the thumb is the target-yes, even for
;;;		reverse-pinches-and that the typical human thumb is 1 inch (72px
;;;		@ 72ppi) wide. So the pinch class has a slot for thumb-width
;;;		initializing to 72, which gets passed to the fitts function for 
;;;		calculating pinch execution time.
;;; 2012.08.18 fpt
;;;		: 3. Does it make more sense to construct two-, three-, and 
;;;		four-fingered swipes as different movement styles instead of
;;;		one movement style with a feature specifying the number of 
;;;		fingers?
;;; 2012.08.18 fpt
;;;		: 4. What about qualitatively different swipes used for 
;;;		scrolling precisely starting and ending within one page
;;;		vs page turning?
;;; 
;;; ----- History -----
;;; 2012.06.01 fpt 1
;;;		: Inception: Extend the motor module with a movement style that 
;;;		allows ACT-R to respond to that style's request and produce some
;;;		model output: swipe.
;;; 2012.06.02 fpt 2
;;;		: Implement other movement styles: tap, pinch (also works for 
;;;		reverse-pinch), and rotate.
;;; 2012.06.07 fpt 3
;;;		: Gave tap its real execution time computation, removed the r &
;;;		theta features since it taps in place, and gave it a stub device
;;;		method.
;;; 2012.06.14 fpt 4
;;;		: Defined additional multitouch movement styles with assumed-to-
;;;		be-sensible time computations: peck-tap, peck-tap-recoil, tap-
;;;		hold, tap-release, tap-drag-release, swipe, pinch, rotate.
;;; 2012.06.19 fpt 5
;;;		: The Device is now instrumented to record things like time and
;;;		location of taps. It uses the virtual-experiment-window library,
;;;		adapted by me from Mike Byrne's experiment-window4 library.
;;;
;;; 2012.06.29 fpt 6
;;; Demo task is now a procedure-window rather than a timed-exp-window so that the
;;; scope of one "trial" is one performance of one multi-action task rather than
;;; one performance of one action.
;;;
;;; 2012.07.13 fpt 7
;;; Move-hand-touch allows the model to move its hand to a visual-location or
;;; a visual-object, just like move-cursor. ...except that noisy movement
;;; is not yet implemented.
;;;
;;; 2012.08.18 fpt 8
;;; Wrote a feat-differences method for tap since it didn't inherit it from a parent 
;;; movement-style.
;;;
;;; 2012.08.26 fpt 9
;;; Increased the length of the procedure from 1 to 8 steps & made some of the steps
;;; require other gestures besides tap, namely swipe, rotate, & pinch.
;;;
;;; 2012.09.29 fpt 10
;;; 1. Changed license from public domain to LGPL.
;;; 2. Forked the virtual multitouch display device and 
;;;demonstration model into their own files.
;;;
;;; 2013.04.03 fpt 11
;;; Quickly hacked move-hand-touch so it could generate some noisy movements
;;; probability of a miss considering index finger tip area and target area
;;;
;;; 2013.11.21 fpt 12
;;; 1. Incorporate Melissa Gallagher's dual-distribution noisy movement mechanism,
;;; based on May & Byrne.
;;; 2. Push ':virtual-multitouch-device onto *features* to ease checking for & 
;;; loading
;;;
;;; 2014.10.04 fpt 13
;;; Update to make compatible with ACT-R 6.1:
;;; 1. Move-hand-touch conforms to the new argument list of verify-single-explicit-
;;; value introduced with ACT-R 6.1.
;;;
;;; 2. A new abstraction became available in ACT-R 6.1, handle-style-request,
;;; to schedule the event for a defined movement style. Now all ACT-Touch's
;;; defined movement styles, such as tap, use that instead of their own
;;; request methods.
;;;
;;; 2014.12.01 Dan Bothell
;;; Added an (unless (fboundp 'aif) ...) around the macro definition since it
;;; should already be defined in the main ACT-R code.
;;;
;;; 2015.05.20 Dan Bothell
;;; * Use the device specific coordinate names (even though this has its own
;;; device) and make sure that xy-loc calls pass the vision module in as well.
;;; 
;;; * The approach-width call in move-hand-touch needs to pass the vision module now.
;;;
;;; 2015.06.05 Dan Bothell
;;; * Changed all scheduling calls to use milliseconds instead of seconds to
;;; avoid the scheduler doing the conversion which requires all the safety checks.
;;;
;;; 2015.08.14 Dan Bothell
;;; * Changed rand-time calls to randomize-time since rand-time was depricated
;;;   a while ago and I've finally decided to remove it.
;;; 2015.09.23 Dan Bothell
;;; * Updated the actions which don't use the "default" style mechanisms to
;;;   complete the request for use with the new utility learning credit assignment
;;;   approach.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(unless
    (member ':act-touch *features*)
  (push :act-touch *features*))

;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Macros from "On Lisp"
;;;; ---------------------------------------------------------------------- ;;;;

(unless (fboundp 'aif)
  (defmacro aif (test-form then-form &optional else-form)
    `(let ((it ,test-form))
       (if it ,then-form ,else-form))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;;   ACT-R Motor Module Extension
;;;; ---------------------------------------------------------------------- ;;;;

(defparameter *index-finger-tip-size* #(45 27) ; ~ 5/8" x 1/4"
  "Width and height of the user's index finger tip, in pixels at 72 ppi.")



;;; tap movement style
;;; analogous to punch movement style, the indicated finger strikes the surface 
;;; of the display at the finger's current x, y location and returns the finger 
;;; to a z-position where it is ready to act again

(defStyle tap () hand finger) 

(defmethod compute-exec-time ((mtr-mod motor-module) (self tap))
  (+ (init-time mtr-mod)  
     (max (burst-time mtr-mod)
          (randomize-time (fitts mtr-mod 
;; borrow peck's "b" coefficient, although really should just be 0
                            (peck-fitts-coeff mtr-mod) 
;; get the current device's index-z
                            (index-z (current-device)))))))

(defmethod feat-differences ((t1 tap) (t2 tap))
  (cond ((not (eq (hand t1) (hand t2))) 2)
        ((not (eq (finger t1) (finger t2))) 1)
        (t 0)))

(defmethod queue-output-events ((mtr-mod motor-module) (self tap))
  (schedule-event-relative 
   (seconds->ms (exec-time self))
   'device-handle-tap 
   :time-in-ms t
   :module :motor 
   :output 'high 
   :params (list (current-device)
                 (ecase (hand self)
                   (right (finger-loc (right-hand mtr-mod) (finger self)))
                   (left (finger-loc (left-hand mtr-mod) (finger self))))
                 (hand self) 
                 (finger self))))








;;; tap-hold movement style
;;; Performs a tap movement, but holds the finger to the multitouch display surface.
(defstyle tap-hold tap hand finger)

(defmethod queue-output-events ((mtr-mod motor-module) (self tap-hold))
  (schedule-event-relative 
   (seconds->ms (exec-time self))
   'device-handle-tap-hold
   :time-in-ms t
   :module :motor 
   :output 'high 
   :params (list (device (current-device-interface))
                 (hand self) 
                 (finger self))))



;;; tap-release movement style
;;; If the finger is held against the surface of the multitouch display,
;;; as in a tap-hold movement, then tap-release moves the finger away from
;;; the multitouch display surface.
(defstyle tap-release tap hand finger)

(defmethod queue-output-events ((mtr-mod motor-module) (self tap-release))
  (schedule-event-relative 
   (seconds->ms (exec-time self))
   'device-handle-tap-release
   :time-in-ms t
   :module :motor 
   :output 'high 
   :params (list (device (current-device-interface))
                 (hand self) 
                 (finger self))))







(defgeneric noisy-loc-em? (mtr-mod xy-loc w theta)
  (:documentation "If the Motor Module is set up for it, make the output location noisy."))
  
;;; NOISY-LOC-em?      [Method]
;;; Description : Adds noise to the output location if noise is on.   
;;;             : Rather than adding the same amount of error on both axis, more is on axis than 
;;;             : off axis.  uses weighted-error to determine how much it is
;;;

(defmethod noisy-loc-em? ((mm motor-module) (xy-loc vector) (w number) (theta number))
  (if (not (cursor-noise mm))
    xy-loc
    (if (zerop w)
          xy-loc
        (weighted-error xy-loc w theta))))

;;; WEIGHTED-ERROR      [Method]
;;; Description : samples from the normal distribution with different error on axis and off axis   
;;;             : based on work by may and byrne (see eq 6)
;;;             : Treats the error area as a circle with a diameter of pixw
(defmethod weighted-error ((xy-loc vector) (pixw number) (theta number))
  (model-output "pixw: ~a	theta: ~a~%" pixw theta)
  (let ((on-noise (act-r-noise (*  (/ pixw 4.133) (/ (sqrt 3) pi)))) 
        (off-noise (act-r-noise (* 0.75 (/ pixw 4.133) (/ (sqrt 3) pi)))))
    (polar-move-xy
     xy-loc 
     (vector 
      (sqrt 
       (+ (* on-noise on-noise) (* off-noise off-noise))) 
      (+ theta (atan (/ off-noise on-noise)))))))

;;; move-hand-touch
;;; Allows the model to move its hand to what it sees. 
;;; Adapted from motor.lisp's move-cursor.

(defmethod move-hand-touch ((mtr-mod motor-module) &key loc object request-spec)
  (if (or 
       (check-jam mtr-mod) 
       (check-specs (or loc object)))
      (complete-request request-spec)
    (let* ((r-theta nil)
           (feat nil) 
           (w nil)
           (vision (get-module :vision))
           (coord-slots (vis-loc-slots vision)))
      
      (setf feat  ;; always refer back to the visicon chunks if possible
        (cond ((and object (chunk-visicon-entry object) (chunk-p-fct (gethash (chunk-visicon-entry object) (visicon vision))))
               (gethash (chunk-visicon-entry object) (visicon vision)))
              ((and object (chunk-slot-value-fct object 'screen-pos)
                    (chunk-p-fct (chunk-slot-value-fct object 'screen-pos))
                    (numberp (chunk-slot-value-fct (chunk-slot-value-fct object 'screen-pos) (first coord-slots))) 
                    (numberp (chunk-slot-value-fct (chunk-slot-value-fct object 'screen-pos) (second coord-slots))))
               (if (chunk-p-fct (gethash (chunk-visicon-entry (chunk-slot-value-fct object 'screen-pos)) (visicon vision)))
                    (gethash (chunk-visicon-entry (chunk-slot-value-fct object 'screen-pos)) (visicon vision))
                 (chunk-slot-value-fct object 'screen-pos)))
              ((and loc (chunk-visicon-entry loc) (chunk-p-fct (gethash (chunk-visicon-entry loc) (visicon vision))))
               (gethash (chunk-visicon-entry loc) (visicon vision)))
              ((and loc (chunk-p-fct loc) (numberp (chunk-slot-value-fct loc (first coord-slots))) (numberp (chunk-slot-value-fct loc (second coord-slots))))
               loc)
              (t 
               (complete-request request-spec)
               (print-warning "No valid location could be generated from ~s or ~s when trying to move the mouse." object loc)
               (return-from move-hand-touch nil))))

                 
      (setf r-theta (xy-to-polar (loc (right-hand mtr-mod)) (xy-loc feat vision)))
      (if (= 0 (vr r-theta))        ; r=0 is a no-op 
          (progn
            (complete-request request-spec)
            (model-warning "Move-hand-touch action aborted because hand is at
                     requested target ~S" (if object object loc)))
        (progn
          (setf w (pm-angle-to-pixels (approach-width feat (vtheta r-theta) vision)))
          (let ((r-theta-new (xy-to-polar 
                              (loc (right-hand mtr-mod)) 
                              (noisy-loc-em? mtr-mod (xy-loc feat vision) w (vtheta r-theta)))))

              (prepare-movement 
               mtr-mod
               (make-instance 
                   'hand-ply
                 :request-spec request-spec
                 :hand 'right
                 :r (vr r-theta-new)
                 :theta (vtheta r-theta-new)
                 :target-width w))))))))

(defmethod move-hand-touch-request ((mtr-mod motor-module) chunk-spec)
  (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                  (verify-single-explicit-value 
                     chunk-spec
                     'object
                     :motor
                     'move-hand-touch)
                  nil))
        (location (if (slot-in-chunk-spec-p chunk-spec 'loc)
                    (verify-single-explicit-value 
                     chunk-spec
                     'loc
                     :motor
                     'move-hand-touch)
                    nil)))
    (if (or object location)
      (schedule-event-now
       'move-hand-touch 
       :destination :motor
       :params (list :object object 
                     :loc location
                     :request-spec chunk-spec)
       :module :motor
       :output 'high
       :details (format nil "~a ~a ~a ~a ~a" 'move-hand-touch :object object :loc location))
      (complete-request chunk-spec))))





;;; Index-Thumb
;;; Subclass for pinch, reverse-pinch, and rotate
(defclass index-thumb (hfrt-movement)
  ((move-time :accessor move-time))
  (:default-initargs
      :style-name :index-thumb))

(defmethod compute-exec-time ((mtr-mod motor-module) (self index-thumb))
  (setf (move-time self)
        (max (burst-time mtr-mod)
             (fitts mtr-mod (peck-fitts-coeff mtr-mod) 
                    (ecase (style-name self)
                      (:index-thumb (r self))
                      (:swipe (r self))
                      (:pinch (abs (- (start-width self) (end-width self))))
                      (:rotate (dist 
                               (finger-loc-m mtr-mod 'right 'thumb)
                               (finger-loc-m mtr-mod 'right 'index)))))))
  (+ (init-time mtr-mod)
     (max (burst-time mtr-mod) 
          (randomize-time (move-time self)))))

(defmethod compute-finish-time ((mtr-mod motor-module) (self index-thumb))
  (+ (exec-time self) 
     (burst-time mtr-mod)
     (max (burst-time mtr-mod)
          (randomize-time (move-time self)))))

(defmethod index-thumb ((mtr-mod motor-module) &key hand finger r theta request-spec)
  (if (or (check-jam mtr-mod) (check-specs 'index-thumb hand finger r theta))
      (complete-request request-spec)
    (progn
      (when (symbolp theta)
        (setf theta (symbol-value theta)))
      (prepare-movement mtr-mod (make-instance 'index-thumb :hand hand :finger finger :r r :theta theta :request-spec request-spec)))))

(defStyle pinch index-thumb hand finger start-width end-width)

(defmethod queue-output-events ((mtr-mod motor-module) (self pinch))
  (schedule-event-relative (seconds->ms (exec-time self)) 'device-handle-pinch :module :motor :output 'medium
                           :time-in-ms t :params (list (current-device) (start-width self) (end-width self))))

(defStyle rotate index-thumb hand finger rotation)

(defmethod queue-output-events ((mtr-mod motor-module) (self rotate))
  (schedule-event-relative (seconds->ms (exec-time self)) 'device-handle-rotate :module :motor :output 'medium
                           :time-in-ms t :params (list 
                                                  (device (current-device-interface))
                                                  (rotation self))))

(defStyle swipe index-thumb hand finger r theta num-fngrs)

(defmethod queue-output-events ((mtr-mod motor-module) (self swipe))
  (schedule-event-relative (seconds->ms (exec-time self)) 'device-handle-swipe :module :motor :output 'medium
                           :time-in-ms t :params (list 
                                                  ; (device (current-device-interface))
                                                  (current-device)
                                                  (polar-move-xy (finger-loc-m mtr-mod (hand self) (finger self))
                                                                 (vector (r self) (theta self)))
                                                  (num-fngrs self))))




(extend-manual-requests-fct '((tap (:include motor-command)) (hand right) (finger index)) 'handle-style-request)
; If you need to redefine a movement style, first remove it:
; (remove-manual-request tap)

(extend-manual-requests-fct '((tap-hold (:include motor-command)) (hand right) (finger index)) 'handle-style-request)
; (remove-manual-request tap-hold)

(extend-manual-requests-fct '((tap-release (:include motor-command)) (hand right) (finger index)) 'handle-style-request)
; (remove-manual-request tap-release)

(extend-manual-requests-fct '((move-hand-touch (:include motor-command)) loc object) 'move-hand-touch-request)
; (remove-manual-request move-hand-touch)

(extend-manual-requests-fct '((index-thumb (:include motor-command)) (hand right) (finger index) r theta) 'index-thumb)
; (remove-manual-request swipe)

(extend-manual-requests-fct '((swipe (:include motor-command)) (hand right) (finger index) r theta (num-fngrs 1)) 'handle-style-request)
; (remove-manual-request swipe)

(extend-manual-requests-fct '((pinch (:include motor-command)) (hand right) (finger index) start-width end-width) 'handle-style-request)
; (remove-manual-request pinch)

(extend-manual-requests-fct '((rotate (:include motor-command)) (hand right) (finger index) rotation) 'handle-style-request)
; (remove-manual-request rotate)








