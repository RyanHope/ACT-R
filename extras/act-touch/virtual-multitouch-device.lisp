;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2012 Cogscent, LLC
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
;;;		: Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
;;;		: USA
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
;;; Filename    : virtual-multitouch-device.lisp
;;; Revision    : 1
;;; 
;;; Description : A virtual multi-touch display device for use with ACT-Touch
;;;
;;; Usage	: Place in ACT-R folder "User Loads." This file will load
;;;		automatically after ACT-R loads.
;;; 
;;; Bugs        : None known
;;;
;;; To do       : Nothing
;;; 
;;; ----- History -----
;;; 2012.09.29 fpt 1
;;;		: Inception: Forked from act-touch.lisp
;;; 2014.12.01 Dan Bothell
;;;             : Commented out the initialize-instance after method for the
;;;             : multitouch-display class since it was specific code to setup
;;;             : and run the example model, and moved the example run code from
;;;             : the end into the comment with the initialize-instance method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ---------------------------------------------------------------------- ;;;;
;;;;   Multitouch Display Device
;;;; ---------------------------------------------------------------------- ;;;;

;;;; A slightly more sophisticated device than a list.
;;;; Let's start with the simplest case: The user is holding the device with the 
;;;; left hand and using the right hand to input. The right hand defaults to being 
;;;; at rest at the right edge of the display, centered vertically, with the index 
;;;; finger hovering an inch above the display surface. There are some multitouch 
;;;; interface widgets on-screen. The user interacts with each of them.





;;;; ---------------------------------------------------------------------- ;;;;
;;;;   The window & trial management stuff
;;;; ---------------------------------------------------------------------- ;;;;


(defclass multitouch-display ()
  ((visual-world :accessor visual-world :initarg :visual-world :initform nil)
;; Distance of the index finger to the display surface, in pixels.
   (index-z :accessor index-z :initarg :index-z :initform 72) 
   (widgets :accessor widgets :initarg :widgets :initform nil)))


#|
This method could be used to setup and run the example model found
in the act-touch-demo-model.lisp file by just using this call:

(progn 
 (reset) 
  (setf *experiment* (make-instance 'multitouch-display)))

However, there is a function in that file which can also be used
without having the after method defined for all instances of the
multitouch-display class. 

; Visual-world & widgets
(defmethod initialize-instance :after ((wind multitouch-display) &key)
   (let* ((vis-locs
          (define-chunks-fct
;; Remember, the display is 1024 x 768 px
              '((screen-x 500 screen-y 350 kind rectangle height 200 width 200 color red))))

          

          (vis-objs
           (define-chunks-fct
               `((height ,(chunk-slot-value-fct (car vis-locs) 'height)
                  width  ,(chunk-slot-value-fct (car vis-locs) 'width)
                  color ,(chunk-slot-value-fct (car vis-locs) 'color)
                  screen-pos ,(car vis-locs)))))

          (wgts (list (make-instance 'test-widget :vwindow wind :vis-loc (car vis-locs) 
                        :vis-obj (car vis-objs) :nick-name :TAP))))
     (setf (visual-world wind) (pairlis vis-locs vis-objs)
           (widgets wind) wgts))
  (set-hand-location left 0 300)
  (set-hand-location right 1024 300)
  (install-device wind)
  (goal-focus goal)
  (proc-display)
;  (start-timing (timer wind))
  (run 20))
|#


  
        


          
;;;; ---------------------------------------------------------------------- ;;;;
;;;;   Multitouch Interface Widgets
;;;; ---------------------------------------------------------------------- ;;;;

(defclass virtual-widget ()
  ((nick-name :accessor nick-name :initarg :nick-name :initform nil)
   (vwindow :accessor vwindow :initarg :vwindow :initform nil)
   (vis-loc :accessor vis-loc :initarg :vis-loc :initform nil)
   (vis-obj :accessor vis-obj :initarg :vis-obj :initform nil)
   (action-type :accessor action-type :initarg :action-type :initform nil)))

(defgeneric vwidget-named (virtual-experiment-window name)
  (:documentation "Vwidget-named returns the first virtual-widget of 
                     virtual-experiment-window whose name is name."))

(defmethod vwidget-named ((vew multitouch-display) name)
  (let ((vw (visual-world vew)))
    (vwn-helper vw name)))

(defun vwn-helper (lst name)
  (if (null lst) nil
    (let* ((wgt (car lst))
           (wnn (nick-name wgt)))
      (if
          (eq name wnn) wgt
        (vwn-helper (cdr lst) name)))))

(defclass test-widget (virtual-widget)
  ()
  (:default-initargs
      :action-type 'test-widget))

(defgeneric current-widget (device finger-loc)
  (:documentation "Given the device and the finger location, return a widget 
  containing the finger location."))

(defmethod current-widget ((device multitouch-display) (finger-loc vector))
  (current-widget-helper (widgets device) finger-loc))

(defun current-widget-helper (widgets finger-loc)
  (cond
   ((null widgets) nil)
   ((inside finger-loc (vis-loc (car widgets))) (car widgets))
   (t (current-widget-helper (cdr widgets) finger-loc))))

(defun inside (loc vl)
  "Takes a display coordinate as a vector and a visual-location chunk name, 
  returns t if the display coordinate 
  is inside the area of the named visual-location chunk."
  (let* ((x1 (chunk-slot-value-fct vl 'screen-x))
         (x2 (+ x1 (chunk-slot-value-fct vl 'width)))
         (y1 (chunk-slot-value-fct vl 'screen-y))
         (y2 (+ y1 (chunk-slot-value-fct vl 'height))))
    (and (>= (svref loc 0) x1)
         (<= (svref loc 0) x2)
         (>= (svref loc 1) y1)
         (<= (svref loc 1) y2))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;   ACT-R Device Handler Methods
;;;; ---------------------------------------------------------------------- ;;;;

(defmethod cursor-to-vis-loc ((device list))
   nil)

(defmethod build-vis-locs-for ((device list) vismod)
  (declare (ignore vismod))
  (mapcar 'car device))

(defmethod build-vis-locs-for ((device multitouch-display) vismod)
  (declare (ignore vismod))
  (mapcar 'car (visual-world device)))

(defmethod vis-loc-to-obj ((device list) vis-loc)
   (cdr (assoc vis-loc device)))

(defmethod vis-loc-to-obj ((device multitouch-display) vis-loc)
   (cdr (assoc vis-loc (visual-world device))))

(defvar *mouse-pos* (vector 0 0))

(defmethod device-move-cursor-to (device loc) 
  (declare (ignore device))
  (model-output "Model moved mouse to ~A" loc)
  (setf *mouse-pos* loc))

(defmethod get-mouse-coordinates (device)
  (declare (ignore device))
  *mouse-pos*)

(defmethod device-handle-click (device)
  (declare (ignore device))
  (model-output "Model clicked the mouse"))

(defmethod device-handle-keypress (device key)
  (declare (ignore device))
  (model-output "Model pressed key ~c" key))

(defgeneric device-handle-tap (device loc &rest params)
  (:documentation "Handle the tap gesture."))

(defmethod device-handle-tap ((device multitouch-display) (loc list) &rest params)
  (model-output "Model tapped ~A at ~A." params loc))

(defmethod device-handle-tap ((device multitouch-display) (loc vector) &rest params)
  (awhen 
   (current-widget device loc) 
   (progn
     (model-output "Model tapped ~A at ~A with ~A." (nick-name it) loc params))))

(defmethod device-handle-tap-hold ((device multitouch-display) &rest params)
  (setf (index-z (current-device)) 0)
  (model-output "Model tap-held ~A." params))

(defmethod device-handle-tap-release ((device multitouch-display) &rest params)
  (setf (index-z (current-device)) 72)
  (model-output "Model tap-released ~A." params))

(defmethod device-handle-tap-drag-release ((device multitouch-display) &rest params)
  (model-output "Model tap-drag-released ~A." params))

(defmethod device-handle-pinch ((device multitouch-display) &rest params)
  (model-output "Model pinched ~A." params))

(defmethod device-handle-rotate ((device multitouch-display) &rest params)
  (model-output "Model rotated ~A." params))

(defgeneric device-handle-swipe (device loc &rest params)
  (:documentation "Handle the swipe gesture."))

(defmethod device-handle-swipe ((device multitouch-display) (loc vector) &rest params)
  (awhen 
   (current-widget device loc) 
   (progn
     (model-output "Model swiped ~A at ~A with ~A." (nick-name it) loc params))))





