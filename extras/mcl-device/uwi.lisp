;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : uwi.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : MCL-specific functions to implement the UWI.
;;;             : NOTE: The UWI is only still around to support the 
;;;             :       ACT-R GUI interface. I don't advocate using it directly.      
;;; 
;;; Bugs        : 
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Moved all of the UWI code from mcl-interface
;;;             : to this file where it belongs.
;;;             : Actually documented the code!
;;; 2002.12.19 Dan
;;;             : Modified the window class and make-static-text-...
;;;             : so that it can handle the color attribute.
;;; 04.04.13 Dan [2.2] (previous change is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : changed the name to uwi to be placed in a folder called mcl
;;; 2007.07.13 Dan
;;;             : * Added the color keyword to make-button-for-rpm-window
;;;             :   though it's not actually used at this point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; RPM-REAL-WINDOW  [Class]
;;; Description : This is the UWI's window class to produce an MCL.
;;;             : It inherits from the MCL dialog class (a real window) and
;;;             : the rpm-window class which is an abstract class used by the
;;;             : ACT-R GUI interface.

(defclass rpm-real-window (rpm-window color-dialog)
  ())

;;; VIEW-KEY-EVENT-HANDLER  [Method]
;;; Description : The method called when a key is pressed.  It
;;;             : just calls the rpm-window-key-event-handler which is
;;;             : to be defined by the modeler.

(defmethod view-key-event-handler ((device rpm-real-window) key)
  (rpm-window-key-event-handler device key))

;;; RPM-WINDOW-KEY-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when a key is pressed.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-key-event-handler ((device rpm-real-window) key)
  (declare (ignore device key))
  (call-next-method))

;;; VIEW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The method called when a mouse click occurs.  It
;;;             : just calls the rpm-window-click-event-handler with the
;;;             : mouse position converted to a list. 
;;;             : The rpm-window-click-event-handler is supposed 
;;;             : to be defined by the modeler.

(defmethod view-click-event-handler ((device rpm-real-window) position)
  (rpm-window-click-event-handler device 
                                  (list (point-h position) (point-v position)))
  (call-next-method))

;;; RPM-WINDOW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when the mouse is clicked.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-click-event-handler ((device rpm-real-window) position)
  (declare (ignore device position))
  (call-next-method))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; These are the UWI Methods.
;;;; ---------------------------------------------------------------------- ;;;;

;;; OPEN-RPM-WINDOW?  [Method]
;;; Description : Returns t if the window is open and nil if not.

(defmethod open-rpm-window? ((win rpm-real-window))
  (wptr win))

;;; CLOSE-RPM-WINDOW  [Method]
;;; Description : Closes the window.

(defmethod close-rpm-window  ((win rpm-real-window))
  (window-close win))

;;; SELECT-RPM-WINDOW  [Method]
;;; Description : Brings the specified window to the foreground.

(defmethod select-rpm-window ((win rpm-real-window))
  (window-select win))

;;; ADD-VISUAL-ITEMS-TO-RPM-WINDOW  [Method]
;;; Description : Makes the specified items subviews of the window and
;;;             : calls view-draw-contents and event-dispatch to make sure
;;;             : that they show up.

(defmethod add-visual-items-to-rpm-window ((win rpm-real-window) &rest items )
  (dolist (item items)
    (add-subviews win item))
  (view-draw-contents win)
  (event-dispatch))

;;; REMOVE-VISUAL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Take the specified items out of the subviews of the
;;;             : window and make it redraw.

(defmethod remove-visual-items-from-rpm-window ((win rpm-real-window) 
                                                &rest items)
  (dolist (item items)
    (remove-subviews win item))
   (view-draw-contents win)
  (event-dispatch))

;;; REMOVE-ALL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Remove all the subvies of the window and redisplay it.

(defmethod remove-all-items-from-rpm-window ((win rpm-real-window))
  (apply #'remove-subviews win (subviews win))
   (view-draw-contents win)
  (event-dispatch))

;;; RPM-WINDOW-TITLE  [Method]
;;; Description : Return the title of the window.

(defmethod rpm-window-title ((win rpm-real-window))
  (window-title win))

;;; RPM-WINDOW-VISIBLE-STATUS  [Method]
;;; Description : Return t to indicate that this is a visible window.

(defmethod rpm-window-visible-status ((win rpm-real-window))
  t)

;;; MAKE-RPM-WINDOW  [Function]
;;; Description : Make and return a window based on the parameters supplied.
;;;             : Visible determines wheter or not it should be a real or
;;;             : virtual and if the environment is connected it will use a 
;;;             : visible-virtual for the real window unless the user explicitly
;;;             : specifies the class to use.

(defun make-rpm-window (&key (visible nil) (class nil) (title "RPM Window") 
                             (width 100) (height 100) (x 0 ) (y 0))
  "Make and return a window for use with the UWI"
  (if visible
      (if (and (visible-virtuals-available?) (null class))
          (make-instance 'visible-virtual-window :window-title title 
            :width width :height height :x-pos x :y-pos y)
        (make-instance (if class class 'rpm-real-window) 
          :window-title title :view-size (make-point width height) 
          :view-position (make-point x y)))
    (make-instance (if class class 'rpm-virtual-window) :window-title title 
      :width width :height height :x-pos x :y-pos y)))


;;; MAKE-BUTTON-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a button-dialog-item based on the
;;;             : parameters supplied.

(defmethod make-button-for-rpm-window ((win rpm-real-window) &key (x 0) (y 0) 
                                       (text "Ok") (action nil) (height 18)  
                                       (width 60) (color 'gray))
  (make-dialog-item 'button-dialog-item
                        (make-point x y)
                        (make-point width height)
                        text
                        action
                        :default-button nil))

;;; MAKE-STATIC-TEXT-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a static-text-dialog-item based on the
;;;             : parameters supplied.

(defmethod make-static-text-for-rpm-window ((win rpm-real-window) 
                                            &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black))
  (let ((item (make-dialog-item 'static-text-dialog-item
                        (make-point x y)
                        (make-point width height)
                        text
                        )))
    (set-part-color item :text (color-symbol->system-color color))
    item))


;;; MAKE-LINE-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return the appropriate liner object for the
;;;             : window based on the parameters supplied.

(defmethod make-line-for-rpm-window ((wind rpm-real-window) start-pt end-pt 
                                     &optional (color 'black))
  (let* ((gx (> (first end-pt) (first start-pt)))
         (gy (> (second end-pt) (second start-pt)))
         (vs (make-point (+ 1 (abs (- (first end-pt) (first start-pt))))
                         (+ 1 (abs (- (second end-pt) (second start-pt)))))))
    (cond ((and gx gy)
           (make-instance 'td-liner
             :color (color-symbol->system-color color)
             :view-position (make-point (first start-pt) (second start-pt)) 
             :view-size vs))
          ((and (not gx) (not gy))
           (make-instance 'td-liner
             :color (color-symbol->system-color color)
             :view-position (make-point (first end-pt) (second end-pt)) 
             :view-size vs))
          ((and gx (not gy))
           (make-instance 'bu-liner
             :color (color-symbol->system-color color)
             :view-position (make-point (first start-pt) (second end-pt))
             :view-size vs))
          (t
           (make-instance 'bu-liner
             :color (color-symbol->system-color color)
             :view-position (make-point (first end-pt) (second start-pt))
                                :view-size vs)))))

;;; ALLOW-EVENT-MANAGER  [Method]
;;; Description : Call event-dispatch.  This is used while waiting for
;;;             : a person to respond.

(defmethod allow-event-manager ((win rpm-real-window))
  (event-dispatch))

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
