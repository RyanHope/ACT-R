;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      :Cogworks Laboratory
;;;              Based on mcl/uwi.lisp written by Mike Byrne
;;; Address     : RPI
;;;             : Cognitive Science Department
;;;             : Troy, NY
;;;             : schoem@rpi.edu
;;; 
;;; Copyright   : (c)2005- Cogworks Laboratory/RPI
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : uwi.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : Lispworks-specific functions to implement the UWI.
;;;             : NOTE: The UWI is only still around to support the 
;;;             :       ACT-R GUI interface. I don't advocate using it directly.      
;;; 
;;; Bugs        : 
;;; --- History ---
;;; 2007.09.10  : Dan
;;;             : * Changed allow-event-manager from calling sleep to useing
;;;             :   mp:process-allow-scheduling.
;;; 2007.10.10  : Dan
;;;             : * Applied David Peebles' change for the class of the text
;;;             :   items from capi:display-pane to capi:title-pane to improve
;;;             :   the appearance.
;;; 2007.12.11  : Dan
;;;             : * Fixed an bug in remove-visual-items-from-rpm-window.
;;;             :   Just doing the opposite of what add-... does because the
;;;             :   manipulate-pinboard call was breaking because the items
;;;             :   weren't of the right type.  I don't know if that's a good
;;;             :   fix or not, but it seems to work.
;;; 2008.09.17  : Dan
;;;             : * Updated the adding/deleting and static-text code with
;;;             :   improvements sent from LispWorks.
;;;             : * Added the mp-process-wait call to remove-all-items too.
;;; 2009.02.09  : Dan
;;;             : * Fixed a bug in the make-line-for-rpm-window method
;;;             :   because it wasn't setting the right slot for the color in
;;;             :   all cases.
;;; 2012.08.31  : Dan
;;;             : * Put the call-next-method back in the rpm-window-click-event-handler
;;;             :   so that a user method on the rpm-window class gets called.
;;;             : * Also changed it to passing a vector of the x-y position 
;;;             :   instead of a list so that it works the same as virtuals.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; RPM-REAL-WINDOW  [Class]
;;; Description : This is the UWI's window class.
;;;             : It inherits from the Lispworks interface and
;;;             : the rpm-window class which is an abstract class used by the
;;;             : ACT-R GUI interface.

(defclass rpm-real-window (rpm-window window)  ;;;color-dialog
  ())

;;; VIEW-KEY-EVENT-HANDLER  [Method]
;;; Description : The method called when a key is pressed.  
;;;             : It must be included in the input-model by the modeler
;;;             : It just calls the rpm-window-key-event-handler which is
;;;             : to be defined by the modeler.

(defmethod view-key-event-handler ((self capi:interface)  key)
  (rpm-window-key-event-handler self  key))

;;; RPM-WINDOW-KEY-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when a key is pressed.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-key-event-handler ((device rpm-real-window) key)
  (declare (ignore device key))
  (call-next-method))

;;; VIEW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The method called when a mouse click occurs.
;;;             : It must be included in the input-model by the modeler
;;;             : It just calls the rpm-window-click-event-handler with the
;;;             : mouse position converted to a list. 
;;;             : The rpm-window-click-event-handler is supposed 
;;;             : to be defined by the modeler.

(defmethod view-click-event-handler  ((self capi:pinboard-layout) x y )
  (rpm-window-click-event-handler (capi:element-interface self) (vector x y))
 ; (call-next-method)
  )

;;; RPM-WINDOW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when the mouse is clicked.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-click-event-handler ((device rpm-real-window) position)
  (declare (ignore device position))
  (call-next-method)
  )


;;;; ---------------------------------------------------------------------- ;;;;
;;;; These are the UWI Methods.
;;;; ---------------------------------------------------------------------- ;;;;

;;; OPEN-RPM-WINDOW?  [Method]
;;; Description : Returns t if the window is open and nil if not.

(defmethod open-rpm-window? ((win rpm-real-window))
  (find win (capi:screen-interfaces (capi:convert-to-screen))))

;;; CLOSE-RPM-WINDOW  [Method]
;;; Description : Closes the window.

(defmethod close-rpm-window  ((win rpm-real-window))
 (capi:execute-with-interface win #'capi:destroy win))

;;; SELECT-RPM-WINDOW  [Method]
;;; Description : Brings the specified window to the foreground.

(defmethod select-rpm-window ((win rpm-real-window))
  (capi:activate-pane win))

;;; ADD-VISUAL-ITEMS-TO-RPM-WINDOW  [Method]
;;; Description : Makes the specified items subviews of the window and
;;;             : calls view-draw-contents and event-dispatch to make sure
;;;             : that they show up.

;;; 29 Jul 2008 LW RE-WRITTEN USING PROCESS-WAIT
(defmethod add-visual-items-to-rpm-window ((win rpm-real-window) &rest items )
    (let* ((pinboard (pinboard win)) 
           (done nil))
      (capi:apply-in-pane-process 
       pinboard     
       #'(lambda(pinboard items)
           (setf (capi:layout-description pinboard)
                 (append (capi:layout-description pinboard) items))
           (setq done t))
       pinboard
       items)
      (mp:process-wait "Waiting for visual items"
                        #'(lambda () done))))

;;; REMOVE-VISUAL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Take the specified items out of the subviews of the
;;;             : window and make it redraw.

;;; 29 Jul 2008 LW RE-WRITTEN USING PROCESS-WAIT
(defmethod remove-visual-items-from-rpm-window ((win rpm-real-window) 
                                                &rest items)

  (let* ((pinboard (pinboard win)) 
         (done nil))
    (capi:apply-in-pane-process 
     pinboard     
     #'(lambda(pinboard items)
         (setf (capi:layout-description pinboard)
               (remove-if #'(lambda (x) (member x items))
                              (capi:layout-description pinboard)))
         (setq done t))
     pinboard
     items)
    (mp:process-wait "Waiting for visual items"
                      #'(lambda () done))))


#| Replaced by Dan because the items (x in the lambda) apparently aren't
   of the appropriate class to be passed to manipulate-pinboard.

(let ((pinboard (pinboard win)))
  (dolist (item items)
    (capi:apply-in-pane-process pinboard     
                           #'(lambda(x)
                               (capi:with-geometry x
                                 (capi:manipulate-pinboard pinboard x :delete)))
                           item))))
|#

;;; REMOVE-ALL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Remove all the subvies of the window and redisplay it.

(defmethod remove-all-items-from-rpm-window ((win rpm-real-window))
  (let* ((pinboard (pinboard win))
         (done nil))
    (capi:apply-in-pane-process pinboard     
                                #'(lambda (x)  (setf (capi:layout-description x) nil) (setq done t)) pinboard)
    (mp:process-wait "Waiting for visual items"
                     #'(lambda () done))))

;;; RPM-WINDOW-TITLE  [Method]
;;; Description : Return the title of the window.

(defmethod rpm-window-title ((win rpm-real-window))
  (capi:interface-title win))

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
(defvar *lw-win* nil)
(defun make-rpm-window (&key (visible nil) (class nil) (title "RPM Window") 
                             (width 100) (height 100) (x 0 ) (y 0))
  "Make and return a window for use with the UWI"
  (let ((win
         (if visible
             (if (and (visible-virtuals-available?) (null class))
                 (make-instance 'visible-virtual-window :window-title title 
                                :width width :height height :x-pos x :y-pos y)
               (make-instance (if class class 'rpm-real-window) 
                              :title title :best-x x :best-y y :best-width width :best-height height ))
           (make-instance (if class class 'rpm-virtual-window) :window-title title 
                          :width width :height height :x-pos x :y-pos y))))
    (aif win (if (typep it 'rpm-real-window) (capi:display it)))
    (setq *lw-win* win)))


;;; MAKE-BUTTON-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a button-dialog-item based on the
;;;             : parameters supplied.

(defmethod make-button-for-rpm-window ((win rpm-real-window) &key (x 0) (y 0) 
                                       (text "Ok") (action nil) (height 18)  
                                       (width 60) (color nil) )
  (make-instance 'capi:push-button 
                        :x x :y y
                        :visible-min-width width :visible-min-height height
                        :text text
                        
                        :callback-type :item
                        :callback action
                        ))

;;; MAKE-STATIC-TEXT-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a static-text-dialog-item based on the
;;;             : parameters supplied.
;;; 29 Jul 2008 LW MODIFIED TO USE ITEM-PINBOARD-OBJECT
(defmethod make-static-text-for-rpm-window ((win rpm-real-window) 
                                            &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black))
  (make-instance 'capi:item-pinboard-object
                       :x x :y y
                       :visible-min-width width :visible-min-height height
                       :text text
                       :graphics-args (list :foreground (read-from-string (format nil ":~S" color)))
                        ))


;;; MAKE-LINE-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return the appropriate liner object for the
;;;             : window based on the parameters supplied.

(defmethod make-line-for-rpm-window ((wind rpm-real-window) start-pt end-pt 
                                     &optional (color 'black))
  (flet ((make-point (x y) (vector x y))
         (color-symbol->system-color (color) (read-from-string (format nil ":~S" color))))
  (let* ((gx (> (first end-pt) (first start-pt)))
         (gy (> (second end-pt) (second start-pt)))
         (vs (make-point (+ 1 (abs (- (first end-pt) (first start-pt)))) (+ 1 (abs (- (second end-pt) (second start-pt))))))
         )
    (cond ((and gx gy)
           (make-instance 'td-liner
             :foreground (color-symbol->system-color color)
             :start-x (first start-pt) :start-y (second start-pt)  
             :end-x (first end-pt) :end-y (second end-pt)))
          ((and (not gx) (not gy))
           (make-instance 'td-liner
             :foreground (color-symbol->system-color color)
             :start-x (first end-pt) :start-y (second end-pt)  
             :end-x (first start-pt) :end-y (second start-pt)))
          ((and gx (not gy))
           (make-instance 'bu-liner
             :foreground (color-symbol->system-color color)
             :start-x (first start-pt) :start-y (second end-pt)  
             :end-x (first end-pt) :end-y (second start-pt)
             :view-position (make-point (first start-pt) (second end-pt))
             :view-size vs))  
          (t
           (make-instance 'bu-liner
             :foreground (color-symbol->system-color color)
             :start-x (first end-pt) :start-y (second start-pt)  
             :end-x (first start-pt) :end-y (second end-pt)
              :view-position (make-point (first end-pt) (second start-pt))
              :view-size vs))
             ))))

;;; ALLOW-EVENT-MANAGER  [Method]
;;; Description : Call event-dispatch.  This is used while waiting for
;;;             : a person to respond.

(defmethod allow-event-manager ((win rpm-real-window))
 (mp:process-allow-scheduling))

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
