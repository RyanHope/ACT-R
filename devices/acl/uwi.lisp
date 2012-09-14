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
;;; Description : ACL-specific functions to implement the UWI.
;;;             : NOTE: The UWI is only still around to support the 
;;;             :       ACT-R GUI interface. I don't advocate using it directly.      
;;; 
;;; Bugs        : [ ] In the Linux version of the ACL IDE the :on-click method
;;;             :     of a button gets called twice for each press.  The :on-change 
;;;                   method seems to work correctly instead.  However since the
;;;                   do-click, do-keypress, and (setf (cursor-position ...) ...)
;;;                   actions don't work for the Linux version and I've changed
;;;                   the loader to only load this for Windows now, until those
;;;                   actions are available (or there's enough demand to develop
;;;                   some alternative) this isn't really an issue.                   
;;;      
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Moved all of the UWI code from acl-interface
;;;             : to this file where it belongs.
;;; 2002.09.23 Dan
;;;             : fixed a bug in the make-button-for-rpm-window
;;;             : method because it was passing the window and
;;;             : not the button to the "action" function as
;;;             : described in the docs.
;;; 2002.12.23 Dan
;;;             : Added color to the make-static-text-for-rpm-window
;;;             : function.
;;; 04.04.13 Dan [2.2] (previous change is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : changed the name to uwi to go in a folder called acl
;;; 2005.02.17 Dan
;;;             : * Added some markers and use-package to clear up issues
;;;             :   with putting ACT-R in its own package.
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to declae ignored variables.
;;; 2005.09.16 Dan
;;;             : * Minor updates to work with ACL 7.
;;; 2007.07.02 Dan
;;;             : * Changed the build-features-for to build-vis-locs-for.
;;; 2007.07.13 Dan
;;;             : * Changed the make-button-for-rpm-window to add the color
;;;             :   option.
;;; 2011.04.28 Dan
;;;             : * Changed a declare ignore to ignorable to suppress an ACL
;;;             :   warning.
;;; 2012.05.07 Dan
;;;             : * Noted the bug about :on-click under Linux, which isn't an
;;;             :   issue for now since the Linux interface doesn't work for a
;;;             :   model anyway.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cg-user))


;;; RPM-REAL-WINDOW  [Class]
;;; Description : This is the UWI's window class to produce an ACL.
;;;             : It inherits from the ACL dialog class (a real window) and
;;;             : the rpm-window class which is an abstract class used by the
;;;             : ACT-R GUI interface.

(defclass rpm-real-window (rpm-window cg:dialog)
  ())

(defmethod mouse-left-down ((device rpm-real-window) buttons cur-pos)
  (declare (ignore buttons))
  (when (null cur-pos)
      (setf cur-pos (cg:cursor-position device)))
    (rpm-window-click-event-handler device (list (position-x cur-pos)
                                                 (position-y cur-pos))))

(defmethod rpm-window-click-event-handler ((device rpm-real-window) position)
  (declare (ignore position))
  (call-next-method))


(defmethod virtual-key-down :before ((device rpm-real-window) buts key)
   (declare (ignorable device buts key))
  (unless (equal key 16) ;;; if it's the shift key could be problems
    ;;; this is a big hack to get the num-pad 1-9 to return correctly
    ;;; I can't come up with a better way right now, so this is
    ;;; how it's got to be
    (rpm-window-key-event-handler device (case key
                                           (97 #\1)
                                           (98 #\2)
                                           (99 #\3)
                                           (100 #\4)
                                           (101 #\5)
                                           (102 #\6)
                                           (103 #\7)
                                           (104 #\8)
                                           (105 #\9)
                                           (t (code-char key)))))
  t)



(defmethod rpm-window-key-event-handler ((device rpm-real-window) key)
  (declare (ignorable device key))
  (call-next-method))


(defmethod close-rpm-window ((win rpm-real-window))
  (close win))


#+:allegro-V5.0.1 
(defmethod open-rpm-window? ((win rpm-real-window))
  (not (or (equal (type-of win) 'closed-stream) (null win))))

#+:allegro-V5.0.1 
(defmethod open-rpm-window? ((win closed-stream))
  nil)

#+(version>= 6)  
(defmethod open-rpm-window? ((win rpm-real-window))
  (not (or (null (cg:handle win)) (null win))))

;;; Windows 
#-(version>= 6 1) 
(defmethod select-rpm-window ((win rpm-real-window))
  (cg:select-window win))


#+(version>= 6 1) 
(defmethod select-rpm-window ((win rpm-real-window))
  (cg:set-foreground-window win))


;;; Windows

(defmethod add-visual-items-to-rpm-window ((win rpm-real-window) &rest items )
  (setf (cg:dialog-items win) (append (copy-list (cg:dialog-items win)) items)))
   

;;; Windows

(defmethod remove-visual-items-from-rpm-window ((win rpm-real-window) &rest items )
  (dolist (item items)
    (setf (cg:dialog-items win) (remove item (cg:dialog-items win)))))

;;; Windows

(defmethod remove-all-items-from-rpm-window ((win rpm-real-window))
  (let ((key-catcher (cg:find-named-object 'key-catcher win)))
    (setf (cg:dialog-items win) (list key-catcher))))

;;; windows
 
(defmethod make-button-for-rpm-window ((win rpm-real-window) &key (x 0) (y 0) (text "Ok")  (action nil) (height 18)  (width 60) (color 'black))
    (make-instance 'cg:button
            :left x
            :width width
            :height height
            :on-click #'(lambda (window button) (declare (ignore window)) (when action (funcall action button)))
            :title text
      :top y
      :foreground-color (color-symbol->system-color color)))

;;; Windows

(defmethod make-static-text-for-rpm-window ((win rpm-real-window) &key (x 0) (y 0) (text "") (height 20) (width 20) (color 'black))
  (make-instance 'cg:static-text
    :left x
    :width width
    :height height
    :value text
    :top y
    :foreground-color (color-symbol->system-color color)
    :on-click #'(lambda (x y) (declare (ignore x y)) (mouse-left-down win nil nil))))


;;; It turns out that due to the bottom up nature of
;;; the event handling in ACL that a key press on a window
;;; only goes to the dialog items, it doesn't get passed
;;; back up the chain.  Also, certain types of dialog
;;; items (static-text) do not have a virtual-key-down method, 
;;; so they will not respond to a key press and thus it can't be 
;;; passed "back up the chain".  This causes problems because
;;; R/PM was designed around MCL where the events go top down
;;; i.e. the window gets first crack at handling them. 
;;; This hack will allow certain experiments to be written
;;; for the UWI, but unfortunatly is not a general solution.
;;; As long as no dialog-item in the rpm-window is selected
;;; as the focus this should allow key presses to be passed
;;; up to the window for handling by the rpm-window-key-event-handler.
;;;

#+(version>= 6 1) (defclass key-catcher (cg:group-box)
  ()
  (:default-initargs
   :name 'key-catcher
   :width 5
    :height 5
    :left -50
    :top 0))

#-(version>= 6 1) (defclass key-catcher (cg:lisp-group-box)
  ()
  (:default-initargs
   :name 'key-catcher
   :width 5
    :height 5
    :left -50
    :top 0))

(defmethod build-vis-locs-for ((self key-catcher)
                                  (vis-mod vision-module))
  nil)

#+(version>= 6 1) (defmethod virtual-key-down :before ((catcher cg::group-box-pane) buttons key-code)
  (when (subtypep (type-of (cg:parent catcher)) 'rpm-window)
    (virtual-key-down (cg:parent catcher) buttons key-code)))

#-(version>= 6 1) (defmethod virtual-key-down :before ((catcher cg::lisp-group-box-pane) buttons key-code)
  (when (subtypep (type-of (cg:parent catcher)) 'rpm-window)
    (virtual-key-down (cg:parent catcher) buttons key-code)))


(defun make-rpm-window (&key (visible nil) (class nil) (title "RPM Window") (width 100) (height 100) (x 0 ) (y 0))
  (if visible
      (if (and (visible-virtuals-available?) (null class))
          (make-instance 'visible-virtual-window :window-title title :width width :height height :x-pos x :y-pos y)
        (let ((win (cg:make-window (new-name-fct "rpm-window") :device (if class class 'rpm-real-window) :title title :width width :height height :left x :top y))
            (catcher (make-instance 'key-catcher)
                     )
            )
        ;(setf (window catcher) catch-pane)
        (setf (cg:dialog-items win) (list catcher))
        win))
      (make-instance (if class class 'rpm-virtual-window) :window-title title :width width :height height :x-pos x :y-pos y)))


(defmethod rpm-window-title ((win rpm-real-window))
  (cg:title win))

(defmethod rpm-window-visible-status ((win rpm-real-window))
  t)

(defmethod allow-event-manager ((win rpm-real-window))
  (let ((interior 
         
         #-(version>= 7)
         (window-interior win)
         #+(version>= 7)
         (cg:interior win)
                  
                  
                  ))
    (cg:erase-contents-box win (cg:make-box 0 0 (cg:box-width interior) 
                                      (cg:box-height interior))))
  (dolist (x (cg:dialog-items win))
    (when (subtypep (type-of x) 'drawable)
      (when (on-redisplay x)
        (funcall (on-redisplay x) x win))))
  (cg:process-pending-events))

(defmethod make-line-for-rpm-window ((wind rpm-real-window) start-pt end-pt &optional (color 'black))
  "returns a view for the specified window which will draw a line from the start-pt to the end-pt
   using the optional color specified (defaulting to black).  The start and end points are 
   specified in list form (x y) for uniformity among systems."
  (make-instance 'liner
    :color (color-symbol->system-color color)
    :start-pt start-pt 
    :end-pt end-pt))

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
