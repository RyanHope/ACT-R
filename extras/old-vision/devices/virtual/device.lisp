;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)2000-2004 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : virtual-view.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : Instantiates "virtual views" so that RPM can act on things
;;;             : other than MCL windows.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 00.06.08 Mike Byrne
;;;             :  Date for new header.
;;; 00.09.13 mdb
;;;             : Updated for vectors.
;;; 00.11.15 mdb
;;;             : Fixed minor compiler warnings, cleaned up some lingering
;;;             : vector bugs.
;;; 01.07.03 mdb
;;;             : Added defgenerics and doc strings.
;;; 02.01.15 Dan
;;;             : Changed the declaration of ignore to
;;;             : ignore-if-unused for the subviews method
;;;             : to eliminate an ugly warning in ACL.  
;;; 02.02.28 Dan
;;;             : changed vv-click-event-handler because
;;;             : functionp doesn't gurantee that the function is
;;;             : returned as the true value.
;;; 02.06.21 Dan [b7]
;;;             : Added the rpm-window class as part of the reorganization
;;;             : of internal window classes.
;;;             : Changed the #+:mcl to better work with openmcl.
;;; 02.06.30 Dan
;;;             : Moved the view based line support in to here.
;;;             : Moved the populate-loc-to-key-array from generic-interface
;;;             : to here.
;;;             : Took the UWI code out of here.
;;; 02.12.19 Dan 
;;;             : Added an around method for text items to handle color.
;;; 04.04.13 Dan [2.2]  (the previous update is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : Changed vw-output to show in the trace and not in the
;;;             :  a window specific stream
;;;             :
;;;             : depends on the vision module and dmi, but that seems
;;;             : reasonable for now.
;;; 2005.05.11 Dan
;;;             : * Added a check of :vwt to vw-output so that it can be
;;;             :   easily shut off (which is actually the default for now).
;;; 2006.01.16 Dan
;;;             : * Discovered that the hash-table implementation of the
;;;             :   virtual-view subviews can lead to non-repeatable performance
;;;             :   of tutorial models (both between different Lisps or even
;;;             :   within a single Lisp!).  So, for now at least, the 
;;;             :   build-features-for method for a virtual-window will sort
;;;             :   the features based on xy coordinates.  That way the tutorial
;;;             :   model results will remain consistent for all systems that
;;;             :   use the virtuals (any hooked up to the environment).
;;; 2006.09.07 Dan
;;;             : * Modified the build-features-for method on the virtual-
;;;             :   windows so that it calls fill-default-dimensions.
;;; 2007.08.23 Dan
;;;             : * Fixed a bug in the build-features-for on buttons to make
;;;             :   sure the text has the same location as the oval.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Base Virtual View class.

(defclass virtual-view ()
  ((view-subviews :accessor view-subviews :initarg :subviews
                  :initform (make-hash-table))
   (x-pos :accessor x-pos :initform nil :initarg :x-pos)
   (y-pos :accessor y-pos :initform nil :initarg :y-pos)
   (width :accessor width :initform nil :initarg :width)
   (height :accessor height :initform nil :initarg :height)
   (id :accessor id :initarg :id :initform (new-name-fct "VV"))
   (handles-click-p :accessor handles-click-p :initform t 
                    :initarg :handles-click-p)
   (view-container :accessor view-container :initform nil)
   (color :accessor color :initarg :color :initform 'black)
   ))

#+(or (not :mcl) :openmcl)
(defgeneric subviews (view &optional subview-type)
  (:documentation "Returns a list of subviews of <view>."))

(defmethod subviews ((vv virtual-view) &optional subview-type)
  (declare (ignore subview-type))
  (let (accum)
    (maphash #'(lambda (x y) (declare (ignore x)) 
                (push y accum)) 
             (view-subviews vv))
    accum))

#+(or (not :mcl) :openmcl)
(defgeneric set-view-position (view x &optional y)
  (:documentation  "Sets the position of <view> to the supplied location."))

(defmethod set-view-position ((vv virtual-view) x &optional y)
  (setf (x-pos vv) x)
  (when y
    (setf (y-pos vv) y)))


#+(or (not :mcl) :openmcl)
(defgeneric set-view-size (view x &optional y)
  (:documentation  "Set the size of <view> to the provided dimensions."))

(defmethod set-view-size ((vv virtual-view) x &optional y)
  (setf (width vv) x)
  (when y
    (setf (height vv) y)))


#+(or (not :mcl) :openmcl)
(defgeneric view-size (view)
  (:documentation  "Return the size of <view> as #(x y)."))

(defmethod view-size ((vv virtual-view))
  (vector (width vv) (height vv)))


#+(or (not :mcl) :openmcl)
(defgeneric view-position (view)
  (:documentation  "Return the top left position of <view> in its container as #(x y)."))

(defmethod view-position ((vv virtual-view))
  (vector (x-pos vv) (y-pos vv)))


(defmethod view-loc ((vv virtual-view))
  (vector (+ (x-pos vv) (/ (width vv) 2))
          (+ (y-pos vv) (/ (height vv) 2))))


#+(or (not :mcl) :openmcl)
(defgeneric add-subviews (view &rest subviews)
  (:documentation  "Add subviews to <view>."))

(defmethod add-subviews ((vv virtual-view) &rest subviews)
  (dolist (sub subviews)
    (setf (gethash (id sub) (view-subviews vv)) sub)
    (setf (view-container sub) vv)))


#+(or (not :mcl) :openmcl)
(defgeneric remove-subviews (view &rest subviews)
  (:documentation  "Remove subviews from <view>."))

(defmethod remove-subviews ((vv virtual-view) &rest subviews)
  (dolist (sub subviews)
    (remhash (id sub) (view-subviews vv))
    (setf (view-container sub) nil)))


(defgeneric point-in-vv-p (vview point)
  (:documentation  "Determine if the supplied point is inside the supplied view."))

(defmethod point-in-vv-p ((vv virtual-view) point)
  (let ((x (the fixnum (px point)))
        (y (the fixnum (py point))))
    (and (>= x (the fixnum (x-pos vv)))
         (>= y (the fixnum (y-pos vv)))
         (<= x (the fixnum (+ (x-pos vv) (width vv))))
         (<= y (the fixnum (+ (y-pos vv) (height vv)))))))


(defgeneric vv-click-event-handler (vview point)
  (:documentation  "Handle a click in <vview> at point <point>."))

(defmethod vv-click-event-handler ((vv virtual-view) point)
  (declare (ignore point))
  nil)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Virtual Window class and methods.

(defclass virtual-window (virtual-view)
  ((window-title :accessor window-title :initarg :window-title
                 :initform "Virtual Window")
   (outstrm :accessor outstrm :initarg :outstrm :initform t)
   (cursor-pos :accessor cursor-pos :initform #(0 0) :initarg :cursor-pos)
   (cursor-shape :accessor cursor-shape :initform 'POINTER 
                 :initarg :cursor-shape)
   )
  (:default-initargs
    :x-pos 0
    :y-pos 0
    :id (new-name-fct "VW")
    ))


(defmethod get-mouse-coordinates ((vw virtual-window))
  (cursor-pos vw))


(defmethod build-features-for ((vw virtual-window) (vis-mod vision-module))
  (let ((base-ls
         (sort (flatten 
                (mapcar #'(lambda (obj) (build-features-for obj vis-mod))
                  (subviews vw)))
               #'loc-sort)))
    (dolist (feat base-ls)
      (fill-default-dimensions feat))
    base-ls))


(defun loc-sort (i1 i2)
  (and (numberp (screen-x i1))
       (numberp (screen-x i2))
       (numberp (screen-y i1))
       (numberp (screen-y i2))
       (or
        (< (screen-x i1) (screen-x i2))
        (and (= (screen-x i1) (screen-x i2))
             (< (screen-y i1) (screen-y i2))))))
       
       


(defmethod cursor-to-feature ((vw virtual-window))
  (when (point-in-vv-p vw (cursor-pos vw))
    (make-instance 'cursor-feature
        :x (px (cursor-pos vw)) :y (py (cursor-pos vw))
        :value (cursor-shape vw))))


(defmethod device-move-cursor-to ((vw virtual-window) (xyloc list))
  (device-move-cursor-to vw (coerce xyloc 'vector))
  )


(defmethod device-move-cursor-to ((vw virtual-window) (loc vector))
  (setf (cursor-pos vw) loc)
  (when (with-cursor-p (current-device-interface))
      (proc-display)))


(defmethod device-handle-keypress ((vw virtual-window) key)
  ;(when (car (no-output (sgp :v)))
   (vw-output vw "got key ~S at time ~S" key (mp-time))
  )




(defmethod device-speak-string ((vw virtual-window) string)
  (vw-output vw "heard speech '~A' at time ~S" string (mp-time))
  )


(defmethod device-handle-click ((vw virtual-window))
  (dolist (sub (subviews vw))
    (when (and (handles-click-p sub)
               (point-in-vv-p sub (cursor-pos vw)))
      (vv-click-event-handler sub (cursor-pos vw))
      (return-from device-handle-click t)))
  ;(when (car (no-output (sgp :v)))
    (vw-output vw "was clicked at time ~S" (mp-time)))



(defgeneric vw-output (vwind base &rest args)
  (:documentation  "Print some output to a virtual window."))

(defmethod vw-output ((vw virtual-window) (base string) &rest args)
  ;; DAN
  ;; seems like this should be in the trace
  ;;(format (outstrm vw)
  
  (when (car (no-output (sgp :vwt)))
    (model-output "~&~%<< Window ~S ~? >>~%"
                  (window-title vw)
                  base args)))



#+(or (not :mcl) :openmcl)
(defgeneric view-window (view)
  (:documentation  "Returns the window associated with <view> (if any), or <view> if <view> is a window."))

(defmethod view-window ((vw virtual-window))
  vw)


#+(or (not :mcl) :openmcl)
(defgeneric window-close (wind)
  (:documentation  "Closes a window."))

;;; there isn't really anything to close in a virtual window, so do nothing
(defmethod window-close ((vw virtual-window))
  nil)

#+(or (not :mcl) :openmcl)
(defgeneric window-select (wind)
  (:documentation  "Brings <wind> to the front."))

;;; there's no "front" in virtual windows, so do nothing.
(defmethod window-select ((vw virtual-window))
  nil)


#+(or (not :mcl) :openmcl)
(defgeneric set-window-title (wind new-title)
  (:documentation  "Set the title of <wind> to <new-title>."))

(defmethod set-window-title ((window virtual-window) (new-title string))
  (setf (window-title window) new-title))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; View classes:  static-text-vv, button-vv, 


(defclass virtual-dialog-item (virtual-view)
  ((text :accessor dialog-item-text :initform "Untitled" 
         :initarg :dialog-item-text)
   (action-function :accessor action-function :initarg :action :initform nil)
   (text-height :accessor text-height :initarg :text-height :initform 10)
   (str-width-fct :accessor str-width-fct :initarg :str-width-fct
                  :initform #'(lambda (str)
                                (* 7 (length str))))
   )
  (:default-initargs
       :height 18
       :width 60
       :subviews nil))


#+(or (not :mcl) :openmcl)
(defgeneric set-dialog-item-text (dialog-item text)
  (:documentation  "Set the text associated with <dialog-item> to <text>."))

(defmethod set-dialog-item-text ((vdi virtual-dialog-item) (text string))
  (setf (dialog-item-text vdi) text))


(defmethod subviews ((vdi virtual-dialog-item) &optional subview-type)
  (declare (ignore-if-unused vdi subview-type))
  nil)


(defmethod view-window ((vdi virtual-dialog-item))
  (let ((vc (view-container vdi)))
    (if (null vc)
      nil
      (if (typep vc 'virtual-window)
        vc
        (view-window vc)))))


(defclass static-text-vdi (virtual-dialog-item)
  ()
  (:default-initargs
       :id (new-name-fct "TEXT-VDI")
       :handles-click-p nil))


(defmethod build-features-for ((self static-text-vdi) 
                                  (vis-mod vision-module))
  (build-string-feats vis-mod :text (dialog-item-text self)
                      :start-x (1+ (x-pos self))
                      :y-pos (py (view-loc self))
                      :width-fct (str-width-fct self)
                      :height (text-height self)
                      :obj self))


(defmethod build-features-for :around ((self static-text-vdi)
                                          (vis-mod vision-module))
  (let ((feats (call-next-method)))
    (mapcar #'(lambda (f)
                (setf (color f) (color self))
                f)
            feats)))


(defclass button-vdi (virtual-dialog-item)
  ()
  (:default-initargs
       :id (new-name-fct "BUTTON-VDI")
       :action #'default-button-action
       ))


(defmethod vv-click-event-handler ((btn button-vdi) where)
  (declare (ignore where))
  (when (functionp (action-function btn))
    (funcall (action-function btn) btn)))


(defmethod default-button-action ((btn button-vdi))
  (format t "~%Button '~S' clicked at time ~S." 
          (dialog-item-text btn) (mp-time)))



(defmethod build-features-for ((self button-vdi) (vis-mod vision-module))
  (cons
   (make-instance 'oval-feature 
     :x (px (view-loc self))
     :y (py (view-loc self))
     :screen-obj self
     :width (width self)
     :height (height self))
   (build-string-feats vis-mod :text (dialog-item-text self)
                       :start-x 
                       (- (px (view-loc self))
                          (round 
                           (funcall (str-width-fct self) (dialog-item-text self))
                           2))
                       :y-pos (py (view-loc self))
                       :width-fct (str-width-fct self)
                       :height (text-height self)
                       :obj self)))




;;; The base class for the view based lines. 

(defclass v-liner (virtual-view)
  ())


(defmethod point-in-click-region-p ((self v-liner) where)
  "Method needed by R/PM so that if the mouse is clicked on the view it
   doesn't get handled by this view, and is passed on to the next"
  (declare (ignore where))
  nil)


(defmethod build-features-for ((lnr v-liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  
  (make-instance 'line-feature
     :color  (color lnr)
     :end1-x (x-pos lnr) 
     :end1-y (y-pos lnr)
     :end2-x (width lnr) 
     :end2-y (height lnr)
     :x (floor (/ (+ (x-pos lnr) (width lnr)) 2))
     :y (floor (/ (+ (y-pos lnr) (height lnr)) 2))
     :width (abs (- (x-pos lnr) (width lnr)))
     :height (abs (- (y-pos lnr) (height lnr)))))


(defmethod feat-to-dmo ((feat line-feature))
  "Convert the line-feature from the visual icon to a DMO (chunk in ACT-R)
   when the feature is attended to"
  (setf (attended-p feat) t)
  (make-dme (dmo-id feat) (kind feat)
            `(screen-pos ,(id (xy-to-dmo (xy-loc feat) t))
              value ,(val feat)
              color ,(color feat)
              height ,(height feat)
              width ,(width feat)
              end1-x ,(end1-x feat)
              end1-y ,(end1-y feat)
              end2-x ,(end2-x feat)
              end2-y ,(end2-y feat))
            :obj (screen-obj feat)
            :where :external))


(defmethod populate-loc-to-key-array ((ar array))
  "Sets all the keys in the array that need to be set"
  ;; function key row
  (setf (aref ar 0 0) 'ESC)
  (setf (aref ar 2 0) 'f1)
  (setf (aref ar 3 0) 'f2)
  (setf (aref ar 4 0) 'f3)
  (setf (aref ar 5 0) 'f4)
  (setf (aref ar 7 0) 'f5)
  (setf (aref ar 8 0) 'f6)
  (setf (aref ar 9 0) 'f7)
  (setf (aref ar 10 0) 'f8)
  (setf (aref ar 12 0) 'f9)
  (setf (aref ar 13 0) 'f10)
  (setf (aref ar 14 0) 'f11)
  (setf (aref ar 15 0) 'f12)
  (setf (aref ar 17 0) 'print-screen)
  (setf (aref ar 18 0) 'scroll-lock)
  (setf (aref ar 19 0) 'pause)
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
  (setf (aref ar 13 2) 'Delete)
  (setf (aref ar 15 2) 'help)
  (setf (aref ar 16 2) 'home)
  (setf (aref ar 17 2) 'pageup)
  (setf (aref ar 19 2) 'ESC)
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
  (setf (aref ar 15 3) 'DEL)
  (setf (aref ar 16 3) 'End)
  (setf (aref ar 17 3) 'Page)
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
  (setf (aref ar 16 5) 'UpArrow)
  (setf (aref ar 19 5) #\1)
  (setf (aref ar 20 5) #\2)
  (setf (aref ar 21 5) #\3)
  (setf (aref ar 22 5) 'enter)
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
  (setf (aref ar 15 6) 'BackArrow)
  (setf (aref ar 16 6) 'DownArrow)
  (setf (aref ar 17 6) 'ForwardArrow)
  (setf (aref ar 19 6) #\0)
  (setf (aref ar 20 6) #\0)
  (setf (aref ar 21 6) #\.)
  (setf (aref ar 22 6) 'enter)
  ar)


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
