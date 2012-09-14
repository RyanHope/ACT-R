;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : handler-class.lisp
;;; Version     : 1.0
;;; 
;;; Description : No system dependent code.
;;;             : Defines the class for the environment "handlers" only.
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;;
;;; 2011.05.24 Dan [1.0]
;;;             : * Split the class definition from the methods to avoid
;;;             :   warnings about definitions without having to declaim
;;;             :   all the accessors elsewhere.
;;; 2011.05.25 Dan
;;;             : * Added the use-model slot since some actions need to be
;;;             :   performed without specifying a model (including the current
;;;             :   one).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; class: environment-handler
;;; This class is for communication between objects on the Tcl
;;; side and here in Lisp.  Every object that exists on the Tcl
;;; side and needs to get data from Lisp will have a corresponding
;;; instance of some subclass of this class.  
;;; The slots are:
;;; name - a unique name for each instance used by Tcl for reference
;;;        and as the key into a hashtable.  When the handler is
;;;        removed the name is uninterned.
;;; object-name, target-name and update-form are the corresponding
;;;    elements from the Tcl -> Lisp create message and update messages 
;;;   (see messages.txt for full details)
;;; update-type is one of the valid update-types from the Lisp -> Tcl
;;;   update message (again see messages.txt for full details)
;;; update-value is set every time an update is sent to Tcl with the
;;;   value piece of the Lisp -> Tcl update.
;;; socket is the TCP socket on which this handler is reading and writing.

(defclass environment-handler ()
  ((name :accessor name :initform (gentemp "HANDLER"))
   (use-model :accessor use-model :initarg :use-model)
   (model :accessor handler-model :initarg :model)
   (object-name :accessor obj-name :initarg :object-name)
   (target-name :accessor target-name :initarg :target-name)
   (update-form :accessor update-form :initarg :update-form)
   (update-type :accessor update-type :initarg :update-type)
   (update-value :accessor update-value)
   (socket :accessor socket :initarg :socket)))

;;; a simple handler responds with simple updates

(defclass simple-handler (environment-handler)
  ()
  (:default-initargs
    :update-type 'simple))

;;; a list handler also responds with simple updates
;;; but has to form them slightly differently

(defclass list-handler (simple-handler)
  ())

;;; a simple-funcall handler responds with simple_funcall updates
;;; otherwise works like a simple handler

(defclass simple-funcall-handler (simple-handler)
  ()
  (:default-initargs
    :update-type 'simple_funcall))


;;; a text-handler is a simple handler which draws
;;; it's string return value into a text box erasing
;;; what's already there.

(defclass text-handler (simple-handler)
  ()
  (:default-initargs
    :update-type 'text))

;;; a simple-text handler works just like a simple handler
;;; except that on the other side it gets handled a little
;;; differently

(defclass simple-text-handler (simple-handler)
  ()
  (:default-initargs
    :update-type 'simpletext))


;;; a window handler passes a list of commands but gets
;;; treated special on the other side

(defclass env-window-handler (list-handler)
  ()
  (:default-initargs
      :update-type 'env_window))

;;; a listbox needs a special list handler

(defclass list-box-handler (list-handler)
  ()
  (:default-initargs
      :update-type 'list_box))

;;; and there's a special handler for listboxes that will
;;; always result in the first item being selected on an
;;; update

(defclass select-first-list-box-handler (list-handler)
  ()
  (:default-initargs
      :update-type 'select_first_list_box))


;;; an output handler gets it's values by capturing *standard-output*
;;; and *command-trace*

(defclass output-handler (environment-handler)
  ())

;;; a simple output handler just sends that output 

(defclass simple-output-handler (output-handler)
  ()
  (:default-initargs
      :update-type 'simple))

;;; a 'special' simple output handler doesn't
;;; "undo" the ENPTY_ENV_STRING on the Tcl side

(defclass special-simple-output-handler (simple-output-handler)
  ()
  (:default-initargs
    :update-type 'special))


;;; a text handler means that the output becomes the entire contents
;;; of a text box, so it does some extra work on the Tcl side

(defclass text-output-handler (output-handler)
  ()
  (:default-initargs
    :update-type 'text))


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
