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
;;; Filename    : loader.lisp
;;; Version     : 2.0
;;; 
;;; Description : Mostly system dependent code.
;;;             : Just use a simple load file to start things up.
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;;
;;; 05/30/2002  Dan
;;;             : Added this header and merged the ACL/MCL loaders
;;; 08/15/2002  Dan
;;;             : Added the LispWorks loader.
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Added standalone.lisp to the file list.
;;; 08/15/2003  Dan
;;;             : Updated to version 1.3
;;;             : Added the loader for CMUCL from Ethan Glasser-Camp at RPI.
;;; 4/22/2004   Dan [1.5]
;;;             : Removed the standalone.lisp from the list unless needed.
;;;             : Added the license info.
;;; ------------------------------------------------------------------------
;;; 2005.04.12  Dan [2.0]
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to remove a warning - wrapped the require
;;;             :   for ACL in an eval-when
;;; 2007.01.17 Dan
;;;             : * Updated for use with SBCL (except that most versions of
;;;             :   SBCL don't have threads so the environment isn't available
;;;             :   anyway).
;;; 2010.11.02 Dan
;;;             : * Added a hack to skip this for ABCL since it throws an
;;;             :   error and I haven't created the appropriate uni-file
;;;             :   additions for it yet anyway.
;;; 2012.09.07 Dan
;;;             : * Removed everything using the :ACTR-ENV-ALONE switches and
;;;             :   some other code that was commented out long ago.
;;;             : * Moved the allegro require :sock to uni-files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; First, add an environment indicator to the features list
;;; so that I can test for it in the "experiment library" files
;;; of RPM when I move the UWI Tcl-side. 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :actr-environment *features*))


(require-compiled "UNI-FILES" "ACT-R6:support;uni-files")


#+(and :mcl (not :openmcl))
(defparameter *environment-file-list* '("handler-class.lisp"
                                        "server.lisp"
                                        "env-module.lisp" 
                                        "mcl-fix.lisp"
                                        "handlers.lisp"
                                        "environment-cmds.lisp"
                                        "stepper-control.lisp"
                                        "env-device.lisp"))

#-(and :mcl (not :openmcl))
(defparameter *environment-file-list* '("handler-class.lisp"
                                        "server.lisp"
                                        "env-module.lisp" 
                                        "handlers.lisp"
                                        "environment-cmds.lisp"
                                        "stepper-control.lisp"
                                        "env-device.lisp"))

#+:abcl (setf *environment-file-list* nil)

;;; Finally, just loop over the file list and load them

(dolist (x *environment-file-list*)
  (compile-and-load (translate-logical-pathname (format nil "ACT-R6:environment;~a" x))))

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
