;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2007 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : version-string.lisp
;;; Version     : 1.0
;;; 
;;; Description : Sets a global variable with the current version of the ACT-R
;;;             : sources so that this is all I need to touch to update that.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2007.01.15 Dan
;;;             : * Initial creation.
;;; 2014.07.17 Dan
;;;             : * Split it into a major and minor version now.  Report the
;;;             :   major version with the "ACT-R ..." part of mp-print-versions
;;;             :   and the minor version as the Framework number.
;;; 2015.07.29 Dan
;;;             : * The version strings are now going to be combined to create
;;;             :   the "version" of the software instead of being separate 
;;;             :   and the ".0" is dropped from the major version info.  
;;;             :   When I build a release it will now append the repository #
;;;             :   as an additional .# on the end and add the date in place of 
;;;             :   the <internal> tag which will be after the number e.g. if
;;;             :   I were to build it today it would be .0.1842-<07/29/2015>
;;;             :   instead of ".0-<internal>".
;;; 2015.08.03 Dan
;;;             : * Actually separate out all the pieces of the version string
;;;             :   and then combine them to set *actr-version-string*.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Just sets a string which will be used as the framework version number.
;;; 
;;; Will not be indicating changes in the history section when they are made.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defvar *actr-major-version-string* "7")
(defvar *actr-minor-version-string* "0")
(defvar *actr-repository-number* "1930")
(defvar *actr-release-tag* "2015-12-21")

(defvar *actr-version-string* (format nil "~a.~a~@[.~a~]-<~a>"
                                *actr-major-version-string*
                                *actr-minor-version-string*
                                *actr-repository-number*
                                *actr-release-tag*))

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
