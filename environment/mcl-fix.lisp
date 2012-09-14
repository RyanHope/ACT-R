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
;;; Filename    : mcl-fix.lisp
;;; Version     : 2.0
;;; 
;;; Description : Purely system dependent code.
;;;             : Patch to make socket connections in MCL not block
;;;             : on any access - by default a read blocks a write
;;;             : and vice versa.
;;;             : Also a crude hack to convert unix style pathnames
;;;             : to MCL style for use with MCL 5 under OSX.
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----

;;; 4/22/2004   Dan [1.5]
;;;             : Added all the header info.
;;; --------------------------------------------------------------------------
;;; 2005.04.12  Dan [2.0]
;;;             : * Moving into ACT-R 6. 
;;;             : * No actual changes.
;;; 2005.04.20  Dan
;;;             : * Moved the requre OpenTransport to uni-files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(in-package :cl-user)

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

#+:ccl-5.0 
(defun create-valid-pathname (path)
 (if (and (osx-p) (eql (aref path 0) #\/))
    (let ((root (find-if  #'(lambda (x) (subtypep x 'string)) 
                      (pathname-directory (ccl::findfolder #$kOnSystemDisk #$kSystemFolderType)) 
                      :key #'type-of)))
       (if root
          (concatenate 'string root (substitute #\: #\/ path))
        (error (format nil "Unable to build a pathname from ~S~%" path))))
   path))


(in-package :ccl)

(defmethod stream-tyi ((stream  opentransport-stream))
  (unblocked-io-buffer-tyi (stream-io-buffer stream) t))

(defun unblocked-io-buffer-tyi (io-buffer &optional (dont-type-check))
  (unless dont-type-check
    (unless (typep io-buffer 'io-buffer)
      (setq io-buffer (require-type io-buffer 'io-buffer))))
  (locally (declare (optimize (speed 3) (safety 0)))
    (if (io-buffer-untyi-char io-buffer)
        (prog1 (io-buffer-untyi-char io-buffer)
          (setf (io-buffer-untyi-char io-buffer) nil
            (io-buffer-bytes-read io-buffer) 
            (the fixnum (1+ (the fixnum (io-buffer-bytes-read io-buffer))))))
      (let ((byte (%io-buffer-read-byte io-buffer nil)))
        (and byte (%code-char byte))))))


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