;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008-2011 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : buffer-history.lisp
;;; Version     : 1.1
;;; 
;;; Description : Code to support the buffer history tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider converting the internal time over to milliseconds
;;;             :     by using buffer-record-ms-time instead of -time-stamp.
;;;             :     The down side of that is the display for the environment
;;;             :     should probably still be in seconds.
;;; 
;;; ----- History -----
;;; 2008.08.20 Dan
;;;             : * Initial creation.
;;; 2011.04.25 Dan
;;;             : * Added the "to do" to consider.
;;; 2011.06.20 Dan [1.1]
;;;             : * Changed the tool to record all the details everytime, not 
;;;             :   just when there is a "change" to the buffer because some
;;;             :   changes are below what the buffer trace detects i.e. custom
;;;             :   queries for modules.
;;; 2011.06.24 Dan
;;;             : * Changed the output so that the query info is first so that
;;;             :   it's easier to watch that for a change in a buffer since it
;;;             :   will be formatted consistently.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Open the buffer history window before running the model or set the
;;; :save-buffer-history parameter to t in the model to enable the recording.
;;; 
;;; Once the model has run click the "Get history" button in the buffer history 
;;; window.  Only those buffers specified with the :traced-buffers parameter
;;; of the buffer-trace module will have thier history recorded.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; :save-buffer-history parameter
;;;  Enables the recording of retrieval history for display (default is nil).
;;;  This will also set the :save-buffer-trace parameter to t if it is not
;;;  already set.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct buffer-history-module
  (table (make-hash-table))
  enabled)
  

(defstruct buffer-history
  time
  chunk  ; a string, :cleared, or nil
  status ; a string
  )


(defun equal-history-samples (h1 h2)
  (and h1 h2
       (or (eq (buffer-history-chunk h1) (buffer-history-chunk h2))
           (and (stringp h1) (stringp h2) (string-equal (buffer-history-chunk h1) (buffer-history-chunk h2))))
       (string-equal (buffer-history-status h1) (buffer-history-status h2))))


(defun buffer-history-recorder (summaries)
  (let ((history (get-module buffer-history))
        (time (buffer-record-time-stamp summaries)))
      
    (when (and history (buffer-history-module-enabled history))
      (dolist (summary (buffer-record-buffers summaries))
        (let* ((name (buffer-summary-name summary))
               (record (make-buffer-history :time time
                                            :chunk (cond ((or (buffer-summary-modified summary)
                                                              (buffer-summary-chunk-name summary))
                                                          (capture-model-output (buffer-chunk-fct (list name))))
                                                         ((buffer-summary-cleared summary)
                                                          :cleared)
                                                         (t nil))
                                            :status (capture-model-output (buffer-status-fct (list name))))))
          (unless (equal-history-samples record (car (gethash name (buffer-history-module-table history))))
            (push record (gethash name (buffer-history-module-table history)))))))))

               
(defun buffer-history-buffer-list ()
  (let ((history (get-module buffer-history)))
    (hash-table-keys (buffer-history-module-table history))))

(defun buffer-history-text (time buffer)
  (if (and time buffer)
      (let ((history (get-module buffer-history)))
        (when history 
          (let* ((record (find-if (lambda (x) 
                                    (<= (buffer-history-time x) time))
                                  (gethash buffer (buffer-history-module-table history))))
                 
                 (chunk (when record (buffer-history-chunk record)))
                 (status (when record (buffer-history-status record))))
            
            (concatenate 'string (cond ((and status (stringp status))
                     status)
                                       (t "No buffer status information available"))
              
              (string #\newline)
              (cond ((and chunk (stringp chunk)) 
                                        chunk)
                                       (t (format nil "buffer empty~%")))))))
    ""))

(defun buffer-history-time-list ()
  (let ((history (get-module buffer-history)))
    (when history
      (let ((times nil))
        (maphash (lambda (key value)
                   (declare (ignore key))
                   (setf times (append (mapcar 'buffer-history-time value) times)))
                 (buffer-history-module-table history))
        (sort (remove-duplicates times) #'<)))))


(defun reset-buffer-history-module (module)
  (clrhash (buffer-history-module-table module)))

  
(defun params-buffer-history-module (instance param)
  (if (consp param)
      (case (car param)
        (:save-buffer-history 
          (no-output
           (progn
             (if (cdr param)
                 (progn
                   (sgp :save-buffer-trace t)
                   (unless (find 'buffer-history-recorder (car (sgp :buffer-trace-hook)))
                     (sgp :buffer-trace-hook buffer-history-recorder)))
               
               (progn
                 (when (find 'buffer-history-recorder (car (sgp :buffer-trace-hook)))
                   (let ((old-hooks (car (sgp :buffer-trace-hook))))
                     (sgp :buffer-trace-hook nil)
                     (dolist (x old-hooks)
                       (unless (eq x 'buffer-history-recorder)
                         (sgp-fct (list :buffer-trace-hook x))))))))
          
             (setf (buffer-history-module-enabled instance) (cdr param))))))
    (case param
      (:save-buffer-history (buffer-history-module-enabled instance)))))

(define-module-fct 'buffer-history nil 
  (list (define-parameter :save-buffer-history :valid-test 'tornil :default-value nil  
          :warning "T or nil" 
          :documentation "Whether or not to record the history of buffer changes."))
  :creation (lambda (x) (declare (ignore x)) (make-buffer-history-module))
  :reset 'reset-buffer-history-module
  :params 'params-buffer-history-module
  :version "1.0"
  :documentation "Module to record buffer change history for display in the environment.")
  

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
