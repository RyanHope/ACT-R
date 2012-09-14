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
;;; Filename    : chunk-tree.lisp
;;; Version     : 1.0
;;; 
;;; Description : Code to support an Environment tool that displays chunks
;;;               in a tree structure based on the work by Andrea Heiberg,
;;;               Jack Harris, and Jerry Ball as presented in:
;;;
;;;               Heiberg, A., Harris, J. & Ball, J. (2007). Dynamic Visualization of
;;;               ACT-R Declarative Memory Structure. In Proceedings of the 8th
;;;               International Conference on Cognitive Modeling.
;;;
;;;               The display is slightly different than as presented in the paper.
;;; 
;;; Bugs        : [ ] Mildly annoying display issue - if there's only one slot
;;;                   and it's "thinner" than the parent (which is the root)
;;;                   then the line isn't drawn straight down to it.
;;;
;;; To do       : [ ] Add an easy way to get into the sorting of the display.
;;; 
;;; ----- History -----
;;; 2007.07.20 Dan
;;;             : * Initial creation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Put this file into the other-files directory and the corresponding Tcl/Tk
;;; file (35a-declarative-tree.tcl) into the environment/GUI/dialogs directory.
;;; Then this tool will be available through the Control Panel.
;;;
;;; In this implementation slot names are shown in italics in blue at the end of
;;; the line out of the parent chunk.  Chunks are shown in green unless it's a
;;; circular reference in which case it's shown as red, and any other slot value 
;;; is shown in black.  If the chunk is light green then it is a chunk which
;;; exists in the model's DM and clicking on that chunk will bring up a new
;;; regular declarative viewer showing the details of that chunk.
;;;
;;; The save button will create an Encapsulated PostScript image of the tree.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; You can redefine the valid-parse-slot-for-tree function if you want more
;;; control over how the chunks get displayed.
;;; 
;;; It will be passed a chunk-type name and a slot name.  If it returns t then
;;; that slot will be shown if it returns nil it will not.
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


(defstruct (chunk-tree-node (:conc-name ctn-))
  chunk slot width children)

(defun chunk-slot-value-size (val)
  (cond ((symbolp val)
         (length (symbol-name val)))
        ((numberp val)
         (length (format nil "~a" val)))
        ((stringp val)
         (+ 2 (length val))) ; want the quotes
        ((listp val)
         (apply #'+ (mapcar 'chunk-slot-value-size val)))
        (t
         10)))

(defun valid-parse-slot-for-tree (chunk-type slot)
  ;; return t if you want to use the given slot in the chunk-type
  ;; during the graph-trace.
  ;; Some sort of look-up or other table reference needs to be
  ;; built for this.
  ;; default assumes all slots valid.
  
  t)

(defun parse-chunk-tree (chunk slot used &optional (exclude-nil nil))
  (let ((node (make-chunk-tree-node :chunk chunk :slot slot :width (+ 2 (max (chunk-slot-value-size slot) (chunk-slot-value-size chunk))))))
    (when (chunk-p-fct chunk)
      (if (find chunk used)
          (setf (ctn-children node) :circle)
        (let ((children nil))
          (dolist (slot (chunk-type-slot-names-fct (chunk-chunk-type-fct chunk)))
            (when (or (and (not exclude-nil)
                           (valid-parse-slot-for-tree (chunk-chunk-type-fct chunk) slot))
                      (and exclude-nil
                           (chunk-slot-value-fct chunk slot)
                           (valid-parse-slot-for-tree (chunk-chunk-type-fct chunk) slot)))
              (push (parse-chunk-tree (chunk-slot-value-fct chunk slot) slot (cons chunk used) exclude-nil) children)))
          (setf (ctn-children node) (reverse children))
          (setf (ctn-width node) (max (ctn-width node) (apply #'+ (mapcar #'ctn-width children)))))))
    node))


(defun draw-tree (node origin &optional (depth 0) (outer-offset 0) parent)
  (let ((points nil))
    (when (null origin)
      (setf origin (list (round (ctn-width node) 2) depth)))
    (push (list (ctn-chunk node) origin parent (ctn-slot node) (eq :circle (ctn-children node))) points)
    (let ((offset outer-offset)
          (last 0)
          (size (unless (or (eq (ctn-children node) :circle) (null (ctn-children node))) (round (ctn-width node) (length (ctn-children node))))))
      (dolist (x (unless (eq :circle (ctn-children node)) (ctn-children node)))
        (setf points (append (draw-tree x (list (+ offset (round (ctn-width x) 2)) (+ depth 1)) (+ depth 1) offset origin) points))
        (incf offset (ctn-width x))))
    points))


(defun sorted-parse-tree-points (chunk show-nil)
  (sort (draw-tree (parse-chunk-tree chunk nil nil show-nil) nil)
        (lambda (a b) (let ((x1 (first (second a))) (x2 (first (second b))) (y1 (second (second a))) (y2 (second (second b))))
                        (if (< y1 y2)
                            t
                          (if (> y1 y2)
                              nil
                            (< x1 x2)))))))


(defparameter *graph-char-width* 10)
(defparameter *graph-char-height* 15)
(defparameter *graph-line-height* 50)

#| for testing purposes this was used to draw them originally
   and I'm just keeping it around for reference

(defun display-in-exp-window (chunk)
  (close-exp-window)
  (let* ((data (sorted-parse-tree-points chunk))
         (window (open-exp-window (format nil "~A tree" chunk) :width (* *graph-char-width* (* 2 (first (second (first data)))))
                                  :height (* (+ (* 2 *graph-char-height*) *graph-line-height*) (+ 1 (second (second (first (last data)))))))))
    (dolist (x data)
      (when (fourth x)
        (add-text-to-exp-window :text (format nil "~s" (fourth x))
                              :x (* *graph-char-width* (first (second x)))
                              :y (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (second x))))
                              )
      (add-text-to-exp-window :text (format nil "~s" (first x))
                              :x (* *graph-char-width* (first (second x)))
                              :y (+ *graph-char-height* (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (second x))))
                              )
      (when (third x)
        (add-line-to-exp-window (list (* *graph-char-width* (first (second x)))
                                      (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (second x))))
                                (list (* *graph-char-width* (first (third x)))
                                      (+ (* 2 *graph-char-height*) (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (third x)))))
                                'blue)))))

|#

(defun parse-chunk-tree-for-env (chunk show-nil)
  (let* ((data (sorted-parse-tree-points chunk show-nil))
         (list nil)
         (result))
    (push (list 'size  (* *graph-char-width* (* 2 (first (second (first data)))))
                (* (+ (* 2 *graph-char-height*) *graph-line-height*) (+ 1 (second (second (first (last data)))))))
          list)
    
    (dolist (x data)
      (when (fourth x)
        (push (list 'slot (format nil "~s" (fourth x))
                    (* *graph-char-width* (first (second x)))
                    (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (second x)))
                    "#20e")
              list))
      (push (list (if (and (chunk-p-fct (first x)) (no-output (dm-fct (list (first x))))) 'chunk 'text) (format nil "~s" (first x))
                  (* *graph-char-width* (first (second x)))
                  (+ *graph-char-height* (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (second x))))
                  (if (fifth x) "#F00" (if (chunk-p-fct (first x)) (if (no-output (dm-fct (list (first x)))) "#0e0" "#0a0") "#000")))
            list)
      (when (third x)
        (push (list 'line (* *graph-char-width* (first (second x)))
                    (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (second x)))
                    (* *graph-char-width* (first (third x)))
                    (+ (* 2 *graph-char-height*) (* (+ (* 2 *graph-char-height*) *graph-line-height*) (second (third x))))
                    "#20e")
              list)))
    (dolist (x list)
      (push (format nil "~{~S ~}" x) result))
    result))

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