;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2014 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : hirschberg-diff-calculation.lisp
;;; Version     : 1.0
;;; 
;;; Description : Compute optimal alignment between two sequences using the
;;;             : Hirschberg algorithm.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [-] Really optimize things by not using subseq and instead
;;;             :     just pass around indexes for the sequences.
;;; 
;;; ----- History -----
;;; 2014.06.05 Dan [1.0]
;;;             : * Initial creation.
;;; 2014.06.06 Dan
;;;             : * Tried an indexes only version, but it didn't seem to be
;;;             :   much different.  I'm guessing that the compilers cheat with
;;;             :   subseq and don't copy for non-modification uses.  So, I've
;;;             :   left it with the subseq calls since it's easier to read.
;;;             : * Coerce the results into string, list, or array based on
;;;             :   the general types of the initial params.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Compute the alignment between two sequences using the Hirschberg algorithm:
;;;
;;;  Hirschberg, D. S. (1975). "A linear space algorithm for computing maximal common 
;;;   subsequences". Communications of the ACM 18 (6): 341-343.
;;; 
;;; Based on the pseudocode from the Wikipedia page:
;;;
;;; <http://en.wikipedia.org/wiki/Hirschberg%27s_algorithm>
;;;
;;; as of 2014.06.05.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; hirschberg (x y &key (strip-ends t) (type 'character) (missing #\-) (key 'identity) 
;;;                      (test 'eql) (ins -2) (del -2) (sub= 2) (sub!= -1) (coerce t)
;;;
;;; Align the sequences x and y and return two values.  Those values are vectors
;;; of the same length.  The first is a vector of elements from x along with 
;;; instances of the 'missing' element and the second is elements from y along 
;;; with instances of the 'missing' element.  The two vectors indicate the 
;;; best alignment of the sequences given the cost values for 'ins'erting
;;; an item, 'del'eting an item, matching an item (sub=), and transforming
;;; two items (sub!=).  It assumes that sub= > sub!= > [ins, del] in creating
;;; the alignment.
;;; 
;;; Type should specify the type of the elements of the sequences x and y.
;;; Missing should be a value of that type to be used for marking the points
;;;   where one sequence has an item the other doesn't.
;;; Key should be a function requiring one parameter.  It will be used to transform
;;;   an element from a sequence into a value to be tested.
;;; Test should be a function requiring two parameters.  It will be passed results
;;;   of the key function applied to elements of the sequences for comparing them
;;;   i.e. this test is performed to determine if an item from position i in 
;;;   sequence x matches the item at position j in sequence y:
;;;    (funcall test (funcall key (elt x i)) (funcall key (elt y j)))
;;; Strip-ends determines whether the beginning and end of the sequences should
;;;   first be tested for equality.  If true then it assumes that the best
;;;   alignment occurs when all the elements at the beginning of the sequences
;;;   which match are aligned and that all of the elements which match at the
;;;   ends of the sequence are aligned.  This can save a lot of time on large
;;;   sequences which have some overlap at either end.
;;; Coerce determines whether or not the resulting sequences are coerced to be
;;;   of the same general types as the respective input values.  If true then
;;;   the results will be coerced into one of string, list, or array based on
;;;   the input values.  If nil then the results will always be vectors.  The
;;;   default value is true.
;;;   
;;; If either x or y isn't a sequence then it returns nil.  It doesn't catch any
;;; other errors in the parameters.
;;;
;;; Here're some example results using the example sequences from the Wikipedia page:
;;; 
;;; > (hirschberg "AGTACGCA" "TATGC")
;;; "AGTACGCA"
;;; "--TATGC-"
;;; > (hirschberg (list #\A #\G #\T #\A #\C #\G #\C #\A) "TATGC")
;;; (#\A #\G #\T #\A #\C #\G #\C #\A)
;;; "--TATGC-"
;;; > (hirschberg "AGTACGCA" "TATGC" :coerce nil)
;;; #(#\A #\G #\T #\A #\C #\G #\C #\A)
;;; #(#\- #\- #\T #\A #\T #\G #\C #\-)
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3) (safety 1) (space 1) (debug 0))))

(defun nwscore (x y &key key test ins del sub= sub!=)
  (let* ((m (length x))
         (n (length y))
         (current)
         (previous (make-array (list (1+ n)) :element-type 'fixnum :initial-element 0)))
    
    (dotimes (j n)
      (setf (aref previous (1+ j)) (+ (aref previous j) ins)))
    
    (dotimes (i m)
      (setf current (make-array (list (1+ n)) :element-type 'fixnum))
      (dotimes (j (1+ n))
        (let ((result (cond ((zerop j)
                             (+ (aref previous 0) del))
                            (t
                             (max
                              (+ (aref previous j) del)
                              (+ (aref current (1- j)) ins)
                              (+ (aref previous (1- j))
                                 (if (funcall test (funcall key (elt x i)) (funcall key (elt y (1- j))))
                                     sub= sub!=)))))))
          (setf (aref current j) result)))
      (setf previous current))
    current))

(defun partitiony (v1 v2)
  (let ((best nil)
        (index nil)
        (len (length v1)))
    (dotimes (i (length v1) index)
      (let ((sum (+ (elt v1 i) (elt v2 (- len i 1)))))
        (when (or (null best) (> sum best))
          (setf best sum
            index i))))))

(defun hirschberg (x y &key (strip-ends t) (type 'character) (missing #\-) (key 'identity) (test 'eql) (ins -2) (del -2) (sub= 2) (sub!= -1) (coerce t))
  (if (and (subtypep (type-of x) 'sequence)
           (subtypep (type-of y) 'sequence))
      (multiple-value-bind (z w)
          (if strip-ends
              (let* ((first-mismatch (do ((i 0 (1+ i)))
                                         ((or (= i (length x)) 
                                              (= i (length y)) 
                                              (not (funcall test (funcall key (elt x i)) (funcall key (elt y i))))) i)))
                     (end-match-count (do ((r (1- (length x)))
                                           (o (1- (length y)))
                                           (i 0 (1+ i)))
                                          ((or (= i (- (length x) first-mismatch)) 
                                               (= i (- (length y) first-mismatch)) 
                                               (not (funcall test (funcall key (elt x (- r i))) (funcall key (elt y (- o i)))))) i))))
                (if (= first-mismatch (length x) (length y))
                    (values x y)
                  (let ((start (subseq x 0 first-mismatch))
                        (end (subseq x (- (length x) end-match-count))))
                    (multiple-value-bind (z w)
                        (hirschberg-rec (subseq x first-mismatch (- (length x) end-match-count))
                                        (subseq y first-mismatch (- (length y) end-match-count))
                                        :missing missing :type type :key key :test test :ins ins :del del :sub= sub= :sub!= sub!=)
                      (values (concatenate 'vector start z end) (concatenate 'vector start w end))))))
            (hirschberg-rec x y :missing missing :type type :key key :test test :ins ins :del del :sub= sub= :sub!= sub!=))
        (if coerce
            (values (coerce z (cond ((stringp x) 'string) ((listp x) 'list) (t 'array)))
                    (coerce w (cond ((stringp y) 'string) ((listp y) 'list) (t 'array))))
          (values z w)))
    nil))
  
  
(defun hirschberg-rec (x y &key type missing key test ins del sub= sub!=)
  (let (z w)
    (cond ((zerop (length x))
           (setf z (make-array (length y) :initial-element missing :element-type type)
             w y)) 
          ((zerop (length y))
           (setf w (make-array (length x) :initial-element missing :element-type type)
             z x)) 
          ((= 1 (length x))
           (let ((pos (position (funcall key (elt x 0)) y :test test :key key)))
             (setf w y
               z (make-array (length y) :initial-element missing :element-type type))
             (if pos
                 (setf (elt z pos) (elt x 0))
               (setf (elt z 0) (elt x 0)))))
          ((= 1 (length y))
           (let ((pos (position (funcall key (elt y 0)) x :test test :key key)))
             (setf z x
               w (make-array (length x) :initial-element missing :element-type type))
             (if pos
                 (setf (elt w pos) (elt y 0))
               (setf (elt w 0) (elt y 0)))))
          (t
           (let* ((mid (floor (length x) 2))
                  (xl (subseq x 0 mid))
                  (xr (subseq x mid))
                  (scorel (nwscore xl y :key key :test test :ins ins :del del :sub= sub= :sub!= sub!=))
                  (scorer (nwscore (reverse xr) (reverse y) :key key :test test :ins ins :del del :sub= sub= :sub!= sub!=))
                  (ymid (partitiony scorel scorer)))
             (multiple-value-bind (zl wl)
                 (hirschberg-rec xl (subseq y 0 ymid) :missing missing :type type :key key :test test :ins ins :del del :sub= sub= :sub!= sub!=)
               (multiple-value-bind (zr wr)
                   (hirschberg-rec xr (subseq y ymid) :missing missing :type type :key key :test test :ins ins :del del :sub= sub= :sub!= sub!=)
                 (setf z (concatenate 'vector zl zr))
                 (setf w (concatenate 'vector wl wr)))))))
    (values z w)))

(provide "HIRSCHBERG")

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
