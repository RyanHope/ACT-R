;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Clayton Stanley 
;;; Copyright   : (c)2003-7 CMU/Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;;             : clayton.stanley@rice.edu 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : ccl-simple-view.lisp
;;; Version     : 1.0 
;;; 
;;; Description : Provides an interface for CCL that allows the implementation to
;;;               read in GUI source code written for Macintosh Common Lisp. 
;;;               This enables task environments written in MCL (e.g., Phaser, 
;;;               Votebox, NextGen from Mike's lab) to work with CCL with 
;;;               minimal code modifications
;;;
;;;		  This file is a concatenation of all bootstrap and feature code
;;;		  so that the file can be loaded from a base/standard CCL core file.
;;;		  
;;;		  The file was built on Tue Nov 25 14:45:28 CST 2014 using GNU Make and Bash.
;;;               Editing was done with vim+slimv, and lisp code was auto indented using
;;;               vim+slimv's auto-indention algorithm.
;;;
;;;		  Git commit hash associated with build: 5875c091ec81f54501cf14f844d7c32a9e4a3e01
;;;
;;;		
;;;
;;; Bugs        : []
;;; 
;;; Todo        : Lots
;;;             : 
;;; 
;;; ----- History -----
;;; 2012.06.29 cts 
;;;             : Initial build that allows Votebox MCL GUI to be read in  and used by CCL
;;; 2012.08.13 cts
;;;             : Release build that works with Votebox, NextGen, and Phaser from Mike's lab
;;; 2013.02.04 cts
;;;             : Fixed bug that did not update window position after window moved with
;;;               mouse drag. Window position is correctly tracked, and a call to
;;;               view-position on the window works correctly both before and after
;;;               the window position is moved with a mouse drag on the window pane.
;;; 2013.02.11 cts
;;;             : Removed easygui::drawing-view dependency in codebase.
;;;               Simplifies OO hierarchy, removes unnecessary cruft, and decreases technical debt
;;; 2013.02.16 cts
;;;             : Changes to ensure code is compatible with CCL 1.8 through CCL 1.9rc2
;;; 2013.04.10 cts
;;;             : Added feature: You can now change the color of the text shown for button objects
;;;               (button-dialog-item, check-box-dialog-item, radio-button-dialog-item) in the usual
;;;               MCL way for dialog items (passing :fore-color #color# or :part-color (list :text #color#)
;;;               to initialize-instance)
;;; 2013.04.20 cts
;;;             : #@ read macro no longer clobbers CCL's original version that created NSString objects.
;;;               The #@ read macro now creates MCL points when provided with a list, and an NSString object
;;;               when provided with a string.
;;;             : Reordered loading a few subcomponent files and renamed a few subcomponent files.
;;;             : Removed stray commented out code that is no longer necessary.
;;;             : Now spell checking comments and strings in the code.
;;; 2013.05.13 cts
;;;             : Added implementation for sequence-dialog-item.
;;;             : Added quickdraw functions for paint-polygon and fill-polygon.
;;;             : Removed extra cruft from file: no longer including thermometer.lisp since
;;;               this library code is not part of the core MCL GUI interface.
;;;             : Also removed unused code (all of the pandoric functions) from lol-subset.lisp
;;;             : Added MCL's :centered keyword for windows.
;;;             : Fixed quickdraw polygon functions to use the polygon provided as input to the function, 
;;;               instead of using the polygon stored within the view. This change matches the MCL spec.
;;; 2013.06.02 cts
;;;             : Added MCL's :top and :bottom list arguments to :view-position for windows
;;;             : Enabled :dialog-item-action initarg from MCL spec to work, and map correctly to 
;;;               :action initarg for CCL spec.
;;;             : Cleaned up :parse-mcl-initarg methods. Created a proper generic method signature for the
;;;               methods, so that they have the needed flexibility to work in all cases.
;;;             : Plugged memory leaks by creating an autorelease pool at the beginning of this file.
;;;               This ensures that any autoreleased objects during both compilation and runtime have 
;;;               a pool to release to. This was only an issue when running CCL via SVN (e.g., with common SLIME setup),
;;;               and not via the Clozure CL.app, because the App already has an autorelease pool set up on startup.
;;; 2013.07.31 cts
;;;             : Incorporated additional functionality needed for a recent project into the ccl-simple-view library:
;;;               -Sequence dialog item now defaults to nil sequence if none specified on init
;;;               -menu-view and menu-item objects now set font correctly
;;;             : Refactored and simplified image-view code 
;;;             : Everything works on 10.6.8 - 10.8.4 (newest Mountain Lion install as of testing date)
;;;               -required a fix for radio buttons. Clustering did not work on 10.8 prior to fix
;;;             : Code-coverage report is now generated for each build. 
;;;               -Used report to remove stray code and discover/fix a few minor bugs
;;; 2013.09.17 cts
;;;             : Swapped cocoa class for editable-text-dialog-item from NSTextField to NSTextView
;;;               -Ensures that keypresses on the view that are relayed to view-key-event-handler are accurate
;;;               -Allows for more control over keypresses in the view, so that for example, the next responder
;;;                can be called when a #\tab is pressed in the view.
;;;             : Reworked #\tab presses for UI, so that tabbing moves between all of the views on the window
;;;               that can become first responder.
;;;             : Reworked #\space presses for buttons, so that if a #\space is pressed when a button is
;;;               first responder, then the action for that button fires (i.e., the button is clicked).
;;;             : Ensured that the window is always the starting first responder when the view is created.
;;;               Afterwards, if the user presses #\tab, then the next responder is activated, and tabbing
;;;               then cycles through all available first responders. This is to ensure that no particular view
;;;               is the first responder in the simplest case: for a basic window where the user does not
;;;               wish to tab between views.
;;;             : If tabbing across buttons is enabled at the OS level:
;;;               http://superuser.com/questions/473143/how-to-tab-between-buttons-on-an-mac-os-x-dialog-box
;;;               then a user can work through most tasks involving editing text views and pressing buttons
;;;               using only the keyboard. #\tab to navigate and #\space to select
;;; 2014.02.08 cts
;;;             : Implemented :close-box-p initarg for windows, so that the red close button does not display
;;;               when this is set to t when initializing the window
;;;             : Fixed a bug where incorrect mouse locations were passed to view-click-event-handler for views.
;;;               All locations where correct for view-click-event-handlers defined on windows, and for 
;;;               rpm-window-click-event-handlers (since those are defined on the window).
;;;               So this does not apply to act-r models using the act-r device interface (i.e., rpm-window-click-event-handler).
;;;               This only applied to view-click-event-handlers defined on views (e.g., buttons).
;;;               MCL spec sets location for view-click-event-handler to the mouse click location relative to that 
;;;               view's local coordinate system, not relative to the window's coordinate system. So if a button
;;;               responds to a mouse click that is 10 by 10 pixels within that button's view rectangle, then
;;;               the location for view-click-event-handler will by 10 by 10, and not 10 by 10 plus that view's
;;;               position within the window for example.
;;;               The prior code e.g., passed 10 by 10 plus the view's position to view-click-event-handler, regardless of view type.
;;; 2014.02.10 Dan Bothell
;;;             : Swapped the order of grey and gray in rgb-list because ACT-R
;;;               uses gray as the color name for things and this: 
;;;               (system-color->symbol (color-symbol->system-color 'gray))
;;;               should return gray.  [Since those are basically ACT-R 
;;;               interface functions they should probably be moved to the uwi
;;;               file.]
;;; 2014.02.11 cts
;;;             : Added mcl-GUI-layer methods to scroll the mouse up and down. When executed, mouse scroll events
;;;               are sent to the OS to relay to the front application, similar to how mouse clicks and keypresses are done.
;;;             : Added a guard to ensure that all views are using proper "flipped" MCL-style (top-left to bottom-right) coordinate systems.
;;; 2014.04.09 cts
;;;             : Using integers for points when possible, even after converting positions from local to window coordinates, adding
;;;               and subtracting points, etc.
;;; 2014.05.02 cts
;;;             : Added ability to extend color symbol rgb mapping by turning data structure into a hash table
;;;             : Ensured gray is always returned for gray/grey rgb color, regardless of order in the rgb-list.
;;;               This is needed since the color data structure is now a hash table and
;;;               insertion order is not guaranteed to be consistent in a cl hash table
;;; 2014.10.14 cts
;;;             : Code is compatible with ccl-1.8 thru ccl-1.10 on OS X SL thru Mavericks
;;;             : Using #/abortModal instead of #/stopModal to close modal dialog windows so
;;;               that close requests can be made on any thread, and not just the new modal dialog window thread.
;;;               Otherwise, strange things were happening on SL and ML where the modal window would not close after a request when that request
;;;               was made on a thread other than the modal dialog's thread (e.g., the REPL ACT-R thread).
;;;               In this case, the window did close once the mouse moved or keyboard was pressed after that request was made.
;;;               Using #/abortModal instead of #/stopModal ensures that the window closes in all cases tested.
;;; 2014.11.25 cts
;;;             : Ensured that image-views are properly sized if the image was provided when the view was created.
;;;               
;;; 2015.11.13 Dan Bothell
;;;             : Wrapped choose-file-dialog with ensure-defined as suggested by
;;;             : Mike Byrne since CCL 1.11 now includes such a function.  Also
;;;             : did choose-new-file-dialog and choose-directory-dialog to be
;;;             : safe.
;;; 2015.11.18 Dan Bothell
;;;             : Removed a spurious - which was accidentally added with the last
;;;             : update, but didn't break anything.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ----------------------------------------------------------------------
; Begin file: build/pre-code.lisp
; ----------------------------------------------------------------------


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui)
  (ccl::create-autorelease-pool))



; ----------------------------------------------------------------------
; End file: build/pre-code.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: bincarbon/sv-language-layer.lisp
; ----------------------------------------------------------------------


(defmacro %ensure-defined (form)
  (destructuring-bind (symb name (&rest arglist) &body body) form
    (declare (ignore arglist body))
    (unless
      (funcall
        (case symb
          (defun #'fboundp)
          (defgeneric #'fboundp)
          (defmacro #'macro-function)
          (defvar #'boundp))
        name)
      form)))

(defmacro ensure-defined (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (form)
                 `(%ensure-defined ,form))
               body)))

(defmacro with-continue (&body body)
  `(handler-bind ((error #'continue))
     ,@body))

(provide :sv-language-layer)



; ----------------------------------------------------------------------
; End file: bincarbon/sv-language-layer.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: bincarbon/lol-subset.lisp
; ----------------------------------------------------------------------


;; Antiweb (C) Doug Hoyte

;; This is a "production" version of LOL with bug-fixes
;; and new features in the spirit of the book.

;; See http://letoverlambda.com

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!
;;
;; 2012-06-29: Clayton Stanley. I took only a subset
;; of Doug Hoyte's LOL production code, which is included
;; below. I did not modify any code within this subset,
;; except to change a few &rest to &body keywords in macro definitions,
;; so that slimv could handle auto-indenting properly. The behavior
;; of the code 'was not changed'.

(require :sv-language-layer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-defined
    (defun mkstr (&rest args)
      (with-output-to-string (s)
        (dolist (a args) (princ a s))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
      (if source (rec source nil) nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-defined
    (defun flatten (lis)
      "Takes a nested list and makes in into a single-level list"
      (declare (list lis))
      (labels ((rec (lis acc)
                 (cond ((null lis) acc)
                       ((atom lis) (cons lis acc))
                       (t (rec (car lis) (rec (cdr lis) acc))))))
        (rec lis nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defmacro/g! (name args &body body)
    (let ((syms (remove-duplicates
                  (remove-if-not #'g!-symbol-p
                                 (flatten body)))))
      `(defmacro ,name ,args
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                  (symbol-name s)
                                  2))))
                syms)
           ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2))))

(defmacro defmacro! (name args &body body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

;; Graham's alambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; Graham's aif
(ensure-defined
  (defmacro aif (test then &optional else)
    `(let ((it ,test))
       (if it ,then ,else))))

(provide :lol-subset)



; ----------------------------------------------------------------------
; End file: bincarbon/lol-subset.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: bincarbon/sv-utilities.lisp
; ----------------------------------------------------------------------


(require :sv-language-layer)
(require :lol-subset)

(ensure-defined
  (defmacro push-to-end (item place)
    "analogous to the push macro; just places 'item' at the end of 'place', instead of the front"
    `(setf ,place (nconc ,place (list ,item)))))

(defun spin-for-fct (ms-delay)
  (without-interrupts
    (let ((start (internal-real-time->ms
                   (get-internal-real-time))))
      (loop until (< ms-delay (- (internal-real-time->ms
                                   (get-internal-real-time))
                                 start))))))

(defun internal-real-time->ms (&optional (internal-real-time (get-internal-real-time)))
  (* 1000
     (/ internal-real-time
        internal-time-units-per-second)))

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string returning two values: the string and the number of bytes read."
  (if path
    (with-open-file (s path)
      (let* ((len (file-length s))
             (data (make-string len)))
        (values data (read-sequence data s))))))

(ensure-defined
  (defmacro awhen (test-form &body body)
    `(aif ,test-form
       (progn ,@body))))

(defmacro! acond (&rest clauses)
  "works just like cond, but stores the value of each condition as 'it', which is accessible in the code following the condition"
  (if clauses
    (let ((cl1 (car clauses)))
      `(let ((,g!sym ,(car cl1)))
         (if ,g!sym
           (let ((it ,g!sym)) 
             (declare (ignorable it)) 
             ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

(ensure-defined
  (defmacro while (test &body body)
    "loops through body, evaluating test each time until test returns false"
    `(do ()
       ((not ,test)) 
       ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro! guard ((guard &rest errstr) &body body)
  (let ((errstr (if errstr 
                  errstr
                  (list "guard ~a failed" `',guard))))
    `(let* ((it (multiple-value-list (progn ,@body)))
            (it1 (first it)))
       (declare (ignorable it it1))
       (assert ,guard nil ,@errstr)
       (apply #'values it))))

(defmacro! with-shadow ((fname fun) &body body)
  "Shadow the function named fname with fun
   Any call to fname within body will use fun, instead of the default function for fname.
   This macro is intentionally unhygienic:
   fun-orig is the anaphor, and can be used in body to access the shadowed function"
  `(let ((fun-orig))
     (cond ((fboundp ',fname) 
            (setf fun-orig (symbol-function ',fname))
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (setf (symbol-function ',fname) fun-orig)))
           (t
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (fmakunbound ',fname))))))

(defmethod getf-include-key (place key)
  (aif (getf place key)
    (list key it)))

(defmethod getf-include-key (place (keys list))
  (loop for key in keys
        append (getf-include-key place key))) 

(provide :sv-utilities)



; ----------------------------------------------------------------------
; End file: bincarbon/sv-utilities.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: bincarbon/logger.lisp
; ----------------------------------------------------------------------


#-:clozure (error "This file only works with Clozure Common Lisp and not RMCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

(defparameter *sv-log-level* 0) 

(defun get-cur-timestring ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
    (declare (ignore day-of-week dst-p tz))
    (let ((ms (floor (mod (internal-real-time->ms) 1000))))
      (format nil "~a-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d:~a" year month date hour minute second ms))))

(defun sv-log (&rest args)
  (#_NSLog 
   (objc:make-nsstring
     (with-output-to-string (strm) 
       (format strm "sv-log: ~a: on thread ~a: " (get-cur-timestring) *current-process*)
       (unwind-protect (apply #'format strm args)
         (fresh-line strm))))))

(defun sv-log-n (log-level &rest args)
  (when (<= log-level *sv-log-level*)
    (apply #'sv-log args)))



; ----------------------------------------------------------------------
; End file: bincarbon/logger.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: easygui/patches.lisp
; ----------------------------------------------------------------------


(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import 'easygui:cocoa-ref)
  (shadowing-import 'easygui:dcc)
  (shadowing-import 'easygui::running-on-main-thread))

(defclass easygui::cocoa-extension-mixin ()
  ((easygui::easygui-view :initarg :eg-view :reader easygui::easygui-view-of)
   (easygui::flipped :initarg :flipped :initform easygui::*screen-flipped*)))

; Providing a keyword argument to allow negative points
(defun easygui::point (x y &key (allow-negative-p t))
  (unless allow-negative-p
    (assert (>= x 0))
    (assert (>= y 0)))
  (make-instance 'easygui::eg-point :x x :y y))

; Patching the function to provide a keyword argument that allows for negative mouse coordinates
(defun easygui::view-mouse-position (view &key (allow-negative-position-p nil))
  (let* ((w (cocoa-ref (easygui::easygui-window-of view)))
         (mouselocation (dcc (#/mouseLocationOutsideOfEventStream w)))
         (cview (if (typep view 'window) (easygui::content-view view) view))
         (nspt (dcc (#/convertPoint:fromView: (cocoa-ref cview) mouselocation NIL))))
    ;; TODO: check point is inside bounds, lest negative coordinates
    (easygui:point (ns:ns-point-x nspt)
                   (ns:ns-point-y nspt)
                   :allow-negative-p allow-negative-position-p)))

; I think I found a bug in these two methods in the easygui package, so redefining them here with correct setNeedsDisplay: call
(defmethod (setf easygui::view-position) (point (self easygui::view))
  (running-on-main-thread ()
    (setf (slot-value self 'easygui::position) point)
    (when (slot-value self 'easygui::frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (easygui::view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

(defmethod (setf easygui::view-size) (point (self easygui::view))
  (running-on-main-thread ()
    (setf (slot-value self 'easygui::size) point)
    (when (slot-value self 'easygui::frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (easygui::view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

; I wanted to instantiate my own extended contained-view class, but I didn't see an easy way to do this given the current
; easygui code. So adding a contained-view-specifically slot to the mixin class, defaulting it to the contained-view class
; defined in easygui. If you want to instantiate a different class for the contained view, just overwrite this default.
(defclass easygui::content-view-mixin ()
  ((easygui::content-view :initarg :content-view)
   (easygui::objc-content-view-accessor :reader easygui::objc-content-view-accessor :initarg :objc-content-view-accessor :initform #'#/contentView)
   (easygui::flipped :initarg :flipped :initform easygui::*screen-flipped*)
   (easygui::contained-view-specifically :initarg :contained-view-specifically :initform 'easygui::contained-view)))

(defmethod easygui::content-view ((view easygui::content-view-mixin))
  (assert (eql (cocoa-ref (slot-value view 'easygui::content-view))
               (dcc (funcall (easygui::objc-content-view-accessor view) (cocoa-ref view)))))
  (slot-value view 'easygui::content-view))

; Added code to instantiate the contained view class that is stored as a slot on the mixin object
(defmethod easygui::initialize-view :after ((view easygui::content-view-mixin))
  (unless (slot-boundp view 'easygui::content-view)
    (let ((containee (make-instance (slot-value view 'easygui::contained-view-specifically) 
                                    :cocoa-ref (dcc (#/contentView (cocoa-ref view)))
                                    :view-nick-name 'easygui::%CONTENT-OF-CONTENT-VIEW%
                                    :flipped (slot-value view 'easygui::flipped))))
      (setf (slot-value view 'easygui::content-view) containee
            (slot-value containee 'easygui::parent) view))))

; Redefining to use the &body body pairing instead of &rest body, so that Slime auto indenting works properly
(defmacro easygui::running-on-this-thread ((&key (waitp t)) &body body)
  ;; The purpose of this trivial macro is to mark places where it is thought possible that
  ;; it may be preferable to use running-on-main-thread.
  (declare (ignore waitp))
  `(progn ,@body))

; Radio buttons in 10.8 require being enclosed within an NSMatrix.
; If not (default easygui implementation), each one is assigned to the
; same virtual NSMatrix, which means they are all part of the same cluster,
; which means that the common lisp clustering implementation breaks since
; Cocoa is now forcing all radio buttons to be part of the same cluster.
; The fix is to use NSSwitchButton Cocoa functionality for radio buttons,
; but use NSRadioButton images (for selected and deselected) for the implementation.
; Another approach would have been to embed each radio button in its own NSMatrix,
; but this required much more code than the image switching technique, and it messed up the overall
; easygui design, since the cocoa-ref of a radio-button object would have been an NSMatrix,
; which breaks the view-text and (setf view-text) mixin methods, as well as auto sizing the
; view to the text it contains on init, etc. So, I went with the simple image switching hack.

(let ((alternate-radio-button-image)
      (radio-button-image)
      (radio-button))
  (defmethod easygui::initialize-view :after ((self easygui::radio-button-view))
    (labels ((init-images ()
               (setf radio-button (make-instance 'easygui::cocoa-button))
               (#/setButtonType: radio-button #$NSRadioButton)
               (setf radio-button-image (#/image radio-button))
               (setf alternate-radio-button-image (#/alternateImage radio-button))))
      (when (cocoa-ref self)
        (unless radio-button
          (init-images))
        (dcc (#/setButtonType: (cocoa-ref self) #$NSSwitchButton))
        (dcc (#/setImage: (cocoa-ref self) radio-button-image))
        (dcc (#/setAlternateImage: (cocoa-ref self) alternate-radio-button-image))
        (when (slot-value self 'easygui::selected) (easygui::radio-button-select self))
        (setf (slot-value (cocoa-ref self) 'easygui::easygui-view) self)))))



; ----------------------------------------------------------------------
; End file: easygui/patches.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: easygui/extensions.lisp
; ----------------------------------------------------------------------


(defun round-if-integer (double)
  (if (equalp double (round double))
    (round double)
    double))

(defun easygui::point-from-ns-point (point)
  (easygui::point 
    (round-if-integer (ns:ns-point-x point))
    (round-if-integer (ns:ns-point-y point))
    :allow-negative-p t))

; easygui by default starts position 0,0 at bottom left, going to the right and up for positive values
; This flips the screen vertically, so that it matches MCL's default. That is, position 0,0 is at top left
(setf easygui::*screen-flipped* t)

(setf easygui::*debug-cocoa-calls* nil)
(setf easygui::*report-flipping-errors* t)

; There are particular window configurations that keep the window from becoming key or main (borderless windows for example).
; And this causes odd behavior for these types of windows (can't select the win when using command `, window is backgrounded behind
; the listener window after a dialog window opens and closes).
;
; For the time being, there are no types of cocoa windows that should not be able to become key or main. So until customization is
; needed, override these methods for cocoa windows and allow everyone the ability to become key and main.

(objc:defmethod (#/canBecomeKeyWindow #>BOOL) ((self easygui::cocoa-window))
  #$YES)

(objc:defmethod (#/canBecomeMainWindow #>BOOL) ((self easygui::cocoa-window))
  #$YES)

; ----------------------------------------------------------------------
; Extend the Objective C cocoa-drawing-view in the easygui package with a view that does not monitor mouse movement or clicks
;
; lisp->objective c class name mapping: drawing-overlay-view->cocoa-drawing-overlay-view
; ----------------------------------------------------------------------

; Create the objective c class
(defclass easygui::cocoa-drawing-overlay-view (easygui::cocoa-drawing-view)
  ()
  (:metaclass ns:+ns-object))

; And create the lisp equivalent class
; And register the objective c extension and lisp class to the easygui package, so that it instantiates a 
; cocoa-drawing-overlay-view object in the cocoa-ref slot when a drawing-overlay-view lisp object is instantiated
(defclass easygui::overlay-view (easygui::view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-drawing-overlay-view))

; Add the hook method in objective c that will cause the new class to not respond to mouse activity
(objc:defmethod #/hitTest: ((self easygui::cocoa-drawing-overlay-view) (point :<NSP>oint))
  ccl:+null-ptr+)

; ----------------------------------------------------------------------
; Use a consuming-view class to say that all subviews within an instance of that class will not respond to mouse clicks. 
;
; This is to work around the differences in first responders between MCL and CCL. MCL looks down the view hierarchy for the first responder
; (breadth first), CCL looks down the hierarchy for the deepest responder (depth first). In order
; to simulate breadth first by stopping at a particular view in the tree (and not inspecting that view's
; subviews), create an instance of the consuming-view class. 
; ----------------------------------------------------------------------

(defclass easygui::cocoa-drawing-consuming-view (easygui::cocoa-drawing-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::consuming-view (easygui::view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-drawing-consuming-view))

; Override hitTest; if a view (or one of its subviews) returns a non-nil value
; for the default hitTest call, then return self; this suppresses subviews of 
; self from responding to mouse clicks
; 
; Reference this URL for call-next-method syntax in objc:defmethod macro: 
; http://clozure.com/pipermail/openmcl-devel/2008-November/008645.html

(objc:defmethod #/hitTest: ((self easygui::cocoa-drawing-consuming-view) (point :<NSP>oint))
  (let ((ret (call-next-method point)))
    (if (not (equal ccl:+null-ptr+ ret))
      self
      ccl:+null-ptr+)))

; ----------------------------------------------------------------------
; Providing a view container to hold and display images.
; ----------------------------------------------------------------------

(defclass easygui::cocoa-image-view (easygui::cocoa-extension-mixin ns:ns-image-view)
  ()
  (:metaclass ns:+ns-object))

(defmacro define-is-flipped-method (class)
  `(objc::defmethod (#/isFlipped :<BOOL>) ((self ,class))
     (handler-case (if (slot-value self 'easygui::flipped) #$YES #$NO)
       (simple-error (condition)
                     (when easygui::*report-flipping-errors* (format t "'isFlipped ~s' ignores error~%" self))
                     (values (if easygui::*screen-flipped* #$YES #$NO) condition)))))


(define-is-flipped-method easygui::cocoa-image-view)

(defclass easygui::image-view (easygui::view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-image-view))

(defclass easygui::cocoa-clickable-image-view (easygui::cocoa-image-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::clickable-image-view (easygui::image-view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-clickable-image-view))

(easygui::define-useful-mouse-event-handling-routines easygui::cocoa-clickable-image-view)
(easygui::define-useful-mouse-event-handling-routines easygui::cocoa-contained-view)
(easygui::define-useful-mouse-event-handling-routines easygui::cocoa-mouseable-text-field)

(defmethod easygui::click-location ((cocoa-self ns:ns-view) (the-event ns:ns-event))
  (let* ((ns-point (#/locationInWindow the-event)))
    (let* ((ns-converted-point (#/convertPoint:fromView: cocoa-self ns-point nil)))
      (let ((where (easygui::point-from-ns-point ns-converted-point)))
        where))))

(objc:defmethod (#/mouseDown: :void) ((self easygui::cocoa-button) the-event)
  (call-next-method the-event)
  (let ((click-location (easygui::click-location self the-event)))
    (unless (ccl:%null-ptr-p (#/window self)) ; Could be nil if view or view parent was removed when :dialog-item-action fired
      (when (#/isVisible (#/window self)) ; Check for if window is closed and run loop hasn't refreshed
        (easygui::mouse-down (easygui::easygui-view-of self) :location click-location)))))

; ----------------------------------------------------------------------
; Providing a mixin class that keeps a view from implicitly redrawing each
; time a subview is added to the display
; ----------------------------------------------------------------------

(defclass easygui::static-view-mixin ()
  ())

(defmethod easygui::set-needs-display ((view easygui::static-view-mixin) flag)
  (declare (ignore flag))
  (values))

(defmethod easygui::invalidate-view ((view easygui::static-view-mixin) &optional total)
  (declare (ignore total))
  (mapc #'easygui::invalidate-view (easygui:view-subviews view)))

; ----------------------------------------------------------------------
; Creating MCL's top-level simple-view class
;
; In order to implement MCL's top-level simple-view class, I needed a cocoa view class that was capable of drawing to the display
; (since simple-view can do this in MCL). Cocoa-drawing-view in easygui seemed like the appropriate class for this. However, the 
; default lisp class (drawing-view) for this class did a bit more than a top-level simple-view class should do. It tracks mouse movement, 
; which was a problem because a cocoa window class should not track movement, MCL's window class inherits from simple-view, so there were 
; many collisions with window class being a subclass of drawing-view. So the current fix is to define a simple-view class that inherits
; only from the easygui view top-level class, but associate that class with cocoa-drawing-view. So make-instance 'simple-view will
; create a simple-view object, and instantiate a cocoa-drawing-view object for that view. This allows window to be a subclass of simple-view,
; simple-view objects to draw to the display, and little code modification/extension to easygui since we're leveraging the objective c methods
; on cocoa-drawing-view.
; ----------------------------------------------------------------------

(defclass easygui::simple-view (easygui::view)
  ((easygui::flipped :initform easygui::*screen-flipped* :initarg :flipped :reader easygui::flipped-p))
  (:default-initargs :specifically 'easygui::cocoa-drawing-view))

; This section is the additional code required to have a simple-view object behave mostly like a drawing-view type object, 
; but without inheriting from drawing-view. Sort of a workaround to avoid the drawing-view mouse-tracking methods, since those aren't mixins (yet).

(defmethod easygui::link-cocoa-view ((cocoa-view t) view)
  (declare (ignore view))
  (values))

(defmethod easygui::link-cocoa-view ((cocoa-view easygui::cocoa-extension-mixin) view)
  (setf (slot-value cocoa-view 'easygui::easygui-view) view))

(defmethod easygui::initialize-view :after ((view easygui::simple-view))
  (easygui::link-cocoa-view (easygui:cocoa-ref view) view))

; This keeps the setDrawsBackground attribute on the Cocoa object in sync with the 
; current background color (is it transparent or not).
(defmethod easygui:set-back-color :after ((view easygui::background-coloring-mixin) (new-color ns:ns-color) &optional redisplay-p)
  (setf (slot-value view 'easygui::drawsbackground) 
        (if (equal (#/clearColor ns:ns-color) new-color) nil t))
  (#/setDrawsBackground: (cocoa-ref view) (slot-value view 'easygui::drawsbackground))
  (when redisplay-p 
    (easygui:invalidate-view view)))

(defmethod easygui::cocoa-win-p ((win t))
  nil)

(defmethod easygui::cocoa-win-p ((win easygui::cocoa-window))
  (cond ((slot-boundp win 'easygui::easygui-window)
         t)
        (t
         (sv-log "not yet a cocoa win ~a" win)
         nil)))

(defmethod easygui::size-to-fit ((view easygui::view))
  (let ((frame (#/frame (cocoa-ref view))))
    (setf (slot-value view 'easygui::size)
          (easygui:point (ns:ns-rect-width frame)
                         (ns:ns-rect-height frame)))))

; Extending this method; patching it so that the view-size slot is initialized after 
; the view is drawn, if it wasn't already.
(defmethod easygui::add-1-subview :around ((view easygui::simple-view) (super-view easygui::simple-view))
  "Correctly initialize view positions"
  (unwind-protect (call-next-method)
    (with-slots (easygui::position easygui::size easygui::frame-inited-p) view
      (unless (slot-boundp view 'easygui::size)
        (easygui::size-to-fit view))
      (easygui::set-needs-display view t)
      (unless (easygui::view-subviews-busy super-view) (easygui::set-needs-display super-view t)))))

; Isolating the code to convert a vertical coordinate if the screen is flipped. Using just this part in ccl-simple-view.lisp
(defun easygui::convert-if-screen-flipped (y height)
  (if easygui::*screen-flipped*
    (- (easygui::screen-height) height y)
    y))

(defmethod easygui::window-hide ((window easygui::window))
  (easygui::running-on-this-thread ()
    (let ((cwin (cocoa-ref window)))
      (unless (easygui::window-hidden window)
        (setf (slot-value window 'easygui::hidden) t)
        (unless (dcc (#/isMiniaturized cwin))
          (dcc (#/miniaturize: cwin cwin))))
      (when (dcc (#/isFlushWindowDisabled cwin))
        (dcc (#/enableFlushWindow cwin))
        (dcc (#/flushWindow cwin)))
      window)))

(defclass easygui::cocoa-matrix (easygui::cocoa-extension-mixin ns:ns-matrix)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-text-field-cell (easygui::cocoa-extension-mixin ns:ns-text-field-cell)
  ()
  (:metaclass ns:+ns-object))

(defun easygui::screen-width nil
  (easygui::running-on-this-thread ()
    (ns:ns-rect-width (dcc (#/frame (#/objectAtIndex: (#/screens ns:ns-screen) 0))))))

; Default makeKeyAndOrderFront: zeros views with negative coordinates. This behavior is 
; undesirable, and does not match the MCL spec. The fix is to ensure the views desired coordinates
; (even if negative) are set for the window by changing the window's position to those coordinates
; after the default makeKeyAndOrderFront: method is called
(objc:defmethod (#/makeKeyAndOrderFront: :void) ((cocoa-win easygui::cocoa-window) (id :id))
  (call-next-method id)
  (let ((win (easygui::easygui-window-of cocoa-win)))
    (setf (easygui::view-position win) (easygui::view-position win))))

; All cocoa windows will auto recalculate the key view loop, since this works most of the time, and isn't computed in the inner loop of a program
; http://stackoverflow.com/questions/4271115/how-should-i-subclass-nswindow-initialization-in-objective-c

; FIXME: Calling #'call-next-method within this method causes a compile-time segfault in the testing environment when a custom ccl app is built using ccl-1.9 on OS X Mavericks.
; Still works fine on OS X Mavericks and ccl-1.9 when the custom app isn't built, and still works in 10.6 and 10.8 when the custom app is built.
; so not worrying about this, yet. The fix is probably to do away with or modify the custom app building, as that was a hack anyways. CTS: 2014-04-08
(objc:defmethod (#/initWithContentRect:styleMask:backing:defer: :id) ((cocoa-win easygui::cocoa-window) (content-rect :<NSR>ECT) (style-mask :<NSUI>NTEGER)
                                                                                                        (backing :<NSB>ACKING<S>TORE<T>YPE) (defer :<BOOL>))
  (unwind-protect (call-next-method content-rect style-mask backing defer)
    (#/setAutorecalculatesKeyViewLoop: cocoa-win #$YES)))

; Class definitions for ns-text-view base cocoa class

(defclass easygui::cocoa-text-view (easygui::cocoa-extension-mixin ns:ns-text-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-scroll-view (easygui::cocoa-extension-mixin ns:ns-scroll-view)
  ()
  (:metaclass ns:+ns-object))

; NSTextView uses #/string and #/setString methods, which are different from #/title (title-mixin) and #/stringValue (string-value-mixin) methods,
; so creating an additional mixin when dealing with text objects that inherit from NSTextView

(defclass easygui::view-text-via-string-mixin ()
  ())

(defmethod easygui::view-text ((view easygui::view-text-via-string-mixin))
  (objc:lisp-string-from-nsstring (#/string (cocoa-ref view))))

(defmethod (setf easygui::view-text) (new-text (view easygui::view-text-via-string-mixin))
  (#/setString: (cocoa-ref view) (objc:make-nsstring new-text))
  new-text)



; ----------------------------------------------------------------------
; End file: easygui/extensions.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: bincarbon/resources.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Some functions to create and manage resource data for CCL
;
; If images or sounds are needed for your application, these functions can 
; be used to manage those resources. Usual getters/setters/creators are available:
;
; #'create-resource: Creates an image or sound resource, given a path to that file
; #'add-resource: Adds a created resource to the pool
; #'get-resource-val: Retrieves a resource's value from the pool
;
; Note that a form of lazy evaluation is used to alloc the resources only when needed
; That is, each resource is alloc'd the first time it's retrieved, 'not' when it's created, or
; added to the pool. If you want to alloc all resources currently in the pool (for pre-caching), 
; call #'alloc-resources

#-:clozure (error "This file only works with Clozure Common Lisp and not RMCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa))

(defun init-pool ()
  (make-hash-table :test #'equalp))

(defparameter *resource-pool* (init-pool))
(defvar *resource-types* nil)

(defun print-pool (&optional (pool *resource-pool*))
  (maphash (lambda (key val)
             (format t "~a->~a~%" key val))
           pool))

(defun get-pool-as-lst (&optional (pool *resource-pool*))
  (loop for value being the hash-values of pool using (hash-key key)
        collect (cons key value)))

(defclass resource ()
  ((val :accessor val :initarg :val)
   (type :accessor type :initarg :type)
   (alloc-fn :accessor alloc-fn :initarg :alloc-fn)))

(defmacro when-bound ((name instance))
  `(if (slot-boundp  ,instance ',name)
     (,name ,instance)
     'slot-unbound))

(defmethod print-object ((obj resource) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (format stream "val->~a,alloc-fun->~a~%"
            (when-bound (val obj))
            (when-bound (alloc-fn obj)))))

(defmethod alloc-resource ((obj resource))
  (unless (slot-boundp obj 'val)
    (setf (val obj) (funcall (alloc-fn obj))))
  obj)

(defun alloc-resources (&optional (pool *resource-pool*))
  (maphash 
    (lambda (key val)
      (declare (ignore key))
      (alloc-resource val))
    pool))

(defmethod get-val ((obj resource))
  (alloc-resource obj)
  (val obj))

(defun resource-present-p (id &optional type (pool *resource-pool*))
  (let ((possible-types
          (if type 
            (list type)
            *resource-types*))
        (out))
    (dolist (type possible-types)
      (multiple-value-bind (resource present-p) (gethash (get-key id type) pool)
        (when present-p
          (push resource out))))
    (when out
      (unless (eq (length out) 1)
        (error "multiple resources with id ~a present in pool ~a~%" id pool))
      (values t (first out)))))

(defun get-resource (id &optional type (pool *resource-pool*))
  (multiple-value-bind (present-p resource) (resource-present-p id type pool)
    (unless present-p
      (error "resource with id ~a not present in pool ~a~%" id pool))
    resource))

(defun get-resource-val (id &optional type (pool *resource-pool*))
  (get-val (get-resource id type pool)))

(defun get-id (resource &optional (pool *resource-pool*))
  (declare (ignore resource pool))
  (error "write this when needed"))

(defun get-key (id type)
  (format nil "~a.~a" id type))

(defun add-resource (resource id &optional (pool *resource-pool*))
  (sv-log "adding resource with key ~a" (get-key id (type resource)))
  (when (resource-present-p id (type resource) pool)
    (sv-log "adding resource with key ~a and overwriting resource with same key that is already present~%"
            (get-key id (type resource))))
  (setf (gethash (get-key id (type resource)) pool) resource))

(defun remove-resource (resource &optional (pool *resource-pool*))
  (declare (ignore resource pool))
  (error "write this when needed"))

(defmethod create-resource ((type (eql 'image)) path)
  (make-instance 
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithContentsOfFile: 
       (#/alloc ns:ns-image) 
       (objc:make-nsstring path)))))

(defmethod create-resource ((type (eql 'sound)) path)
  (make-instance
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithContentsOfFile:byReference:
       (#/alloc ns:ns-sound)
       (objc:make-nsstring path)
       nil))))

(defmethod create-resource :around (type path)
  (declare (ignore path))
  (let ((res (call-next-method)))
    (pushnew type *resource-types*)
    (setf (type res) type)
    res))

; I am requiring all objective-c/lisp functions to not ever use ns-arrays as inputs or outputs
; This slows down computation time b/c conversions have to be done within each function, but it
; makes each one much easier to use in a lisp environment (keep lisp types for inputs and outputs).
;
; I am not doing this for ns-mutable string; I'm putting up with doing the conversion on that one when 
; needed. It also would be problematic to convert the type of anything that can be placed within an ns-array.
; Done this way, where the containers (arrays/lists) are converted, but not the containees, the container
; conversion functions do not have to do any type conversion or type checking.

(defmacro! do-array ((varsym array &optional ret) &body body)
  `(loop for ,g!i below (#/count ,array)
         for ,varsym = (#/objectAtIndex: ,array ,g!i)
         do (progn ,@body)
         ,@(if ret `(finally (return ,ret)))))

(defun ns-array->list (ns-array)
  (let ((out))
    (do-array (item ns-array out)
      (push-to-end item out))))

(defun list->ns-array (lst)
  (let ((out (#/array ns:ns-mutable-array)))
    (dolist (item lst out)
      (#/addObject: out item))))

(defun contents-of-directory (dir)
  (ns-array->list
    (#/contentsOfDirectoryAtPath:error:
     (#/defaultManager ns:ns-file-manager)
     (objc:make-nsstring dir)
     ccl:+null-ptr+)))

(defun remove-if-not-predicate (lst predicate)
  (ns-array->list
    (#/filteredArrayUsingPredicate:
     (list->ns-array lst)
     (#/predicateWithFormat:
      ns:ns-predicate
      (objc:make-nsstring predicate)))))

(defun remove-if-not-image (lst)
  (union
    (remove-if-not-predicate lst "self ENDSWITH '.tiff'")
    (remove-if-not-predicate lst "self ENDSWITH '.png'")))

(defun remove-if-not-sound (lst)
  (remove-if-not-predicate lst "self ENDSWITH '.aif'"))

(defun open-resource-folder (dir)
  (let ((dir (if (pathnamep dir) 
               (directory-namestring dir)
               dir)))
    (let ((dir (format nil "~a/" (string-right-trim (list #\/) dir))))
      (loop for (type filter-fun) in (list (list 'image #'remove-if-not-image)
                                           (list 'sound #'remove-if-not-sound))
            do (dolist (image-name (funcall filter-fun (contents-of-directory dir)))
                 (let* ((image-name-lisp-str (objc:lisp-string-from-nsstring image-name))
                        (image-name-no-ext (#/stringByDeletingPathExtension image-name))
                        (res (create-resource type (format nil "~a~a" dir image-name-lisp-str))))
                   (add-resource res (objc:lisp-string-from-nsstring image-name-no-ext))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :resources))



; ----------------------------------------------------------------------
; End file: bincarbon/resources.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: bincarbon/mcl-ccl-colors.lisp
; ----------------------------------------------------------------------


#-:clozure (error "This file only works with Clozure Common Lisp and not RMCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

; ----------------------------------------------------------------------
; Defining color-symbol->system-color and system-color->symbol for CCL.
;
; These functions may have been in the base CCL distribution, but I couldn't find them.
; So I searched online for a table of color names -> RGB mappings, threw that
; data into a bash shell, cleaned up the text, and then pasted it here. A few lisp
; parentheses were wrapped around that, which turned the data into a lexical closure.
; ----------------------------------------------------------------------

(defparameter *color-table* (make-hash-table :test #'eq))

(defun add-color-symbol-mapping (symb rgb-lst)
  (setf (gethash symb *color-table*) rgb-lst))

(let ((rgb-list
        (list
          'Grey (list 84 84 84)
          'gray (list 190 190 190)
          'grey (list 190 190 190)
          'LightGray (list 211 211 211)
          'Light-Gray (list 211 211 211)
          'LightSlateGrey (list 119 136 153)
          'dark-gray (list 169 169 169)
          'SlateGray (list 112 128 144)
          'black (list 0 0 0)
          'AliceBlue (list 240 248 255)
          'BlueViolet (list 138 43 226)
          'CadetBlue (list 95 158 160)
          'CadetBlue (list 95 158 160)
          'CornflowerBlue (list 100 149 237)
          'DarkSlateBlue (list 72 61 139)
          'DarkTurquoise (list 0 206 209)
          'DeepSkyBlue (list 0 191 255)
          'DodgerBlue (list 30 144 255)
          'LightBlue (list 173 216 230)
          'Light-Blue (list 173 216 230)
          'LightCyan (list 224 255 255)
          'LightSkyBlue (list 135 206 250)
          'LightSlateBlue (list 132 112 255)
          'LightSteelBlue (list 176 196 222)
          'Aquamarine (list 112 219 147)
          'MediumBlue (list 0 0 205)
          'MediumSlateBlue (list 123 104 238)
          'MediumTurquoise (list 72 209 204)
          'MidnightBlue (list 25 25 112)
          'NavyBlue (list 0 0 128)
          'PaleTurquoise (list 175 238 238)
          'PowderBlue (list 176 224 230)
          'RoyalBlue (list 65 105 225)
          'SkyBlue (list 135 206 235)
          'SlateBlue (list 106 90 205)
          'SteelBlue (list 70 130 180)
          'aquamarine (list 127 255 212)
          'azure (list 240 255 255)
          'blue (list 0 0 255)
          'aqua (list 0 255 255)
          'cyan (list 0 255 255)
          'navy (list 0 0 128)
          'teal (list 0 128 128)
          'turquoise (list 64 224 208)
          'DarkSlateGray (list 47 79 79)
          'Iris (list 3 180 200)
          'RosyBrown (list 188 143 143)
          'SaddleBrown (list 139 69 19)
          'SandyBrown (list 244 164 96)
          'beige (list 245 245 220)
          'brown (list 165 42 42)
          'brown (list 166 42 42)
          'burlywood (list 222 184 135)
          'chocolate (list 210 105 30)
          'peru (list 205 133 63)
          'tan (list 210 180 140)
          'Sienna (list 142 107 35)
          'Tan (list 219 147 112)
          'DarkGreen (list 0 100 0)
          'dark-green (list 0 100 0)
          'DarkKhaki (list 189 183 107)
          'DarkOliveGreen (list 85 107 47)
          'olive (list 128 128 0)
          'DarkSeaGreen (list 143 188 143)
          'ForestGreen (list 34 139 34)
          'GreenYellow (list 173 255 47)
          'LawnGreen (list 124 252 0)
          'LightSeaGreen (list 32 178 170)
          'LimeGreen (list 50 205 50)
          'MediumSeaGreen (list 60 179 113)
          'MediumSpringGreen (list 0 250 154)
          'MintCream (list 245 255 250)
          'OliveDrab (list 107 142 35)
          'PaleGreen (list 152 251 152)
          'SpringGreen (list 0 255 127)
          'YellowGreen (list 154 205 50)
          'chartreuse (list 127 255 0)
          'green (list 0 255 0)
          'green (list 0 128 0)
          'lime (list 0 255 0)
          'khaki (list 240 230 140)
          'DarkOrange (list 255 140 0)
          'DarkSalmon (list 233 150 122)
          'LightCoral (list 240 128 128)
          'LightSalmon (list 255 160 122)
          'PeachPuff (list 255 218 185)
          'bisque (list 255 228 196)
          'coral (list 255 127 0)
          'coral (list 255 127 80)
          'honeydew (list 240 255 240)
          'orange (list 255 165 0)
          'salmon (list 250 128 114)
          'sienna (list 160 82 45)
          'Orange (list 255 127 0)
          'DeepPink (list 255 20 147)
          'HotPink (list 255 105 180)
          'IndianRed (list 205 92 92)
          'LightPink (list 255 182 193)
          'MediumVioletRed (list 199 21 133)
          'MistyRose (list 255 228 225)
          'OrangeRed (list 255 69 0)
          'PaleVioletRed (list 219 112 147)
          'VioletRed (list 208 32 144)
          'firebrick (list 178 34 34)
          'pink (list 255 192 203)
          'Flesh (list 245 204 176)
          'Feldspar (list 209 146 117)
          'red (list 255 0 0)
          'tomato (list 255 99 71)
          'Firebrick (list 142 35 35)
          'Pink (list 188 143 143)
          'Salmon (list 111 66 66)
          'Scarlet (list 140 23 23)
          'DarkOrchid (list 153 50 204)
          'DarkViolet (list 148 0 211)
          'LavenderBlush (list 255 240 245)
          'MediumOrchid (list 186 85 211)
          'MediumPurple (list 147 112 219)
          'lavender (list 230 230 250)
          'magenta (list 255 0 255)
          'fuchsia (list 255 0 255)
          'maroon (list 176 48 96)
          'orchid (list 218 112 214)
          'Orchid (list 219 112 219)
          'plum (list 221 160 221)
          'purple (list 160 32 240)
          'purple (list 128 0 128)
          'thistle (list 216 191 216)
          'violet (list 238 130 238)
          'Maroon (list 128 0 0)
          'Plum (list 234 173 234)
          'Thistle (list 216 191 216)
          'Turquoise (list 173 234 234)
          'Violet (list 79 47 79)
          'AntiqueWhite (list 250 235 215)
          'FloralWhite (list 255 250 240)
          'GhostWhite (list 248 248 255)
          'NavajoWhite (list 255 222 173)
          'OldLace (list 253 245 230)
          'WhiteSmoke (list 245 245 245)
          'gainsboro (list 220 220 220)
          'ivory (list 255 255 240)
          'linen (list 250 240 230)
          'seashell (list 255 245 238)
          'snow (list 255 250 250)
          'wheat (list 245 222 179)
          'white (list 255 255 255)
          'Quartz (list 217 217 243)
          'Wheat (list 216 216 191)
          'BlanchedAlmond (list 255 235 205)
          'DarkGoldenrod (list 184 134 11)
          'LemonChiffon (list 255 250 205)
          'LightGoldenrod (list 238 221 130)
          'LightGoldenrodYellow (list 250 250 210)
          'LightYellow (list 255 255 224)
          'PaleGoldenrod (list 238 232 170)
          'PapayaWhip (list 255 239 213)
          'cornsilk (list 255 248 220)
          'goldenrod (list 218 165 32)
          'moccasin (list 255 228 181)
          'yellow (list 255 255 0)
          'gold (list 255 215 0)
          'Goldenrod (list 219 219 112)
          'copper (list 184 115 51)
          'brass (list 181 166 66)
          'bronze (list 140 120 83)
          'CSS (list 204 153 0)
          'gold (list 205 127 50)
          'silver (list 230 232 250))))
  (loop for item on rgb-list by #'cddr
        do (destructuring-bind (cur-symb cur-rgb) (list (first item) (second item))
             (add-color-symbol-mapping cur-symb cur-rgb))))

(defun color-symbol->rgb (symb)
  (gethash symb *color-table*))

(defun rgb->color-symbol (rgb)
  (let ((res-symb
          (loop for cur-symb being the hash-keys of *color-table* using (hash-value cur-rgb)
                when (equal cur-rgb rgb) collect cur-symb)))
    (ecase (length res-symb)
      (0 nil)
      (1 (first res-symb))
      (2 (progn
           (guard ((null (set-difference (list 'gray 'grey) res-symb))))
           'gray)))))

(defun color-symbol->system-color (symb)
  (destructuring-bind (red green blue) (color-symbol->rgb symb)
    (make-color red green blue)))

(defun system-color->symbol (color)
  (let ((red (easygui:rgb-red color))
        (green (easygui:rgb-green color))
        (blue (easygui:rgb-blue color)))
    (rgb->color-symbol (list red green blue))))

(defun make-color (red green blue &optional (opacity 1.0))
  (easygui:make-rgb :red red :green green :blue blue :opacity opacity))

(defun color-red (color)
  (easygui:rgb-red color))

(defun color-green (color)
  (easygui:rgb-green color))

(defun color-blue (color)
  (easygui:rgb-blue color))

(defun color-opacity (color)
  (easygui:rgb-opacity color))

(defun mcl-color-red (color &optional (component (logand (the fixnum (lsh color -16)) #xff)))
  "Returns the red portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun mcl-color-green (color &optional (component (logand (the fixnum (lsh color -8)) #xff)))
  "Returns the green portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun mcl-color-blue (color &optional (component (logand color #xff)))
  "Returns the blue portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun mcl-color-values (color)
  "Given an encoded color, returns the red, green, and blue components"
  (values
    (ceiling (* (/ (float (mcl-color-red color)) (float 65535)) 255))
    (ceiling (* (/ (float (mcl-color-green color)) (float 65535)) 255))
    (ceiling (* (/ (float (mcl-color-blue color)) (float 65535)) 255))))

(defun mcl-color->system-color (color)
  "Converts an MCL color to a CCL system color"
  (etypecase color
    (integer (multiple-value-bind (r g b) (mcl-color-values color)
               (make-color r g b)))
    (ns:ns-color color)))

(defparameter *black-color* (color-symbol->system-color 'black))
(defparameter *red-color* (color-symbol->system-color 'red))
(defparameter *light-gray-pattern* (color-symbol->system-color 'gray))
(defparameter *green-color* (color-symbol->system-color 'green))
(defparameter *blue-color* (color-symbol->system-color 'blue))
(defparameter *dark-green-color* (color-symbol->system-color 'DarkGreen))
(defparameter *white-color* (color-symbol->system-color 'white))
(defparameter *gray-color* (mcl-color->system-color 8421504))
(defparameter *yellow-color* (color-symbol->system-color 'yellow))
(defparameter *orange-color* (mcl-color->system-color 16737282))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :mcl-ccl-colors))



; ----------------------------------------------------------------------
; End file: bincarbon/mcl-ccl-colors.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: actr6/devices/ccl/share.lisp
; ----------------------------------------------------------------------


(require :resources)
(require :mcl-ccl-colors)

; These are shorthand guard macros for usual cases. Only use these if you quickly want to add
; a guard statement with minimal useful error messages. Otherwise, use the guard macro and 
; provide a more meaningful error message

(defmacro guard-!null-ptr (&body body)
  `(guard ((not (equal it1 ccl:+null-ptr+)) "null ptr returned when evaling form ~a" ',body)
     (progn ,@body)))

(defmacro guard-!nil (&body body)
  `(guard (it1 "nil returned when evaling form ~a" ',body)
     (progn ,@body)))

(defmacro guard-nil (&body body)
  `(guard ((null it1) "~a returned when evaling form ~a; expected nil" it1 ',body)
     (progn ,@body)))

(defmacro guard-t-or-nil (&body body)
  `(guard ((or (eq it1 nil) (eq it1 t)) "~a returned when evaling form ~a: expected t or nil" it1 ',body)
     (progn ,@body)))

(defmacro! return-time-ms (&body body)
  `(let ((,g!ctime (get-internal-real-time)))
     ,@body
     (* (/ (- (get-internal-real-time) ,g!ctime) internal-time-units-per-second)
        1000)))

; ----------------------------------------------------------------------
; Building class definitions to match MCL's GUI class hierarchy
;
; Most of the class definitions used by MCL are available in CCL using
; the easygui package. However, a few of the slot initargs in the easygui
; package do not match up with MCL initargs. So for these, use mixin
; classes that override initargs in easygui with initargs that match
; MCL's spec.
; ----------------------------------------------------------------------

(defconstant $tejustleft :left)
(defconstant $tejustcenter :center)
(defconstant $tejustright :right)
(defparameter *screen-width* (easygui::screen-width))
(defparameter *screen-height* (easygui::screen-height))

(defclass view-text-via-title-mixin (easygui::view-text-via-title-mixin)
  ((easygui::text :initarg :window-title)))

(defclass view-text-via-button-title-mixin (view-text-via-title-mixin)
  ())

(defclass view-text-via-stringvalue-mixin (easygui::view-text-via-stringvalue-mixin)
  ())

(defclass view-text-mixin (easygui::view-text-mixin)
  ((text-justification :accessor text-justification :initarg :text-justification))
  (:default-initargs :text-justification $tejustleft))

(defparameter *fred-default-font-spec* '("Monaco" 9 :SRCOR :PLAIN (:COLOR-INDEX 0)))

(defclass view-mixin (easygui::view)
  ((easygui::size :initarg :view-size)
   (easygui::position :initarg :view-position :initform (make-point 0 0))
   (temp-view-subviews :initarg :view-subviews)
   (easygui::foreground :initform (color-symbol->system-color 'black))
   (easygui::background :initform (make-color 0 0 0 0.0)))
  (:default-initargs :view-font *fred-default-font-spec*))

; MCL allows for subviews to be passed at object initialization. I tried shadowing the 'easygui::subviews :initargs symbol
; with :view-subviews, so that MCL code cleanly initialized easygui's subviews slot, but it turns out that this slot isn't always 
; where the subviews are supposed to go. If the view is a window, then the subviews go as subviews under the content-view slot.
; easygui handles all of this in their add-subviews method, so the technique here is to use a temp slot on the view-mixin class,
; make that :initarg :view-subviews, and then on object initialization, take any provided subviews and call easygui's add-subviews method
; on them. Then clear the temp slot. It's a hack, but it seems to work, and requires minimal code additions and still uses
; easygui's add-subviews machinery, etc.

(defmethod initialize-instance :after ((view view-mixin) &key) 
  (when (slot-boundp view 'temp-view-subviews)
    (apply #'add-subviews view (slot-value view 'temp-view-subviews))
    (slot-makunbound view 'temp-view-subviews)))

; Try to keep the class hierarchy of the public interface the same as it is for MCL.
; So, simple-view is top; then view (allows subviews); then types that inherit from view,
; like, window, dialog stuff, etc.

(defclass simple-view (easygui::simple-view view-mixin output-stream pen-mixin)
  ((bezier-path :accessor bezier-path :initform nil)
   (direction :initarg :direction :initform :output)
   (wptr :initarg :wptr :initform nil)
   (help-spec :initarg :help-spec :initform nil)
   (view-scroll-position :initarg :view-scroll-position :initform (make-point 0 0))))

(defmethod view-default-size ((view simple-view))
  (make-point 100 100))

(defmethod parse-mcl-initargs (&rest list)
  (loop for (keyword val . rest) in list
        when val append (apply #'parse-mcl-initarg keyword val rest)))

; easygui expects the font slot to be initialized with an ns-font type. However, MCL uses the
; same slot name and expects the font slot to be initialized with a font spec as a list.
; So in order to make it so that the font slot is correct for easygui, shadow the :view-font
; initarg if it is provided by the equivalent ns-font value

(defmethod initialize-instance :around ((view simple-view) &rest args &key back-color view-font view-size view-position view-scroll-position)
  (let ((accum (parse-mcl-initargs
                 (list :back-color back-color)
                 (list :view-font view-font)
                 (list :view-size view-size)
                 (list :view-position view-position)
                 (list :view-scroll-position view-scroll-position))))
    (apply #'call-next-method view (nconc accum args))))

(defmethod initialize-instance :after ((view simple-view) &key)
  (guard ((cocoa-is-flipped (cocoa-ref view))))
  (guard ((slot-value view 'easygui::flipped)))
  (with-slots (direction wptr view-scroll-position) view
    (guard ((eq direction :output) "only :output for direction slot is allowed"))
    (guard ((eq wptr nil) "only nil for wptr slot is allowed"))
    (guard ((points-equal-p view-scroll-position
                            (make-point 0 0)) "non-(0,0) view-scroll-position is not currently implemented"))))


(defmethod cocoa-is-flipped :around ((self easygui::cocoa-extension-mixin))
  (and (slot-value self 'easygui::flipped)
       (call-next-method)))

(defmethod cocoa-is-flipped ((self ns:ns-view))
  (#/isFlipped self))

(defmethod cocoa-is-flipped ((self t))
  t)

; Parsing MCL initarg lists, and converting to CCL/Easygui equivalents

(defun make-font (name pt)
  (guard ((not (equal it1 ccl:+null-ptr+)) "font not found for font-name ~a" name)
    (#/fontWithName:size: ns:ns-font
     (objc:make-nsstring name)
     pt)))

(defun color-lst->color (lst)
  (destructuring-bind (type val) lst
    (ecase type
      (:color (mcl-color->system-color val))
      (:color-index
        (unless (eq val 0)
          (error "need to support this")
          ; Default, so return nil
          ())))))

(defmethod parse-mcl-initarg ((keyword (eql :view-font)) font-lst &key)
  (let ((name) (pt) (color))
    (dolist (atom font-lst)
      (etypecase atom
        (string (setf name atom))
        (integer (setf pt atom))
        ; FIXME; Parse these style and transfer mode values
        (keyword ())
        (list (setf color (color-lst->color atom)))))
    (nconc
      (list :view-font (make-font name pt))
      (if color
        (list :fore-color color)))))

(defmethod parse-mcl-initarg ((keyword (eql :back-color)) back-color &key)
  (list :back-color (mcl-color->system-color back-color)))

(defmethod parse-mcl-initarg ((keyword (eql :view-size)) view-size &key)
  (list :view-size (mcl-point->system-point view-size)))

(defmethod parse-mcl-initarg ((keyword (eql :view-position)) view-position &key size)
  (list :view-position
        (if size
          (parse-view-position-argument view-position size)
          (mcl-point->system-point view-position))))

(defmethod parse-view-position-argument ((keyword (eql :centered)) size)
  (destructuring-bind (sizex sizey) (as-list (mcl-point->system-point size))
    (make-point (/ (- *screen-width* sizex) 2)
                (/ (- *screen-height* sizey) 2))))

(defmethod parse-view-position-argument ((keyword list) size)
  (guard ((eq t easygui::*screen-flipped*)))
  (destructuring-bind (keyword offset) keyword
    (destructuring-bind (x y) (as-list (parse-view-position-argument :centered size))
      (destructuring-bind (sizex sizey) (as-list (mcl-point->system-point size))
        (ecase keyword
          (:top (setf y offset))
          (:bottom (setf y (- *screen-height*
                              (+ sizey offset))))
          (:left (setf x offset))
          (:right (setf x (- *screen-width*
                             (+ sizex offset)))))
        (make-point x y)))))

(defmethod parse-view-position-argument ((keyword easygui::eg-point) size)
  (declare (ignore size))
  keyword)

(defmethod parse-view-position-argument ((keyword integer) size)
  (declare (ignore size))
  keyword)

(defmethod parse-mcl-initarg ((keyword (eql :view-scroll-position)) view-scroll-position &key)
  (list :view-scroll-position (mcl-point->system-point view-scroll-position)))

(defclass view (simple-view)
  ()
  (:documentation "Top-level class for views"))

(defclass contained-view (easygui::contained-view view)
  ((easygui::background :initform (color-symbol->system-color 'white))))

(defclass static-view-mixin (easygui::static-view-mixin) ())

(defclass window (easygui:window view-text-via-title-mixin view)
  ((grow-icon-p :initform nil :initarg :grow-icon-p :reader grow-icon-p)
   (grow-box-p :initarg :grow-box-p)
   (theme-background :initarg :theme-background)
   (window-show :initarg :window-show)
   (window-type :initarg :window-type)
   (easygui::closable-p :initarg :close-box-p)
   (maintenance-thread :accessor maintenance-thread)
   (initialized-p :accessor initialized-p :initform nil)
   (easygui::background :initform (color-symbol->system-color 'white))
   (close-requested-p :accessor close-requested-p :initform nil)
   (sema-finished-close :accessor sema-finished-close :initform (make-semaphore))
   (sema-request-close :accessor sema-request-close :initform (make-semaphore))
   (window-do-first-click :initarg :window-do-first-click :initform nil)
   (window-other-attributes :initarg :window-other-attributes :initform 0)
   (process :initarg :process :initform nil)
   (auto-position :initarg :auto-position :initform nil))
  (:default-initargs 
    :view-position (make-point 200 200)
    :view-size (make-point 200 200)
    :contained-view-specifically 'contained-view))

(defun process-active-p (p)
  (ccl::process-active-p p))

(defmethod initialize-instance :after ((win window) &key)
  (with-slots (window-do-first-click window-other-attributes process auto-position) win
    (guard ((null window-do-first-click) "non-nil window-do-first-click not currently implemented"))
    (guard ((eq 0 window-other-attributes) "non-zero window-other-attributes not currently implemented"))
    (guard ((null process) "process slot should be nil"))
    (guard ((member auto-position (list nil :noAutoCenter)) "auto-position not currently implemented")))
  (let ((started-sema (make-semaphore)))
    (setf (maintenance-thread win)
          (process-run-function 
            (format nil "maintenance thread for win ~a" win)
            (lambda ()
              (setf (initialized-p win) t)
              (ccl::create-autorelease-pool)
              (signal-semaphore started-sema)
              (while (wptr win)
                (cond ((close-requested-p win)
                       (sv-log "closing ~a on thread ~a~%" win *current-process*)
                       (funcall (window-close-fct win) win)
                       (signal-semaphore (sema-finished-close win)))
                      ((aand (front-window) (eq win it))
                       (window-null-event-handler win)))
                (timed-wait-on-semaphore (sema-request-close win) .1)))))
    (guard-!nil
      (timed-wait-on-semaphore started-sema 1))))

; Give each window a maintenance thread. In that thread,
; periodically check if the window is the frontmost window.
; If it is, call window-null-event-handler on the window. 

; I took a sample of the refresh rate of MCL's
; calls to window-null-event-handler, and it
; was around 100ms. So using that rate here.
(defmethod initialize-instance :around ((win window) &rest args &key view-position view-size)
  (let ((accum (when (and view-position view-size)
                 (parse-mcl-initarg :view-position view-position :size view-size))))
    (apply #'call-next-method win (nconc accum args))))

(objc:defmethod (#/close :void) ((self easygui::cocoa-window))
  (let ((win (easygui::easygui-window-of self)))
    (slot-makunbound win 'easygui::ref)
    (call-next-method)))

(defparameter *window-null-event-handler-lock* (make-lock "window-null-event-handler-lock")) 

(defmethod window-null-event-handler ((win window))
  ())

(defmethod window-null-event-handler :around ((win window))
  (cond ((try-lock *window-null-event-handler-lock*)
         (unwind-protect (call-next-method)
           (release-lock *window-null-event-handler-lock*)))
        (t
         (sv-log "not calling null-event-handler for win ~a because another null-event-handler is active~%" win))))

(defmethod window-close ((win window))
  (unless (wptr win)
    (sv-log "Attempting to close window ~a which is already closed" win)
    (return-from window-close nil))
  (when (close-requested-p win)
    (sv-log "Already requested for window ~a to be closed" win)
    (return-from window-close nil))
  (setf (close-requested-p win) t)
  (signal-semaphore (sema-request-close win))
  (sv-log "requesting to close win ~a on thread ~a~%" win *current-process*)
  (let ((time
          (return-time-ms
            (timed-wait-on-semaphore (sema-finished-close win) .5))))
    (sv-log "waited for ~,2f ms before win ~a was closed by maintenance thread~%" time win)))

(defclass static-contained-view (static-view-mixin contained-view) ())

(defclass static-window (static-view-mixin window)
  ()
  (:default-initargs :contained-view-specifically 'static-contained-view)) 

; You could return easygui::perform-close (which calls #/performClose:), but this doesn't close the window when the window doesn't have a close box.
; So to keep things simple, bypass easygui's perform-close method (so bypass #/performClose:) and go directly to #/close.
(defmethod window-close-fct ((win window))
  (lambda (win) (#/close (cocoa-ref win))))

(defclass windoid (window)
  ((easygui::level :initform 1)
   (easygui::resizable-p :initform nil)
   (easygui::minimizable-p :initform nil)
   (easygui::closable-p :initform nil)))

(defclass borderless-window (window)
  ((easygui::resizable-p :initform nil)
   (easygui::minimizable-p :initform nil)
   (easygui::closable-p :initform nil)
   (easygui::style :initform #$NSBorderlessWindowMask)))

(defmethod windoid-p ((win t))
  nil)

(defmethod windoid-p ((win windoid))
  t)

(defclass simple-overlay-view (easygui::overlay-view view) 
  ()
  (:documentation "Top-level class for views that do not monitor mouse clicks and mouse movement"))

(defclass consuming-view (easygui::consuming-view view)
  ())

(defclass liner (view)
  ((liner-type :reader liner-type :initarg :liner-type)
   (easygui::foreground :reader color :initarg :color)))

(defclass dialog (window)
  ()
  (:default-initargs
    :window-title "Untitled Dialog"
    :window-type :document))

(defclass color-dialog (dialog)
  ()
  (:documentation "Top-level class for windows"))

(defclass action-view-mixin (easygui::action-view-mixin) ())

(defmethod initialize-instance :around ((view action-view-mixin) &rest args &key action)
  (let ((accum (parse-mcl-initargs (list :action action :view view))))
    (apply #'call-next-method view (nconc accum args))))

(defmethod parse-mcl-initarg ((keyword (eql :action)) action &key view)
  (guard ((not (null view)) "view must associated with an action"))
  (list :action
        (lambda ()
          (sv-log-n 1 "calling action for ~a" view)
          (funcall action)
          (sv-log-n 1 "finished calling action for ~a" view))))

(defclass dialog-item (view view-text-mixin action-view-mixin)
  ((easygui::dialog-item-enabled-p :initarg :enabled-p)
   (part-color-list :reader part-color-list :initarg :part-color-list)
   (dialog-item-handle :accessor dialog-item-handle :initarg :dialog-item-handle :initform nil)
   (dialog-item-action :accessor dialog-item-action :initarg :dialog-item-action :initform nil)
   (allow-tabs :reader allow-tabs :initarg :allow-tabs :initform nil)
   (compress-text :accessor compress-text :initarg :compress-text :initform nil))
  (:default-initargs 
    :view-font '("Lucida Grande" 13 :SRCCOPY :PLAIN (:COLOR-INDEX 0))))

; easygui's action slot takes a lambda with zero arguments. MCL's dialog-item-action slots take a lambda 
; with the object/view as an argument. So to enable this feature, wrap a :dialog-item-action 
; initarg function in a closure that takes zero arguments, and assign that clozure to the :action initarg

(defmethod initialize-instance :around ((view dialog-item) &rest args &key dialog-item-action)
  (let ((accum (parse-mcl-initargs
                 (list :dialog-item-action dialog-item-action :view view))))
    (apply #'call-next-method view (nconc accum args))))

(defmethod parse-mcl-initarg ((keyword (eql :dialog-item-action)) action &key view)
  (guard ((not (null view)) "dialog item action must be associated with a view"))
  (list :action (lambda () (funcall action view))))

(defmethod initialize-instance :before ((view dialog-item) &key text)
  (guard ((null text) "Do not use :text initarg; use :dialog-item-text instead")))

(defmethod initialize-instance :after ((view dialog-item) &key)
  (guard ((null (dialog-item-handle view)) "Not utilizing dialog-item-handle"))
  (guard ((null (compress-text view)) "Not utilizing compress-text"))
  (size-to-fit view)
  (when (slot-boundp view 'part-color-list)
    (loop for (part color) in (group (part-color-list view) 2)
          do (set-part-color view part (mcl-color->system-color color)))))

; Note that the :specifically initarg says what cocoa view class to associate with an instance of the object. 
; These really should have been specified in the easygui package, alongside each easygui class definition IMHO, but they weren't.
; Most of the easygui package uses a global easygui::*view-class-to-ns-class-map* variable that contains mappings of lisp
; classes to cocoa view classes, but I found using this flat mapping to be problematic with clos hierarchies. 
; Easygui also provides a :specifically method to overrule the easygui::*view-class-to-ns-class-map* variable, and I like this better, 
; so I'm using it. The benefits of the :specifically method are: 
; [1] cocoa view class mappings are explicitly written, and contained within each clos class definition. 
; [2] As the clos classes are extended, the :specifically values are inherited/over-ridden in the usual way.

(defclass button-dialog-item (easygui:push-button-view view-text-via-button-title-mixin easygui::text-fonting-mixin dialog-item)
  ((easygui::default-button-p :initarg :default-button)
   (cancel-button :initarg :cancel-button))
  (:default-initargs :specifically 'easygui::cocoa-button :text-justification $tejustcenter))

(defmethod get-mutable-title ((view view-text-via-title-mixin))
  (let ((attributed-title (guard-!null-ptr (#/attributedTitle (cocoa-ref view)))))
    (let ((mutable-title (#/alloc ns:ns-mutable-attributed-string)))
      (#/initWithAttributedString: mutable-title attributed-title))))

(defmethod add-to-attributed-title ((view view-text-via-title-mixin) name val)
  (let* ((mutable-title (get-mutable-title view))
         (title-range (ns:make-ns-range 0 (#/length mutable-title))))
    (#/addAttribute:value:range: mutable-title name val title-range)
    (#/setAttributedTitle: (cocoa-ref view) mutable-title)))

(defmethod set-fore-color :before ((view view-text-via-button-title-mixin) (color ns:ns-color))
  (add-to-attributed-title view #$NSForegroundColorAttributeName color))

(defmethod initialize-instance :after ((view view-text-via-button-title-mixin) &key)
  (set-fore-color view (slot-value view 'easygui::foreground)))

(defclass default-button-dialog-item (button-dialog-item)
  ()
  (:default-initargs :dialog-item-text "OK" :default-button t :cancel-button nil))

(defclass static-text-dialog-item (easygui:static-text-view view-text-via-stringvalue-mixin dialog-item)
  ((bordered-p :reader bordered-p)
   (text-truncation :initarg :text-truncation :reader text-truncation :initform #$NSLineBreakByTruncatingTail)
   (easygui::drawsbackground :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-mouseable-text-field))

(defmethod initialize-instance :around ((view static-text-dialog-item) &rest args &key text-truncation)
  (let ((accum (parse-mcl-initargs (list :text-truncation text-truncation))))
    (apply #'call-next-method view (nconc accum args))))

; Default line break mode is to wrap and not tighten. Cocoa has a TighteningFactorForTruncation value that is set to .05.
; And that threshold default is fine, but it doesn't kick in (tighten the text) unless the cell is set to truncate. 

(defmethod initialize-instance :after ((view static-text-dialog-item) &key)
  (awhen (text-truncation view)
    (#/setLineBreakMode: (#/cell (cocoa-ref view)) it)))

(defmethod parse-mcl-initarg ((keyword (eql :text-truncation)) val &key)
  (list
    :text-truncation
    (etypecase val
      (keyword (ecase val
                 (:end #$NSLineBreakByTruncatingTail)))
      (integer val))))

; Cocoa doesn't automatically determine the value of drawsbackground dependent on the background color.
; If the back color is clear, drawsbackground should be nil, otherwise t. So if a back-color is passed in
; as an initform, and that color is not clear, inform easygui that the background should be drawn by passing
; a t for :draws-background keyword.

(defmethod initialize-instance :around ((view easygui::background-coloring-mixin) &rest args &key back-color)
  (if (and back-color
           (not (equalp (color-opacity (mcl-color->system-color back-color))
                        0)))
    (apply #'call-next-method view :draws-background t args)
    (call-next-method)))

(defmethod (setf bordered-p) (bordered-p (view static-text-dialog-item))
  (unwind-protect (setf (slot-value view 'bordered-p) bordered-p)
    (#/setBordered: (easygui:cocoa-ref view) (if bordered-p #$YES #$NO))))

(defclass scroll-bar-dialog-item (easygui::content-view-mixin dialog-item)
  ((scrollee :accessor scrollee :initarg :scrollee)
   (scrollee-class :initarg :scrollee-class)
   (v-scroll-p :reader v-scroll-p :initarg :v-scroll-p))
  (:default-initargs
    :v-scroll-p t
    :specifically 'easygui::cocoa-scroll-view
    :objc-content-view-accessor #'#/documentView))

(defclass editable-text-dialog-item (easygui::editable-mixin easygui::mouse-tracking-mixin scroll-bar-dialog-item) 
  ((allow-returns :initarg :allow-returns)
   (draw-outline :initarg :draw-outline))
  (:default-initargs
    :v-scroll-p nil
    :scrollee-class 'inner-text-view))

(defclass inner-text-view (dialog-item easygui::view-text-via-string-mixin easygui::text-coloring-mixin easygui::text-fonting-mixin)
  ()
  (:default-initargs
    :specifically 'easygui::cocoa-text-view))

(defmethod initialize-instance :around ((view scroll-bar-dialog-item) &rest args &key scrollee scrollee-class)
  (setf (scrollee view)
        (if scrollee
          scrollee
          (apply #'make-instance
                 scrollee-class
                 (getf-include-key args (list :view-size :allow-tabs :view-font :dialog-item-text :text-justification)))))
  ; Not removing :view-size, since the content-view-mixin should make use of this information as well, if available
  (mapc (lambda (indicator) (remf args indicator))
        (list :allow-tabs :view-font :dialog-item-text :text-justification))
  (apply #'call-next-method view :content-view (scrollee view) args))

(defmethod easygui::initialize-view :after ((view scroll-bar-dialog-item))
  (let ((content-view (slot-value view 'easygui::content-view)))
    (#/setDocumentView: (cocoa-ref view) (cocoa-ref content-view))
    (#/setBorderType: (cocoa-ref view) #$NSBezelBorder)
    (setf (slot-value content-view 'easygui::parent) view)))

(defmethod initialize-instance :after ((view scroll-bar-dialog-item) &key v-scroll-p)
  (when v-scroll-p
    (#/setHasVerticalScroller: (cocoa-ref view) #$YES)))

; I couldn't get #/sizeToFit to work for NSTextView objects, so doing things manually in this case
; http://stackoverflow.com/questions/2654580/how-to-resize-nstextview-according-to-its-content

(defmethod size-to-fit ((view inner-text-view))
  (when (and (slot-boundp view 'easygui::text)
             (not (slot-boundp view 'easygui::size)))
    (easygui::running-on-main-thread ()
      (#/setFrameSize: (cocoa-ref view) (ns:make-ns-size 100000 10000)) ; Needed so that #/usedRectForTextContainer: doesn't return a size 0 frame; not sure why
      (let ((container (#/textContainer (cocoa-ref view))))
        (let ((manager (#/layoutManager (cocoa-ref view))))
          (#/ensureLayoutForTextContainer: manager container)
          (let ((frame (#/usedRectForTextContainer: manager container))) ; The frame will be the size required to fit all of the view's current text
            (#/setFrameSize: (cocoa-ref view)
             (ns:make-ns-size (ns:ns-rect-width frame)
                              (ns:ns-rect-height frame)))
            (easygui::size-to-fit view)))))))

(defmethod size-to-fit ((view editable-text-dialog-item))
  (when (and (not (slot-boundp view 'easygui::size))
             (slot-boundp (content-view view) 'easygui::size))
    (set-view-size view (add-points (make-point 5 5) (view-size (content-view view))))))

(defmethod size-to-fit ((view dialog-item))
  (when (and (slot-boundp view 'easygui::text)
             (not (slot-boundp view 'easygui::size)))
    (#/sizeToFit (cocoa-ref view))
    (easygui::size-to-fit view)))

(defmethod cocoa-text-view ((view editable-text-dialog-item))
  (cocoa-ref (content-view view)))

(defclass radio-button-dialog-item (easygui:radio-button-view view-text-via-button-title-mixin dialog-item)
  ((easygui::cluster :initarg :radio-button-cluster)
   (easygui::selected :initarg :radio-button-pushed-p))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass check-box-dialog-item (easygui:check-box-view view-text-via-button-title-mixin dialog-item)
  ((easygui::text :initform ""))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass table-dialog-item (view-text-via-stringvalue-mixin dialog-item)
  ())

(defclass sequence-dialog-item (table-dialog-item)
  ((table-sequence :reader table-sequence :initarg :table-sequence)
   (columns :reader columns :initform 1)
   (table-print-function :accessor table-print-function :initarg :table-print-function :initform nil)
   (rows :reader rows)
   (cell-size :reader cell-size :initarg :cell-size)
   (table-hscrollp :reader table-hscrollp :initarg :table-hscrollp :initform nil)
   (table-vscrollp :reader table-vscrollp :initarg :table-vscrollp :initform t)
   (selection-type :initarg :selection-type))
  (:default-initargs
    :specifically 'easygui::cocoa-matrix
    :table-sequence ()))

(defmethod (setf rows) (new-rows (view sequence-dialog-item))
  (with-slots (columns rows) view
    (unwind-protect (setf rows new-rows)
      (#/renewRows:columns: (cocoa-ref view) rows columns))))

(defmethod set-cell-size ((view sequence-dialog-item) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (with-slots (cell-size) view
      (unwind-protect (setf cell-size (make-point h v))
        (#/setCellSize: (cocoa-ref view)
         (ns:make-ns-size h v)))))) 

(defmethod cell-contents ((view sequence-dialog-item) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (guard ((eq h 0)))
    (nth v (table-sequence view))))

(defmethod set-table-sequence ((view sequence-dialog-item) new-sequence)
  (with-slots (table-sequence table-print-function) view
    (with-accessors ((view-font view-font)) view
      (setf (rows view) (length new-sequence))
      (unwind-protect (setf table-sequence new-sequence)
        (loop for item in new-sequence
              for index from 0
              with cell-array = (#/cells (cocoa-ref view))
              for cell = (#/objectAtIndex: cell-array index)
              do (#/setTitle: cell 
                  (objc::make-nsstring (funcall table-print-function item nil)))
              when view-font do (#/setFont: cell view-font))))))

(defmethod selected-cells ((view sequence-dialog-item))
  (let ((selected-cells))
    (do-array (cell (#/selectedCells (cocoa-ref view)) selected-cells)
      (push-to-end (get-location-of-cell view cell)
                   selected-cells))))

(defmethod get-location-of-cell ((view sequence-dialog-item) (cell ns:ns-cell))
  (rlet ((rownum #>NSInteger)
         (colnum #>NSInteger))
    (#/getRow:column:ofCell: (cocoa-ref view) rownum colnum cell)
    (make-point (pref colnum #>NSInteger)
                (pref rownum #>NSInteger))))

(defmethod initialize-instance :after ((view sequence-dialog-item) &key)
  (let ((cocoa-matrix (cocoa-ref view))
        (prototype (make-instance 'easygui::cocoa-text-field-cell)))
    (with-slots (table-hscrollp table-vscrollp columns) view
      (guard (table-vscrollp "Sequence dialog item must allow vertical scrolling"))
      (guard ((not table-hscrollp) "Sequence dialog item must not allow horizontal scrolling"))
      (guard ((eq columns 1) "Only supporting a single column currently")))
    (unless (table-print-function view)
      (setf (table-print-function view)
            (lambda (item strm) (format strm "~a" item))))
    (unless (slot-boundp view 'cell-size)
      (setf (slot-value view 'cell-size)
            (make-point (point-h (view-size view)) 20)))
    (#/setPrototype: cocoa-matrix prototype)
    (#/setMode: cocoa-matrix #$NSListModeMatrix)
    (#/setIntercellSpacing: cocoa-matrix (ns:make-ns-size 0 0))
    (set-cell-size view (cell-size view))
    (set-table-sequence view (table-sequence view))))

(defclass image-view (easygui::image-view view)
  ((pict-id :reader pict-id :initarg :pict-id)))

(defmethod size-to-fit ((view image-view))
  (let ((view-size
          (cond ((not (slot-boundp view 'easygui::size))
                 (let ((ns-size (#/size (#/image (cocoa-ref view)))))
                   (destructuring-bind (width height) (as-list ns-size)
                     (make-point width height))))
                (t
                 (view-size view)))))
    (set-view-size view view-size)))

(defmethod initialize-instance :after ((view image-view) &key)
  (when (slot-boundp view 'pict-id)
    (setf (pict-id view) (pict-id view)))
  (size-to-fit view))

(defmethod (setf easygui::view-size) :after (size (view image-view))
  (destructuring-bind (width height) (as-list size)
    (#/setSize: (#/image (cocoa-ref view)) (ns:make-ns-size width height))))

(defmethod (setf pict-id) (pict-id (view image-view))
  (unwind-protect (setf (slot-value view 'pict-id) pict-id)
    (#/setImage: (easygui:cocoa-ref view) (get-resource-val pict-id 'image))))

(defclass back-image-view (image-view) ())

(defclass clickable-image-view (easygui::clickable-image-view image-view) ())

(defclass icon-dialog-item (clickable-image-view dialog-item view)
  ((pict-id :reader icon :initarg :icon)
   (easygui::view-text :initarg :view-text)))

; Place all images in the background (behind all other views). Do this by
; specializing on the add-1-subview method in the easygui package. And call
; cocoa's method for adding a subview that is behind all other views

(defmethod easygui::add-1-subview ((view back-image-view) (super-view view))
  (setf (slot-value view 'easygui::parent) super-view)
  (push view (slot-value super-view 'easygui::subviews))
  (#/addSubview:positioned:relativeTo: 
   (easygui:cocoa-ref super-view) 
   (easygui:cocoa-ref view)
   #$NSWindowBelow
   nil))

(provide :icon-dialog-item)

(defun make-dialog-item (class position size text &optional action &rest attributes)
  (apply #'make-instance class
         (nconc
           (list
             :view-position position
             :view-size size
             :dialog-item-text text)
           (if action (list :dialog-item-action action))
           attributes)))

(defclass menu-view (easygui::menu-view view view-text-via-title-mixin easygui::text-fonting-mixin easygui::decline-menu-mixin)
  ((easygui::text :initarg :menu-title)
   (default-item :initarg :default-item :initform 1)
   (auto-update-default :initarg :auto-update-default)
   (item-display :initarg :item-display))
  (:default-initargs :specifically 'easygui::cocoa-pop-up-button))

; FIXME: menu-item-checked isn't being used at all; default-item from menu-view determines which item is checked. Is it worth the time
; to use this slot, and make it so that the char rendered for the checked item can be changed, or also that multiple items can be checked,
; etc.?

(defclass menu-item (easygui::menu-item-view view view-text-via-title-mixin easygui::text-fonting-mixin action-view-mixin easygui::decline-menu-mixin)
  ((easygui::text :initarg :menu-item-title)
   (style :initarg :style)
   (menu-item-checked :initarg :menu-item-checked :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-menu-item))

(defmethod initialize-instance :around ((view menu-view) &rest args &key default-item menu-items)
  (if menu-items
    (apply #'call-next-method view :selection (nth (1- default-item) menu-items) args)
    (call-next-method)))

(defclass pop-up-menu (easygui::pop-up-menu menu-view) ())

; ----------------------------------------------------------------------
; Building methods that allow CCL to understand basic MCL drawing commands

; Many of the functions/methods for basic MCL drawing are available in CCL's 
; easygui package. For the functions, import them into the current package.
; For the methods, add a generic method to the current CCL package
; that calls the generic method in the easygui package. Don't import the 
; generic functions from the easygui package, because this will cause 
; symbol collisions for the generic methods in the current package that are
; already defined (might be because they are an act-r interface method, or
; because they are an already-defined CCL method)
; ----------------------------------------------------------------------

(defun point-h (pt)
  (easygui:point-x pt))

(defun point-v (pt)
  (easygui:point-y pt))

(defun point-x (pt)
  (easygui:point-x pt))

(defun point-y (pt)
  (easygui:point-y pt))

(ccl::register-character-name "UpArrow" #\U+F700)
(ccl::register-character-name "DownArrow" #\U+F701)
(ccl::register-character-name "BackArrow" #\U+F702)
(ccl::register-character-name "ForwardArrow" #\U+F703)
(ccl::register-character-name "CheckMark" #\t)

(defparameter *arrow-cursor* (#/arrowCursor ns:ns-cursor))
(defparameter *crosshair-cursor* (#/crosshairCursor ns:ns-cursor))
(defparameter *i-beam-cursor* (#/IBeamCursor ns:ns-cursor))
(defparameter *black-pattern* 'black-pattern-fixme)

(defun make-point (x y)
  (easygui::point x y :allow-negative-p t))

(defmethod add-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (+ (point-x p1) (point-x p2))
    (+ (point-y p1) (point-y p2))))

(defmethod subtract-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (- (point-x p1) (point-x p2))
    (- (point-y p1) (point-y p2))))

(defmethod as-list ((p easygui::eg-point))
  (list (point-h p)
        (point-v p)))

(defmethod as-list ((r ns:ns-range))
  (list (ns:ns-range-location r)
        (ns:ns-range-length r)))

(defmethod as-list ((r ns:ns-rect))
  (list (ns:ns-rect-width r)
        (ns:ns-rect-height r)
        (ns:ns-rect-x r)
        (ns:ns-rect-y r)))

(defmethod as-list ((s ns:ns-size))
  (list (ns:ns-size-width s)
        (ns:ns-size-height s)))

(defmethod points-equal-p ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (destructuring-bind (x1 y1) (as-list p1)
    (destructuring-bind (x2 y2) (as-list p2)
      (and (eq x1 x2)
           (eq y1 y2)))))

; This reverse engineering was done entirely by pattern matching.
; I need to learn more about how bit shifting works if I want to 
; be completely confident that this implementation is correct.

(defun mcl-point-h (pt)
  (logand (lsh pt 0) #xffff))

(defun mcl-point-v (pt)
  (logand (lsh pt -16) #xffff))

(defun mcl-point->system-point (mcl-point)
  (etypecase mcl-point
    (integer (make-point
               (mcl-point-h mcl-point)
               (mcl-point-v mcl-point)))
    (easygui::eg-point mcl-point)))

(defmethod point-string ((point easygui::eg-point))
  (format nil "#@(~a ~a)" (point-x point) (point-y point)))

(defmethod add-subviews ((view simple-view) &rest subviews)
  (when subviews
    (apply #'easygui:add-subviews view subviews)))

(defmethod remove-subviews ((view simple-view) &rest subviews)
  (when subviews
    (apply #'easygui:remove-subviews view subviews)))

(defmethod subviews ((view simple-view) &optional subview-type)
  (declare (ignore subview-type))
  (easygui:view-subviews view))

(defmethod view-subviews ((view simple-view))
  (easygui:view-subviews view))

(defmethod view-named (name (view simple-view))
  (acond ((easygui:view-named name view)
          it)
         (t
           (sv-log "no subview with view-nick-name ~a found in ~a" name view)
           nil)))

(defmethod find-named-sibling ((view simple-view) name)
  (let ((container (view-container view)))
    (and container (view-named name container))))

(defmethod view-nick-name ((view simple-view))
  (easygui:view-nick-name view))

(defmethod window-select ((win window))
  (easygui:window-show win))

(defmethod window-show ((win window))
  (easygui:window-show win))

(defmethod window-hide ((win window))
  (easygui::window-hide win))

(defmethod window-shown-p ((window window))
  (not (easygui::window-hidden window)))

(defun find-window (title &optional class)
  (let ((title (format nil "~a" title)))
    (do-array (cocoa-win (#/windows (#/sharedApplication ns:ns-application)))
      (when (easygui::cocoa-win-p cocoa-win)
        (let* ((wintitle (objc:lisp-string-from-nsstring (#/title cocoa-win)))
               (clos-win (easygui::easygui-window-of cocoa-win))
               (winclass (class-name (class-of clos-win)))
               (prefix (subseq wintitle 0 (min (length wintitle) (length title)))))
          (when (string-equal prefix title)
            (when (or (not class) (eq class winclass))
              (return-from find-window clos-win)))))))
  nil)

(defun front-window (&key class include-invisibles include-windoids)
  (guard ((null include-invisibles)))
  (when class 
    (when (symbolp class)
      (setq class (find-class class)))
    (when (class-inherit-from-p class (find-class 'windoid))
      (setq include-windoids t)))
  (let ((wins (gui::windows)))
    (dolist (win wins)
      (when (easygui::cocoa-win-p win)
        (let ((wob (easygui::easygui-window-of win)))
          (when (and wob
                     (initialized-p wob)
                     (or include-windoids
                         (not (windoid-p wob)))
                     (or (null class)
                         (inherit-from-p wob class)))
            (return wob)))))))

(defun inherit-from-p (ob parent)
  (ccl::inherit-from-p ob parent))

(defun class-inherit-from-p (class parent-class)
  (flet ((get-class (value)
           (if (symbolp value) (find-class value nil) value)))
    (let ((pclass (get-class parent-class)))
      (memq pclass
        (ccl::%inited-class-cpl (get-class class))))))

;FIXME: This looks very strange. Prob related to Phaser's floating window
(defun ccl::window-bring-to-front (w &optional (wptr (wptr w)))
  #-:sv-dev (declare (ignore wptr))
  nil)

(defmethod set-window-layer ((window window) new-layer &optional include-invisibles)
  #-:sv-dev (declare (ignore new-layer include-invisibles))
  'fixme)

(defmethod window-title ((view window))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defmethod dialog-item-text ((view view-text-mixin))
  (easygui:view-text view))

(defmethod set-dialog-item-text ((view view-text-mixin) text)
  (easygui::running-on-main-thread ()
    (setf (easygui:view-text view) text)))

(defmethod easygui::view-text ((view editable-text-dialog-item))
  (easygui::view-text (content-view view)))

(defmethod (setf easygui::view-text) (new-text (view editable-text-dialog-item))
  (setf (easygui::view-text (content-view view)) new-text))

(defmethod text-just ((view view-text-mixin))
  (text-justification view))

(defun convert-justification (justification)
  (let ((mapping (list (cons $tejustleft #$NSLeftTextAlignment)
                       (cons nil #$NSLeftTextAlignment)
                       (cons $tejustcenter #$NSCenterTextAlignment)
                       (cons $tejustright #$NSRightTextAlignment))))
    (guard (it1 "No mapping found for justification ~a" justification)
      (cdr (assoc justification mapping)))))

(defmethod set-text-justification ((view view-text-mixin) justification)
  (#/setAlignment: (easygui:cocoa-ref view) (convert-justification justification))
  (setf (text-justification view) justification))

(defmethod initialize-instance :after ((view view-text-mixin) &key)
  (when (slot-boundp view 'text-justification)
    (set-text-justification view (text-justification view))))

(defmethod selection-range ((view easygui::content-view-mixin))
  (selection-range (content-view view)))

(defmethod selection-range ((view simple-view))
  (destructuring-bind (start length) (as-list (#/selectedRange (cocoa-ref view)))
    (values start (+ start length))))

(defmethod set-selection-range ((view easygui::content-view-mixin) &optional position cursorpos)
  (set-selection-range (content-view view) position cursorpos))

(defmethod set-selection-range ((view view-text-mixin) &optional position cursorpos)
  (destructuring-bind (position cursorpos) (if position
                                             (list position cursorpos)
                                             (list 0 0))
    (easygui::running-on-main-thread ()
      (#/setSelectedRange: (cocoa-ref view)
       (ns:make-ns-range position (- cursorpos position))))))

(defmethod set-selection-range :after ((view editable-text-dialog-item) &optional position cursorpos)
  (declare (ignore position cursorpos))
  (easygui::running-on-main-thread ()
    (#/becomeFirstResponder (cocoa-ref view))))

(defmethod dialog-item-enable ((view action-view-mixin))
  (easygui:set-dialog-item-enabled-p view t))

(defmethod dialog-item-disable ((view action-view-mixin))
  (easygui:set-dialog-item-enabled-p view nil))

; FIXME: What now to do about these, since etdi is a text-view, which is not an NSControl

(defmethod dialog-item-enable ((view editable-text-dialog-item))
  ())

(defmethod dialog-item-disable ((view editable-text-dialog-item))
  ())

(defmethod check-box-check ((item check-box-dialog-item))
  (easygui:check-box-check item nil))

(defmethod check-box-uncheck ((item check-box-dialog-item))
  (easygui:check-box-uncheck item nil))

(defmethod check-box-checked-p ((item check-box-dialog-item))
  (easygui:check-box-checked-p item))

(defmethod radio-button-unpush ((item radio-button-dialog-item))
  (easygui:radio-button-deselect item))

(defmethod radio-button-push ((item radio-button-dialog-item))
  (easygui:radio-button-select item))

(defmethod radio-button-pushed-p ((item radio-button-dialog-item))
  (easygui:radio-button-selected-p item))

(defmethod view-position ((view simple-view))
  (easygui:view-position view))

(defmethod view-position :before ((window window))
  (let ((frame (#/frame (cocoa-ref window))))
    (let ((position 
            (make-point
              (ns:ns-rect-x frame)
              (easygui::convert-if-screen-flipped
                (ns:ns-rect-y frame)
                (point-y (view-size window))))))
      (setf (slot-value window 'easygui::position) position))))

(defmethod view-global-position ((view simple-view))
  (local-to-global view (make-point 0 0)))

(defmethod view-center ((view simple-view))
  (destructuring-bind (x y) (as-list (view-position view))
    (destructuring-bind (sizex sizey) (as-list (view-size view))
      (make-point (+ x (/ sizex 2))
                  (+ y (/ sizey 2))))))

(defmethod view-global-center ((view simple-view))
  (local-to-global
    view
    (subtract-points (view-center view) (view-position view))))

; FIXME: This seems to work properly, but I don't currently understand why,
; or what view-origin is supposed to do in MCL
(defmethod view-origin ((view simple-view))
  (let ((bounds (#/bounds (cocoa-ref view))))
    (make-point (ns:ns-rect-x bounds)
                (ns:ns-rect-y bounds))))

(defmethod origin ((view simple-view))
  (view-origin view))

(defmethod set-origin ((view simple-view) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (#/setBoundsOrigin: (cocoa-ref view) (ns:make-ns-point h v))))

; Note that this is MCL's arglist spec. The erase-p isn't needed for CCL,
; but it should be kept here so that MCL code calling invalidate-view still works.
(defmethod invalidate-view ((view simple-view) &optional erase-p)
  (declare (ignore erase-p))
  (easygui:invalidate-view view))

; FIXME: What is validate-view supposed to do differently than invalidate-view?
; And is that difference already handled within cocoa?
(defmethod validate-view ((view simple-view))
  (invalidate-view view))

(defun canonicalize-point (x y)
  (cond (y (list x y))
        (t (list (point-h x) (point-v x)))))

(defmethod set-view-position ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((pos (make-point x y)))
      (setf (easygui:view-position view) pos))))

(defmethod set-view-size ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((size (make-point x y)))
      (setf (easygui:view-size view) size))))

(defmethod view-size ((view simple-view))
  (easygui:view-size view))

(defmethod width ((view simple-view))
  (point-h (view-size view)))

(defmethod height ((view simple-view))
  (point-v (view-size view)))

(defmethod view-container ((view simple-view))
  (easygui:view-container view))

(defmethod view-window ((view simple-view))
  (easygui::easygui-window-of view))

(defmethod content-view ((view easygui::content-view-mixin))
  (easygui:content-view view))

(defmethod content-view ((view simple-view))
  view)

; Other MCL drawing methods are not available in the easygui package.
; For these, move down a layer below easygui, and implement the functionality
; using CCL's Objective C bridge. Most bridge calls will have #/ or #_ reader
; macros in the expression

; A few with- macros to handle setup/teardown, and make programming a bit easier

; This one uses Doug Hoyte's "defmacro!" and ",g!" syntax to easily handle unwanted variable capture. 
(defmacro! with-graphics-context (&body body)
  "Any changes to the graphics environment by body, will be valid only in body"
  `(let ((,g!context (#/currentContext ns:ns-graphics-context)))
     (unwind-protect (progn 
                       (#/saveGraphicsState ,g!context)
                       ,@body)
       (#/restoreGraphicsState ,g!context))))

; ----------------------------------------------------------------------
; Section to handle current focused view and font focused view.
; 
; The dynamic variables are used to keep track of any views that are focused in the dynamic environment.
; Code could call with-focused-view explicitly, or a view might become focused because code called paint-rect
; and passed a view to that function. The goal is to have the code do the right thing and try to
; figure out which view has focus. If it can't figure this out, then an exception will be thrown.
; These can be seen where the guard macros are used.
; ----------------------------------------------------------------------

(defparameter *current-graphics-context-stroke-color* nil)

(defmacro! with-fore-color (o!color &body body)
  `(progn
     (guard ((eq (type-of ,g!color) 'ns:ns-color) "color ~a is not a system color" ,g!color) ())
     (let ((*current-graphics-context-stroke-color* ,g!color))
       (with-graphics-context
         (#/set ,g!color)
         ,@body))))

(defmacro with-fallback-fore-color (color &body body)
  `(if (null *current-graphics-context-stroke-color*)
     (with-fore-color ,color
       ,@body)
     (progn ,@body)))

(defmacro with-window-fallback-fore-color (view &body body)
  `(with-fallback-fore-color (get-fore-color (view-window ,view))
     ,@body))

(defparameter *current-focused-view* nil)
(defparameter *current-font-view* nil)
(defparameter *current-graphics-context-font* nil)

(defmacro! with-focused-view (o!view &body body)
  "Any changes to the graphics environment by body will be directed to the view object"
  `(let ((*current-focused-view* (content-view ,g!view)))
     (easygui:with-focused-view (easygui:cocoa-ref (content-view ,g!view))
       ,@body)))

(defmacro! with-font-view (o!view &body body)
  `(let ((*current-font-view* ,g!view))
     ,@body))

(defmacro! with-font-focused-view (o!view &body body)
  `(with-font-view ,g!view
     (with-focused-view ,g!view
       ,@body)))

(defmacro with-fallback-focused-view (view &body body)
  `(if (null *current-focused-view*)
     (with-focused-view ,view
       ,@body)
     (progn ,@body)))

(defmacro with-fallback-font-view (view &body body)
  `(if (null *current-font-view*)
     (with-font-view ,view
       ,@body)
     (progn ,@body)))

(defmacro! with-fallback-font-focused-view (o!view &body body)
  `(with-fallback-font-view ,g!view
     (with-fallback-focused-view ,g!view
       ,@body)))

(defmacro with-window-of-focused-view-fallback-fore-color (&body body)
  `(with-window-fallback-fore-color (guard-!nil *current-focused-view*)
     ,@body))

(defmacro with-font-view-fallback-font (&body body)
  `(let ((*current-graphics-context-font* (view-font (guard-!nil *current-font-view*))))
     ,@body))

(defmethod wptr ((view window))
  (slot-boundp view 'easygui::ref))

(defmethod local-to-global ((win window) local-pos)
  (let ((eg-point (add-points (view-position win) local-pos)))
    eg-point))

(defmethod local-to-global ((view simple-view) local-pos)
  (let* ((ns-converted-point (#/convertPoint:fromView:
                              (cocoa-ref (content-view (view-window view)))
                              (ns:make-ns-point (point-h local-pos) (point-v local-pos))
                              (cocoa-ref view))))
    (let ((eg-point (easygui::point-from-ns-point ns-converted-point)))
      eg-point)))

(defmethod part-color ((view view-text-mixin) (part (eql :text)))
  (declare (ignore part))
  (get-fore-color view))

(defmethod set-part-color ((view dialog-item) (part (eql :body)) new-color)
  (set-back-color view new-color))

(defmethod set-part-color ((view dialog-item) (part (eql :text)) new-color)
  (set-fore-color view new-color))

; FIXME: Keep this as a compiler warning until you figure out how to color a border with Cocoa
(defmethod set-part-color ((view dialog-item) (part (eql :frame)) new-color)
  #-:sv-dev (declare (ignore new-color))
  (setf (bordered-p view) t))

(defmethod get-fore-color ((view simple-view))
  (easygui:get-fore-color view))

(defmethod get-back-color ((view view))
  (easygui:get-back-color view))

(defmethod set-fore-color ((view simple-view) new-color)
  (easygui:set-fore-color view new-color))

(defmethod set-back-color ((view simple-view) new-color)
  (easygui:set-back-color view new-color))

; FIXME: What does this do? Keep as compiler warning until you figure it out
(defmethod window-update-cursor ((window window) point)
  #-:sv-dev (declare (ignore point))
  nil)

; Handling mouse movement/interaction

(defmethod easygui::mouse-down :after ((view simple-view) &key location &allow-other-keys)
  (let ((win (guard-!nil
               (guard-!null-ptr
                 (view-window view)))))
    (let ((global-location (local-to-global view location)))
      (view-click-event-handler view location)
      (view-click-event-handler win global-location)
      (post-view-click-event-handler win global-location)))) 

(defmethod post-view-click-event-handler ((view window) position)
  (declare (ignore position))
  (values))

(defmethod view-click-event-handler :around ((device simple-view) position)
  (declare (ignore position))
  (sv-log-n 1 "starting view-click-event-handler for ~a" device)
  (unwind-protect (call-next-method)
    (sv-log-n 1 "ending view-click-event-handler for ~a" device)))

(defmethod view-click-event-handler ((device simple-view) position)
  (declare (ignore position))
  ; Default primary method is to do nothing
  (values))

(objc:defmethod (#/reflectScrolledClipView: :void) ((self easygui::cocoa-scroll-view) (clip :ID))
  (call-next-method clip)
  (let ((view (easygui::easygui-view-of self)))
    (view-scroll-event-handler view) 
    (let ((win (#/window self)))
      (unless (ccl:%null-ptr-p win)
        (let ((win (easygui::easygui-window-of win)))
          (view-scroll-event-handler win)
          (post-view-scroll-event-handler win))))))

(defmethod view-scroll-event-handler ((view simple-view))
  ())

(defmethod post-view-scroll-event-handler ((view simple-view))
  ())

(defmethod view-mouse-position ((view simple-view))
  (easygui:view-mouse-position view :allow-negative-position-p t))

(defparameter *default-scroll-speed* 5)

(defun create-scroll-event (speed)
  (let ((event
          (#_CGEventCreateScrollWheelEvent
           ccl:+null-ptr+
           #$kCGScrollEventUnitLine
           1
           speed)))
    event))

(defun scroll-mouse-up (&optional (speed *default-scroll-speed*))
  (guard ((>= speed 0)))
  (let ((event (create-scroll-event speed)))
    (#_CGEventPost 0 event)
    (#_CFRelease event)))

(defun scroll-mouse-down (&optional (speed *default-scroll-speed*))
  (guard ((>= speed 0)))
  (let ((event (create-scroll-event (* -1 speed))))
    (#_CGEventPost 0 event)
    (#_CFRelease event)))

(defun create-mouse-event (event pos)
  (#_CGEventCreateMouseEvent
   ccl:+null-ptr+
   event
   pos
   0))

(defun left-mouse-up (pos)
  (let ((event
          (create-mouse-event #$NSLeftMouseUp pos)))
    (sv-log-n 1 "posting mouse-up event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing mouse-up event ~a" event)
    (#_CFRelease event)))

(defun left-mouse-down (pos)
  (let ((event
          (create-mouse-event #$NSLeftMouseDown pos)))
    (sv-log-n 1 "posting mouse-down event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing mouse-down event ~a" event)
    (#_CFRelease event)))

; It takes roughly 1 ms for an event to hit the application's run loop, so sleep for 50x 
; longer than this, to make extra extra sure that the event has hit the run loop before returning.

(defun left-mouse-click (pos &optional (delay t))
  (sv-log-n 1 "starting left mouse click")
  (easygui::running-on-main-thread ()
    (let ((pos (easygui::ns-point-from-point pos)))
      (left-mouse-down pos)
      (left-mouse-up pos)))
  (sv-log-n 1 "sleeping so that mouse click enters nsrun loop")
  (when delay (spin-for-fct 50))
  (sv-log-n 1 "ending left mouse click"))

; Handling keyboard interaction

(defun create-keyboard-event (event key)
  (let ((key (format nil "~a" key)))
    (guard ((eq (length key) 1) "key: ~a is not a single character; not supporting command/control key events" key) ())
    (let ((ret (#_CGEventCreateKeyboardEvent
                ccl:+null-ptr+
                0
                event)))
      (#_CGEventKeyboardSetUnicodeString
       ret
       (length key)
       (#/cStringUsingEncoding: (objc:make-nsstring key) #$NSUTF8StringEncoding))
      ret)))

(defun keypress-down (key)
  (let ((event 
          (create-keyboard-event #$YES key)))
    (sv-log-n 1 "posting keypress-down event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing keypress-down event ~a" event)
    (#_CFRelease event)))

(defun keypress-up (key)
  (let ((event 
          (create-keyboard-event #$NO key)))
    (sv-log-n 1 "posting keypress-up event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing keypress-up event ~a" event)
    (#_CFRelease event)))

; Same sleep time here.

(defun keypress (key &optional (delay t))
  (sv-log-n 1 "starting keypress")
  (easygui::running-on-main-thread ()
    (keypress-down key)
    (keypress-up key))
  (sv-log-n 1 "sleeping so that keypress enters nsrun loop")
  (when delay (spin-for-fct 50))
  (sv-log-n 1 "ending keypress"))

; Relay keypress events in editable text views to the view and the window, after allowing the text field to handle the keypress properly.

; http://stackoverflow.com/questions/2484072/how-can-i-make-the-tab-key-move-focus-out-of-a-nstextview
(objc:defmethod (#/doCommandBySelector: :void) ((cocoa-self easygui::cocoa-text-view) (selector :<SEL>))
  (unless (cond ((ccl::%ptr-eql selector (ccl::@selector #/insertTab:))
                 (select-next-key-view (easygui::easygui-view-of cocoa-self)))
                ((ccl::%ptr-eql selector (ccl::@selector #/insertBacktab:))
                 (select-prev-key-view (easygui::easygui-view-of cocoa-self))))
    (call-next-method selector)))

(defmethod select-next-key-view ((view dialog-item))
  (unless (allow-tabs view)
    (select-next-key-view (view-window view))))

(defmethod select-next-key-view ((win window))
  (unwind-protect t
    (#/selectNextKeyView: (cocoa-ref win) ccl:+null-ptr+)))

(defmethod select-prev-key-view ((view dialog-item))
  (unless (allow-tabs view)
    (select-prev-key-view (view-window view))))

(defmethod select-prev-key-view ((win window))
  (unwind-protect t
    (#/selectPreviousKeyView: (cocoa-ref win) ccl:+null-ptr+)))

; http://superuser.com/questions/473143/how-to-tab-between-buttons-on-an-mac-os-x-dialog-box
; Tabbing does not cycle through buttons by default, but this can be changed in system preferences
; If this is changed and an NSButton becomes key, then (currently) the NSButton does not respond by pressing a tab.
; As a workaround subclass the #/keyDown: method and explicitly cycle through previous and next views if tab or backtab is pressed.
; I wouldn't think that you would need to explicitly write this method, but I can't find the setting/issue with the NSButtons
; to make this behavior default.

; I could not get #/setKeyEquivalent: to work for NSButtons, so as a workaround,
; explicitly call #/performClick: when the user presses the #\space key.
; This allows the user to navigate the UI by using the keyboard only, and also 
; press buttons with the keyboard. The #\space as the action button seems to be 
; the fairly consistent technique across OS X Cocoa apps and web-browser apps

(defparameter *view-of-keypress* nil)

(objc:defmethod (#/keyDown: :void) ((cocoa-self easygui::cocoa-button) the-event)
  (let ((*view-of-keypress* (easygui::easygui-view-of cocoa-self)))
    (unwind-protect (call-next-method the-event)
      (let* ((str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers the-event)))
             (char (char str 0)))
        (case char
          (#\tab (select-next-key-view (easygui::easygui-view-of cocoa-self)))
          (#\^Y (select-prev-key-view (easygui::easygui-view-of cocoa-self)))
          (#\space (#/performClick: cocoa-self ccl:+null-ptr+)))))))

(objc:defmethod (#/keyDown: :void) ((cocoa-self easygui::cocoa-text-view) the-event)
  (call-next-method the-event)
  (handle-keypress-in-editable-text
    (view-container
      (easygui::easygui-view-of cocoa-self))
    the-event))

(defmethod handle-keypress-in-editable-text ((view dialog-item) the-event)
  (let ((*view-of-keypress* view))
    (#/keyDown: (#/window (cocoa-ref view)) the-event)))

; #/keyDown: method on cocoa-window calls easygui::view-key-event-handler on the window (see views.lisp in easygui)

; The default first responder when the window is created is the reference to the window
; If the user presses the #\tab key, select the next key view in the responder loop
; This technique usually does the right thing, for instance, it will select the top-most
; editable text in the view after the first tab press

(defmethod easygui::view-key-event-handler :after ((device window) key)
  (when (eq key #\tab)
    (when (equal (cocoa-ref device) (#/firstResponder (cocoa-ref device)))
      (select-next-key-view device)))
  (when *view-of-keypress*
    (view-key-event-handler *view-of-keypress* key))
  (view-key-event-handler device key)
  (post-view-key-event-handler device key))

(defmethod post-view-key-event-handler ((device window) key)
  (declare (ignore key))
  (values))

(defmethod view-key-event-handler :around ((device simple-view) key)
  (declare (ignore key))
  (sv-log-n 1 "starting view-key-event-handler")
  (unwind-protect (call-next-method)
    (sv-log-n 1 "ending view-key-event-handler")))

(defmethod view-key-event-handler ((device simple-view) key)
  (declare (ignore key))
  ; Default primary method on the window is to do nothing
  (values))

; MCL's Pen

(defclass pen-mixin ()
  ((pen-mode :accessor pen-mode)
   (pen-size :accessor pen-size)
   (pen-position :accessor pen-position :initform (make-point 0 0))
   (pen-pattern :accessor pen-pattern)))

(defmethod initialize-instance :after ((view pen-mixin) &key)
  (pen-normal view))

(defmethod set-pen-mode ((view simple-view) newmode)
  (setf (pen-mode view) newmode))

(defmethod set-pen-pattern ((view simple-view) newpattern)
  (setf (pen-pattern view) newpattern))

(defmethod set-pen-size ((view simple-view) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (setf (pen-size view) (make-point h v))))

(defmethod pen-normal ((view simple-view))
  (setf (pen-mode view) :patCopy)
  (setf (pen-size view) (make-point 1 1))
  (setf (pen-pattern view) *black-pattern*))

; ----------------------------------------------------------------------
; Triggering MCL's view-draw-contents method on a Cocoa redraw of views.
;
; CCL's Objective C bridge provides an interface to define objective c 
; methods. Use this to define a method that will be called any time
; an object on the screen needs to be redrawn. This will in turn call
; view-draw-contents in CCL, which means that the way to describe how an 
; object is drawn in CCL is the same way that it is in MCL: Add a view-draw-contents
; method that dispatches on the object type, and code to draw that type of object
; to the display
; ----------------------------------------------------------------------


; Note that Cocoa focuses the view before calling #/drawRect, so there's no reason to have a
; with-focused-view inside of the lisp code. But in order for the with-fallback-focused-view stuff to 
; work, it needs to know that a view is already focused. So just set the global var to the view
; in order to do this.

(objc:defmethod (#/drawRect: :void) ((self easygui::cocoa-drawing-view) (rect :<NSR>ect))
  (let* ((view (easygui::easygui-view-of self))
         (*current-focused-view* view))
    (easygui::dcc
      (view-draw-contents view))))

; Drawing methods

(defmacro with-rectangle-arg ((var left &optional top right bottom) &body body)
  `(let ((,var (make-rect :from-mcl-spec ,left ,top ,right ,bottom)))
     ,@body))

(defmethod make-rect ((mode (eql :from-mcl-spec)) &rest args)
  (destructuring-bind (left top right bottom) args
    (destructuring-bind (left top right bottom) (canonicalize-rect left top right bottom)
      (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
        (ns:make-ns-rect startx starty width height)))))

(defun canonicalize-rect (left top right bottom)
  (cond (bottom (list left top right bottom))
        (top (list (point-h left)
                   (point-v left)
                   (point-h top)
                   (point-v top)))
        (t (list (ns:ns-rect-x left)
                 (ns:ns-rect-y left)
                 (+ (ns:ns-rect-x left) (ns:ns-rect-width left))
                 (+ (ns:ns-rect-y left) (ns:ns-rect-height left))))))

(defmethod view-draw-contents ((view simple-view))
  ())

(defmethod get-start ((view liner))
  (get-start-using-liner-type view (liner-type view)))

(defmethod get-end ((view liner))
  (get-end-using-liner-type view (liner-type view)))

(defmethod get-start-using-liner-type ((view liner) (liner-type (eql 'bu)))
  (make-point 0 (point-y (view-size view))))

(defmethod get-start-using-liner-type ((view liner) (liner-type (eql 'td)))
  (make-point 0 0))

(defmethod get-end-using-liner-type ((view liner) (liner-type (eql 'bu)))
  (make-point (point-x (view-size view)) 0))

(defmethod get-end-using-liner-type ((view liner) (liner-type (eql 'td)))
  (view-size view))

(defmethod view-draw-contents ((view liner))
  (move-to view (get-start view))
  (with-fore-color (get-fore-color view) 
    (line-to view (get-end view))))

; Drawing commands on windows are directed to the window's content view.
; This is achieved by having with-focused-view focus on the window's content
; view (if it's a window), and also by specializing on the accessors that are 
; used when drawing. The window's pen-position and bezier-path are never used;
; instead, those are directed to the content view of the window.
;
; Another approach would have been to write a specialized method for the window
; for each public drawing method, and have that method call the method with the
; same name on the window's content view, but this would require adding a 
; window-specialized method for each public drawing method. So instead I looked
; at what accessors the public methods are using, and specialized on those, so that
; the necessary code changes for drawing to window's content view could be isolated
; in the few methods below. Adding/removing this functionality can be archived by
; adding/deleting the few methods here.

(defmethod pen-position ((view window))
  (pen-position (content-view view)))

(defmethod (setf pen-position) (new (view window))
  (setf (pen-position (content-view view)) new))

(defmethod bezier-path ((view window))
  (bezier-path (content-view view)))

(defmethod (setf bezier-path) (new (view window))
  (setf (bezier-path (content-view view)) new))

; Actual drawing methods

(defmethod move-to ((view simple-view) x &optional (y nil))
  (with-fallback-focused-view view
    (destructuring-bind (x y) (canonicalize-point x y)
      (qd-move-to x y))))

(defmethod qd-move-to ((val1 (eql :long)) (val2 easygui::eg-point))
  (destructuring-bind (endx endy) (as-list val2)
    (qd-move-to endx endy)))

(defmethod qd-move-to ((x number) (y number))
  (let ((view *current-focused-view*))
    (unless (points-equal-p (pen-position view) (make-point x y))
      (let ((position (make-point x y)))
        (when (bezier-path view)
          (#/moveToPoint: (bezier-path view) (ns:make-ns-point x y)))
        (setf (pen-position view) position)))))

(defmethod line ((view simple-view) x &optional (y nil))
  (with-fallback-focused-view view
    (destructuring-bind (x y) (canonicalize-point x y)
      (line-to view (add-points
                      (pen-position view)
                      (make-point x y))))))

(defmethod line-to ((view simple-view) x &optional (y nil))
  (with-fallback-focused-view view
    (destructuring-bind (endx endy) (canonicalize-point x y)
      (qd-line-to endx endy))))

(defmethod qd-line-to ((val1 (eql :long)) (val2 easygui::eg-point))
  (destructuring-bind (endx endy) (as-list val2)
    (qd-line-to endx endy)))

(defmethod qd-line-to ((endx number) (endy number))
  (let ((view *current-focused-view*))
    (unless (points-equal-p (pen-position view) (make-point endx endy))
      (destructuring-bind (startx starty) (list (point-x (pen-position view))
                                                (point-y (pen-position view)))
        (when (bezier-path view)
          (#/lineToPoint: (bezier-path view) (ns:make-ns-point endx endy)))
        (setf (pen-position view) (make-point endx endy))
        (with-window-of-focused-view-fallback-fore-color
          (#/strokeLineFromPoint:toPoint:
           ns:ns-bezier-path
           (ns:make-ns-point startx starty) 
           (ns:make-ns-point endx endy)))))))

(defmethod frame-oval ((view simple-view) left &optional top right bottom)
  (let* ((rect (make-rect :from-mcl-spec left top right bottom))
         (path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
    (with-fallback-focused-view view
      (with-window-of-focused-view-fallback-fore-color
        (#/stroke path)))))

(defmethod paint-oval ((view simple-view) left &optional top right bottom)
  (with-fallback-focused-view view
    (fill-oval view (pen-pattern view) left top right bottom)))

(defmethod fill-oval ((view simple-view) pattern left &optional top right bottom)
  #-:sv-dev (declare (ignore pattern))
  (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
    (with-fallback-focused-view view
      (qd-paint-oval rect))))

(defmethod qd-paint-oval ((rect ns:ns-rect))
  (let ((path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
    (with-window-of-focused-view-fallback-fore-color
      (#/fill path))))

(defmethod frame-rect ((view simple-view) left &optional top right bottom)
  (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
    (with-fallback-focused-view view
      (qd-frame-rect rect))))

(defmethod qd-frame-rect ((rect ns:ns-rect))
  (with-window-of-focused-view-fallback-fore-color
    (#/strokeRect: ns:ns-bezier-path rect)))

(defmethod paint-rect ((view simple-view) left &optional top right bottom)
  (with-fallback-focused-view view
    (fill-rect view (pen-pattern view) left top right bottom)))

(defmethod fill-rect ((view simple-view) pattern left &optional top right bottom)
  (with-fallback-focused-view view
    (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
      (qd-paint-rect rect pattern))))

(defmethod qd-paint-rect ((rect ns:ns-rect) &optional pattern)
  #-:sv-dev (declare (ignore pattern))
  (with-window-of-focused-view-fallback-fore-color
    (#/fillRect: ns:ns-bezier-path rect)))

(defmethod erase-rect ((view simple-view) left &optional top right bottom)
  (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
    (with-fallback-focused-view view
      (with-fore-color (get-back-color (content-view view))
        (qd-paint-rect rect)))))

(defmethod start-polygon ((view simple-view))
  (setf (bezier-path view) (#/bezierPath ns:ns-bezier-path))
  (#/retain (bezier-path view))
  (#/moveToPoint: (bezier-path view)
   (easygui::ns-point-from-point (pen-position view))))

(defun pattern->system-color (pattern)
  (color-symbol->system-color
    (guard-!nil
      (cond ((eq pattern *black-pattern*) 'black)))))

(defmethod paint-polygon ((view simple-view) polygon)
  (with-fallback-focused-view view
    (fill-polygon view (pen-pattern view) polygon)))

(defmethod erase-polygon ((view simple-view) polygon)
  (with-fallback-focused-view view 
    (with-fore-color (get-back-color (content-view view))
      (fill-polygon view (pen-pattern view) polygon))))

(defmethod fill-polygon ((view simple-view) pattern polygon)
  #-:sv-dev (declare (ignore pattern))
  (with-fallback-focused-view view
    (with-window-of-focused-view-fallback-fore-color
      (#/fill polygon))))

(defmethod frame-polygon ((view simple-view) polygon)
  (with-fallback-focused-view view
    (with-window-of-focused-view-fallback-fore-color
      (#/stroke polygon))))

(defmethod kill-polygon ((polygon ns:ns-bezier-path))
  (#/release polygon)
  (setf polygon nil))

(defmethod get-polygon ((view simple-view))
  (prog1 (bezier-path view)
    (start-polygon view)))

; FIXME: Currently it's expected that a format call to a view is done only once per view-draw-contents. So write
; a single string to the view, etc. But CCL calls write-char when the string has a negative sign at the beginning.
; So the current workaround is to keep a dynamic variable around that keeps track of all of this, and throw in a few
; guard statements to make sure that things are being called in a way that won't break the formatting.

(defparameter *stream-prefix-char* nil)

(defmethod stream-write-char ((v simple-view) char)
  (guard ((null *stream-prefix-char*) "expecting only a single prefix char before the string; prefix was ~a; new char is ~a" *stream-prefix-char* char) ())
  (setf *stream-prefix-char* char))

(defun draw-string (string)
  (with-window-of-focused-view-fallback-fore-color
    (with-font-view-fallback-font
      (let ((dict (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
                   *current-graphics-context-font* #$NSFontAttributeName
                   *current-graphics-context-stroke-color* #$NSForegroundColorAttributeName
                   ccl:+null-ptr+))
            (pt (pen-position *current-focused-view*)))
        (unwind-protect (#/drawAtPoint:withAttributes: string
                         (ns:make-ns-point
                           (point-h pt)
                           ; To mimic MCL positioning, I had to subtract of the ascend pixels from the y position of the pen
                           (- (point-v pt)
                              (first (multiple-value-list (font-info *current-graphics-context-font*)))))
                         dict)
          (setf *stream-prefix-char* nil))))))

(defmethod stream-write-string ((v simple-view) string &optional start end)
  (with-fallback-font-focused-view v
    (let* ((string
             (objc:make-nsstring
               (format nil "~a~a" (aif *stream-prefix-char* it "")
                       (if start
                         (subseq string start end)
                         string)))))
      (draw-string string))))

; Handling fonts and string width/height in pixels

(defmethod view-font ((view easygui::content-view-mixin))
  (view-font (content-view view)))

(defmethod view-font ((view simple-view))
  (guard-!null-ptr
    (guard-!nil
      (easygui:view-font view))))

(defun font-info (font-spec)
  (values (guard-!null-ptr (#/ascender font-spec))
          (abs (guard-!null-ptr (#/descender font-spec)))))

(defun get-dict-for-font (font)
  (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
   font #$NSFontAttributeName 
   ccl:+null-ptr+))

(defun string-width (str font)
  (let* ((dict (get-dict-for-font font))
         (attr (#/initWithString:attributes: (#/alloc ns:ns-attributed-string)
                (objc:make-nsstring str)
                dict))
         (size (#/size attr)))
    (ns:ns-size-width size)))

(defun font-point (font)
  (#/pointSize font))

(defun font-name (font)
  (objc:lisp-string-from-nsstring (#/fontName font)))

; Miscellaneous wrappers

; Provide the :quickdraw package on *modules*. Keeps from having to comment out the (require :quickdraw) lines in the MCL code.
; Also since the file here implements the quickdraw interface, it makes since to announce that the quickdraw library is available to use.

(provide :quickdraw)

; To implement event-dispatch for Clozure, send a dummy function over to
; the main Cocoa thread to be evaluated, and block until that function is 
; processed. This guarantees that all current event code in the Cocoa run loop
; has been processed before event-dispatch returns.

(defun event-dispatch ()
  (sv-log-n 1 "starting event dispatch")
  (let ((time
          (return-time-ms
            (unless (eq ccl::*current-process* ccl::*initial-process*)
              (dotimes (i 2)
                (let ((sema (make-semaphore)))
                  (gui::queue-for-gui 
                    (lambda ()
                      (signal-semaphore sema))
                    :at-start nil)
                  (wait-on-semaphore sema nil "semaphore event-dispatch wait")))))))
    (sv-log-n 1 "ending event dispatch after ~,2f ms" time)))

(defparameter *current-dialog-directory* nil)

(defun get-directory-with-fallback (directory)
  (setf *current-dialog-directory*
        (acond (directory it)
               (*current-dialog-directory* it)
               (*load-truename* (directory-namestring it))
               (t nil))))

; It turns out that objc functions are defined in the symbol table. So in order to set the title of
; the panel that is opened in the dialog, dynamically shadow the #/openPanel objc function. 
; And in that shadowed function, call the original, and then set the title of the resulting panel to prompt

(defun make-panel-and-set-prompt (fun-orig prompt)
  (lambda (&rest args)
    (let ((panel (apply fun-orig args)))
      (when prompt
        (#/setTitle: panel (objc:make-nsstring prompt)))
      panel)))

(ensure-defined
 (defun choose-file-dialog (&key directory mac-file-type button-string prompt file)
   (with-shadow (#/openPanel (make-panel-and-set-prompt fun-orig prompt))
     (gui::cocoa-choose-file-dialog :directory (get-directory-with-fallback directory)
                                    :file-types (aif mac-file-type (os-type->extensions it))
                                    :file file
                                    :button-string button-string))))

; FIXME: Write this
(defun os-type->extensions (os-type)
  #-:sv-dev (declare (ignore os-type))
  ())

; And use the shadowing technique here.

(ensure-defined
 (defun choose-new-file-dialog (&key directory mac-file-type button-string prompt file)
   #-:sv-dev (declare (ignore button-string))
   (with-shadow (#/savePanel (make-panel-and-set-prompt fun-orig prompt))
     (gui::cocoa-choose-new-file-dialog :directory (get-directory-with-fallback directory)
                                        :file-types (aif mac-file-type (os-type->extensions it))
                                        :file file))))
 
; And here as well. Except in this case latch into the #/setTitle: method, since that is being used in the
; cocoa-choose-directory-dialog function.

(defun set-title-and-use-prompt (fun-orig prompt)
  (lambda (panel string)
    (funcall fun-orig 
             panel 
             (aif prompt
               (objc:make-nsstring it)
               string))))

(ensure-defined
 (defun choose-directory-dialog (&key directory prompt)
   (with-shadow (#/setTitle: (set-title-and-use-prompt fun-orig prompt))
     (gui::cocoa-choose-directory-dialog :directory (get-directory-with-fallback directory)))))

(labels ((gen-dict-for-immutable-attr (bool)
           (#/dictionaryWithObject:forKey: ns:ns-dictionary
            (#/numberWithBool: ns:ns-number bool)
            #$NSFileImmutable))
         (set-immutable-attr (path bool)
           (#/setAttributes:ofItemAtPath:error: (#/defaultManager ns:ns-file-manager)
            (gen-dict-for-immutable-attr bool)
            (objc:make-nsstring path)
            ccl:+null-ptr+)))
  (defun file-locked-p (path)
    (let ((dict (guard-!null-ptr 
                  (#/attributesOfItemAtPath:error: (#/defaultManager ns:ns-file-manager)
                   (objc:make-nsstring path)
                   ccl:+null-ptr+))))
      (guard-t-or-nil
        (#/boolValue
         (#/objectForKey: dict (objc:make-nsstring "NSFileImmutable"))))))
  (defun lock-file (path)
    (let ((path (namestring path)))
      (unless (file-locked-p path)
        (guard-!nil
          (set-immutable-attr path #$YES)))))
  (defun unlock-file (path)
    (let ((path (namestring path)))
      (when (file-locked-p path)
        (guard-!nil
          (set-immutable-attr path #$NO))))))

; FIXME: Write this
(defun set-mac-file-creator (path mac-file-creator)
  (declare (ignore path mac-file-creator))
  t)

; FIXME: And maybe write this
(defun set-mac-file-type (path mac-file-type)
  (declare (ignore path mac-file-type))
  t)

(defparameter *current-cursor* *arrow-cursor*)

(defmethod set-cursor ((cursor ns:ns-cursor))
  (unwind-protect (setf *current-cursor* cursor)
    (awhen (front-window)
      (sv-log "setting cursor for window ~a to ~a" it cursor)
      (#/invalidateCursorRectsForView: (cocoa-ref it)
       (cocoa-ref (content-view it))))))

(objc:defmethod (#/resetCursorRects :void) ((self easygui::cocoa-contained-view))
  (call-next-method)
  (#/addCursorRect:cursor: self
   (#/bounds self)
   *current-cursor*))

; Another option here is to call #/currentCursor on ns-cursor class, but since 
; *current-cursor* is (currently) the current cursor for all windows of the application,
; just use this.

(defmethod window-cursor ((window window))
  *current-cursor*)

(defmethod color ((cursor ns:ns-cursor))
  (guard-!nil
    (cond ((eq cursor *i-beam-cursor*) *black-color*)
          ((eq cursor *arrow-cursor*) *black-color*)
          ((eq cursor *crosshair-cursor*) *black-color*))))

(defmethod create-resource ((type (eql 'cursor)) id)
  (make-instance
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithImage:hotSpot: (#/alloc ns:ns-cursor)
       (get-resource-val id 'image)
       (#/hotSpot *arrow-cursor*)))))

(defmethod get-cursor :before (id)
  (unless (resource-present-p id 'cursor)
    (add-resource
      (create-resource 'cursor id)
      id)))

(defmethod get-cursor (id)
  (get-resource-val id 'cursor))

(defun hide-cursor ()
  (#_CGDisplayHideCursor
   (#_CGMainDisplayID)))

(defun show-cursor ()
  (#_CGDisplayShowCursor
   (#_CGMainDisplayID)))

; Running on main GUI thread is required for the menubar functions. Otherwise Cocoa crashes fairly often when these are called.

(defun hide-menubar ()
  (easygui::running-on-main-thread ()
    (#/setPresentationOptions: (#/sharedApplication ns:ns-application)
     (logior
       #$NSApplicationPresentationHideDock
       #$NSApplicationPresentationHideMenuBar))))

(defun show-menubar ()
  (easygui::running-on-main-thread ()
    (#/setPresentationOptions: (#/sharedApplication ns:ns-application)
     #$NSApplicationPresentationDefault)))

(defun beep ()
  (#_NSBeep))

; ----------------------------------------------------------------------
; Manipulate the read table so that MCL's #@(a b) make-point shorthand works. 
;
; CCL does not support this by default, and the objective-c bridge uses #@ to make an NSString.
; So the #@ read macro defined below performs both tasks: If a list is passed, it 
; will convert the points in the list to a point representation. If a string is
; passed, it will call the CCL default read function for #@
; 
; Examples:
; #@"a string" -> #<NS-CONSTANT-STRING "hello"
; #@(5 4) -> #<EG-POINT (5.00/4.00)>)
; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *nonhacked-readtable* (copy-readtable))
  (set-dispatch-macro-character 
    #\# #\@
    (defun |#@-reader| (stream char arg)
      (let ((first-char (peek-char nil stream)))
        (ecase first-char
          (#\( (unless *read-suppress*
                 `(make-point ,@(read stream))))
          (#\" (funcall (get-dispatch-macro-character #\# #\@ *nonhacked-readtable*)
                        stream char arg)))))))

; ----------------------------------------------------------------------
; Manipulate reader functionality so that references to foreign functions that no longer exist can
; be defined as native functions, while keeping the same access syntax
;
; I did not want to have to modify the source code in the Phaser task where all of these carbon foreign 
; functions were used. CCL does not support the carbon framework, as far as I can tell. So in order to 
; trick CCL into thinking that these foreign functions are defined, add a bit of a 'before' section of 
; code to the load-external-function call. If the symbol name of the external function being loaded is
; in the list of function names that are being defined natively, then just return the symbol that maps
; to that function in the function symbol table. Otherwise, call the usual load-external-function function,
; and have CCL do the standard thing to try to find the foreign function
; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *load-external-function-orig* #'ccl::load-external-function)
  (with-continue 
    (defun ccl::load-external-function (sym query)
      (let* ((fun-names (list "showmenubar" "hidemenubar" "getcursor" "showcursor" "ShowCursor" "hidecursor" "HideCursor"
                              "paintrect" "PaintRect" "framerect" "drawstring" "moveto" "MoveTo" "lineto" "PaintOval"))
             (the-package (find-package :X86-Darwin64))
             (fun-syms (mapcar (lambda (name)
                                 (intern name the-package))
                               fun-names)))
        (if (member sym fun-syms)
          sym
          (funcall *load-external-function-orig* sym query))))))

; Use the same approach to define foreign constants that MCL uses that no longer exist for CCL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *load-os-constant-orig* #'ccl::load-os-constant)
  (with-continue
    (defun ccl::load-os-constant (sym &optional query)
      (let* ((con-names (list "tejustleft" "tejustcenter" "tejustright" "crossCursor"))
             (the-package (find-package :X86-Darwin64))
             (con-syms (mapcar (lambda (name)
                                 (intern name the-package))
                               con-names)))
        (if (member sym con-syms)
          sym
          (funcall *load-os-constant-orig* sym query))))))

; All of the functions being natively defined are here

(defun X86-Darwin64::|showmenubar| ()
  (show-menubar))

(defun X86-Darwin64::|hidemenubar| ()
  (hide-menubar))

(defun X86-Darwin64::|getcursor| (id)
  (get-cursor id))

(defun X86-Darwin64::|showcursor| ()
  (show-cursor))

(defun X86-Darwin64::|ShowCursor| ()
  (show-cursor))

(defun X86-Darwin64::|hidecursor| ()
  (hide-cursor))

(defun X86-Darwin64::|HideCursor| ()
  (hide-cursor))

(defun X86-Darwin64::|paintrect| (rect)
  (qd-paint-rect rect))

(defun X86-Darwin64::|PaintRect| (rect)
  (qd-paint-rect rect))

(defun X86-Darwin64::|framerect| (rect)
  (qd-frame-rect rect))

(defun X86-Darwin64::|drawstring| (str)
  (draw-string str))

(defun X86-Darwin64::|lineto| (x y)
  (qd-line-to x y))

(defun X86-Darwin64::|moveto| (x y)
  (qd-move-to x y))

(defun X86-Darwin64::|MoveTo| (x y)
  (qd-move-to x y))

(defun X86-DARWIN64::|PaintOval| (rect)
  (qd-paint-oval rect))

; And the constants are here
(defparameter X86-Darwin64::|tejustleft| $tejustleft)
(defparameter X86-Darwin64::|tejustcenter| $tejustcenter)
(defparameter X86-Darwin64::|tejustright| $tejustright)
(defparameter X86-Darwin64::|crossCursor| *crosshair-cursor*)



; ----------------------------------------------------------------------
; End file: actr6/devices/ccl/share.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: rmcl/level-1/l1-init.lisp
; ----------------------------------------------------------------------


(defparameter *menubar-bottom* 38) ;the location of the line under the menu bar.

(defparameter *modal-dialog-on-top* nil)



; ----------------------------------------------------------------------
; End file: rmcl/level-1/l1-init.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: rmcl/lib/color.lisp
; ----------------------------------------------------------------------


(defparameter *tool-back-color* 15658734)



; ----------------------------------------------------------------------
; End file: rmcl/lib/color.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: rmcl/lib/dialogs.lisp
; ----------------------------------------------------------------------



; FIXME: Is there any way not to use a global for this? The problem is that *modal-dialog-ret* is 
; set on a different thread than the modal dialog thread, so the cleanest way that I've
; found to communicate between threads is to use a shared global
(defparameter *modal-dialog-ret* nil)

; Use #/abortModal and not #/stopModal
; https://groups.google.com/forum/#!msg/wx-commits-diffs/fB0pl10Kw90/jZtHXQkkVxEJ
(defun stop-modal ()
  (#/abortModal (#/sharedApplication ns:ns-application))
  )

; Form could be (values ...), which is why this is a macro. Don't eval form
; until it's wrapped in a multiple-value-list call
(defmacro return-from-modal-dialog (form)
  `(progn
     (sv-log "returning from modal dialog with form ~a" ',form)
     (guard ((null *modal-dialog-ret*)
             "modal dialog system on thread ~a in inconsistent state; about to set val; but it's already set as ~a"
             *current-process*
             *modal-dialog-ret*) ())
     (setf *modal-dialog-ret* (cons :return (multiple-value-list ,form)))
     (stop-modal)
     (pop *modal-dialog-on-top*)
     (values)))

(defmethod modal-dialog ((dialog window) &optional (close-on-exit t))
  (push dialog *modal-dialog-on-top*)
  (guard ((null *modal-dialog-ret*)
          "modal dialog system on thread ~a in inconsistent state; val should be clear at this point, but it's set as ~a" 
          *current-process*
          *modal-dialog-ret*) ())
  (guard ((null *modal-dialog-ret*) "modal dialog system in inconsistent state; aborting"))
  (#/runModalForWindow: (#/sharedApplication ns:ns-application) (cocoa-ref dialog))
  (unwind-protect (apply #'values (cdr *modal-dialog-ret*))
    (when close-on-exit
      (window-close dialog))
    (guard (*modal-dialog-ret* "modal dialog system in inconsistent state; ret should be nil but it's ~a; aborting" *modal-dialog-ret*))
    (setf *modal-dialog-ret* nil)))

(defmethod find-subview-of-type ((view easygui::view) subview-type)
  (dolist (sub (view-subviews view) nil)
    (when (typep sub subview-type)
      (return sub))))



; ----------------------------------------------------------------------
; End file: rmcl/lib/dialogs.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: rmcl/lib/ccl-menus.lisp
; ----------------------------------------------------------------------


(defun return-cancel (i)
  (declare (ignore i))
  (return-from-modal-dialog :cancel))

(defclass string-dialog (dialog)
  ((allow-empty-strings :initform nil :initarg :allow-empty-strings)))

(defclass get-string-dialog (string-dialog)())

(defmethod set-view-size ((dialog get-string-dialog) h &optional v)
  (declare (ignore h v))
  (let* ((old-size (view-size dialog)))
    (call-next-method)
    (let* ((new-size (view-size dialog))
           (hdelta (make-point (- (point-h old-size)(point-h new-size)) 0))
           (subs (view-subviews dialog))
           (len (length subs)))
      (dotimes (i len)
        (let ((sub (elt subs i)))
          (if (typep sub 'button-dialog-item)
            (set-view-position sub (subtract-points (view-position sub) hdelta))
            (if (typep sub 'editable-text-dialog-item)
              (set-view-size sub (subtract-points (view-size sub) hdelta)))))))))

;; could be prettier, need a set-view-size method - move buttons, resize editable-text - done
; 140 x 80 is about minimum useful size - neg size is invisible
(with-continue
  (defun get-string-from-user (message 
                                &key
                                initial-string
                                (size #@(365 100))
                                (position '(:bottom 140))
                                (ok-text "OK")
                                (cancel-text "Cancel")
                                (modeless nil)
                                (window-title "")
                                (window-type :document-with-grow)
                                (back-color *tool-back-color*)
                                (allow-empty-strings nil)
                                (action-function #'identity)
                                cancel-function
                                (theme-background t)
                                &aux dialog (delta 0) (message-len 0) message-item)
    (when (not initial-string) (setq initial-string ""))
    (if t (setq delta 20)(setq delta 10))  
    (when message 
      (setq message-item (make-instance 'static-text-dialog-item
                                        :text-truncation :end
                                        :view-position (make-point 6 (- (point-v size) 54 delta))
                                        :dialog-item-text message))
      (let* ((msize (view-size message-item))
             (mh (point-h msize)))  ;; would be nice if static text had a truncate option -now it does
        (setq mh (min mh (- (point-h size) 100))) 
        (set-view-size message-item (make-point mh (point-v msize))))
      (setq message-len (+ 6 (point-h (view-size message-item)))))
    (flet ((act-on-text (item)
             (let ((e-item
                     (find-subview-of-type (view-container item)
                                           'editable-text-dialog-item)))
               (funcall action-function (dialog-item-text e-item)))))    
      (setq dialog (make-instance 
                     'get-string-dialog
                     :view-position position
                     :view-size size
                     :close-box-p (if modeless t nil)
                     :grow-box-p t
                     :window-type window-type
                     :window-title window-title
                     :window-show nil
                     :back-color back-color
                     :theme-background theme-background
                     :allow-empty-strings allow-empty-strings
                     :view-subviews
                     (list
                       (make-dialog-item
                         'default-button-dialog-item
                         (make-point (- (point-h size) 74)
                                     (- (point-v size) 20 delta))
                         #@(62 25)
                         ok-text
                         (if (not modeless)
                           #'(lambda (item)
                               (return-from-modal-dialog (act-on-text item)))
                           #'act-on-text))                     
                       (make-dialog-item 'button-dialog-item
                                         (make-point (- (point-h size) 154)
                                                     (- (point-v size) 20 delta))
                                         #@(70 25)
                                         cancel-text
                                         (or cancel-function
                                             #'(lambda (item)
                                                 (if (not modeless) 
                                                   (return-from-modal-dialog :cancel)
                                                   (window-close (view-window item)))))
                                         :cancel-button t)
                       (make-dialog-item 'editable-text-dialog-item
                                         (make-point (+ 6 message-len) (- (point-v size) 54 delta))
                                         (make-point (- (point-h size) delta message-len) 23)
                                         initial-string))))
      (when message (add-subviews  dialog  message-item))
      (update-default-button dialog)
      (cond ((not modeless)         
              (modal-dialog dialog))
            (t (window-show dialog)
             dialog)))))


(defmethod update-default-button ((obj string-dialog)) ())

; need close box if modal nil 
(defun message-dialog (message &key (ok-text "OK")
                               (size #@(335 100))
                               (modal t)   ; if not modal its callers job to select
                               (title "Warning")
                               window-type
                               (on-ok-click
                                 #'(lambda (item)
                                     (declare (ignore item))
                                     (return-from-modal-dialog t)))
                               (back-color *tool-back-color*)
                               (theme-background t)
                               (position (list :top (+ *menubar-bottom* 10))))
  (let* ((message-width (- (point-h size) 85))
         (new-dialog
           (make-instance
             'dialog
             :view-position position
             :view-size size
             :window-title title
             :window-type (or window-type (if modal :movable-dialog :document))
             :close-box-p (if modal nil t)
             :window-show nil
             :back-color back-color
             :theme-background theme-background
             :view-subviews
             `(,(make-instance
                  'static-text-dialog-item
                  :dialog-item-text message
                  :text-truncation #$NSLineBreakByWordWrapping
                  :view-size (make-point
                               message-width
                               (- (point-v size)
                                  30)))
                ,@(if modal
                    (list (make-dialog-item
                            'default-button-dialog-item
                            (subtract-points size #@(75 35))
                            #@(62 25)
                            ok-text
                            on-ok-click)))))))
    (if modal
      (modal-dialog new-dialog)
      new-dialog)))

(defclass select-dialog (window) ())

(with-continue
  (defun select-item-from-list (the-list &key (window-title "Select an Item")
                                         (selection-type :single)
                                         table-print-function 
                                         (action-function #'identity)
                                         (default-button-text "OK")
                                         (sequence-item-class 'sequence-dialog-item)
                                         (view-size (make-point 400 (+ 80 (* (length the-list) 20))))
                                         (view-position '(:top 90) pos-p)
                                         (theme-background t)
                                         dialog-class
                                         modeless
                                         (help-spec 14086)
                                         (list-spec 14087)
                                         (button-spec 14088))
    "Displays the elements of a list, and returns the item chosen by the user"
    (let (debutton dialog)
      (flet ((act-on-items (item)
               (let ((s-item (find-subview-of-type (view-container item)
                                                   'sequence-dialog-item)))
                 (funcall action-function 
                          (mapcar #'(lambda (c) (cell-contents s-item c))
                                  (selected-cells s-item))))))
        (when (and dialog-class (not pos-p) modeless)
          (let ((w (front-window :class 'select-dialog)))  ; or dialog-class?
            (when w (setq view-position (add-points (view-position w) #@(15 15))))))
        (setq debutton
              (make-instance 
                'default-button-dialog-item
                :dialog-item-text default-button-text
                :dialog-item-enabled-p the-list
                :help-spec button-spec
                :dialog-item-action
                (cond 
                  ((not modeless)
                   #'(lambda (item)
                       (return-from-modal-dialog (act-on-items item))))
                  (t
                   #'act-on-items ))))
        (let* ((bsize (view-default-size debutton))
               bpos)
          (setq bsize (make-point (max 60 (point-h bsize)) (point-v bsize))
            bpos (make-point (- (point-h view-size) 25 (point-h bsize))
                             (- (point-v view-size) 7 (point-v bsize))))
          (set-view-size debutton bsize)
          (set-view-position debutton bpos)
          (setq dialog
            (make-instance
              (or dialog-class 'select-dialog)
              :window-type :document-with-grow
              :close-box-p (if modeless t nil)
              :window-title window-title
              :view-size view-size
              :view-position view-position
              :window-show nil ;modeless
              :back-color *tool-back-color*
              :theme-background theme-background
              :help-spec help-spec
              :view-subviews
              (list*
                (make-instance
                  sequence-item-class
                  :view-position #@(4 4)
                  :view-size (make-point (- (point-h view-size) 8)
                                         (+ 50 (- (point-v view-size) (point-v bsize) 20)))
                  ;:table-hscrollp nil
                  :table-sequence the-list
                  :table-print-function table-print-function
                  :selection-type selection-type
                  :help-spec list-spec)
                debutton
                (if (not modeless)
                  (list
                    (make-dialog-item 'button-dialog-item
                                      (make-point (- (point-h bpos) 80)
                                                  (point-v bpos))
                                      (make-point (if t #|(osx-p)|# 74 60) (point-v bsize))
                                      "Cancel"
                                      #'return-cancel
                                      :cancel-button t
                                      :help-spec 15012))
                  nil))))
          ;(when the-list (cell-select sdi (index-to-cell sdi 0))) ; let arrow-dialog-item do this
          (cond (modeless ; select first then show is prettier
                  (window-show dialog)
                  dialog)

                (t ;(#_changewindowattributes (wptr dialog) 0 #$kWindowCollapseBoxAttribute)
                 (modal-dialog dialog))))))))



; ----------------------------------------------------------------------
; End file: rmcl/lib/ccl-menus.lisp
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; Begin file: build/post-code.lisp
; ----------------------------------------------------------------------



(provide "CCL-SIMPLE-VIEW")



; ----------------------------------------------------------------------
; End file: build/post-code.lisp
; ----------------------------------------------------------------------
