;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2012 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : act-gui-interface.lisp
;;; Version     : 2.0a1
;;; 
;;; Description : Contains the functions that implement the abstract GUI
;;;             : interface used by the tutorial units and the misc functions
;;;             : that go with them (correlation and mean-deviation).  
;;;             : I'm calling it the ACT-R GUI interface (AGI) as suggested by Mike.
;;;             : It relies on the old UWI to handle the "real" interface to any
;;;             : particular windowing environment.
;;; Bugs        : 
;;; To Do       : [x] Consider making it support multiple interfaces to go with
;;;             :     multiple models.
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Renamed this file from uniform-interface-exp to 
;;;             : act-gui-interface.
;;;             : Added comments.
;;; 2002.12.17 Dan
;;;             : Modified correlation and mean-deviation so that
;;;             : the output keyword parameter is "more useful" -
;;;             : specifying a stream works right now (it doesn't try to
;;;             : open a file for it) and specifying nil suppress
;;;             : all output.
;;; 2002.12.19 Dan
;;;             : Updated add-text-to-exp-window so that it now includes
;;;             : a color option.
;;; 04.04.13 Dan [2.2]  (previous two changes also "new" as of 2.2)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : reset version to 1.0a1
;;;             : added the packaging switches
;;;             : changed permute-list to use act-r-random
;;;
;;; 04.12.17 Dan
;;;             : Added get-time as a replacement for pm-get-time.
;;;   
;;; 2005.02.25 Dan
;;;             : * Removed the ~\newline usages because that causes problems
;;;             :   when a Lisp only wants to see native new lines there.
;;; 2006.09.07 Dan
;;;             : * Changed permute-list so that it's safe when passed nil or
;;;             :   a non-list.
;;; 2007.07.13 Dan
;;;             : * Added color as an option to add-button-to-exp-window.
;;; 2007.12.13 Dan
;;;             : * Adding an add-items-to-exp-window function to compliment
;;;             :   the remove-... and to avoid the need to call the UWI
;;;             :   function when such an action is necessary.
;;; 2009.09.10 Dan
;;;             : * Moved permute-list to the random module's file.
;;; 2010.03.08 Dan
;;;             : * Changed close-exp-window so that it checks to see if the
;;;             :   window is still there before trying to close it.  Avoids
;;;             :   a problem where a user has closed an environment side
;;;             :   window.
;;; 2010.07.31 Dan
;;;             : * Changed add-line-to-exp-window to constrain the x,y positions
;;;             :   to be fixnums to avoid problems in ACL and LispWorks if 
;;;             :   a float or rational is used instead.
;;; 2011.04.26 Dan
;;;             : * Changed get-time to just return mp-time-ms.
;;; 2011.11.14 Dan
;;;             : * Make open-exp-window set the visual center to the center of
;;;             :   the window.
;;; 2011.11.21 Dan
;;;             : * Get-time changed to not use *actr-enabled-p*.  Instead
;;;             :   it uses an optional parameter to determine whether or not 
;;;             :   to use mp-time.
;;;             :   If the optional parameter is provided then a true value
;;;             :   means model time and nil means internal-real-time.  If 
;;;             :   no parameter is provided then model time will be returned.
;;; 2012.06.08 Dan [2.0a1]
;;;             : * Use a module to maintain the information for the open
;;;             :   window(s) and allow multiple open windows for a model.
;;;             :   Multiple models may have the same titled window open and
;;;             :   that should make it "easy" to have multiple models use the
;;;             :   same code to provide separate interfaces.  The real window's
;;;             :   title (which the model can't see anyway) will include the 
;;;             :   model's name to make things easier for the modeler.
;;;             : * The significant changes to the commands are:
;;;             :   - open-exp-window doesn't automatically close a window with
;;;             :     a different title when it creates a new one.   It does
;;;             :     still clear and bring to the front a window by the same
;;;             :     title within the same model.
;;;             :   - Open-exp-window will only create a window if there is a
;;;             :     current model.  The main reason for that is because it 
;;;             :     avoids a lot of hassle of dealing with "unowned"
;;;             :     windows.  It also makes things much easier with respect
;;;             :     to displaying multiple visible virtual windows through
;;;             :     the ACT-R Environment.
;;;             :   - All windows now automatically close when the owning model
;;;             :     is deleted (either explicitly or via clear-all) and if
;;;             :     the new system parameter :close-exp-windows-on-reset is set
;;;             :     it will also close them when the owning model is reset.
;;;             :     [I'd prefer the default for :close-exp-windows-on-reset
;;;             :     to be t, but unfortunately because of how the single
;;;             :     experiment window code worked doing so would likely break 
;;;             :     lots of existing models (since some in the tutorial models
;;;             :     used it in a manner that assumed the window persists).]
;;;             :   - Most of the commands now have an additional keyword 
;;;             :     parameter :window which can be used to indicate which
;;;             :     window to perform the action upon.  If it is not provided
;;;             :     and there is only one window open in the current model
;;;             :     the command will be applied to that window, otherwise it
;;;             :     will result in a warning and no action performed.
;;;             :     The window value can be specified as either the window 
;;;             :     object returned by open-exp-window or the title which
;;;             :     was specified for open-exp-window.  When specifying a title
;;;             :     as the :window parameter that will be relative to the current
;;;             :     model since different models can have windows with the same
;;;             :     title.
;;;             :   - For add-line-to-exp-window the color is now a keyword
;;;             :     parameter instead of optional so that it isn't confusing 
;;;             :     when also specifying a window.  This change is the only one
;;;             :     that's not backward compatible with the previous "single
;;;             :     window" usage and will require changing existing models.
;;; 2012.07.02 Dan
;;;             : * Open-exp-window now calls device-update-attended-loc with a
;;;             :   nil xy-loc when it reuses a window so that the attended marker
;;;             :   gets cleared.
;;; 2012.07.12 Dan
;;;             : * Added the exp-window-owner function because there are times
;;;             :   when it would be useful to know that.
;;; 2012.12.12 Dan
;;;             : * Adding a check to make sure that the text is a string in
;;;             :   add-text-to-exp-window.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Class for the AGI module instance.  
;;; Have a "global" table which maps window instances to meta-process & model that
;;; owns it and a model based alist of (title . window instance) elements.
;;; That way things like close and select can work without having to be in
;;; the correct model.  Also allow for opening outside of a model by just
;;; not setting anything within a module i.e. title is never meaningful
;;; and the user is responsible for closing before reopening instead of letting
;;; it automatically close/clear.

(defclass agi-module ()
   ;; the global table maps a window object to a cons of meta-process and model names
  ((global-agi-table :accessor global-agi-table :initform (make-hash-table) :allocation :class)
   ;; whether or not to close windows upon reset which is a global feature
   (close-exp-on-reset :accessor close-exp-on-reset :initform nil :allocation :class)   
   ;; the agi-window-list 
   (agi-window-list :accessor agi-window-list :initform nil)))

;;; Here's all I need for the module -- creation, reset, and delete 
;;; functions to initialize and close things as needed.

;;; Create just needs to return a new instance of the class.

(defun create-agi-module (name)
  (declare (ignore name))
  (make-instance 'agi-module))

;;; Delete just closes all of the model's windows making sure to remove
;;; them from the global table as well, but doesn't need to clear the
;;; module list since the module is going away.

(defun delete-agi-module (instance)
  (dolist (x (agi-window-list instance))
    (close-rpm-window (cdr x))
    (remhash (cdr x) (global-agi-table instance))))

;;; Reset may need to close the windows (like delete) and also needs to
;;; clean up the model's list by setting it to nil to make sure
;;; things are synchronized since the delete method doesn't need
;;; to do so.

(defun reset-agi-module (instance)
  (when (close-exp-on-reset instance) 
    (delete-agi-module instance)
    (setf (agi-window-list instance) nil)))


(define-module agi nil nil
  :version "2.0a1"
  :documentation "Module based manager for AGI windows"
  :creation create-agi-module
  :reset reset-agi-module
  :delete delete-agi-module)

;;; Create a system parameter to handle the close on reset flag

(create-system-parameter :close-exp-windows-on-reset
                         :valid-test 'tornil
                         :warning "t or nil"
                         :default-value nil
                         :documentation "Whether the AGI closes windows associated with a model when that model is reset"
                         :handler (lambda (set-or-get value)
                                    (if set-or-get
                                        (setf (close-exp-on-reset (make-instance 'agi-module)) value)
                                      (close-exp-on-reset (make-instance 'agi-module)))))


;;; The rest of this is basically the AGI commands available to the modeler.
  
;;; GET-TIME
;;; Return time in milliseconds
;;; If the optional parameter is specified as true then model time is returned
;;; and if it is nil then get-internal-real-time is used.  
;;; If time is not based on the model it's only meaningful as a relative
;;; time.

(defun get-time (&optional (model-time t))
  (if model-time
      (mp-time-ms)
    (round (* 1000 (/ (get-internal-real-time) internal-time-units-per-second)))))

;;; OPEN-EXP-WINDOW  [Function]
;;; Description : This function opens a window with the properties specified and
;;;             : returns that window object if there is a current model.
;;;             : 
;;;             : If there's already a window with that title open in the current 
;;;             : model with the same visible status then it is cleared and not 
;;;             : changed.  Otherwise a new window is created with the provided details.
;;;             : In either case the window is brough to the front (assuming that
;;;             : the UWI select-rpm-window command works properly for the window
;;;             : type), the vision module's center point is set to the center of 
;;;             : the height and width specified (which could be different from
;;;             : the center of the existing window since they aren't verified against
;;;             : the current "real" display properties), and the window which is
;;;             : created is returned.
;;;             : 
;;;             : The title of the window can be used to reference the window as long
;;;             : as the model in which it was created is the current one.  Note that
;;;             : different models may have windows with the same title so one may need
;;;             : to be careful to set the current model context properly when working
;;;             : with multiple models.  If there is not a current model then the title 
;;;             : cannot be used as a reference.
;;;             :
;;;             : If there is not a current model then 
;;;             : Might want to consider a flag upon opening that says this is a
;;;             : unique window for the current model which would cause all actions
;;;             : upon the window to be executed within that model's context, but
;;;             : that could be tricky and may not really be necessary.

(defun open-exp-window (title &key (width 300) (height 300) (visible t) (x 300) (y 300))
  "Open an experiment window"
  (if (or (stringp title) (symbolp title))
      (if (current-model)
          (let* ((instance (get-module agi))
                 (exists (assoc title (agi-window-list instance) :test 'string-equal))
                 (win (and exists (cdr exists))))
            (if (and win (open-rpm-window? win)
                     (or (and visible (rpm-window-visible-status win)) ;; both visible 
                         (not (or visible (rpm-window-visible-status win))))) ;; both not visible
                (progn
                  (remove-all-items-from-rpm-window win)
                  (device-update-attended-loc win nil))
              (when win
                (when (open-rpm-window? win)
                  (close-rpm-window win))
                (setf (agi-window-list instance) (remove exists (agi-window-list instance)))
                (remhash (cdr exists) (global-agi-table instance))
                (setf win nil)))
            (unless win
              (setf win (make-rpm-window :visible visible :title (format nil "~a (~a)" title (current-model))
                                         :width width 
                                         :height height
                                         :x x
                                         :y y))
              (push (cons title win) (agi-window-list instance))
              (setf (gethash win (global-agi-table instance)) (cons (current-meta-process) (current-model))))
            (select-rpm-window win)
            (set-visual-center-point (round width 2) (round height 2))
            win)
        
        (print-warning "Cannot create an experiment window without a current model"))
    (print-warning "Experiment window title must be a string or symbol, but ~s was specified." title)))


(defun exp-window-owner (win)
  (let* ((instance (make-instance 'agi-module))
         (owner (gethash win (global-agi-table instance))))
    (if owner
        (values (car owner) (cdr owner))
      (values nil nil))))

;;; Internal function for mapping a user provided window reference: window obj, title, or nil
;;; to some real window object if possible.


(defun determine-exp-window (window)
  (let ((instance (suppress-warnings (get-module agi))))
    
    (cond ((subtypep (type-of window) 'rpm-window)
           (if (open-rpm-window? window)
               window
             (print-warning "Window ~s is not currently open." window)))
          ((and window (or (stringp window) (symbolp window))                          ;; it's a window title
                instance                                                               ;; there's a current model
                (assoc window (agi-window-list instance) :test 'string-equal))  ;; and that title exists in this model
           (cdr (assoc window (agi-window-list instance) :test 'string-equal))) ;; use that window
          ((and window (or (stringp window) (symbolp window)) instance)
           (print-warning "~s is not the title of an open window in the current model." window))
          ((and window (or (stringp window) (symbolp window)))
           (print-warning "There is no current model therefore a window title (~s) cannot be used as a reference." window))
          (window
           (print-warning "~s is not a reference to an open window." window))
          (t
           (if instance
               (if (= (length (agi-window-list instance)) 1)
                   (cdar (agi-window-list instance))
                 (print-warning "There is ~:[no~;more than one~] window to use in the current model." (agi-window-list instance)))
             (let ((table (global-agi-table (make-instance 'agi-module))))
               (if (= 1 (hash-table-count table))
                   (car (hash-table-keys table))
                 (print-warning "There is no current model and more than one open experiment window.~%Therefore all window operations must specify a window."))))))))
  

;;; CLOSE-EXP-WINDOW  [Function]
;;; Description : Closes an experiment window.  The window can be specified
;;;             : by title in current model or explicit window object.
;;;             : If no window is provided then if there is only one window or only
;;;             : one window in the current model that window will be closed and t
;;;             : will be returned, otherwise it will print a warning and return nil.

(defun close-exp-window (&optional (window nil))
  "Close the experiment window"
  
  (aif (determine-exp-window window)
       ;; just create a new agi-module instance so that I have access to the global table
       (let* ((instance (make-instance 'agi-module))
              (owner (gethash it (global-agi-table instance))))
         
         ;; Close the window 
         (close-rpm-window it)
         
         
         ;; remove it from the global table
         (remhash it (global-agi-table instance))
         
         ;; remove it from the owning module's instance 
         (let* ((owner-instance (with-meta-process-eval (car owner) (with-model-eval (cdr owner) (get-module agi))))
                (owner-windows (agi-window-list owner-instance)))
             (setf (agi-window-list owner-instance) (remove (rassoc it owner-windows) owner-windows)))
         t)
       (print-warning "Could not close window ~s" window)))


(defun close-all-exp-windows ()
  (maphash (lambda (key value)
             (declare (ignore value))
             (close-exp-window key))
           (global-agi-table (make-instance 'agi-module))))
       
       
;;; SELECT-EXP-WINDOW  [Function]
;;; Description : Brings a window to the front.  The window can be specified
;;;             : by title in current model or explicit window object.
;;;             : If no window is provided then if there is only one window or only
;;;             : one window in the current model that window will be brought to the 
;;;             : front, otherwise it will print a warning.
;;;             : Returns t if a window was attempted to be brought to the front
;;;             : (success depends on select-rpm-window working) and nil otherwise.


(defun select-exp-window (&optional (window nil))
  "select the experiment window"
  (aif (determine-exp-window window)
       (progn
         (select-rpm-window it)
         t)
       (print-warning "Select-exp-window failed.")))

;;; CLEAR-EXP-WINDOW  [Function]
;;; Description : Removes all items from the specified window.  The window
;;;             : can be specified by title in current model or explicit window
;;;             : object.  If no window is provided then if there is only one window 
;;;             : or only one window in the current model it will clear that window.
;;;             : If a valid window is available then all items will be removed
;;;             : from it and t will be returned.  Otherwise a warning will be 
;;;             : printed and nil returned.

(defun clear-exp-window (&optional (window nil))
  "Erases everything in the experiment window"
    (aif (determine-exp-window window)
       (progn
         (remove-all-items-from-rpm-window it)
         t)
       (print-warning "clear-exp-window failed.")))
  

;;; Internal function to pull the simulated keyword parameter
;;; out of the arbitrary item lists.

(defun determine-window-and-items (items)
  (cond ((= 0 (count :window items))
         (values (determine-exp-window nil) items))
        ((and (= 1 (count :window items))
              (> (length items) (position :window items)))
         (values (determine-exp-window (nth (1+ (position :window items)) items))
                 (remove :window (remove (nth (1+ (position :window items)) items) items))))
        ((= 1 (count :window items))
         (print-warning "The :window keyword parameter was specified by no value given.")
         (values nil items))
        ((find :window items)
         (print-warning "The :window parameter was specified more than once.")
         (values nil items))
        (t ;; shouldn't get here but just to be safe
         (values (determine-exp-window nil) items))))


;;; ADD-ITEMS-TO-EXP-WINDOW  [Function]
;;; Description : Adds the requested items into the window specified with the :window
;;;             : keyword parameter or the "default" window if no keyword parameter is
;;;             : specified.  Returns t if the items were attempted to be added
;;;             : (success depends on whether or not they were accepted by add-visual-
;;;             : items-to-rpm-window), but if there is no window provided and no
;;;             : current "default" window then a warning is printed and nil is 
;;;             : returned.


(defun add-items-to-exp-window (&rest items)
  "Add the specified items to the experiment window"
  (multiple-value-bind (window items) (determine-window-and-items items)
    (if window
        (progn
          (apply 'add-visual-items-to-rpm-window window items)
          t)
      (print-warning "No window available for add-items-to-exp-window."))))


;;; REMOVE-ITEMS-FROM-EXP-WINDOW  [Function]
;;; Description : Removes the requested items from the window specified with the :window
;;;             : keyword parameter or the "default" window if no keyword parameter is
;;;             : specified.  Returns t if the items were attempted to be removed
;;;             : (success depends on whether or not they were accepted by remove-visual-
;;;             : items-from-rpm-window), but if there is no window provided and no
;;;             : current "default" window then a warning is printed and nil is 
;;;             : returned.

(defun remove-items-from-exp-window (&rest items)
  "Remove the specified items from the experiment window"
  (multiple-value-bind (window items) (determine-window-and-items items)
    (if window
        (progn
          (apply 'remove-visual-items-from-rpm-window window items)
          t)
      (print-warning "No window available for remove-items-from-exp-window."))))

;;; ADD-TEXT-TO-EXP-WINDOW  [Function]
;;; Description : Build a text item based on the parameters supplied and
;;;             : add it to the specified window (or current default if
;;;             : none povided).

(defun add-text-to-exp-window (&key (x 0) (y 0) (text "") (height 20) (width 75) (color 'black) (window nil))
  "Create and display a text item in the experiment window"
  (if (stringp text)
      (aif (determine-exp-window window)
           (let ((item (make-static-text-for-rpm-window it
                                                        :text text 
                                                        :x x
                                                        :y y
                                                        :width width
                                                        :height height
                                                        :color color)))
             (add-visual-items-to-rpm-window it item)
             item)
           (print-warning "No window available for adding a text item."))
    (print-warning "Text must be a string in add-text-to-exp-window cannot add ~s." text)))

;;; ADD-BUTTON-TO-EXP-WINDOW  [Function]
;;; Description : Build a button item based on the parameters supplied and
;;;             : add it to the specified window (or current default if
;;;             : none povided).

(defun add-button-to-exp-window (&key (x 0) (y 0) (text "Ok") (action nil) (height 18) (width 60) (color 'gray) (window nil))
  "Create and display a button item in the experiment window"
  (aif (determine-exp-window window)
       (let ((item (make-button-for-rpm-window it
                                               :x x
                                               :y y
                                               :text text
                                               :action action
                                               :height height
                                               :width width :color color)))
         (add-visual-items-to-rpm-window it item)
         item)
       (print-warning "No window available for adding a button.")))

;;; ADD-LINE-TO-EXP-WINDOW  [Function]
;;; Description : Build a line item based on the parameters supplied and
;;;             : add it to the specified window (or current default if
;;;             : none povided).

(defun add-line-to-exp-window (start-pt end-pt &key (color 'black) (window nil))
  "Create and display a line item in the experiment window"
  (aif (determine-exp-window window)
       (let ((item (make-line-for-rpm-window it (mapcar 'round start-pt) (mapcar 'round end-pt) color)))
         (add-visual-items-to-rpm-window it item)
         item)
       (print-warning "No window available for adding a line.")))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; The miscelaneous functions used in the tutorial.
;;;; ---------------------------------------------------------------------- ;;;;

;;; These are the correlation and deviation functions from the scripting
;;; extensions file and the necessary support.  I figured since they are
;;; still used they should be put here because the scripting extensions 
;;; aren't part of ACT-R 5, but making people load the scripting file
;;; separately is a pain...  I also changed mean-deviation so that it
;;; actually returned the deviation.

(defstruct data labels array)

(defmacro /-safe (number &rest dividers)
  `(/ ,number ,@(let ((max nil))
                  (dolist (divider dividers max)
                    (push-last `(if (zerop ,divider) 1 ,divider) max)))))

(defun numbers-list (structure)
  (let ((list nil))
    (when (data-p structure) (setf structure (data-array structure)))
    (cond ((arrayp structure)
           (dotimes (i (array-total-size structure))
             (let ((data (row-major-aref structure i)))
               (when (numberp data) (push data list)))))
          ((listp structure)
           (dolist (data structure)
             (cond ((listp data)
                    (setf list (append (nreverse (numbers-list data)) list)))
                   ((numberp data)
                    (push data list)))))
          ((numberp structure)
           (push structure list))
          (t (format t "~&UNKNOWN DATA FORMAT ~S NOT COMPATIBLE WITH NUMBERS LIST.~%"
                     structure)))
    (nreverse list)))

(defun square-data (x)
  (* x x))

(defun sum-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum data))))

(defun square-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum (square-data data)))))

(defun product-list (list1 list2)
  (let ((sum 0.0))
    (loop
      (when (or (null list1) (null list2)) (return sum))
      (incf sum (* (pop list1) (pop list2))))))

(defun mean-deviation (results data &key (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (open output :direction :output :if-exists :append
                              :if-does-not-exist :create))
           (setf opened t))
          ((not (or (streamp output) (null output) (eq output t)))
           (format t "~&OUTPUT ARGUMENT ~S TO MEAN-DEVIATION IS NOT VALID.~%"
             output)
           (format t "IT MUST BE A STRING, PATHNAME, STREAM, T OR NIL.~%")
           (setf output t)))
    
    (unless (= (length results-list) (length data-list))
      (format t "~&ERROR: ~S AND ~S DO NOT HAVE THE SAME NUMBER OF NUMBERS.~%"
              results data))
    (let ((result (sqrt (/ (+ (square-list results-list) (square-list data-list)
                              (* -2.0 (product-list results-list data-list)))
                           n))))
      (format output "~&MEAN DEVIATION: ~6,3F~%" result)
      (when opened (close output))
      
      result)))

(defun correlation (results data &key (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (average-results (/-safe (sum-list results-list) n))
         (average-data (/-safe (sum-list data-list) n))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (open output :direction :output :if-exists :append
                              :if-does-not-exist :create))
           (setf opened t))
          ((not (or (streamp output) (null output) (eq output t)))
           (format t "~&OUTPUT ARGUMENT ~S TO CORRELATION IS NOT VALID.~%"
             output)
           (format t "IT MUST BE A STRING, PATHNAME, STREAM, T OR NIL.~%")
           (setf output t)))
    (unless (= (length results-list) (length data-list))
      (format t "~&ERROR: ~S AND ~S DO NOT HAVE THE SAME NUMBER OF NUMBERS.~%"
              results data))
    (let ((result (/-safe (- (/-safe (product-list results-list data-list) n)
                       (* average-results average-data))
                    (* (sqrt (- (/-safe (square-list results-list) n)
                                (square-data average-results)))
                       (sqrt (- (/-safe (square-list data-list) n)
                                (square-data average-data)))))))
      (format output "~&CORRELATION: ~6,3F~%"
            result)
    (when opened (close output))
      result)))


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
