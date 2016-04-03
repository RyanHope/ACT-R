;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author      : John Anderson & Dan Bothell
;;; Copyright   : (c) 2007
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filename    : bold.lisp
;;; Version     : 3.0
;;;
;;; Description : Computes predictions of the BOLD response based on activity
;;;               of the buffers in a module.
;;;
;;; Bugs        :
;;;
;;; To do       : [x] Consider a change from seconds to milliseconds for the
;;;             :     internal data since buffer-record-ms-time is now a 
;;;             :     replacement for buffer-record-time-stamp.
;;;             :     - A compromise is to still use seconds, but represent
;;;             :     them with rationals to avoid any loss of accuracy until
;;;             :     they get "used" in some floating point calculation.
;;;
;;; ----- History -----
;;;
;;; 2007.06.05 Dan
;;;             : * Initial creation.
;;; 2007.06.25 John
;;;             : * Previously, all data was displayed with a maximum point of 1.   
;;;             :   However, this makes it impossible to compare two conditions 
;;;             :   where the response is suppose to be larger in one of the conditions.   
;;;             :   Therefore, I changed the normalizing process so that it now  
;;;             :   assumes the gamma distribution with constant area of 1.   This  
;;;             :   means that if one region is active longer it will add up to 
;;;             :   produce a bigger response.
;;;             :   Right now the normalization only works for integer exponents.
;;;             :   If one replaced bold_factorial by a more general gamma function  
;;;             :   one could have continuous exponents.
;;; 2007.07.16 Dan
;;;             : * Adding in the support code to provide some graphing tools on
;;;             :   the environment side.
;;; 2007.07.18 Dan
;;;             : * Added the reset function to handle the caching of data for
;;;             :   the environment.
;;;             : * Added the code that draws the ROI in the brain images in
;;;             :   the environment.
;;; 2007.07.20 Dan
;;;             : * Cleaning up the graphing and brain region code to allow
;;;             :   for "normalizing" within regions instead of across all
;;;             :   as an option.
;;; 2007.08.02 Dan
;;;             : * Added the function to create the data lists needed for the
;;;             :   crude 3d viewer.
;;;             : * Cleaned up the viewer functions so they don't crash the
;;;             :   inspectors when there's no data.
;;; 2007.08.08 Dan
;;;             : * Sped things up considerably by taking out the sgp calls
;;;             :   and just passing the module's parameters into the critical
;;;             :   computation functions.
;;; 2007.08.10 Dan
;;;             : * Added the calls to handle the "on the fly" scanning and
;;;             :   a mechanism that records the max value in a region which
;;;             :   persists across a reset (but not clear-all).  The max value
;;;             :   computation allows for the scaling on the fly assuming that
;;;             :   there was a previous run with reasonable max values.
;;; 2007.08.13 Dan
;;;             : * Changed bold-brain-3d-data so that it puts out the data
;;;             :   in lists the same way the real-time viewer uses so that the
;;;             :   same display code can be used by either viewer.
;;; 2007.08.14 Dan
;;;             : * Modified the brain data functions so that they send out the
;;;             :   floating point value instead of scaling 0-20 so that the
;;;             :   display can show that 0-1.0 number.
;;; 2007.08.23 Dan
;;;             : * Changed the test on the parameter from fixnump to integerp
;;;             :   since not all Lisps provide a fixnump but integerp is in
;;;             :   the ANSI spec.
;;; 2007.08.31 Dan
;;;             : * Added start and end times as optional parameters for the 
;;;             :   predict-bold-response which restricts the predictions to
;;;             :   the range specified.
;;;             : * Added the :bold-settle parameter which controls the window
;;;             :   in which the predictions are computed.
;;; 2007.09.04 Dan
;;;             : * Make sure that the start time is a multiple of bold-inc to
;;;             :   ensure consistency in the results when checking ranges, and if
;;;             :   the value provided isn't then it uses the closest lower value
;;;             :   which is an increment (and gives a warning).
;;; 2007.09.04 Dan
;;;             : * Modified parse-bold-predictions-for-graph so that it can
;;;             :   draw data from a subset of the time sequence (provided by
;;;             :   start and end times in the new dialog.
;;; 2007.10.10 Dan [1.1]
;;;             : * Adding a saftey check to predict-bold-response to catch
;;;             :   when there isn't a sample's worth of time available.
;;; 2008.04.29 Dan [1.2]
;;;             : * Changed the default values for the scale and exponent to
;;;             :   .75 and 6 respectively.
;;; 2010.07.14 Dan [2.0]
;;;             : * Adding a negative component to the hemodynamic curve.
;;;             :   It is now composed of two gamma distributions one of which
;;;             :   is subtracted from the other.  There are separate scale and
;;;             :   exponent parameters for each distribution and there are two
;;;             :   additional parameters used as the coefficients in combining
;;;             :   the curves.  The predicted values are now computed based on:
;;;             :
;;;             :       (c1*positive - c2*negative)/ (c1 - c2)
;;;             :
;;;             :   where c1 is the :bold-positive parameter, c2 is the :bold-negative
;;;             :   parameter, positive is the distribution generated from the :bold-scale
;;;             :   and :bold-exp parameters, and negative is the distribution generated
;;;             :   from the :neg-bold-scale and :neg-bold-exp parameters.
;;;             :
;;;             :   The defaults for c1 and c2 are 1 and 0 which leaves the calculation
;;;             :   the same as it was previously.
;;;             :   
;;;             :   One suggestion for useful values would be to set all of the
;;;             :   parameters like this which would be the defaults used by SPM
;;;             :   for computing hemodynamic curves:
;;;             :   
;;;             :    :bold-exp 5 
;;;             :    :bold-scale 1 
;;;             :    :neg-bold-exp 15 
;;;             :    :neg-bold-scale 1 
;;;             :    :bold-positive 6 
;;;             :    :bold-negative 1
;;;             :        
;;; 2010.07.16 Dan
;;;             : * Changed bold-point to ignore possible underflow errors and just
;;;             :   return 0.
;;; 2010.07.20 Dan
;;;             : * Updated the environment code that generates Tcl/Tk colors to
;;;             :   just use black for negatives as well.
;;; 2011.03.04 Dan
;;;             : * Added a bold-demand-functions function that works like predict-
;;;             :   bold-response except it doesn't convolve with the hemodynamic 
;;;             :   response curve. So the results are 1 if the buffer was busy during 
;;;             :   that time and 0 if not with the times being reported at the start 
;;;             :   of an increment instead of the midpoints.  So if one sets 
;;;             :   :bold-inc to .001 it would report 0/1 for free/busy for the
;;;             :   buffer over the whole run.
;;; 2011.04.25 Dan
;;;             : * Added the to do about converting to ms internally.
;;; 2011.04.28 Dan
;;;             : * Added a declare and removed some unused variables to quiet
;;;             :   compiler warnings.
;;; 2011.05.13 Dan
;;;             : * Modified the bold graphic code to return the data with a
;;;             :   color attached in the same way the graphic traces generate
;;;             :   it.
;;; 2011.06.03 Dan
;;;             : * Adjusted the BOLD display functions for the environment to 
;;;             :   better handle bad stuations.
;;; 2011.09.13 Dan
;;;             : * Added the require-compiled for environment-colors since 
;;;             :   this uses *gt-colors*.
;;; 2013.01.09 Dan
;;;             : * Changed the environment data cache to an equalp hashtable
;;;             :   since the keys are now a cons of the handler name and the
;;;             :   window to prevent issues with multi-environment settings.
;;; 2013.12.12 Dan
;;;             : * Actually changed the :bold-scale parameter to 1 like it
;;;             :   said in the 2.0 update.
;;; 2014.08.01 Dan [3.0]
;;;             : * Cleaning up some issues with the BOLD calculation:
;;;             :   - The implementation of the gammapdf function here is
;;;             :     computing with an exponent of (a+1) instead of a.
;;;             :     That's why the default used to be 5 which would match
;;;             :     up with the value 6 used in the SPM calculations.
;;;             :     Now it computes using the right value i.e. if the
;;;             :     :bold-exp parameter is set to 6 the gammapdf is computed
;;;             :     with an exponent of 6.
;;;             :   - The default values are thus now being set as follows:
;;;             :
;;;             :      :bold-exp 6
;;;             :      :bold-scale 1 
;;;             :      :neg-bold-exp 16 
;;;             :      :neg-bold-scale 1 
;;;             :      :bold-positive 6 
;;;             :      :bold-negative 1
;;;             :    
;;;             :     with a negative component by default.
;;;             :   - The way that positive and negative values were combined
;;;             :     previously was scaling values based on the difference 
;;;             :     between the factors.  Now it's simply scaling
;;;             :     them and subtracting the negative:
;;;             :     (:bold-positive * positive-value)/(max :bold-positive :bold-negative) -
;;;             :     (:bold-negative * negative-value)/(max :bold-positive :bold-negative)
;;;             :   - The sliding window for efficiency (the :bold-settle 
;;;             :     parameter) now uses the "off" time of the signal to determine
;;;             :     the window so that it still captures activity which starts
;;;             :     outside of the window but continues into the window.
;;;             :   - Renamed a bunch of functions and parameters for the internal 
;;;             :     code so that it may be easier to follow what's happening.
;;;             :   - If there's a currently busy module when computing the 
;;;             :     trace lists don't drop it anymore -- consider it closed at
;;;             :     the current time.
;;; 2014.08.05 Dan
;;;             : * When splitting a demand longer than the increment into pieces
;;;             :   do it with equal length segments instead of blocks increment
;;;             :   long plus the remainder.
;;;             : * The :bold-inc is now 2 instead of 1.5.
;;; 2014.08.06 Dan
;;;             : * In order to maintain consistency with existing models a new
;;;             :   parameter is being added, :bold-param-mode.  If it is set
;;;             :   to act-r (the default) then the parameter settings will still
;;;             :   be handled as they were before (exponent needing to be set to
;;;             :   1 less than "desired" and scaling based on the difference between
;;;             :   the factors).  If it is set to spm then the parameters will be
;;;             :   handled as described above in the 3.0 update.
;;;             : * The default values will be set based on the act-r mode:
;;;             :
;;;             :      :bold-exp 5
;;;             :      :bold-scale 1 
;;;             :      :neg-bold-exp 15
;;;             :      :neg-bold-scale 1 
;;;             :      :bold-positive 6 
;;;             :      :bold-negative 1
;;;             :      :bold-inc 2
;;;             :
;;;             :  Setting the mode to spm will automatically change the exp
;;;             :  parameters to 6 and 16 if they are 5 and 15, otherwise it will
;;;             :  leave them where they are, and setting it to act-r will change
;;;             :  them from 6 and 16 to 5 and 15.  If you want to set them to 5 and 15
;;;             :  in spm mode (or 6 and 16 in act-r mode) then that must be done after 
;;;             :  setting spm mode i.e.
;;;             :  this will work: (sgp :bold-param-mode spm :bold-exp 5 :neg-bold-exp 15)
;;;             :  this will not: (sgp :bold-exp 5 :neg-bold-exp 15 :bold-param-mode spm).
;;; 2014.08.07 Dan
;;;             : * Added the module-demand-time function, renamed bold-demand-
;;;             :   functions to module-demand-functions, and added a similar
;;;             :   function module-percent-demand which returns the proportion of
;;;             :   time that the module was busy during the step instead of just
;;;             :   1 or 0.
;;;             : * Those functions also allow specifying a step directly instead
;;;             :   of having to change :bold-inc.
;;; 2015.06.10 Dan
;;;             : * Update how the bold graph is drawn in the environment so 
;;;             :   that it only draws the axes once.  Leave the times in seconds
;;;             :   for the graph range since a ms here or there shouldn't matter.
;;;             : * Changed the run-time bold viewer so that it only needs to 
;;;             :   calculate the "current" value instead of doing everything
;;;             :   for each update.
;;; 2015.06.11 Dan
;;;             : * Convert the times to a rational of seconds to avoid loss of
;;;             :   accuracy.
;;;             : * Use the safe-seconds->ms on input values just to warn of any
;;;             :   possible issues even though the calculations are actually 
;;;             :   done with the seconds.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; After running a model, you can call the predict-bold-response function to get a
;;; printout of the BOLD predictions for all of the buffers that were traced
;;; during the run.  It will return a list of lists where each sub-list is the
;;; data for a buffer which was traced.  The car of the list is the buffer's name
;;; and the rest is the BOLD values spaced by the :bold-inc time (where the first
;;; point occurs at :bold-inc/2 not 0).  Wrapping the call with no-output will 
;;; prevent the printing if one only wants to get the data returned.
;;;
;;; To use the tool you must set the parameter :save-buffer-trace to t in the
;;; model.  The buffers set with the :traced-buffers parameter will be the ones
;;; for which the predictions will be printed.
;;;
;;; There are two mechanisms that can be used to determine the bold response -
;;; either point based which assumes each request occurs instantaneously at the
;;; time of the request or interval based which considers all the time which
;;; the module spends as busy during the processing of the request.  For most
;;; modules the interval based approach seems the more reasonable one, but for
;;; a module like the goal module, which spends no time busy with requests, the
;;; point prediction would have to be used to produce any predictions.  There
;;; is a parameter which controls how each buffer is computed, and by default
;;; only the goal buffer uses the point based prediction (visual-location is
;;; another buffer for which there is no time spent busy with requests, but
;;; predictions for visual processing are typically done by tracking the visual
;;; buffer).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Commands:
;;;
;;; predict-bold-response {start {end}}
;;;
;;;  Prints out and returns the BOLD response predictions based on module requests 
;;;  through buffers after a model has been run.  The optional parameters start and
;;;  end can be provided to specify the starting and ending times for which the 
;;;  predictions should be generated.  The defaults are 0 for the start and the
;;;  current model time for the end if not provided.  The start time should be 
;;;  a multiple of the time increment (the :bold-inc parameter).  If it is not,
;;;  then a warning will be printed and the closest lower multiple will be used.
;;;
;;; Parameters:
;;;
;;; :bold-param-mode (default act-r)
;;;
;;;   Possible values are act-r or spm.  Determines how the exp and scaling paramters
;;;   are used to generate the BOLD response.
;;;
;;; :bold-scale (default 1)
;;;
;;;  The scale parameter used in the computation of the positive component of the BOLD response.
;;;
;;; :bold-exp (default 6)
;;;
;;;  The exponent (shape) parameter used in the computation of the positive component of the BOLD response.
;;;
;;; :neg-bold-scale (default 1)
;;;
;;;  The scale parameter used in the computation of the negative component of the BOLD response.
;;;
;;; :bold-exp (default 16)
;;;
;;;  The exponent (shape) parameter used in the computation of the negative component of the BOLD response.
;;;
;;; :bold-positive (default 6)
;;;
;;;  The coefficient for the positive component of the BOLD response.
;;;
;;; :bold-negative (default 1)
;;;
;;;  The coefficient for the negative component of the BOLD response.
;;;
;;; :bold-inc (default 2)
;;; 
;;;  The intervals at which the BOLD computation is computed in seconds with the first one 
;;;  occurring at :bold-inc/2.
;;;
;;; :point-predict (default (goal))
;;;
;;;  The list of buffers for which the point prediction mechanism will be used.
;;;
;;; :bold-settle (default 40)
;;;
;;;  The time in seconds used as the window of time for computing the 
;;;  bold value - the value at time T is based on the activity that has
;;;  happened between time  (- T :bold-settle) and time T.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;;
;;; This computes the convolution of the module demand curve with a gammapdf
;;; function representing the hemodynamic response curve.  The demand curve
;;; is assumed to be a square wave of height 1 thus the convolution is
;;; just the area under the gammapdf.
;;; 
;;; The Gammapdf function used is: (t/b)^a * [e^(-t/b)] / (b*a!)
;;; which technically is a gammapdf with a value a' = a+1 to simplify the 
;;; computation.  The user setting for a (the bold-exp parameter) is decremented
;;; by one for the computation.
;;;
;;; When computing the area under the curve the trapezoidal rule is used to
;;; approximate the area using the endpoints and the midpoint.  To improve the 
;;; approximation further the demand is broken into segements that are no larger
;;; than the current increment (split into equal length subsegments).
;;;
;;; Both a positive and negative component are computed separately and the result
;;; is the positive component minus the negative component weighted by the factors
;;; provided.  Since the area under the gammapdf is 1 the maximum BOLD value will
;;; be slightly less than 1 if the negative component has a non-zero weight.
;;; 
;;; For the modules with 0 duration demand curves the point based predictions
;;; assume that the demand at that time has an area of 1 (basically a Dirac delta 
;;; function) which means that the convolution is just the value of the gammapdf 
;;; at that time.  If the point based preditions are used when the module has
;;; >0 demand duration it uses only the start time of the demand.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "ENVIRONMENT-COLORS" "ACT-R-support:environment-colors")

(defun bold-buffer-record-times (x)
  (/ (buffer-record-ms-time x) 1000))

(defstruct bold-module
   scale (exp 0) inc settle point env-cache v cmdt buffers max-table c1 c2 (neg-exp 0) neg-scale mode)

(defun bold_partial_gammapdf (time a b)
  "Compute the gammapdf function without this part: 1/(b*a!) because that will be multiplied through at the end"
  ;; This computation technically does the calculation for a+1 but the module subtracts
  ;; one from the user's setting so that it all works out as one would expect.
  ;; (t/b)^a * [e^(-t/b)] / b*a!
  (let* ((scale (/ time b))
         (result (ignore-errors (* (expt scale a) (exp (- scale))))))
     (if result result 0)))


(defun area-under-bold-subsegment (time time1 time2 a b)
  "Compute the area under the gammapdf using the trapezoidal rule with the ends and midpoint and inverting the time for the convolution"
  (* (+ (bold_partial_gammapdf (- time time1) a b) 
        (* 2 (bold_partial_gammapdf (- time (/ (+ time1 time2) 2)) a b)) 
        (bold_partial_gammapdf (- time time2) a b)) 
     (- time2 time1) 
     .25))

(defun area-under-bold-curve (time time1 time2 inc a b)
  "Compute the area under the bold curve for the demand from time1 to time2 in blocks no bigger than the increment"
  (setf time2 (min time time2))
  (if (= time1 time2)
      0
    (let* ((width (- time2 time1))
           (steps (ceiling width inc))
           (step (/ width steps)))
      (do ((current 1 (1+ current))
           (x (- time2 step) (- x step))
           (y time2 x)
           (sum 0 (+ sum (area-under-bold-subsegment time x y a b))))
          ((= current steps) (+ sum (area-under-bold-subsegment time time1 y a b)))))))

(defun sum-bold-demands (times time inc a b point-based?)
  "Iterate over the demand segments and sum the area"
   (do ((temp times (cdr temp))
        (sum 0 (+ sum (if point-based?
                          (bold_partial_gammapdf (- time (caar temp)) a b)
                        (area-under-bold-curve time (caar temp) (cdar temp) inc a b)))))
       ((or (null temp) (> (caar temp) time)) sum)))


(defun compute-bold-values (times start end inc settle a b point-based?)
  (let* (results
         (time (+ start (/ inc 2))))
     (while (<= time (- end (/ inc 2)))
       (setf times (member (- time settle) times :test '< :key (if point-based? 'car 'cdr)))
       (push-last (sum-bold-demands times time inc a b point-based?) results)
       (incf time inc))
    results))

(defun bold-predict (times start end inc settle c1 c2 ap bp pos-factor an bn neg-factor point-based?)
  (let ((pos (unless (zerop c1)
               (compute-bold-values times start end inc settle ap bp point-based?)))
        (neg (unless (zerop c2)
               (compute-bold-values times start end inc settle an bn point-based?))))
    
    (cond ((and (not (zerop c1)) (not (zerop c2)))
           (mapcar (lambda (p n) (- (* p pos-factor) (* n neg-factor))) pos neg))
          ((zerop c2) (mapcar (lambda (p) (* p pos-factor)) pos))
          (t (mapcar (lambda (n) (- (* n neg-factor))) neg)))))


(defun bold_factorial (n)
   (do* ((i n (1- i))
         (v i (* v i)))
        ((= i 1) v)))

(defun parse-trace-lists-for-bold (bm)
   (let* ((trace (get-current-buffer-trace))
          (b (bold-module-buffers bm))
          (buffers (if (listp b) b (buffers)))
          (all-data nil))

     (dolist (x buffers)
       (let ((rects nil)
             (current-rect nil))
           (dolist (z trace)
             (let ((record (find x (buffer-record-buffers z) :key 'buffer-summary-name)))
               (if current-rect
                  (when (or (null (buffer-summary-busy record))
                             (buffer-summary-busy->free record)
                             (buffer-summary-request record))

                    (push-last (cons current-rect (bold-buffer-record-times z)) rects)
                    (if (buffer-summary-request record)
                         (setf current-rect (bold-buffer-record-times z))
                       (setf current-rect nil)))

                 (if (buffer-summary-busy record)
                     (if (and (buffer-summary-request record)
                              (or (buffer-summary-chunk-name record)
                                  (and (buffer-summary-error record)
                                       (not (buffer-summary-error->clear record)))
                                  (buffer-summary-busy->free record)))
                         (push-last (cons (bold-buffer-record-times z) (bold-buffer-record-times z)) rects)
                       (setf current-rect (bold-buffer-record-times z)))
                   (if (buffer-summary-request record)
                       (push-last (cons (bold-buffer-record-times z) (bold-buffer-record-times z)) rects)
                     (when (buffer-summary-chunk-name record)
                       (push-last (cons (bold-buffer-record-times z) (bold-buffer-record-times z)) rects)))))))
         
         (when current-rect (push-last (cons current-rect (mp-time)) rects))
         (push (cons x rects) all-data)))
     
     all-data))


(defun predict-bold-response (&optional (start 0) end)
  (verify-current-mp 
   "Predict-bold-response requires a current meta-process."
   (verify-current-model
    "Predict-bold-response requires a current model."
    (when (null end)
      (setf end (/ (mp-time-ms) 1000)))
    (unless (and (numberp start) (numberp end))
      (print-warning "Predict-bold-response requires start and end times to be numbers, but given ~s,~s." start end)
      (return-from predict-bold-response nil))
    (safe-seconds->ms start 'predict-bold-response)
    (safe-seconds->ms end 'predict-bold-response)
    (let* ((bm (get-module bold))
           (data (parse-trace-lists-for-bold bm))
           (bold nil)
           (point (bold-module-point bm))
           (inc (bold-module-inc bm))
           (c1 (bold-module-c1 bm))
           (c2 (bold-module-c2 bm))
           (ap (if (eq (bold-module-mode bm) 'spm)
                   (1- (bold-module-exp bm))
                 (bold-module-exp bm)))
           (an (if (eq (bold-module-mode bm) 'spm)
                   (1- (bold-module-neg-exp bm))
                 (bold-module-neg-exp bm)))
           (bp (bold-module-scale bm))
           (bn (bold-module-neg-scale bm))
           ;; since the 1/(b*factorial(a)) is a constant in the gammapdf function it can be
           ;; factored out and applied at the end along with the relative scaling between positive
           ;; and negative 
           (pos-factor (unless (zerop c1) 
                         (if (eq (bold-module-mode bm) 'spm)
                             (/ c1 (* (max c1 c2) bp (bold_factorial ap)))
                           (/ c1 (- c1 c2) bp (bold_factorial ap)))))
           (neg-factor (unless (zerop c2)
                         (if (eq (bold-module-mode bm) 'spm)
                             (/ c2 (* (max c1 c2) bn (bold_factorial an)))
                           (/ c2 (- c1 c2) bn (bold_factorial an))))))
      
      
      (if (< (- end start) inc)
          (print-warning "Sample time too short for BOLD predictions - must be at least :bold-inc seconds (currently ~s)" inc)
        (progn
          (unless (zerop (mod start inc))
            (setf start (* inc (floor start inc)))
            (model-warning "Start time should be a multiple of :bold-inc (~S).  Using start time of ~S." inc start))
          
          (dolist (x data)
            (push (cons (car x) (bold-predict (cdr x) start end inc (bold-module-settle bm) c1 c2 ap bp pos-factor an bn neg-factor (find (car x) point))) bold))
          
          ;; Cache the max values for each buffer so that they can
          ;; be used in normalizing things later if desired
          
          (dolist (x bold)
            (let* ((buffer (car x))
                   (data (cdr x))
                   (max (when data (apply #'max data))))
              (when (or (null (gethash buffer (bold-module-max-table bm)))
                        (> max (gethash buffer (bold-module-max-table bm))))
                (setf (gethash buffer (bold-module-max-table bm)) max))))
          (output-bold-response-data bold bm start end)
          bold))))))
   
(defun module-demand-times (&key (start 0) end)
  (verify-current-mp 
   "Module-demand-times requires a current meta-process."
   (verify-current-model
    "Module-demand-times requires a current model."
    (when (null end)
      (setf end (/ (mp-time-ms) 1000)))
    (if (and (numberp start) (numberp end))
        (progn
          (safe-seconds->ms start 'predict-bold-response)
          (safe-seconds->ms end 'predict-bold-response)
          (let* ((bm (get-module bold))
                 (data (parse-trace-lists-for-bold bm)))
            (reverse
             (mapcar (lambda (x)
                       (cons (car x) (remove-if (lambda (y)
                                                  (or (< (cdr y) start) (> (car y) end)))
                                                (cdr x))))
               data))))
      (print-warning "Start and end times for module-demand-times must be numbers, but given ~s,~s." start end)))))
  

(defun module-demand-functions (&key (start 0) end step)
  (verify-current-mp 
   "Module-demand-functions requires a current meta-process."
   (verify-current-model
    "Module-demand-functions requires a current model."
    (when (null step)
      (setf step (bold-module-inc (get-module bold))))
    (when (null end)
      (setf end (/ (mp-time-ms) 1000)))
    (if (and (numberp start) (numberp end) (numberp step))
        (progn
          (safe-seconds->ms start 'predict-bold-response)
          (safe-seconds->ms end 'predict-bold-response)
          
          (compute-module-demand start end step nil))
      (print-warning "Start, end, and step for module-demand-functions must be numbers, but given ~s,~s,~s." start end step)))))

(defun module-demand-proportion (&key (start 0) end step)
  (verify-current-mp 
   "Module-demand-proportion requires a current meta-process."
   (verify-current-model
    "Module-demand-proportion requires a current model."
    (when (null step)
      (setf step (bold-module-inc (get-module bold))))
    (when (null end)
      (setf end (/ (mp-time-ms) 1000)))
    (if (and (numberp start) (numberp end) (numberp step))
        (progn
          (safe-seconds->ms start 'predict-bold-response)
          (safe-seconds->ms end 'predict-bold-response)
          
          (compute-module-demand start end step t))
      (print-warning "Start, end, and step for module-demand-proportion must be numbers, but given ~s,~s,~s." start end step)))))


(defun compute-module-demand (start end step percent)
  (let* ((bm (get-module bold))
         (data (parse-trace-lists-for-bold bm))
         (bold nil))
    (dolist (x data)
      (push (cons (car x) (module-demand-internal (cdr x) step start end percent))
            bold))
      
    (output-module-demand-data bold bm start end step)
    bold))

(defun module-demand-internal (times step start end percent)
  (let ((results (make-list (ceiling (- end start) step) :initial-element 0)))
    (dolist (x times)
      (when (or (and (= (car x) (cdr x)) (>= (car x) start) (< (car x) end))
                (and (> (cdr x) start) (< (car x) end)))
        (let ((s (max 0 (- (car x) start)))
              (e (min (- (cdr x) start) (- end start))))
          (if (= (car x) (cdr x))
              (if percent
                  (incf (nth (floor s step) results) 1)
                (setf (nth (floor s step) results) 1))
            (do* ((index (floor s step) (1+ index))
                  (ending (multiple-value-list (floor e step))))
                ((if (zerop (second ending))
                     (= index (first ending))
                   (> index (first ending))))
              (if percent
                  (incf (nth index results) (- (* 1000 (min e (* (1+ index) step))) (* 1000 (max s (* index step)))))
                (setf (nth index results) 1)))))))
    (if percent
        (mapcar (lambda (x) (/ (/ x step) 1000)) results)
      results)))


(defun output-module-demand-data (data bm start end step)
  (when (and data (bold-module-v bm) (bold-module-cmdt bm))
    (do* ((times (list 'time))
          (x start (+ x step)))
         ((> x end) (push (reverse times) data))
      (push x times))    
    
    (let* ((transposed (apply 'mapcar 'list data))
           (max-len (+ 2 (apply 'max (mapcar (lambda (x) (length (symbol-name x))) (first transposed))))))
      
      (command-output "~?" (format nil "~~{~~~d,@a~~}" max-len) transposed)
      (command-output (format nil "~~{~~{~~~d,3f~~}~~%~~}" max-len) (cdr transposed)))))


        

(defun output-bold-response-data (data bm start end)
  (when (and data (bold-module-v bm) (bold-module-cmdt bm))
    (do* ((times (list 'time))
          (inc (bold-module-inc bm))
          (x (+ start (/ inc 2)) (+ x inc)))
         ((> x end) (push (reverse times) data))
      (push x times))    
    
    (let* ((transposed (apply 'mapcar 'list data))
           (max-len (+ 2 (apply 'max (mapcar (lambda (x) (length (symbol-name x))) (first transposed))))))
      
      (command-output "~?" (format nil "~~{~~~d,@a~~}" max-len) transposed)
      (command-output (format nil "~~{~~{~~~d,3f~~}~~%~~}" max-len) (cdr transposed)))))

(defun reset-bold-module (bm)
  (setf (bold-module-env-cache bm) (make-hash-table :test 'equalp)))

(defun create-bold-module (name) 
  (declare (ignore name))
  (let ((bm (make-bold-module)))
    (setf (bold-module-max-table bm) (make-hash-table))
    bm))



(defun handle-bold-params (instance param)
   (cond ((consp param)
          (case (car param)
            (:v
            (setf (bold-module-v instance) (cdr param)))
            (:cmdt
             (setf (bold-module-cmdt instance) (cdr param)))
            (:traced-buffers
             (setf (bold-module-buffers instance) (cdr param)))
            (:bold-scale
             (setf (bold-module-scale instance) (cdr param)))
            (:bold-exp
             (setf (bold-module-exp instance) (cdr param)))
            (:bold-settle
             (setf (bold-module-settle instance) (cdr param)))
            
            (:neg-bold-exp
             (setf (bold-module-neg-exp instance) (cdr param)))
            (:neg-bold-scale
             (setf (bold-module-neg-scale instance) (cdr param)))
            (:bold-positive
             (setf (bold-module-c1 instance) (cdr param)))
            (:bold-negative
             (setf (bold-module-c2 instance) (cdr param)))
            
            (:bold-inc
             (setf (bold-module-inc instance) (cdr param)))
            (:point-predict
             (setf (bold-module-point instance) (cdr param)))
            (:bold-param-mode
             (cond ((and (eq (cdr param) 'act-r) (= (bold-module-exp instance) 6) (= (bold-module-neg-exp instance) 16))
                    (setf (bold-module-exp instance) 5)
                    (setf (bold-module-neg-exp instance) 15))
                   ((and (eq (cdr param) 'spm) (= (bold-module-exp instance) 5) (= (bold-module-neg-exp instance) 15))
                    (setf (bold-module-exp instance) 6) 
                    (setf (bold-module-neg-exp instance) 16)))
             (setf (bold-module-mode instance) (cdr param)))))
         (t
          (case param
            (:bold-scale
             (bold-module-scale instance))
            (:bold-exp
             (bold-module-exp instance))
            
            (:neg-bold-exp
             (bold-module-neg-exp instance))
            (:neg-bold-scale
             (bold-module-neg-scale instance))
            (:bold-positive
             (bold-module-c1 instance))
            (:bold-negative
             (bold-module-c2 instance))
            
            (:bold-settle
             (bold-module-settle instance))
            (:bold-inc
             (bold-module-inc instance))
            (:point-predict
             (bold-module-point instance))
            (:bold-param-mode
             (bold-module-mode instance))))))

(define-module-fct 'bold nil
   (list
    (define-parameter :v :owner nil)
    (define-parameter :cmdt :owner nil)
    (define-parameter :traced-buffers :owner nil)
    (define-parameter :bold-param-mode
      :valid-test (lambda (x) (or (eq x 'act-r) (eq x 'spm)))
      :warning "either act-r or spm"
      :default-value 'act-r
      :documentation "Set how exp and scale parameters determine the hemodynamic response curve.")
    (define-parameter :bold-scale
      :valid-test 'numberp
      :warning "a number"
      :default-value 1.0
      :documentation "Scale parameter for computing the BOLD response.")
    (define-parameter :bold-exp
      :valid-test 'integerp
      :warning "an integer"
      :default-value 5
      :documentation "Exponent parameter for computing the BOLD response.")
    
    (define-parameter :neg-bold-scale
      :valid-test 'numberp
      :warning "a number"
      :default-value 1.0
      :documentation "Scale parameter for computing a negative component of the BOLD response.")
    (define-parameter :neg-bold-exp
      :valid-test 'integerp
      :warning "an integer"
      :default-value 15
      :documentation "Exponent parameter for computing a negative component of the BOLD response.")
    
    (define-parameter :bold-positive
      :valid-test 'nonneg
      :warning "a non-negative number"
      :default-value 6
      :documentation "Factor for the positive component of the hemodynamic response curve.")
    (define-parameter :bold-negative
      :valid-test 'nonneg
      :warning "a non-negative number"
      :default-value 1
      :documentation "Factor for the negative component of the hemodynamic response curve.")
    

    (define-parameter :bold-inc
      :valid-test 'posnum
      :warning "a positive number"
      :default-value 2
      :documentation "Time increment in seconds for computing the BOLD response.")
    (define-parameter :bold-settle
      :valid-test 'posnum
      :warning "a positive number"
      :default-value 40
      :documentation "Time window in seconds for computing the BOLD response.")
    (define-parameter :point-predict
      :valid-test (lambda (x) (and (listp x) (every (lambda (y) (find y (buffers))) x)))
      :warning "a list of buffer names"
      :default-value (list 'goal)
      :documentation "List of buffers for which the point based computation should be used to compute the BOLD response."))

   :creation 'create-bold-module
  :reset 'reset-bold-module
  :params #'handle-bold-params
   :version "3.0"
   :documentation "A module to produce BOLD response predictions from buffer request activity.")

(defun bold-data-buffer-max (buffer)
  (let ((bm (get-module bold)))
    (gethash buffer (bold-module-max-table bm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; The code below supports the tools available through the environment.
;;;; 

(defparameter *pixels-per-second-for-bold* 100)
(defparameter *bold-vertical-scale* 400)

(defun cache-bold-data (dialog)
  (let ((bm (get-module bold)))
    (aif (gethash dialog (bold-module-env-cache bm))
         it
         (setf (gethash dialog (bold-module-env-cache bm)) (predict-bold-response)))))

(defun uncache-bold-data (dialog)
  (let ((bm (get-module bold)))
    (remhash dialog (bold-module-env-cache bm)))
  (list 'gone))

(defun parse-bold-predictions-for-graph (chart buffer local start end)
  
  (let* ((d (no-output (cache-bold-data chart)))
         (all-data (when (> (length (car d)) 1)
                     (if local
                         (normalize-bold-data-for-env-local d)
                       (normalize-bold-data-for-env d))))
         (time-inc (car (no-output (sgp :bold-inc))))
         (dummy-data (cdar all-data))
         (data nil)
         (s-index (if (= start -1) 0 (min (floor start time-inc) (length dummy-data))))
         (e-index (if (= end -1) (length dummy-data) (min (floor end time-inc) (length dummy-data))))
         (s-time (* s-index time-inc)))
    
    (if (>= s-index e-index) 
        (push (list 'text "No data available for time requested" 40 40 "#f00") data)
      
      (if (null buffer)
          
          (progn ;; just draw the axes
            (push (list 'size (round (+ 50 (* (1+ (- e-index s-index)) time-inc *pixels-per-second-for-bold*)))
                        (+ *bold-vertical-scale* 40))
                  data)
            (push (list 'line 40 20 40 (+ 20 *bold-vertical-scale*) "#000") data)
            (push (list 'line 40 (+ 20 *bold-vertical-scale*) 
                        (round (+ 40 (* (- e-index s-index) time-inc *pixels-per-second-for-bold*))) 
                        (+ 20 *bold-vertical-scale*) "#000") data)
            (dotimes (i 11)
              (push (list 'line 35 (- (+ 20 *bold-vertical-scale*) (* i (/ *bold-vertical-scale* 10)))
                          45 (- (+ 20 *bold-vertical-scale*) (* i (/ *bold-vertical-scale* 10)))
                          "#000") 
                    data)
              (push (list 'text_y (format nil "~,1f" (* i .1)) 35 (- (+ 20 *bold-vertical-scale*) (* i (/ *bold-vertical-scale* 10)))
                          "#000") 
                    data))
            
            (dotimes (i (1+ (- e-index s-index)))
              (push (list 'line (+ 40 (* i time-inc *pixels-per-second-for-bold*)) (+ 15 *bold-vertical-scale*)
                          (+ 40 (* i time-inc *pixels-per-second-for-bold*)) (+ 25 *bold-vertical-scale*)
                          "#000") 
                    data)
              (push (list 'text_x (format nil "~,2f" (+ s-time (* i time-inc)))
                          (+ 40 (* i time-inc *pixels-per-second-for-bold*)) (+ 25 *bold-vertical-scale*)
                          "#000") 
                    data)))
        
        (let ((bd (cdr (find buffer all-data :key #'car))))
          (if bd
              (let* ((buffer-data (subseq bd s-index e-index))
                     (b (no-output (car (sgp :traced-buffers))))
                     (buffers (if (listp b) b (buffers)))
                     (colors (no-output (car (sgp :buffer-trace-colors))))
                     (color-list (create-color-list (if (and (listp b) colors) colors nil) buffers *gt-colors*))
                     (cur-color (nth (position buffer buffers) color-list)))
                  
                  (push (list 'color cur-color) data)
                  
                  (do ((p1 (butlast buffer-data) (cdr p1))
                       (p2 (cdr buffer-data) (cdr p2))
                       (inc (round (* *pixels-per-second-for-bold* time-inc)))
                       (time (+ 40 (round (* *pixels-per-second-for-bold* (/ time-inc 2))))
                             (round (+ time inc))))
                      ((null p1))
                    (push (list 'line time (- (+ 20 *bold-vertical-scale*)
                                              (* (car p1) *bold-vertical-scale*))
                                (+ time inc) (- (+ 20 *bold-vertical-scale*)
                                                (* (car p2) *bold-vertical-scale*)) cur-color)
                          data)))
              (push (list 'text "No Data Available" 40 40 "#f00") data)))))
  
  (let ((result nil))
    (dolist (x data)
      (push (format nil "~{~S ~}" x) result))
    result)))



(defun normalize-bold-data-for-env (data)
  (let ((max (apply #'max (mapcar (lambda (x) (apply #'max (cdr x))) data)))
        (new-data (copy-tree data)))
    (when (zerop max)
      (setf max 1.0))
    (dotimes (i (1- (length (car data))))
      (dolist (j new-data)
        (setf (nth (1+ i) j) (/ (nth (1+ i) j) max))))
    new-data))


(defun normalize-bold-data-for-env-local (data)
  (let ((new-data (copy-tree data)))
    (dolist (j new-data)
      (let ((max (apply #'max (cdr j))))
        (when (zerop max)
          (setf max 1.0))
        (dotimes (i (1- (length j)))
          (setf (nth (1+ i) j) (/ (nth (1+ i) j) max)))))
  new-data))


(defun bold-brain-data-results (chart local)
  (let* ((d (no-output (cache-bold-data chart)))
         (result nil)
         (size (length (first d)))
         )
    
    (if (<= size 1)
        (make-list 8 :initial-element (format nil "{} \"#000\""))
      (let ((data (if local
                      (normalize-bold-data-for-env-local d)
                    (normalize-bold-data-for-env d))))
        
        
        (do ((region '(manual goal vocal imaginal retrieval production aural visual) (cdr region))
             (color '("#~2,'0x0000" "#00~2,'0x00" "#0000~2,'0x" "#~2,'0x~2,'0x00" "#00~2,'0x~2,'0x" "#~2,'0x00~2,'0x" "#00~2,'0x00" "#0000~2,'0x") (cdr color))
             )
            ((null region))
          (let ((nums (assoc (car region) data)))
            (if nums
                (push (format nil "{} ~{~s ~}" 
                        (mapcar (lambda (x) 
                                  (if (= 2 (count #\x (car color)))
                                      (format nil (car color)
                                        (max 0 (floor (* (floor (* x 20)) (/ 255 20))))
                                        (max 0 (floor (* (floor (* x 20)) (/ 255 20)))))
                                    (format nil (car color) (max 0 (floor (* (floor (* x 20)) (/ 255 20)))))))
                          (cdr nums)))
                      result)
              (push (format nil "~{~a ~}" (make-list size :initial-element "{}")) result))))
        (reverse result)))))

(defun bold-brain-3d-data ()
  (let* ((d (no-output (predict-bold-response)))
         (result nil)
         (size (length (first d))))
    (if (<= size 1)
        (setf result (make-list 8 :initial-element (list 'none))) 
      (let ((data (normalize-bold-data-for-env-local d)))
        (dolist (region '(manual goal vocal imaginal retrieval production aural visual))
          (let ((nums (assoc region data)))
            (if nums
                (push (cons 'none (mapcar (lambda (x) 
                                            x)
                                    (cdr nums)))
                      result)
              (push (make-list size :initial-element 'none) result))))))
    (mapcar #'(lambda (x) (format nil "~{~s ~}" x)) (apply 'mapcar 'list (reverse result)))))


;; Code for the run-time viewer of bold data in the environment

(defvar *brain-scan-event* nil)

(defun remove-brain-scan (event)
  (declare (ignore event))
  (when *brain-scan-event*
    (delete-event *brain-scan-event*)
    (setf *brain-scan-event* nil))
  "close")

(defun start-brain-scan (event)
  (if (subtypep (type-of event) 'environment-handler)
    (let* ((bm (get-module bold))
           (inc (bold-module-inc bm)))
      
      (setf *brain-scan-event* (schedule-periodic-event inc 'brain-scan :module 'bold :priority :max :params (list event) :details "Brain-scan" :initial-delay inc :maintenance t))
      "{none none none none none none none none}")
    event))
        

(defun brain-scan (handler)
  (let* ((bm (get-module bold))
         (start (max 0 (* (bold-module-inc bm) (1- (floor (mp-time) (bold-module-inc bm))))))
         (d (no-output (predict-bold-response start)))
         (result ""))
        
        (dolist (region '(manual goal vocal imaginal retrieval production aural visual))
          (let ((nums (assoc region d)))
            (setf result (concatenate 'string result 
                           (if nums
                               (let* ((val (car (last nums)))
                                      (max (gethash region (bold-module-max-table bm))))
                                 (if (numberp val)
                                     (princ-to-string 
                                      (if (and max (numberp max) (not (zerop max)))
                                          (/ val max)
                                        val))
                                   "none"))
                                 "none")
                           " "))))
            
    (update-handler handler (concatenate 'string "{" result "}"))))



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
