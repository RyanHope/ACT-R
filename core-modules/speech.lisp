;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2005 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LICENSE.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : speech.lisp
;;; Version     : 4.0
;;; 
;;; Description : Source code for the ACT-R/PM Speech Module.  This Module
;;;             : is pretty brain-damaged but should get the job done
;;;             : for short, simple strings.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : [ ] Should a jam signal an error state?
;;; 
;;; ----- History -----
;;; 2004.12.24 Dan [First pass at moving to ACT-R 6]
;;;             : Changed name to speech and reset version to 1.0a1
;;;             :
;;;             : Moved *char-per-syllable into the module and made it a
;;;             :   real parameter :char-per-syllable.
;;;             : 
;;;             : Removed the state chunk from the initialize-instance method.
;;;             : 
;;;             : Added the :from :speech to the queue-command to the device
;;;             : 
;;;             : Not flagging any errors at this point.
;;;             :
;;;             : Not placing a response chunk into the buffer at this point.
;;;             :
;;;             : Changed art-rate-ht in register-art-time to art-time-ht
;;; 2005.01.08 Dan
;;;             : Removed the NEW-WORD-SOUND method since there's now
;;;             : an audio module.
;;; 2005.01.11 mdb
;;;             : * Added parameter doc strings.
;;;             : * Added toplevel commands.
;;;             : * Went to more generic methods on buffer interactions.
;;; 2005.01.12 Dan
;;;             : * Moved the device interface parameters to the device module.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium"  or ":output 'low" to some of the 
;;;             :   events scheduled to play friendly with the new detail level.
;;; 2005.04.23 Dan
;;;             : * Added the status printing function to the buffer definition.
;;;             : * Noticed that the last-command query is unhandled...
;;; 2005.06.15 Dan
;;;             : * Added a check and warning that the string parameter to speak
;;;             :   and subvocalize is actually a real string because it seems
;;;             :   to throw a hard error when passed a chunk-name...
;;;             : * Updated version to 2.2a2.
;;; 2006.09.08 Dan
;;;             : * Changed a parameter test from posnum to nonneg.
;;; 2007.01.10 Dan
;;;             : * Changed the version to 2.2.
;;;             : * Cleaned out most of the old (commented out) code.
;;;             : * Added a new parameter :subvocalize-detect-delay
;;;             :   which is the time required for a subvocalized speech act
;;;             :   to be detecteable by the audio module.  The reason for it
;;;             :   is because the default of .3 (the digit-detect-delay) seems
;;;             :   way off - the model can't keep up with hearing it's
;;;             :   own internal speech with that delay for short utterances
;;;             :   because it can generate them faster than it can hear them...
;;;             :   So, by making it a parameter people can adjust that as needed.
;;;             : * Changed the subvocalize action to create the internal sound
;;;             :   using new-other-sound so that the delay can be set.
;;;             : * Also changed the recode time on that to 0ms - because
;;;             :   it seems odd for a model to not immediately be able to understand 
;;;             :   what it's saying in it's own head.
;;;             : * Changed the get-articulation-time method to get-art-time
;;;             :   and made get-articulation-time a function which doesn't
;;;             :   require passing the module as a parameter.
;;;             : * Made the set/get articulation check to make sure there is
;;;             :   a current mp/model/module.
;;;             : * Removed the speech-utils from being loaded for now - does
;;;             :   that even still work or get used by anyone?
;;; 2007.03.15 Dan
;;;             : * Changed the speak action so that a model's own voice
;;;             :   is tagged with a location of self for the audio system
;;;             :   so a model can differentiate its own speech from another
;;;             :   model's!
;;; 2008.07.22 Dan
;;;             : * Why did this require central-parameters?  Taking that out.
;;; 2011.04.28 Dan
;;;             : * Changed a declare ignore to ignorable to supprress an ACL warning.
;;; 2011.05.17 Dan
;;;             : * Replaced queue-command calls with schedule-event-relative.
;;; 2011.05.18 Dan
;;;             : * Replaced calls to new-word-sound and new-other-sound with
;;;             :   the lower level sound creation code which has been changed
;;;             :   over to using ms internally to avoid having to do math on
;;;             :   times in seconds.
;;; 2014.02.12 Dan
;;;             : * The request chunk-types are no longer subtypes of speech-command
;;;             :   since that wasn't actually used for anything.
;;; 2014.05.16 Dan [3.0]
;;;             : * Start the conversion to type-less chunks.
;;;             : * In addition to isa speak string <text> one can now just do
;;;             :   speak <text> as the request.
;;; 2014.05.30 Dan
;;;             : * Use the test-for-clear-request function in pm-module-request.
;;; 2014.10.31 Dan
;;;             : * Changed the speak and subvocalize chunks to be of the
;;;             :   corresponding type.
;;; 2015.06.04 Dan [3.1]
;;;             : * Convert to using ms for internal math and convert user
;;;             :   times in seconds to ms with safe-seconds->ms.
;;;             : * Added an optional parameter to get-articulation-time so 
;;;             :   that it can be returned in ms if needed.
;;;             : * Add :time-in-ms t to all scheduled events and explicitly
;;;             :   convert the exec-time to ms.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2015.09.22 Dan [4.0]
;;;             : * Track and complete requests to the vocal buffer. Should mostly
;;;             :   just work since this is built upon movement-styles and that's 
;;;             :   benn generally updated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GENERAL-PM" "ACT-R-support:general-pm")

(defclass speech-module (pm-module)
  ((syllable-rate :accessor s-rate :initform 150)
   (subvocalize-delay :accessor subvocalize-delay :initform 300)
   (char-per-syllable :accessor char-per-syllable :initform 3)
   (art-time-ht :accessor art-time-ht :initarg :art-time-ht 
                :initform (make-hash-table :test 'equal)))
  (:default-initargs
    :version-string "4.0"
    :name :SPEECH))


(defmethod register-art-time ((spch-mod speech-module) (text string) (time number))
  (setf (gethash text (art-time-ht spch-mod)) (safe-seconds->ms time 'register-articulation-time)))


(defmethod get-art-time ((spch-mod speech-module) (text string) &optional time-in-ms)
  (let ((time
         (aif (gethash text (art-time-ht spch-mod))
              it
              (round (* (s-rate spch-mod) (/ (length text) (char-per-syllable spch-mod)))))))
    (if time-in-ms time (ms->seconds time))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; SPEAK movement style


(defStyle speak () text)

(defmethod num-to-prepare ((mvmt speak))
  3)

(defmethod compute-exec-time ((spch-mod speech-module) (mvmt speak))
  (init-time spch-mod))

(defmethod compute-finish-time ((spch-mod speech-module) (mvmt speak))
  (+ (exec-time mvmt) (get-art-time spch-mod (text mvmt))))


(defmethod feat-differences ((s1 speak) (s2 speak))
  (if (string= (text s1) (text s2))
    0
    2))


(defmethod queue-output-events ((spch-mod speech-module) (mvmt speak))
  (new-sound-event (make-instance 'word-sound-evt :onset (+ (mp-time-ms) (seconds->ms (exec-time mvmt))) 
                              :string (text mvmt) :location 'self))
  (schedule-event-relative (seconds->ms (exec-time mvmt)) 'output-speech :time-in-ms t :params (list (text mvmt)) :destination :device :module :speech))


;;;; SUBVOCALIZE movement style

(defStyle subvocalize speak text)

(defmethod queue-output-events ((spch-mod speech-module) (mvmt subvocalize))
  (new-sound-event (make-instance 'sound-event :onset (+ (mp-time-ms) (seconds->ms (exec-time mvmt)))
                              :duration (get-art-time spch-mod (text mvmt) t) :content (text mvmt) 
                              :delay (subvocalize-delay spch-mod) :recode 0 :location 'internal
                              :kind 'word)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dan 
;;; everything below here is additional stuff for the ACT-R 6 interface


(defun query-speech-module (speech buffer slot value)
  (if (and (eq slot 'state) (eq value 'error))
    nil
    (generic-state-query speech buffer slot value)))


(defmethod pm-module-last-cmd-name ((speech speech-module) buffer-name chunk-spec)
  (declare (ignorable speech buffer-name))
  (let ((main-spec (chunk-spec-slot-spec chunk-spec)))
    (and (= (length main-spec) 1)
         (or (eq 'speak (spec-slot-name (first main-spec)))
             (eq 'subvocalize (spec-slot-name (first main-spec))))
         (spec-slot-name (first main-spec)))))
    
  
(defmethod pm-module-request ((speech speech-module) buffer-name chunk-spec)
  (declare (ignorable speech buffer-name))
  (cond ((test-for-clear-request chunk-spec)
         (schedule-event-now 'clear :module :speech :destination :speech :output 'low))
        
        ((= (length (chunk-spec-slot-spec chunk-spec 'cmd)) 1)
         (let ((cmd (spec-slot-value (first (chunk-spec-slot-spec chunk-spec 'cmd)))))
           (case cmd
             ((speak subvocalize)
              (let ((string (verify-single-explicit-value chunk-spec 'string :speech cmd)))
                (if (stringp string)
                    (schedule-event-now cmd :destination :speech :params (list :text string :request-spec chunk-spec) 
                                        :module :speech :output 'low
                                        :details (format nil "~a ~a ~a" cmd :text string))
                  (progn
                    (complete-request chunk-spec)
                    (model-warning "String slot in a ~s request must be a Lisp string." cmd)))))
             (t
              (complete-request chunk-spec)
              (print-warning "Invalid command ~a sent to the vocal buffer" cmd)))))
        ((chunk-spec-slot-spec chunk-spec 'cmd)
         (complete-request chunk-spec)
         (print-warning "Invalid command to speech module specifies the cmd slot multiple times."))
        ;; allow a request of just speak <text> or subvocalize <text>
        (t
         (let ((speak-spec (chunk-spec-slot-spec chunk-spec 'speak))
               (subv-spec (chunk-spec-slot-spec chunk-spec 'subvocalize)))
           (if (and (= (length speak-spec) 1)
                    (stringp (spec-slot-value (first speak-spec))))
               (schedule-event-now 'speak :destination :speech 
                                   :params (list :text (spec-slot-value (first speak-spec)) :request-spec chunk-spec) 
                                   :module :speech :output 'low
                                   :details (format nil "~a ~a ~a" 'speak :text (spec-slot-value (first speak-spec))))
             (if (and (= (length subv-spec) 1)
                      (stringp (spec-slot-value (first subv-spec))))
                 (schedule-event-now 'subvocalize :destination :speech 
                                     :params (list :text (spec-slot-value (first subv-spec)) :request-spec chunk-spec) 
                                     :module :speech :output 'low
                                     :details (format nil "~a ~a ~a" 'subvocalize :text (spec-slot-value (first speak-spec))))
               (progn
                 (complete-request chunk-spec)
                 (print-warning "Invalid command to speech module does not specify a valid action."))))))))

    

(defun reset-speech-module (instance)
  (reset-pm-module instance)
    
  (chunk-type speech-command (cmd "speech command"))
  (chunk-type (speak (:include speech-command)) (cmd speak) string speak)
  (chunk-type (subvocalize (:include speech-command)) (cmd subvocalize) string subvocalize)
  
  (dolist (c '(speak subvocalize))
    (unless (chunk-p-fct c)
      (define-chunks-fct `((,c isa ,c)))
      (make-chunk-immutable c)))
  
  (dolist (c '(self internal))
    (unless (chunk-p-fct c)
      (define-chunks-fct `((,c name ,c)))
      (make-chunk-immutable c))))

(defun params-speech-module (speech param)
  (if (consp param)
      (case (car param)
        (:syllable-rate
         (setf (s-rate speech) (safe-seconds->ms (cdr param) 'sgp))
         (cdr param))
        (:char-per-syllable
         (setf (char-per-syllable speech) (cdr param)))
        (:subvocalize-detect-delay
         (setf (subvocalize-delay speech) (safe-seconds->ms (cdr param) 'sgp))
         (cdr param))
        )
    (case param
       (:syllable-rate
       (ms->seconds (s-rate speech)))
      (:char-per-syllable
       (char-per-syllable speech))
      (:subvocalize-detect-delay
       (ms->seconds (subvocalize-delay speech))))))

(define-module-fct :speech 
    (list (define-buffer-fct 'vocal :queries '(modality preparation execution processor last-command)
            :status-fn (lambda () 
                         (print-module-status (get-module :speech)))
            :trackable t))
  (list 
    (define-parameter :syllable-rate
     :valid-test 'nonneg 
     :default-value 0.15
     :warning "a non-negative number"
     :documentation "Seconds per syllable.")
   (define-parameter :subvocalize-detect-delay
     :valid-test 'nonneg 
     :default-value 0.3
     :warning "a non-negative number"
     :documentation "Sound detect time for a subvocalized word.")
   (define-parameter :char-per-syllable
     :valid-test 'posnum 
     :default-value 3
     :warning "a positive number"
     :documentation "Characters per syllable."))
  :version "4.0"
  :documentation "A module to provide a model with the ability to speak"
  :creation (lambda (x) 
              (declare (ignore x)) (make-instance 'speech-module))
  :reset #'reset-speech-module
  :query #'query-speech-module
  :request 'pm-module-request
  :params #'params-speech-module
  )

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Misc toplevel stuff

(defun register-articulation-time (string time)
  "Register the articulation time of a string."
  (verify-current-mp
   "No current meta-process.  Cannot set articulation time."
   (verify-current-model 
    "No current model.  Cannot set articulation time."
    (aif (get-module :speech)
         (cond ((or (not (numberp time)) (minusp time))
                (print-warning "Articulation time must be a non-negative number."))
               ((not (stringp string))
                (print-warning "Must specify a string for which the articulation time is to be set."))
               (t
                (register-art-time it string time)))
         (print-warning "No Speech module found.  Cannot set articulation time.")))))
    
(defun get-articulation-time (string &optional time-in-ms)
  "Return the articulation time of a string."
  (verify-current-mp
   "No current meta-process.  Cannot get articulation time."
   (verify-current-model 
    "No current model.  Cannot get articulation time."
    (aif (get-module :speech)
         (cond ((not (stringp string))
                (print-warning "Must specify a string for which to get the articulation time."))
               (t
                (get-art-time it string time-in-ms)))
         (print-warning "No Speech module found.  Cannot get articulation time.")))))

;;; backward compatibility only

(defun pm-register-articulation-time (string time)
  "Register the articulation time of a string."
  (register-articulation-time string time))


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
