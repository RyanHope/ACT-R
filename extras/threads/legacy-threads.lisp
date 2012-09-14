;;;--------------------------------------------------------------------------
;;;
;;;  Threaded Cognition
;;;  ACT-R Implementation
;;;  Dario Salvucci & Niels Taatgen
;;;  
;;;  [Original]
;;;  This file contains the code that implements the threaded cognition theory
;;;  of concurrent multitasking.  It has been tested on a Macintosh running
;;;  OS 10.4.9 and MCL 5.1 with ACT-R version 1.1 [r222].
;;;
;;;  [2/19/2009]
;;;  Updated for new version of ACT-R and to eliminate global variables.
;;;
;;;  [5/21/2009]
;;;  Updated to include Jelmer Borst's fix for a bug involving one goal.
;;;
;;;  [7/20/2009]
;;;  Dan: put the act-r-buffer-requested setting back into the modified
;;;  set-buffer-chunk function.

;;;  For more information please see:
;;;  Salvucci, D. D., & Taatgen, N. A. (2008).  Threaded cognition: An integrated
;;;  theory of concurrent multitasking.  Psychological Review, 115, 101-130.
;;;
;;;  Copyright 2008-2009 Dario Salvucci and Niels Taatgen
;;;
;;;--------------------------------------------------------------------------


(defstruct threads-module 
  goals 
  last-time 
  cr-time 
  cr-count)


(defun create-threads-module (model-name)
  (declare (ignore model-name))
  (make-threads-module))


(defun threads-reset (module)
  (setf (threads-module-goals module) nil)
  (setf (threads-module-last-time module) 0)
  (setf (threads-module-cr-time module) -1)
  (setf (threads-module-cr-count module) 0))


(defun threads-force-add-goal ()
  (let ((module (get-module threads)))
    (setf (threads-module-last-time module) (mp-time))))


(defun threads-update ()
  (let ((module (get-module threads)))
    (when (> (length (threads-module-goals module)) 1)
      (let ((buffer (buffer-instance 'goal)))
        (let ((chunk (act-r-buffer-chunk buffer)))
          (if (member chunk (threads-module-goals module))
              ;; not popped...
              (setf (threads-module-goals module) (append (remove chunk (threads-module-goals module)) (list chunk)))
            ;; popped...
            (setf (threads-module-goals module) (append (rest (threads-module-goals module)) (list (first (threads-module-goals module)))))))
        (setf (act-r-buffer-chunk buffer) (first (threads-module-goals module)))
        ))))


(defun threads-try-goal (index)
  (let ((module (get-module threads)))
    (let ((g (nth index (threads-module-goals module)))
          (buffer (buffer-instance 'goal)))
      (setf (act-r-buffer-chunk buffer) g))))


(defun threads-pprint ()
  (let ((module (get-module threads)))
    (pprint-chunks-fct (threads-module-goals module))))


(defun set-buffer-chunk (buffer-name chunk-name &key (requested t))
  "Forces a copy...."
  (verify-current-mp  
   "set-buffer-chunk called with no current meta-process."
   (verify-current-model
    "set-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning 
              "set-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning 
              "set-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            (t

             ;;------------------------------------------------------------------------
             ;; dds: added lines below
             
             (let* ((cname (copy-chunk-fct chunk-name)))
               (if (not (equalp buffer-name 'goal))
                 (progn
                   (when (act-r-buffer-chunk buffer)
                     (clear-buffer buffer-name))
                   (setf (act-r-buffer-chunk buffer) cname))
                 
                 (let ((module (get-module threads)))
                   (when (< (threads-module-last-time module) (mp-time))
                     (let ((chunk (act-r-buffer-chunk (buffer-instance 'goal))))
                       (setf (threads-module-goals module) (remove chunk (threads-module-goals module))))
                     (setf (threads-module-last-time module) (mp-time)))
                   (setf (threads-module-goals module) (append (threads-module-goals module) (list cname)))
                   (setf (act-r-buffer-chunk buffer) cname)

                   ;(setf (threads-module-goals module) (cons cname (threads-module-goals module)))
                   ;(setf (act-r-buffer-chunk buffer) (first (threads-module-goals module)))
                   
                   ;(format t "add: ") (threads-print)
                   ))
               
               ;; Dan: Put this back from the comments below since not having it breaks other things
               (setf (act-r-buffer-requested buffer) requested)
               
               ;;-----------------------------------------
               ;; jpb 090521: moved when-clause below from below, leaving out the second chunk copy
               (when (show-copy-buffer-trace)
                 (schedule-event-relative 0 'show-buffer-copy :maintenance t :module 'buffer 
                                          :priority :max 
                                          :details (concatenate 'string "Buffer " (string buffer-name) " copied chunk " (string chunk-name) " to " (string cname)) 
                                          :output 'medium)))))))))
           
             ;;------------------------------------------------------------------------
             ;; dds: removed the following lines (effectively replaced above)
             ;; 
             ;; (when (act-r-buffer-chunk buffer)
             ;;   (clear-buffer buffer-name))
             ;;------------------------------------------------------------------------
             
             ;;-----------------------------------
             ;; jpb 090521: removed following lines, copied first part to above
             ;;(let ((copy-name (copy-chunk-fct chunk-name)))
             ;;  (when (show-copy-buffer-trace)
             ;;    (schedule-event-relative 0 'show-buffer-copy :maintenance t :module 'buffer 
             ;;                             :priority :max 
             ;;                             :details (concatenate 'string "Buffer " (string buffer-name) " copied chunk " (string chunk-name) " to " (string copy-name)) 
             ;;                             :output 'medium))
             ;;(setf (act-r-buffer-chunk buffer) copy-name)))))))
             ;;-------------------------------------

(defun clear-buffer (buffer-name)
  (verify-current-mp  
   "clear-buffer called with no current meta-process."
   (verify-current-model
    "clear-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning 
              "clear-buffer called with an invalid buffer name ~S" 
              buffer-name))
            (t
             (let ((chunk (act-r-buffer-chunk buffer)))
               (when chunk

                 ;;------------------------------------------------------------------------
                 ;; dds: added lines below
                 
                 (if (not (equalp buffer-name 'goal))
                   (setf (act-r-buffer-chunk buffer) nil)
                   
                   (let ((module (get-module threads)))
                     (when (< (threads-module-last-time module) (mp-time))
                       (let ((chunk (act-r-buffer-chunk (buffer-instance 'goal))))
                         (setf (threads-module-goals module) (remove chunk (threads-module-goals module))))
                       (setf (threads-module-last-time module) (mp-time)))
                     (if (threads-module-goals module)
                       (setf (act-r-buffer-chunk buffer) (first (threads-module-goals module)))
                       (setf (act-r-buffer-chunk buffer) nil))
                     ))
                 
                 ;;------------------------------------------------------------------------
                 ;; dds: removed the following lines (effectively replaced above)
                 ;; 
                 ;;   (setf (act-r-buffer-chunk buffer) nil)
                 ;; 
                 ;;------------------------------------------------------------------------
             
                 (dolist (module (notified-modules))
                   (notify-module module buffer-name chunk)))
               chunk)))))))


(defun production-fired (procedural production)
  ;(if t 
  ; productions can't fail to fire 
  ;(< (act-r-random 1.0)
  ;   (or (production-chance production)
  ;       (production-p production)))
  
  
  (dolist (x (production-actions production))
    (when (car x)
      (funcall (car x))))
  
  ;; This never happens now
  ;  (schedule-event-relative 0 'production-failed :module 'procedural
  ;                           :prioriy :max :output 'low))
  
  (learn-parameters (production-name production))

  ;;------------------------------------------------------------------------
  ;; dds: added lines below
  
  (schedule-event-relative 
   0 
   'update-threads :module 'procedural 
   :priority :min 
   :destination 'procedural
   :output 'medium)
  
  ;;------------------------------------------------------------------------
  ;; dds: removed the following lines (effectively replaced above)
  ;; 
  ;; 
  ;; (schedule-event-relative 
  ;;  0 
  ;;  'conflict-resolution :module 'procedural 
  ;;  :priority :min 
  ;;  :destination 'procedural
  ;;  :output 'medium)
  ;; 
  ;;------------------------------------------------------------------------

  (dolist (hook (procedural-cycle-hook procedural))
    (funcall hook (production-name production)))
  
  ;; Call this explicitly now...
  (compile-productions production)
  
  (setf (procedural-busy procedural) nil))


(defun update-threads (procedural)
  (declare (ignore procedural))
  
  (threads-update)
  
  (schedule-event-relative 
   0 
   'conflict-resolution :module 'procedural 
   :priority :min 
   :destination 'procedural
   :output 'medium))


(defun conflict-resolution (procedural)
  
  (setf (procedural-delayed-resolution procedural) nil)
  
  (setf (procedural-buffer-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural)) :initial-element :untested))
  (setf (procedural-slot-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural) (largest-chunk-type-size)) :initial-element :untested))
  
  (let* ((conflict-set nil)
         (hook-set nil)
         (best nil)
         (best-ut (minimum-utility))
         (mu best-ut))
    
    (if (or (null (procedural-use-tree procedural))
            (procedural-crt procedural))
        
        (progn
          (when (procedural-use-tree procedural)
            (model-warning "Conflict resolution not using the decision tree when :crt is enabled."))
             
          (dolist (production (procedural-productions procedural))
            
            (setf (production-bindings production) nil)
            (setf (production-failure-condition production) nil)
            
            (unless (production-disabled production)
              (when (procedural-crt procedural)
                (model-output "Trying production: ~s" (production-name production)))
              
              (let ((constant-tests (if (null (production-constants production)) (cons t nil)
                                      (do* ((tests (production-constants production) (cdr tests))
                                            (result (test-constant-condition procedural (car tests))
                                                    
                                                    (test-constant-condition procedural (car tests))))
                                           
                                           ((or (null result) (null (cdr tests))) (cons result (car tests)))))))
                
                (if (null (car constant-tests))
                    (progn
                      (setf (production-failure-condition production) (cdr constant-tests))
                      (when (procedural-crt procedural) ; report failures..
                        (model-output "Fails because: ")
                        (model-output (failure-reason-string (cdr constant-tests) procedural production))))
                  
                  (let ((user-bindings (if (null (production-binds production)) (cons t nil)
                                         (do* ((tests (production-binds production) (cdr tests))
                                               (result (test-and-perfrom-bindings procedural (car tests) production)
                                                       
                                                       (test-and-perfrom-bindings procedural (car tests) production)))
                                              
                                              ((or (null result) (null (cdr tests))) (cons result (car tests)))))))
                    
                    (if (null (car user-bindings))
                        (progn
                          (setf (production-failure-condition production) (cdr user-bindings))
                          (when (procedural-crt procedural) ; report failures..
                            (model-output "Fails because: ")
                            (model-output (failure-reason-string (cdr user-bindings) procedural production))))
                      
                      (let ((others-pass (if (null (production-others production)) (cons t nil)
                                           
                                           (do* ((conditions (production-others production) (cdr conditions))
                                                 (result (test-other-condition procedural (car conditions) production)
                                                         (test-other-condition procedural (car conditions) production)))
                                                ((or (null result) (null (cdr conditions))) (cons result (car conditions)))))))
                        
                        (if (null (car others-pass))
                            (progn
                              (setf (production-failure-condition production) (cdr others-pass))
                              (when (procedural-crt procedural) ; report failures..
                                (model-output "Fails because: ")
                                (model-output (failure-reason-string (cdr others-pass) procedural production))))
                          
                          
                          (let ((u (compute-utility (production-name production) t)))
                            
                            (when (procedural-crt procedural)
                              (model-output "Production ~s matches" (production-name production)))
                            
                            (push-last production conflict-set)
                            
                            (setf (production-conflict-val production) u)
                            
                            (when (and (procedural-crt procedural) mu (< u mu))
                              (model-output "Fails because:~%Utility ~s is below the threshold ~s" u mu))
                            
                            (cond ((or (null best-ut) (> u best-ut))
                                   (setf best-ut u)
                                   (setf best (list production)))
                                  ((= u best-ut)
                                   (if (procedural-er procedural)
                                       (push-last production best)
                                     (setf best (list production)))))))))))))))
      
      (let ((cs (get-valid-productions procedural)))
        
        (dolist (production (mapcar (lambda (x) (get-production-internal x procedural)) cs))
          
          
          (unless (production-disabled production)
            
            (let ((constant-tests (if (null (production-constants production)) (cons t nil)
                                    (do* ((tests (production-constants production) (cdr tests))
                                          (result (test-constant-condition procedural (car tests))
                                                  (test-constant-condition procedural (car tests))))
                                         
                                           ((or (null result) (null (cdr tests))) (cons result (car tests)))))))
                
                (if (null (car constant-tests))
                    (setf (production-failure-condition production) (cdr constant-tests))
                      
                  
                  (let ((user-bindings (if (null (production-binds production)) (cons t nil)
                                         (do* ((tests (production-binds production) (cdr tests))
                                               (result (test-and-perfrom-bindings procedural (car tests) production)
                                                       
                                                       (test-and-perfrom-bindings procedural (car tests) production)))
                                              
                                              ((or (null result) (null (cdr tests))) (cons result (car tests)))))))
                    
                    (if (null (car user-bindings))
                        (setf (production-failure-condition production) (cdr user-bindings))
                      
                      (let ((others-pass (if (null (production-others production)) (cons t nil)
                                           
                                           (do* ((conditions (production-others production) (cdr conditions))
                                                 (result (test-other-condition procedural (car conditions) production)
                                                         (test-other-condition procedural (car conditions) production)))
                                                ((or (null result) (null (cdr conditions))) (cons result (car conditions)))))))
                        
                        (if (null (car others-pass))
                            (setf (production-failure-condition production) (cdr others-pass))
                          
                          (let ((u (compute-utility (production-name production) t)))
                            
                            (push-last production conflict-set)
                            
                            (setf (production-conflict-val production) u)
                            
                            (cond ((or (null best-ut) (> u best-ut))
                                   (setf best-ut u)
                                   (setf best (list production)))
                                  ((= u best-ut)
                                   (if (procedural-er procedural)
                                       (push-last production best)
                                     (setf best (list production))))))))))))))))
          
    
    
    (when (and (listp best) best (procedural-er procedural))
      (setf best (permute-list best)))
    
    (when (procedural-conflict-set-hook procedural)
      (let ((val nil)
            (old-val nil)
        
            (cs-names (mapcar #'production-name
                        (sort (copy-list conflict-set)
                              #'(lambda (x y) 
                                  (sort-productions x y best))))))

        (dolist (hook (procedural-conflict-set-hook procedural))
          (when val
            (setf old-val val))
          (setf val (funcall hook cs-names))
          (unless (or (null val) (stringp val) (member val cs-names))
            (print-warning "Only productions in the conflict set, a string, or nil are valid return values of a conflict-set-hook function.")
            (setf val nil))
          (when (and val old-val)
            (print-warning "Multiple functions on the conflict-set-hook returned a value")))
        (setf hook-set (or val old-val))))
    
    (when (and (procedural-cst procedural) (procedural-v procedural))
      (dolist (x conflict-set)
        (print-instantiation x)))
    
    (cond ((null hook-set) ;; default mechanims are used
           (let ((best-production (car best))) ; not (car conflict-set) because that's only sorted for the hook
             (if best-production 
                 (progn
                   (schedule-event-relative 0 'production-selected 
                                            :module 'procedural
                                            :destination 'procedural
                                            :priority :max
                                            :params (list best-production)
                                            :details 
                                            (concatenate 'string
                                              (symbol-name 'production-selected)
                                              " "
                                              (symbol-name (production-name best-production))))
                   
                   (awhen (production-conflict-code best-production)
                          (dolist (code it)
                            (funcall code)))
                   
                   
                   (when (production-break best-production)
                     
                     (schedule-event-relative 0 'print-instantiation
                                             :module 'procedural
                                              :output nil
                                              :priority :max
                                              :params (list best-production))
                     
                     (schedule-break-relative 0 :priority :max 
                                              :details 
                                              (concatenate 'string
                                                (symbol-name 'production)
                                                " "
                                                (symbol-name (production-name best-production))))))
               
              ;;------------------------------------------------------------------------
              ;; dds: added the following lines
              ;; 
              
              (let ((module (get-module threads)))
                
                (when (< (threads-module-cr-time module) (mp-time))
                  (setf (threads-module-cr-time module) (mp-time))
                  (setf (threads-module-cr-count module) 1))
                (cond ((< (threads-module-cr-count module) (length (threads-module-goals module)))
                       (threads-try-goal (threads-module-cr-count module))
                       (incf (threads-module-cr-count module))
                       (conflict-resolution procedural))
                      (t
                       (when (> (length (threads-module-goals module)) 1)
                         (threads-try-goal 0))
                       (setf (procedural-delayed-resolution procedural) 
                             (schedule-event-after-change 'conflict-resolution
                                                          :module 'procedural
                                                          :destination 'procedural
                                                          :output 'medium))))
                )
              
              ;;------------------------------------------------------------------------
              ;; dds: removed the following lines (effectively replaced above)
              ;; 
              ;;
              ;; (setf (procedural-delayed-resolution procedural) 
              ;;       (schedule-event-after-change 'conflict-resolution
              ;;                              :module 'procedural
              ;;                              :destination 'procedural
              ;;                              :output 'medium))
              ;; 
              ;;------------------------------------------------------------------------
               
               )))
          
          ((symbolp hook-set) ;; an over-ride production specified
           
           (schedule-event-relative 0 'production-selected :module 'procedural
                                    :destination 'procedural :priority :max
                                    :params (list (get-production-internal hook-set procedural))
                                    :details 
                                    (concatenate 'string
                                      (symbol-name 'production-selected)
                                      " "
                                      (symbol-name hook-set)))
           
           (awhen (production-conflict-code (get-production-internal hook-set procedural))
                  (dolist (code it)
                    (funcall code)))
           
           (when (production-break (get-production-internal hook-set procedural))
            
             (schedule-event-relative 0 'print-instantiation
                                      :module 'procedural
                                      :output nil
                                      :priority :max
                                      :params (list (get-production-internal hook-set procedural)))
             
             (schedule-break-relative 0 :priority :max 
                                      :details (concatenate 'string
                                                 (symbol-name 'production)
                                                 " "
                                                 (symbol-name hook-set)))))
          
          ((stringp hook-set) ;; an abort selection reason provided
            (model-warning "conflict-set-hook function canceled selection because : ~a" hook-set)
            (schedule-event-relative (procedural-dat procedural) 'conflict-resolution
                                     :module 'procedural
                                     :destination 'procedural
                                     :output 'medium))
          (t ;; shouldn't happen but this is a saftey case
           (print-warning "Illegal conflict resolution situation occured. Contact Dan to let him know.")))))


(define-module-fct 'threads nil nil
                   :version "1.0a2"
                   :documentation "Threads module for multitasking models"
                   :creation #'create-threads-module
                   :reset #'threads-reset)


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
