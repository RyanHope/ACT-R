;;; This model simply tests the extended motor module actions by performing
;;; all the old and new actions at least once each with some instances of
;;; automatically releasing things.
;;;
;;; It is by no means a good model, nor does it completely test everything,
;;; but does verify that all the commands and new parameters work.
;;;

;;; Variables for changing the new parameters.  Set initially to the
;;; corresponding default values.
(defparameter *kct* .01)
(defparameter *krt* .04)
(defparameter *dp* nil)
(defparameter *de* t)
(defparameter *psd* 0)

;;; Variables for modifying the productions in the test model
(defvar *query* 'state)

;;; variable for recording 
(defvar *data* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (declare (ignorable win))
  (push-last (list :pressed (mp-time) key) *data*))

(defmethod rpm-window-key-release-event-handler ((win rpm-window) key)
  (declare (ignorable win))
  (push-last (list :released (mp-time) key) *data*))

(defmethod rpm-window-click-event-handler ((win rpm-window) pos)
  (declare (ignorable win))
  (push-last (list :clicked (mp-time) pos) *data*))

(defmethod rpm-window-click-release-event-handler ((win rpm-window) pos)
  (declare (ignorable win))
  (push-last (list :unclicked (mp-time) pos) *data*))

(defun run-test (name)
  (setf *data* nil)
  (funcall name)
  (summarize-tests *data*))

(defun summarize-tests (data)
  (do* ((start 0 end)
        (end (position :test data :key 'car :start (if start (1+ start) 0))
             (position :test data :key 'car :start (if start (1+ start) 0))))
       ((null start) data)
    (display-data (subseq data start end))))

(defun display-data (data)
  (format t "~&#####~%~S~%#####~%" (cdar data))
  (dolist (action '(:pressed :released :clicked :unclicked))
    (format t " ~a:~{~^~%~{   ~7,3f ~10S ~@[(~5,3f)~]~}~}~%" 
      action
      (let ((last nil))
        (mapcan (lambda (x) 
                  (when (eq (car x) action)
                    (list (prog1
                              (if last
                                  (list (second x) (third x) (- (second x) last))
                                (list (second x) (third x) nil))
                              (setf last (second x))))))
          data))))
  (format t " Held:~%")
  (dolist (pressed (remove-if-not (lambda (x)
                                    (eq (car x) :pressed))
                                  data))
    (format t "  ~10s ~@[~5,3f~]~%" (third pressed)
      (let ((release (find-if (lambda (x)
                                (and (eq (car x) :released) (eql (third x) (third pressed))))
                              data :start (position pressed data))))
        (when release
          (- (second release) (second pressed))))))
  (format t " Clicked:~%")
  (dolist (clicked (remove-if-not (lambda (x)
                                    (eq (car x) :clicked))
                                  data))
    (format t "  ~10s ~@[~{~10s ~5,3f~}~]~%" (third clicked)
      (let ((release (find-if (lambda (x) (eq (car x) :unclicked))
                              data :start (position clicked data))))
        (when release
          (list (third release) (- (second release) (second clicked))))))))

  
                                    


(defun test1 ()
  (dolist (hand '(same diff))
    (dolist (query '(state processor preparation))
      (dolist (processors '(t nil))
        (dolist (executions '(t nil))
          (let ((details (format nil "~s hand ~s processors ~s executions query ~s"
                           hand (if processors 2 1) (if executions 2 1) query))
                (*dp* processors)
                (*de* executions)
                (*query* query))
            (reset)
            (format t "########################~%~S~%########################~%" details)
            (push-last (cons :test details) *data*)
            (goal-focus-fct (car (define-chunks-fct `((isa goal test test1 param ,hand)))))
            (run 10)))))))


(defun test2 ()
  (push-last (cons :test "click and hold mouse") *data*)
  (reset)
  (start-hand-at-mouse)
  (goal-focus-fct (car (define-chunks (isa goal test test2))))
  (run 10))

(defun test3 ()
  (push-last (cons :test "Auto release key & mouse when moving hand") *data*)
  (let ((*krt* .025)
        (*kct* .025))
    (reset)
    
    (goal-focus-fct (car (define-chunks (isa goal test test3))))
    (run .5)
    (buffer-status manual-left manual-right)
    (run .75)
    (buffer-status manual-left manual-right)
    (run 5)))


(defun test4 ()
  (push-last (cons :test "Some various key pressing") *data*)
  (let ((*psd* .2))
    (reset)
    (goal-focus-fct (car (define-chunks (isa goal test test4))))
    (pbreak test4-7)
    (run 10)
    (buffer-status manual-left manual-right)
    (dolist (hand '(left right))
      (dolist (finger '(index middle ring pinkie thumb))
        (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger))))
    (run 10)
    (buffer-status manual-left manual-right)
    (dolist (hand '(left right))
      (dolist (finger '(index middle ring pinkie thumb))
        (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger))))
      ))
             
(defun test5 ()
  (push-last (cons :test "Move the fingers and hands around") *data*)
  (reset)
  (goal-focus-fct (car (define-chunks (isa goal test test5))))
  
  (pbreak test5-5 test5-9 test5-11 test5-12)
  
  (do ((time (run 10) (run 10)))
      ((= time 0.0))
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger))))))

(defun test6 ()
  (push-last (cons :test "Some type-key actions from non-home position.") *data*)
  (reset)
  (goal-focus-fct (car (define-chunks (isa goal test test6))))
  (run 10)
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger)))))

                  
(clear-all)

(define-model test-motor
    (sgp-fct (list :v t :trace-detail 'high :key-closure-time *kct* 
                   :key-release-time *krt* :dual-processor-stages *dp*
                   :dual-execution-stages *de* :peck-strike-distance *psd*))
  
  (chunk-type goal test param state)
  
  (define-chunks 
      (loc1 isa visual-location screen-x 10 screen-y 20) 
      (loc2 isa visual-location screen-x 100 screen-y 40))
  
  (install-device (open-exp-window "" :visible nil))
  
  (dolist (x '(test1 test2 test3 press2 same diff click hold move 
               hold-next to-mouse to-keys test4 test5 test6))
    (define-chunks-fct `((,x isa chunk))))
  
  (goal-focus-fct (car (define-chunks (isa goal test empty param empty state empty))))
  
  (p test1-first-press
     =goal>
     isa goal
     test test1
     state nil
     ?manual>
     state free
     ==>
     =goal>
     state press2
     +manual>
     isa press-key
     key "r")
  
  (p-fct `(test1-second-press-same
           =goal>
           isa goal
           test test1
           state press2
           param same
           ?manual>
           ,*query* free
           ==>
           -goal>
           +manual>
           isa press-key
           key "e"))
  
  (p-fct `(test1-second-press-diff
           =goal>
           isa goal
           test test1
           state press2
           param diff
           ?manual>
           ,*query* free
           ==>
           -goal>
           +manual>
           isa press-key
           key "i"))
  
  (p test2-start
     =goal>
     isa goal
     test test2
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa move-cursor
     loc loc1
     =goal>
     state click)
  (p test2-click
     =goal>
     isa goal
     test test2
     state click
     ?manual>
     state free
     ==>
     =goal>
     state hold
     +manual>
     isa click-mouse)
  (p test2-hold
     =goal>
     isa goal
     test test2
     state hold
     ?manual>
     state free
     ==>
     +manual>
     isa hold-mouse
     =goal>
     state move
     )
  (p test2-move
     =goal>
     isa goal
     state move
     test test2
     ?manual>
     state free
     ==>
     +manual>
     isa move-cursor
     loc loc2
     =goal>
     state release)
  (p test2-release
     =goal>
     isa goal
     test test2
     state release
     ?manual>
     state free
     ==>
     +manual>
     isa release-mouse
     -goal>)
  
  
  (p test3-start
     =goal>
     isa goal
     test test3
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa hold-key
     key shift
     =goal>
     state hold-next)
  
  (p test3-hold-k
     =goal>
     isa goal
     test test3
     state hold-next
     ?manual>
     preparation free
     ==>
     +manual>
     isa hold-key
     key "k"
     =goal>
     state to-mouse)
  

  
  (p test3-to-mouse
     =goal>
     isa goal
     test test3
     state to-mouse
     ?manual>
     state free
     ==>
     =goal>
     state hold-mouse
     +manual>
     isa hand-to-mouse)
  
  (p test3-hold
     =goal>
     isa goal
     test test3
     state hold-mouse
     ?manual>
     state free
     ==>
     +manual>
     isa hold-mouse
     =goal>
     state to-keys
     )
  (p test3-keys
     =goal>
     isa goal
     test test3
     state to-keys
     ?manual>
     state free
     ==>
     +manual>
     isa hand-to-home
     -goal>)
  
  (p test4
     =goal>
     isa goal
     test test4
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa hit-key
     key "t"
     =goal>
     state 1)
  (p test4-1
     =goal>
     isa goal
     test test4
     state 1
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "r"
     =goal>
     state 2)
  (p test4-2
     =goal>
     isa goal
     test test4
     state 2
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "i"
     =goal>
     state 3)
    (p test4-3
     =goal>
     isa goal
     test test4
     state 3
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "f"
     =goal>
       state 4)
    (p test4-4
     =goal>
     isa goal
     test test4
     state 4
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "n"
     =goal>
     state 5)
  (p test4-5
     =goal>
     isa goal
     test test4
     state 5
     ?manual>
     preparation free
     ==>
     +manual>
     isa hold-key
     key "x"
     =goal>
     state 6)
  (p test4-6
     =goal>
     isa goal
     test test4
     state 6
     ?manual>
     preparation free
     ==>
     +manual>
      isa move-finger-to-home
      hand right
      finger index
     =goal>
     state 7)
  (p test4-7
     =goal>
     isa goal
     test test4
     state 7
     ?manual>
     state free
     ==>
     +manual>
     isa all-fingers-to-home
     -goal>)
  
  (p test5
     =goal>
     isa goal
     test test5
     state nil
     ?manual>
     state free
     ==>
     +manual>
      isa move-to-key
      key "w"
     =goal>
     state 1)
  (p test5-1
     =goal>
     isa goal
     test test5
     state 1
     ?manual>
     state free
     ==>
     +manual>
      isa hold-punch
     hand left
     finger ring
     =goal>
     state 2)
  
  (p test5-2
     =goal>
     isa goal
     test test5
     state 2
     ?manual>
     state free
     ==>
     +manual>
      isa release-key
     key "w"
     =goal>
     state 3)
  
    (p test5-3
     =goal>
     isa goal
     test test5
     state 3
     ?manual>
     state free
     ==>
     +manual>
      isa hit-key
     key "w"
     =goal>
       state 4)
  (p test5-4
     =goal>
     isa goal
     test test5
     state 4
     ?manual>
     state free
     ==>
     +manual>
      isa hold-key
     key "w"
     =goal>
       state 5)
  (p test5-5
     =goal>
     isa goal
     test test5
     state 5
     ?manual>
     state free
     ==>
     +manual>
      isa release-key-to-home
     key "w"
     =goal>
       state 6)
  (p test5-6
     =goal>
     isa goal
     test test5
     state 6
     ?manual>
     state free
     ==>
     +manual>
      isa move-to-key
     key "g"
     =goal>
       state 7)
  (p test5-7
     =goal>
     isa goal
     test test5
     state 7
     ?manual>
     state free
     ==>
     +manual>
      isa hold-key
     key "g"
     =goal>
     state 8)
  (p test5-8
     =goal>
     isa goal
     test test5
     state 8
     ?manual>
     state free
     ==>
     +manual>
     isa prepare
     style hand-ply
     hand right
     r 1
     theta 3.14
     =goal>
     state 9)
    (p test5-9
     =goal>
     isa goal
     test test5
     state 9
     ?manual>
     state free
     ==>
     +manual>
     isa execute
     =goal>
       state 10)
  (p test5-10
     =goal>
     isa goal
     test test5
     state 10
     ?manual>
     state free
     ==>
     +manual>
     isa point-hand-at-key
     hand right
     to-key "y"
     =goal>
       state 11)
  (p test5-11
     =goal>
     isa goal
     test test5
     state 11
     ?manual>
     state free
     ==>
     +manual>
     isa hold-key
     key "k"
     =goal>
     state 12)
  (p test5-12
     =goal>
     isa goal
     test test5
     state 12
     ?manual>
     state free
     ==>
     +manual>
     isa all-fingers-to-home
     hand right
     -goal>
     )
  
   (p test6
     =goal>
     isa goal
     test test6
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa move-to-key
     key "t"
     =goal>
     state 1)
  (p test6-1
     =goal>
     isa goal
     test test6
     state 1
     ?manual>
     preparation free
     ==>
     +manual>
     isa type-key
     key "r"
     =goal>
     state 2)
  (p test6-2
     =goal>
     isa goal
     test test6
     state 2
     ?manual>
     preparation free
     ==>
     +manual>
     isa type-key
     key "t"
     =goal>
     state 3)
    (p test6-3
     =goal>
     isa goal
     test test6
     state 3
     ?manual-left>
       index free
       ?manual>
       buffer empty
     ==>
     +manual>
     isa move-finger-to-home
       hand left
       finger index
     -goal>)
  
  )
