;;;--------------------------------------------------------------------------
;;;
;;;  Threaded Cognition
;;;  Sample Model
;;;  Dario Salvucci & Niels Taatgen
;;;  
;;;  This file contains a sample threaded model that performs two simple
;;;  concurrent tasks: repeatedly typing a letter and repeatedly speaking
;;;  a word.  Enter (m) to run a 2.0 sec trace of the model.
;;;
;;;  For more information please see:
;;;  Salvucci, D. D., & Taatgen, N. A. (2008).  Threaded cognition: An integrated
;;;  theory of concurrent multitasking.  Psychological Review, 115, 101-130.
;;;
;;;  Copyright © 2008 Dario Salvucci and Niels Taatgen
;;;
;;;--------------------------------------------------------------------------


(defun m ()
  (let ((window (open-exp-window "window" :visible nil)))
    (install-device window)
    (reset)
    (run 2.0)))


(clear-all)


(define-model sample

(sgp :v t :esc t)

(chunk-type do-concurrent-tasks)
(chunk-type type)
;(chunk-type speak)

(add-dm
 (goal isa do-concurrent-tasks))


(p start-concurrent-tasks
   =goal>
      isa do-concurrent-tasks
==>
   +goal>
      isa type
   +goal>
      isa speak
)


(p type-letter
   =goal>
      isa type
   ?manual>
      state free
==>
   +manual>
      isa press-key
      key a
)


(p speak-word
   =goal>
      isa speak
   ?vocal>
      state free
==>
   +vocal>
      isa speak
      string "hello"
)


(goal-focus goal)
)
