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
  (reset)
  (let ((window (open-exp-window "window" :visible nil)))
    (install-device window)
    (run 2.0)))


(clear-all)

(define-model sample

(sgp :v t :esc t)

(chunk-type type key)
(chunk-type speak-word word)


(p start-concurrent-tasks
   ?goal>
     buffer empty
==>
   +goal>
     isa type
     key "a"
   +goal>
     isa speak-word
     word "hello"
)


(p type-letter
   =goal>
     isa type
     key =key
   ?manual>
      state free
==>
   +manual>
      isa press-key
      key =key
)


(p speak-word
   =goal>
     isa speak-word
     word =word
   ?vocal>
      state free
==>
   +vocal>
      isa speak
      string =word
))
