(defvar *response* nil)
(defvar *response-time* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (unless *response-time* (setf *response-time* (get-time)))
  (push (string key) *response*)) 

(defun simple-task (&optional which)
  
  (reset)
  
  (let* ((alphabet (list "a" "b" "c" "d" "e" "f" "g" "h" 
                         "i" "j" "k" "l" "m" "n" "o" "p" "q"
                         "r" "s" "t" "u" "v" "w" "x" "y" "z"))
         (prompt (first (permute-list alphabet)))
         (task (if (or (string-equal which "next") (string-equal which "previous"))
                   which
                 (if (zerop (act-r-random 2)) "next" "previous")))
         (time (+ 1500 (act-r-random 1000))))
    
    (push "z" alphabet)
    
    (install-device (open-exp-window "Simple task"))
    
    (add-text-to-exp-window :text prompt :x 130 :y 150)
    (proc-display)
    (schedule-event-relative time 'display-prompt :params (list task) :time-in-ms t :output 'medium)
    (setf *response* nil) 
    (setf *response-time* nil)
    
    (proc-display)
    (run 10 :real-time t)
        
    (list task  (and (= (length *response*) 2)
                     (< time *response-time*)
                     (or (and (string-equal task "next") (search (reverse *response*) alphabet :test 'string-equal))
                         (and (string-equal task "previous") (search *response* alphabet :test 'string-equal)))
                     t))))
          
    
(defun display-prompt (prompt)
  (clear-exp-window)
  (add-text-to-exp-window :text prompt :x 125 :y 150)
  (proc-display))

(defun flash-letter (prompt)
  (clear-exp-window)
  (add-text-to-exp-window :text prompt :x 125 :y 150)
  (proc-display))


(clear-all)

(define-model perceptual-motor-issues

    (sgp :seed (101 1))
    (sgp :v t :needs-mouse nil :show-focus t :trace-detail medium :er t)

(chunk-type letter name next previous)
(chunk-type task letter)

(add-dm
 (a isa letter name "a" next "b" previous "z")
 (b isa letter name "b" next "c" previous "a")
 (c isa letter name "c" next "d" previous "b")
 (d isa letter name "d" next "e" previous "c")
 (e isa letter name "e" next "f" previous "d")
 (f isa letter name "f" next "g" previous "e")
 (g isa letter name "g" next "h" previous "f")
 (h isa letter name "h" next "i" previous "g")
 (i isa letter name "i" next "j" previous "h")
 (j isa letter name "j" next "k" previous "i")
 (k isa letter name "k" next "l" previous "j")
 (l isa letter name "l" next "m" previous "k")
 (m isa letter name "m" next "n" previous "l")
 (n isa letter name "n" next "o" previous "m")
 (o isa letter name "o" next "p" previous "n")
 (p isa letter name "p" next "q" previous "o")
 (q isa letter name "q" next "r" previous "p")
 (r isa letter name "r" next "s" previous "q")
 (s isa letter name "s" next "t" previous "r")
 (t isa letter name "t" next "u" previous "s")
 (u isa letter name "u" next "v" previous "t")
 (v isa letter name "v" next "w" previous "u")
 (w isa letter name "w" next "x" previous "v")
 (x isa letter name "x" next "y" previous "w")
 (y isa letter name "y" next "z" previous "x")
 (z isa letter name "z" next "a" previous "y"))


(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk))

(p find-letter
   
   =visual-location>
     isa         visual-location
   
   ?visual>
     state       free
 ==>
   +visual>
     isa         move-attention
     screen-pos  =visual-location
   +imaginal>
     isa         task
     letter      nil
)

(p encode-letter
   
   =imaginal>
     isa         task
     letter      nil
   =visual>
     isa         text
     value       =letter
 ==>
   =imaginal>
     letter      =letter
)

(p respond-next
   
   =imaginal>
     isa         task
     letter      =letter
   =visual>
     isa         text
     value       "next"
   ?manual>
     state       busy
 ==>
   +retrieval>
     isa         letter
     previous    =letter
   +manual>
     isa         press-key
     key         =letter
)

(p respond-previous
   
   =imaginal>
     isa         task
     letter      =letter
   =visual>
     isa         text
     value       "previous"
   
   ?manual>
     state       free
 ==>
   +retrieval>
     isa         letter
     next        =letter
   +manual>
     isa         press-key
     key         =letter
)

(p respond-final
   
   =retrieval>
     isa         letter
     name        =letter
 ==>
   +manual>
     isa         press-key
     key         =letter
))
