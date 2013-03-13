(defmacro defp (&rest body)
  `(p-fct ',body))

(defmacro defp* (&rest body)
  `(p*-fct ',body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clear-all)

(define-model drive-model  ;;name of model

(set-visloc-default-fct '(isa visual-location kind dummy))

(sgp :jni-hostname "localhost" :jni-port 5555 :jni-sync t
     :v  t
     :needs-mouse nil :DECLARATIVE-FINST-SPAN 12 :ol nil
     :visual-movement-tolerance 5.0 :bll .5  :blc 2.0 :act nil :crt nil :ans .26  :mp 10
     :esc t :trace-detail low :er t  :show-focus nil  :lf 0.5
     :do-not-harvest imaginal :do-not-harvest contextual
     :MOTOR-FEATURE-PREP-TIME 0
     :time-master-start-increment 1.0
     :TIME-MULT 1.0001 
     
)

(set-audloc-default :location external :attended nil)

(chunk-type arrow-task state)
(chunk-type (rec-location (:include visual-location)) name)
(chunk-type (arrow-location (:include visual-location)) dir)
(chunk-type (rec (:include visual-object)))
(chunk-type (arrow (:include visual-object)) dir)


(add-dm (goal0 isa arrow-task state find-arrow))

(goal-focus goal0)

(defp *find-arrow
 =goal> isa arrow-task  state find-arrow
 
 ?manual> state free
==>
 =goal> state attend-arrow
 +visual-location> isa visual-location kind arrow)

(defp *find-arrow-error
 =goal> isa arrow-task  state attend-arrow
 ?visual-location> state error
 ?manual> state free
==>
 =goal> state find-arrow
 )

(defp *attend-arrow
 =goal> isa arrow-task state attend-arrow 
 =visual-location> isa visual-location kind arrow screen-x =x  
 ?visual> state free
 ?visual-location> buffer requested
==>
; !output! (attend arrow = =x )
 =goal> state proc-arrow
 +visual> isa move-attention screen-pos =visual-location)

(defp *attend-arrow-error
 =goal> isa arrow-task state attend-arrow    
 ?visual> state error 
==>
 +visual> isa clear
 -visual-location>
 =goal> state find-arrow
)

(defp *jitter-left
 =goal> isa arrow-task state proc-arrow 
 =visual> isa arrow < value 465 value =x
 ?manual> state free
==>
; !output! (proc-arrow = =x left)
 +manual> isa press-key key "d"
 =goal> state find-arrow
)

(defp *jitter-right
 =goal> isa arrow-task state proc-arrow 
 =visual> isa arrow > value 465 value =x
 ?manual> state free
==>
 ;!output! (proc-arrow = =x right)
 +manual> isa press-key key "a"
=goal> state find-arrow
)

(defp *jitter-middle
 =goal> isa arrow-task state proc-arrow
 =visual> isa arrow = value 465 value =x
 ?manual> state free
==>
;!output! (proc-arrow = =x middle)
=goal> state find-arrow
)
)
