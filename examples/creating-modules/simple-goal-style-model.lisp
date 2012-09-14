;;; This model has three productions which make a
;;; query, request and modification request to test 
;;; the new-goal module created by the simple-goal-style.lisp 
;;; file.

(clear-all)

(define-model simple-goal-test
    (sgp :v t :trace-detail high)
  
  (chunk-type goal-chunk state)
  
  (define-chunks (start isa chunk) (next isa chunk) (done isa chunk))
  
  (p check-it-is-empty
     ?new-goal>
       buffer empty
     ==>
     +new-goal>
       isa goal-chunk
       state start)
  
  (p note-start
     =new-goal>
       isa goal-chunk
       state start
     ==>
     +new-goal>
       state next)
  
  (p finish
     =new-goal>
     isa goal-chunk
     state next
     ==>
     =new-goal>
     state done))

#|
Here is an example run of this model along with the buffer-status
and buffer-chunk results after the run

> (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.000   PROCEDURAL             PRODUCTION-SELECTED CHECK-IT-IS-EMPTY 
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION NEW-GOAL 
     0.050   PROCEDURAL             PRODUCTION-FIRED CHECK-IT-IS-EMPTY 
     0.050   PROCEDURAL             MODULE-REQUEST NEW-GOAL 
     0.050   PROCEDURAL             CLEAR-BUFFER NEW-GOAL 
     0.050   NEW-GOAL               CREATE-NEW-BUFFER-CHUNK NEW-GOAL ISA GOAL-CHUNK 
     0.050   NEW-GOAL               SET-BUFFER-CHUNK NEW-GOAL GOAL-CHUNK0 
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-SELECTED NOTE-START 
     0.050   PROCEDURAL             BUFFER-READ-ACTION NEW-GOAL 
     0.100   PROCEDURAL             PRODUCTION-FIRED NOTE-START 
     0.100   PROCEDURAL             MODULE-MOD-REQUEST NEW-GOAL 
     0.100   NEW-GOAL               MOD-BUFFER-CHUNK NEW-GOAL 
     0.100   PROCEDURAL             CONFLICT-RESOLUTION 
     0.100   PROCEDURAL             PRODUCTION-SELECTED FINISH 
     0.100   PROCEDURAL             BUFFER-READ-ACTION NEW-GOAL 
     0.150   PROCEDURAL             PRODUCTION-FIRED FINISH 
     0.150   PROCEDURAL             MOD-BUFFER-CHUNK NEW-GOAL 
     0.150   PROCEDURAL             CONFLICT-RESOLUTION 
     0.150   ------                 Stopped because no events left to process 
0.15
25
NIL

> (buffer-status new-goal)
NEW-GOAL:
  buffer empty          : NIL
  buffer full           : T
  buffer requested      : T
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
(NEW-GOAL)

> (buffer-chunk new-goal)
NEW-GOAL: GOAL-CHUNK0-0 
GOAL-CHUNK0-0
  ISA GOAL-CHUNK
   STATE  DONE

(GOAL-CHUNK0-0)
|#