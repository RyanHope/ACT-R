;;; This model runs with the all-components-module and makes
;;; queries and requests of that module to show its components
;;; in action.
;;;
;;; The run-it function will reset the model and then run it
;;; in small sections stopping to print out some query and 
;;; buffer content information between sections.  It calls
;;; clear-all at the end to delete the model.


(clear-all)

(defun run-it ()
  (run .15)
  (buffer-status demo1 demo2)
  (buffer-chunk demo1 demo2)
  (run-until-time .35)
  (buffer-status demo2)
  (buffer-chunk demo2)
  (run-until-time .5)
  (buffer-status demo2)
  (buffer-chunk demo2)
  (run-until-time .65)
  (buffer-status demo2)
  (buffer-chunk demo2)
  (run-until-time .8)
  (buffer-status demo2)
  (buffer-chunk demo2)
  (clear-all))


(define-model test-demo-module
    (model-output "Model definition code starts evaluating here")
  
    (sgp :esc t :demo-param .2 :trace-detail high)
  
  (chunk-type state slot)
  
  ;; just put some chunk in the goal buffer
  (goal-focus free)
  
  (p clear-goal-request-demo1
     ?demo1>
       buffer empty
       state free
     ==>
     -goal>
     +demo1>
       isa state)
  
  (p harvest-demo1-mod-request-demo1
     =demo1>
       isa state
       slot nil
     ==>
     +demo1>
       slot 0)
  
  (p make-demo2-request
     =demo1>
       isa state
       slot 0
     ?demo2>
       state free
       buffer empty
     ==>
     =demo1>
       slot 1
     +demo2>
       isa create-chunk)
 
  (p mod-request-to-demo2
     "This will jam its second time"
     =demo1>
       isa state
       slot 1
     =demo2>
       isa result
       answer nil
     ?demo2>
       detect-jam nil
     ==>
     +demo2>
       answer 10)
  
  (p demo2-request-with-request-parameter
     =demo1>
       isa state
       slot 1
     =demo2>
       isa result
       answer =value
     ==>
     =demo1>
       slot 2
     +demo2>
       isa create-chunk
       :value 15)
  
  (p invalid-demo2-request
     =demo1>
       isa state
       slot 2
     =demo2>
       isa result
     ?demo2>
       state free
     ==>
     +demo2>
       isa visual-location)
  )
  

#|
; Loading C:\Documents and Settings\db30\Desktop\actr6\examples\creating-modules\all-components-model.lisp
Creating a demo module for model TEST-DEMO-MODULE
Demo module's primary reset function called.
Demo module's parameter function called with parameter (:ESC)
  :esc change noted
Demo module's parameter function called with parameter (:DEMO-PARAM . 0.15)
Demo module's secondary reset function called.
Model definition code starts evaluating here
Demo module's parameter function called with parameter (:ESC . T)
  :esc change noted
Demo module's parameter function called with parameter (:DEMO-PARAM . 0.2)
CG-USER(59): (run-it)
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL FREE REQUESTED NIL 
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
Demo module detects that a production will be making a request to the DEMO1 buffer of chunk-type STATE
     0.000   PROCEDURAL             PRODUCTION-SELECTED CLEAR-GOAL-REQUEST-DEMO1 
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION DEMO1 
     0.050   PROCEDURAL             PRODUCTION-FIRED CLEAR-GOAL-REQUEST-DEMO1 
     0.050   PROCEDURAL             MODULE-REQUEST DEMO1 
Request to the DEMO1 buffer made with a request of type STATE
     0.050   PROCEDURAL             CLEAR-BUFFER GOAL 
The demo module detects that the GOAL buffer is clearing chunk FREE-0
     0.050   PROCEDURAL             CLEAR-BUFFER DEMO1 
     0.050   DEMO                   SET-BUFFER-CHUNK DEMO1 STATE0 
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
     0.050   PROCEDURAL             PRODUCTION-SELECTED HARVEST-DEMO1-MOD-REQUEST-DEMO1 
     0.050   PROCEDURAL             BUFFER-READ-ACTION DEMO1 
     0.100   PROCEDURAL             PRODUCTION-FIRED HARVEST-DEMO1-MOD-REQUEST-DEMO1 
     0.100   PROCEDURAL             MODULE-MOD-REQUEST DEMO1 
A buffer modification request was made to the DEMO1 buffer with mods: (SLOT 0)
     0.100   DEMO                   MOD-BUFFER-CHUNK DEMO1 
     0.100   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
Demo module's query function called to query the DEMO2 buffer for STATE FREE
Demo module detects that a production will be making a request to the DEMO2 buffer of chunk-type CREATE-CHUNK
     0.100   PROCEDURAL             PRODUCTION-SELECTED MAKE-DEMO2-REQUEST 
     0.100   PROCEDURAL             BUFFER-READ-ACTION DEMO1 
     0.100   PROCEDURAL             QUERY-BUFFER-ACTION DEMO2 
     0.150   PROCEDURAL             PRODUCTION-FIRED MAKE-DEMO2-REQUEST 
     0.150   PROCEDURAL             MOD-BUFFER-CHUNK DEMO1 
     0.150   PROCEDURAL             MODULE-REQUEST DEMO2 
Request to the DEMO2 buffer made with a request of type CREATE-CHUNK
     0.150   PROCEDURAL             CLEAR-BUFFER DEMO2 
     0.150   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
     0.150   ------                 Stopped because time limit reached 
DEMO1:
  buffer empty          : NIL
  buffer full           : T
  buffer requested      : T
  buffer unrequested    : NIL
Demo module's query function called to query the DEMO1 buffer for STATE FREE
  state free            : T
Demo module's query function called to query the DEMO1 buffer for STATE BUSY
  state busy            : NIL
Demo module's query function called to query the DEMO1 buffer for STATE ERROR
  state error           : NIL
DEMO2:
  buffer empty          : T
  buffer full           : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
Demo module's query function called to query the DEMO2 buffer for STATE FREE
  state free            : NIL
Demo module's query function called to query the DEMO2 buffer for STATE BUSY
  state busy            : T
Demo module's query function called to query the DEMO2 buffer for STATE ERROR
  state error           : NIL
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM T
  detect-jam            : NIL
DEMO1: STATE0-0 
STATE0-0
  ISA STATE
   SLOT  1

DEMO2: NIL 
     0.250   DEMO                   SET-BUFFER-CHUNK DEMO2 RESULT0 
     0.250   DEMO                   FINISH-DEMO2-REQUEST 
     0.250   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM NIL
     0.250   PROCEDURAL             PRODUCTION-SELECTED MOD-REQUEST-TO-DEMO2 
     0.250   PROCEDURAL             BUFFER-READ-ACTION DEMO1 
     0.250   PROCEDURAL             BUFFER-READ-ACTION DEMO2 
     0.250   PROCEDURAL             QUERY-BUFFER-ACTION DEMO2 
     0.300   PROCEDURAL             PRODUCTION-FIRED MOD-REQUEST-TO-DEMO2 
     0.300   PROCEDURAL             MODULE-MOD-REQUEST DEMO2 
A buffer modification request was made to the DEMO2 buffer with mods: (ANSWER 10)
     0.300   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM NIL
     0.300   PROCEDURAL             PRODUCTION-SELECTED MOD-REQUEST-TO-DEMO2 
     0.300   PROCEDURAL             BUFFER-READ-ACTION DEMO1 
     0.300   PROCEDURAL             BUFFER-READ-ACTION DEMO2 
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION DEMO2 
     0.350   PROCEDURAL             PRODUCTION-FIRED MOD-REQUEST-TO-DEMO2 
     0.350   PROCEDURAL             MODULE-MOD-REQUEST DEMO2 
A buffer modification request was made to the DEMO2 buffer with mods: (ANSWER 10)
#|Warning: Demo module's demo2 buffer can only process one request at a time. |#
     0.350   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM NIL
     0.350   ------                 Stopped because time limit reached 
DEMO2:
  buffer empty          : NIL
  buffer full           : T
  buffer requested      : T
  buffer unrequested    : NIL
Demo module's query function called to query the DEMO2 buffer for STATE FREE
  state free            : NIL
Demo module's query function called to query the DEMO2 buffer for STATE BUSY
  state busy            : T
Demo module's query function called to query the DEMO2 buffer for STATE ERROR
  state error           : NIL
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM T
  detect-jam            : T
DEMO2: RESULT0-0 [RESULT0]
RESULT0-0
  ISA RESULT
   ANSWER  NIL

     0.500   DEMO                   MOD-BUFFER-CHUNK DEMO2 
     0.500   DEMO                   FINISH-DEMO2-REQUEST 
     0.500   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
Demo module detects that a production will be making a request to the DEMO2 buffer of chunk-type CREATE-CHUNK
     0.500   PROCEDURAL             PRODUCTION-SELECTED DEMO2-REQUEST-WITH-REQUEST-PARAMETER 
     0.500   PROCEDURAL             BUFFER-READ-ACTION DEMO1 
     0.500   PROCEDURAL             BUFFER-READ-ACTION DEMO2 
     0.500   ------                 Stopped because time limit reached 
DEMO2:
  buffer empty          : NIL
  buffer full           : T
  buffer requested      : T
  buffer unrequested    : NIL
Demo module's query function called to query the DEMO2 buffer for STATE FREE
  state free            : T
Demo module's query function called to query the DEMO2 buffer for STATE BUSY
  state busy            : NIL
Demo module's query function called to query the DEMO2 buffer for STATE ERROR
  state error           : NIL
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM T
  detect-jam            : NIL
DEMO2: RESULT0-0 
RESULT0-0
  ISA RESULT
   ANSWER  10

     0.550   PROCEDURAL             PRODUCTION-FIRED DEMO2-REQUEST-WITH-REQUEST-PARAMETER 
     0.550   PROCEDURAL             MOD-BUFFER-CHUNK DEMO1 
     0.550   PROCEDURAL             MODULE-REQUEST DEMO2 
Request to the DEMO2 buffer made with a request of type CREATE-CHUNK
     0.550   PROCEDURAL             CLEAR-BUFFER DEMO2 
The demo module detects that the DEMO2 buffer is clearing chunk RESULT0-0
     0.550   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
     0.650   DEMO                   SET-BUFFER-CHUNK DEMO2 RESULT1 
     0.650   DEMO                   FINISH-DEMO2-REQUEST 
     0.650   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
Demo module's query function called to query the DEMO2 buffer for STATE FREE
Demo module detects that a production will be making a request to the DEMO2 buffer of chunk-type VISUAL-LOCATION
     0.650   PROCEDURAL             PRODUCTION-SELECTED INVALID-DEMO2-REQUEST 
     0.650   PROCEDURAL             BUFFER-READ-ACTION DEMO1 
     0.650   PROCEDURAL             BUFFER-READ-ACTION DEMO2 
     0.650   PROCEDURAL             QUERY-BUFFER-ACTION DEMO2 
     0.650   ------                 Stopped because time limit reached 
DEMO2:
  buffer empty          : NIL
  buffer full           : T
  buffer requested      : T
  buffer unrequested    : NIL
Demo module's query function called to query the DEMO2 buffer for STATE FREE
  state free            : T
Demo module's query function called to query the DEMO2 buffer for STATE BUSY
  state busy            : NIL
Demo module's query function called to query the DEMO2 buffer for STATE ERROR
  state error           : NIL
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM T
  detect-jam            : NIL
DEMO2: RESULT1-0 [RESULT1]
RESULT1-0
  ISA RESULT
   ANSWER  15

     0.700   PROCEDURAL             PRODUCTION-FIRED INVALID-DEMO2-REQUEST 
     0.700   PROCEDURAL             MODULE-REQUEST DEMO2 
Request to the DEMO2 buffer made with a request of type VISUAL-LOCATION
#|Warning: Invalid request to the demo2 buffer with this chunk-spec: |#
    ISA VISUAL-LOCATION
     0.700   PROCEDURAL             CLEAR-BUFFER DEMO2 
The demo module detects that the DEMO2 buffer is clearing chunk RESULT1-0
     0.700   PROCEDURAL             CONFLICT-RESOLUTION 
Demo module's query function called to query the DEMO1 buffer for STATE FREE
     0.800   ------                 Stopped because time limit reached 
DEMO2:
  buffer empty          : T
  buffer full           : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
Demo module's query function called to query the DEMO2 buffer for STATE FREE
  state free            : T
Demo module's query function called to query the DEMO2 buffer for STATE BUSY
  state busy            : NIL
Demo module's query function called to query the DEMO2 buffer for STATE ERROR
  state error           : T
Demo module's query function called to query the DEMO2 buffer for DETECT-JAM T
  detect-jam            : NIL
DEMO2: NIL 
Deletion of demo module called for instance in model TEST-DEMO-MODULE.
NIL
CG-USER(60): 
|#
