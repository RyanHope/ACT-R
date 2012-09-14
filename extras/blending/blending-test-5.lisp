;; Simple model to show blending in action.
;; Load the model and call run to see the 
;; trace of two blended retrievals. 
;;
;; This example is similar to the others, except that 
;; it sets the :blend-all-slots parameter to t and
;; makes a couple of other changes to show that in
;; action: 
;;  - the keys are created using a specific type and added 
;;    to DM (the specific type is used because otherwise
;;    all chunks of type chunk would be considered valid
;;    values)
;;  - the value slot is used in the second request instead 
;;    of the key.


(clear-all)


(define-model test-blending
    (sgp :seed (2 1) :v t :blt t :esc t :ans .25 :rt 4 :mp 1 :tmp 1.5 :blend-all-slots t)
  
  (chunk-type target key value size)
  (chunk-type size)
  (chunk-type key)
  
  ;; some chunks which don't need to be in DM
  
  (define-chunks 
      (dummy isa chunk))

  
  ;; Here are the chunks for the blending test
  
  (add-dm 
   
   ;; the keys
   
   (key-1 isa key)
   (key-2 isa key)
   
   ;; A set of size chunks
   
   (tiny isa size)
   (small isa size)
   (medium isa size)
   (large isa size)
   (x-large isa size)
   
   ;; A set of target chunks which will be
   ;; the items for the blended retrieval.
   
   (a isa target key key-1 value 1 size large)
   (b isa target key key-1 value 2 size x-large)
   (c isa target key key-1 value 3 size tiny)
   (d isa target key key-2 value 1 size nil)
   (e isa target key key-2 value 3 size small))
  
  
  ;; Set some arbitrary base-level activations for the target
  ;; chunks to provide some systematic variability in the values.
  
  (set-base-levels (a 4 -10) (b 4 -10) (c 5 -10) (d 2 -10) (e 1 -10))
  
  ;; Provide the similarities between the sizes
  ;; because blending will use that even though 
  ;; partial matching is not enabled.
  
  (set-similarities (tiny small -.1)
                    (small medium -.1)
                    (medium large -.1)
                    (large x-large -.1)
                    (tiny medium -.3)
                    (small large -.3)
                    (medium x-large -.3)
                    (tiny large -.6)
                    (small x-large -.6)
                    (tiny x-large -.9))
  
  ;; Very simple model
  ;; Make a blending request for a target chunk
  ;; with key value key-1 and if such a chunk
  ;; is found make a blending request for a
  ;; target chunk with a key value of key-2
  
  (p p1
     ?blending>
       state free
       buffer empty
       error nil
     ==>
     +blending>
       isa target
       key key-2)
  
  (p p2
     =blending>
       isa target
       value =val
       size  =size
     ?goal>
       buffer empty
     
     ==>
     !output! (blended value is =val and size is =size)
     
     ; Overwrite the blended chunk to prevent it from 
     ; being added to dm.  Not necessary, but keeps the 
     ; examples simpler.
     
     =blending> dummy
     
     +blending>
       isa target
       value 2
     
     ;; just set some goal to prevent repeat firing
     +goal> isa chunk     
     )
  )

#| Here's a trace of the run

CG-USER(40): (reload)
; Loading C:\Documents and Settings\db30\Desktop\actr6\extras\blending\blending-test-5.lisp
T
CG-USER(41): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.050   BLENDING               START-BLENDING 
Blending request for chunks of type TARGET
Blending temperature is: 1.5
Chunk E matches blending request
  Activation 1.370572
  Probability of recall 0.081871115

Chunk D matches blending request
  Activation 2.906558
  Probability of recall 0.22795242

Chunk C matches blending request
  Activation 3.3473492
  Probability of recall 0.30582032

Chunk B matches blending request
  Activation 2.9501731
  Probability of recall 0.23467784

Chunk A matches blending request
  Activation 2.2755852
  Probability of recall 0.14967832


Slots to be blended: (KEY VALUE SIZE)
Finding blended value for slot: KEY
Matched chunks' slots contain: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
Magnitude values for those items: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
When all magnitudes are chunks or nil blending based on common chunk-types and similiarities
Common chunk-type for values is: KEY
 Comparing value KEY-1
  Chunk E with probability 0.081871115 slot value KEY-2 similarity: -1.0 cumulative result: 0.081871115
  Chunk D with probability 0.22795242 slot value KEY-2 similarity: -1.0 cumulative result: 0.30982354
  Chunk C with probability 0.30582032 slot value KEY-1 similarity: 0.0 cumulative result: 0.30982354
  Chunk B with probability 0.23467784 slot value KEY-1 similarity: 0.0 cumulative result: 0.30982354
  Chunk A with probability 0.14967832 slot value KEY-1 similarity: 0.0 cumulative result: 0.30982354
 Comparing value KEY-2
  Chunk E with probability 0.081871115 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.22795242 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.30582032 slot value KEY-1 similarity: -1.0 cumulative result: 0.30582032
  Chunk B with probability 0.23467784 slot value KEY-1 similarity: -1.0 cumulative result: 0.54049814
  Chunk A with probability 0.14967832 slot value KEY-1 similarity: -1.0 cumulative result: 0.6901765
 Final result: KEY-1
Finding blended value for slot: VALUE
Matched chunks' slots contain: (3 1 3 2 1)
Magnitude values for those items: (3 1 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.081871115 times magnitude 3.0 cumulative result: 0.24561334
 Chunk D with probability 0.22795242 times magnitude 1.0 cumulative result: 0.47356576
 Chunk C with probability 0.30582032 times magnitude 3.0 cumulative result: 1.3910267
 Chunk B with probability 0.23467784 times magnitude 2.0 cumulative result: 1.8603824
 Chunk A with probability 0.14967832 times magnitude 1.0 cumulative result: 2.0100608
 Final result: 2.0100608
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL NIL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL NIL TINY X-LARGE LARGE)
When all magnitudes are chunks or nil blending based on common chunk-types and similiarities
Common chunk-type for values is: SIZE
 Comparing value TINY
  Chunk E with probability 0.081871115 slot value SMALL similarity: -0.1 cumulative result: 8.187112e-4
  Chunk D with probability 0.22795242 slot value NIL similarity: -1.0 cumulative result: 0.22877114
  Chunk C with probability 0.30582032 slot value TINY similarity: 0.0 cumulative result: 0.22877114
  Chunk B with probability 0.23467784 slot value X-LARGE similarity: -0.9 cumulative result: 0.41886017
  Chunk A with probability 0.14967832 slot value LARGE similarity: -0.6 cumulative result: 0.47274438
 Comparing value SMALL
  Chunk E with probability 0.081871115 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.22795242 slot value NIL similarity: -1.0 cumulative result: 0.22795242
  Chunk C with probability 0.30582032 slot value TINY similarity: -0.1 cumulative result: 0.23101063
  Chunk B with probability 0.23467784 slot value X-LARGE similarity: -0.6 cumulative result: 0.31549466
  Chunk A with probability 0.14967832 slot value LARGE similarity: -0.3 cumulative result: 0.3289657
 Comparing value MEDIUM
  Chunk E with probability 0.081871115 slot value SMALL similarity: -0.1 cumulative result: 8.187112e-4
  Chunk D with probability 0.22795242 slot value NIL similarity: -1.0 cumulative result: 0.22877114
  Chunk C with probability 0.30582032 slot value TINY similarity: -0.3 cumulative result: 0.25629497
  Chunk B with probability 0.23467784 slot value X-LARGE similarity: -0.3 cumulative result: 0.27741596
  Chunk A with probability 0.14967832 slot value LARGE similarity: -0.1 cumulative result: 0.27891275
 Comparing value LARGE
  Chunk E with probability 0.081871115 slot value SMALL similarity: -0.3 cumulative result: 0.0073684007
  Chunk D with probability 0.22795242 slot value NIL similarity: -1.0 cumulative result: 0.23532082
  Chunk C with probability 0.30582032 slot value TINY similarity: -0.6 cumulative result: 0.34541613
  Chunk B with probability 0.23467784 slot value X-LARGE similarity: -0.1 cumulative result: 0.3477629
  Chunk A with probability 0.14967832 slot value LARGE similarity: 0.0 cumulative result: 0.3477629
 Comparing value X-LARGE
  Chunk E with probability 0.081871115 slot value SMALL similarity: -0.6 cumulative result: 0.029473603
  Chunk D with probability 0.22795242 slot value NIL similarity: -1.0 cumulative result: 0.25742602
  Chunk C with probability 0.30582032 slot value TINY similarity: -0.9 cumulative result: 0.5051405
  Chunk B with probability 0.23467784 slot value X-LARGE similarity: 0.0 cumulative result: 0.5051405
  Chunk A with probability 0.14967832 slot value LARGE similarity: -0.1 cumulative result: 0.5066373
 Final result: MEDIUM
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-1 VALUE 2.0100608 SIZE MEDIUM)

Computing activation and latency for the blended chunk
 Activation of chunk E is 1.370572
 Activation of chunk D is 2.906558
 Activation of chunk C is 3.3473492
 Activation of chunk B is 2.9501731
 Activation of chunk A is 2.2755852
Activation for blended chunk is: 4.375776
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.063   BLENDING               BLENDING-COMPLETE 
     0.063   BLENDING               SET-BUFFER-CHUNK BLENDING TARGET0 
     0.063   PROCEDURAL             CONFLICT-RESOLUTION 
     0.113   PROCEDURAL             PRODUCTION-FIRED P2 
BLENDED VALUE IS 2.0100608 AND SIZE IS MEDIUM 
     0.113   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.113   PROCEDURAL             CLEAR-BUFFER GOAL 
     0.113   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK0 
     0.113   BLENDING               START-BLENDING 
Blending request for chunks of type TARGET
Blending temperature is: 1.5
Chunk E matches blending request
  Activation 0.065042
  Probability of recall 0.024388567

Chunk D matches blending request
  Activation 1.0152556
  Probability of recall 0.04595166

Chunk C matches blending request
  Activation 4.080316
  Probability of recall 0.35459056

Chunk B matches blending request
  Activation 4.187827
  Probability of recall 0.3809384

Chunk A matches blending request
  Activation 3.1766677
  Probability of recall 0.19413069


Slots to be blended: (KEY VALUE SIZE)
Finding blended value for slot: KEY
Matched chunks' slots contain: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
Magnitude values for those items: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
When all magnitudes are chunks or nil blending based on common chunk-types and similiarities
Common chunk-type for values is: KEY
 Comparing value KEY-1
  Chunk E with probability 0.024388567 slot value KEY-2 similarity: -1.0 cumulative result: 0.024388567
  Chunk D with probability 0.04595166 slot value KEY-2 similarity: -1.0 cumulative result: 0.07034023
  Chunk C with probability 0.35459056 slot value KEY-1 similarity: 0.0 cumulative result: 0.07034023
  Chunk B with probability 0.3809384 slot value KEY-1 similarity: 0.0 cumulative result: 0.07034023
  Chunk A with probability 0.19413069 slot value KEY-1 similarity: 0.0 cumulative result: 0.07034023
 Comparing value KEY-2
  Chunk E with probability 0.024388567 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.04595166 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.35459056 slot value KEY-1 similarity: -1.0 cumulative result: 0.35459056
  Chunk B with probability 0.3809384 slot value KEY-1 similarity: -1.0 cumulative result: 0.73552895
  Chunk A with probability 0.19413069 slot value KEY-1 similarity: -1.0 cumulative result: 0.9296596
 Final result: KEY-1
Finding blended value for slot: VALUE
Matched chunks' slots contain: (3 1 3 2 1)
Magnitude values for those items: (3 1 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.024388567 times magnitude 3.0 cumulative result: 0.0731657
 Chunk D with probability 0.04595166 times magnitude 1.0 cumulative result: 0.119117364
 Chunk C with probability 0.35459056 times magnitude 3.0 cumulative result: 1.1828891
 Chunk B with probability 0.3809384 times magnitude 2.0 cumulative result: 1.9447659
 Chunk A with probability 0.19413069 times magnitude 1.0 cumulative result: 2.1388967
 Final result: 2.1388967
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL NIL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL NIL TINY X-LARGE LARGE)
When all magnitudes are chunks or nil blending based on common chunk-types and similiarities
Common chunk-type for values is: SIZE
 Comparing value TINY
  Chunk E with probability 0.024388567 slot value SMALL similarity: -0.1 cumulative result: 2.4388569e-4
  Chunk D with probability 0.04595166 slot value NIL similarity: -1.0 cumulative result: 0.046195548
  Chunk C with probability 0.35459056 slot value TINY similarity: 0.0 cumulative result: 0.046195548
  Chunk B with probability 0.3809384 slot value X-LARGE similarity: -0.9 cumulative result: 0.35475564
  Chunk A with probability 0.19413069 slot value LARGE similarity: -0.6 cumulative result: 0.42464268
 Comparing value SMALL
  Chunk E with probability 0.024388567 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.04595166 slot value NIL similarity: -1.0 cumulative result: 0.04595166
  Chunk C with probability 0.35459056 slot value TINY similarity: -0.1 cumulative result: 0.049497567
  Chunk B with probability 0.3809384 slot value X-LARGE similarity: -0.6 cumulative result: 0.1866354
  Chunk A with probability 0.19413069 slot value LARGE similarity: -0.3 cumulative result: 0.20410717
 Comparing value MEDIUM
  Chunk E with probability 0.024388567 slot value SMALL similarity: -0.1 cumulative result: 2.4388569e-4
  Chunk D with probability 0.04595166 slot value NIL similarity: -1.0 cumulative result: 0.046195548
  Chunk C with probability 0.35459056 slot value TINY similarity: -0.3 cumulative result: 0.0781087
  Chunk B with probability 0.3809384 slot value X-LARGE similarity: -0.3 cumulative result: 0.112393156
  Chunk A with probability 0.19413069 slot value LARGE similarity: -0.1 cumulative result: 0.114334464
 Comparing value LARGE
  Chunk E with probability 0.024388567 slot value SMALL similarity: -0.3 cumulative result: 0.002194971
  Chunk D with probability 0.04595166 slot value NIL similarity: -1.0 cumulative result: 0.04814663
  Chunk C with probability 0.35459056 slot value TINY similarity: -0.6 cumulative result: 0.17579925
  Chunk B with probability 0.3809384 slot value X-LARGE similarity: -0.1 cumulative result: 0.17960863
  Chunk A with probability 0.19413069 slot value LARGE similarity: 0.0 cumulative result: 0.17960863
 Comparing value X-LARGE
  Chunk E with probability 0.024388567 slot value SMALL similarity: -0.6 cumulative result: 0.008779884
  Chunk D with probability 0.04595166 slot value NIL similarity: -1.0 cumulative result: 0.054731544
  Chunk C with probability 0.35459056 slot value TINY similarity: -0.9 cumulative result: 0.34194988
  Chunk B with probability 0.3809384 slot value X-LARGE similarity: 0.0 cumulative result: 0.34194988
  Chunk A with probability 0.19413069 slot value LARGE similarity: -0.1 cumulative result: 0.34389117
 Final result: MEDIUM
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-1 VALUE 2.1388967 SIZE MEDIUM)

Computing activation and latency for the blended chunk
 Activation of chunk E is 0.065042
 Activation of chunk D is 1.0152556
 Activation of chunk C is 4.080316
 Activation of chunk B is 4.187827
 Activation of chunk A is 3.1766677
Activation for blended chunk is: 5.0293765
     0.113   PROCEDURAL             CONFLICT-RESOLUTION 
     0.120   BLENDING               BLENDING-COMPLETE 
     0.120   BLENDING               SET-BUFFER-CHUNK BLENDING TARGET1 
     0.120   PROCEDURAL             CONFLICT-RESOLUTION 
     0.120   ------                 Stopped because no events left to process 
0.12
33
NIL
|#
