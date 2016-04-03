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
  (chunk-type size (size-type t))
  (chunk-type key (key-type t))
  
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
     ?blending>
       state free
     ==>
     !output! (blended value is =val and size is =size)
     
     ; Overwrite the blended chunk to prevent it from 
     ; being added to dm.  Not necessary, but keeps the 
     ; examples simpler.
     
     @blending> dummy
     
     +blending>
       isa target
       value 2
     
     ;; just set some goal to prevent repeat firing
     +goal> isa chunk     
     )
  )

#| Here's a trace of the run
CG-USER(39): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.050   BLENDING               START-BLENDING 
Blending request for chunks with slots (KEY) 
Blending temperature is: 1.5
Chunk D matches blending request
  Activation 2.370572
  Probability of recall 0.1649606

Chunk E matches blending request
  Activation 1.906558
  Probability of recall 0.121069394

Chunk C matches blending request
  Activation 3.3473492
  Probability of recall 0.31636336

Chunk B matches blending request
  Activation 2.9501731
  Probability of recall 0.24276826

Chunk A matches blending request
  Activation 2.2755852
  Probability of recall 0.15483841


Slots to be blended: (VALUE SIZE KEY)
Finding blended value for slot: VALUE
Matched chunks' slots contain: (1 3 3 2 1)
Magnitude values for those items: (1 3 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk D with probability 0.1649606 times magnitude 1.0 = 0.1649606 cumulative result: 0.1649606
 Chunk E with probability 0.121069394 times magnitude 3.0 = 0.36320817 cumulative result: 0.5281688
 Chunk C with probability 0.31636336 times magnitude 3.0 = 0.9490901 cumulative result: 1.4772589
 Chunk B with probability 0.24276826 times magnitude 2.0 = 0.48553652 cumulative result: 1.9627955
 Chunk A with probability 0.15483841 times magnitude 1.0 = 0.15483841 cumulative result: 2.1176338
 Final result: 2.1176338
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL TINY X-LARGE LARGE)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (SIZE-TYPE)
 Comparing value X-LARGE
  Chunk E with probability 0.121069394 slot value SMALL similarity: -0.6 cumulative result: 0.043584984
  Chunk C with probability 0.31636336 slot value TINY similarity: -0.9 cumulative result: 0.2998393
  Chunk B with probability 0.24276826 slot value X-LARGE similarity: 0.0 cumulative result: 0.2998393
  Chunk A with probability 0.15483841 slot value LARGE similarity: -0.1 cumulative result: 0.30138767
 Comparing value LARGE
  Chunk E with probability 0.121069394 slot value SMALL similarity: -0.3 cumulative result: 0.010896246
  Chunk C with probability 0.31636336 slot value TINY similarity: -0.6 cumulative result: 0.12478706
  Chunk B with probability 0.24276826 slot value X-LARGE similarity: -0.1 cumulative result: 0.12721474
  Chunk A with probability 0.15483841 slot value LARGE similarity: 0.0 cumulative result: 0.12721474
 Comparing value MEDIUM
  Chunk E with probability 0.121069394 slot value SMALL similarity: -0.1 cumulative result: 0.001210694
  Chunk C with probability 0.31636336 slot value TINY similarity: -0.3 cumulative result: 0.029683398
  Chunk B with probability 0.24276826 slot value X-LARGE similarity: -0.3 cumulative result: 0.051532544
  Chunk A with probability 0.15483841 slot value LARGE similarity: -0.1 cumulative result: 0.053080928
 Comparing value SMALL
  Chunk E with probability 0.121069394 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.31636336 slot value TINY similarity: -0.1 cumulative result: 0.0031636339
  Chunk B with probability 0.24276826 slot value X-LARGE similarity: -0.6 cumulative result: 0.09056021
  Chunk A with probability 0.15483841 slot value LARGE similarity: -0.3 cumulative result: 0.104495674
 Comparing value TINY
  Chunk E with probability 0.121069394 slot value SMALL similarity: -0.1 cumulative result: 0.001210694
  Chunk C with probability 0.31636336 slot value TINY similarity: 0.0 cumulative result: 0.001210694
  Chunk B with probability 0.24276826 slot value X-LARGE similarity: -0.9 cumulative result: 0.19785297
  Chunk A with probability 0.15483841 slot value LARGE similarity: -0.6 cumulative result: 0.25359482
 Final result: MEDIUM
Finding blended value for slot: KEY
Matched chunks' slots contain: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
Magnitude values for those items: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (KEY-TYPE)
 Comparing value KEY-2
  Chunk D with probability 0.1649606 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk E with probability 0.121069394 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.31636336 slot value KEY-1 similarity: -1.0 cumulative result: 0.31636336
  Chunk B with probability 0.24276826 slot value KEY-1 similarity: -1.0 cumulative result: 0.5591316
  Chunk A with probability 0.15483841 slot value KEY-1 similarity: -1.0 cumulative result: 0.71397007
 Comparing value KEY-1
  Chunk D with probability 0.1649606 slot value KEY-2 similarity: -1.0 cumulative result: 0.1649606
  Chunk E with probability 0.121069394 slot value KEY-2 similarity: -1.0 cumulative result: 0.28603
  Chunk C with probability 0.31636336 slot value KEY-1 similarity: 0.0 cumulative result: 0.28603
  Chunk B with probability 0.24276826 slot value KEY-1 similarity: 0.0 cumulative result: 0.28603
  Chunk A with probability 0.15483841 slot value KEY-1 similarity: 0.0 cumulative result: 0.28603
 Final result: KEY-1
This is the definition of the blended chunk:
(VALUE 2.1176338 SIZE MEDIUM KEY KEY-1)

Computing activation and latency for the blended chunk
 Activation of chunk D is 2.370572
 Activation of chunk E is 1.906558
 Activation of chunk C is 3.3473492
 Activation of chunk B is 2.9501731
 Activation of chunk A is 2.2755852
Activation for blended chunk is: 4.3135276
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.063   BLENDING               BLENDING-COMPLETE 
     0.063   BLENDING               SET-BUFFER-CHUNK BLENDING CHUNK0 
     0.063   PROCEDURAL             CONFLICT-RESOLUTION 
     0.113   PROCEDURAL             PRODUCTION-FIRED P2 
BLENDED VALUE IS 2.1176338 AND SIZE IS MEDIUM 
     0.113   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.113   PROCEDURAL             CLEAR-BUFFER GOAL 
     0.113   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK1 
     0.113   BLENDING               START-BLENDING 
Blending request for chunks with slots (VALUE) 
Blending temperature is: 1.5
Chunk D matches blending request
  Activation 1.065042
  Probability of recall 0.047466617

Chunk E matches blending request
  Activation 0.015255626
  Probability of recall 0.023574583

Chunk C matches blending request
  Activation 4.080316
  Probability of recall 0.3543232

Chunk B matches blending request
  Activation 4.187827
  Probability of recall 0.38065118

Chunk A matches blending request
  Activation 3.1766677
  Probability of recall 0.19398431


Slots to be blended: (VALUE SIZE KEY)
Finding blended value for slot: VALUE
Matched chunks' slots contain: (1 3 3 2 1)
Magnitude values for those items: (1 3 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk D with probability 0.047466617 times magnitude 1.0 = 0.047466617 cumulative result: 0.047466617
 Chunk E with probability 0.023574583 times magnitude 3.0 = 0.07072375 cumulative result: 0.11819036
 Chunk C with probability 0.3543232 times magnitude 3.0 = 1.0629697 cumulative result: 1.1811601
 Chunk B with probability 0.38065118 times magnitude 2.0 = 0.76130235 cumulative result: 1.9424624
 Chunk A with probability 0.19398431 times magnitude 1.0 = 0.19398431 cumulative result: 2.1364467
 Final result: 2.1364467
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL TINY X-LARGE LARGE)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (SIZE-TYPE)
 Comparing value X-LARGE
  Chunk E with probability 0.023574583 slot value SMALL similarity: -0.6 cumulative result: 0.00848685
  Chunk C with probability 0.3543232 slot value TINY similarity: -0.9 cumulative result: 0.29548863
  Chunk B with probability 0.38065118 slot value X-LARGE similarity: 0.0 cumulative result: 0.29548863
  Chunk A with probability 0.19398431 slot value LARGE similarity: -0.1 cumulative result: 0.29742846
 Comparing value LARGE
  Chunk E with probability 0.023574583 slot value SMALL similarity: -0.3 cumulative result: 0.0021217125
  Chunk C with probability 0.3543232 slot value TINY similarity: -0.6 cumulative result: 0.12967807
  Chunk B with probability 0.38065118 slot value X-LARGE similarity: -0.1 cumulative result: 0.13348459
  Chunk A with probability 0.19398431 slot value LARGE similarity: 0.0 cumulative result: 0.13348459
 Comparing value MEDIUM
  Chunk E with probability 0.023574583 slot value SMALL similarity: -0.1 cumulative result: 2.3574584e-4
  Chunk C with probability 0.3543232 slot value TINY similarity: -0.3 cumulative result: 0.032124836
  Chunk B with probability 0.38065118 slot value X-LARGE similarity: -0.3 cumulative result: 0.066383444
  Chunk A with probability 0.19398431 slot value LARGE similarity: -0.1 cumulative result: 0.068323284
 Comparing value SMALL
  Chunk E with probability 0.023574583 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.3543232 slot value TINY similarity: -0.1 cumulative result: 0.0035432323
  Chunk B with probability 0.38065118 slot value X-LARGE similarity: -0.6 cumulative result: 0.14057766
  Chunk A with probability 0.19398431 slot value LARGE similarity: -0.3 cumulative result: 0.15803625
 Comparing value TINY
  Chunk E with probability 0.023574583 slot value SMALL similarity: -0.1 cumulative result: 2.3574584e-4
  Chunk C with probability 0.3543232 slot value TINY similarity: 0.0 cumulative result: 2.3574584e-4
  Chunk B with probability 0.38065118 slot value X-LARGE similarity: -0.9 cumulative result: 0.30856317
  Chunk A with probability 0.19398431 slot value LARGE similarity: -0.6 cumulative result: 0.37839752
 Final result: MEDIUM
Finding blended value for slot: KEY
Matched chunks' slots contain: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
Magnitude values for those items: (KEY-2 KEY-2 KEY-1 KEY-1 KEY-1)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (KEY-TYPE)
 Comparing value KEY-2
  Chunk D with probability 0.047466617 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk E with probability 0.023574583 slot value KEY-2 similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.3543232 slot value KEY-1 similarity: -1.0 cumulative result: 0.3543232
  Chunk B with probability 0.38065118 slot value KEY-1 similarity: -1.0 cumulative result: 0.7349744
  Chunk A with probability 0.19398431 slot value KEY-1 similarity: -1.0 cumulative result: 0.9289587
 Comparing value KEY-1
  Chunk D with probability 0.047466617 slot value KEY-2 similarity: -1.0 cumulative result: 0.047466617
  Chunk E with probability 0.023574583 slot value KEY-2 similarity: -1.0 cumulative result: 0.0710412
  Chunk C with probability 0.3543232 slot value KEY-1 similarity: 0.0 cumulative result: 0.0710412
  Chunk B with probability 0.38065118 slot value KEY-1 similarity: 0.0 cumulative result: 0.0710412
  Chunk A with probability 0.19398431 slot value KEY-1 similarity: 0.0 cumulative result: 0.0710412
 Final result: KEY-1
This is the definition of the blended chunk:
(VALUE 2.1364467 SIZE MEDIUM KEY KEY-1)

Computing activation and latency for the blended chunk
 Activation of chunk D is 1.065042
 Activation of chunk E is 0.015255626
 Activation of chunk C is 4.080316
 Activation of chunk B is 4.187827
 Activation of chunk A is 3.1766677
Activation for blended chunk is: 5.029959
     0.113   PROCEDURAL             CONFLICT-RESOLUTION 
     0.120   BLENDING               BLENDING-COMPLETE 
     0.120   BLENDING               SET-BUFFER-CHUNK BLENDING CHUNK2 
     0.120   PROCEDURAL             CONFLICT-RESOLUTION 
     0.120   ------                 Stopped because no events left to process 
0.12
37
NIL
|#
