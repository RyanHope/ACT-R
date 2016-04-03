;; Simple model to show blending in action.
;; Load the model and call run to see the 
;; trace of two blended retrievals.  The
;; second one will usually fail because
;; it will be below the retrieval threshold.
;;
;; This is a simple case without partial
;; matching enabled and demonstrating both
;; a numeric value blending and a chunk based 
;; blending.


(clear-all)


(define-model test-blending
    (sgp :seed (1 1) :v t :blt t :esc t :ans .25 :rt 4)
  
  (chunk-type target key value size)
  (chunk-type size (size-type t))
  
  ;; some chunks which don't need to be in DM
  (define-chunks 
      (dummy isa chunk)
      (key-1 isa chunk)
      (key-2 isa chunk))

  
  ;; Here are the chunks for the blending test
  
  (add-dm 
   
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
       key key-1)
  
  (p p2
     =blending>
       isa target
       value =val
       size  =size
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
       key key-2)
  )

#| Here's a trace of the run
CG-USER(24): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.050   BLENDING               START-BLENDING 
 0[5]: (ACT-R-RANDOM 1.0)
 0[5]: returned 0.9971848
 0[5]: (ACT-R-RANDOM 1.0)
 0[5]: returned 0.7203245
 0[5]: (ACT-R-RANDOM 1.0)
 0[5]: returned 0.93255734
chunk-list: (D E C B A) matching-chunks: (A B C) activation-list: ((2.5325232 1290.8514 A) (3.763482 41971.047 B) (4.3433366 216384.25 C))
Blending request for chunks with slots (KEY) 
Blending temperature defaults to (* (sqrt 2) :ans): 0.35355338
Chunk A matches blending request
  Activation 2.5325232
  Probability of recall 0.004971579

Chunk B matches blending request
  Activation 3.763482
  Probability of recall 0.1616471

Chunk C matches blending request
  Activation 4.3433366
  Probability of recall 0.8333813


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (LARGE X-LARGE TINY)
Magnitude values for those items: (LARGE X-LARGE TINY)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (SIZE-TYPE)
 Comparing value X-LARGE
  Chunk A with probability 0.004971579 slot value LARGE similarity: -0.1 cumulative result: 4.9715796e-5
  Chunk B with probability 0.1616471 slot value X-LARGE similarity: 0.0 cumulative result: 4.9715796e-5
  Chunk C with probability 0.8333813 slot value TINY similarity: -0.9 cumulative result: 0.6750885
 Comparing value LARGE
  Chunk A with probability 0.004971579 slot value LARGE similarity: 0.0 cumulative result: 0.0
  Chunk B with probability 0.1616471 slot value X-LARGE similarity: -0.1 cumulative result: 0.0016164711
  Chunk C with probability 0.8333813 slot value TINY similarity: -0.6 cumulative result: 0.30163375
 Comparing value MEDIUM
  Chunk A with probability 0.004971579 slot value LARGE similarity: -0.1 cumulative result: 4.9715796e-5
  Chunk B with probability 0.1616471 slot value X-LARGE similarity: -0.3 cumulative result: 0.014597955
  Chunk C with probability 0.8333813 slot value TINY similarity: -0.3 cumulative result: 0.08960227
 Comparing value SMALL
  Chunk A with probability 0.004971579 slot value LARGE similarity: -0.3 cumulative result: 4.4744214e-4
  Chunk B with probability 0.1616471 slot value X-LARGE similarity: -0.6 cumulative result: 0.058640398
  Chunk C with probability 0.8333813 slot value TINY similarity: -0.1 cumulative result: 0.06697421
 Comparing value TINY
  Chunk A with probability 0.004971579 slot value LARGE similarity: -0.6 cumulative result: 0.0017897686
  Chunk B with probability 0.1616471 slot value X-LARGE similarity: -0.9 cumulative result: 0.1327239
  Chunk C with probability 0.8333813 slot value TINY similarity: 0.0 cumulative result: 0.1327239
 Final result: SMALL
Finding blended value for slot: VALUE
Matched chunks' slots contain: (1 2 3)
Magnitude values for those items: (1 2 3)
With numeric magnitudes blending by weighted average
 Chunk A with probability 0.004971579 times magnitude 1.0 = 0.004971579 cumulative result: 0.004971579
 Chunk B with probability 0.1616471 times magnitude 2.0 = 0.3232942 cumulative result: 0.3282658
 Chunk C with probability 0.8333813 times magnitude 3.0 = 2.500144 cumulative result: 2.8284097
 Final result: 2.8284097
This is the definition of the blended chunk:
(KEY KEY-1 SIZE SMALL VALUE 2.8284097)

Computing activation and latency for the blended chunk
 Activation of chunk A is 2.5325232
 Activation of chunk B is 3.763482
 Activation of chunk C is 4.3433366
Activation for blended chunk is: 4.8876944
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.058   BLENDING               BLENDING-COMPLETE 
     0.058   BLENDING               SET-BUFFER-CHUNK BLENDING CHUNK0 
     0.058   PROCEDURAL             CONFLICT-RESOLUTION 
     0.108   PROCEDURAL             PRODUCTION-FIRED P2 
BLENDED VALUE IS 2.8284097 AND SIZE IS SMALL 
     0.108   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.108   BLENDING               START-BLENDING 
 0[5]: (ACT-R-RANDOM 1.0)
 0[5]: returned 1.1438108e-4
 0[5]: (ACT-R-RANDOM 1.0)
 0[5]: returned 0.12812445
chunk-list: (D CHUNK0-0 E C B A) matching-chunks: (E D) activation-list: ((3.2689652 10363.34 E) (2.4794111 1110.7976 D))
Blending request for chunks with slots (KEY) 
Blending temperature defaults to (* (sqrt 2) :ans): 0.35355338
Chunk E matches blending request
  Activation 3.2689652
  Probability of recall 0.90319115

Chunk D matches blending request
  Activation 2.4794111
  Probability of recall 0.096808806


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL)
Magnitude values for those items: (SMALL)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (SIZE-TYPE)
 Comparing value X-LARGE
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.6 cumulative result: 0.32514882
 Comparing value LARGE
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.3 cumulative result: 0.081287205
 Comparing value MEDIUM
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.1 cumulative result: 0.009031912
 Comparing value SMALL
  Chunk E with probability 0.90319115 slot value SMALL similarity: 0.0 cumulative result: 0.0
 Comparing value TINY
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.1 cumulative result: 0.009031912
 Final result: SMALL
Finding blended value for slot: VALUE
Matched chunks' slots contain: (3 1)
Magnitude values for those items: (3 1)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.90319115 times magnitude 3.0 = 2.7095735 cumulative result: 2.7095735
 Chunk D with probability 0.096808806 times magnitude 1.0 = 0.096808806 cumulative result: 2.8063824
 Final result: 2.8063824
This is the definition of the blended chunk:
(KEY KEY-2 SIZE SMALL VALUE 2.8063824)

Computing activation and latency for the blended chunk
 Activation of chunk E is 3.2689652
 Activation of chunk D is 2.4794111
Activation for blended chunk is: 3.643316
Not above threshold so blending failed
     0.108   PROCEDURAL             CONFLICT-RESOLUTION 
     0.126   BLENDING               BLENDING-FAILURE 
     0.126   PROCEDURAL             CONFLICT-RESOLUTION 
     0.126   ------                 Stopped because no events left to process 
0.126
28
NIL
|#
