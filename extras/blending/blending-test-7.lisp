;; Simple model to show blending in action
;; while using inequality constraints to
;; limit the set of chunks retrieved and
;; that when the values to be blended 
;; don't have common slots every chunk
;; in dm is a potential candidate.
;; Load the model and call run to see the 
;; trace of a blended retrieval. 
;;


(clear-all)


(define-model test-blending
    (sgp :seed (1 1) :v t :blt t :esc t :ans .25 :rt 4)
  
  (chunk-type target key value size)
  (chunk-type size (size-type t))
  
  ;; some chunks which don't need to be in DM
  (define-chunks 
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
  
  ;; Very simple model.
  ;; Make a blending request for a target chunk
  ;; with any value in the key slot and a value
  ;; slot which is in the range (1,3].
  
  (p p1
     ?blending>
       state free
       buffer empty
       error nil
     ==>
     +blending>
       isa target
     - key nil
     <= value 3
     > value 1)
  
  (p p2
     =blending>
       isa target
       value =val
       size  =size
       key   =key
     ?blending>
       state free
     ==>
     !output! (blended value is =val size is =size and key is =key)
     =blending>
       value nil
     )
  )

#| Here's a trace of the run
CG-USER(35): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-FIRED P1
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING
     0.050   BLENDING               START-BLENDING
Blending request for chunks with slots (KEY VALUE) 
Blending temperature defaults to (* (sqrt 2) :ans): 0.35355338
Chunk B matches blending request
  Activation 2.5325232
  Probability of recall 0.0018145364

Chunk C matches blending request
  Activation 4.763482
  Probability of recall 0.99818176

Chunk E matches blending request
  Activation 0.34333676
  Probability of recall 3.7122234e-6


Slots to be blended: (SIZE VALUE KEY)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (X-LARGE TINY SMALL)
Magnitude values for those items: (X-LARGE TINY SMALL)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (SIZE-TYPE)
 Comparing value X-LARGE
  Chunk B with probability 0.0018145364 slot value X-LARGE similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.99818176 slot value TINY similarity: -0.9 cumulative result: 0.8085272
  Chunk E with probability 3.7122234e-6 slot value SMALL similarity: -0.6 cumulative result: 0.8085285
 Comparing value LARGE
  Chunk B with probability 0.0018145364 slot value X-LARGE similarity: -0.1 cumulative result: 1.8145365e-5
  Chunk C with probability 0.99818176 slot value TINY similarity: -0.6 cumulative result: 0.3593636
  Chunk E with probability 3.7122234e-6 slot value SMALL similarity: -0.3 cumulative result: 0.3593639
 Comparing value MEDIUM
  Chunk B with probability 0.0018145364 slot value X-LARGE similarity: -0.3 cumulative result: 1.6330829e-4
  Chunk C with probability 0.99818176 slot value TINY similarity: -0.3 cumulative result: 0.08999967
  Chunk E with probability 3.7122234e-6 slot value SMALL similarity: -0.1 cumulative result: 0.089999706
 Comparing value SMALL
  Chunk B with probability 0.0018145364 slot value X-LARGE similarity: -0.6 cumulative result: 6.5323315e-4
  Chunk C with probability 0.99818176 slot value TINY similarity: -0.1 cumulative result: 0.010635052
  Chunk E with probability 3.7122234e-6 slot value SMALL similarity: 0.0 cumulative result: 0.010635052
 Comparing value TINY
  Chunk B with probability 0.0018145364 slot value X-LARGE similarity: -0.9 cumulative result: 0.0014697744
  Chunk C with probability 0.99818176 slot value TINY similarity: 0.0 cumulative result: 0.0014697744
  Chunk E with probability 3.7122234e-6 slot value SMALL similarity: -0.1 cumulative result: 0.0014698115
 Final result: TINY
Finding blended value for slot: VALUE
Matched chunks' slots contain: (2 3 3)
Magnitude values for those items: (2 3 3)
With numeric magnitudes blending by weighted average
 Chunk B with probability 0.0018145364 times magnitude 2.0 = 0.0036290728 cumulative result: 0.0036290728
 Chunk C with probability 0.99818176 times magnitude 3.0 = 2.9945452 cumulative result: 2.9981742
 Chunk E with probability 3.7122234e-6 times magnitude 3.0 = 1.113667e-5 cumulative result: 2.9981854
 Final result: 2.9981854
Finding blended value for slot: KEY
Matched chunks' slots contain: (KEY-1 KEY-1 KEY-2)
Magnitude values for those items: (KEY-1 KEY-1 KEY-2)
When all magnitudes are chunks blending based on similarities to all related chunks
No intersecting slots found all chunks will be tested
 Comparing value D
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value E
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value C
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value B
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value A
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value X-LARGE
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value LARGE
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value MEDIUM
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value SMALL
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Comparing value TINY
  Chunk B with probability 0.0018145364 slot value KEY-1 similarity: -1.0 cumulative result: 0.0018145364
  Chunk C with probability 0.99818176 slot value KEY-1 similarity: -1.0 cumulative result: 0.9999963
  Chunk E with probability 3.7122234e-6 slot value KEY-2 similarity: -1.0 cumulative result: 1.0
 Final result: D
This is the definition of the blended chunk:
(SIZE TINY VALUE 2.9981854 KEY D)

Computing activation and latency for the blended chunk
 Activation of chunk B is 2.5325232
 Activation of chunk C is 4.763482
 Activation of chunk E is 0.34333676
Activation for blended chunk is: 4.8763266
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.058   BLENDING               BLENDING-COMPLETE
     0.058   BLENDING               SET-BUFFER-CHUNK BLENDING CHUNK0
     0.058   PROCEDURAL             CONFLICT-RESOLUTION
     0.108   PROCEDURAL             PRODUCTION-FIRED P2
BLENDED VALUE IS 2.9981854 SIZE IS TINY AND KEY IS D 
     0.108   PROCEDURAL             CONFLICT-RESOLUTION
     0.108   ------                 Stopped because no events left to process
0.108
23
NIL
|#
