;; This model is essentially the same as blending-test-1
;; except now it uses the :ignore-slots request parameter
;; so that not all of the slots get a blended result.


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
       :ignore-slots (value)
       key key-1)
  
  (p p2
     =blending>
       isa target
       size  =size
     ?blending>
       state free
     ==>
     !output! (blended size is =size)
     
     ; Overwrite the blended chunk to prevent it from 
     ; being added to dm.  Not necessary, but keeps the 
     ; examples simpler.
     
     @blending> dummy
     
     +blending>
       isa target
       :ignore-slots (value size)
       key key-2)
  )

#| Here's a trace of the run
CG-USER(46): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.050   BLENDING               START-BLENDING 
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


Slots to be blended: (SIZE)
Slots being explicitly ignored: (VALUE)

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
This is the definition of the blended chunk:
(KEY KEY-1 SIZE SMALL)

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
BLENDED SIZE IS SMALL 
     0.108   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.108   BLENDING               START-BLENDING 
Blending request for chunks with slots (KEY) 
Blending temperature defaults to (* (sqrt 2) :ans): 0.35355338
Chunk E matches blending request
  Activation 3.2689652
  Probability of recall 0.90319115

Chunk D matches blending request
  Activation 2.4794111
  Probability of recall 0.096808806


Slots to be blended: NIL
Slots being explicitly ignored: (VALUE SIZE)

This is the definition of the blended chunk:
(KEY KEY-2)

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
