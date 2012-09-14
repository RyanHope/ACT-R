;; Simple model to show blending in action.
;; Load the model and call run to see the 
;; trace of two blended retrievals.  
;;
;; This is the same model as blending-test-1 
;; except this one uses the :do-not-generalize
;; request parameter in the first request now
;; for the size slot.  Thus, instead of considering
;; all chunks which "isa size" only those values
;; which were requested are tested.

(clear-all)


(define-model test-blending
    (sgp :seed (1 1) :v t :blt t :esc t :ans .25 :rt 4)
  
  (chunk-type target key value size)
  (chunk-type size)
  
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
       key key-1
      ; Do not allow the size slot be blended with
      ; blending method c as described in the readme
       :do-not-generalize (size)
     )
  
  (p p2
     =blending>
       isa target
       value =val
       size  =size
     ==>
     !output! (blended value is =val and size is =size)
     
     ; Overwrite the blended chunk to prevent it from 
     ; being added to dm.  Not necessary, but keeps the 
     ; examples simpler.
     
     =blending> dummy
     
     +blending>
       isa target
       key key-2)
  )

#| Here's a trace of the run
CG-USER(8): (run 10)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.050   BLENDING               START-BLENDING 
Blending request for chunks of type TARGET
Blending temperature defaults to (* (sqrt 2) :ans): 0.35355338
Chunk C matches blending request
  Activation 3.5325232
  Probability of recall 0.2851124

Chunk B matches blending request
  Activation 3.763482
  Probability of recall 0.5479227

Chunk A matches blending request
  Activation 3.3433368
  Probability of recall 0.16696489


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (TINY X-LARGE LARGE)
Magnitude values for those items: (TINY X-LARGE LARGE)
When not all magnitudes are numbers or chunks blending based on similiarities using only those values
 Comparing value TINY
  Chunk C with probability 0.2851124 slot value TINY similarity: 0.0 cumulative result: 0.0
  Chunk B with probability 0.5479227 slot value X-LARGE similarity: -0.9 cumulative result: 0.44381732
  Chunk A with probability 0.16696489 slot value LARGE similarity: -0.6 cumulative result: 0.50392467
 Comparing value X-LARGE
  Chunk C with probability 0.2851124 slot value TINY similarity: -0.9 cumulative result: 0.23094104
  Chunk B with probability 0.5479227 slot value X-LARGE similarity: 0.0 cumulative result: 0.23094104
  Chunk A with probability 0.16696489 slot value LARGE similarity: -0.1 cumulative result: 0.23261069
 Comparing value LARGE
  Chunk C with probability 0.2851124 slot value TINY similarity: -0.6 cumulative result: 0.10264047
  Chunk B with probability 0.5479227 slot value X-LARGE similarity: -0.1 cumulative result: 0.1081197
  Chunk A with probability 0.16696489 slot value LARGE similarity: 0.0 cumulative result: 0.1081197
 Final result: LARGE
Finding blended value for slot: VALUE
Matched chunks' slots contain: (3 2 1)
Magnitude values for those items: (3 2 1)
With numeric magnitudes blending by weighted average
 Chunk C with probability 0.2851124 times magnitude 3.0 cumulative result: 0.85533726
 Chunk B with probability 0.5479227 times magnitude 2.0 cumulative result: 1.9511826
 Chunk A with probability 0.16696489 times magnitude 1.0 cumulative result: 2.1181474
 Final result: 2.1181474
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-1 SIZE LARGE VALUE 2.1181474)

Computing activation and latency for the blended chunk
 Activation of chunk C is 3.5325232
 Activation of chunk B is 3.763482
 Activation of chunk A is 3.3433368
Activation for blended chunk is: 4.6598654
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.059   BLENDING               BLENDING-COMPLETE 
     0.059   BLENDING               SET-BUFFER-CHUNK BLENDING TARGET0 
     0.059   PROCEDURAL             CONFLICT-RESOLUTION 
     0.109   PROCEDURAL             PRODUCTION-FIRED P2 
BLENDED VALUE IS 2.1181474 AND SIZE IS LARGE 
     0.109   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.109   BLENDING               START-BLENDING 
Blending request for chunks of type TARGET
Blending temperature defaults to (* (sqrt 2) :ans): 0.35355338
Chunk E matches blending request
  Activation 3.2689652
  Probability of recall 0.90319115

Chunk D matches blending request
  Activation 2.4794111
  Probability of recall 0.096808806


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL NIL)
Magnitude values for those items: (SMALL NIL)
When all magnitudes are chunks or nil blending based on common chunk-types and similiarities
Common chunk-type for values is: SIZE
 Comparing value TINY
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.1 cumulative result: 0.009031912
  Chunk D with probability 0.096808806 slot value NIL similarity: -1.0 cumulative result: 0.10584072
 Comparing value SMALL
  Chunk E with probability 0.90319115 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.096808806 slot value NIL similarity: -1.0 cumulative result: 0.096808806
 Comparing value MEDIUM
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.1 cumulative result: 0.009031912
  Chunk D with probability 0.096808806 slot value NIL similarity: -1.0 cumulative result: 0.10584072
 Comparing value LARGE
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.3 cumulative result: 0.081287205
  Chunk D with probability 0.096808806 slot value NIL similarity: -1.0 cumulative result: 0.17809601
 Comparing value X-LARGE
  Chunk E with probability 0.90319115 slot value SMALL similarity: -0.6 cumulative result: 0.32514882
  Chunk D with probability 0.096808806 slot value NIL similarity: -1.0 cumulative result: 0.4219576
 Final result: SMALL
Finding blended value for slot: VALUE
Matched chunks' slots contain: (3 1)
Magnitude values for those items: (3 1)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.90319115 times magnitude 3.0 cumulative result: 2.7095735
 Chunk D with probability 0.096808806 times magnitude 1.0 cumulative result: 2.8063824
 Final result: 2.8063824
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-2 SIZE SMALL VALUE 2.8063824)

Computing activation and latency for the blended chunk
 Activation of chunk E is 3.2689652
 Activation of chunk D is 2.4794111
Activation for blended chunk is: 3.643316
Not above threshold so blending failed
     0.109   PROCEDURAL             CONFLICT-RESOLUTION 
     0.127   BLENDING               BLENDING-FAILURE 
     0.127   PROCEDURAL             CONFLICT-RESOLUTION 
     0.127   ------                 Stopped because no events left to process 
0.127
26
NIL
CG-USER(6): 
|#
