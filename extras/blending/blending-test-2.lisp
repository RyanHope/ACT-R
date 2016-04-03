;; Simple model to show blending in action.
;; Load the model and call run to see the 
;; trace of two blended retrievals.  The
;; second one will usually fail because
;; it will be below the retrieval threshold.
;;
;; Same model as test-1 except now partial
;; matching is enabled and the temperature 
;; parameter is set explicitly.


(clear-all)


(define-model test-blending
    (sgp :seed (2 1) :v t :blt t :esc t :ans .25 :rt 5 :mp 1 :tmp 1.5)
  
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
CG-USER(31): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.050   BLENDING               START-BLENDING 
Blending request for chunks with slots (KEY) 
Blending temperature is: 1.5
Chunk D matches blending request
  Activation 1.370572
  Probability of recall 0.05508611

Chunk E matches blending request
  Activation 0.906558
  Probability of recall 0.040429305

Chunk C matches blending request
  Activation 4.347349
  Probability of recall 0.40078118

Chunk B matches blending request
  Activation 3.9501731
  Probability of recall 0.30754817

Chunk A matches blending request
  Activation 3.2755852
  Probability of recall 0.19615525


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL TINY X-LARGE LARGE)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (SIZE-TYPE)
 Comparing value X-LARGE
  Chunk E with probability 0.040429305 slot value SMALL similarity: -0.6 cumulative result: 0.014554551
  Chunk C with probability 0.40078118 slot value TINY similarity: -0.9 cumulative result: 0.3391873
  Chunk B with probability 0.30754817 slot value X-LARGE similarity: 0.0 cumulative result: 0.3391873
  Chunk A with probability 0.19615525 slot value LARGE similarity: -0.1 cumulative result: 0.34114885
 Comparing value LARGE
  Chunk E with probability 0.040429305 slot value SMALL similarity: -0.3 cumulative result: 0.0036386377
  Chunk C with probability 0.40078118 slot value TINY similarity: -0.6 cumulative result: 0.14791988
  Chunk B with probability 0.30754817 slot value X-LARGE similarity: -0.1 cumulative result: 0.15099536
  Chunk A with probability 0.19615525 slot value LARGE similarity: 0.0 cumulative result: 0.15099536
 Comparing value MEDIUM
  Chunk E with probability 0.040429305 slot value SMALL similarity: -0.1 cumulative result: 4.042931e-4
  Chunk C with probability 0.40078118 slot value TINY similarity: -0.3 cumulative result: 0.036474604
  Chunk B with probability 0.30754817 slot value X-LARGE similarity: -0.3 cumulative result: 0.06415394
  Chunk A with probability 0.19615525 slot value LARGE similarity: -0.1 cumulative result: 0.06611549
 Comparing value SMALL
  Chunk E with probability 0.040429305 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.40078118 slot value TINY similarity: -0.1 cumulative result: 0.004007812
  Chunk B with probability 0.30754817 slot value X-LARGE similarity: -0.6 cumulative result: 0.11472515
  Chunk A with probability 0.19615525 slot value LARGE similarity: -0.3 cumulative result: 0.13237913
 Comparing value TINY
  Chunk E with probability 0.040429305 slot value SMALL similarity: -0.1 cumulative result: 4.042931e-4
  Chunk C with probability 0.40078118 slot value TINY similarity: 0.0 cumulative result: 4.042931e-4
  Chunk B with probability 0.30754817 slot value X-LARGE similarity: -0.9 cumulative result: 0.24951829
  Chunk A with probability 0.19615525 slot value LARGE similarity: -0.6 cumulative result: 0.3201342
 Final result: MEDIUM
Finding blended value for slot: VALUE
Matched chunks' slots contain: (1 3 3 2 1)
Magnitude values for those items: (1 3 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk D with probability 0.05508611 times magnitude 1.0 = 0.05508611 cumulative result: 0.05508611
 Chunk E with probability 0.040429305 times magnitude 3.0 = 0.12128791 cumulative result: 0.17637402
 Chunk C with probability 0.40078118 times magnitude 3.0 = 1.2023436 cumulative result: 1.3787177
 Chunk B with probability 0.30754817 times magnitude 2.0 = 0.61509633 cumulative result: 1.993814
 Chunk A with probability 0.19615525 times magnitude 1.0 = 0.19615525 cumulative result: 2.1899693
 Final result: 2.1899693
This is the definition of the blended chunk:
(KEY KEY-1 SIZE MEDIUM VALUE 2.1899693)

Computing activation and latency for the blended chunk
 Activation of chunk D is 1.370572
 Activation of chunk E is 0.906558
 Activation of chunk C is 4.347349
 Activation of chunk B is 3.9501731
 Activation of chunk A is 3.2755852
Activation for blended chunk is: 5.0881505
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.056   BLENDING               BLENDING-COMPLETE 
     0.056   BLENDING               SET-BUFFER-CHUNK BLENDING CHUNK0 
     0.056   PROCEDURAL             CONFLICT-RESOLUTION 
     0.106   PROCEDURAL             PRODUCTION-FIRED P2 
BLENDED VALUE IS 2.1899693 AND SIZE IS MEDIUM 
     0.106   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.106   BLENDING               START-BLENDING 
Blending request for chunks with slots (KEY) 
Blending temperature is: 1.5
Chunk D matches blending request
  Activation 2.065042
  Probability of recall 0.10480823

Chunk E matches blending request
  Activation 1.0152556
  Probability of recall 0.052053634

Chunk C matches blending request
  Activation 4.080316
  Probability of recall 0.40167704

Chunk B matches blending request
  Activation 3.1878269
  Probability of recall 0.22155161

Chunk A matches blending request
  Activation 3.1766677
  Probability of recall 0.21990952


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL TINY X-LARGE LARGE)
When all magnitudes are chunks blending based on similarities to all related chunks
Intersection of slots for values is: (SIZE-TYPE)
 Comparing value X-LARGE
  Chunk E with probability 0.052053634 slot value SMALL similarity: -0.6 cumulative result: 0.01873931
  Chunk C with probability 0.40167704 slot value TINY similarity: -0.9 cumulative result: 0.3440977
  Chunk B with probability 0.22155161 slot value X-LARGE similarity: 0.0 cumulative result: 0.3440977
  Chunk A with probability 0.21990952 slot value LARGE similarity: -0.1 cumulative result: 0.3462968
 Comparing value LARGE
  Chunk E with probability 0.052053634 slot value SMALL similarity: -0.3 cumulative result: 0.0046848273
  Chunk C with probability 0.40167704 slot value TINY similarity: -0.6 cumulative result: 0.14928856
  Chunk B with probability 0.22155161 slot value X-LARGE similarity: -0.1 cumulative result: 0.15150408
  Chunk A with probability 0.21990952 slot value LARGE similarity: 0.0 cumulative result: 0.15150408
 Comparing value MEDIUM
  Chunk E with probability 0.052053634 slot value SMALL similarity: -0.1 cumulative result: 5.205364e-4
  Chunk C with probability 0.40167704 slot value TINY similarity: -0.3 cumulative result: 0.03667147
  Chunk B with probability 0.22155161 slot value X-LARGE similarity: -0.3 cumulative result: 0.056611117
  Chunk A with probability 0.21990952 slot value LARGE similarity: -0.1 cumulative result: 0.05881021
 Comparing value SMALL
  Chunk E with probability 0.052053634 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk C with probability 0.40167704 slot value TINY similarity: -0.1 cumulative result: 0.0040167705
  Chunk B with probability 0.22155161 slot value X-LARGE similarity: -0.6 cumulative result: 0.08377536
  Chunk A with probability 0.21990952 slot value LARGE similarity: -0.3 cumulative result: 0.10356721
 Comparing value TINY
  Chunk E with probability 0.052053634 slot value SMALL similarity: -0.1 cumulative result: 5.205364e-4
  Chunk C with probability 0.40167704 slot value TINY similarity: 0.0 cumulative result: 5.205364e-4
  Chunk B with probability 0.22155161 slot value X-LARGE similarity: -0.9 cumulative result: 0.17997734
  Chunk A with probability 0.21990952 slot value LARGE similarity: -0.6 cumulative result: 0.25914478
 Final result: MEDIUM
Finding blended value for slot: VALUE
Matched chunks' slots contain: (1 3 3 2 1)
Magnitude values for those items: (1 3 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk D with probability 0.10480823 times magnitude 1.0 = 0.10480823 cumulative result: 0.10480823
 Chunk E with probability 0.052053634 times magnitude 3.0 = 0.1561609 cumulative result: 0.26096913
 Chunk C with probability 0.40167704 times magnitude 3.0 = 1.2050312 cumulative result: 1.4660003
 Chunk B with probability 0.22155161 times magnitude 2.0 = 0.44310322 cumulative result: 1.9091035
 Chunk A with probability 0.21990952 times magnitude 1.0 = 0.21990952 cumulative result: 2.129013
 Final result: 2.129013
This is the definition of the blended chunk:
(KEY KEY-2 SIZE MEDIUM VALUE 2.129013)

Computing activation and latency for the blended chunk
 Activation of chunk D is 2.065042
 Activation of chunk E is 1.0152556
 Activation of chunk C is 4.080316
 Activation of chunk B is 3.1878269
 Activation of chunk A is 3.1766677
Activation for blended chunk is: 4.770789
Not above threshold so blending failed
     0.106   PROCEDURAL             CONFLICT-RESOLUTION 
     0.113   BLENDING               BLENDING-FAILURE 
     0.113   PROCEDURAL             CONFLICT-RESOLUTION 
     0.113   ------                 Stopped because no events left to process 
0.113
28
NIL
|#
