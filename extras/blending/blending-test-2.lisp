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
       key key-1)
  
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

CG-USER(6): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.050   BLENDING               START-BLENDING 
Blending request for chunks of type TARGET
Blending temperature is: 1.5
Chunk E matches blending request
  Activation 0.370572
  Probability of recall 0.027960265

Chunk D matches blending request
  Activation 1.906558
  Probability of recall 0.07784931

Chunk C matches blending request
  Activation 4.347349
  Probability of recall 0.3962198

Chunk B matches blending request
  Activation 3.9501731
  Probability of recall 0.30404788

Chunk A matches blending request
  Activation 3.2755852
  Probability of recall 0.19392276


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL NIL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL NIL TINY X-LARGE LARGE)
When all magnitudes are chunks or nil blending based on common chunk-types and similiarities
Common chunk-type for values is: SIZE
 Comparing value TINY
  Chunk E with probability 0.027960265 slot value SMALL similarity: -0.1 cumulative result: 2.7960268e-4
  Chunk D with probability 0.07784931 slot value NIL similarity: -1.0 cumulative result: 0.07812892
  Chunk C with probability 0.3962198 slot value TINY similarity: 0.0 cumulative result: 0.07812892
  Chunk B with probability 0.30404788 slot value X-LARGE similarity: -0.9 cumulative result: 0.3244077
  Chunk A with probability 0.19392276 slot value LARGE similarity: -0.6 cumulative result: 0.39421988
 Comparing value SMALL
  Chunk E with probability 0.027960265 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.07784931 slot value NIL similarity: -1.0 cumulative result: 0.07784931
  Chunk C with probability 0.3962198 slot value TINY similarity: -0.1 cumulative result: 0.08181151
  Chunk B with probability 0.30404788 slot value X-LARGE similarity: -0.6 cumulative result: 0.19126874
  Chunk A with probability 0.19392276 slot value LARGE similarity: -0.3 cumulative result: 0.20872179
 Comparing value MEDIUM
  Chunk E with probability 0.027960265 slot value SMALL similarity: -0.1 cumulative result: 2.7960268e-4
  Chunk D with probability 0.07784931 slot value NIL similarity: -1.0 cumulative result: 0.07812892
  Chunk C with probability 0.3962198 slot value TINY similarity: -0.3 cumulative result: 0.1137887
  Chunk B with probability 0.30404788 slot value X-LARGE similarity: -0.3 cumulative result: 0.14115301
  Chunk A with probability 0.19392276 slot value LARGE similarity: -0.1 cumulative result: 0.14309223
 Comparing value LARGE
  Chunk E with probability 0.027960265 slot value SMALL similarity: -0.3 cumulative result: 0.002516424
  Chunk D with probability 0.07784931 slot value NIL similarity: -1.0 cumulative result: 0.08036574
  Chunk C with probability 0.3962198 slot value TINY similarity: -0.6 cumulative result: 0.22300488
  Chunk B with probability 0.30404788 slot value X-LARGE similarity: -0.1 cumulative result: 0.22604536
  Chunk A with probability 0.19392276 slot value LARGE similarity: 0.0 cumulative result: 0.22604536
 Comparing value X-LARGE
  Chunk E with probability 0.027960265 slot value SMALL similarity: -0.6 cumulative result: 0.010065696
  Chunk D with probability 0.07784931 slot value NIL similarity: -1.0 cumulative result: 0.08791501
  Chunk C with probability 0.3962198 slot value TINY similarity: -0.9 cumulative result: 0.40885302
  Chunk B with probability 0.30404788 slot value X-LARGE similarity: 0.0 cumulative result: 0.40885302
  Chunk A with probability 0.19392276 slot value LARGE similarity: -0.1 cumulative result: 0.41079226
 Final result: MEDIUM
Finding blended value for slot: VALUE
Matched chunks' slots contain: (3 1 3 2 1)
Magnitude values for those items: (3 1 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.027960265 times magnitude 3.0 cumulative result: 0.0838808
 Chunk D with probability 0.07784931 times magnitude 1.0 cumulative result: 0.16173011
 Chunk C with probability 0.3962198 times magnitude 3.0 cumulative result: 1.3503895
 Chunk B with probability 0.30404788 times magnitude 2.0 cumulative result: 1.9584852
 Chunk A with probability 0.19392276 times magnitude 1.0 cumulative result: 2.1524081
 Final result: 2.1524081
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-1 SIZE MEDIUM VALUE 2.1524081)

Computing activation and latency for the blended chunk
 Activation of chunk E is 0.370572
 Activation of chunk D is 1.906558
 Activation of chunk C is 4.347349
 Activation of chunk B is 3.9501731
 Activation of chunk A is 3.2755852
Activation for blended chunk is: 5.098981
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.056   BLENDING               BLENDING-COMPLETE 
     0.056   BLENDING               SET-BUFFER-CHUNK BLENDING TARGET0 
     0.056   PROCEDURAL             CONFLICT-RESOLUTION 
     0.106   PROCEDURAL             PRODUCTION-FIRED P2 
BLENDED VALUE IS 2.1524081 AND SIZE IS MEDIUM 
     0.106   PROCEDURAL             CLEAR-BUFFER BLENDING 
     0.106   BLENDING               START-BLENDING 
Blending request for chunks of type TARGET
Blending temperature is: 1.5
Chunk E matches blending request
  Activation 1.065042
  Probability of recall 0.053900074

Chunk D matches blending request
  Activation 2.0152557
  Probability of recall 0.101555735

Chunk C matches blending request
  Activation 4.080316
  Probability of recall 0.4023469

Chunk B matches blending request
  Activation 3.1878269
  Probability of recall 0.22192109

Chunk A matches blending request
  Activation 3.1766677
  Probability of recall 0.22027625


Slots to be blended: (SIZE VALUE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL NIL TINY X-LARGE LARGE)
Magnitude values for those items: (SMALL NIL TINY X-LARGE LARGE)
When all magnitudes are chunks or nil blending based on common chunk-types and similiarities
Common chunk-type for values is: SIZE
 Comparing value TINY
  Chunk E with probability 0.053900074 slot value SMALL similarity: -0.1 cumulative result: 5.390008e-4
  Chunk D with probability 0.101555735 slot value NIL similarity: -1.0 cumulative result: 0.10209473
  Chunk C with probability 0.4023469 slot value TINY similarity: 0.0 cumulative result: 0.10209473
  Chunk B with probability 0.22192109 slot value X-LARGE similarity: -0.9 cumulative result: 0.28185079
  Chunk A with probability 0.22027625 slot value LARGE similarity: -0.6 cumulative result: 0.36115023
 Comparing value SMALL
  Chunk E with probability 0.053900074 slot value SMALL similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.101555735 slot value NIL similarity: -1.0 cumulative result: 0.101555735
  Chunk C with probability 0.4023469 slot value TINY similarity: -0.1 cumulative result: 0.105579205
  Chunk B with probability 0.22192109 slot value X-LARGE similarity: -0.6 cumulative result: 0.18547079
  Chunk A with probability 0.22027625 slot value LARGE similarity: -0.3 cumulative result: 0.20529565
 Comparing value MEDIUM
  Chunk E with probability 0.053900074 slot value SMALL similarity: -0.1 cumulative result: 5.390008e-4
  Chunk D with probability 0.101555735 slot value NIL similarity: -1.0 cumulative result: 0.10209473
  Chunk C with probability 0.4023469 slot value TINY similarity: -0.3 cumulative result: 0.13830596
  Chunk B with probability 0.22192109 slot value X-LARGE similarity: -0.3 cumulative result: 0.15827885
  Chunk A with probability 0.22027625 slot value LARGE similarity: -0.1 cumulative result: 0.16048162
 Comparing value LARGE
  Chunk E with probability 0.053900074 slot value SMALL similarity: -0.3 cumulative result: 0.004851007
  Chunk D with probability 0.101555735 slot value NIL similarity: -1.0 cumulative result: 0.10640674
  Chunk C with probability 0.4023469 slot value TINY similarity: -0.6 cumulative result: 0.25125164
  Chunk B with probability 0.22192109 slot value X-LARGE similarity: -0.1 cumulative result: 0.25347084
  Chunk A with probability 0.22027625 slot value LARGE similarity: 0.0 cumulative result: 0.25347084
 Comparing value X-LARGE
  Chunk E with probability 0.053900074 slot value SMALL similarity: -0.6 cumulative result: 0.019404028
  Chunk D with probability 0.101555735 slot value NIL similarity: -1.0 cumulative result: 0.12095976
  Chunk C with probability 0.4023469 slot value TINY similarity: -0.9 cumulative result: 0.44686073
  Chunk B with probability 0.22192109 slot value X-LARGE similarity: 0.0 cumulative result: 0.44686073
  Chunk A with probability 0.22027625 slot value LARGE similarity: -0.1 cumulative result: 0.44906348
 Final result: MEDIUM
Finding blended value for slot: VALUE
Matched chunks' slots contain: (3 1 3 2 1)
Magnitude values for those items: (3 1 3 2 1)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.053900074 times magnitude 3.0 cumulative result: 0.16170022
 Chunk D with probability 0.101555735 times magnitude 1.0 cumulative result: 0.26325595
 Chunk C with probability 0.4023469 times magnitude 3.0 cumulative result: 1.4702967
 Chunk B with probability 0.22192109 times magnitude 2.0 cumulative result: 1.9141389
 Chunk A with probability 0.22027625 times magnitude 1.0 cumulative result: 2.1344151
 Final result: 2.1344151
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-2 SIZE MEDIUM VALUE 2.1344151)

Computing activation and latency for the blended chunk
 Activation of chunk E is 1.065042
 Activation of chunk D is 2.0152557
 Activation of chunk C is 4.080316
 Activation of chunk B is 3.1878269
 Activation of chunk A is 3.1766677
Activation for blended chunk is: 4.7687354
Not above threshold so blending failed
     0.106   PROCEDURAL             CONFLICT-RESOLUTION 
     0.113   BLENDING               BLENDING-FAILURE 
     0.113   PROCEDURAL             CONFLICT-RESOLUTION 
     0.113   ------                 Stopped because no events left to process 
0.113
25
NIL
CG-USER(7): 
|#