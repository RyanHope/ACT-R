;; Simple model to show blending in action.
;; Load the model and call run to see the 
;; trace of two blended retrievals.  The
;; second one will usually fail because
;; it will be below the retrieval threshold.
;;
;; This test does not use partial matching 
;; and does the blending over the third case -
;; non-number and non-chunk values in the slots.
;; That requires a similarity hook function to
;; be defined as one would if using partial 
;; matching to provide the similarities for those
;; non-chunk values.


(clear-all)


(defun string-similarities (s1 s2)
  (when (and (stringp s1) (stringp s2))
    (case (read-from-string s1)
      (tiny
       (case (read-from-string s2)
         (tiny 0)
         (small -.1)
         (medium -.3)
         (large -.6)
         (x-large -.9)))
      (small 
       (case (read-from-string s2)
         (tiny -.1)
         (small 0)
         (medium -.1)
         (large -.3)
         (x-large -.6)))
      (medium 
       (case (read-from-string s2)
         (tiny -.3)
         (small -.1)
         (medium 0)
         (large -.1)
         (x-large -.3)))
      (large 
       (case (read-from-string s2)
         (tiny -.6)
         (small -.3)
         (medium -.1)
         (large 0)
         (x-large -.1)))
      (x-large 
       (case (read-from-string s2)
         (tiny -.9)
         (small -.6)
         (medium -.3)
         (large -.1)
         (x-large 0))))))
    

(define-model test-blending
    (sgp :seed (1 1) :v t :blt t :esc t :ans .25 :rt 4 :sim-hook string-similarities)
  
  (chunk-type target key size)
  
  ;; some chunks which don't need to be in DM
  (define-chunks 
      (dummy isa chunk)
      (key-1 isa chunk)
      (key-2 isa chunk))

  
  ;; Here are the chunks for the blending test
  
  (add-dm 
   
   ;; A set of target chunks which will be
   ;; the items for the blended retrieval.
   
   (a isa target key key-1 size "large")
   (b isa target key key-1 size "x-large")
   (c isa target key key-1 size "tiny")
   (d isa target key key-2 size nil)
   (e isa target key key-2 size "small"))
  
  
  ;; Set some arbitrary base-level activations for the target
  ;; chunks to provide some systematic variability in the values.
  
  (set-base-levels (a 4 -10) (b 4 -10) (c 5 -10) (d 2 -10) (e 1 -10))
  
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
       size  =size
     ==>
     !output! (blended size is =size)
     
     ; Overwrite the blended chunk to prevent it from 
     ; being added to dm.  Not necessary, but keeps the 
     ; examples simpler.
     
     =blending> dummy
     
     +blending>
       isa target
       key key-2)
  )

#| Here's a trace of the run
CG-USER(14): (run 1)
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


Slots to be blended: (SIZE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: ("tiny" "x-large" "large")
Magnitude values for those items: ("tiny" "x-large" "large")
When not all magnitudes are numbers or chunks blending based on similiarities using only those values
 Comparing value "tiny"
  Chunk C with probability 0.2851124 slot value "tiny" similarity: 0.0 cumulative result: 0.0
  Chunk B with probability 0.5479227 slot value "x-large" similarity: -0.9 cumulative result: 0.44381732
  Chunk A with probability 0.16696489 slot value "large" similarity: -0.6 cumulative result: 0.50392467
 Comparing value "x-large"
  Chunk C with probability 0.2851124 slot value "tiny" similarity: -0.9 cumulative result: 0.23094104
  Chunk B with probability 0.5479227 slot value "x-large" similarity: 0.0 cumulative result: 0.23094104
  Chunk A with probability 0.16696489 slot value "large" similarity: -0.1 cumulative result: 0.23261069
 Comparing value "large"
  Chunk C with probability 0.2851124 slot value "tiny" similarity: -0.6 cumulative result: 0.10264047
  Chunk B with probability 0.5479227 slot value "x-large" similarity: -0.1 cumulative result: 0.1081197
  Chunk A with probability 0.16696489 slot value "large" similarity: 0.0 cumulative result: 0.1081197
 Final result: large
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-1 SIZE "large")

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
BLENDED SIZE IS "large" 
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


Slots to be blended: (SIZE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: ("small" NIL)
Magnitude values for those items: ("small" NIL)
When not all magnitudes are numbers or chunks blending based on similiarities using only those values
 Comparing value "small"
  Chunk E with probability 0.90319115 slot value "small" similarity: 0.0 cumulative result: 0.0
  Chunk D with probability 0.096808806 slot value NIL similarity: -1.0 cumulative result: 0.096808806
 Comparing value NIL
  Chunk E with probability 0.90319115 slot value "small" similarity: -1.0 cumulative result: 0.90319115
  Chunk D with probability 0.096808806 slot value NIL similarity: 0.0 cumulative result: 0.90319115
 Final result: small
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-2 SIZE "small")

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
25
NIL
|#