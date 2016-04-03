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
       key key-2)
  )

#| Here's a trace of the run
CG-USER(34): (run 10)
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
Finding blended value for slot: SIZE
Matched chunks' slots contain: ("large" "x-large" "tiny")
Magnitude values for those items: ("large" "x-large" "tiny")
When not all magnitudes are numbers or chunks blending based on similarities using those values
 Comparing value "large"
  Chunk A with probability 0.004971579 slot value "large" similarity: 0.0 cumulative result: 0.0
  Chunk B with probability 0.1616471 slot value "x-large" similarity: -0.1 cumulative result: 0.0016164711
  Chunk C with probability 0.8333813 slot value "tiny" similarity: -0.6 cumulative result: 0.30163375
 Comparing value "x-large"
  Chunk A with probability 0.004971579 slot value "large" similarity: -0.1 cumulative result: 4.9715796e-5
  Chunk B with probability 0.1616471 slot value "x-large" similarity: 0.0 cumulative result: 4.9715796e-5
  Chunk C with probability 0.8333813 slot value "tiny" similarity: -0.9 cumulative result: 0.6750885
 Comparing value "tiny"
  Chunk A with probability 0.004971579 slot value "large" similarity: -0.6 cumulative result: 0.0017897686
  Chunk B with probability 0.1616471 slot value "x-large" similarity: -0.9 cumulative result: 0.1327239
  Chunk C with probability 0.8333813 slot value "tiny" similarity: 0.0 cumulative result: 0.1327239
 Final result: tiny
This is the definition of the blended chunk:
(KEY KEY-1 SIZE "tiny")

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
BLENDED SIZE IS "tiny" 
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


Slots to be blended: (SIZE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: ("small")
Magnitude values for those items: ("small")
When not all magnitudes are numbers or chunks blending based on similarities using those values
 Comparing value "small"
  Chunk E with probability 0.90319115 slot value "small" similarity: 0.0 cumulative result: 0.0
 Final result: small
This is the definition of the blended chunk:
(KEY KEY-2 SIZE "small")

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
