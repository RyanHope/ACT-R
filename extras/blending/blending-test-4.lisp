;; Simple model to show blending in action.
;; Load the model and call run to see the 
;; trace of two blended retrievals.  The
;; second one will usually fail because
;; it will be below the retrieval threshold.
;;
;; This example uses the magnitude to value and
;; value to magnitude functions for the 
;; blending module to map the values in the
;; slots to numbers and then back.

(clear-all)

;; A simple linear mapping of the
;; size chunks onto numbers.

(defun map-chunks-to-numbers (value)
  (case value
    (tiny 1)
    (small 2)
    (medium 3)
    (large 4)
    (x-large 5)
    (t ;assume medium for an empty slot
     3)))

;; return the closest chunk to the 
;; number based on the above mapping

(defun map-numbers-to-sizes (value chunk-type)
  (declare (ignore chunk-type)) ;; don't need it for this example
  (if (numberp value)
      (cond ((< value 1.5)
             'tiny)
            ((< value 2.5)
             'small)
            ((< value 3.5)
             'medium)
            ((< value 4.5)
             'large)
            (t 'x-large))))
    

(define-model test-blending
    (sgp :seed (1 1) :v t :blt t :esc t :ans .25 :rt 4
         :value->mag map-chunks-to-numbers
         :mag->value map-numbers-to-sizes)
  
  (chunk-type target key size)
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
   
   (a isa target key key-1 size large)
   (b isa target key key-1 size x-large)
   (c isa target key key-1 size tiny)
   (d isa target key key-2 size nil)
   (e isa target key key-2 size small))
  
  
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
CG-USER(17): (run 1)
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
Matched chunks' slots contain: (TINY X-LARGE LARGE)
Magnitude values for those items: (1 5 4)
With numeric magnitudes blending by weighted average
 Chunk C with probability 0.2851124 times magnitude 1.0 cumulative result: 0.2851124
 Chunk B with probability 0.5479227 times magnitude 5.0 cumulative result: 3.0247257
 Chunk A with probability 0.16696489 times magnitude 4.0 cumulative result: 3.6925852
 Final result: 3.6925852  Converted to value: LARGE
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-1 SIZE LARGE)

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
BLENDED SIZE IS LARGE 
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
Matched chunks' slots contain: (SMALL NIL)
Magnitude values for those items: (2 3)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.90319115 times magnitude 2.0 cumulative result: 1.8063823
 Chunk D with probability 0.096808806 times magnitude 3.0 cumulative result: 2.0968087
 Final result: 2.0968087  Converted to value: SMALL
This is the definition of the blended chunk:
(ISA TARGET KEY KEY-2 SIZE SMALL)

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