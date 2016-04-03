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
;;
;; This model demonstrates a potential issue one
;; should be careful about: chunks which match
;; the blending request but don't have a value
;; for a slot which is blended do not get considered
;; for that slot.  However the probabilities are
;; only calculated once over all the chunks which
;; matched the request.  That means that if a blended
;; slot value is computed via the weighted average
;; it's possible that the weights will not total 1.0
;; because of missing values.
;;
;; To be safe, one probably wants to ensure that 
;; only chunks which have values for all of the
;; important slots match the request.


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
CG-USER(38): (run 10)
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
Matched chunks' slots contain: (LARGE X-LARGE TINY)
Magnitude values for those items: (4 5 1)
With numeric magnitudes blending by weighted average
 Chunk A with probability 0.004971579 times magnitude 4.0 = 0.019886317 cumulative result: 0.019886317
 Chunk B with probability 0.1616471 times magnitude 5.0 = 0.80823547 cumulative result: 0.8281218
 Chunk C with probability 0.8333813 times magnitude 1.0 = 0.8333813 cumulative result: 1.6615031
 Final result: 1.6615031  Converted to value: SMALL
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


Slots to be blended: (SIZE)
Finding blended value for slot: SIZE
Matched chunks' slots contain: (SMALL)
Magnitude values for those items: (2)
With numeric magnitudes blending by weighted average
 Chunk E with probability 0.90319115 times magnitude 2.0 = 1.8063823 cumulative result: 1.8063823
 Final result: 1.8063823  Converted to value: SMALL
This is the definition of the blended chunk:
(KEY KEY-2 SIZE SMALL)

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
