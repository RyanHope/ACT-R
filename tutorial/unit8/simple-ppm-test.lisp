(clear-all)

(define-model test-ppm
  (sgp :v t :trace-detail high :esc t :ppm nil :egs 0 :cst t)
    
  (add-dm (small isa chunk)
          (medium isa chunk)
          (large isa chunk))
  
  (chunk-type test value)
  
  (set-similarities (small medium -.5) (medium large -.5))
  
  (set-buffer-chunk 'goal (car (define-chunks (isa test value small))))
  
  (p small
     =goal>
       isa test
       value small
       value =val
     ==>
     !output! =val
     -goal>)
  
  (p medium
     =goal>
       isa test
       value medium
       value =val
     ==>
     !output! =val
     -goal>)
  
  (p large
     =goal>
       isa test
       value large
       value =val
     ==>
     !output! =val
     -goal>))
