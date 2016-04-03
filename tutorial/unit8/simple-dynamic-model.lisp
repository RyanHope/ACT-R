(clear-all)

(define-model dynamic-introduction
    
    (sgp :v t :trace-detail high :cst t :style-warnings nil)
  
  (chunk-type fact context data)
  (chunk-type step step destination)
  (chunk-type result data1 data2)
  
  (add-dm (first isa chunk) (second isa chunk)
          (destination isa chunk) (data2 isa chunk)
          (data3 isa chunk) (data isa chunk)
          (a isa step step first destination data2)
          (b isa step step second destination data3))
  
  (goal-focus-fct (car (define-chunks (isa fact context data data 10))))
  
  (p start
     =goal>
       isa      fact
       context  =context
       =context =x
     ?imaginal>
       state    free
       buffer   empty
    ==>
     =goal>
       context  destination
     +imaginal>
       isa      result
       data1    =x
     +retrieval>
       isa      step
       step     first)
  
  (p retrieve-first-step
     =goal>
       isa     fact
       context =slot
       data    =x
     =imaginal>
       isa     result
       data1   =x
     =retrieval>
       isa     step
       =slot   =target
       step    first
    ==>
     =goal>
       data    11
     =imaginal>
       =target =x
     +retrieval>
       isa     step
       step    second)
  
  (p retrieve-second-step
     =goal>
       isa     fact
       context =slot
       data    =x
     =imaginal>
     =retrieval>
       isa     step
       =slot   =target
       step    second
    ==>
     =imaginal>
       =target =x))
