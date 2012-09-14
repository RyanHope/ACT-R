(clear-all)
(define-model test-demo-module
    
    (sgp :esc t :create-delay .15 :trace-detail high)
    
    (p p1
       ?create>
       state free
       buffer empty
       ==>
       +create>
       isa visual-location
       screen-x 10
       screen-y 20)
  
  (p p2
     =create>
     isa visual-location
     ==>
     +output>
     isa demo-output
     value =create
     ))