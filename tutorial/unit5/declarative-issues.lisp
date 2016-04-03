(clear-all)

(define-model declarative-issues
    
  ;; Set an explicit seed value so that the model performs
  ;; the same everytime.
    
  (sgp :seed (100 1))
  
  ;; Set the ACT-R parameters. 
  
  (sgp :esc t :v t :bll .5 :ans .25 :mas 2 :mp 10)
  
  ;; Specify the new chunk-types that the model will use.
  
  (chunk-type simple-value result)
  (chunk-type simple-fact item attribute)
  (chunk-type task-state state)
  (chunk-type number representation)
  (chunk-type math-fact arg1 arg2 result operator)
  (chunk-type context val1 val2 val3 goal)
  
  
  ;; Define chunks for the states used in the goal to avoid the
  ;; warnings for undefined chunks.
  
  (define-chunks (r2 isa chunk) (r3 isa chunk) (r4 isa chunk) 
    (r5 isa chunk) (r6 isa chunk) (r7 isa chunk) (r8 isa chunk))
  
  ;; Create the initial declarative memory chunks.
  
  (add-dm 
   (zero isa number representation "0")
   (one isa number representation "1")
   (two isa number representation "2")
   (three isa number representation "3")
   (add isa chunk)
   (subtract isa chunk)
   (sky isa chunk)
   (grass isa chunk)
   (rose isa chunk)
      
   (1-1 isa math-fact arg1 one arg2 one result zero operator subtract)
   (2-1 isa math-fact arg1 two arg2 one result one operator subtract)
   (3-2 isa math-fact arg1 three arg2 two result one operator subtract)
   (1+1 isa math-fact arg1 one arg2 one result two operator add)
   (1+2 isa math-fact arg1 one arg2 two result three operator add)
   
   (v1 isa simple-value result "true")
   (v2 isa simple-value result "false")
   (v3 isa simple-value result nil)
   
   (f1 isa simple-fact item sky attribute blue)
   (f2 isa simple-fact item rose attribute red)
   (f3 isa simple-fact item grass attribute green)
   
   (g1 isa task-state state r8)
   (old-context isa context val1 one val2 two val3 add goal g1))
  
  
  ;; Set the history of use explicitly for some chunks.
  
  (set-base-levels (v1 1 -1500)
                   (v2 1 -1500)
                   (v3 1 -1500))
  
  ;; Note that we are using all the slots of the context type
  ;; in the imaginal buffer to avoid warnings.
  
  (declare-buffer-usage imaginal context :all)

  ;; The productions which perform the demonstration actions.
  
  (p p1
     ?goal>
       buffer empty
   ==>
     +goal>
       isa task-state
       state r2
     +retrieval>
       isa simple-value
      - result nil)
  
  (p p2
     =goal>
       isa task-state
       state r2
     =retrieval>
   ==>
     =goal>
       state r3
     +retrieval>
       isa simple-value)
  
  (p p3
     =goal>
       isa task-state
       state r3
     =retrieval>
       isa simple-value
   ==>
     =goal>
       state r4
     +retrieval>
       isa simple-fact
       attribute pink)
  
  (p p4
     =goal>
       isa task-state
       state r4
     ?retrieval>
       state free
   ==>
     =goal>
       state r5
     +retrieval>
       isa simple-fact
       attribute black)
  
  (p p5
     =goal>
       isa task-state
       state r5
     ?imaginal>
       state free
     =retrieval>
   ==>
     =goal>
       state r6
     +imaginal>
       isa context 
       val1 one
       val2 two
     +retrieval>
       isa math-fact
       - arg1 nil
       - arg2 nil
       - result nil)
  
  (p p6
     =goal>
       isa task-state
       state r6
     =imaginal>
       isa context
   ==>
     +retrieval>
       isa math-fact
       - arg1 nil
       - arg2 nil
       - result nil
     =goal>
       state r7
     =imaginal>
       val3 add)
  
  (p p7 
     =goal>
       isa task-state
       state r7
     =imaginal>
     =retrieval>
   ==>
     =goal>
       state r8
     =imaginal>
       goal =goal)
  
  (p p8 
     =goal>
       isa task-state
       state r8
   ==>
     +goal>
     -imaginal>)
  )
