;;; This is all the models of unit1 defined simultaneously
;;; but each within its own meta-process which means
;;; a separate run call will be necessary for each.
;;;
;;; That is:
;;; (with-meta-process default (run 10)) 
;;; will run count because it is in the default meta-process.
;;;
;;; (with-meta-process semantic (run 10)) 
;;; will run the semantic model.
;;;
;;; (with-meta-process addition (run 10))
;;; will run the addition model.


(clear-all)

;; count will be in the default meta-process

(define-model count

(sgp :esc t :lf .05 :trace-detail high)


(chunk-type count-order first second)
(chunk-type count-from start end count)

(add-dm
 (b ISA count-order first 1 second 2)
 (c ISA count-order first 2 second 3)
 (d ISA count-order first 3 second 4)
 (e ISA count-order first 4 second 5)
 (f ISA count-order first 5 second 6)
 (first-goal ISA count-from start 2 end 4)
 )

(p start
   =goal>
      ISA         count-from
      start       =num1
      count       nil
 ==>
   =goal>
      count       =num1
   +retrieval>
      ISA         count-order
      first       =num1
)

(P increment
   =goal>
      ISA         count-from
      count       =num1
    - end         =num1
   =retrieval>
      ISA         count-order
      first       =num1
      second      =num2
 ==>
   =goal>
      count       =num2
   +retrieval>
      ISA         count-order
      first       =num2
   !output!       (=num1)
)

(P stop
   =goal>
      ISA         count-from
      count       =num
      end         =num
 ==>
   -goal>
   !output!       (=num)

)

(goal-focus first-goal)
)


(define-meta-process semantic)

(with-meta-process semantic

(define-model semantic

(sgp :esc t :lf .05)

(chunk-type property object attribute value)
(chunk-type is-member object category judgment)

(add-dm
 (shark isa chunk) (dangerous isa chunk)
 (locomotion isa chunk) (swimming isa chunk) 
 (fish isa chunk) (salmon isa chunk)
 (edible isa chunk) (breathe isa chunk)
 (gills isa chunk) (animal isa chunk)
 (moves isa chunk) (skin isa chunk)
 (canary isa chunk) (color isa chunk)
 (sings isa chunk) (bird isa chunk) 
 (ostrich isa chunk) (flies isa chunk) 
 (height isa chunk) (tall isa chunk)
 (wings isa chunk) (flying isa chunk) 
 (true isa chunk) (false isa chunk) 
 (p1 ISA property object shark attribute dangerous value true)
 (p2 ISA property object shark attribute locomotion value swimming)
 (p3 ISA property object shark attribute category value fish)
 (p4 ISA property object salmon attribute edible value true)
 (p5 ISA property object salmon attribute locomotion value swimming)
 (p6 ISA property object salmon attribute category value fish)
 (p7 ISA property object fish attribute breathe value gills)
 (p8 ISA property object fish attribute locomotion value swimming)
 (p9 ISA property object fish attribute category value animal)
 (p10 ISA property object animal attribute moves value true)
 (p11 ISA property object animal attribute skin value true)
 (p12 ISA property object canary attribute color value yellow)
 (p13 ISA property object canary attribute sings value true)
 (p14 ISA property object canary attribute category value bird)
 (p15 ISA property object ostrich attribute flies value false)
 (p16 ISA property object ostrich attribute height value tall)
 (p17 ISA property object ostrich attribute category value bird)
 (p18 ISA property object bird attribute wings value true)
 (p19 ISA property object bird attribute locomotion value flying)
 (p20 ISA property object bird attribute category value animal)
 (g1 ISA is-member object canary category bird judgment nil)
 (g2 ISA is-member object canary category animal judgment nil)
 (g3 ISA is-member object canary category fish judgment nil))

(p initial-retrieve
   =goal>
      ISA         is-member
      object      =obj
      category    =cat
      judgment    nil
==>
   =goal>
      judgment    pending
   +retrieval>  
      ISA         property
      object      =obj
      attribute   category
)


(P direct-verify
   =goal>
      ISA         is-member
      object      =obj
      category    =cat
      judgment    pending
   =retrieval>
      ISA         property
      object      =obj
      attribute   category
      value       =cat
==>
   =goal>
      judgment    yes
)

(P chain-category
   =goal>
      ISA         is-member
      object      =obj1
      category    =cat
      judgment    pending
   =retrieval>
      ISA         property
      object      =obj1
      attribute   category
      value       =obj2
    - value       =cat
==>
   =goal>
      object      =obj2
   +retrieval>  
      ISA         property
      object      =obj2
      attribute   category
)

 (P fail
   =goal>
      ISA         is-member
      object      =obj1
      category    =cat  
      judgment    pending
    
    ?retrieval>
      state       error
==>
   =goal>
      judgment    no
)


(goal-focus g1)
)
)

(define-meta-process addition)

(with-meta-process addition
  
(define-model addition

(sgp :esc t :lf .05)

(chunk-type count-order first second)
(chunk-type add arg1 arg2 sum count)

(add-dm
   (a ISA count-order first 0 second 1)
   (b ISA count-order first 1 second 2)
   (c ISA count-order first 2 second 3)
   (d ISA count-order first 3 second 4)
   (e ISA count-order first 4 second 5)
   (f ISA count-order first 5 second 6)
   (g ISA count-order first 6 second 7)
   (h ISA count-order first 7 second 8)
   (i ISA count-order first 8 second 9)
   (j ISA count-order first 9 second 10)
   (second-goal ISA add arg1 5 arg2 2))

(P initialize-addition
   =goal>
      ISA         add
      arg1        =num1
      arg2        =num2
      sum         nil
==>
   =goal>
      sum         =num1
      count       0
   +retrieval>
      isa        count-order
      first      =num1
)

(P terminate-addition
   =goal>
      ISA         add
      count       =num
      arg2        =num
      sum         =answer
==>
   =goal>
      count nil
)

(P increment-sum
   =goal>
      ISA         add
      sum         =sum
      count       =count
    - arg2        =count
   =retrieval>
      ISA         count-order
      first       =sum
      second      =newsum
==>
   =goal>
      sum         =newsum
   +retrieval>
      isa        count-order
      first      =count
)

(P increment-count
   =goal>
      ISA         add
      sum         =sum
      count       =count
   =retrieval>
      ISA         count-order
      first       =count
      second      =newcount
==>
   =goal>
      count       =newcount
   +retrieval>
      isa        count-order
      first      =sum
)


(goal-focus second-goal)
))

