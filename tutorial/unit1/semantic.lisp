(clear-all)

(define-model semantic

(sgp :esc t :lf .05)

(chunk-type property object attribute value)
(chunk-type is-member object category judgment)

(add-dm
 (shark) (dangerous) (locomotion) (swimming) (fish) 
 (salmon) (edible) (breathe) (gills) (animal)
 (moves) (skin) (canary) (color) (sings) (bird) 
 (ostrich) (flies) (height) (tall) (wings) (flying) 
 (true) (false) 
 
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
 
 (g1 ISA is-member object canary category bird)
 (g2 ISA is-member object canary category animal)
 (g3 ISA is-member object canary category fish))

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
      buffer      failure
==>
   =goal>
      judgment    no
)


(goal-focus g1)
)
