(clear-all)

(define-model semantic

(sgp :esc t :lf .05)

(chunk-type property object category dangerous locomotion edible
            breathe moves skin color sings flies height wings)

(chunk-type test-attribute object original attribute value state answer)

(add-dm
 (shark) (salmon) (fish) (animal) (canary) (ostrich) (bird)
 (dangerous) (locomotion) (category) (edible) (breathe) (moves) 
 (skin) (color) (sings) (flies) (height) (wings)
 (gills) (swimming) (tall) (flying) (true) (false) 
 (yes) (no) (dont-know)
 
 (attr-check) (record) (create) (done) (check-category)
 
 (p1 ISA property object shark dangerous true)
 (p2 ISA property object shark locomotion swimming)
 (p3 ISA property object shark category fish)
 (p4 ISA property object salmon edible true)
 (p5 ISA property object salmon locomotion swimming)
 (p6 ISA property object salmon category fish)
 (p7 ISA property object fish breathe gills)
 (p8 ISA property object fish locomotion swimming)
 (p9 ISA property object fish category animal)
 (p10 ISA property object animal moves true)
 (p11 ISA property object animal skin true)
 (p12 ISA property object canary color yellow)
 (p13 ISA property object canary sings true)
 (p14 ISA property object canary category bird)
 (p15 ISA property object ostrich flies false)
 (p16 ISA property object ostrich height tall)
 (p17 ISA property object ostrich category bird)
 (p18 ISA property object bird wings true)
 (p19 ISA property object bird locomotion flying)
 (p20 ISA property object bird category animal)
 (p21 ISA property object bird flies true)
 
 (g1 ISA test-attribute object canary attribute category value bird)
 (g2 ISA test-attribute object canary attribute wings value true)
 (g3 ISA test-attribute object canary attribute dangerous value true))

(declare-buffer-usage goal test-attribute :all)

(p retrieve-attribute
   =goal>
     ISA         test-attribute
     object      =obj
     attribute   =attr
     value       =val
     state       nil
  ==>
   =goal>
     state       attr-check
   +retrieval>  
     ISA         property
     object      =obj
     - =attr     nil)

(P direct-verify
   =goal>
     ISA         test-attribute
     original    nil
     object      =obj
     attribute   =attr
     value       =val
     state       attr-check
   =retrieval>
     ISA         property
     object      =obj
     =attr       =val
  ==>
   =goal>
     answer      yes
     state       done)

(P indirect-verify
   =goal>
     ISA         test-attribute
     original    =ori
     object      =obj
     attribute   =attr
     value       =val
     state       attr-check
   =retrieval>
     ISA         property
     object      =obj
     =attr       =val
==>
   =goal>
     object      =ori
     answer      yes
     state       record)

(p record-result
   =goal>
     ISA         test-attribute
     object      =obj
     attribute   =attr
     value       =val
     state       record
   ?imaginal>
     state       free
  ==>
   +imaginal>
     object      =obj
     =attr       =val
   =goal>
     answer      yes
     state       create)

(p create
   =goal>
     isa         test-attribute
     state       create
   ?imaginal>
     state       free
     buffer      full
  ==>
   -imaginal>
   =goal>
     state       done)

(P disprove
   =goal>
     ISA         test-attribute
     object      =obj
     attribute   =attr
     value       =val
     state       attr-check
   =retrieval>
     ISA         property
     object      =obj
     - =attr     =val
  ==>
   =goal>
     answer      no
     state       done)

(P missing-attr
   =goal>
     ISA         test-attribute
     object      =obj
     state       attr-check
   ?retrieval>
     buffer      failure
  ==>
   =goal>
     state       check-category
   +retrieval>
     isa         property
     object      =obj
     - category  nil
)

(P chain-first-category
   =goal>
     ISA         test-attribute
     original    nil
     object      =obj
     state       check-category
   =retrieval>
     ISA         property
     object      =obj
     category    =obj2
  ==>
   =goal>
     original    =obj
     object      =obj2
     state       nil)

(P chain-subsequent-category
   =goal>
     ISA         test-attribute
     - original  nil
     object      =obj
     state       check-category
   =retrieval>
     ISA         property
     object      =obj
     category    =obj2
  ==>
   =goal>
     object      =obj2
     state       nil)

 (P failed-to-find-category
   =goal>
     ISA         test-attribute
     state       check-category
    ?retrieval>
     buffer      failure
  ==>
   =goal>
     answer      dont-know
     state       done)

(p respond
   =goal>
     isa         test-attribute
     state       done
     object      =obj
     attribute   =attr
     value       =val
     answer      =ans
  ==>
   !output! (=obj =attr =val answer is =ans)
   -goal>)
)
