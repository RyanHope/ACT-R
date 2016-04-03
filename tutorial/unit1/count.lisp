(clear-all)

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
 (first-goal ISA count-from start 2 end 4))

(goal-focus first-goal)

(p start
   =goal>
      ISA         count-from
      start       =num1
      count       nil
 ==>
   =goal>
      ISA         count-from
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
      ISA         count-from
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
)
