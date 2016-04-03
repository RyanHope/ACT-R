
(clear-all)

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
      ISA         add
      sum         =num1
      count       0
   +retrieval>
      ISA        count-order
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
      ISA         add
      count       nil
   !output!       =answer
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
      ISA         add
      count       =newcount
   +retrieval>
      ISA        count-order
      first      =sum
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
      ISA         add
      sum         =newsum
   +retrieval>
      ISA        count-order
      first      =count
)

(goal-focus second-goal)
)
