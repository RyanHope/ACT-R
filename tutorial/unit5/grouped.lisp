(defvar *response* nil)

(defun run-grouped-recall ()
  (setf *response* nil)
  (reset)
  (run 20)
  (reverse *response*))

(defun record-response (value)
    (push value *response*))

(clear-all)
(define-model grouped

(sgp  :rt -.5 :ans .15 :esc t :mp 1.0 :v t :act nil
     :declarative-num-finsts 15
     :declarative-finst-span 20
     :trace-detail low)

(sgp :seed (77093543977 39813))

(chunk-type recall-list  group element list group-position)
(chunk-type group parent position id)
(chunk-type item name parent position)
(chunk-type list)

(add-dm
   (list ISA list)
   (first isa chunk) (second isa chunk)
   (third isa chunk) (fourth isa chunk)
   (group1 ISA group parent list position first id group1)
   (group2 ISA group parent list position second id group2)
   (group3 ISA group parent list position third id group3)
   (item1 ISA item name "1" parent group1 position first)
   (item2 ISA item name "2" parent group1 position second)
   (item3 ISA item name "3" parent group1 position third)
   (item4 ISA item name "4" parent group2 position first)
   (item5 ISA item name "5" parent group2 position second)
   (item6 ISA item name "6" parent group2 position third)
   (item7 ISA item name "7" parent group3 position first)
   (item8 ISA item name "8" parent group3 position second)
   (item9 ISA item name "9" parent group3 position third)
   (goal ISA recall-list list list))



(p recall-first-group
   =goal>
      isa      recall-list
      list     =list
   ?retrieval>
      buffer    empty
      state     free
    - state     error
==>
   =goal>
      group-position first
   +retrieval>
      isa      group
      parent   =list
      position first
)

(p start-recall-of-group
   =goal>
      isa      recall-list
   =retrieval>
      isa      group
      id       =group
==>    
   =goal>
      group    =group 
      element  first
   +retrieval>
      isa      item
      :recently-retrieved nil
      parent   =group
      position first
)

(p harvest-first-item
   =goal>
      isa      recall-list
      element  first
      group    =group
   =retrieval>
      isa      item
      name     =name
==>
   =goal>
      element  second
   +retrieval>
      isa      item
      parent   =group
      position second 
      :recently-retrieved nil
   !eval! (record-response =name)
)

(p harvest-second-item
   =goal>
      isa      recall-list
      element  second
      group    =group
   =retrieval>
      isa      item
      name     =name
==>
   =goal>
      element  third
   +retrieval>
      isa      item
      parent   =group
      position third
      :recently-retrieved nil
   !eval! (record-response =name)
)

(p harvest-third-item
   =goal>
      isa      recall-list
      element  third
      group    =group
   =retrieval>
      isa      item
      name     =name
==>
   =goal>
      element  fourth
   +retrieval>
      isa      item
      parent   =group
      position fourth
      :recently-retrieved nil
   !eval! (record-response =name)
)

(p second-group
   =goal>
      isa      recall-list
      group-position first
      list     =list
   
   ?retrieval>
       state    error
==>
   =goal>
      group-position second
   +retrieval>
      isa      group
      parent   =list
      position second
)

(p third-group
   =goal>
      isa      recall-list
      group-position second
      list     =list
   
   ?retrieval>
       state    error
   ==>
   =goal>
      group-position third
   +retrieval>
      isa      group
      parent   =list
      position third
)


(goal-focus goal)

(set-similarities 
 (first second -0.5)
 (second third -0.5)
 (first third -1))
)
