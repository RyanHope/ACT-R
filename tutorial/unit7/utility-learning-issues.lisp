(clear-all)

(defun present-choose ()
  (goal-focus initial-goal)
  (schedule-event-relative 5 'show-result :params (list (if (< (act-r-random 1.0) .6) 'a (if (> (act-r-random 1.0) .5) 'b nil))) :output 'medium))

(defun show-result (choice)
  (mod-chunk-fct 'response (list 'answer choice))
  (set-buffer-chunk 'imaginal 'response)
  (schedule-event-relative 2 'present-choose :output 'medium))


(define-model utility-learning-test
    
  (sgp :seed (100 2))
  (sgp :esc t :ul t :egs .5)
  
  (chunk-type choose choice)
  (chunk-type response answer)
         
  (define-chunks (initial-goal isa choose)
      (response isa response)
      (a isa chunk)
      (b isa chunk))
         
  (p choose-a
     =goal>
       isa choose
       choice nil
   ==>
     =goal>
       choice a)
  
  (p choose-b
     =goal>
       isa choose
       choice nil
   ==>
     =goal>
       choice b)
  
  (p response-matches
     =goal>
       isa choose
       choice =choice
     =imaginal>
       isa response
       answer =choice
   ==>
     -goal>)
  
  (p response-doesnt-match
     =goal>
       isa choose
       choice =choice
     =imaginal>
       isa response
       - answer =choice
       - answer nil
   ==>
     -goal>)
  
  (p unknown-response
     =goal>
       isa choose
       choice =choice
     =imaginal>
       isa response
       answer nil
   ==>
     -goal>)
  
  (spp response-matches :reward 4)
  (spp response-doesnt-match :reward 0)
         
  (schedule-event 0 'present-choose))
