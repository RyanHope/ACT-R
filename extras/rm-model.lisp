(defmacro defp (&rest body)
  `(p-fct ',body))

(clear-all)

(define-model rm-test)

(defp test1
 ?testbuf1> state free buffer empty
 ==>
 +testbuf1> color "red" num 10
 )
