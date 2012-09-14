(defstruct demo-module delay esc busy)

(defun create-demo-module (model-name)
  (declare (ignore model-name))
  (make-demo-module))

(defun reset-demo-module (demo)
  (declare (ignore demo))
  (chunk-type demo-output value))

(defun delete-demo-module (demo)
  (declare (ignore demo)))

(defun demo-module-params (demo param)
  (if (consp param)
    (case (car param)
      (:create-delay
       (setf (demo-module-delay demo) (cdr param)))  
      (:esc
       (setf (demo-module-esc demo) (cdr param))))
    (case param
      (:create-delay
       (demo-module-delay demo)))))

(defun demo-module-requests (demo buffer spec)
  (if (eq buffer 'create)
     (demo-create-chunk demo spec)
    (demo-handle-print-out spec)))


(defun demo-handle-print-out (spec)
  (let* ((type (chunk-spec-chunk-type spec))
         (value? (slot-in-chunk-spec-p spec 'value))
         (v1 (when value? (chunk-spec-slot-spec spec 'value))))
     (if (eq type 'demo-output)
         (if value?
             (if (= (length v1) 1)
                 (if (eq (caar v1) '=)
                     (model-output "Value: ~s" (caddar v1))
                   (model-warning "Invalid slot modifier ~s in output buffer request" (caar v1)))
               (model-warning "Value slot specified multiple times in output buffer request"))
           (model-warning "Value slot missing in output buffer request"))
       (model-warning "bad chunk-type in request to output buffer"))))

(defun demo-create-chunk (demo spec)
   (if (demo-module-busy demo)
      (model-warning "Cannot handle request when busy")
     (let* ((chunk-def (chunk-spec-to-chunk-def spec))
            (chunk (when chunk-def
                    (car (define-chunks-fct (list chunk-def))))))
        (when chunk
          (let ((delay (if (demo-module-esc demo) 
                           (demo-module-delay demo)
                         0)))
            (setf (demo-module-busy demo) t)
            (schedule-set-buffer-chunk 'create chunk delay :module 'demo)
            (schedule-event-relative delay 'free-demo-module :params (list demo) :module 'demo))))))

(defun free-demo-module (demo)
  (setf (demo-module-busy demo) nil))


(defun demo-module-queries (demo buffer query value)
  (declare (ignore buffer))
  (case query
    (state
     (case value
       (busy (demo-module-busy demo))
       (free (not (demo-module-busy demo)))
       (error nil)
       (t (print-warning "Bad state query to the ~s buffer" buffer))))
    (t (print-warning "Invalid query ~s to the ~s buffer" query buffer))))





(define-module-fct 'demo
                   '(create output) 
  (list (define-parameter :create-delay 
               :documentation 
                  "time to create the chunk for the demo module"
               :default-value .1
               :valid-test (lambda (x) (and (numberp x) (>= x 0)))
               :warning "Non-negative number"
               :owner t)
        (define-parameter :esc :owner nil))

   :request 'demo-module-requests
   :query 'demo-module-queries

   :version "1.0a1"
   :documentation "Demo module for ICCM tutorial"
   :creation 'create-demo-module
   :reset 'reset-demo-module 
   :delete 'delete-demo-module
   :params 'demo-module-params
)


#|
(trace demo-module-requests demo-module-queries create-demo-module reset-demo-module delete-demo-module demo-module-params)


; Loading C:\Documents and Settings\Root\Desktop\demo-model.lisp
 0[4]: (CREATE-DEMO-MODULE TEST-DEMO-MODULE)
 0[4]: returned #S(DEMO-MODULE :DELAY NIL :BUSY NIL :ESC NIL)
 0[4]: (RESET-DEMO-MODULE #S(DEMO-MODULE :DELAY NIL :BUSY NIL :ESC NIL))
 0[4]: returned DEMO-OUTPUT
 0[4]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY NIL :BUSY NIL :ESC NIL) (:CREATE-DELAY . 0.1))
 0[4]: returned 0.1
 0[4]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY 0.1 :BUSY NIL :ESC NIL) (:ESC))
 0[4]: returned NIL
 0[4]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY 0.1 :BUSY NIL :ESC NIL) (:ESC . T))
 0[4]: returned T
 0[4]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY 0.1 :BUSY NIL :ESC T) (:CREATE-DELAY . 0.15))
 0[4]: returned 0.15

CG-USER(27): (run .25)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
 0[4]: (DEMO-MODULE-QUERIES #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) CREATE STATE FREE)
 0[4]: returned T
     0.000   PROCEDURAL             PRODUCTION-SELECTED P1 
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION CREATE 
     0.050   PROCEDURAL             PRODUCTION-FIRED P1 
     0.050   PROCEDURAL             MODULE-REQUEST CREATE 
 0[4]: (DEMO-MODULE-REQUESTS #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) CREATE
                             #S(ACT-R-CHUNK-SPEC :TYPE #S(ACT-R-CHUNK-TYPE :NAME VISUAL-LOCATION
                                                                           :DOCUMENTATION NIL
                                                                           :SUPERTYPES (VISUAL-LOCATION)
                                                                           :SUBTYPES (CHAR-PRIMITIVE VISUAL-LOCATION)
                                                                           :SLOTS (SCREEN-X SCREEN-Y DISTANCE KIND COLOR VALUE HEIGHT WIDTH SIZE)
                                                                           :EXTENDED-SLOTS NIL)
                                                 :SLOTS (#S(ACT-R-SLOT-SPEC :MODIFIER = :NAME SCREEN-X :VALUE 10)
                                                         #S(ACT-R-SLOT-SPEC :MODIFIER = :NAME SCREEN-Y :VALUE 20))))
 0[4]: returned
         #S(ACT-R-EVENT :TIME 0.2
                        :PRIORITY 0
                        :ACTION FREE-DEMO-MODULE
                        :MODEL TEST-DEMO-MODULE
                        :MP DEFAULT
                        :MODULE DEMO
                        :DESTINATION NIL
                        :PARAMS (#S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY T))
                        :DETAILS NIL
                        :OUTPUT T
                        :WAIT-CONDITION NIL)
     0.050   PROCEDURAL             CLEAR-BUFFER CREATE 
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
 0[4]: (DEMO-MODULE-QUERIES #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY T) CREATE STATE FREE)
 0[4]: returned NIL
     0.200   DEMO                   SET-BUFFER-CHUNK CREATE VISUAL-LOCATION0 
     0.200   DEMO                   FREE-DEMO-MODULE #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY T) 
     0.200   PROCEDURAL             CONFLICT-RESOLUTION 
     0.200   PROCEDURAL             PRODUCTION-SELECTED P2 
     0.200   PROCEDURAL             BUFFER-READ-ACTION CREATE 
     0.250   PROCEDURAL             PRODUCTION-FIRED P2 
     0.250   PROCEDURAL             MODULE-REQUEST OUTPUT 
 0[4]: (DEMO-MODULE-REQUESTS #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) OUTPUT
                             #S(ACT-R-CHUNK-SPEC :TYPE #S(ACT-R-CHUNK-TYPE :NAME DEMO-OUTPUT
                                                                           :DOCUMENTATION NIL
                                                                           :SUPERTYPES (DEMO-OUTPUT)
                                                                           :SUBTYPES (DEMO-OUTPUT)
                                                                           :SLOTS (VALUE)
                                                                           :EXTENDED-SLOTS NIL)
                                                 :SLOTS (#S(ACT-R-SLOT-SPEC :MODIFIER = :NAME VALUE :VALUE VISUAL-LOCATION0-0))))
Value: VISUAL-LOCATION0-0
 0[4]: returned NIL
     0.250   PROCEDURAL             CLEAR-BUFFER CREATE 
     0.250   PROCEDURAL             CLEAR-BUFFER OUTPUT 
     0.250   PROCEDURAL             CONFLICT-RESOLUTION 
 0[4]: (DEMO-MODULE-QUERIES #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) CREATE STATE FREE)
 0[4]: returned T
     0.250   PROCEDURAL             PRODUCTION-SELECTED P1 
     0.250   PROCEDURAL             QUERY-BUFFER-ACTION CREATE 
     0.250   ------                 Stopped because time limit reached 
0.25
22
NIL
CG-USER(31): (clear-all)
 0[4]: (DELETE-DEMO-MODULE #S(DEMO-MODULE :DELAY 0.15 :BUSY NIL :ESC T))
 0[4]: returned NIL
NIL

|#