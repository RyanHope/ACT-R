(clear-all)

(defvar *cat1* '((small  large  medium  small)
                 (medium  small  large  medium)
                 (small  medium  medium  medium)
                 (small  medium  large  medium)
                 (small  medium  large  large)))
  
(defvar *cat2* '((large  small  small  small)
                 (medium  medium  small  large)
                 (large  small  large  small)
                 (large  small  large  large)
                 (large  small  small  large)))

(defparameter *cat-data* '(0.975 0.85 0.987 1.0 0.963 0.075 0.138 0.087 0.05 0.025 0.937 0.544 0.988 0.087))

(defparameter *stims* '((-1.025  0.493  0.048  -0.666)
                        (-0.172  -0.557  0.337  0.163)
                        (-0.98  0.275  -0.005  -0.067)
                        (-0.951  0.259  0.399  0.093)
                        (-0.96  0.198  0.38  0.527)
                        (0.665  -0.441  -0.508  -0.396)
                        (-0.059  0.243  -0.602  0.624)
                        (0.586  -0.511  0.381  -0.507)
                        (0.823  -0.539  0.332  0.633)
                        (0.823  -0.504  -0.487  0.776)
                        (-1.114  -0.52  0.636  -0.028)
                        (-0.154  -0.562  -0.043  0.057)
                        (-0.856  0.197  0.241  0.007)
                        (0.704  -0.287  -0.164  0.178)))


(defun scale-sim (x max)
  (- (/ x max) 1.0))

(defun normal (x sigma2 m)
  (* (/ 1 (sqrt (* 2 pi sigma2))) (exp (- (/ (* (- x m) (- x m)) (* 2 sigma2))))))

(defparameter *sigma2* .15)
(defparameter *size-mappings* (list (cons 'small -.9) (cons 'medium 0) (cons 'large .9)))
(defparameter *max-norm* (normal 0 *sigma2* 0))

(defvar *attribute-offset* 0)
(defvar *slots* '(eh es nl mh))

(defun size-similarities (a b)
  (let ((number (if (numberp b) (- b *attribute-offset*) nil))
        (size (if (find a '(small medium large)) a nil)))
    
    (when (and number size)
      (scale-sim (normal number *sigma2* (cdr (assoc size *size-mappings*))) *max-norm*))))

(defmacro categorize (n &optional offset &rest slots)
  `(categorize-fct ,n ,offset ',slots))

(defun categorize-fct (n offset slots)
  (if (numberp offset)
      (setf *attribute-offset* offset)
    (setf *attribute-offset* 0))
  
  (cond ((null slots)
         (setf *slots* '(EH ES NL MH)))
        ((not (= (length slots) 4))
         (print-warning "Must specify exactly 4 slot names.  Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        ((not (every 'symbolp slots))
         (print-warning "Slot names must be symbols.  Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        ((some (lambda (x) (> (count x slots) 1)) slots)
         (print-warning "Slot names must be unique. Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        ((find 'category slots)
         (print-warning "Examples already have a category slot. Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        (t (setf *slots* slots)))
  
  (let ((results (make-list (length *cat-data*) :initial-element 0))
        (counts (make-list (length *cat-data*) :initial-element 0)))
    (dotimes (i n)
      (let ((data (do-experiment)))
        (setf results (mapcar '+ results (car data)))
        (setf counts (mapcar '+ counts (cdr data)))))
    (setf results (mapcar (lambda (x) (/ x n)) results))
    
    (setf *attribute-offset* 0)
    
    (mean-deviation results *cat-data*)
    (correlation results *cat-data*)
    (format t "P(C=1)~%")
    (format t "        ~{(~4d) ~}~%" counts)
    (format t "data  ~{~7,3f~}~%" *cat-data*)
    (format t "model ~{~7,3f~}~%" results)))

(defun do-experiment ()
  (let ((result nil)
        (counts nil))
    (dolist (stim *stims*)
      (let ((data (run-trial stim)))
        (push (car data) result)
        (push (cdr data) counts)))
    (cons (reverse result) (reverse counts))))

(defun present-one-stimulus (a b c d)
  (setf *attribute-offset* 0)
  (setf *slots* '(EH ES NL MH))
  (let ((result (run-trial (list a b c d))))
    (unless (zerop (cdr result))
      (if (zerop (car result))
          2 1))))

(defun run-trial (stim)
  (reset)
  
  (dolist (x (permute-list (mapcar 'cons *slots* stim)))
    (present-one-attribute-fct (car x) (cdr x)))
  
  (let ((goal (car (define-chunks (isa categorize)))))
    (goal-focus-fct goal)
    (run 20)
    (let ((category (chunk-slot-value-fct (buffer-read 'imaginal) 'category)))
      
      (cond ((not (numberp category))
             (model-output "Model did not provide a category.")
             (cons 0 0))
            ((= 1 category)
             (cons 1 1))
            ((= 2 category)
             (cons 0 1))
            (t 
             (model-output "Model provided invalid category.")
             (cons 0 0))))))

(defun create-example-memories ()
  (dolist (x *slots*)
    (extend-chunk-type-slots 'example x)
    (define-chunks-fct `((,x isa chunk))))
  (dolist (x *cat1*)
    (add-dm-fct `((isa example category 1 ,@(mapcan 'list *slots* x)))))
  (dolist (x *cat2*)
    (add-dm-fct `((isa example category 2 ,@(mapcan 'list *slots* x))))))

(defmacro present-one-attribute (name value)
  `(present-one-attribute-fct ',name ,value))

(defun present-one-attribute-fct (name value)
  (goal-focus-fct (car (define-chunks-fct `((isa attribute name ,name value ,(+ *attribute-offset* value))))))
  (run 20))


(define-model categorize
  (sgp :v nil :act t)
  (sgp :esc t :lf .01 :blc 10 :rt -100 :sim-hook size-similarities :do-not-harvest imaginal)
  (sgp :mp 1 :ppm 1 :egs .25 :ans .25)
  
  
  (chunk-type attribute name value)
  (chunk-type categorize state)
  (chunk-type example category)
  
  (add-dm (small isa chunk)
          (medium isa chunk)
          (large isa chunk)
          (unknown isa chunk))
  
  (create-example-memories)
  
  (set-similarities (small medium -.4)
                    (medium large -.4)
                    (small large -.8))
  
    
  (set-buffer-chunk 'imaginal (car (define-chunks (isa example category unknown))))
  
  
  )
