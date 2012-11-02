;;; Elio model
;;; 
;;; Copyright 2012 Niels Taatgen
;;;
;;; Model of the Elio (1986) experiment
;;;

;;; To run through the experiment once for each of the conditions, call (do-elio)
;;; This will generate an output file elio.dat
;;; 
;;; To see what happens on a single run, call (test), and then (run 100)
;;;
;;; First code that simulates the experiment

(defstruct elio line start trial responses)

(defvar *etask*)
(defvar *Results*)

;;; Initialization is called init-proc-a, but it is the same for all

(defun init-proc-a ()
  (setf *etask* (make-elio))
  (setf *results* nil)
  (setf (elio-line *etask*) 1)
  (setf (elio-start *etask*) (mp-time))
  (setf (elio-trial *etask*) 1)
  (setf *perception* nil)
  (let 
      ((ins-chunks (no-output (sdm isa instr))))
       (dolist (x ins-chunks)
         (when (or (< (chunk-creation-time x) -100000)  ;; we haven't yet set it   
                   (< (chunk-last-base-level x) -0.5))  ;; it has decayed too much
           (eval `(sdp ,x :creation-time ,(- (mp-time) 20000) :references 40))))) ;;; changed was 10000
  

)

;;; We'll use the same numbers on the screen over and over again. The model will not notice.
;;; Possible actions are:
;;; Enter a number
;;; Look up a number (read). 

(defun proc-pm (action &optional h1 h2)
  (let ((latency .3)  ;; looking up a number
        (disp-values '((solid 6)(algae 2)(lime1 3)(lime2 5)(lime3 1)(lime4 9)(limemax 9)(limemin 1)(toxin1 4)(toxin2 8)(toxin3 7)(toxin4 2)(toxinmin 2)(toxinmax 8))))
    (cond ((eq action 'read)
           (setf *perception* (list h1 (second (assoc h1 disp-values))))
           (when (member h1 '(limemax limemin toxinmax toxinmin)) (setf latency 1.0)) ;; longer latency for finding the min or max
           )
          ((eq action 'enter)
           (setf *perception* nil)
           (push (list (elio-trial *etask*) (- (mp-time)(elio-start *etask*)) (elio-line *etask*) h1) *results*)
;           (print (first *results*))
           (setf (elio-start *etask*) (mp-time))
           (trigger-reward (task-reward *task*))
           (incf (elio-line *etask*))
           ))

    latency)
)

;;; Initialize the model and set up so it can be run manually

(defun test ()
  (set-task 'procedure-a)
  (init-task)
  (setf *verbose* t)
  (sgp :v nil :save-buffer-trace t))



(defun do-elio ()
  (dolist (task '(procedure-b procedure-c procedure-d))
  (reset)
  (spp retrieve-instruction :u 5.0) 
  (sdp :reference-count 3000 :creation-time -1000000)
  (set-task 'procedure-a)
  (init-task)
  (sgp :v nil :save-buffer-trace nil)
  (setf *verbose* nil)
  (dotimes (i 50)
    (run 200.0)
    (clear-buffer 'goal)
    (setf (elio-line *etask*) 1)
    (setf (elio-start *etask*) (mp-time))
    (incf (elio-trial *etask*)))

  (with-open-file (f "~/elio.dat" :direction :output :if-exists :append :if-does-not-exist :create)
  (dolist (x (reverse *results*))
    (format t "~%procedure-a  ~5D ~5D ~7,3F ~5D" (first x)(third x)(second x)(fourth x))
    (format f "procedure-a  ~5D ~5D ~7,3F ~5D~%" (first x)(third x)(second x)(fourth x))))
  (set-task task)
  (init-task)
  (sgp :v nil :save-buffer-trace nil)
  (dotimes (i 50)
    (run 100.0)
    (clear-buffer 'goal)
    (setf (elio-line *etask*) 1)
    (setf (elio-start *etask*) (mp-time))
    (incf (elio-trial *etask*)))
  (with-open-file (f "~/elio.dat" :direction :output :if-exists :append :if-does-not-exist :create)
  (dolist (x (reverse *results*))
    (format t "~%~A  ~5D ~5D ~7,3F ~5D" task (first x)(third x)(second x)(fourth x))
    (format f "~A  ~5D ~5D ~7,3F ~5D~%" task (first x)(third x)(second x)(fourth x))))))
  

(setf *vertices* nil *edges* nil)

(defun run-experiment (n)
  (dotimes (i n) (do-elio)))

(defun run-sample (&optional x)
  (case x
    (1 (print "Running Procedure A") (test)(run 200))
    (otherwise (print "1 - Procedure A"))))

(clear-all)
(define-model-transfer

;;; These are all the facts we need for the particular screen that the model solves over and over again

(add-dm
 (isa fact slot1 subtract slot2 9 slot3 5 slot4 4)
 (isa fact slot1 mult slot2 6 slot3 4 slot4 24)
 (isa fact slot1 div slot2 2 slot3 2 slot4 1)
 (isa fact slot1 div slot2 6 slot3 3 slot4 2)
 (isa fact slot1 greater-of slot2 1 slot3 2 slot4 2)
 (isa fact slot1 add slot2 24 slot3 2 slot4 26)
 (isa fact slot1 add slot2 8 slot3 2 slot4 10)
 (isa fact slot1 div slot2 10 slot3 2 slot4 5)
 (isa fact slot1 div slot2 26 slot3 5 slot4 5)
 (isa fact slot1 subtract slot2 5 slot3 2 slot4 3)
 (isa fact slot1 div slot2 24 slot3 2 slot4 12)
 (isa fact slot1 mult slot2 5 slot3 12 slot4 60)
 (isa fact slot1 add slot2 60 slot3 12 slot4 72)
 (isa fact slot1 mult slot2 1 slot3 3 slot4 3)
 (isa fact slot1 add slot2 3 slot3 2 slot4 5)
 (isa fact slot1 add slot2 6 slot3 3 slot4 9)
 (isa fact slot1 add slot2 2 slot3 7 slot4 9)
 (isa fact slot1 greater-than slot2 9 slot3 9 slot4 false)
 (isa fact slot1 add slot2 5 slot3 9 slot4 14)
 (isa fact slot1 div slot2 6 slot3 3 slot4 2)
 (isa fact slot1 div slot2 14 slot3 2 slot4 7)
 (isa fact slot1 subtract slot2 7 slot3 9 slot4 -2)
 (isa fact slot1 div slot2 9 slot3 2 slot4 4)
 (isa fact slot1 mult slot2 5 slot3 4 slot4 20)
 (isa fact slot1 add slot2 20 slot3 4 slot4 24)
)

(add-instr solid-lime-diff :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
;;; Other stuff is inherited from main goal
(ins :condition (Vlabel = nil) :action ( (read lime4) -> AC ) :description "Entering solid-lime-diff, reading lime4")
(ins :condition (Vlabel = lime4) :action (Vvalue -> WMinter (read lime2) -> AC) :description "Reading lime2")
(ins :condition (RT1 = nil   Vlabel = lime2) :action ((subtract WMinter Vvalue) -> RT (read solid) -> AC) :description "Calculating the difference and reading Solid")
(ins :condition (RTtype = subtract  Vlabel = solid ) :action ((mult Vvalue RTans) -> RT) :description "Multiplying the result by Solid")
(ins :condition (RTtype = mult) :action (RTans -> WMinter sld-finish -> Gcontrol WMparent -> Gtask) :description "Finishing solid-lime-diff"))

(add-instr greater-algae :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (Vlabel = nil) :action ((read algae) -> AC) :description "Entering greater-algae, reading Algae")
(ins :condition (RT1 = nil Vlabel = algae) :action ((div Vvalue 2) -> RT) :description "Dividing algae by 2")
(ins :condition (RTans <> nil Vlabel = algae) :action (RTans -> WMinter (read solid) -> AC) :description "Reading solid")
(ins :condition (RT1 = nil Vlabel = solid) :action ((div Vvalue 3) -> RT) :description "Dividing solid by 3")
(ins :condition (RTtype = div Vlabel = solid) :action ((greater-of WMinter RTans) -> RT) :description "Determining the greater of the two")
(ins :condition (RTtype = greater-of) :action (RTans -> WMinter ga-finish -> Gcontrol WMparent -> Gtask) :description "Finishing up greater-algae"))

(add-instr part-plus-mineral :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (RT1 = nil) :action ( (WMtop) -> RT) :description "Enter part-plus-mineral, retrieving top DM item")
(ins :condition (RTitem1 = particulate) :action ((add RTitem2 WMinter) -> RT (top mineral WMinter) -> EP EPself -> WMtop EPself -> WMcurrent) :description "Add the retrieved particulate to stored Mineral")
(ins :condition (RTtype = add) :action (RTans -> WMinter ppm-finish -> Gcontrol WMparent -> Gtask) :description "Finishing up part-plus-mineral"))

(add-instr mean-toxin :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (Vlabel = nil) :action ((read toxinmax) -> AC) :description "Entering mean-toxin, determining toxinmax")
(ins :condition (Vlabel = toxinmax) :action (Vvalue -> WMinter (read toxinmin) -> AC) :description "Determining toxinmin")
(ins :condition (RT1 = nil Vlabel = toxinmin) :action ((add WMinter Vvalue) -> RT) :description "Subtract them from each other")
(ins :condition (RTtype = add) :action ((div RTans 2) -> RT) :description "Divide the result by 2")
(ins :condition (RTtype = div) :action (RTans -> WMinter tox-finish -> Gcontrol WMparent -> Gtask) :description "Finishing up Mean Toxin"))

(add-instr index1-div-marine :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (RT1 = nil) :action ( (WMtop) -> RT) :description "Entering Index1-div-marine, retrieving top DM item")
(ins :condition (RTitem1 = mineral) :action (RTself -> RTprev (top RTitem1 RTitem2) -> EP EPself -> WMtop EPself -> WMcurrent) :description "If retrieved item is Mineral, put it back as top of the stack and retrieve next")
(ins :condition (RTitem1 = index1) :action ((div RTitem2 WMinter) -> RT) :description "If it is index1 then divide it by marine which is still in WM")
(ins :condition (RTtype = div) :action (RTans -> WMinter idm-finish -> Gcontrol WMparent -> Gtask) :description "Finishing up index1-div-marine"))

(add-instr index2-min-mineral :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (RT1 = nil) :action ( (WMtop) -> RT) :description "Entering Index2-min-mineral, retrieving top DM item")
(ins :condition (RTitem1 = mineral) :action ((subtract WMinter RTitem2) -> RT) :description "Subtract stored Index2 from the retrieved mineral")
(ins :condition (RTtype = subtract) :action (RTans -> WMinter  imm-finish -> Gcontrol WMparent -> Gtask) :description "Finishing up index2-min-mineral"))

(add-instr procedure-a :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
:pm-function proc-pm
:init init-proc-a
:reward 20.0
:parameters ((sgp :lf 0.15 :egs 0.3 :ans 0.1 :rt -2 :state-activation 0.0 :mas nil :alpha 0.2)(setf *condition-spread* 0.0 *condition-penalty* -0.4)(setf *verbose* nil))  

(ins :condition (Gcontrol = nil) :action (Gtask -> WMparent solid-lime-diff -> Gtask) :description "Starting step 1 in procedure A")
(ins :condition (Gcontrol = sld-finish) :action ((enter WMinter) -> AC (top particulate WMinter) -> EP EPself -> WMtop EPself -> WMcurrent Gtask -> WMparent greater-algae -> Gtask) :description "Starting step 2 in procedure A")
(ins :condition (Gcontrol = ga-finish) :action ((enter WMinter) -> AC Gtask -> WMparent part-plus-mineral -> Gtask) :description "Starting step 3 in procedure A")
(ins :condition (Gcontrol = ppm-finish) :action ((enter WMinter) -> AC (WMcurrent index1 WMinter) -> EP EPself -> WMcurrent Gtask -> WMparent mean-toxin -> Gtask) :description "Starting step 4 in procedure A")
(ins :condition (Gcontrol = tox-finish) :action ((enter WMinter) -> AC Gtask -> WMparent index1-div-marine -> Gtask) :description "Starting step 5 in procedure A")
(ins :condition (Gcontrol = idm-finish) :action ((enter WMinter) -> AC Gtask -> WMparent index2-min-mineral -> Gtask) :description "Starting step 6 in procedure A")
(ins :condition (Gcontrol = imm-finish) :action ((enter WMinter) -> AC finish -> Gtask) :description "Entering last result and finishing procedure A")
)

(add-instr mineral-div-marine :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (RT1 = nil) :action ( (WMtop) -> RT))
(ins :condition (RTitem1 = particulate) :action ((? RTself mineral) -> RT (top RTitem1 RTitem2) -> EP EPself -> WMtop EPself -> WMcurrent))  ;; need particulate later
(ins :condition (RTitem1 = mineral) :action ((div RTitem2 WMinter) -> RT)) 
(ins :condition (RTtype = div) :action (RTans -> WMinter mdm-finish -> Gcontrol WMparent -> Gtask)))

(add-instr part-mult-index1 :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (RT1 = nil) :action ( (WMtop) -> RT))
(ins :condition (RTitem1 = particulate) :action ((mult RTitem2 WMinter) -> RT (top index1 WMinter) -> EP EPself -> WMtop EPself -> WMcurrent))
(ins :condition (RTtype = mult) :action (RTans -> WMinter  pmi-finish -> Gcontrol WMparent -> Gtask)))

(add-instr index1-plus-index2 :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (RT1 = nil) :action ( (WMtop) -> RT))
(ins :condition (RTitem1 = index1) :action ((add WMinter RTitem2) -> RT))
(ins :condition (RTtype = add) :action (RTans -> WMinter ipi-finish -> Gcontrol WMparent -> Gtask)))

(add-instr procedure-b :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
:pm-function proc-pm
:init init-proc-a
:reward 20.0
:parameters ((sgp :lf 0.15 :egs 0.3 :ans 0.1 :rt -2 :state-activation 0.0 :mas nil :alpha 0.2)(setf *condition-spread* 0.0 *condition-penalty* -0.4)(setf *verbose* nil))  

(ins :condition (Gcontrol = nil) :action (Gtask -> WMparent mean-toxin -> Gtask))
(ins :condition (Gcontrol = tox-finish) :action ((enter WMinter) -> AC (top particulate WMinter) -> EP EPself -> WMtop EPself -> WMcurrent Gtask -> WMparent solid-lime-diff -> Gtask))
(ins :condition (Gcontrol = sld-finish) :action ((enter WMinter) -> AC (WMcurrent mineral WMinter) -> EP EPself -> WMcurrent Gtask -> WMparent greater-algae -> Gtask))
(ins :condition (Gcontrol = ga-finish) :action ((enter WMinter) -> AC Gtask -> WMparent mineral-div-marine -> Gtask))
(ins :condition (Gcontrol = mdm-finish) :action ((enter WMinter) -> AC Gtask -> WMparent part-mult-index1 -> Gtask))
(ins :condition (Gcontrol = pmi-finish) :action ((enter WMinter) -> AC Gtask -> WMparent index1-plus-index2 -> Gtask))
(ins :condition (Gcontrol = ipi-finish) :action ((enter WMinter) -> AC finish -> Gtask))
)

(add-instr triple-lime :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (Vlabel = nil) :action ( (read limemin) -> AC ))
(ins :condition (RT1 = nil Vlabel = limemin ) :action ((mult Vvalue 3) -> RT))
(ins :condition (RTans <> nil Vlabel = limemin) :action (RTans -> WMinter (read algae) -> AC))
(ins :condition (RT1 = nil Vlabel = algae ) :action ((add WMinter Vvalue) -> RT))
(ins :condition (RTtype = add ) :action (RTans -> WMinter trl-finish -> Gcontrol WMparent -> Gtask)))

(add-instr lesser-evil :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (Vlabel = nil) :action ((read solid) -> AC))
(ins :condition (Vlabel = solid) :action (Vvalue -> WMinter (read lime1) -> AC))
(ins :condition (RT1 = nil Vlabel = lime1) :action ((add WMinter Vvalue) -> RT))
(ins :condition (RTtype = add Vlabel = lime1) :action ((WMcurrent intermediate RTans) -> EP EPself -> WMcurrent (read algae) -> AC)) ;; We can only remember one intermediate result, so the second needs to go to DM
(ins :condition (Vlabel = algae) :action (Vvalue -> WMinter (read toxin3) -> AC))
(ins :condition (RT1 = nil Vlabel = toxin3) :action ((add WMinter Vvalue) -> RT))
(ins :condition (RTtype = add Vlabel = toxin3) :action  (RTans -> WMinter (WMcurrent) -> RT))
(ins :condition (RTitem1 = intermediate) :action (RTprev -> WMcurrent (greater-than WMinter RTitem2) -> RT))
(ins :condition (RTtype = greater-than RTans = true) :action (RTarg1 -> WMinter lev-finish -> Gcontrol WMparent -> Gtask))
(ins :condition (RTtype = greater-than RTans = false) :action (RTarg2 -> WMinter lev-finish -> Gcontrol WMparent -> Gtask)))

(add-instr solid-div-lime  :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
(ins :condition (Vlabel = nil)  :action ((read solid) -> AC))
(ins :condition (Vlabel = solid) :action (Vvalue -> WMinter (read lime1) -> AC))
(ins :condition (RT1 = nil Vlabel = lime1) :action ((div WMinter Vvalue) -> RT))
(ins :condition (RTtype = div) :action (RTans -> WMinter sdl-finish -> Gcontrol WMparent -> Gtask)))

(add-instr procedure-c :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
:pm-function proc-pm
:init init-proc-a
:reward 20.0
:parameters ((sgp :lf 0.15 :egs 0.3 :ans 0.1 :rt -2 :state-activation 0.0 :mas nil :alpha 0.2)(setf *condition-spread* 0.0 *condition-penalty* -0.4)(setf *verbose* nil))  
(ins :condition (Gcontrol = nil) :action (Gtask -> WMparent triple-lime -> Gtask))
(ins :condition (Gcontrol = trl-finish) :action ((enter WMinter) -> AC (top particulate WMinter) -> EP EPself -> WMtop EPself -> WMcurrent Gtask -> WMparent lesser-evil -> Gtask))
(ins :condition (Gcontrol = lev-finish) :action ((enter WMinter) -> AC Gtask -> WMparent part-plus-mineral -> Gtask))
(ins :condition (Gcontrol = ppm-finish) :action ((enter WMinter) -> AC (WMcurrent index1 WMinter) -> EP EPself -> WMcurrent Gtask -> WMparent solid-div-lime -> Gtask))
(ins :condition (Gcontrol = sdl-finish) :action ((enter WMinter) -> AC Gtask -> WMparent index1-div-marine -> Gtask))
(ins :condition (Gcontrol = idm-finish) :action ((enter WMinter) -> AC Gtask -> WMparent index2-min-mineral -> Gtask))
(ins :condition (Gcontrol = imm-finish) :action ((enter WMinter) -> AC finish -> Gtask))
)


(add-instr procedure-d :input (Vlabel Vvalue) :variables (WMinter WMtop WMcurrent WMparent) :declarative ((RTtype RTarg1 RTarg2 RTans))
:pm-function proc-pm
:init init-proc-a
:reward 20.0
:parameters ((sgp :lf 0.15 :egs 0.3 :ans 0.1 :rt -2 :state-activation 0.0 :mas nil :alpha 0.2)(setf *condition-spread* 0.0 *condition-penalty* -0.4)(setf *verbose* nil))  
(ins :condition (Gcontrol = nil) :action (Gtask -> WMparent triple-lime -> Gtask))
(ins :condition (Gcontrol = trl-finish) :action ((enter WMinter) -> AC (top particulate WMinter) -> EP EPself -> WMtop EPself -> WMcurrent Gtask -> WMparent lesser-evil -> Gtask))
(ins :condition (Gcontrol = lev-finish) :action ((enter WMinter) -> AC (WMcurrent mineral WMinter) -> EP EPself -> WMcurrent Gtask -> WMparent solid-div-lime -> Gtask))
(ins :condition (Gcontrol = sdl-finish) :action ((enter WMinter) -> AC Gtask -> WMparent mineral-div-marine -> Gtask))
(ins :condition (Gcontrol = mdm-finish) :action ((enter WMinter) -> AC Gtask -> WMparent part-mult-index1 -> Gtask))
(ins :condition (Gcontrol = pmi-finish) :action ((enter WMinter) -> AC Gtask -> WMparent index1-plus-index2 -> Gtask))
(ins :condition (Gcontrol = ipi-finish) :action ((enter WMinter) -> AC finish -> Gtask))
)

)