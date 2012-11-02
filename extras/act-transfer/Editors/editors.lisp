;;; Editors model
;;; 
;;; Copyright 2012 Niels Taatgen
;;;
;;; Model of the Anderson & Singley (1985) experiment
;;;

;;; First we have a bunch of code that simulates the editors. 

;;; To run the full experiment, call the function (do-sa n) with n the number of simulated subjects
;;; There are also some functions to run individual editors for just a few trials and see the trace
;;; (do-ed 1) runs ED for one page of edits, same for (do-edt 1) and (do-emacs 2)
;;; Warning: running this takes quite some time!
;;; The simulation will create an output file res-ed2.txt
;;; which can be read by an R script to generate graphs

;;; Model code to simulate the text editors ed, edt and emacs
;;; To make things a bit easier, this editor will accept commands from all three editors
;;;
;;; An editing task consists of two main items: a list of edits to be made, and a list of lines in the text.
;;; 

;;; To run this model faster, and depending on the Lisp you run it in, it may be worthwhile to put the Lisp code
;;; in a separate file and compile it.


(defstruct etask edits text line-pos cursor-pos vlist line)

(defstruct report state task time ll mt strt temp)

(defvar *ed-task*)
(defvar *numbers* '(one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty))

(defvar *rep*)
(defvar *report*)
(defvar *task-count* 0)

(defun split-by-one-space (string)
    "Returns a list of substrings of string
     divided by ONE space each.
     Note: Two consecutive spaces will be seen as
     if there were an empty string between them."
     (when string
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j)))
          
(defun determine-v (x)
	"Set V2 to the word pointed at by line-pos and cursor-pos. Set V1 to word. 
	Set V4 to the current line. Only Emacs can use this"
	(let ((word (nth (etask-cursor-pos x) (aref (etask-text x)(etask-line-pos x))))
		  (line (nth (etask-line-pos x) '(one two three four five six seven eight nine ten 
		  eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty))))
	  (setf (etask-vlist x) (list 'word (if (and word (not (Equal word ""))) word 'eol) nil line))))

(defun extend-to (n l)
	(cond ((>= (length l) n) l)
		(t (append (extend-to (1- n) l) '(nil)))))

;;; The edit task uses the same text page, but has a number of different sets of edits to apply to it

(defvar *edit-tasks*
  '(((replace-word "vader" "moeder" one)
         (insert-word "inhoud" "nieuwe" three)
         (delete-word "slome" "" five)
         (replace-line "ebooks en sociale medi" "electronisch boeken en andere vormen van sociale media" eight)
         (delete-line "of the rings trilogie" "" fifteen)
         (insert-line "Oscar of niet Rowling zal er niet om rouwen want de buit is al binnen" "net zo groot als tien jaar geleden" eighteen))
    ((insert-word "pers" "muskieten" two)
     (replace-line "fans mochten een blik op de inhoud werpen onder voorwaarde van strikte geheimhouding" "fans hadden de gelegenheid om alvast een kijkje te nemen" three)
     (replace-word "medi" "media" eight)
     (delete-word "eindelijk" "" fourteen)
     (delete-line "We all know what happened in the end but" "" sixteen)
     (insert-line "kassucces De spanning is daarom groot dit jaar" "succes Het zal Roling waarschijnlijk een worst wezen" seventeen))
    ((replace-line "Geestelijk vader van de tovenaarsleerling JK Rowling lanceert morgen de site pottermorecom" "Wederom is het tijd voor een nieuwe website over harry potter maar deze keer van Rowling zelf" one)
     (insert-word "paar" "klein" two)
     (delete-word "nieuwe" "" five)
     (delete-line  "Op dit moment staat de laatste film in de serie op het punt om in de bioscoop" "" twelve)
     (replace-word "Oscar" "prijs" thirteen)
     (insert-line "kassucces De spanning is daarom groot dit jaar" "And here we have another meaningless line that makes this text een more unreadable" seventeen))))

(defun init-ed-task ()
	(setf *ed-task* (make-etask :line-pos 0 :cursor-pos 0))
        (setf *rep* (make-report :state 'll-noread :strt (mp-time)))
        (setf *report* nil)
	(setf (etask-text *ed-task*)
		(make-array 20 :initial-contents (mapcar #'split-by-one-space 
		(extend-to 20 '("Geestelijk vader van de tovenaarsleerling JK Rowling lanceert morgen de site pottermorecom" ; one
		 "Rowling laat de pers in het duister tasten over de inhoud ervan Alleen een paar van de grootste"  ; 2

		 "fans mochten een blik op de inhoud werpen onder voorwaarde van strikte geheimhouding" ;3
	     "Ondertussen wordt er gespeculeerd over de inhoud van de website"  ; 4
	     "nu is alleen nog een slome voorpagina te zien Fans snakken naar nieuwe avonturen" ;5
	     "van het ongekend populaire personage Maar volgens een woordvoerder zal op de" ;6
	     "site geen nieuw boek worden aangekondigd Dus wordt er gefluisterd over online spelletjes" ;7
	     "ebooks en sociale medi" ;8
         "De zeven Potter boeken zijn wereldwijd meer dan 450 miljoen keer over de toonbank gegaan" ;9
         "Samen met de recordopbrengsten van de filmreeks hebben ze Rowling tot de rijkste auteur"  ;10
         "ter wereld gemaakt"  ;11
         "Op dit moment staat de laatste film in de serie op het punt om in de bioscoop" ;12
         "te komen De verwachtingen zijn hooggespannen Zal deze keer de Harry Potter" ;13
         "film eindelijk een Oscar krijgen net als bij het laatste deel van de lord" ;14
         "of the rings trilogie"  ;15
         "We all know what happened in the end but"  ;16
         "Of zullen de leden van de academy voorbijgaan aan dit hollywood"  ;17
         "kassucces De spanning is daarom groot dit jaar"  ;18
         "Oscar of niet Rowling zal er niet om rouwen want de buit is al binnen")))))  ;19
    (setf (etask-edits *ed-task*) (nth *task-count* *edit-tasks*))
    (setf *task-count* (mod (1+ *task-count*) 3))
    
    (determine-v *ed-task*))

(defvar *standard-motor-time* 0.25)
(defvar *standard-visual-time* 0.25) ;; includes the production for the attention shift
(setf *standard-visual-time* 0.25)

(defun substitute-insert (element new l)
	(cond ((null l) nil)
		((and (equal (first l) element) (equal new '(""))) (rest l))
		((equal (first l) element) (append new (rest l)))
		(t (cons (first l) (substitute-insert element new (rest l))))))

(defun find-position (x l)
  (cond ((null l) 0)
  	((eq x (first l)) 0)
  	(t (1+ (find-position x (rest l))))))

(defun perform-action (action &optional h1 h2)
	"Depending on the action, change the *ed-task* representation, and return the latency of the action"
	(let ((latency 0.05))
          (case (report-state *rep*)
            (ll-noread (when (eq action 'read-instruction) 
                         (setf (report-state *rep*) 'll) (setf (report-task *rep*) (caar (etask-edits *ed-task*)))))
            (ll (when (member action '(number-p enter t-word))
                      (setf (report-ll *rep*) (- (mp-time) (report-strt *rep*)))
                      (setf (report-state *rep*) 'LL-motor))

                (when (and (eq action 'read-instruction) (eq (fourth (first (etask-edits *ed-task*))) (nth (etask-line-pos *ed-task*) *numbers*)))
                  (when (eq (report-state *rep*) 'll) (setf (report-ll *rep*) (- (mp-time) (report-strt *rep*))))
                  (setf (report-state *rep*) 'mt
                        (report-temp *rep*) (mp-time))
                        ))
            (ll-motor (when (eq action 'read-instruction)                              
                        (setf (report-state *rep*) 'mt
                              (report-temp *rep*) (mp-time))))
            (mt (when (member action '(substitute-ed substitute-edt insert-ed insert-edt period-d type-text
                                                     type-text-enter d control-k control-k-twice esc-d))
                  (setf (report-state *rep*) 'mt-motor
                        (report-mt *rep*) (- (mp-time) (report-temp *rep*)))))
            (mt-motor (when (eq action 'next-instruction) 
                        (setf (report-state *rep*) 'll)
                        (push (format nil "~15A ~6,3F ~6,3F ~6,3F" (report-task *rep*)(report-ll *rep*)(report-mt *rep*)(- (mp-time) (report-strt *rep*))) *report*)
                        (setf (report-task *rep*) (caadr (etask-edits *ed-task*)))
                        (setf (report-strt *rep*) (mp-time)))))
                               
                              

	(case action
	  ((enter control-n) (incf (etask-line-pos *ed-task*))
	  		(setf (etask-cursor-pos *ed-task*) 0)
	  		  (if (eq action 'enter)
 		  		  (setf latency (+ *standard-motor-time* *standard-visual-time*))
 		  		  (setf latency *standard-motor-time*))
	  		 (determine-v *ed-task*))
	  ((esc-f move-attention-right) (incf (etask-cursor-pos *ed-task*))
	  		 (setf latency *standard-visual-time*)
	  		 (when (eq action 'esc-f) (setf latency (* 2 *standard-motor-time*)))
	  		 (determine-v *ed-task*))
	  (read-screen (determine-v *ed-task*) (setf latency *standard-visual-time*))
	  (read-instruction (setf (etask-vlist *ed-task*) (first (etask-edits *ed-task*)))
	          (setf latency *standard-visual-time*) )
	  (next-instruction (pop (etask-edits *ed-task*))
	  	    (when (etask-edits *ed-task*) (setf (etask-vlist *ed-task*) (first (etask-edits *ed-task*))))
	  	    (when (null (etask-edits *ed-task*))	 (setf (etask-vlist *ed-task*) '(end)))
	        (setf latency *standard-visual-time*) )
          (focus-on-word (setf (etask-line *ed-task*) (split-by-one-space (second (first (etask-edits *ed-task*)))))
                         (setf (etask-vlist *ed-task*) (list 'single-word (first (etask-line *ed-task*)) (if (or (= (length (etask-line *ed-task*)) 1) (> (length (first (etask-line *ed-task*))) 4)) 'long 'short)))
                         (pop (etask-line *ed-task*))
                         (setf latency *standard-visual-time*) )
                          
          (focus-on-next-word                         (setf (etask-vlist *ed-task*) (list 'single-word (first (etask-line *ed-task*)) (if (or (= (length (etask-line *ed-task*)) 1) (> (length (first (etask-line *ed-task*))) 4)) 'long 'short)))
                         (pop (etask-line *ed-task*))
                         (setf latency *standard-visual-time*))
	          	
	  (esc-d (setf (aref (etask-text *ed-task*) (etask-line-pos *ed-task*))
	  				 (append (subseq (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) 0 (etask-cursor-pos *ed-task*))
	  				 	     (subseq (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) (1+ (etask-cursor-pos *ed-task*)))))
	  		  (setf latency  *standard-motor-time*)
	  		  (determine-v *ed-task*))
	  (type-text (setf (aref (etask-text *ed-task*)(etask-line-pos *ed-task*))
	  			  (append (subseq (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) 0 (etask-cursor-pos *ed-task*))
	  			  	       (split-by-one-space h1)
	  				 	     (subseq (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) (etask-cursor-pos *ed-task*))))
	  		(setf latency (* (length h1) *standard-motor-time*))
	  		(determine-v *ed-task*))
          (type-text-enter 
           (let ((pos (etask-line-pos *ed-task*)))
             (dotimes (i (- 19 pos))
               (setf (aref (etask-text *ed-task*) (- 19 i)) (aref (etask-text *ed-task*) (- 18 i))))
             (setf (aref (etask-text *ed-task*) pos) nil))
           (setf (aref (etask-text *ed-task*)(etask-line-pos *ed-task*))
                 (append (subseq (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) 0 (etask-cursor-pos *ed-task*))
                         (split-by-one-space h1)
                         (subseq (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) (etask-cursor-pos *ed-task*))))
           (setf latency (* (1+ (length h1)) *standard-motor-time*))
           (setf (etask-cursor-pos *ed-task*) 0)
           (determine-v *ed-task*))
          

	  ((substitute-ed substitute-edt) (setf (aref (etask-text *ed-task*)(etask-line-pos *ed-task*))
	  				(substitute-insert h1 (split-by-one-space h2) (aref (etask-text *ed-task*)(etask-line-pos *ed-task*)) ))
	  		(setf latency (* (+ (length h1) (length h2) (if (eq action 'substitute-ed) 5 4)) *standard-motor-time*))
	  		(setf (etask-cursor-pos *ed-task*) 0)
	  		(determine-v *ed-task*))
	  ((insert-ed insert-edt) (let ((h3 (concatenate 'string h2 " " h1)))
                                    (setf (aref (etask-text *ed-task*)(etask-line-pos *ed-task*))
                                          (substitute-insert h1 (split-by-one-space h3) (aref (etask-text *ed-task*)(etask-line-pos *ed-task*)) ))
                                    (setf latency (* (+ (length h1) (length h3) (if (eq action 'insert-ed) 5 4)) *standard-motor-time*))
                                    (setf (etask-cursor-pos *ed-task*) 0)
                                    (determine-v *ed-task*)))
                                    
;                                        (new-action (if (eq action 'insert-ed) 'substitute-ed  'substitute-edt)))
;	  			(setf latency (perform-action new-action h1 h3))))
	  ((period-d d control-k-twice) 
	  		(let ((pos (etask-line-pos *ed-task*)))
	  			(dotimes (i (- 19 pos))
	  				(setf (aref (etask-text *ed-task*) (+ i pos)) (aref (etask-text *ed-task*) (+ i pos 1)))))
                        
	  		(setf latency (if (eq action 'control-k-twice) (* 2 *standard-motor-time*)
                                        (+ *standard-visual-time* (* (if (eq action 'period-d) 3 2) *standard-motor-time*))))
	  		(setf (etask-cursor-pos *ed-task*) 0)
	  		(determine-v *ed-task*))
	  (control-k
	       (if (null (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)))
	       	 (perform-action 'd)
	         (setf (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) nil))
	       (setf latency  *standard-motor-time*)
	       	(determine-v *ed-task*))

	  ((period-a i)
	  		(let ((pos (etask-line-pos *ed-task*)))
	  			(dotimes (i (- 19 pos))
	  				(setf (aref (etask-text *ed-task*) (- 19 i)) (aref (etask-text *ed-task*) (- 18 i))))
	  			(setf (aref (etask-text *ed-task*) pos) nil))
	  		(setf latency (+ *standard-visual-time* (* (if (eq action 'period-a) 3 2) *standard-motor-time*)))
	  		(determine-v *ed-task*))
	  ((period-c r)
	         (setf (aref (etask-text *ed-task*) (etask-line-pos *ed-task*)) nil)
	  		(setf latency (+ *standard-visual-time* (* (if (eq action 'period-c) 3 2) *standard-motor-time*)))
	  		(determine-v *ed-task*))
	  ((period control-z)
	  		(setf latency (+ *standard-visual-time* (* (if (eq action 'period) 2 1) *standard-motor-time*))))
	  (number-p
	        (setf (etask-line-pos *ed-task*) (find-position h1 *numbers*))
	  		(setf (etask-cursor-pos *ed-task*) 0)       
	  	    (determine-v *ed-task*))
			(setf latency (+ *standard-visual-time* (* 2 *standard-motor-time*)))	
	  (t-word
	       (let ((line (etask-line-pos *ed-task*)))
	       	   (loop while (not (member h1 (aref (etask-text *ed-task*) line) :test #'equal) ) do (incf line))
	       	   (setf (etask-line-pos *ed-task*) line))
	  		(setf (etask-cursor-pos *ed-task*) 0)       
	  		(setf latency (* (+ 5 (length h1)) *standard-motor-time*))
	  		(determine-v *ed-task*))
	       	   
	  	    )
	(setf *perception* (etask-vlist *ed-task*))
	latency))

;;; These three functions just run a particular editor n times

(defun do-ed (n)
  (set-task 'ed)
  (setf *verbose* t)
                           (let* ((condition 'ed)
                                  (ins-chunks (no-output (eval `(sdm task ,condition)))))
                             (dolist (x ins-chunks)
                               (base-level-activation (get-module declarative) x)
                               (when (or (< (chunk-creation-time x) -100000)  ;; we haven't yet set it
                                         (< (chunk-last-base-level x) 0.2))  ;; it has decayed too much
                                 (eval `(sdp ,x :creation-time ,(- (mp-time) 600) :references 20)))))
  (sgp :v nil)                         
  (let (result)
    (dotimes (i n) (init-task) (push (run 800.0) result))
    (reverse result)))

(defun do-edt (n)
  (set-task 'edt)
  (setf *verbose* t)
                           (let* ((condition 'edt)
                                  (ins-chunks (no-output (eval `(sdm task ,condition)))))
                             (dolist (x ins-chunks)
                               (base-level-activation (get-module declarative) x)
                               (when (or (< (chunk-creation-time x) -100000)  ;; we haven't yet set it
                                         (< (chunk-last-base-level x) 0.2))  ;; it has decayed too much
                                 (eval `(sdp ,x :creation-time ,(- (mp-time) 600) :references 20)))))
  (sgp :v nil)     (let (result)
    (dotimes (i n) (init-task) (push (run 800.0) result))
    (reverse result)))
    
(defun do-emacs (n)
  (set-task 'emacs)
  (setf *verbose* t)
                           (let* ((condition 'emacs)
                                  (ins-chunks (no-output (eval `(sdm task ,condition)))))
                             (dolist (x ins-chunks)
                               (base-level-activation (get-module declarative) x)
                               (when (or (< (chunk-creation-time x) -100000)  ;; we haven't yet set it
                                         (< (chunk-last-base-level x) 0.2))  ;; it has decayed too much
                                 (eval `(sdp ,x :creation-time ,(- (mp-time) 600) :references 20)))))
  (sgp :v nil)     (let (result)
    (dotimes (i n) (init-task) (push (run 800.0) result))
    (reverse result)))

;;; Conditions are:
;;; - ed, ed, emacs
;;; - edt, edt, emacs
;;; - ed, edt, emacs
;;; - edt, ed, emacs

;;; The numbers are the number of seconds sujects need per edit.
;;; These are used to estimate how many pages they can edit in a session

(defvar *sa-conditions*)
(setf *sa-conditions*
	'((ed-ed-emacs (ed ed emacs)(115 54 44 42 43 28))
	  (edt-edt-emacs(edt edt emacs)(115 54 55 49 43 28))
	  (ed-edt-emacs (ed edt emacs)(115 54 63 44 41 26))
	  (edt-ed-emacs (edt ed emacs)(115 54 46 37 41 26))
	  (emacs-emacs-emacs (emacs emacs emacs)(77 37 29 23 23 21))))

(defun do-sa (n &optional (fname "~/res-ed2.txt"))
  (dotimes (index n)
    (format t "~%Model run ~D~%~%" (1+ index))
	(with-open-file (f fname :direction :output :if-exists :append :if-does-not-exist :create)
	(dolist (x *sa-conditions*)
		(let (result j condition)
			(reset)
			(sgp :v nil :save-buffer-trace nil)
                        (setf *verbose* nil)
			(dotimes (i 6)
			   (setf j (round (/ 1800 (nth i (third x))))) ;;; number of trials
			   (setf condition (nth (floor (/ i 2)) (second x)))
;			   (format t "~%** Going to do condition ~A, ~D times~%" condition j)
			   (set-task condition)
;;; here we set the baselevels
;;; If the baselevel activation of an instruction chunk for the current task is below 0.48, we set it to 20 references and a creation-time
;;; 600 seconds before the current time, otherwise we leave it as it is.
;;; This simulates the repeated instruction on each day
                           (let ((ins-chunks (no-output (eval `(sdm task ,condition)))))
                             (dolist (x ins-chunks)
                               (base-level-activation (get-module declarative) x)
                               (when (or (< (chunk-creation-time x) -100000)  ;; we haven't yet set it
                                         (< (chunk-last-base-level x) 0.2))  ;; it has decayed too much
                                 (eval `(sdp ,x :creation-time ,(- (mp-time) 600) :references 20)))))

			   (dotimes (k j)
			      (init-task)
			      (setf result (run 2000.0))
                              (clear-buffer 'goal)
                              (dolist (y *report*)                               
                                (format t "~A ~D ~A ~D ~A~%" (first x) (1+ i) condition (1+ k) y)
                                (format f "~A ~D ~A ~D ~A~%" (first x) (1+ i) condition (1+ k) y)))))))))
(defun run-experiment (n)
  (do-sa n))

(defun run-sample (&optional x)
  (case x
    (1 (print "Running ED") (do-ed 1))
    (2 (print "Running EDT") (do-edt 1))
    (3 (print "Running Emacs") (do-emacs 1))
    (otherwise (print "1 - ED  2 - EDT  3 - Emacs"))))


(define-model-transfer			
			
(add-dm

;;; count-facts
 (count0 isa fact slot1 count-fact slot2 zero slot3 one)
 (count1 isa fact slot1 count-fact slot2 one slot3 two)
 (count2 isa fact slot1 count-fact slot2 two slot3 three)
 (count3 isa fact slot1 count-fact slot2 three slot3 four)
 (count4 isa fact slot1 count-fact slot2 four slot3 five)
 (count5 isa fact slot1 count-fact slot2 five slot3 six)
 (count6 isa fact slot1 count-fact slot2 six slot3 seven)
 (count7 isa fact slot1 count-fact slot2 seven slot3 eight)
 (count8 isa fact slot1 count-fact slot2 eight slot3 nine)
 (count9 isa fact slot1 count-fact slot2 nine slot3 ten)
 (count10 isa fact slot1 count-fact slot2 ten slot3 eleven)
 (count11 isa fact slot1 count-fact slot2 eleven slot3 twelve)
 (count12 isa fact slot1 count-fact slot2 twelve slot3 thirteen)
 (count13 isa fact slot1 count-fact slot2 thirteen slot3 fourteen)
 (count14 isa fact slot1 count-fact slot2 fourteen slot3 fifteen)
 (count15 isa fact slot1 count-fact slot2 fifteen slot3 sixteen)
 (count16 isa fact slot1 count-fact slot2 sixteen slot3 seventeen)
 (count17 isa fact slot1 count-fact slot2 seventeen slot3 eighteen)
 (count18 isa fact slot1 count-fact slot2 eighteen slot3 nineteen)
 (count19 isa fact slot1 count-fact slot2 nineteen slot3 twenty)
)

;;; Generate all the diff-3-facts. These are used to check whether two numbers are more or less than three apart. If they are not, there is a fact.
;;; If they are more than 3 apart, there is not fact, so the retrieval failure signals the two numbers are far apart.

(dotimes (i (length *numbers*))
	(dotimes (j 3)
		(let* ((num1 (nth i *numbers*))
			   (num2pos (+ i j))
			   (num2 (if (< num2pos (length *numbers*)) (nth num2pos *numbers*) nil)))
			(when num2 (eval `(add-dm (isa fact slot1 diff-3-fact slot2 ,num1 slot3 ,num2)))))))
			


(add-instr ed :input (Vtype Vword1 Vword2 Vline) :working-memory (WMcurline WMsearch-goal) 
  :declarative ((RTcount-fact RTfirst RTsecond)(RTdiff-3-fact RTnum1 RTnum2))
  :pm-function perform-action
  :init init-ed-task
  :parameters ((sgp :lf 1.5 :egs 0.1 :ans 0.1 :rt -0.5 :ncnar nil)(setf *condition-spread* 0.0 *condition-penalty* -10.0))  ;; :ans was 0.2  
  :reward 800
  ;; Initialize: start in line 1, intialize the Gcontrol, read the first instruction and determine the first target line
  (ins :condition (Gcontrol = nil) :action (one -> WMcurline   read-instruction -> AC1  find-goal -> Gcontrol) :description "Start at line 1, read the next instruction")
  (ins :condition (Gcontrol = find-goal Vtype <> end) :action (Vline -> WMsearch-goal line-strategy -> Gcontrol read-screen -> AC1) :description "Look at the screen to determine where to go")
  
  ;; Determine the line strategy. More than three lines -> Do a <number>p operation. Otherwise repeatedly push enter.
  (ins :condition (Gcontrol = line-strategy  RT1 = nil) :action ( (diff-3-fact WMcurline WMsearch-goal) -> RT) :description "Is the goal line more than 3 lines away?")
  (ins :condition (Gcontrol = line-strategy  RTnum1 = WMcurline) :action (find-line -> Gcontrol) :description "No, so we press enter") ;;; Less than three: use enter strategy
  (ins :condition (Gcontrol = line-strategy RT1 = error) :action ((number-p  WMsearch-goal) -> AC  WMsearch-goal -> WMcurline find-line -> Gcontrol) :description "Yes, we use the p command")
  
  ;; Now search for the target line by going down in the file. 
  (ins :condition (WMcurline <> WMsearch-goal RTsecond = nil Gcontrol = find-line) :action (enter -> AC1 (count-fact WMcurline) -> RT) :description "Pressing enter")
  (ins :condition (WMcurline = WMsearch-goal Gcontrol = find-line) :action (find-task -> Gcontrol read-instruction -> AC1) :description "Done pressing enter, rereading the instruction")
  (ins :condition (WMsearch-goal <> RTsecond RTfirst = WMcurline Gcontrol = find-line) :action (RTsecond -> WMcurline enter -> AC1 (count-fact WMcurline) -> RT) :description "Not done yet, pressing enter again")
  (ins :condition (WMsearch-goal = RTsecond Gcontrol = find-line) :action (RTsecond -> WMcurline find-task -> Gcontrol read-instruction -> AC1) :description "Done pressing enter, rereading the instruction")
  
  ;; If the task is something to do with a word, we first find the word by visually scanning the line
  (ins :condition (Vtype = replace-word  Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "It is something with a word, so searching the word on the line")
  (ins :condition (Vtype = delete-word Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "It is something with a word, so searching the word on the line")
  (ins :condition (Vtype = insert-word Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "It is something with a word, so searching the word on the line")
  (ins :condition (Vtype = word Vword1 <> WMsearch-goal Gcontrol = find-word) :action (move-attention-right -> AC1) :description "Looking at the next word")
  (ins :condition (Vword1 = WMsearch-goal Gcontrol = find-word) :action (word-action -> Gcontrol read-instruction -> AC1) :description "Found the word")
  
  ;; Now that we found the word, we are going to do something with it depending on the type of edit
  (ins :condition (Vtype = replace-word Gcontrol = word-action) :action ((substitute-ed Vword1 Vword2) -> AC) :description "Doing a replace word")
  (ins :condition (Vtype = delete-word Gcontrol = word-action) :action ((substitute-ed Vword1 Vword2) -> AC) :description "Deleting the word")
  (ins :condition (Vtype = insert-word Gcontrol = word-action) :action ((insert-ed Vword1 Vword2) -> AC) :description "Inserting a word")
  (ins :condition (Vtype = word  Gcontrol = word-action) :action (next-instruction -> AC1  find-goal -> Gcontrol) :description "Going to the next instruction")
  
  ;;; If the task is something with the whole line, carry out the appropriate line command
  (ins :condition (Vtype = delete-line Gcontrol = find-task) :action (period-d -> AC1  word-action -> Gcontrol) :description "Deleting the line")
  (ins :condition (Vtype = insert-line Gcontrol = find-task) :action (period-a -> AC1  Vword2 -> WMsearch-goal  type-line -> Gcontrol) :description "Inserting a new line")
  (ins :condition (Vtype = replace-line Gcontrol = find-task) :action (period-c -> AC1  Vword2 -> WMsearch-goal  type-line -> Gcontrol) :description "Replacing the line")
  (ins :condition (Gcontrol = type-line) :action ((type-text WMsearch-goal) -> AC  word-action -> Gcontrol) :description "Typing the text for the new line")
  
  
  (ins :condition (Gcontrol <> nil Vtype = end) :action (finish -> Gtask) :description "We're done!")
  )


(add-instr edt :input (Vtype Vword1 Vword2 Vline) :variables (WMcurline WMsearch-goal) 
  :declarative ((RTcount-fact RTfirst RTsecond)(RTdiff-3-fact RTnum1 RTnum2))
  :pm-function perform-action
  :init init-ed-task
  :reward 800
  :parameters ((sgp :lf 1.5 :egs 0.1 :ans 0.1 :rt -0.5 :ncnar nil)(setf *condition-spread* 0.0 *condition-penalty* -10.0))   ;; :ans was 0.2  :reward 500
  ;; Initialize: start in line 1, intialize the Gcontrol, read the first instruction and determine the first target line
  (ins :condition (Gcontrol = nil) :action (one -> WMcurline   read-instruction -> AC1  find-goal -> Gcontrol) :description "Start at line 1, read the next instruction")
  (ins :condition (Gcontrol = find-goal Vtype <> end) :action (Vline -> WMsearch-goal line-strategy -> Gcontrol read-screen -> AC1) :description "Look at the screen to determine where to go")
  
  ;; Determine the line strategy. More than three lines -> Do a t 'word' operation. Otherwise repeatedly push enter.
  (ins :condition (Gcontrol = line-strategy  RT1 = nil) :action ( (diff-3-fact WMcurline WMsearch-goal) -> RT) :description "Is the goal line more than 3 lines away?")
  (ins :condition (Gcontrol = line-strategy  RTnum1 = WMcurline) :action (find-line -> Gcontrol) :description "No, so we press enter") ;;; Less than three: use enter strategy
  (ins :condition (Gcontrol = line-strategy RT1 = error) :action (read-instruction -> AC1 word-strategy -> Gcontrol) :description "Yes, so we'll look for a word that we do a search on")
  (ins :condition (Gcontrol = word-strategy Vtype <> single-word Vtype <> word) :action (focus-on-word -> AC1) :description "Look for a first word")
  (ins :condition (Gcontrol = word-strategy  Vword2 = long) :action ((t-word Vword1) -> AC) :description "If it is a long word, then do the t command to search it")
  (ins :condition (Gcontrol = word-strategy  Vword2 = short) :action (focus-on-next-word -> AC1) :description "If it is a short word, then look at the next word")
  (ins :condition (Gcontrol = word-strategy Vtype = word) :action (find-line -> Gcontrol Vline -> WMcurline) :description "We are done with the move, but need to check we're actually there")
  
  ;; Now search for the target line by going down in the file. Has to be extended for longer jumps
  (ins :condition (WMcurline <> WMsearch-goal RTsecond = nil Gcontrol = find-line) :action (enter -> AC1 (count-fact WMcurline) -> RT) :description "Pressing enter")
  (ins :condition (WMcurline = WMsearch-goal Gcontrol = find-line) :action (find-task -> Gcontrol read-instruction -> AC1) :description "Done pressing enter, rereading the instruction")
  (ins :condition (WMsearch-goal <> RTsecond RTfirst = WMcurline Gcontrol = find-line) :action (RTsecond -> WMcurline enter -> AC1 (count-fact WMcurline) -> RT) :description "Not done yet, pressing enter again")
  (ins :condition (WMsearch-goal = RTsecond Gcontrol = find-line) :action (RTsecond -> WMcurline find-task -> Gcontrol read-instruction -> AC1) :description "Done pressing enter, rereading the instruction")
  
  ;; If the task is something to do with a word, we first find the word by visually scanning the line
  (ins :condition (Vtype = replace-word  Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "It is something with a word, so searching the word on the line")
  (ins :condition (Vtype = delete-word Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "It is something with a word, so searching the word on the line")
  (ins :condition (Vtype = insert-word Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "It is something with a word, so searching the word on the line")
  (ins :condition (Vtype = word Vword1 <> WMsearch-goal Gcontrol = find-word) :action (move-attention-right -> AC1) :description "Looking at the next word")
  (ins :condition (Vword1 = WMsearch-goal Gcontrol = find-word) :action (word-action -> Gcontrol read-instruction -> AC1) :description "Found the word")
  
  ;; Now that we found the word, we are going to do something with it depending on the type of edit
  (ins :condition (Vtype = replace-word Gcontrol = word-action) :action ((substitute-edt Vword1 Vword2) -> AC) :description "Doing a replace word")
  (ins :condition (Vtype = delete-word Gcontrol = word-action) :action ((substitute-edt Vword1 Vword2) -> AC) :description "Deleting the word")
  (ins :condition (Vtype = insert-word Gcontrol = word-action) :action ((insert-edt Vword1 Vword2) -> AC) :description "Inserting a word")
  (ins :condition (Vtype = word  Gcontrol = word-action) :action (next-instruction -> AC1  find-goal -> Gcontrol) :description "Going to the next instruction")
  
  ;;; If the task is something with the whole line, carry out the appropriate line command
  (ins :condition (Vtype = delete-line Gcontrol = find-task) :action (d -> AC1  word-action -> Gcontrol) :description "Deleting the line")
  (ins :condition (Vtype = insert-line Gcontrol = find-task) :action (i -> AC1  Vword2 -> WMsearch-goal  type-line -> Gcontrol) :description "Inserting a new line")
  (ins :condition (Vtype = replace-line Gcontrol = find-task) :action (r -> AC1  Vword2 -> WMsearch-goal  type-line -> Gcontrol) :description "Replacing the line")
  (ins :condition (Gcontrol = type-line) :action ((type-text WMsearch-goal) -> AC  word-action -> Gcontrol) :description "Typing the text for the new line")
  
  
  (ins :condition (Gcontrol <> nil Vtype = end) :action (finish -> Gtask) :description "We're done!")
  )

(add-instr emacs :input (Vtype Vword1 Vword2 Vline) :variables (WMsearch-goal)
  :pm-function perform-action
  :init init-ed-task
  :reward 800
  :parameters ((sgp :lf 1.5 :egs 0.1 :ans 0.1 :rt -0.5 :ncnar nil)(setf *condition-spread* 0.0 *condition-penalty* -10.0))  ;; :ans was 0.2 
  ;; Initialize: intialize the Gcontrol, read the first instruction and determine the first target line
  (ins :condition (Gcontrol = nil) :action (read-instruction -> AC1  find-goal -> Gcontrol) :description "Read the first instruction")
  (ins :condition (Gcontrol = find-goal Vtype <> end) :action (Vline -> WMsearch-goal find-line -> Gcontrol read-screen -> AC1) :description "Look at the screen to determine where to go")
  
  ;; Now search for the target line by going down in the file. 
  (ins :condition (Vline <> WMsearch-goal Gcontrol = find-line) :action (control-n -> AC1) :description "As long as we are not on the right line we press control-n")
  (ins :condition (Vline = WMsearch-goal Gcontrol = find-line) :action (find-task -> Gcontrol read-instruction -> AC1) :description "Done moving down, reread the instruction")
  
  ;; If the task is something to do with a word, we first find the word by moving the cursor down the line
  (ins :condition (Vtype = replace-word  Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "If it is something with a word, then move the cursor to that word")
  (ins :condition (Vtype = delete-word Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "If it is something with a word, then move the cursor to that word")
  (ins :condition (Vtype = insert-word Gcontrol = find-task) :action (find-word -> Gcontrol Vword1 -> WMsearch-goal read-screen -> AC1) :description "If it is something with a word, then move the cursor to that word")
  (ins :condition (Vtype = word Vword1 <> WMsearch-goal Gcontrol = find-word) :action (esc-f -> AC1) :description "Not at the right word yet, press esc-f to go to the next")
  (ins :condition (Vword1 = WMsearch-goal Gcontrol = find-word) :action (word-action -> Gcontrol read-instruction -> AC1) :description "Found the word, reread instruction to see what we need to do with it")

  ;; Now that we found the word, we are going to do something with it depending on the type of edit
  (ins :condition (Vtype = replace-word Gcontrol = word-action) :action (esc-d -> AC1  Vword2 -> WMsearch-goal still-type -> Gcontrol) :description "Doing a replace word, delete it first")
  (ins :condition (Vtype = delete-word Gcontrol = word-action) :action (esc-d -> AC1) :description "Deleting the word")
  (ins :condition (Vtype = insert-word Gcontrol = word-action) :action ((type-text Vword2) -> AC) :description "Inserting a word")
  (ins :condition (Gcontrol = still-type) :action ((type-text WMsearch-goal) -> AC word-action -> Gcontrol) :description "Inserting a word after a delete")
  (ins :condition (Vtype = word  Gcontrol = word-action) :action (next-instruction -> AC1  find-goal -> Gcontrol) :description "Go to the next instruction" )
  
  ;;; If the task is something with the whole line, carry out the appropriate line command
  (ins :condition (Vtype = delete-line Gcontrol = find-task) :action (control-k-twice -> AC1  word-action -> Gcontrol) :description "Deleting the line")
  (ins :condition (Vtype = insert-line Gcontrol = find-task) :action ((type-text-enter Vword2) -> AC  word-action -> Gcontrol) :description "Inserting a new line")
  (ins :condition (Vtype = replace-line Gcontrol = find-task) :action (control-k -> AC1  Vword2 -> WMsearch-goal  type-line -> Gcontrol) :description "Deleting the line to replace it")
  (ins :condition (Gcontrol = type-line) :action ((type-text WMsearch-goal) -> AC  word-action -> Gcontrol) :description "Typing the text for the new line")
  
  (ins :condition (Gcontrol <> nil Vtype = end) :action (finish -> Gtask) :description "We're done!")
)

(sdp :reference-count 800 :creation-time -1000000)

)
			      
			      
			