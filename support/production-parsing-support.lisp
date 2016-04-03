;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : production-parsing-support.lisp
;;; Version     : 2.0
;;; 
;;; Description : Functions and code that's used by both p and p* parsing.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider collapsing multiple conditions for the
;;;             :     same buffer into one spec and test for conflicts (how
;;;             :     often does that happen though?)
;;;             : [ ] Does sort-for-binding actually do anything useful now?
;;; 
;;; ----- History -----
;;; 2009.09.10 Dan [3.0]
;;;             : * Initial creation.
;;; 2009.09.18 Dan
;;;             : * Now all the real work of the production parsing is here and
;;;             :   p and p* are done by a single set of code.
;;; 2009.09.28 Dan
;;;             : * Added the additional parsing necessary for handling the 
;;;             :   searchable buffers.
;;; 2010.01.12 Dan
;;;             : * Augmented the search condition to include the buffer's
;;;             :   variable so that doesn't have to be recreated every
;;;             :   conflict-resolution.
;;; 2010.04.23 Dan
;;;             : * Modified the condition creation so that results are always
;;;             :   explicitly t or nil (where appropriate).
;;;             : * Changed number-test to make sure it returns t or nil.
;;;             : * Instead of chunk-slot-equal as a test in a condition now
;;;             :   use safe-chunk-slot-equal which makes sure to return t or
;;;             :   nil explicitly.
;;; 2010.05.04 Dan
;;;             : * Moved the test to determine if a searched buffer is used
;;;             :   more than once so that it actually can catch that and reject
;;;             :   the production.
;;; 2010.07.27 Dan
;;;             : * Fixed a bug which broke the ability to use a variablized
;;;             :   slot in a request in a p*.
;;; 2010.07.27 Dan
;;;             : * Fixed a bug that would allow a p to have p* actions if they
;;;             :   were identical to ones cached for a p*.
;;; 2010.07.28 Dan
;;;             : * Fixed a bug introduced with the last fix that would break
;;;             :   for some requests which used modifiers.
;;; 2010.09.30 Dan 
;;;             : * Properly order the binds on the RHS and make sure that they
;;;             :   come before any other actions that use them.
;;; 2010.09.30 Dan
;;;             : * Fixed a bug in circular-references introduced when mv-bind
;;;             :   was added that broke the check for normal binds.
;;; 2011.01.21 Dan
;;;             : * Ensure that all non-reordered binds stay in 'production 
;;;             :   order' which, while not technically specified, is how
;;;             :   people expect it to happen.
;;; 2011.01.26 Dan
;;;             : * Give a warning if RHS binds/evals are going to be executed
;;;             :   in an order other than specified in the production.
;;; 2011.02.09 Dan
;;;             : * Changed stable-sorting with bind-order to a special sorting function
;;;             :   because it was easier than trying to fix bind-order to get 
;;;             :   things right when there were intermixed dependencies.
;;; 2011.02.11 Dan
;;;             : * Updated that new sort function to use splice-into-position-des
;;;             :   instead of splcie-into-list-des because the latter unpacks
;;;             :   list elements being inserted.
;;; 2011.04.01 Dan
;;;             : * Adjusted the RHS action sorting some more so that evals and
;;;             :   outputs remain in original order with binds if there's no
;;;             :   dependencies.
;;; 2011.04.28 Dan
;;;             : * Remove some unneeded variables from lets and added some declaims.
;;; 2011.10.11 Dan [1.1]
;;;             : * Changed the warning output when defining a production so that
;;;             :   it can be read top-down instead of bottom-up and added a
;;;             :   marker to indicate where it stops.  It relies on capturing
;;;             :   error-output and then parsing the #|---|# bracketed warnings
;;;             :   out of it.  However, if the production text includes #| or |#
;;;             :   sequences then it can't parse it properly and just writes it
;;;             :   out in the old bottom-up fashion with an additional warning
;;;             :   indicating that.
;;; 2011.12.08 Dan
;;;             : * Fixed a bug in the production error printing that caused 
;;;             :   errors in some Lisps for some warning situations.
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;; 2012.07.26 Dan
;;;             : * Don't like this but when the one-stream-hack is turned
;;;             :   on (which happens for all Lisps when the environment is 
;;;             :   connected in some situations like reloading) every warning 
;;;             :   goes to only *error-output*.  That means that they will be
;;;             :   captured by the special stream that's set up during 
;;;             :   the production parsing.  Therefore, if there's a
;;;             :   warning which didn't prevent the production from
;;;             :   being created it still needs to output those to the
;;;             :   original *error-stream* since otherwise they are lost.
;;;             :   So, create-production has to do that when it is successful
;;;             :   as well as when it fails.
;;; 2012.12.06 Dan
;;;             : * Don't allow an unnecessary empty buffer modification now.
;;; 2013.01.28 Dan
;;;             : * Replaced calls to the internal chunk-type functions with
;;;             :   the appropriate command.
;;; 2013.03.13 Dan [1.2]
;;;             : * Changed valid-buffer-request-slot to allow slots of a child
;;;             :   type to be specified in a parent type for requests.  This could
;;;             :   lead to run time errors/warnings for productions that previously
;;;             :   would have been disallowed at definition time.
;;; 2013.03.19 Dan
;;;             : * Allow the modifications to also specify child slots as long
;;;             :   as they are in the conditions as well.  That guarantees that
;;;             :   it is safe to modify because otherwise it would either be a
;;;             :   run time problem or constant slots would need to be allowed to 
;;;             :   extend chunks.
;;; 2013.04.10 Dan
;;;             : * Instead of calling extend-buffer-chunk use the newly built
;;;             :   in capability of schedule-mod-buffer-chunk.
;;; 2013.05.16 Dan [1.3]
;;;             : * Added the ability to pass request parameters to direct requests
;;;             :   by including them in a list with the chunk:
;;;             :   (p test ==> +retrieval> (chunk :recently-retrieved nil)).
;;; 2013.05.23 Dan
;;;             : * Valid-chunk-mod-spec now allows a static type to modify any
;;;             :   of the possible slots in a static type since mod-buffer-chunk
;;;             :   is able to resolve the type as needed and otherwise it can't
;;;             :   do the "same" thing as the old mechanism during compilation 
;;;             :   since the type specified in the production still won't "have"
;;;             :   the slot.
;;; 2013.07.25 Dan
;;;             : * Added the check for production style warnings to create-
;;;             :   production so that subsequent productions test them if 
;;;             :   needed.
;;; 2013.08.09 Dan
;;;             : * Save the production !output! string in the buffer trace too.
;;; 2014.03.17 Dan [2.0]
;;;             : * Start of conversion to chunks and chunk-specs without types.
;;; 2014.04.03 Dan
;;;             : * Fixed a problem with must-be in creating implicit condtions.
;;; 2014.04.08 Dan
;;;             : * The parse tables need to keep track of any new slots created
;;;             :   because on reset they have to create them again for things
;;;             :   pulled out of the table.
;;; 2014.04.23 Dan
;;;             : * Added the parsing for search buffers as well.
;;; 2014.05.07 Dan
;;;             : * Changed a warning from "direct request" to "indirect request"
;;;             :   because I want to change that nomenclature everywhere.
;;; 2014.05.13 Dan
;;;             : * Allow an "empty" buffer condition which translates to isa chunk,
;;;             :   and basically just means there's some chunk there.
;;; 2014.05.27 Dan
;;;             : * Also allow an "empty" buffer request which also translates
;;;             :   to "isa chunk".  The reason for this (and the previous change)
;;;             :   is because production compilation may result in conditions or
;;;             :   actions with no constraints when the only thing in one of the 
;;;             :   productions was an isa in the definition.
;;; 2014.05.29 Dan
;;;             : * Add in the backward hack test to make sure that the old syntax
;;;             :   for modification requests (+ without an isa) is handled correctly.
;;; 2014.05.30 Dan
;;;             : * Same hack for the old syntax of overwrite requests.
;;;             : * Only apply the backward hacks if (procedural-in-model-definition prod)
;;;             :   is true.
;;; 2014.06.06 Dan
;;;             : * Fixed a bug with some bad calls to bad-condition-exit.
;;; 2014.06.09 Dan
;;;             : * Fixed a bug with the buffer overwrite hack for backwards 
;;;             :   because it was being rejected before the swap to the new
;;;             :   action occurred.
;;; 2014.06.24 Dan
;;;             : * Allow a production to use mismatched slots relative to a
;;;             :   specified type (the <:mismatch x y> lists from define-
;;;             :   chunk-spec) and just warn about it.
;;; 2014.08.12 Dan
;;;             : * Pass the production name to parse-conditions and parse-actions
;;;             :   so that warnings can indicate which production caused the
;;;             :   issue.
;;; 2014.09.30 Dan
;;;             : * Fixed a bug with parse-action with respect to providing a
;;;             :   request parameter to a modification request.
;;; 2015.03.13 Dan
;;;             : * Don't try to create a chunk for a keyword used in a slot
;;;             :   value.  Treat it like a number or string and leave it alone.
;;; 2015.03.20 Dan
;;;             : * Add strict harvesting for a ?buffer> buffer failure query
;;;             :   in the conditions.
;;;             : * Allow an empty modification when there's an = buffer failure
;;;             :   query to avoid the strict harvesting.
;;; 2015.05.06 Dan
;;;             : * Schedule events in ms.
;;; 2015.07.28 Dan
;;;             : * Removed the *act-r-6.0-compatibility* hack.
;;; 2015.08.28 Dan
;;;             : * Have a copied search buffer rebind the buffer variable 
;;;             :   once the new chunk is created for the buffer.
;;; 2015.09.10 Dan [3.0]
;;;             : * The production requests (and mod requests) need to be tracked
;;;             :   now and saved on the production-requested-actions list.
;;; 2015.12.16 Dan
;;;             : * Those tracking tags are now a second return value from the
;;;             :   function that schedules them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Moved common code for production parsing to support to avoid some warnings
;;; at load time and as a first step to a better integration of p and p*.
;;;
;;; This code cheats and uses the internal rep of a chunk-spec...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Because of the way that conditions are tested in the procedural matching
;;; functions the results which are being tested for must be eq to the 
;;; values returned from the testing functions.  In particular, that means
;;; that I can't just return something like (eq val1 val2) from a test function
;;; if t is the result that I'm looking for because eq (and all of the Lisp
;;; equality/testing functions) are only required to return a generalized-boolean
;;; with anything non-nil representing true.
;;;
;;; I've chosen to handle this in the test functions instead of changing the
;;; matching code because I don't see a fast and clean replacement for the current: 
;;; (eq (result ...) (funcall ...)) test that's there now which handles either
;;; t or nil directly and also allows for the possibility of other values with future
;;; updates.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(declaim (ftype (function (t t) t) add-buffer-trace-notes))

(defun buffer-name->variable (name)
  (intern (concatenate 'string "=" (symbol-name name))))

(defun safe-chunk-slot-equal (c1 c2)
  (when (chunk-slot-equal c1 c2) t))

(defun create-production (prod definition)
  (let* ((original-error-stream *error-output*)
         (error-string-stream (make-string-output-stream))
         (*error-output* error-string-stream)
         (production (make-production :text (copy-tree definition))))
    
    (flet ((bad-production-exit (&rest strings)
                                (let ((*error-output* original-error-stream)
                                      (warn-text (get-output-stream-string error-string-stream)))
                                  (print-warning "No production defined for ~s." (production-text production))
                                  (dolist (x strings)
                                    (print-warning x))
                                  (when (> (length warn-text) 2) ;; make sure it has something in it and 
                                                                 ;; checking for more than 2 chars to be safe
                                                                 ;; since length 1 could cause an error
                                    (let ((starts 
                                         (do* ((start -1 end)
                                               (warn-indices nil)
                                               (end (search "#|" warn-text :start2 (1+ start))
                                                    (search "#|" warn-text :start2 (1+ start))))
                                              ((null end) warn-indices)
                                           (push-last end warn-indices)))
                                        (ends 
                                         (do* ((start 0 end)
                                               (warn-indices nil)
                                               (end (search "|#" warn-text :start2 (1+ start))
                                                    (search "|#" warn-text :start2 (1+ start))))
                                              ((null end) warn-indices)
                                           (push-last end warn-indices))))
                                    (if (= (length starts) (length ends))
                                        (dolist (x (pairlis starts ends))
                                          (format *error-output* "~a~%" (subseq warn-text (car x) (+ 2 (cdr x)))))
                                      (progn 
                                        (print-warning "Production has #| or |# sequences within it which affects how the warnings are displayed")
                                        (format *error-output* warn-text)))))
                                  (print-warning "--- end of warnings for undefined production ~s ---" 
                                                 (aif (production-name production) it (car (production-text production)))))
                                (close error-string-stream)
                                (return-from create-production nil)))
    
    (unless (symbolp (car definition))
      (bad-production-exit "Production name must be a symbol."))
    
    (setf (production-name production) (pop definition))
    
    (when (stringp (car definition))
      (setf (production-documentation production) (pop definition)))
    
    (aif (position '==> definition)
         (let* ((buffers (buffers))
                (pre-lhs (parse-conditions prod (production-name production) (subseq definition 0 it) buffers))
                (lhs (if (eq :error pre-lhs) :error (sort-for-binding pre-lhs)))
                (rhs (unless (eq lhs :error)
                       (parse-actions prod (production-name production) (subseq definition (1+ it)) lhs buffers)))
                (searched-buffers nil))
           
           (when (or (eq lhs :error)
                     (eq rhs :error))
             (bad-production-exit))
           
           (setf (production-lhs production) lhs)
           (setf (production-rhs production) rhs)
           
           (setf (production-lhs-buffers production)
             (mapcan (lambda (x)  
                       (when (eq (production-statement-op x) #\=) 
                         (list (production-statement-target x))))
               lhs))
           
           (setf (production-rhs-buffers production)
             (remove-duplicates 
              (mapcan (lambda (x)
                        (unless (eq (production-statement-op x) #\!)
                          (list (production-statement-target x))))
                rhs)))
           
           
           (dolist (x (production-lhs-buffers production))
             (unless (assoc x (procedural-buffer-indices prod))
               (push (cons x (procedural-buffer-lookup-size prod)) (procedural-buffer-indices prod))
               (incf (procedural-buffer-lookup-size prod))))                            
           
           (dolist (x (production-rhs-buffers production))
             (unless (assoc x (procedural-buffer-indices prod))
               (push (cons x (procedural-buffer-lookup-size prod)) (procedural-buffer-indices prod))
               (incf (procedural-buffer-lookup-size prod))))
           
           
           (let* ((lhs-variables (remove-duplicates (mapcan (lambda (x)
                                                              (aif (production-statement-spec x)
                                                                   (copy-list (act-r-chunk-spec-variables it))
                                                                   (find-variables (production-statement-definition x))))
                                                      lhs)))
                  (rhs-variables (remove-if (lambda (x)
                                              (find x lhs-variables))
                                            (remove-duplicates (mapcan (lambda (x)
                                                                         (aif (production-statement-spec x)
                                                                              (copy-list (act-r-chunk-spec-variables it))
                                                                              (find-variables (production-statement-definition x))))
                                                                 rhs))))
                  (buffer-variables (mapcan (lambda (x)
                                              (when (eql (production-statement-op x) #\=)
                                                (list (buffer-name->variable (production-statement-target x)))))
                                      lhs))
                  (slot-variables nil)
                  (dependencies nil)
                  (rhs-dependencies nil)
                  (unbound-vars nil))
             
             (setf (production-variables production)
               (remove-duplicates (append lhs-variables rhs-variables buffer-variables)))
             
             (setf unbound-vars lhs-variables)
             
             (let ((constants nil)
                   (vars nil)
                   (var2s nil)
                   (others nil)
                   (binds nil)
                   (selection nil)
                   (implicit nil)
                   (search nil)
                   (search-bind nil)
                   (search-other nil)
                   (var-table (make-hash-table)))
        
               (dolist (buffer-name (production-lhs-buffers production))
                 (let ((bn buffer-name)  ;; closure voodoo
                       (bi (cdr (assoc buffer-name (procedural-buffer-indices prod))))
                       (bv (buffer-name->variable buffer-name)))
                   
                   (setf unbound-vars (remove bv unbound-vars))
                   
                   (if (searchable-buffer buffer-name)
                       (progn
                         (setf (gethash bv var-table) 'search)
                         
                         (push (make-cr-condition :type 'bind-buffer :buffer bn :bi bi :value bv) search-bind)
                         (push (lambda ()
                                 (let* ((buffer-set-chunk (cdr (assoc bv (production-bindings production))))
                                        (buffer-chunk (overwrite-buffer-chunk bn buffer-set-chunk)))
                                   (unless (eq buffer-chunk buffer-set-chunk)
                                     ;; for a copy buffer rebind the buffer variable to the actual chunk
                                     (setf (cdr (assoc bv (production-bindings production))) buffer-chunk))))
                               (production-conflict-code production)))
                     
                     (progn
                       (setf (gethash bv var-table) 'buffer)
                       (push (make-cr-condition :type 'bind-buffer :buffer bn :bi bi :value bv) vars)))))
               
               ;; new first step 
               ;; classify all of the variables as to how they are used
               ;; so that things can be put into the right places later.
               ;; 
               ;; A variable can be bound through one of: buffer, bind, fixed-slot, var-slot, search, search-slot, rhs-bind
               
               (dolist (c lhs)
                 (let ((target (production-statement-target c)))
                   
                   (case (production-statement-op c)
                     (#\=
                      (let ((constant-bound-slots (mapcan (lambda (x)
                                                            (when (and (chunk-spec-variable-p (act-r-slot-spec-value x))
                                                                       (not (chunk-spec-variable-p (act-r-slot-spec-name x)))
                                                                       (eq '= (act-r-slot-spec-modifier x)))
                                                              (list (act-r-slot-spec-value x))))
                                                    (act-r-chunk-spec-slots (production-statement-spec c))))
                            (variable-bound-slots (mapcan (lambda (x)
                                                            (when (and (chunk-spec-variable-p (act-r-slot-spec-value x))
                                                                       (chunk-spec-variable-p (act-r-slot-spec-name x))
                                                                       (eql '= (act-r-slot-spec-modifier x)))
                                                              (list (cons (act-r-slot-spec-name x) (act-r-slot-spec-value x)))))
                                                    (act-r-chunk-spec-slots (production-statement-spec c)))))
                        
                        (setf slot-variables (append slot-variables (act-r-chunk-spec-slot-vars (production-statement-spec c))))
                        (setf dependencies (append dependencies (act-r-chunk-spec-dependencies (production-statement-spec c))))
                        
                        (dolist (v constant-bound-slots)
                          (let ((current (gethash v var-table)))
                            (when (or (null current) (eq current 'var-slot))
                              (setf (gethash v var-table) (if (searchable-buffer target) 'search-slot 'fixed-slot)))))
                        
                        (dolist (s-v variable-bound-slots)
                          (if (searchable-buffer target)
                              (case (gethash (car s-v) var-table)
                                ((buffer bind fixed-slot)
                                 (unless (gethash (cdr s-v) var-table)
                                   (setf (gethash (cdr s-v) var-table) 'search-slot)))
                                (t
                                 (bad-production-exit "indirection not allowed among search buffer slots.")))
                            (unless (gethash (cdr s-v) var-table)
                              (setf (gethash (cdr s-v) var-table) 'var-slot))))))
                     (#\?
                      (setf slot-variables (append slot-variables (act-r-chunk-spec-slot-vars (production-statement-spec c))))
                      (setf dependencies (append dependencies (act-r-chunk-spec-dependencies (production-statement-spec c)))))
                     
                     (#\!
                      (case target
                        ((bind safe-bind)
                         (let ((var (first (production-statement-definition c))))
                           (if (gethash var var-table)
                               (bad-production-exit (format nil "Because variable ~s is bound more than once on the LHS." var))
                             (setf (gethash var var-table) 'bind))
                           (awhen (find-variables (rest (production-statement-definition c)))
                                  (push (push var it) dependencies))))
                        ((mv-bind)
                         (let ((bind-vars (first (production-statement-definition c))))
                           (dolist (x bind-vars)
                             (if (gethash x var-table)
                                 (bad-production-exit (format nil "Because variable ~s is bound more than once on the LHS." x))
                               (setf (gethash x var-table) 'bind))
                             (awhen (find-variables (rest (production-statement-definition c)))
                                    (push (push x it) dependencies))))))))))
               
               
               ;; up front check for disallowed indirection and unbound vars
               ;; - more than one layer deep
               ;; - from a search buffer
               ;; - from an explicit binding
               
               (dolist (x slot-variables)
                 
                 (case (gethash x var-table)
                   ((buffer fixed-slot)
                    )
                   (var-slot
                    (bad-production-exit (format nil "Because slot-name variable ~s requires more than one level of indirection." x)))
                   ((search search-slot)
                    (bad-production-exit (format nil "Because slot-name variable ~s is bound in a search buffer match." x)))
                   (bind
                    (bad-production-exit (format nil "Because slot-name variable ~s is bound in an explicit bind." x)))
                   (t
                    (bad-production-exit (format nil "Variable ~s is not bound on the LHS." x)))))
               
               ;; check the rest to make sure they're bound
               
               (dolist (x lhs-variables)
                 (unless (gethash x var-table)
                   (bad-production-exit (format nil "Variable ~s is not bound on the LHS." x))))
               
               ;; check for circularity up front now too
               
               (let ((d-vars (mapcar (lambda (x) (list x)) lhs-variables)))
                 (dolist (x dependencies)
                   (awhen (assoc (car x) d-vars)
                          (rplacd it (append (cdr it) (cdr x)))))
                 (if (circular-references d-vars)
                     (bad-production-exit "Because there are circular variable bindings on the LHS.")
                   (setf dependencies d-vars)))
               
               ;(pprint dependencies)
               
               (dolist (c rhs) ;; also check the explicit binds on the RHS
                 (let ((target (production-statement-target c)))
                   (case (production-statement-op c)
                     (#\!
                      (case target
                        ((bind safe-bind)
                         (let ((var (first (production-statement-definition c))))
                           (if (gethash var var-table)
                               (bad-production-exit (format nil "Because variable ~s is bound more than once." var))
                             (setf (gethash var var-table) 'rhs-bind))
                           (awhen (find-variables (rest (production-statement-definition c)))
                                  (push (push var it) rhs-dependencies))))
                        ((mv-bind)
                         (let ((bind-vars (first (production-statement-definition c))))
                           (dolist (x bind-vars)
                             (if (gethash x var-table)
                                 (bad-production-exit (format nil "Because variable ~s is bound more than once." x))
                               (setf (gethash x var-table) 'rhs-bind))
                             (awhen (find-variables (rest (production-statement-definition c)))
                                    (push (push x it) rhs-dependencies))))))))))
               
               
               (dolist (x rhs-variables)
                 (unless (gethash x var-table)
                   (bad-production-exit (format nil "Variable ~s is not bound on the RHS." x))))
               
               ;; check for circularity 
               
               (let ((d-vars (mapcar (lambda (x) (list x)) rhs-variables)))
                 (dolist (x rhs-dependencies)
                   (awhen (assoc (car x) d-vars)
                          (rplacd it (append (cdr it) (cdr x)))))
                 (when (circular-references d-vars)
                   (bad-production-exit "Because there are circular variable bindings on the RHS.")))
               
                              
               ;;; iterate over the lhs here
               
               (dolist (cond lhs)
                 (let* ((buffer (production-statement-target cond))
                        (bi (cdr (assoc buffer (procedural-buffer-indices prod))))
                        (spec (production-statement-spec cond))
                        (definition (production-statement-definition cond)))
                   
                   (case (production-statement-op cond)
                     (#\=
                      (if (searchable-buffer buffer)
                          (let ((search-specs (list (make-cr-condition :type 'isa :buffer buffer :bi bi 
                                                        :value (cons (act-r-chunk-spec-filled-slots spec)
                                                                     (act-r-chunk-spec-empty-slots spec))))))
                            
                          (push-last (list 'buffer-search buffer) selection)
                          
                          
                          (dolist (slot-spec (act-r-chunk-spec-slots spec))
                            (let* ((value (act-r-slot-spec-value slot-spec))
                                   (slot (act-r-slot-spec-name slot-spec))
                                   (si (unless (chunk-spec-variable-p slot) (slot-name->index slot)))
                                   (variable (act-r-slot-spec-variable slot-spec))
                                   (modifier (act-r-slot-spec-modifier slot-spec)))
                              (flet ((new-condition (&key type value test result)
                                                    (make-cr-condition :buffer buffer 
                                                                       :bi bi
                                                                       :slot slot
                                                                       :si si
                                                                       :type type 
                                                                       :value value 
                                                                       :test test
                                                                       :result result))
                                     )
                                (if (null variable) ;; A constant
                                    (progn ;;put it onto the search-specs list
                                      (case modifier 
                                        (=
                                         (push-last (new-condition :type 'slot :value value) search-specs))
                                        
                                        (-
                                         (push-last (new-condition :type 'test-slot :value value :test 'safe-chunk-slot-equal :result nil) search-specs))
                                        
                                        (t
                                         ;; Explicitly this must be a number
                                         (push-last (new-condition :type 'test-slot :test 'number-test :result t) search-specs)
                                         
                                         ;; The specified test
                                         
                                         (push-last (new-condition :type 'test-slot :value (if (numberp value) value nil) 
                                                                   :test (case modifier 
                                                                           (> 'safe>)
                                                                           (< 'safe<)
                                                                           (>= 'safe>=)
                                                                           (<= 'safe<=))
                                                                   :result t) 
                                                    search-specs))))
                                  
                                  ;; it's got a variable somewhere...
                                  
                                  (cond (;; only the value is a variable and this is the binding
                                         (and (eq modifier '=)
                                              (eq variable :value) 
                                              (eq (gethash value var-table) 'search-slot)
                                              (find value unbound-vars))
                                         
                                         (setf unbound-vars (remove value unbound-vars))
                                         
                                         (push-last (new-condition :type 'bind-slot :value value) search-bind))
                                             
                                        (;;bound in a var slot which is ok since the slot had to be bound elsewhere     
                                        (and (eq modifier '=)
                                             (eq variable :both) 
                                             (eq (gethash value var-table) 'search-slot)
                                             (find value unbound-vars))
                                         
                                         ;; add a test for the slot existing to the constant tests since the 'isa' won't 
                                         ;; have the variablized slot in the required set
                                         
                                         (push-last (new-condition :type 'test-var-slot :value nil :test 'safe-chunk-slot-equal :result nil) search-specs)
                                         
                                         (setf unbound-vars (remove value unbound-vars))
                                         
                                         (push-last (new-condition :type 'bind-var-slot :value value) search-bind))
                                        
                                        (;; something else...
                                         t
                                         (let* ((type (if (eq variable :value) 'test-slot 'test-var-slot))
                                                (var-kind (gethash value var-table))
                                                (search (and var-kind (or (eq var-kind 'search) (eq var-kind 'search-slot))))
                                                (condition (new-condition :type type
                                                                          :value value
                                                                          :test (case modifier
                                                                                  ((= -) 'safe-chunk-slot-equal)
                                                                                  (> 'safe>)
                                                                                  (< 'safe<)
                                                                                  (>= 'safe>=)
                                                                                  (<= 'safe<=))
                                                                          :result (if (eq modifier '-) nil t))))
                                        
                                        ;; nothing implicit is known nor is a slot index available
                                        ;; just add a test to the appropriate list (based on whether it
                                        ;; requires the search to have completed)
                                        
                                        (if search 
                                            (push-last condition search-other)
                                          (push-last condition search-specs)))))))))
                            
                           (push-last (make-cr-condition :type 'search :buffer buffer :bi bi :value search-specs :test (intern (concatenate 'string "=" (symbol-name buffer)))) search)) 
                            
                        (progn                  
                          (push-last (make-cr-condition :type 'isa :buffer buffer :bi bi 
                                                        :value (cons (act-r-chunk-spec-filled-slots spec)
                                                                     (act-r-chunk-spec-empty-slots spec)))
                                     constants)
                          
                          (push-last (list 'buffer-read buffer) selection)
                          
                          
                          (dolist (slot-spec (act-r-chunk-spec-slots spec))
                            (let* ((value (act-r-slot-spec-value slot-spec))
                                   (slot (act-r-slot-spec-name slot-spec))
                                   (si (unless (chunk-spec-variable-p slot) (slot-name->index slot)))
                                   (variable (act-r-slot-spec-variable slot-spec))
                                   (modifier (act-r-slot-spec-modifier slot-spec))
                                   (other t))
                              (flet ((new-condition (&key type value test result)
                                                    (make-cr-condition :buffer buffer 
                                                                       :bi bi
                                                                       :slot slot
                                                                       :si si
                                                                       :type type 
                                                                       :value value 
                                                                       :test test
                                                                       :result result))
                                     (must-be (target result)
                                              (unless (find (list buffer slot) target :test 'equalp
                                                            :key (lambda (x) 
                                                                   (when (or (and (eq (cr-condition-type x) 'slot) ;; slot with a non-nil value
                                                                                  (cr-condition-value x))
                                                                             (and (eq (cr-condition-type x) 'test-slot) ;; test-slot with either inequality test or not equal nil
                                                                                  (or (not (eq (cr-condition-test x) 'safe-chunk-slot-equal))
                                                                                      (and (null (cr-condition-value x)) (null (cr-condition-result x)))
                                                                                      (and (cr-condition-value x) (cr-condition-result x)))))
                                                                     (list (cr-condition-buffer x) (cr-condition-slot x)))))
                                                
                                                (make-cr-condition :buffer buffer 
                                                                              :bi bi
                                                                              :slot slot
                                                                              :si si
                                                                              :type 'test-slot 
                                                                              :value nil 
                                                                              :test 'safe-chunk-slot-equal 
                                                                              :result result))))
                                (cond ((null variable) ;; A constant
                                       (setf other nil)
                                       (case modifier 
                                         (=
                                          ;; add the slot value for the "wide" test
                                          (push-last (new-condition :type 'slot :value value) constants)
                                          
                                          ;; note that the slot should be full or empty for the implicit tests if needed
                                          (awhen (must-be implicit (if value nil t)) (push-last it implicit))
                                          ;; implicit test for numbers to allow better filtering when any of the relative tests used
                                          (push (new-condition :type 'test-slot :test 'number-test :result (if (numberp value) t nil)) implicit))
                                         
                                         (-
                                          ;; negation test is on the others
                                          (push-last (new-condition :type 'test-slot :value value :test 'safe-chunk-slot-equal :result nil) others)
                                          
                                          ;; if it's '- <slot> nil' then that's got an implicit test the slot must be full
                                          (awhen (must-be implicit nil) (push it implicit)))
                                         
                                         (t
                                          ;; Explicitly this must be a number
                                          (push-last (new-condition :type 'test-slot :test 'number-test :result t) constants)
                                          
                                          ;; The specified test
                                          
                                          (push-last (new-condition :type 'test-slot :value (if (numberp value) value nil) 
                                                                    :test (case modifier 
                                                                            (> 'safe>)
                                                                            (< 'safe<)
                                                                            (>= 'safe>=)
                                                                            (<= 'safe<=))
                                                                    :result t) 
                                                     constants)
                                          
                                          ;; implicitly it must be full
                                          (awhen (must-be implicit nil) (push-last it implicit))
                                          
                                          ;; implicitly it's not the opposite test
                                          (push-last (new-condition :type 'test-slot :value (if (numberp value) value nil) 
                                                                    :test (case modifier 
                                                                            (> 'safe<=)
                                                                            (< 'safe>=)
                                                                            (>= 'safe<)
                                                                            (<= 'safe>))
                                                                    :result nil) 
                                                     implicit))))
                                      
                                      
                                      ((eq modifier '=) ;; a variable which may require a binding
                            
                                       (cond ((and (eq variable :value) ;; constant slot with a variable
                                                   (eq (gethash value var-table) 'fixed-slot) ;; that's bound that way
                                                   (find value unbound-vars)) 
                                              
                                              (setf unbound-vars (remove value unbound-vars))
                                              (setf other nil)
                                              
                                              (push-last (new-condition :type 'bind-slot :value value) vars)
                                              
                                              ;; if there isn't already an explicit test for it not being nil add one
                                              ;; put it on the implicit list since the buffer slot vectors will have
                                              ;; already handled that for the "real" testing 
                                              (awhen (must-be implicit nil) (push-last it implicit)))
                                             
                                             
                                             ((and (eq variable :both) ;; variable value in variable slot
                                                   (eq (gethash value var-table) 'var-slot) ;; and that's how it's bound
                                                   (find value unbound-vars))
                                              
                                              (setf unbound-vars (remove value unbound-vars))
                                              (setf other nil)
                                              
                                              (push-last (new-condition :type 'bind-var-slot :value value) var2s)))))
                                
                                (when other
                                  (if (and variable (not (eq variable :value))) ;; a variablized slot and maybe value
                                                                                ;; slot var can't be "search bound" is a restriction imposed above
                                      (let* ((var-kind (gethash value var-table))
                                             (search (and var-kind (or (eq var-kind 'search) (eq var-kind 'search-slot))))
                                             (condition (new-condition :type 'test-var-slot
                                                                       :value value
                                                                       :test (case modifier
                                                                               ((= -) 'safe-chunk-slot-equal)
                                                                               (> 'safe>)
                                                                               (< 'safe<)
                                                                               (>= 'safe>=)
                                                                               (<= 'safe<=))
                                                                       :result (if (eq modifier '-) nil t))))
                                        
                                        
                                        ;; nothing implicit is known nor is a slot index available
                                        ;; just add a test to the appropriate list (based on whether it
                                        ;; requires the search to have completed)
                                        
                                        (if search 
                                            (push-last condition search-other)
                                          (push-last condition others)))
                                    
                                    ;; it's a fixed slot but may be a varible value
                                    ;; treat it basically like a constant, but make sure
                                    ;; if it's a search bound var to put it on the search-other
                                    ;; list.
                                    
                                    (let* ((var-kind (gethash value var-table))
                                           (search (and var-kind (or (eq var-kind 'search) (eq var-kind 'search-slot)))))
                                      
                                               
                                      (case modifier 
                                        (=
                                         ;; add the slot value test
                                         (if search
                                             (push-last (new-condition :type 'test-slot :test 'safe-chunk-slot-equal :value value :result t) search-other)
                                           (push-last (new-condition :type 'test-slot :test 'safe-chunk-slot-equal :value value :result t) others))
                                         ;; note that the slot should be full or empty for the implicit tests if needed
                                         (awhen (must-be implicit (if value nil t)) (push-last it implicit)))
                                        
                                        (-
                                         ;; negation test is on the others
                                         (if search
                                             (push-last (new-condition :type 'test-slot :test 'safe-chunk-slot-equal :value value :result nil) search-other)
                                           (push-last (new-condition :type 'test-slot :test 'safe-chunk-slot-equal :value value :result nil) others)))
                                        
                                        (t
                                         ;; non empty
                                         (awhen (must-be implicit nil) (push-last it implicit))

                                         ;; The specified test
                                          
                                         (let ((condition (new-condition :type 'test-slot :value value 
                                                                         :test (case modifier 
                                                                                 (> 'safe>)
                                                                                 (< 'safe<)
                                                                                 (>= 'safe>=)
                                                                                 (<= 'safe<=))
                                                                         :result t)))
                                           (if search
                                               (push-last condition search-other)
                                             (push-last condition others))
                                           
                                           (awhen (must-be implicit nil) (push-last it implicit))))))))))))))
                     (#\?
                      (dolist (slot-spec (act-r-chunk-spec-slots spec))
                        
                        (let ((cr (make-cr-condition :type 'query :buffer buffer 
                                                     :slot (act-r-slot-spec-name slot-spec) 
                                                     :value (act-r-slot-spec-value slot-spec) 
                                                     :result (if (eq '= (act-r-slot-spec-modifier slot-spec)) t nil))))
                          
                          (if (act-r-slot-spec-variable slot-spec) 
                              (push-last cr search-other)
                            (push-last cr constants))))
                      
                      (push-last (list 'query-buffer buffer spec) selection))
                     
                     (t ;#\!
                      (case buffer ;; (really the command)
                        ((eval safe-eval)
                         (push-last (make-cr-condition :type 'eval :value (first definition)) search-other))
                        
                        ((bind safe-bind)
                         
                         (setf unbound-vars (remove (first definition) unbound-vars))
                         
                         (push-last (cons
                                     (assoc (first definition) dependencies)
                                     (make-cr-condition :type 'bind :value (first definition) :result (second definition)))
                                    binds))
                        (mv-bind
                         
                         (let ((bind-vars (first definition))
                               (depends nil))
                           
                           (dolist (x bind-vars)
                             (setf unbound-vars (remove x unbound-vars)))
                           
                           (dolist (x bind-vars)
                             (setf depends (append depends (cdr (assoc x dependencies)))))
                           
                           (push-last (cons
                                       (cons bind-vars (remove-duplicates depends))
                                       (make-cr-condition :type 'mv-bind :value bind-vars :result (second definition)))
                                      binds))))))))
               
               
               ;;; sort the binding operations based on dependency
               
               (setf binds (sort-for-bind-order binds))
               
               ;; split the binds if any need search vars...
               
               (let ((split (position-if (lambda (x) 
                                           (some (lambda (y)
                                                   (or (eq (gethash y var-table) 'search)
                                                       (eq (gethash y var-table) 'search-slot)))
                                                 (cdr x))) 
                                         binds :key 'car)))
                 
                 (if split
                     (progn
                       (setf search-bind (append search-bind (mapcar 'cdr (subseq binds split))))
                       (setf binds (mapcar 'cdr (subseq binds 0 split))))
                   (setf binds (mapcar 'cdr binds))))
               
               ;;; Now set the production slots
               
               (setf (production-constants production) constants)
               (setf (production-binds production) (append vars var2s binds))
               (setf (production-others production) others)
               (setf (production-searches production) search)
               (setf (production-search-binds production) search-bind)
               (setf (production-search-others production) search-other)
               
               (setf (production-selection-code production) selection)
               (setf (production-implicit production) implicit)))
           
           (let ((rhs-binds nil))
             (dolist (action rhs)
               (let ((op (production-statement-op action))
                     (target (production-statement-target action))
                     (definition (production-statement-definition action))
                     (spec (production-statement-spec action)))
                 
                 (case op
                   (#\=
                    (cond ((null definition)
                           ;; That's a dummy action to keep the
                           ;; buffer alive
                           )
                          ((= (length definition) 1) ;; it's a chunk so it won't need to extend
                           (push-last 
                              (lambda () 
                                (schedule-mod-buffer-chunk target (define-chunk-spec-fct 
                                                                      (replace-variables definition (production-bindings production)))
                                                           0
                                                           :time-in-ms t
                                                           :module 'procedural
                                                           :priority 100
                                                           :output (procedural-rhst prod)))
                              (production-actions production)))
                          (t
                           (push-last 
                            (lambda () 
                              (multiple-value-bind (spec extended)
                                  (instantiate-chunk-spec spec (production-bindings production))
                                (schedule-mod-buffer-chunk target spec 0
                                                           :time-in-ms t
                                                           :module 'procedural
                                                           :priority 100
                                                           :output (procedural-rhst prod))
                                (when extended
                                  (schedule-event-now 'extend-buffer-chunk
                                                      :module 'procedural
                                                      :priority 101
                                                      :params (list target)
                                                      :output (procedural-rhst prod)))))
                            (production-actions production)))))
                   (#\*
                    (cond ((null definition)
                           ;; not really allowed, but do these even get through?
                           )
                          ((= (length definition) 1)
                           (push-last 
                            (lambda () 
                              (push-last (multiple-value-bind (event tag)
                                             (schedule-module-mod-request target 
                                                                          (define-chunk-spec-fct 
                                                                              (replace-variables definition (production-bindings production)))
                                                                          0
                                                                          :time-in-ms t
                                                                          :module 'procedural
                                                                          :priority 60
                                                                          :output (procedural-rhst prod)
                                                                          :track t)
                                           (declare (ignore event))
                                           tag)
                              (production-requested-actions (production-name production))))
                            (production-actions production)))
                          (t
                           (push-last 
                            (lambda () 
                              (multiple-value-bind (spec extended)
                                  (instantiate-chunk-spec spec (production-bindings production))
                                (push-last (multiple-value-bind (event tag)
                                               (schedule-module-mod-request target spec 0
                                                                            :time-in-ms t 
                                                                            :module 'procedural
                                                                            :priority 60
                                                                            :output (procedural-rhst prod)
                                                                            :track t)
                                             (declare (ignore event))
                                             tag)
                                      (production-requested-actions (production-name production)))
                                (when extended
                                  (schedule-event-now 'extend-buffer-chunk
                                                      :module 'procedural
                                                      :priority 61
                                                      :params (list target)
                                                      :output (procedural-rhst prod)))))
                            (production-actions production)))))
                   (#\@
                    (push-last 
                     (lambda () 
                       (schedule-overwrite-buffer-chunk target
                                                        (replace-variables (first definition) (production-bindings production))
                                                        0
                                                        :time-in-ms t
                                                        :module 'procedural
                                                        :priority 90
                                                        :output (procedural-rhst prod)))
                     (production-actions production)))
                   (#\-
                    (push-last 
                     (lambda () 
                       (schedule-clear-buffer target
                                              0
                                              :time-in-ms t
                                              :module 'procedural
                                              :priority 10
                                              :output (when (procedural-rhst prod) 'medium)))
                     (production-actions production)))
                   
                   (#\+  ;; The "direct" requests need to schedule an event to schedule the event
                         ;; instead of scheduling it directly because it has to delay the evaluation
                         ;; of the chunk->chunk-spec expansion since the production modifications
                         ;; need to happen first and they have a priority of 100.
                         ;; Is there a better way to handle that?
                    (cond ((null spec) ;; request specifying only a chunk/variable
                           (push-last 
                            (lambda () 
                              (schedule-event-now (lambda () 
                                                    (push-last (multiple-value-bind (event tag)
                                                                   (schedule-module-request target
                                                                                            (define-chunk-spec-fct 
                                                                                                (replace-variables definition (production-bindings production)))
                                                                                            0
                                                                                            :time-in-ms t
                                                                                            :module 'procedural
                                                                                            :priority 50
                                                                                            :output (procedural-rhst prod)
                                                                                            :track t)
                                                                 (declare (ignore event))
                                                                 tag)
                                                          (production-requested-actions (production-name production))))
                                                  :module 'procedural
                                                  :priority 99
                                                  :output nil))
                            (production-actions production)))
                          ((= (length definition) 1) ;; a request specifying a chunk and params
                           (let ((chunk (first (first definition))))
                             (push-last 
                              (lambda () 
                                (schedule-event-now (lambda () 
                                                      (push-last (multiple-value-bind (event tag)
                                                                     (schedule-module-request target
                                                                                              (merge-chunk-specs 
                                                                                               (chunk-name-to-chunk-spec (replace-variables chunk (production-bindings production)))
                                                                                               spec)
                                                                                              0
                                                                                              :time-in-ms t
                                                                                              :module 'procedural
                                                                                              :priority 50
                                                                                              :output (procedural-rhst prod)
                                                                                              :track t)
                                                                   (declare (ignore event))
                                                                   tag)
                                                            (production-requested-actions (production-name production))))
                                                    :module 'procedural
                                                    :priority 99
                                                    :output nil))
                              (production-actions production))))
                          
                          (t  ;; a full request 
                           (push-last 
                            (lambda ()
                              (push-last (multiple-value-bind (event tag)
                                             (schedule-module-request target
                                                                      (instantiate-chunk-spec spec (production-bindings production))
                                                                      0
                                                                      :time-in-ms t
                                                                      :module 'procedural
                                                                      :priority 50
                                                                      :output (procedural-rhst prod)
                                                                      :track t)
                                           (declare (ignore event))
                                           tag)
                                    (production-requested-actions (production-name production))))
                            (production-actions production)))))
                   
                   (t ;; #\!
                    (case target
                      (stop
                       (push-last 
                        (lambda ()
                          (schedule-break-relative 0 :priority :min :details "Stopped by !stop!"))
                        (production-actions production)))
                      
                      ((eval safe-eval)
                       (push-last 
                        (cons
                         (cons nil (find-variables definition))
                         (lambda ()
                           (eval (replace-variables-for-eval (first definition) (production-bindings production)))))
                        rhs-binds))
                      
                      (output
                       (push-last 
                        (cons
                         (cons nil (find-variables definition))
                         (lambda ()
                           (print-production-output (first definition) (replace-variables definition (production-bindings production)))))
                        rhs-binds))
                      
                      
                      ((bind safe-bind)
                       (push-last 
                        (cons
                         (cons (first definition) (find-variables (rest definition)))
                         (lambda ()
                           (bind-variable (first definition)
                                          (eval (replace-variables-for-eval (second definition) (production-bindings production)))
                                          production)))
                        rhs-binds))
                      
                      ((mv-bind)
                       (let ((bind-vars (first definition)))
                         (push-last 
                          (cons
                           (cons bind-vars (find-variables (rest definition)))
                           (lambda ()
                             (let ((vals (multiple-value-list 
                                          (eval (replace-variables-for-eval (second definition) (production-bindings production))))))
                               (dolist (x bind-vars)
                                 (bind-variable x (if vals (pop vals) nil) production)))))
                          rhs-binds))))))))
             
             
             ;; just get the dependencies set right -- I know there aren't any circularities
             (circular-references (mapcar 'car rhs-binds))
             
             (let ((original-binds (copy-list rhs-binds)))
               
               (setf rhs-binds (sort-for-bind-order rhs-binds))
               (unless (equalp rhs-binds original-binds)
                 (let ((*error-output* original-error-stream))
                   (print-warning "RHS !bind!, !eval!, and/or !output! actions of production ~s had to be reordered because of interdependencies of variables." 
                                  (production-name production))
                   (print-warning "If those actions have side effects check the actions carefully to ensure proper operation." (production-name production)))))
             
             ;; make sure the bindings happen first
             (setf (production-actions production) (append (mapcar 'cdr rhs-binds) (production-actions production))))
           
           ;;; Add the implicit clears for strict harvesting
           (dolist (y lhs)
             (when (or (eql #\= (production-statement-op y))
                       ;; test for the buffer failure queries
                       ;; don't worry about doubling up on this
                       ;; since a buffer can't both have a chunk and
                       ;; a failure so even if it's done it'll never
                       ;; actually happen.
                       
                       (and (eql #\? (production-statement-op y))
                            (find (list '= 'buffer 'failure)
                                  (chunk-spec-slot-spec (production-statement-spec y) 'buffer)
                                  :test 'equalp)))
               (let ((target (production-statement-target y)))
                 (unless (or (find target (procedural-unharvested-buffers prod))
                             (find target rhs :key 'production-statement-target))
                   (push-last 
                    (lambda () 
                      (schedule-clear-buffer target
                                             0
                                             :time-in-ms t
                                             :module 'procedural
                                             :priority 10
                                             :output (when (procedural-rhst prod) 'medium)))
                    (production-actions production))))))
           
           ;;; Add the implicit clears for requests
           (dolist (y rhs)
             (let ((target (production-statement-target y)))
               (when (and (eql #\+ (production-statement-op y)) 
                          (not (find-if (lambda (x)
                                          (and (eql #\- (production-statement-op x))
                                               (eql target (production-statement-target x))))
                                        rhs)))
                 (push-last 
                  (lambda () 
                    (schedule-clear-buffer target
                                           0
                                           :time-in-ms t
                                           :module 'procedural
                                           :priority 10
                                           :output (when (procedural-rhst prod) 'medium)))
                  (production-actions production)))))
           
           ;;; Parse LHS for unknown chunks
           
           (dolist (x lhs)
             (when (or (eql #\= (production-statement-op x))
                       (eql #\? (production-statement-op x)))
               (dolist (slot (act-r-chunk-spec-slots (production-statement-spec x)))
                 (let ((value (act-r-slot-spec-value slot)))
                   (unless (or (chunk-spec-variable-p value)
                               (chunk-p-fct value)
                               (stringp value)
                               (listp value)
                               (numberp value)
                               (keywordp value)
                               (eq t value))
                     (create-undefined-chunk value))))))
           
           ;;; Parse RHS for unknown chunks
           ;;; Only in modifications, overwrites, and indirect requests (+ or *)
           ;;; anything in a full + or * is up to the module to figure out
           ;;; which allows for things like &variable in visual-location
           ;;; requests 
           
           (dolist (x rhs)
             (let ((op (production-statement-op x))
                   (definition (production-statement-definition x))
                   (spec (production-statement-spec x)))
               (cond ((or (eql #\= op)
                          (eql #\@ op))
                      (cond ((= (length definition) 1)
                             (unless (or (chunk-spec-variable-p (first definition))
                                         (chunk-p-fct (first definition)))
                               (create-undefined-chunk (first definition))))
                            (spec
                             (dolist (slot (act-r-chunk-spec-slots spec))
                               (let ((val (act-r-slot-spec-value slot)))
                                 
                                 (unless (or (chunk-spec-variable-p val)
                                             (chunk-p-fct val)
                                             (numberp val)
                                             (stringp val)
                                             (listp val)
                                             (keywordp val)
                                             (eq t val))
                                   (when (symbolp val)
                                     (create-undefined-chunk val))))))))
                     ((and (or (eql #\+ op)
                               (eql #\* op))
                           (= (length definition) 1))
                      (if (atom (first definition))
                          (unless (or (chunk-spec-variable-p (first definition))
                                      (chunk-p-fct (first definition)))
                            (create-undefined-chunk (first definition)))
                        (let ((val (first (first definition))))
                          (unless (or (chunk-spec-variable-p val)
                                      (chunk-p-fct val))
                            (create-undefined-chunk val))))))))
           
           ;; Send any module which needs it a warning after conflict resolution
           ;; only for + requests right now, but should that change to allow * too?
           
           (dolist (x rhs)
             (let ((op (production-statement-op x))
                   (target (production-statement-target x))
                   (definition (production-statement-definition x))
                   (spec (production-statement-spec x)))
               
               (when (and (eql #\+ op)
                          (require-module-warning? target))
                 
                 (cond ((null spec) ;; request specifying only a chunk/variable
                         (push-last 
                          (lambda () 
                            (module-warning target (define-chunk-spec-fct (replace-variables definition (production-bindings production)))))
                          (production-conflict-code production)))
                        ((= (length definition) 1) 
                         (let ((chunk (first (first definition))))
                           (push-last 
                            (lambda () 
                              (module-warning target (merge-chunk-specs (chunk-name-to-chunk-spec (replace-variables chunk (production-bindings production)))
                                                                        spec)))
                            (production-conflict-code production))))
                       (t  ;; a full request
                         (push-last 
                          (lambda ()
                            (module-warning target (instantiate-chunk-spec spec (production-bindings production))))
                          (production-conflict-code production)))))))
           
           ;; warn if it exists
           (awhen (get-production-internal (production-name production) prod)
                  (let ((*error-output* original-error-stream))
                    (print-warning "Production ~S already exists and it is being redefined." (production-name production)))
                  (remove-production it prod))
           
           (add-production production prod) 
           
           ;; If a new production is created and conflict resolution 
           ;; is waiting put it back on the queue
           
           (un-delay-conflict-resolution)
           
           ;; record which buffers are used for searches 
           
           (dolist (x searched-buffers)
             (pushnew x (procedural-used-search-buffers prod)))
           
           ;; Don't like this but when the one-stream-hack is turned
           ;; on every warning goes to *error-output* which means
           ;; that they're captured by the special stream for 
           ;; the production parsing.  Therefore, if there's a
           ;; warning which didn't prevent the production from
           ;; being created we still need to output those to the
           ;; original *error-stream* since otherwise they are lost.
           
           (when *one-stream-hack* 
             (let ((*error-output* original-error-stream)
                   (warn-text (get-output-stream-string error-string-stream)))
               (when (> (length warn-text) 2) 
                 (let ((starts 
                        (do* ((start -1 end)
                              (warn-indices nil)
                              (end (search "#|" warn-text :start2 (1+ start))
                                   (search "#|" warn-text :start2 (1+ start))))
                             ((null end) warn-indices)
                          (push-last end warn-indices)))
                       (ends 
                        (do* ((start 0 end)
                              (warn-indices nil)
                              (end (search "|#" warn-text :start2 (1+ start))
                                   (search "|#" warn-text :start2 (1+ start))))
                             ((null end) warn-indices)
                          (push-last end warn-indices))))
                   (if (= (length starts) (length ends))
                       (dolist (x (pairlis starts ends))
                         (format *error-output* "~a~%" (subseq warn-text (car x) (+ 2 (cdr x)))))
                     (progn 
                       (format *error-output* warn-text)))))))
           
           (when (and (procedural-style-warnings prod) (procedural-style-check prod)
                      (not (procedural-delay-tree prod)))
             (check-production-for-style prod production)
             (check-between-production-style prod))
           
           (close error-string-stream)
           
           (production-name production))
         (bad-production-exit "Production is missing the ==> separator.")))))


(defun parse-conditions (procedural p-name definition buffers)
  (aif (gethash definition (procedural-condition-parse-table procedural))
       (progn
         (when (cdr it)
           (dolist (x (cdr it))
             (if (listp x)
                 (model-warning "Production ~s tests invalid slot ~s for type ~s." p-name (third x) (second x))
               (unless (valid-slot-name x)
                 (model-warning "Production ~s uses previously undefined slot ~s." p-name x)
                 (extend-possible-slots x nil)))))
         
         (car it))
       (flet ((bad-condition-exit (string)
                                  (print-warning string)
                                  (return-from parse-conditions :error)))

         (let ((segments (segment-production definition buffers))
               (conditions nil)
               (new-slots nil))
           (if (eq segments :error)
               (bad-condition-exit "First item on LHS is not a valid command")
             (dolist (seg segments)
               (let ((statement (parse-statement seg)))
               
               (case (production-statement-op statement) 
                 (#\=
                  (cond ((find statement conditions :test (lambda (x y) 
                                                           (and (eql (production-statement-op x) (production-statement-op y))
                                                                (eql (production-statement-target x) (production-statement-target y)))))
                         (bad-condition-exit (format nil "Only one ~s condition allowed." (car seg))))
                                                                         
                        ;; Don't allow a "direct" chunk comparison to the buffer -- Should that be allowed now?
                        ((= (length (production-statement-definition statement)) 1)
                         (bad-condition-exit (format nil "Insufficient conditions specified in ~s" seg)))
                        ((= (length (production-statement-definition statement)) 0)
                         (setf (production-statement-definition statement) (list 'isa 'chunk))
                         (setf (production-statement-spec statement) (define-chunk-spec isa chunk))
                         (push-last statement conditions))
                        (t
                         (multiple-value-bind (spec slots)
                             (define-chunk-spec-fct (production-statement-definition statement) t nil)
                           (if spec
                               (if (= 0 (act-r-chunk-spec-request-param-slots spec))
                                   (progn
                                     (when slots
                                       (let ((extended-slots (remove-if 'listp slots))
                                             (invalid-type-slots (remove-if-not 'listp slots)))
                                         (when extended-slots
                                           (model-warning "Production ~s uses previously undefined slots ~s." p-name extended-slots))
                                         (dolist (x invalid-type-slots)
                                           (model-warning "Production ~s tests invalid slot ~s for type ~s." p-name (third x) (second x))))
                                       
                                       (dolist (x slots) (pushnew x new-slots)))
                                     (setf (production-statement-spec statement) spec)
                                     (push-last statement conditions))
                                 (bad-condition-exit (format nil "Request parameters not allowed in production conditions: ~s" seg)))
                             (bad-condition-exit (format nil "Invalid syntax in ~s condition." seg)))))))
                 (#\?
                  (cond ((find statement conditions :test (lambda (x y) 
                                                           (and (eql (production-statement-op x) (production-statement-op y))
                                                                (eql (production-statement-target x) (production-statement-target y)))))
                         (bad-condition-exit (format nil "Only one ~s condition allowed." (car seg))))
                        
                        ;; query must be of the form {mod} query value
                        ((< (length (production-statement-definition statement)) 2)
                         (bad-condition-exit (format nil "Insufficient conditions specified in ~s" seg)))
                        
                        (t
                         (aif (define-query-spec-fct (production-statement-definition statement))
                              (cond ((not (zerop (act-r-chunk-spec-relative-slots it)))
                                     (bad-condition-exit (format nil "Invalid buffer query uses modifiers other than = or -: ~s" seg)))
                                    ((act-r-chunk-spec-slot-vars it)
                                     (bad-condition-exit (format nil "Invalid buffer query uses variables in query names: ~s" seg)))
                                    ((not (every (lambda (x)
                                                  (member (act-r-slot-spec-name x) (valid-buffer-queries (production-statement-target statement))))
                                                 (act-r-chunk-spec-slots it)))
                                     (print-warning "Production ~s makes invalid query of buffer ~S.  Only available queries are ~S." p-name
                                                    (production-statement-target statement) (act-r-buffer-queries (production-statement-target statement))))
                                    (t
                                     (setf (production-statement-spec statement) it)
                                     (push-last statement conditions)))
                              (bad-condition-exit (format nil "Invalid syntax in ~s condition." seg))))))
                 (#\! 
                  (case (production-statement-target statement)
                    ((bind safe-bind)
                     (if (and (= (length (production-statement-definition statement)) 2)
                              (chunk-spec-variable-p (first (production-statement-definition statement))))
                         (push-last statement conditions)
                       (bad-condition-exit (format nil "Invalid bind command: ~s" seg))))
                    ((mv-bind)
                     (if (and (= (length (production-statement-definition statement)) 2)
                              (listp (first (production-statement-definition statement)))
                              (every 'chunk-spec-variable-p (first (production-statement-definition statement))))
                         (push-last statement conditions)
                       (bad-condition-exit (format nil "Invalid mv-bind command: ~s" seg))))
                    ((eval safe-eval)
                     (if (= (length (production-statement-definition statement)) 1)
                         (push-last statement conditions)
                       (bad-condition-exit (format nil "Invalid eval command: ~s" seg))))
                    (t
                     (bad-condition-exit (format nil "Invalid condition: ~s" seg)))))
                 (t
                  (bad-condition-exit (format nil "Invalid condition: ~s" seg)))))))
             
             (car (setf (gethash definition (procedural-condition-parse-table procedural)) (cons conditions new-slots)))))))



(defun parse-actions (procedural p-name definition conditions buffers)
  (aif (gethash (cons definition conditions) (procedural-action-parse-table procedural))
       (progn
         (when (cdr it)
           (dolist (x (cdr it))
             (if (listp x)
                 (model-warning "Production ~s action has invalid slot ~s for type ~s." p-name (third x) (second x))
               (unless (valid-slot-name x)
                 (model-warning "Production ~s uses previously undefined slot ~s." p-name x)
                 (extend-possible-slots x nil)))))
         
         (car it))
       (flet ((bad-action-exit (string)
                               (print-warning string)
                               (return-from parse-actions :error)))

         (let ((segments (segment-production definition buffers))
               (actions nil)
               (new-slots nil))
           (if (eq segments :error)
               (bad-action-exit "First item on RHS is not a valid command")
             (dolist (seg segments)
               (let ((statement (parse-statement seg)))
                  
                 (case (production-statement-op statement) 
                   (#\-
                    (if (= (length (production-statement-definition statement)) 0)
                        (push-last statement actions)
                      (bad-action-exit (format nil "Invalid - buffer command: ~s" seg))))
                   
                   (#\@ ;; now this is the specific overwrite action instead of =buffer> <chunk>
                    (if (= (length (production-statement-definition statement)) 1)
                        (let ((chunk? (first (production-statement-definition statement))))
                          (if (or (chunk-spec-variable-p chunk?) (chunk-p-fct chunk?))
                              (push-last statement actions)
                            (bad-action-exit (format nil "Invalid overwrite action: ~s.  Must provide a variable or chunk name." seg))))
                      (bad-action-exit (format nil "Invalid overwrite action: ~s.  Must have a single parameter." seg))))

                   ((#\= #\*)  ;; production modifications and module modifications 
                               ;; have the same requirements and allow the same syntax
                               ;; except that an empty * is not allowed only an empty
                               ;; = can be used to avoid strict harvesting
                    
                    ;; must test buffer on LHS
                    (unless (or (find-if (lambda (x)
                                           (and (eql (production-statement-op x) #\=)
                                                (eql (production-statement-target x) (production-statement-target statement))))
                                         conditions)
                                
                                ;; a buffer failure query allows the empty modifications
                                
                                (and (= (length (production-statement-definition statement)) 0)
                                     (eql (production-statement-op statement) #\=)
                                     
                                     (find-if (lambda (x)
                                                (and (eql (production-statement-op x) #\?)
                                                     (eql (production-statement-target x) (production-statement-target statement))
                                                     (find (list '= 'buffer 'failure)
                                                           (chunk-spec-slot-spec (production-statement-spec x) 'buffer)
                                                           :test 'equalp)))
                                              conditions)))
                      (bad-action-exit (format nil "Buffer modification action for untested buffer ~s." seg)))
                    
                    (cond ((= (length (production-statement-definition statement)) 0) ;; empty =buffer operation
                           (if (eql #\= (production-statement-op statement))
                               (progn
                                 (setf (production-statement-spec statement) (define-chunk-spec isa chunk))
                                 (push-last statement actions))
                             (bad-action-exit (format nil "Modification request ~s requires some modification." (car seg)))))
                          ;; this is not an overwrite anymore!
                          ((= (length (production-statement-definition statement)) 1)
                           (let ((chunk? (first (production-statement-definition statement))))
                             (if (or (chunk-spec-variable-p chunk?) (chunk-p-fct chunk?))
                                 (push-last statement actions)
                               (bad-action-exit (format nil "Invalid single operator modification action: ~s.  Must be a variable or chunk name." seg)))))
                          (t ;; it should be a full modification
                           ;; now that's allowed to have an ISA !!!
                           ;; and there's no "carry over" of the LHS isa to the RHS validity testing
                           
                           (multiple-value-bind (spec slots)
                             (define-chunk-spec-fct (production-statement-definition statement) t nil)
                           (if spec
                               (if (or (zerop (act-r-chunk-spec-request-param-slots spec))
                                        (and (eql #\* (production-statement-op statement))
                                             (valid-request-params-for-buffer (production-statement-target statement) (act-r-chunk-spec-request-param-slots spec))))
                                    (progn 
                                      (when slots
                                       (let ((extended-slots (remove-if 'listp slots))
                                             (invalid-type-slots (remove-if-not 'listp slots)))
                                         (when extended-slots
                                           (model-warning "Production ~s uses previously undefined slots ~s." p-name extended-slots))
                                         (dolist (x invalid-type-slots)
                                           (model-warning "Production ~s action has invalid slot ~s for type ~s." p-name (third x) (second x))))
                                       
                                       (dolist (x slots) (pushnew x new-slots)))
                                      
                                      (setf (production-statement-spec statement) spec)
                                      (push-last statement actions))
                                  (bad-action-exit (format nil "Invalid request parameters in modification action ~s." seg)))
                               (bad-action-exit (format nil "Invalid buffer modification ~s." seg)))))))
                   (#\+
                    (cond ((= (length (production-statement-definition statement)) 0)
                           ; Don't error out now -- just make it "isa chunk" because production compilation
                           ; can result in requests which have no specified slots and they should still result
                           ; in a production being defined.
                           ;(bad-action-exit (format nil "Buffer request ~s requires some parameters." (car seg)))
                           (setf (production-statement-spec statement) (define-chunk-spec isa chunk))
                           (push-last statement actions))
                          ((= (length (production-statement-definition statement)) 1)
                           (let ((chunk? (first (production-statement-definition statement))))
                             (if (or (chunk-spec-variable-p chunk?) (chunk-p-fct chunk?))
                                 (push-last statement actions)
                               (if (listp chunk?)
                                   (let ((c2? (first chunk?))
                                         (request-spec (define-chunk-spec-fct (cdr chunk?) nil)))
                                     (if (and (or (chunk-spec-variable-p c2?) (chunk-p-fct c2?))
                                              request-spec
                                              (valid-request-params-for-buffer (production-statement-target statement) (act-r-chunk-spec-request-param-slots request-spec)))
                                         (progn 
                                           (setf (production-statement-spec statement) request-spec)
                                           (push-last statement actions))
                                       (bad-action-exit (format nil "Invalid indirect request list: ~s.  Must be a list with a chunk or variable and a valid request parameters settings." seg))))
                                 (bad-action-exit (format nil "Invalid single operator modification action: ~s.  Must be a variable or chunk name." seg))))))
                          (t ;; a full request which may or may not have an isa
                           
                           (multiple-value-bind (spec slots)
                               (define-chunk-spec-fct (production-statement-definition statement) t nil)
                             (if spec
                                 (if (or (zerop (act-r-chunk-spec-request-param-slots spec))
                                         (valid-request-params-for-buffer (production-statement-target statement) (act-r-chunk-spec-request-param-slots spec)))
                                     (progn 
                                       (when slots
                                       (let ((extended-slots (remove-if 'listp slots))
                                             (invalid-type-slots (remove-if-not 'listp slots)))
                                         (when extended-slots
                                           (model-warning "Production ~s uses previously undefined slots ~s." p-name extended-slots))
                                         (dolist (x invalid-type-slots)
                                           (model-warning "Production ~s action has invalid slot ~s for type ~s." p-name (third x) (second x))))
                                       
                                         (dolist (x slots) (pushnew x new-slots)))
                                       
                                       (setf (production-statement-spec statement) spec)
                                       (push-last statement actions))
                                   (bad-action-exit (format nil "Invalid request parameters in request ~s." seg)))
                               (bad-action-exit (format nil "Invalid buffer request ~s." seg)))))))
                   (#\!
                    (case (production-statement-target statement)
                      ((bind safe-bind)
                       (if (and (= (length (production-statement-definition statement)) 2)
                                (chunk-spec-variable-p (first (production-statement-definition statement))))
                           (push-last statement actions)
                         (bad-action-exit (format nil "Invalid bind command: ~s" seg))))
                      ((mv-bind)
                       (if (and (= (length (production-statement-definition statement)) 2)
                                (listp (first (production-statement-definition statement)))
                                (every 'chunk-spec-variable-p (first (production-statement-definition statement))))
                           (push-last statement actions)
                         (bad-action-exit (format nil "Invalid mv-bind command: ~s" seg))))
                      ((eval safe-eval output)
                       (if (= (length (production-statement-definition statement)) 1)
                           (push-last statement actions)
                         (bad-action-exit (format nil "Invalid ~s command: ~s" (production-statement-target statement) (cdr seg)))))
                      (stop
                       (if (zerop (length (production-statement-definition statement)))
                           (push-last statement actions)
                         (bad-action-exit (format nil "Invalid stop command: ~s" seg))))
                      (t
                       (bad-action-exit (format nil "Invalid action: ~s" seg)))))
                   (bad-action-exit (format nil "Invalid command on RHS: ~S" seg))))))
                              
           (car (setf (gethash (cons definition conditions) (procedural-action-parse-table procedural)) (cons actions new-slots)))))))


(defun sort-for-binding (condition-list) ;; eval and binds before buffers and queries, regular buffers before searchable buffers
  (stable-sort (copy-tree condition-list) 
               (lambda (x y) 
                 (or (and (eq (production-statement-op x) #\!) (not (eq (production-statement-op y) #\!)))  
                     (and (eq (production-statement-op x) #\=) (not (searchable-buffer (production-statement-target x))) 
                          (eq (production-statement-op y) #\=) (searchable-buffer (production-statement-target y)))))))


(defun number-test (val ignore)
  (declare (ignore ignore))
  (if (numberp val) t nil))


(defun sort-for-bind-order (ordering)
  (let ((result nil))
    (dolist (x ordering result)
      (aif (position-if (lambda (y) (find (caar x) (cdar y))) result)
           (setf result (splice-into-position-des result it x))
           (push-last x result)))))


(defun print-production-output (original data)
  (let* ((vals (car data))
         (text (cond ((atom vals)
                      (format nil "~s" vals))
                     ((stringp (car original))
                      (format nil "~?" (car vals) (cdr vals)))
                     (t
                      (format nil "~{~S ~}" vals)))))
    (add-buffer-trace-notes 'production text)
    (model-output text)))
    

(defun find-variables (arg)
  (cond ((listp arg) (remove-duplicates (mapcan 'find-variables arg)))
        ((chunk-spec-variable-p arg) (list arg))
        (t nil)))


(defun replace-variables-for-eval (arg bindings)
  (cond ((listp arg) 
         (cond ((listp (car arg))
                ;; If the head is a list just parse the whole thing
                (mapcar (lambda (x)
                          (replace-variables-for-eval x bindings))
                  arg))
               ((eq (car arg) 'quote)
                ;; If it's already quoted don't requote
                (mapcar (lambda (x)
                          (replace-variables x bindings))
                  arg))
               (t
                (cons (replace-variables (car arg) bindings)
                      (mapcar (lambda (x)
                                (replace-variables-for-eval x bindings))
                        (cdr arg))))))
        
        ((chunk-spec-variable-p arg) 
         (aif (assoc arg bindings)
              (if (or (chunk-spec-variable-p (cdr it))
                      (stringp (cdr it)))
                  (cdr it)
                (list 'quote (cdr it)))
              arg))
        (t arg)))


(defun segment-production (definition buffers)
  (when definition
    (do ((res nil (push-last where res))
         (where (position-if (lambda (x) (command-symbol-p x buffers)) definition)
                (position-if (lambda (x) (command-symbol-p x buffers)) definition :start (1+ where))))
        ((null where)
         (if (and res (zerop (car res)))
             (mapcar (lambda (start end)
                       (subseq definition start end))
               res (push-last nil (cdr res)))
           :error)))))

(defun command-symbol-p (cmd buffers 
                        &key (operators '(#\? #\= #\- #\+ #\* #\@))
                             (commands '(eval safe-eval bind safe-bind mv-bind stop output)))
  (and (symbolp cmd)
       (let* ((name (symbol-name cmd))
              (len (length name)))
         (and (> len 2)
              (let ((cmd-name (intern (subseq name 1 (1- len))))
                    (first-char (aref name 0))
                    (last-char (aref name (1- len))))
                (or (and (eql first-char #\!) 
                         (eql last-char #\!)
                         (find cmd-name commands))
                    (and (eql last-char #\>)
                         (find first-char operators)
                         (find cmd-name buffers))))))))


;; know it's valid since it has already passed segment-production
;; but might be something like =goal> =var so don't want to make
;; the spec here since that's not a valid spec but fine for the RHS action

(defun parse-statement (segment)  
  (let ((name (symbol-name (car segment))))
    (make-production-statement :op (aref name 0) 
                               :target (intern (subseq name 1 (1- (length name))))
                               :definition (cdr segment))))


;; Dummy function for showing in the trace from an = or * action

(defun extend-buffer-chunk (buffer-name)
  "just a dummy function now"
  (declare (ignore buffer-name)))

(provide "PRODUCTION-PARSING")

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
