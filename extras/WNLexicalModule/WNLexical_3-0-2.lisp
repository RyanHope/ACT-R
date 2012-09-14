;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filename    : WNLexical_3-0-2.lisp
;;; Version     : 3.0.2
;;; 
;;; Description : ACT-R/WNLexical
;;;
;;;               WNLexical is an implementation of WordNet for the Act-r  
;;;               cognitive architecture. WNLexical is an Act-r module and
;;;               was developped from the prolog files distributed with 
;;;               WordNet Release 3.0. 
;;;               The first digits of the version number match the WordNet version number.
;;;               The last digit distinguishes WNLexical versions. 
;;;
;;; License     : **** WNLexical license ****
;;;
;;;               Copyright   : (C) 2005, Bruno Emond,
;;;               Institute for Information Technology,
;;;               National Research Council Canada. 
;;;
;;;               This library is free software; you can redistribute it and/or
;;;               modify it under the terms of the Preamble to the GNU Lesser 
;;;               General Public License as published by Franz Incorporated, 
;;;               Berkeley, CA 94704, and the GNU Lesser General Public License
;;;               as published by the Free Software Foundation; either
;;;               version 2.1 of the License.
;;;
;;;               This library is distributed in the hope that it will be useful,
;;;               but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;               MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;               Lesser General Public License for more details.
;;;
;;;               You should have received a copy of the Preamble to the GNU Lesser 
;;;               General Public License and the GNU Lesser General Public
;;;               License along with this library; if not, write to the Free Software
;;;               Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;;
;;;               The Preamble to the GNU Lesser General Public License and the
;;;               GNU Lesser General Public License is located in the following 
;;;               directory relative to the location of this file:
;;;               ../WNLexicalData/LLGPL.txt
;;;               ../WNLexicalData/LGPL.txt
;;;              
;;;
;;; License     : **** WordNet Release 3.0 license ****
;;;
;;;              WordNet Release 3.0
;;;              
;;;              This software and database is being provided to you, the LICENSEE, by  
;;;              Princeton University under the following license.  By obtaining, using  
;;;              and/or copying this software and database, you agree that you have  
;;;              read, understood, and will comply with these terms and conditions.:  
;;;                
;;;              Permission to use, copy, modify and distribute this software and  
;;;              database and its documentation for any purpose and without fee or  
;;;              royalty is hereby granted, provided that you agree to comply with  
;;;              the following copyright notice and statements, including the disclaimer,  
;;;              and that the same appear on ALL copies of the software, database and  
;;;              documentation, including modifications that you make for internal  
;;;              use or for distribution.  
;;;                
;;;              WordNet 3.0 Copyright 2006 by Princeton University.  All rights reserved.  
;;;                
;;;              THIS SOFTWARE AND DATABASE IS PROVIDED "AS IS" AND PRINCETON  
;;;              UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR  
;;;              IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PRINCETON  
;;;              UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES OF MERCHANT-  
;;;              ABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT THE USE  
;;;              OF THE LICENSED SOFTWARE, DATABASE OR DOCUMENTATION WILL NOT  
;;;              INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR  
;;;              OTHER RIGHTS.  
;;;                
;;;              The name of Princeton University or Princeton may not be used in  
;;;              advertising or publicity pertaining to distribution of the software  
;;;              and/or database.  Title to copyright in this software, database and  
;;;              any associated documentation shall at all times remain with  
;;;              Princeton University and LICENSEE agrees to preserve same.  
;;;               
;;;
;;;               The Wordnet License is located in the following directory relative to
;;;               the location of this file:
;;;               ../WNLexicalData/WordNet-License.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; History
;;;
;;; BE - Bruno Emond
;;; DB - Dan Bothell
;;;
;;; 2005.11.15  BE    - Version 0.1 of WNLexical for Act-r 6.0
;;; 2006.05.17  BE    - Version 1.0 for first SourceForge distribution.
;;; 2006.05.22  DB    - Add packaged-actr compiling instructions.
;;;                   - Change location of the 'WNLexixalData' directory and use of the 
;;;                     logical pathname "ACT-R6:".
;;;                   - Remove buffer case in method 'wnl-module-and-buffer-query',
;;;                     which handles module queries.
;;;                   - Move the call that loads WNChunks into declarative memory
;;;                     from the module reset method to the one that handles the
;;;                     module parameters. 
;;; 2006.05.26  BE    - Replace the function (use-wnl-lexical) by a general parameter, :wnl.
;;;                   - Add a warning message when a model makes a WNLexical module or buffer 
;;;                     queriy and wnl-chunks are not loaded in the module. 
;;; 
;;; 2007.08.22  BE    - WNLexical version 3.0.1 : Upgrade to WordNet 3.0
;;;
;;; 2008.06.16  BE    - Add the condition that if the synset context is empty, select a chunk
;;;                     at random. Otherwise use the set-difference or intersection operators.
;;;                     Set difference applies to chunk id, while intersection applies to 
;;;                     synset-ids.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To do
;;;
;;; 2005.05.26  BE    - Loading in declarative memory is very slow.
;;;                   - Add word frequency information.  
;;;
;;; 2007.08.22  BE    - Proper integration sense key operator (sk).
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; actr package
;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parameters
;;;

(defparameter *wordnet-lexical-version* "3.0.2")
(defparameter *wordnet-lexical-documentation* "WN-Lexical: An ACT-R module implementing the Wordnet lexical database." )

(defparameter *wnl-code-pathname-directory* 
  (pathname-directory
   (translate-logical-pathname (logical-pathname "ACT-R6:Modules;"))))

(defparameter *wnl-data-pathname-directory* 
  (pathname-directory
   (translate-logical-pathname (logical-pathname "ACT-R6:WNLexicalData;"))))

(defparameter *wordnet-lexical-modules* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; wn-htable
;;;

(defclass wn-htable ()
  ((htable :initform (make-hash-table :test #'equal) :accessor htable)
   (data-directory :initform *wnl-data-pathname-directory* :accessor data-directory)
   (data-source :initarg :data-source :accessor data-source)))

(defmethod empty-htable ((wn-htable wn-htable))
  (if (equal 0 (hash-table-count (htable wn-htable)))
      t nil))

(defclass wn-chunks (wn-htable)
  ((data-source :initform "WNChunks")))

(defmethod get-index-value ((wn-htable wn-chunks) (key symbol))
  (gethash key (htable wn-htable)))

(defclass wn-indexes (wn-htable)
  ((data-source :initform "WNChunksIndexes")))

(defmethod get-index-value ((wn-htable wn-indexes) (key number))
  (gethash key (htable wn-htable)))

(defmethod get-index-value ((wn-htable wn-indexes) (key string))
  (gethash key (htable wn-htable)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; indexes
;;;

(defclass word-senses-index ()
  ((word :initform nil :initarg :key :accessor key)
   (senses :initform nil :accessor senses)))

(defclass synset-wn-operators-index ()
  ((synset-id :initform nil :initarg :key :accessor key)
   (s :initform nil)
   (sk :initform nil)
   (g :initform nil)
   (syntax :initform nil)
   (hyp :initform nil)
   (ins :initform nil)
   (ent :initform nil) 
   (sim :initform nil) 
   (mm :initform nil) 
   (ms :initform nil) 
   (mp :initform nil) 
   (der :initform nil) 
   (cls :initform nil) 
   (cs :initform nil) 
   (vgp :initform nil) 
   (at :initform nil) 
   (ant :initform nil) 
   (sa :initform nil) 
   (ppl :initform nil) 
   (per :initform nil) 
   (fr :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; wordnet-lexical-module
;;;

(defclass wordnet-lexical-module ()
  ((load-wnl-chunks :initform nil 
                    :accessor load-wnl-chunks
                    :documentation "Parameter for determining if WN-CHUNKS are loaded in the module. Save time and memory resources when actr does not use WNLexical.")
   (wnl-dm :initform nil 
           :accessor wnl-dm
           :documentation "Parameter for determining if WN-CHUNKS are also loaded in declarative memory")
   (wnl-chunks :initform (make-instance 'wn-chunks)
               :accessor wnl-chunks
               :documentation "WordNet-Lexical chunks.")
   (wnl-indexes :initform (make-instance 'wn-indexes)
                :accessor wnl-indexes
                :documentation "Indexes for the WordNet-Lexical chunks.")
   (synset-context :initform nil
                   :accessor synset-context
                   :documentation "A list of recent wordnet chunks retrieved.")
   (busy-state :initarg :busy-state 
               :initform nil 
               :accessor busy-state
               :documentation "Hold the state of the module. Request on the state returns either busy or free")
   (failed-state :initarg :failed-state
                 :initform nil 
                 :accessor failed-state
                 :documentation "Hold the state of the module it a process failled. Request on the state returns either error or t.")
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; wordnet-lexical-modules
;;;

(defclass wordnet-lexical-modules ()
  ((model-names :initarg :model-names :initform nil :accessor model-names)
   (shared-module :initarg :shared-module 
                  :initform (make-instance 'wordnet-lexical-module)
                  :accessor shared-module)))

(defun reset-wordnet-lexical-modules ()
  (setf *wordnet-lexical-modules* 
        (make-instance 'wordnet-lexical-modules)))

(reset-wordnet-lexical-modules)

(defmethod loaded-wnl-for-model ((wordnet-lexical-modules wordnet-lexical-modules) model-name)
  (member model-name (model-names wordnet-lexical-modules)))
  
(defmethod set-wnl-for-model ((wordnet-lexical-modules wordnet-lexical-modules) model-name)
  (push model-name (model-names wordnet-lexical-modules))
  (setf *wordnet-lexical-modules* wordnet-lexical-modules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; wordnet-lexical-module 
;;; Methods
;;;

(defmethod wnl-ready ((wnl wordnet-lexical-module))
  (if (and (load-wnl-chunks wnl)
           (not (empty-htable (wnl-chunks wnl)))
           (not (empty-htable (wnl-indexes wnl))))
      t nil))

(defmethod clear-synset-context ((wnl wordnet-lexical-module))
  (setf (synset-context wnl) nil)
  (setf (busy-state wnl) nil))

;;; wn-chunks methods
;;;
(defmethod get-chunk-spec ((wnl wordnet-lexical-module) (wn-chunk-name symbol))
  (get-index-value (wnl-chunks wnl) wn-chunk-name))

(defmethod get-synset-id ((wnl wordnet-lexical-module) (wn-chunk-name symbol))
  (if (chunk-p-fct wn-chunk-name)
      (chunk-slot-value-fct wn-chunk-name 'synset-id)
    (nth 4 (get-chunk-spec wnl wn-chunk-name))))

(defmethod define-wn-chunk ((wnl wordnet-lexical-module) (wn-chunk-name symbol))
  (if (chunk-p-fct wn-chunk-name)
      wn-chunk-name
    (car (define-chunks-fct (list (get-chunk-spec wnl wn-chunk-name))))))

;;; indexes methods
;;;
(defmethod get-synset-operator-chunk-names ((wnl wordnet-lexical-module) (synset-id number) (operator symbol))
  (slot-value (get-index-value (wnl-indexes wnl) synset-id) operator))

(defmethod get-synset-operator-chunk-specs ((wnl wordnet-lexical-module) (synset-id number) (operator symbol))
  (let (wn-chunk-specs)
    (dolist (wn-chunk-name (get-synset-operator-chunk-names wnl synset-id operator))
      (push (get-chunk-spec wnl wn-chunk-name) wn-chunk-specs))
    wn-chunk-specs))

(defmethod get-word-sense-names ((wnl wordnet-lexical-module) (word string))
  (let ((word-senses-index (get-index-value (wnl-indexes wnl) word)))
    (when word-senses-index
      (senses word-senses-index))))

(defmethod get-word-senses ((wnl wordnet-lexical-module) (word string))
  (let (wn-chunk-specs)
    (dolist (wn-chunk-name (get-word-sense-names wnl word))
      (push (get-chunk-spec wnl wn-chunk-name) wn-chunk-specs))
    wn-chunk-specs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; act-r interface
;;; 

;;;
;;; Module creation
;;;

(defmethod wnl-module-creation ((model-name symbol))
  (unless (loaded-wnl-for-model *wordnet-lexical-modules* model-name)
    (set-wnl-for-model *wordnet-lexical-modules* model-name))
  (shared-module *wordnet-lexical-modules*))

;;;
;;; Module reset
;;;

(defmethod wnl-module-reset ((wnl (eql nil)))
  nil)

(defmethod wnl-module-reset ((wnl wordnet-lexical-module))
  (make-actr-wnl-chunk-types)
  (setf (synset-context wnl) nil
        (busy-state wnl)     nil
        (failed-state wnl)   nil)
  wnl)

;;;
;;; Module query
;;;

(defmethod wnl-module-and-buffer-query ((wnl wordnet-lexical-module) 
                                        (buffer-name symbol) (slot-name symbol) (slot-value symbol))
  (if (wnl-ready wnl)
      (case slot-name
        (state
         (case slot-value
           (busy
            (busy-state wnl))
           (free
            (not (busy-state wnl)))
           (success
            (not (failed-state wnl)))
           (error
            (failed-state wnl))
           (empty-context
            (not (synset-context wnl)))
           (t (print-warning  
               "Invalid module-state query for buffer ~S with value ~S (must be either busy, free, success, error, or empty-context)." 
               buffer-name slot-value))))
        (t (print-warning  
            "Invalid module query for buffer ~S with slot ~S (must be either state, or buffer)." 
            buffer-name slot-name)))
    (model-warning "WNL-CHUNKS are not loaded. Use (sgp :wnl-chunks wnl) at the top of your model.")))
  
;;;
;;; Module request
;;;

(defmethod get-wn-chunks ((wnl wordnet-lexical-module) wn-operator word synset-id)
  (let (chunk-specs)
    (cond ((and word synset-id)
           (if (member synset-id (get-word-senses wnl word) :key #'get-synset-id)
               (setf chunk-specs (get-synset-operator-chunk-specs wnl synset-id wn-operator))
             (model-warning "The request with word ~S and synset-id ~S is inconsistent. Word senses are: ~S." word synset-id (get-word-senses wnl word))))
          (word
           (if (equal 's wn-operator)
               (setf chunk-specs (get-word-senses wnl word))
             (model-warning "An initial request with word ~S and wn-operator S must be done prior to a request with wn-operator ~S." word wn-operator)))
          (synset-id
           (setf chunk-specs (get-synset-operator-chunk-specs wnl synset-id wn-operator))))
    chunk-specs))

(defmethod apply-selection-criterion ((wnl wordnet-lexical-module) (retrieved-wn-chunks list) (criterion symbol))
  (flet ((pick (lst)
           (when lst
             (nth (random (length lst)) lst))))
    (case criterion
      (set-intersection 
       (if (synset-context wnl)
           (pick (intersection (synset-context wnl) retrieved-wn-chunks  
                               :key #'(lambda (x) (get-synset-id wnl (car x)))
                               ))
         (pick retrieved-wn-chunks)))
      (set-difference 
       (if (synset-context wnl)
           (pick (set-difference retrieved-wn-chunks (synset-context wnl) 
                                 :key  #'car ;#'(lambda (x) (get-synset-id wnl (car x)))
                                 ))
         (pick retrieved-wn-chunks)))
      (otherwise 
       (pick retrieved-wn-chunks)))))

(defmethod select-from-retrieved-wn-chunks ((wnl wordnet-lexical-module) (retrieved-wn-chunks list) (criterion symbol))
  (let ((wn-chunk-spec (apply-selection-criterion wnl retrieved-wn-chunks criterion)))
    (format nil "~S" wn-chunk-spec)
    (dolist (event (busy-state wnl))
      (delete-event event))
    (setf (busy-state wnl) nil
          (synset-context wnl) (union (synset-context wnl) 
                                      (when wn-chunk-spec (list wn-chunk-spec)) :key #'car))
    (if wn-chunk-spec
        (schedule-set-buffer-chunk 'wn-lexical (define-wn-chunk wnl (car wn-chunk-spec)) 0 
                                   :module 'wn-lexical
                                   :priority :max)
      (setf (failed-state wnl) t))))

(defmethod retrieve-wn-chunks ((wnl wordnet-lexical-module) (request t))
  (if (and (equal 'wnl-request (chunk-spec-chunk-type request))
           (slot-in-chunk-spec-p request 'wn-operator)
           (or (slot-in-chunk-spec-p request 'word)
               (slot-in-chunk-spec-p request 'synset-id)))
      (let* ((wn-operator (third (car (chunk-spec-slot-spec request 'wn-operator))))
             (word (when (slot-in-chunk-spec-p request 'word)
                     (third (car (chunk-spec-slot-spec request 'word)))))
             (synset-id (when (slot-in-chunk-spec-p request 'synset-id)
                          (third (car (chunk-spec-slot-spec request 'synset-id)))))
             (context-criterion (when (slot-in-chunk-spec-p request 'context-criterion)
                                  (third (car (chunk-spec-slot-spec request 'context-criterion)))))
             (retrieved-wn-chunks (get-wn-chunks wnl wn-operator word synset-id)))
        (if retrieved-wn-chunks
            (push (schedule-event-relative 0 'select-from-retrieved-wn-chunks
                                           :module 'wn-lexical
                                           :destination 'wn-lexical
                                           :details (if context-criterion
                                                        (format nil "SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE ~S CRITERION IS ~S BETWEEN RETRIEVED-CHUNKS ~S AND CONTEXT ~S"
                                                                (length retrieved-wn-chunks) context-criterion 
                                                                (mapcar #'(lambda (x) (nth 0 x)) retrieved-wn-chunks) (mapcar #'(lambda (x) (nth 0 x)) (synset-context wnl)))
                                                      (format nil "SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE ~S CRITERION IS RANDOM FROM RETRIEVED-CHUNKS ~S" 
                                                              (length retrieved-wn-chunks) 
                                                              (mapcar #'(lambda (x) (nth 0 x)) retrieved-wn-chunks)))
                                           :priority -2000
                                           :params (list retrieved-wn-chunks context-criterion)
                                           :output 'medium)
                  (busy-state wnl))
          (progn 
            (dolist (event (busy-state wnl))
              (delete-event event))
            (setf (busy-state wnl) nil
                  (failed-state wnl) t))))
    (model-warning "A request to the wn-lexical module must be a chunk of type WNL-REQUEST, and have a value for the slot WN-OPERATOR, and have values for either WORD or SYNSET-ID, or both. Processing stopped.")))

(defmethod wnl-module-request ((wnl wordnet-lexical-module) (buffer-name symbol) (request t))
  (declare (ignore buffer-name)) 
  (if (wnl-ready wnl)
      (progn
        (when (busy-state wnl)
          (model-warning "A lexical access event has been aborted by a new request")
          (dolist (event (busy-state wnl))
            (delete-event event)))
        (setf (failed-state wnl) nil)  
        (setf (busy-state wnl)
              (case (chunk-spec-chunk-type request)
                (wnl-request
                 (list (schedule-event-relative 0 'retrieve-wn-chunks
                                                :module 'wn-lexical
                                                :destination 'wn-lexical
                                                :details (symbol-name 'retrieve-wn-chunks)                             
                                                :priority -1000
                                                :params (list request)
                                                :output 'medium)))
                (wnl-clear-context
                 (list (schedule-event-relative 0 'clear-synset-context
                                                :module 'wn-lexical
                                                :destination 'wn-lexical
                                                :details (symbol-name 'clear-wn-lexical-synset-context)                             
                                                :priority -1000
                                                :params nil
                                                :output 'medium))))))
    (model-warning "WNL-CHUNKS are not loaded. Use (sgp :wnl-chunks wnl) at the top of your model.")))

;  (case (chunk-spec-chunk-type chunk-spec)
;    (clear ;; replaces the implicit clear from -manual
;     (schedule-event-relative 0 'clear :module :motor :destination :motor
;                              :output 'medium)
;     )
;    (execute
;     (schedule-event-relative 0 'execute :module :motor :destination :motor
;                              :output 'medium)


;;;
;;; Module parameters
;;;

(defmethod get-wn-slot-names ((wn-operator symbol))
  (case wn-operator
    ;;;;;;   s(synset_id,w_num,'word',ss_type,sense_number,tag_count). 
    ;;; A s operator is present for every word sense in WordNet. 
    ;;; In wn_s.pl , w_num specifies the word number for word in the synset.
    (s '(synset-id w-num word ss-type sense-number tag-count))
    ;;;;;;   sk(synset_id,w_num,'sense_key').
    ;;; A sk operator is present for every word sense in WordNet. 
    ;;; This gives the WordNet sense key for each word sense.
    (sk '(synset_id w_num sense_key))
    ;;;;;;   g(synset_id,'(gloss)'). 
    ;;; The g operator specifies the gloss for a synset.
    (g '(synset-id gloss))
    ;;;;;;   syntax(synset_id,w_num,syntax).
    ;;; The syntax operator specifies the syntactic marker for a given 
    ;;; word sense if one is specified.
    (syntax '(synset_id w_num syntax))
    ;;;;;;   hyp(synset_id,synset_id).
    ;;; The hyp operator specifies that the second synset is a hypernym 
    ;;; of the first synset. This relation holds for nouns and verbs.
    ;;; The reflexive operator, hyponym, implies that the first synset 
    ;;; is a hyponym of the second synset.
    (hyp '(synset-id synset-id2))
    ;;;;;;   ins(synset_id,synset_id).
    ;;; The ins operator specifies that the first synset is an instance 
    ;;; of the second synset. This relation holds for nouns. The reflexive 
    ;;; operator, has_instance, implies that the second synset is an
    ;;; instance of the first synset.
    (ins '(synset_id synset_id2))
    ;;;;;;   ent(synset_id,synset_id).
    ;;; The ent operator specifies that the second synset is an
    ;;; entailment of first synset. This relation only holds for verbs.
    (ent '(synset-id synset-id2))
    ;;;;;;   sim(synset_id,synset_id).
    ;;; The sim woperator specifies that the second synset is similar 
    ;;; in meaning to the first synset. This means that the second synset 
    ;;; is a satellite the first synset, which is the cluster head. 
    ;;; This relation only holds for adjective senses contained in 
    ;;; adjective clusters.
    (sim '(synset-id synset-id2))
    ;;;;;;   mm(synset_id,synset_id).
    ;;; The mm operator specifies that the second synset is a member 
    ;;; meronym of the first synset. This relation only holds for nouns.
    ;;; The reflexive wn-operator, member holonym, can be implied.
    (mm '(synset-id synset-id2))
    ;;;;;;   ms(synset_id,synset_id). 
    ;;; The ms operator specifies that the second synset is a substance 
    ;;; meronym of the first synset. This relation only holds for nouns. 
    ;;; The reflexive operator, substance holonym, can be implied.
    (ms '(synset-id synset-id2))
    ;;;;;;   mp(synset_id,synset_id). 
    ;;; The mp operator specifies that the second synset is a part
    ;;; meronym of the first synset. This relation only holds for nouns. 
    ;;; The reflexive operator, part holonym, can be implied.
    (mp '(synset-id synset-id2))
    ;;;;;;   der(synset_id,synset_id). 
    ;;; The der operator specifies that there exists a reflexive 
    ;;; lexical morphosemantic relation between the first and second 
    ;;; synset terms representing derivational morphology.
    (der '(synset-id w-num1 synset-id2 w-num2))
    ;;;;;;   cls(synset_id,synset_id,class_type).
    ;;; The cls operator specifies that the first synset has been 
    ;;; classified as a member of the class represented by the 
    ;;; second synset.
    (cls '(synset-id synset-id2 class-type))
    ;;;;;;   cs(synset_id,synset_id). 
    ;;; The cs operator specifies that the second synset is a 
    ;;; cause of the first synset. This relation only holds for verbs.
    (cs '(synset-id synset-id2))
    ;;;;;;   vgp(synset_id,synset_id).
    ;;; The vgp operator specifies verb senses that are similar in
    ;;; meaning and should be grouped together when displayed in response 
    ;;; to a grouped synset search.
    (vgp '(synset-id w-num1 synset-id2 w-num2))
    ;;;;;;   at(synset_id,synset_id).
    ;;; The at operator defines the attribute relation between noun and
    ;;; adjective synset pairs in which the adjective is a value of the noun.
    ;;; For each pair, both relations are listed (ie. each synset_id is both 
    ;;; a source and target).
    (at '(synset-id synset-id2))
    ;;;;;;   ant(synset_id,w_num,synset_id,w_num). 
    ;;; The ant operator specifies antonymous word s. This is a lexical 
    ;;; relation that holds for all syntactic categories. For each antonymous
    ;;; pair, both relations are listed (ie. each synset_id,w_num pair is both 
    ;;; a source and target word.)
    (ant '(synset-id w-num1 synset-id2 w-num2))
    ;;;;;;   sa(synset_id,w_num,synset_id,w_num). 
    ;;; The sa operator specifies that additional information about the first 
    ;;; word can be obtained by seeing the second word. This wn-operator is only 
    ;;; defined for verbs and adjectives. There is no reflexive relation 
    ;;; (ie. it cannot be inferred that the additional information about the 
    ;;; second word can be obtained from the first word).
    (sa '(synset-id w-num1 synset-id2 w-num2))
    ;;;;;;   ppl(synset_id,w_num,synset_id,w_num). 
    ;;; The ppl operator specifies that the adjective first word is a participle 
    ;;; of the verb second word. The reflexive wn-operator can be implied.
    (ppl '(synset-id w-num1 synset-id2 w-num2))
    ;;;;;;   per(synset_id,w_num,synset_id,w_num).
    ;;; The per operator specifies two different relations based on the parts 
    ;;; of speech involved. If the first word is in an adjective synset, that 
    ;;; word pertains to either the noun or adjective second word. If the first 
    ;;; word is in an adverb synset, that word is derived from the adjective 
    ;;; second word.
    (per '(synset-id w-num1 synset-id2 w-num2))
    ;;;;;;   fr(synset_id,f_num,w_num).
    ;;; The fr operator specifies a generic sentence frame for one or all 
    ;;; words in a synset. The wn-operator is defined only for verbs.
    (fr  '(synset-id f-num w-num))))

(defun make-actr-wnl-chunk-types ()
  (unless (chunk-type-p wnl-request)
    (chunk-type wnl-request word synset-id wn-operator context-criterion))
  (unless (chunk-type-p wnl-clear-context)
    (chunk-type wnl-clear-context))
  (unless (chunk-p success)
    (define-chunks (success isa chunk)))
  (dolist (wn-operator '(s g hyp ent sim mm ms mp der cls cs vgp at ant sa ppl per fr))
    (unless (chunk-type-p-fct wn-operator)
      (eval `(chunk-type ,wn-operator ,@(get-wn-slot-names wn-operator))))))
 
(defmethod load-chunks-from-data-source ((wn-htable wn-htable) &optional (nb-lines nil))
  (clrhash (htable wn-htable))
  (format t "~%Loading ~S for the WN-Lexical module...~%" (data-source wn-htable))
  (with-open-file (stream (make-pathname :directory (data-directory wn-htable) 
                                         :name (data-source wn-htable)
                                         :type "data")
                          :direction :input
                          :if-does-not-exist :error)
    (format t " 00%")
    (do* ((nb-line 0 (incf nb-line))
          (line (read-line stream nil 'eof)
                (read-line stream nil 'eof))
          (filelength (file-length stream))
          (fileposition (file-position stream)
                        (file-position stream))
          (proportion 10))
         ((or (equal nb-line nb-lines)
              (eql line 'eof))
          (format t " 100%~%"))
      (when (> fileposition (* (/ proportion 100) filelength))
        (format t " ~S%" proportion)
        (setf proportion (+ 10 proportion)))
      (let* ((index-spec (read-from-string line))
             (index (gethash (first index-spec) (htable wn-htable))))
        (unless index
          (cond ((stringp (first index-spec))
                 (setf index (make-instance 'word-senses-index :key (first index-spec))))
                ((numberp (first index-spec))
                 (setf index (make-instance 'synset-wn-operators-index :key (first index-spec))))
                ((symbolp (first index-spec))
                 (setf index index-spec))))
        (unless (listp index)
          (setf (slot-value index (second index-spec)) (third index-spec)))
        (setf (gethash (first index-spec) (htable wn-htable)) index))))
  wn-htable)

(defmethod load-wn-lexical-in-declarative-memory ((wnl wordnet-lexical-module))
  (let ((proportion 10)
        (loaded-chunks 0)
        (tot-chunks (hash-table-count (htable (wnl-chunks wnl)))))
    (format t "~%Loading a total of ~S WN-Lexical chunks in declarative memory ...~%" tot-chunks)
    (format t " 00%")
    (maphash #'(lambda (key chunk-spec)
                 (declare (ignore key))
                 (add-dm-fct (list chunk-spec))
                 (incf loaded-chunks)
                 (when (> loaded-chunks (* (/ proportion 100) tot-chunks))
                   (format t " ~S%" proportion)
                   (setf proportion (+ 10 proportion))))
             (htable (wnl-chunks wnl)))
    (format t " 100%~%")))

(defmethod print-number-of-chunks ((wnl wordnet-lexical-module))
  (format t "~%~S WNL-Chunks loaded in module WNLexical." (hash-table-count (htable (wnl-chunks wnl)))))

(defmethod load-all-data-sources-in-wn-lexical ((wnl wordnet-lexical-module))
  (setf (wnl-chunks wnl) 
        (load-chunks-from-data-source (wnl-chunks wnl)))
  (setf (wnl-indexes wnl) 
        (load-chunks-from-data-source (wnl-indexes wnl)))
  wnl)


(defmethod wn-lexical-parameters ((wnl wordnet-lexical-module) (parameter list))
  (case (car parameter)
    (:wnl-chunks
     (setf (load-wnl-chunks wnl) (cdr parameter))
     (case (load-wnl-chunks wnl)
       (wnl (make-actr-wnl-chunk-types)
            (when (not (wnl-ready wnl))
              (load-all-data-sources-in-wn-lexical wnl))
            (print-number-of-chunks wnl))
       (dm (make-actr-wnl-chunk-types)
           (when (not (wnl-ready wnl))
             (load-all-data-sources-in-wn-lexical wnl))
           (print-number-of-chunks wnl)
           (load-wn-lexical-in-declarative-memory wnl))
       (nil (reset-wordnet-lexical-modules))))
    (otherwise (print-warning  "Unknown parameter ~S for module WNLexical." (car parameter)))))

(defmethod wn-lexical-parameters ((wnl wordnet-lexical-module) (parameter symbol))
  (case parameter
    (:wnl-chunks
     (load-wnl-chunks wnl))
    (otherwise (print-warning  "Unknown parameter ~S for module WNLexical." parameter))))

;;;
;;; Module definition
;;;

(defmethod valid-wnl-chunks-param-value ((value symbol))
  (member value '(wnl dm nil)))

(define-module-fct 'wn-lexical 
                   '(wn-lexical) 
                   (list (define-parameter :wnl-chunks :valid-test #'valid-wnl-chunks-param-value :default-value nil
                                           :warning "Possible values are wnl, dm or nil" :documentation "To load WN-CHUNKS in WNLexical use WNL, to load WN-CHUNKS in declarative memory (and WNLexical) use DM."))
                   :version *wordnet-lexical-version*
                   :documentation *wordnet-lexical-documentation*
                   :creation #'wnl-module-creation
                   :reset #'wnl-module-reset
                   :query #'wnl-module-and-buffer-query
                   :request #'wnl-module-request
                   :buffer-mod nil
                   :params #'wn-lexical-parameters
                   :delete nil
                   :notify-on-clear nil
                   :update nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:eof