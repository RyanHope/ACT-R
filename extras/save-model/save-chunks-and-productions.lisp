;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2010 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : save-chunks-and-productions.lisp
;;; Version     : 1.0a2
;;; 
;;; Description : Saves a model's declarative and procedural components to a file
;;;             : which can be loaded later as a model.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2010.06.11 Dan
;;;             : * First pass at a version of this for inclusion with the sources.
;;; 2013.05.31 Dan [1.0a2]
;;;             : * Adding support for static chunk-types.  For now it's just 
;;;             :   going to write out the entire subtree for a static instead
;;;             :   of trying to figure out which were user defined and which
;;;             :   were automatic.
;;;             : * Realized that the sort based on subtype status isn't right
;;;             :   since it doesn't work for unrelated types because sort may
;;;             :   not test every pair together.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Add a new command which allows a model's current declarative memory, set of
;;; productions, general parameters related to declarative and procedural, and the
;;; appropriate chunk and proceduction parameters to be written out to a model file.
;;; 
;;; That does not capture all of the current state because it does not record 
;;; chunks which are in buffers, It does not record internal information from 
;;; any module (nothing from the perceptual or motor modules is recorded nor are
;;; things like finsts in declarative), events on the meta-process queue are not
;;; stored in any way, nor are any commands or settings which are in the original 
;;; model (other than sgp settings for some declarative and procedural settings).  
;;; 
;;; So the assumptions are that the module is "stopped" (no ongoing actions) before 
;;; saving the state and it's up to the modeler to manually add any other settings 
;;; or commands which are also needed for the model to run.
;;; 
;;; In terms of declarative parameters there is one exception worth noting as
;;; to what is saved -- the Sji values are not written out.  That is because
;;; the assumption is that they are dynamic based on the fans of the items.  Thus,
;;; when the new model is loaded it will restore those automatically and they
;;; will continue to change as new chunks are added to DM.  However, if they had
;;; been written out as explicit parameter settings then those values would be
;;; fixed and the chunks would no longer adapt as new chunks were added to DM.
;;; If explicit values are set in the model those would have to be manually
;;; added to the saved model file.
;;;
;;; 
;;; Here's a list of the general parameters which are written out if the current
;;; value differs from the default value: 
#|

DECLARATIVE module
--------
:MD     
:RT     
:LE     
:MS     
:MP     
:PAS    
:MAS    
:ANS    
:BLC    
:LF     
:BLL    
--------------------------------
CENTRAL-PARAMETERS module
--------------------------------
:ESC    
:ER     
:OL     
--------------------------------
UTILITY module
--------------------------------
:IU     
:UL     
:ALPHA  
:UT     
:NU     
:EGS    
--------------------------------
PRODUCTION-COMPILATION module
--------------------------------
:EPL    
:TT     
--------------------------------
PROCEDURAL module
-------------------------------
:DAT    
:PPM    
|#

;;; It will also write out the current :seed parameter in a comment so that it
;;; could be used to reproduce the same "future" after the save point as the 
;;; model which was saved i.e. if you run a model, save it, then run it some more
;;; uncommenting the seed in the saved version then loading that and running it
;;; will produce the same results as the you had with the run after the save.

;;; The appropriate declarative parameters among the following will be written out based 
;;; on the :mp, :bll, and :ol settings using sdp:

:creation-time
:reference-count
:reference-list
:similarities

;;; When writing out the creation-time and reference-list the times will be
;;; adjusted to a zero reference since the model time will be back to 0 when
;;; reloaded.  If one wants to avoid the re-referencing of those times then
;;; the optional parameter to save-chunks-and-productions needs to be specified
;;; as nil.

;;; For productions the :u and :at values will always be written out.  A non-nil 
;;; :reward will be saved if utility learning is enabled.  If production 
;;; compilation is enabled then an additional production parameter which is not
;;; part of spp will be set to indicate which productions were learned.  The
;;; reason for that last setting is because the utility learning differs for
;;; productions which were "original" vs learned and without the setting all
;;; the productions loaded would be marked as originals.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; save-chunks-and-productions
;;;
;;; (defun save-chunks-and-productions (file-name &optional (zero-ref t))
;;;
;;; This function takes one required parameter which is a string that names
;;; a file to create and write the saved model definition to.  If the optional
;;; parameter is specified as nil then the current reference times for the
;;; declarative parameters are written out exactly as they are.  If it is not
;;; provided or has any non-nil value then those times will be adjusted so that
;;; they correspond to a current time of 0.0s (the default time when the model
;;; is later loaded).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Only writing out those things that are "publically" accessible.  Not going
;;; deep into any module or looking at model components which aren't part of the
;;; declarative and procedural systems.
;;;
;;; This should capture most of what people are looking for in a "save model"
;;; type action.  Of course it's probably also lacking in something for anyone
;;; that wants to use it...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defparameter *critical-params* '((:MD -1.0) (:RT 0.0) (:LE 1.0) (:MS 0.0) (:MP NIL) (:PAS NIL) (:MAS NIL) (:ANS NIL)
                                  (:BLC 0.0) (:LF 1.0) (:BLL NIL) (:ESC NIL) (:ER NIL) (:OL T) (:IU 0) (:UL NIL)
                                  (:ALPHA 0.2) (:UT NIL) (:NU 0) (:EGS 0.0) (:EPL NIL) (:TT 2.0) (:DAT 0.05) (:PPM NIL)))
  

(defun order-chunk-types (type-list)
  (let* ((root-types (remove-if (lambda (x) (> (length (chunk-type-supertypes-fct x)) 1)) type-list))
         (families (mapcar 'list root-types))
         (others (set-difference type-list root-types)))
    (dolist (x others)
      (nconc (find-if (lambda (y) (chunk-type-subtype-p-fct x (car y))) families) (list x)))
    (do ((res nil)
         (fams families (cdr fams)))
        ((null fams) (flatten res))
      (push (sort (car fams) #'< :key (lambda (x) (length (chunk-type-supertypes-fct x)))) res))))

(defun save-chunks-and-productions (file-name &optional (zero-ref t))
  
  (let ((chunks (no-output (dm)))
        (productions (no-output (pp)))
        (cmdt (car (no-output (sgp :cmdt)))))
    
    ;;; Use the command trace to write things out since it will 
    ;;; handle the opening of the file and pprint-chunks and pp will then work
    ;;; "automatically".
    
    (sgp-fct (list :cmdt file-name))
    
    ;;; Write out a comment indicating what and when
    
    (multiple-value-bind (sec min hour date month year) (get-decoded-time)
      (command-output ";;; Saved version of model ~s at run time ~f on ~d/~d/~d ~d:~2,'0d:~2,'0d"
                      (current-model) (mp-time) year month date hour min sec))
    
    ;;; assume that a clear-all is appropriate for a model file
    
    (command-output "~%(clear-all)")
    
    ;;; write this as a model so it could theoretically just be loaded
    
    (command-output "~%(define-model ~s-saved" (current-model))
    
    ;;; Start with the general parameters
    
    (command-output "~%(sgp ")
    
    (dolist (param *critical-params*)
      (unless (equalp (second param) (car (no-output (sgp-fct (list (first param))))))
        (command-output "~s ~s" (first param) (car (no-output (sgp-fct (list (first param))))))))
    
    (command-output ")")
    
    ;;; Write out the current seed in a comment if continuing exactly is desired
    
    (command-output "~%;;; (sgp :seed ~s)~%" (no-output (car (sgp :seed))))
    
    ;;; Determine which chunk-types are added in the model and sort them for printing
    ;;; so super types are defined first
    
    (let* ((dummy-name (gensym))
           (default-types (prog2
                            (define-model-fct dummy-name nil)
                            (with-model-eval dummy-name
                                (no-output (chunk-type)))
                            (delete-model-fct dummy-name)))
           ;; group them into subtype clusters
           (model-types (order-chunk-types (set-difference (no-output (chunk-type)) default-types))))
      
      (dolist (ct model-types)
        (aif (chunk-type-static-p-fct ct)
            (if (> (length (chunk-type-supertypes-fct ct)) 1)
                (command-output "(chunk-type (~a (:include ~a)) ~@[~s~]" ct it (chunk-type-documentation-fct ct))
              (command-output "(chunk-type (~a (:static t)) ~@[~s~]" ct (chunk-type-documentation-fct ct)))
          
          (if (> (length (chunk-type-supertypes-fct ct)) 1)
              (command-output "(chunk-type (~a (:include ~a)) ~@[~s~]" ct (second (chunk-type-supertypes-fct ct))
                              (chunk-type-documentation-fct ct))
            (command-output "(chunk-type ~a ~@[~s~]" ct (chunk-type-documentation-fct ct))))
          
        (dolist (slot (chunk-type-slot-names-fct ct))
          (aif (chunk-type-slot-default-fct ct slot)
               (command-output "  (~a ~s)" slot it)
               (command-output "  ~a" slot)))
        
        (command-output ")")))
    
    ;; Now write out all of the declarative chunks
    
    (command-output "(add-dm ")
    (dolist (x (reverse chunks))
      (command-output "(")
      (pprint-chunks-fct (list x))
      (command-output ")"))
    (command-output ")")
    
    (command-output "")
    (command-output "")
      

    ;;; If declarative options are enabled write out the necessary parameters
    
    (let ((esc (no-output (car (sgp :esc))))
          (mp (no-output (car (sgp :mp))))
          (bll (no-output (car (sgp :bll))))
          (ol (no-output (car (sgp :ol))))
          (params nil))
          
      (when (and esc (or mp bll))
        
        (when mp (push :similarities params))
        
        (cond ((null bll)
               ;;; no extra params needed
               )
              ((null ol)
               ;;; need creation and list
               (push :reference-list params)
               (push :creation-time params))
              ((numberp ol)
               (push :reference-list params)
               (push :reference-count params)
               (push :creation-time params))
              (t ;;; :ol is t
               (push :reference-count params)
               (push :creation-time params)))
        
        (dolist (c chunks)
          (command-output "(sdp ~a" c)
          (dolist (param params)
            (let ((val (caar (no-output (sdp-fct (list c param))))))
              (case param
                (:similarities 
                 (command-output "  ~s (~{~s~})" param val))
                (:creation-time
                 (command-output "  ~s ~f" param (if zero-ref (- val (mp-time)) val)))
                (:reference-count
                 (command-output "  ~s ~d" param val))
                (:reference-list
                 (command-output "  ~s (~{~F~^ ~})" param (if zero-ref (mapcar (lambda (x) (- x (mp-time))) val) val))))))
          (command-output ")")))
      
      ;; write out the productions 
      
      (command-output "")
      (command-output "")
      
      
      (pp)
      
      (command-output "")
      (command-output "")
      
      ;;; Write out the production parameters
      
      (let ((params (no-output (spp :name :u :at :reward)))
            (ul (car (no-output (sgp :ul)))))
        
        ;;; always :u and :at then :reward if utility learning is enabled
        
        (dolist (x params)
          (command-output "(spp ~a :u ~f :at ~f)" (first x) (second x) (third x))
          (when (and ul (fourth x))
            (command-output "(spp ~a :reward ~s)" (first x) (fourth x))))
        
        ;;; If utility learning and production compilation are on save which
        ;;; productions were learned along the way
        
        (when (and ul (car (no-output (sgp :epl))))
          (dolist (x productions)
            (unless (production-user-created x)
              (command-output "(setf (production-user-created '~a) nil)" x)))))
      
      ;;; write out the closing of the model
      
      (command-output ")")
      
      ;;; set the command trace back to where it was
      
      (sgp-fct (list :cmdt cmdt)))))


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
