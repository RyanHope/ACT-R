;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : server.lisp
;;; Version     : 3.0
;;; 
;;; Description : Contains no system dependent code because it relies on the
;;;             : definitions in uni-files.
;;;             : Holds the Environment global variables and the
;;;             : code to open and manage an environment socket connection. 
;;; Bugs        : 
;;; 
;;; Todo        : Reconstruct why all the error trapping ignores the
;;;             : unbound-variable errors because that's a bad thing
;;;             : for the stand-alone version...
;;; 
;;; ----- History -----
;;;
;;; 05/10/2002  Dan
;;;             : Added this header
;;; 05/20/2002  Dan
;;;             : Think I've finally got it working on Macs w/ OS < 10 and MCL
;;;             : and for now w/OSX it's got to be OpenMcl which also seems
;;;             : to work (though it's not in here yet...)
;;; 05/21/2002  Dan
;;;             : Serious reorganization took place with the files...
;;; 05/22/2002  Dan
;;;             : Fixed some more MCL problems.  At this point it's working
;;;             : mostly, so I'm not wasting any more time 'fixing' it there
;;;             : since MacOS < 10 is "dead"...
;;;             : Changed close-connection so that killing the process was
;;;             : optional and altered the message-process and process-
;;;             : connection functions so that they don't kill their own
;;;             : process (doesn't seem to change things, but seemed like a
;;;             : good idea and I thought it'd fix a closeing bug I had
;;;             : in MCL, but it didn't).
;;; 05/23/2002  Dan
;;;             : Made a very subtle change, that may have a big impact
;;;             : later on.  When a handler is created it preforms an initial
;;;             : update - that's as it was, but now that first time the
;;;             : handler itself is passed as the parameter to the updater.
;;;             : Thus if an updater needs to use its parameter it had better
;;;             : check its type because that first time it'll be something
;;;             : other than it normally expects.
;;; 08/23/2002  Dan
;;;             : Updated the version number to 6.0b3, which doesn't 
;;;             : correspond to the versions in the headers, so I
;;;             : need to fix that at some point for consistency...
;;; 09/17/2002  Dan
;;;             : Updated the environment and ACT-R version numbers...
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Added the conditionalized in-package to message-process
;;;             : to make building a standalone easier.
;;;             : Changed the environment version to correspond to the
;;;             : file version numbers and add a -S if it's the standalone.
;;; 01/20/2003  Dan
;;;             : Updated version number again for the env because
;;;             : of a quick change for the class.
;;; 07/07/2003  Dan
;;;             : Updated version because I rolled in most of Mike's newest
;;;             : RPM (my UWI and ACL device files were newer).
;;; 08/15/2003  Dan
;;;             : Updated version to 1.3 because I've added the support for
;;;             : CMUCL from Ethan Glasser-Camp at RPI.
;;; 12/19/2003  Dan
;;;             : Updated version to 1.4 for this file because I've fixed
;;;             : a bug in the uni-files and moved the ACT-R version
;;;             : variable to the actr5.lisp file itself.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info, updated the version to 1.5
;;;             : and removed the ACT-R version variable since that's
;;;             : in the main ACT-R file now.
;;; -----------------------------------------------------------------------
;;; 2005.04.11  Dan [2.0]
;;;             : Start of the move to ACT-R 6.0.
;;; 2007.08.03  Dan
;;;             : * Moved the *environment-sockets* defvar to env-module
;;;             :   to avoid a compiler warning.
;;; 2007.08.13  Dan
;;;             : * Adding reset as a possible update condtion for handlers.
;;; 2007.08.17 Dan
;;;             : * Added a without-interrupts call to close-connection to
;;;             :   fix a problem in stopping things under LispWorks.
;;; 2008.01.15 Dan
;;;             : * Changed close-connection because SBCL doesn't have
;;;             :   without-interrupts in the default package...
;;; 2008.04.08 Dan
;;;             : * Refixed that last update using uni-without-interrupts.
;;;             :   Also added the require-compiled of uni-files just to be
;;;             :   safe.
;;; 2008.05.16 Dan
;;;             : * Added the conflict-nil option for when to update the
;;;             :   handler.  That's the same as conflict except that it 
;;;             :   doesn't pass the return value back to the conflict-set-
;;;             :   hook.
;;; 2009.01.08 Dan
;;;             : * Fixed an issue with message-process which could cause
;;;             :   problems with some Lisps (CCLx86 in particular).
;;; 2009.04.13  Dan
;;;             : * Uni-send-string doesn't have an automatic newline now,
;;;             :   so need to put one in the string to be sent.
;;; 2009.04.14  Dan
;;;             : * Seems that adding the newline here breaks the stopping
;;;             :   of the connection.  However, since it doesn't seem to be 
;;;             :   necessary I'm just dropping it again.
;;; 2009.06.08  Dan
;;;             : * Adding the run-environment command which spawns the
;;;             :   environment app and then makes the connection.
;;;             :   Only available for LispWorks under MacOSX or Windows and
;;;             :   ACL under MacOSX or Windows at this time.
;;; 2009.06.09 Dan
;;;             : * Upped the default delay on run-environment to 12 seconds
;;;             :   to be safer.
;;; 2009.07.17 Dan
;;;             : * Added a test to the remove handler code so that it 
;;;             :   shouldn't throw an error on initial connection anymore
;;;             :   like it would occasionally for some Lisps.
;;; 2011.01.07 Dan
;;;             : * Lock all the background processes from the environment
;;;             :   for now until I come up with a better way to protect the
;;;             :   ACT-R code.
;;; 2011.01.14 Dan
;;;             : * Fixed the name of the application to run for the Mac
;;;             :   environment in the ACL run-environment command.
;;; 2011.01.14 Dan
;;;             : * Added a return reply for the keep alive message to 
;;;             :   possibly avoid some time out issues in Windows 7.
;;; 2011.02.21 Dan 
;;;             : * Added the ability to specify the new run-start and run-end hooks.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.05.25 Dan 
;;;             : * Updated create message parsing to set the use-model 
;;;             :   slot.  The semantics now are that if there are only 6
;;;             :   items in the create no model is used, if there are 7 then
;;;             :   the 7th is either a model name to use when evaluating the
;;;             :   update or nil which means use the current-model explicitly
;;;             :   (which is not the same as not using a model).
;;;             : * Added the which-hook slot to the control structure and
;;;             :   set that in the hook updating functions.
;;; 2011.05.26 Dan
;;;             : * Fixed a cut-and-paste error in delete-post-hook-if-necessary.
;;; 2011.06.01 Dan
;;;             : * Changed the remove message handling to only evaluate it in
;;;             :   a model if that's a valid model now.
;;; 2011.09.02 Dan
;;;             : * Added a background option to connect-to-environment so
;;;             :   that it's possible to have the environment message handling
;;;             :   thread be the main one (for use with standalone versions that
;;;             :   have a slave Lisp running headless).
;;; 2011.09.07 Dan
;;;             : * Fixed some format strings that were missing '~'s.
;;; 2011.09.13 Dan
;;;             : * Readjust how the default run-environment gets defined to
;;;             :   avoid a warning for multiple definitions.
;;; 2011.11.07 Dan
;;;             : * Added a check for LW 6 to the windows version of run-
;;;             :   environment so it'll work there too.
;;; 2011.12.19 Dan
;;;             : * Added some checks to start-environment so it better warns
;;;             :   when the Lisp isn't able to use the Environment.
;;; 2012.02.01 Dan
;;;             : * Added some more declaims to avoid undefined function warnings
;;;             :   at load time.
;;; 2012.02.07 Dan
;;;             : * Fixed the declaim for update-handler.
;;; 2012.03.21 Dan
;;;             : * Fixed a bug with stopping the environment while the stepper
;;;             :   was open that prevented one from using the stepper if a
;;;             :   new environment connection was made.
;;; 2012.09.07 Dan
;;;             : * Removed the in-package from message-process based on the
;;;             :   :actr-env-alone feature.
;;; 2012.09.21 Dan
;;;             : * Trying to eliminate an intermittent environment error
;;;             :   with multiple models in CCL (perhaps elsewhere too just
;;;             :   unreported).  So adding additional lock-outs so that 
;;;             :   updates can't occur while the model is running by locking
;;;             :   during the pre-event-hook and unlocking during the post as
;;;             :   well as during anything else which calls update handlers.
;;;             : * The problem with that is if someone breaks the system
;;;             :   between pre and post it leaves the lock set and basically
;;;             :   kills the environment -- need a better alternative.
;;; 2013.02.22 Dan
;;;             : * Added the wait-for-environment command to provide a way
;;;             :   to check that it has processed all outstanding actions.
;;;             :   Primary intended use is with creating complex visible
;;;             :   virtual displays, especially those that update faster than 
;;;             :   real time, because if the "drawing" falls behind things
;;;             :   can be difficult to use or display errors could result.
;;; 2013.02.25 Dan
;;;             : * Adding run-environment commands for CCL in Win, Mac, and
;;;             :   linux.
;;;             : * Changed how the delay parameter works for run-environment.
;;;             :   It still waits that long before the first try to connect,
;;;             :   but it will now continue to try to connect after every 
;;;             :   delay seconds pass.
;;; 2013.02.26 Dan
;;;             : * Fixed a bug with the CCL Mac run-environment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R6:support;uni-files")

(declaim (ftype (function (t t) t) delete-handler))
(declaim (ftype (function (t) t) send-register))
(declaim (ftype (function (t t) t) update-handler))

;;; The environment-control structure is created once and maintains all of the
;;; top-level information about the state of all connected environments.

(defstruct environment-control
  address port connections handler-lock busy-flag stepper stepper-open windows pre-hook post-hook use-env-windows which-hook)

(defstruct stepper-control
  skip-type skip-val wait (handlers (make-hash-table)) current-event mode tutor-bindings tutor-responses)
  
(defvar *environment-control* (make-environment-control :address "127.0.0.1" :port 2621
                                                        :connections nil :busy-flag nil
                                                        :handler-lock (uni-make-lock "Environment Lock")
                                                        :stepper (make-stepper-control)))


;;; The environment-connection structure is created for each environment connection
;;; and maintains all of the information necessary for handling that connection.

(defstruct environment-connection
  stream local process (handlers (make-hash-table)) (hooks (make-hash-table)) sync)


(defun call-model-environment-hooks (hook &optional (value nil given))
  (setf (environment-control-which-hook *environment-control*) hook)
  
  (dolist (connection (environment-control-connections *environment-control*))
    (dolist (handler (gethash hook (environment-connection-hooks connection)))
      (when (or (eq (handler-model handler) (current-model))
                ;; Do I want to do it this way?  -- if there's no model set then just do it with current?
                (null (handler-model handler)))
        
        (update-handler handler (if given value handler)))))
  (setf (environment-control-which-hook *environment-control*) nil))


(defun call-model-environment-hooks-with-return (hook &optional (value nil given))
  (setf (environment-control-which-hook *environment-control*) hook)
  (let ((val nil))
    (dolist (connection (environment-control-connections *environment-control*))
      (dolist (handler (gethash hook (environment-connection-hooks connection)))
        (when (or (eq (handler-model handler) (current-model))
                  ;; Do I want to do it this way?  -- if there's no model set then just do it with current?
                  (null (handler-model handler)))
          
          (let ((r (update-handler handler (if given value handler))))
            (when r
              (setf val r))))))
    (setf (environment-control-which-hook *environment-control*) nil)
    val))

(defun call-all-environment-hooks (hook &optional (value nil given))
  
  (setf (environment-control-which-hook *environment-control*) hook)
  (dolist (connection (environment-control-connections *environment-control*))
    (dolist (handler (gethash hook (environment-connection-hooks connection)))
      (update-handler handler (if given value handler))))
  (setf (environment-control-which-hook *environment-control*) nil)
  )
  
(defun add-pre-hook-if-needed ()
  (unless (environment-control-pre-hook *environment-control*)
    (setf (environment-control-pre-hook *environment-control*)
      (add-pre-event-hook #'(lambda (event)
                              (call-all-environment-hooks 'pre event))))))

(defun add-post-hook-if-needed ()
  (unless (environment-control-post-hook *environment-control*)
    (setf (environment-control-post-hook *environment-control*)
      (add-post-event-hook #'(lambda (event)
                               (call-all-environment-hooks 'post event))))))

(defun delete-pre-hook-if-necessary ()
  (when (and (environment-control-pre-hook *environment-control*)
             (<= (length (mp-models)) 1))
    (delete-event-hook (environment-control-pre-hook *environment-control*))
    (setf (environment-control-pre-hook *environment-control*) nil)))

(defun delete-post-hook-if-necessary ()
  (when (and (environment-control-post-hook *environment-control*)
             (<= (length (mp-models)) 1))
    (delete-event-hook (environment-control-post-hook *environment-control*))
    (setf (environment-control-post-hook *environment-control*) nil)))

;;; These are the default address and port that will be used for connecting to 
;;; the ACT-R Environment.
;;; The port used on the Environment side is set in the 0-net-config.tcl file
;;; and also defaults to 2621.

(create-system-parameter :default-environment-port :valid-test 'posnum  :default-value 2621 
                         :warning "positive number"
                         :documentation "Default port for connecting to ACT-R Environment" 
                         :handler (simple-system-param-handler (environment-control-port *environment-control*)))

(create-system-parameter :default-environment-host :valid-test 'stringp :default-value "127.0.0.1"
                         :warning "string of an ipaddress or full host name"
                         :documentation "Default address for connecting to ACT-R Environment" 
                         :handler (simple-system-param-handler (environment-control-address *environment-control*)))



;;; close-connection
;;; This function takes one parameter which should be an environment-connection
;;; and a keywork parameter kill which defaults to t.  If kill is t, then
;;; it kills the process that is associated with that connection.  The only
;;; time one wouldn't want to kill the process is if the close comes from
;;; within the process itself.  It always removes all the handlers in the table 
;;; that are associated with that stream and then closes the stream.

(defun close-connection (connection &key (kill t))
  
  (when (and kill (environment-connection-process connection))
    (uni-process-kill (environment-connection-process connection)))
  
  (uni-without-interrupts
   (ignore-errors 
    (uni-send-string (environment-connection-stream connection) "close nil nil<end>")))
    
    
  (setf (environment-control-connections *environment-control*)
    (remove connection (environment-control-connections *environment-control*)))
  
  (maphash #'(lambda (key value) 
               (declare (ignore key))
               (delete-handler value connection))
           (environment-connection-handlers connection))
  
  (ignore-errors (close (environment-connection-stream connection))))

;;; close-all-connections
;;; This function takes no parameters. It kills all of the processes
;;; that are handling environment connections, closes all of the 
;;; sockets associated with them and removes all of the handlers from
;;; the table.  It also clears the stepper open flag just in case the
;;; environment was closed while a stepper was open.

(defun close-all-connections ()
  (dolist (connection (environment-control-connections *environment-control*))
    (close-connection connection))
  (setf (environment-control-stepper-open *environment-control*) nil))

  
;;; connect-to-environment 
;;; It takes 4 keyword parameters. Clean specifies whether or not to close 
;;; all open environment connections before making the new one, and it defaults 
;;; to t.  The keywords host and port specify the address of the Tcl environment 
;;; listening for a connection and default to the system-parameters 
;;; :default-environment-port and :default-environment-host.
;;; Background indicates whether the message-process function should
;;; be run in a separate thread and defaults to t, if it is nil then
;;; this command will not return until the environment connection is
;;; broken which is only useful for use with building a standalone
;;; environment.
;;; This function opens a socket connection to that Tcl environment, starts
;;; a process that will read and process the input on that socket, and returns
;;; an environment-connection which handles that connection.

(defun connect-to-environment (&key (clean t) (host nil) (port nil) (background t))
  
  (unless host
    (setf host (car (ssp :default-environment-host))))
  
  (unless port
    (setf port (car (ssp :default-environment-port))))
  
  (unless (numberp port)
    (print-warning "Port must be a number.")
    (return-from connect-to-environment nil))
  
  (unless (stringp host)
    (print-warning "Host must be a string.")
    (return-from connect-to-environment nil))
  
  (when clean
    (close-all-connections))
  
  (multiple-value-bind (s err) 
      (ignore-errors (uni-make-socket host port))
    (if (and (subtypep (type-of err) 'condition)
             (not (equal (type-of err) 'unbound-variable))) 
        (uni-report-error err "Unable to Connect")
      
      (let ((connection (make-environment-connection 
                         :stream s
                         :local (if (string-equal host "127.0.0.1") 1 0))))
        (if background
            (progn 
              (setf (environment-connection-process connection)
                (uni-run-process "Environment-Connection" 
                                 #'(lambda ()
                                     (message-process connection))))
              (push connection (environment-control-connections *environment-control*))
              connection)
          (progn 
            (push connection (environment-control-connections *environment-control*))
            (message-process connection)))))))

;;; This variable can be tested in a handler to determine 
;;; whether or not the current connection was a local one.

(defvar *local-connection* nil)


;;; message-process
;;; This function takes one parameter which is the connection to
;;; process messages for.
;;; It reads the Tcl->Lisp commands from the socket (see
;;; the messages.txt file for details) and creates a process to handle
;;; each one as it arrives.  If there is a connection error or the socket
;;; is closed then this function terminates.


(defun message-process (connection)
  (let ((old-string "") ;; be careful about new lines with Scott's read-line
        (input-stream (environment-connection-stream connection)))
    
    (loop 
      (unless (uni-wait-for-char input-stream)
        (return))
      
      ;; read a line from the socket
      (let ((current-string (multiple-value-bind (value condition)
                                (ignore-errors (uni-socket-read-line input-stream))
                              (if (subtypep (type-of condition) 'condition)
                                  (progn
                                    (uni-report-error condition "Read line failed")
                                    (format *error-output* "Environment Connection ended.~%")
                                    (close-connection connection :kill nil)
                                    (return))
                                value))))
        
          ;; check for the end marker because the line could have been split
          (if (not (search "<end>" current-string :test #'string-equal))
              (setf old-string (concatenate 'string old-string current-string))
            
            (progn
              (setf current-string (concatenate 'string old-string current-string))
              (setf old-string "")
              
              (multiple-value-bind (cmd-list condition)
                  (ignore-errors (read-from-string current-string nil 'problem))
                
              
                (cond ((subtypep (type-of condition) 'condition) ;; an error
                       (uni-report-error condition (format nil "Error reading from message: ~s" current-string))
                       ;; not closing connection anymore, maybe still should ?
                       ;(format *error-output* "Closing connection to environment~%")
                       ;(close-connection connection :kill nil)
                       ;(return)
                       )
                      
                      ((equal 'problem cmd-list) ;; not likely to occur now
                       (format *error-output* "Invalid environment message ~s ~%" current-string)
                       ;; don't close for this either, but again maybe it should
                       ;(close-connection connection :kill nil)
                       ;(return)
                       )
                      
                      ((listp cmd-list) ;; any list is assumed to be a good cmd
                       (let ((cmd-copy (copy-tree cmd-list)))
                         (uni-run-process "Environment-Handler" 
                                          #'(lambda () 
                                                (unwind-protect 
                                                    (progn
                                                      (uni-lock (environment-control-handler-lock *environment-control*))
                                                      (process-connection connection cmd-copy))
                                                  (progn
                                                    (uni-unlock (environment-control-handler-lock *environment-control*))))))))
                      (t ;; anything else 
                       (format *error-output* "Incorrect environment command recieved: ~S~%" current-string))))))))))


;;; process-connection
;;; This function takes 2 parameters connection which should be an environment-connection
;;; and cmd-list which is the list containing the message.  This function gets 
;;; called in a separate process for each message sent and does what the message requests:
;;; create a new handler, remove a handler, update a handler, or just keep the connection
;;; alive (an MCL issue).


(defun process-connection (connection cmd-list) 
  (let ((*local-connection* (environment-connection-local connection)))
    
    (case (car cmd-list) 
      (create ;; make a new handler instance and send a register + update back
       (if (or (= (length cmd-list) 6)(= (length cmd-list) 7))
           (let ((new-handler (make-instance (second cmd-list) 
                                :use-model (= (length cmd-list) 7)
                                :model (if (= (length cmd-list) 7) (seventh cmd-list) nil)
                                :socket (environment-connection-stream connection)
                                :object-name (third cmd-list) 
                                :target-name (fourth cmd-list)
                                :update-form (functionify (fifth cmd-list)))))
             (setf (gethash (name new-handler) (environment-connection-handlers connection)) new-handler)
             (send-register new-handler)
             (update-handler new-handler new-handler)
             (dolist (x (sixth cmd-list))
               (case x
                 ((pre post conflict conflict-nil create delete reset run-start run-end)
                  (push new-handler (gethash x (environment-connection-hooks connection))))
                 (t (model-warning "Invalid hook ~s for handler ~S" x cmd-list)))))
         (format *error-output* "Invalid create message: ~s" cmd-list)))
      (update ;; change the update form if requested and send an update back
       (cond ((= (length cmd-list) 2)
              (let ((handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                (when handler
                  (update-handler handler nil))
                ;; the when used to be an if but now it'll just
                ;; silently ignore removed handlers
                ;(format *error-output* "Warning: update for removed handler ~S~%" (second cmd-list))
                ))
             ((= (length cmd-list) 3)
              (let ((handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                (when handler
                  (setf (update-form handler) (functionify (third cmd-list)))
                  (update-handler handler nil))
                ;; the when used to be an if but now it'll just
                ;; silently ignore removed handlers
                ;(format *error-output* "Warning: update for removed handler ~S~%" (second cmd-list))
                ))
             (t
              (format *error-output* "Invalid update message: ~s" cmd-list))))
      (remove ;; take the handler off the lists and free its name
              ;; calling the optional end function if necessary
       (cond ((or (= (length cmd-list) 2) (= (length cmd-list) 3))
              (let ((handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                
                ;; This should be unnecessary now since it locks out handling 
                ;; and a create must finish before the remove could be processed
                ;(while (null handler)
                ;  (uni-process-system-events)
                ;  (setf handler (gethash (second cmd-list) (environment-connection-handlers connection))))
                
                (when (= (length cmd-list) 3)
                  (safe-evaluation (third cmd-list) handler))
                
                (delete-handler handler connection)))
             (t (format *error-output* "Invalid remove message: ~s" cmd-list))))
      (k-a ;; don't do anything - just to make sure the socket doesn't timeout
       (ignore-errors (uni-send-string (environment-connection-stream connection) (format nil "ka nil nil<end>~%"))))
      (sync
       (setf (environment-connection-sync connection) t))
      (goodbye ;; kill the connection
       (format t "Environment Closed~%")
       (close-connection connection :kill nil))
      (t 
       (format *error-output* "Invalid command request: ~s~%" cmd-list)))))

(defmethod safe-evaluation (form handler)
    (let ((model (aif (handler-model handler) it (current-model))))
    (if (and model (find model (mp-models)))
        
        (with-model-eval model
          (multiple-value-bind (result err)
              (ignore-errors (funcall (functionify form)))
            (if (and (subtypep (type-of err) 'condition)
                     (not (equal (type-of err) 'unbound-variable)))
                (progn
                  (format t "~S~%" (type-of err))
                  (uni-report-error err (format nil "Error in remove message for: ~S~%Removing: ~S~%Message: ~S~%" 
                                          (name handler) (obj-name handler) form))
                  
                  (values nil nil))
              (values result t))))
      (multiple-value-bind (result err)
          (ignore-errors (funcall (functionify form)))
        (if (and (subtypep (type-of err) 'condition)
                 (not (equal (type-of err) 'unbound-variable)))
            (progn
              (format t "~S~%" (type-of err))
              (uni-report-error err (format nil "Error in remove message for: ~S~%Removing: ~S~%Message: ~S~%" 
                                      (name handler) (obj-name handler) form))
              
              (values nil nil))
          (values result t))))))

;;; Primary functions to connect/disconnect the environment.

(defun start-environment ()
  (if (environment-control-connections *environment-control*)
      (format t "There is already a connection to the environment.~%You should either stop that first, or if you want to connect to a second or remote environment use the connect-to-environment command~%")
    (if (and (fboundp 'uni-run-process) (fboundp 'uni-make-socket))
        (connect-to-environment)
      (progn
        (print-warning "The ACT-R Environment cannot be used with the current Lisp.")
        (print-warning "Please see the docs/QuickStart.txt file for a list of compatable Lisp versions.")))))

(defun stop-environment ()
  (if (> (length (environment-control-connections *environment-control*)) 1)
      (format t "There is more than one environment currently connected.~%You must use either close-all-connections to stop them all or close-connection to stop a specific one.~%")
    (close-all-connections)))


(defun wait-for-environment (&optional (max-delay 10))
  "Send sync pulse to all current env connections and wait for all to respond or max-delay seconds to pass"
  (let ((start-time (get-internal-real-time)))
    
    (dolist (connection (environment-control-connections *environment-control*))
      (setf (environment-connection-sync connection) nil)
      (ignore-errors (uni-send-string (environment-connection-stream connection) (format nil "sync nil nil<end>~%"))))
    
    (uni-wait-for 
     (lambda ()
       (or (every 'environment-connection-sync (environment-control-connections *environment-control*))
           (> (/ (- (get-internal-real-time) start-time) internal-time-units-per-second) max-delay))))))


#+(and :ccl :windows)
(defun run-environment (&optional (delay 5))
  (run-program (namestring (translate-logical-pathname "ACT-R6:environment;start environment.exe")) nil :wait nil)
  (sleep delay)
  (while (null (start-environment)) (sleep delay)))

#+(and :ccl :linux)
(defun run-environment (&optional (delay 5))
  (let ((c (ccl::cd "."))) 
    (ccl::cd "ACT-R6:environment;GUI")
    (run-program "wish" (list "starter.tcl") :wait nil)
    (sleep delay)
    (while (null (start-environment)) (sleep delay))
    (ccl::cd c)))

#+(and :ccl :darwin)
(defun run-environment (&optional (delay 5))
  (let ((c (ccl::cd "."))) 
    (ccl::cd "ACT-R6:environment")
    (run-program (namestring (translate-logical-pathname "ACT-R6:environment;Start Environment OSX.app;Contents;MacOS;start-environment-osx")) nil :wait nil)
    (sleep delay)
    (while (null (start-environment)) (sleep delay))
    (ccl::cd c)))

#+(and :lispworks (or :win32 :win64) (or :lispworks5 :lispworks6.0))
(defun run-environment (&optional (delay 5))
  (sys:call-system "\"Start Environment.exe\"" :current-directory (translate-logical-pathname "ACT-R6:environment") :wait nil)
  (sleep delay)
  (while (null (start-environment)) (sleep delay)))

#+(and :lispworks :macosx)
(defun run-environment (&optional (delay 5))
  (sys:call-system (format nil "'~a/Start Environment OSX.app/Contents/MacOS/start-environment-osx'"
                     (namestring (translate-logical-pathname "ACT-R6:environment")))
                   :wait nil)
  (sleep delay)
  (while (null (start-environment)) (sleep delay)))


#+(and :allegro :mswindows)
(defun run-environment (&optional (delay 5))
  (let ((c (current-directory)))
    (chdir "ACT-R6:environment")
    (run-shell-command "\"Start Environment.exe\"" :wait nil)
    (chdir c))
  (sleep delay)
  (while (null (start-environment)) (sleep delay)))

#+(and :allegro :macosx)
(defun run-environment (&optional (delay 5))
  (let ((c (current-directory)))
    (chdir "ACT-R6:environment")
    (run-shell-command "'Start Environment OSX.app/Contents/MacOS/start-environment-osx'" :wait nil)
    (chdir c))
  (sleep delay)
  (while (null (start-environment)) (sleep delay)))


(unless (fboundp 'run-environment)
  (defun run-environment (&optional (delay 0))
    (declare (ignore delay))
    (print-warning "The run-environment command is not available for your current Lisp & OS combination.")))

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
