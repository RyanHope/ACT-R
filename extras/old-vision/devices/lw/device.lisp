;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename    : device.lisp                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author      : Cogworks Laboratory
;;               Section 3 based on mcl/device.lisp written by Mike Byrne
;; Address     : Department of Cognitive Science
;;               Carnegie Building
;;               Rensselaer Polytechnic Institute
;;               110 8th St
;;               Troy, NY 12180-3590
;;               schoem@rpi.edu
;;
;; Copyright   : (c)2003-2007 Cogworks Laboratory/RPI
;;
;; History:    8/27/07 MJS - Fixed several bugs concerning menubar height and
;;             cursor positioning
;; 2007.09.10  : Dan
;;             : * Added a device-update method that calls mp:process-allow-scheduling
;;             :   but still had issues with faster than real-time so may need to do
;;             :   something else there...
;; 2007.09.14 Dan
;;             : * Two changes to allow it to "work" in LispWorks 4.4 (doesn't
;;             :   work well since the fixation circle is opaque).
;;             :  - Changed get-border-height to use capi::simple-pane-window-styles
;;             :    which seems to work in either 4.4 or 5.0.
;;             :  - Changed capi:define-interface focus-win so that transparency
;;             :    is only added for LispWorks 5.
;;; 2007.10.10  : Dan
;;;             : * Applied David Peebles' change for the class of the text
;;;             :   items from capi:display-pane to capi:title-pane to improve
;;;             :   the appearance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of Contents
;; 1. Foreign Function definitions and functions
;; 2. virtual mcl - to be as compatable with existing stuff as possible
;; 3. Capi:interface and pinboard layout
;; 4. Code similar to  mcl device.lisp
;; 5. Windows implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Foreign Function Stuff;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Derived from file : "/usr/include/ppc/types.h"

(fli:define-c-typedef (int8-t (:foreign-name "int8_t")) (:signed :char))
(fli:define-c-typedef (u-int8-t (:foreign-name "u_int8_t"))
                      (:unsigned :char))
(fli:define-c-typedef (int16-t (:foreign-name "int16_t")) :short)
(fli:define-c-typedef (u-int16-t (:foreign-name "u_int16_t"))
                      (:unsigned :short))
(fli:define-c-typedef (int32-t (:foreign-name "int32_t")) :int)
(fli:define-c-typedef (u-int32-t (:foreign-name "u_int32_t"))
                      (:unsigned :int))
(fli:define-c-typedef (int64-t (:foreign-name "int64_t")) :long-long)
(fli:define-c-typedef (u-int64-t (:foreign-name "u_int64_t"))
                      (:unsigned :long-long))
(fli:define-c-typedef (register-t (:foreign-name "register_t")) int32-t)
(fli:define-c-typedef (intptr-t (:foreign-name "intptr_t")) :long)
(fli:define-c-typedef (uintptr-t (:foreign-name "uintptr_t"))
                      (:unsigned :long))

;;; Derived from file : "/usr/include/ppc/ansi.h"

(fli:define-c-typedef (--mbstate-t (:foreign-name "__mbstate_t"))
                      (:union
                       (--mbstate8 (:c-array :char 128))
                       (-mbstatel :long-long)))

;;; Derived from file : "/usr/include/stddef.h"

(fli:define-c-typedef (ptrdiff-t (:foreign-name "ptrdiff_t")) :int)
(fli:define-c-typedef (size-t (:foreign-name "size_t"))
                      (:unsigned :long))
(fli:define-c-typedef (rune-t (:foreign-name "rune_t")) :int)
(fli:define-c-typedef (wchar-t (:foreign-name "wchar_t")) :int)

;;; Derived from file : "/usr/include/gcc/darwin/3.1/stdint.h"

(fli:define-c-typedef (uint8-t (:foreign-name "uint8_t")) u-int8-t)
(fli:define-c-typedef (uint16-t (:foreign-name "uint16_t")) u-int16-t)
(fli:define-c-typedef (uint32-t (:foreign-name "uint32_t")) u-int32-t)
(fli:define-c-typedef (uint64-t (:foreign-name "uint64_t")) u-int64-t)
(fli:define-c-typedef (int-least8-t (:foreign-name "int_least8_t"))
                      int8-t)
(fli:define-c-typedef (int-least16-t (:foreign-name "int_least16_t"))
                      int16-t)
(fli:define-c-typedef (int-least32-t (:foreign-name "int_least32_t"))
                      int32-t)
(fli:define-c-typedef (int-least64-t (:foreign-name "int_least64_t"))
                      int64-t)
(fli:define-c-typedef (uint-least8-t (:foreign-name "uint_least8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-least16-t (:foreign-name "uint_least16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-least32-t (:foreign-name "uint_least32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-least64-t (:foreign-name "uint_least64_t"))
                      uint64-t)
(fli:define-c-typedef (int-fast8-t (:foreign-name "int_fast8_t"))
                      int8-t)
(fli:define-c-typedef (int-fast16-t (:foreign-name "int_fast16_t"))
                      int16-t)
(fli:define-c-typedef (int-fast32-t (:foreign-name "int_fast32_t"))
                      int32-t)
(fli:define-c-typedef (int-fast64-t (:foreign-name "int_fast64_t"))
                      int64-t)
(fli:define-c-typedef (uint-fast8-t (:foreign-name "uint_fast8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-fast16-t (:foreign-name "uint_fast16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-fast32-t (:foreign-name "uint_fast32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-fast64-t (:foreign-name "uint_fast64_t"))
                      uint64-t)
(fli:define-c-typedef (intmax-t (:foreign-name "intmax_t")) :long-long)
(fli:define-c-typedef (uintmax-t (:foreign-name "uintmax_t"))
                      (:unsigned :long-long))

;;; Derived from file : "/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGError.h"

(fli:define-c-enum (-cgerror (:foreign-name "_CGError"))
                   (kcgerrorsuccess 0)
                   (kcgerrorfirst 1000)
                   (kcgerrorfailure 1000)
                   (kcgerrorillegalargument 1001)
                   (kcgerrorinvalidconnection 1002)
                   (kcgerrorinvalidcontext 1003)
                   (kcgerrorcannotcomplete 1004)
                   (kcgerrornametoolong 1005)
                   (kcgerrornotimplemented 1006)
                   (kcgerrorrangecheck 1007)
                   (kcgerrortypecheck 1008)
                   (kcgerrornocurrentpoint 1009)
                   (kcgerrorinvalidoperation 1010)
                   (kcgerrornoneavailable 1011)
                   (kcgerrorapplicationrequiresnewersystem 1015)
                   (kcgerrorapplicationnotpermittedtoexecute 1016)
                   (kcgerrorlast 1015))
(fli:define-c-typedef (cgerror (:foreign-name "CGError")) int32-t)

;;; Derived from file : "/usr/include/mach/ppc/boolean.h"

(fli:define-c-typedef (boolean-t (:foreign-name "boolean_t")) :int)

;;; Derived from file : "/private/tmp/LWtemp.butane.3283.44.h"

(fli:define-c-struct (cgpoint (:foreign-name "CGPoint"))
                     (x :float)
                     (y :float))

(fli:define-c-typedef (cgpoint (:foreign-name "CGPoint"))
                      (:struct cgpoint))

(fli:define-c-struct (point (:foreign-name "Point"))
  (y :short)
  (x :short))
(fli:define-c-typedef (point (:foreign-name "Point"))
  (:struct point))


(fli:define-c-typedef (cgdisplaycount (:foreign-name "CGDisplayCount"))
                      uint32-t)
(fli:define-c-typedef (cgdisplayerr (:foreign-name "CGDisplayErr"))
                      cgerror)
(fli:define-c-typedef (cgeventerr (:foreign-name "CGEventErr"))
  cgerror)
(fli:define-c-struct (-cgdirectdisplayid
                      (:foreign-name "_CGDirectDisplayID")
                      (:forward-reference t)))
(fli:define-c-typedef (cgdirectdisplayid
                       (:foreign-name "CGDirectDisplayID"))
                      (:pointer (:struct -cgdirectdisplayid)))

(fli:define-c-typedef (cgcharcode (:foreign-name "CGCharCode")) :short)

(fli:define-c-typedef (cgkeycode (:foreign-name "CGKeyCode")) :short)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(fli:define-foreign-function (cgpostkeyboardevent "CGPostKeyboardEvent" :source)
    ((key-char cgcharcode)
     (v-key cgkeycode)
     (key-down (:boolean :int)))
  :result-type :int
  :language :c)

(fli:define-foreign-function (cgdisplaymovecursortopoint "CGDisplayMoveCursorToPoint"
                                                         :source)
                             ((display cgdirectdisplayid)
                              (point cgpoint))
                             :result-type
                             cgdisplayerr
                             :language
                             :c)

(fli:define-foreign-function (cggetactivedisplaylist
                              "CGGetActiveDisplayList"
                              :source)
                             ((maxdisplays cgdisplaycount)
                              (activedspys
                               (:pointer cgdirectdisplayid))
                              (dspycnt (:pointer cgdisplaycount)))
                             :result-type
                             cgdisplayerr
                             :language
                             :c)

(fli:define-c-typedef (cgmousedelta (:foreign-name "CGMouseDelta")) int32-t)

(fli:define-foreign-function (cggetlastmousedelta "CGGetLastMouseDelta" :source)
                             ((deltax (:pointer cgmousedelta))
                              (deltay (:pointer cgmousedelta)))
                             :result-type cgdisplayerr
                             :language :c)

(fli:define-c-typedef (cgbuttoncount (:foreign-name "CGButtonCount")) :int)

(fli:define-foreign-function (getglobalmouse "GetGlobalMouse" :source)
    ((p (:pointer point)))
  :result-type :void
  :language :c)

(fli:define-foreign-function (cgpostmouseevent "CGPostMouseEvent" :source)
    ((mouse-position cgpoint)
     (update-mouse-position (:boolean :int))
     (button-count cgbuttoncount)
     (mouse-button-down (:boolean :int)))
  :result-type :int
  :language :c)

(fli:define-foreign-function (getcurrenteventbuttonstate "GetCurrentEventButtonState" :source)
    ()
  :result-type u-int32-t
  :language :c)

(fli:define-foreign-function (hidemenubar "HideMenuBar" :source)
    ()
  :result-type :void
  :language :c)

(fli:define-foreign-function (showmenubar "ShowMenuBar" :source)
    ()
  :result-type :void
  :language :c)

(fli:define-foreign-function (cgcapturealldisplays "CGCaptureAllDisplays" :source)
    ()
  :result-type cgdisplayerr
  :language :c)

(fli:define-foreign-function (cgreleasealldisplays "CGReleaseAllDisplays" :source)
    ()
  :result-type cgdisplayerr
  :language :c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VoiceSpec Structure
(fli:define-c-struct (voice-spec (:foreign-name "VoiceSpec"))
  (creator :int)
  (id :int))

(fli:define-c-typedef (voice-spec (:foreign-name "VoiceSpec"))
  (:struct voice-spec))

;;; Speech Channel Structure
(fli:define-c-struct (speech-channel-record (:foreign-name "SpeechChannelRecord"))
  (data (:c-array :long 1)))

(fli:define-c-typedef (speech-channel-record (:foreign-name "SpeechChannelRecord"))
  (:struct speech-channel-record))

(fli:define-c-typedef (speech-channel (:foreign-name "SpeechChannel"))
  (:struct speech-channel-record))

;;; GetIndVoice Function
;;; Fills VoiceSpec structure with correct information
(fli:define-foreign-function (get-ind-voice  "GetIndVoice" :source)
    ((index :short)
     (voice (:pointer voice-spec)))
  :result-type :int
  :language :c)

;;; NewSpeechChannel Function
;;; Creates a new speech channel with specified voice spec
(fli:define-foreign-function (new-speech-channel "NewSpeechChannel" :source)
    ((v-s (:pointer voice-spec))
     (s-c (:pointer speech-channel)))
  :result-type :int
  :language :c)

;;; SpeakText Function
;;; Speaks string of specified length on speech channel s-c
(fli:define-foreign-function (speak-text "SpeakText" :source)
    ((s-c speech-channel)
     (string (:reference-pass :ef-mb-string))
     (length :int))
  :result-type :int
  :language :c)

(fli:define-foreign-function (sys-err "SysError" :source)
    ((error-code :short))
  :language :c)

;;; Disposes of speech channel s-c
(fli:define-foreign-function (dispose-speech-channel "DisposeSpeechChannel" :source)
    ((s-c speech-channel))
  :result-type :int
  :language :c)

;;; Counts the number of voices
(fli:define-foreign-function (count-voices "CountVoices" :source)
    ((num-voices (:pointer :short)))
  :result-type :int
  :language :c)

;;; Determines if speech is being synthesized elsewhere
;;; 0 if it is not
;;; 1 if it is
(fli:define-foreign-function (speech-busy "SpeechBusy" :source)
    ()
  :result-type :short
  :language :c)


;;;;;;;;;;;;;;;;;;;;;
;; Lisp FF Callers ;;
;;;;;;;;;;;;;;;;;;;;;

(defun get-button-state ()
  (getcurrenteventbuttonstate))

(defun get-active-display ()
  (fli:with-dynamic-foreign-objects ((activedspys cgdirectdisplayid)
                                     (dspycnt cgdisplaycount))
    (cggetactivedisplaylist 1 activedspys dspycnt)
    (fli:dereference activedspys)))

(defun move-cursor-to-point (x y)
  (fli:with-dynamic-foreign-objects ()
    (let ((point (fli:allocate-foreign-object :type 'cgpoint)))
      (setf (fli:foreign-slot-value point 'x) (float x)
            (fli:foreign-slot-value point 'y) (float y))
      (cgdisplaymovecursortopoint (get-active-display) (fli:dereference point :copy-foreign-object nil))
      )))

(defun lw-move-cursor (x y)
  (move-cursor-to-point x y))

(defun capture-all-displays ()
  (fli:with-dynamic-foreign-objects ()
    (cgcapturealldisplays)))

(defun release-all-displays ()
  (fli:with-dynamic-foreign-objects ()
    (cgreleasealldisplays)))

(defun mouse-position ()
  (fli:with-dynamic-foreign-objects ()
    (let ((point (fli:allocate-foreign-object :type 'point)))
     (getglobalmouse point)
     (list (fli:foreign-slot-value point 'x) (fli:foreign-slot-value point 'y)))))


(defun lw-click-mouse ()
  (fli:with-dynamic-foreign-objects ((point point)
                                     (cg-mouse-point cgpoint))
      (getglobalmouse point)
      (setf (fli:foreign-slot-value cg-mouse-point 'x)
            (float (fli:foreign-slot-value point 'x)))
      (setf (fli:foreign-slot-value cg-mouse-point 'y)
            (float (fli:foreign-slot-value point 'y)))
      (cgpostmouseevent (fli:dereference 
                         cg-mouse-point 
                         :copy-foreign-object nil) t 1 t)
      (cgpostmouseevent (fli:dereference 
                         cg-mouse-point 
                         :copy-foreign-object nil) t 1 nil)
      ))

(defun hide-menu-bar ()
  (fli:with-dynamic-foreign-objects ()
    (hidemenubar))
  (setf (title-bar (current-device)) nil))

(defun show-menu-bar ()
  (fli:with-dynamic-foreign-objects ()
    (showmenubar))
  (setf (title-bar (current-device)) t))

;;;;;;;;;;;;;;;;;;
;; Speech Stuff ;;
;;;;;;;;;;;;;;;;;;

;;; Checks availible voices to see if selected voice ID exists
(defun check-voices (voice-num)
  (block nil
    (let ((voices (fli:allocate-foreign-object :type :short)))
      (count-voices voices)
      (if (or (> voice-num (fli:dereference voices))
              (< voice-num 1))
          (progn
            (format t "ERROR: MUST SELECT A VOICE BETWEEN 1 AND ~A" 
                    (fli:dereference voices))
            (return nil)))
      t)))

;;; Checks if other speech is being synthesized
;;; if so, waits until it finishes
(defun other-speech-being-synthesized ()
  (do
      ((busy (speech-busy)))
      ((= busy 0))
    (sleep 0.5)
    (setf busy (speech-busy))))

;;; Allocates memory to a voice spec and returns the pointer to it
(defun return-voice-spec ()
  (fli:allocate-foreign-object :type 'voice-spec))

;;; Deallocates memory associated with voice spec vs
(defun free-voice-spec (vs)
  (fli:free-foreign-object vs))

;;; Allocates memory to a speech channel and returns the pointer to it
(defun return-speech-channel ()
  (fli:allocate-foreign-object :type 'speech-channel))

;;; Deallocates memory associated with speech channel sc
(defun free-speech-channel (sc)
  (fli:free-foreign-object sc))

;;; Deallocates memory for both the speech channel and the voice-spec
(defun free-all-memory (sc vs)
  (free-speech-channel sc)
  (free-voice-spec vs))

;;; Disposes of speech channel sc when no longer wanted
(defun kill-speech-channel (sc)
  (dispose-speech-channel (fli:dereference sc :copy-foreign-object nil)))

;;; Creates a new speech channel based on voice-id
;;; structs voice-spc and speech-ch are filled after it executes
;;; Returns nil if invalid voice is selected
(defun create-new-speech-channel (voice-id voice-spc speech-ch)
  (if (check-voices voice-id)
      (progn
        (get-ind-voice voice-id voice-spc)
        (new-speech-channel voice-spc speech-ch))))

;;; Speaks str using on speech channel speech-ch
;;; Voice ID and VoiceSpec must have been initialized already
(defun speak-phrase (str speech-ch)
  ;; Waits for any other speech sythesis to finish
  (other-speech-being-synthesized)

  ;; Because :copy-foreign-object is nil, returns object directly
  ;; just as SpeakText function needs
  (speak-text (fli:dereference speech-ch :copy-foreign-object nil) 
              str 
              (length str)))
  
;;; All-in-one function -> keeps all variables local
;;; Speak the given string str with the voice of ID voice-num
;;; Will return nil if an invalid voice is selected
(defun lw-speak (str &key (voice 19))
  (block nil
    (let ((sc (return-speech-channel))
          (vs (return-voice-spec)))
 
      ;; Checks to see if voice selected exists
      (if (equal 0 (create-new-speech-channel voice vs sc))
          (speak-phrase str sc)
        (return nil))
      
      ;; Waits for current speech sythesis to finish
      (other-speech-being-synthesized)

      ;; Disposes of speech channel and frees memory
      (kill-speech-channel sc)
      (free-voice-spec vs)
      (free-speech-channel sc))
    t))

(defun beep ()
  (capi:beep-pane))
;;;
;;;             Keypress
;;;
(defparameter keycodes nil)

;;; Initializes hash table for virtual key codes
(defun init-hashtbl ()
  (setf keycodes  (make-hash-table))
  (setf (gethash (char-code (aref "a" 0)) keycodes) 0)
  (setf (gethash (char-code (aref "s" 0)) keycodes) 1)
  (setf (gethash (char-code (aref "d" 0)) keycodes) 2)
  (setf (gethash (char-code (aref "f" 0)) keycodes) 3)
  (setf (gethash (char-code (aref "g" 0)) keycodes) 5)
  (setf (gethash (char-code (aref "h" 0)) keycodes) 4)
  (setf (gethash (char-code (aref "j" 0)) keycodes) 38)
  (setf (gethash (char-code (aref "k" 0)) keycodes) 40)
  (setf (gethash (char-code (aref "l" 0)) keycodes) 37)
  (setf (gethash (char-code (aref ";" 0)) keycodes) 41)
  (setf (gethash (char-code (aref "'" 0)) keycodes) 39)

  (setf (gethash (char-code (aref "q" 0)) keycodes) 12)
  (setf (gethash (char-code (aref "w" 0)) keycodes) 13)
  (setf (gethash (char-code (aref "e" 0)) keycodes) 14)
  (setf (gethash (char-code (aref "r" 0)) keycodes) 15)
  (setf (gethash (char-code (aref "t" 0)) keycodes) 17)
  (setf (gethash (char-code (aref "y" 0)) keycodes) 16)
  (setf (gethash (char-code (aref "u" 0)) keycodes) 32)
  (setf (gethash (char-code (aref "i" 0)) keycodes) 34)
  (setf (gethash (char-code (aref "o" 0)) keycodes) 31)
  (setf (gethash (char-code (aref "p" 0)) keycodes) 35)
  (setf (gethash (char-code (aref "[" 0)) keycodes) 33)
  (setf (gethash (char-code (aref "]" 0)) keycodes) 30)
  (setf (gethash (char-code (aref "\ " 0)) keycodes) 42)

  (setf (gethash (char-code (aref "z" 0)) keycodes) 6)
  (setf (gethash (char-code (aref "x" 0)) keycodes) 7)
  (setf (gethash (char-code (aref "c" 0)) keycodes) 8)
  (setf (gethash (char-code (aref "v" 0)) keycodes) 9)
  (setf (gethash (char-code (aref "b" 0)) keycodes) 11)
  (setf (gethash (char-code (aref "n" 0)) keycodes) 45)
  (setf (gethash (char-code (aref "m" 0)) keycodes) 46)
  (setf (gethash (char-code (aref "," 0)) keycodes) 43)
  (setf (gethash (char-code (aref "." 0)) keycodes) 47)
  (setf (gethash (char-code (aref "/" 0)) keycodes) 44)

  (setf (gethash (char-code (aref "`" 0)) keycodes) 50)
  (setf (gethash (char-code (aref "1" 0)) keycodes) 18)
  (setf (gethash (char-code (aref "2" 0)) keycodes) 19)
  (setf (gethash (char-code (aref "3" 0)) keycodes) 20)
  (setf (gethash (char-code (aref "4" 0)) keycodes) 21)
  (setf (gethash (char-code (aref "5" 0)) keycodes) 23)
  (setf (gethash (char-code (aref "6" 0)) keycodes) 22)
  (setf (gethash (char-code (aref "7" 0)) keycodes) 26)
  (setf (gethash (char-code (aref "8" 0)) keycodes) 28)
  (setf (gethash (char-code (aref "9" 0)) keycodes) 25)
  (setf (gethash (char-code (aref "0" 0)) keycodes) 29)
  (setf (gethash (char-code (aref "-" 0)) keycodes) 27)
  (setf (gethash (char-code (aref "=" 0)) keycodes) 24)
)

(init-hashtbl)

;;; Presses and releases key ch (ch must be a string)
;;; ex: (key-press "a")
(defun key-press (ch) 
  (let ()
    (cond
     ((equal ch "return")  (press-specified-key 36))
     ((equal ch "space")  (press-specified-key 49))
     ((= 1 (length ch)) 
      (press-specified-key (gethash (char-code (aref ch 0)) keycodes)))
     (t '(ERROR NOT VALID STRING)))))

;;; Takes a list of strings such as '("apple" "q")
;;; equates to pressing both apple and q keys
(defun press-key-combo (str-lst)
  (if (stringp (car str-lst))
      (let
          ((ch (car str-lst)))
        (cond
         ((equal ch "apple")  (progn (cgpostkeyboardevent 1 55 t) 
                                (press-key-combo (cdr str-lst))
                                (cgpostkeyboardevent 1 55 nil)))
         ((equal ch "option")  (progn (cgpostkeyboardevent 1 58 t) 
                                  (press-key-combo (cdr str-lst))
                                  (cgpostkeyboardevent 1 58 nil)))
         ((equal ch "control")  (progn (cgpostkeyboardevent 1 59 1) 
                                 (press-key-combo (cdr str-lst))
                                 (cgpostkeyboardevent 1 59 nil)))
         ((equal ch "shift")  (progn (cgpostkeyboardevent 1 56 t) 
                                (press-key-combo (cdr str-lst))
                                (cgpostkeyboardevent 1 56 nil)))
         (t  (progn 
               (cgpostkeyboardevent 1 (gethash (char-code (aref ch 0)) keycodes) t) 
               (press-key-combo (cdr str-lst))
               (cgpostkeyboardevent 1 (gethash (char-code (aref ch 0)) keycodes) nil)
               ))))))


;;; Presses and releases key k
(defun press-specified-key (k)
  (cgpostkeyboardevent 1 k t)
  (cgpostkeyboardevent 1 k nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  virtual mcl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro make-point (h v)
  `(vector ,h ,v))

(defmacro point-h (pt)
  `(svref ,pt 0))

(defmacro point-v (pt)
  `(svref ,pt 1))

(defmacro add-points (p1 p2)
  `(vector (+ (svref ,p1 0) (svref ,p2 0)) (+ (svref ,p1 1) (svref ,p2 1))))

(defmacro subtract-points (p1 p2)
  `(vector (- (svref ,p1 0) (svref ,p2 0)) (+ (svref ,p1 1) (svref ,p2 1))))

(defmacro p2vpt (p) `(symbol-value ,p))

(defmacro vpt2p (p) `(symbol-value ,p))

(defmacro line-to (view end-pt)
  `(progn
     (gp:draw-line (pinboard ,view) (px (pen-position ,view)) (py (pen-position ,view)) (px ,end-pt) (py ,end-pt))
     (setf (pen-position ,view) ,end-pt)))

(defmacro cdialog-item-text (view) `(capi:item-text ,view))

(defmacro view-font (view) `(capi:simple-pane-font ,view))

(defmacro font-info (&optional font-spec a-pane)
  `(values (gp:get-font-ascent (aif ,a-pane it (current-device)) ,font-spec)
           (gp:get-font-descent (aif ,a-pane it (current-device)) ,font-spec)
           (gp:get-font-width (aif ,a-pane it (current-device)) ,font-spec)
           0))

(defmacro string-width (str &optional font-spec)
  `(multiple-value-bind (left top right bottom)
       (gp:get-string-extent (current-device)  ,str ,font-spec)
     (declare (ignore top bottom))
     (- right left)))

(defmacro color-symbol->system-color (color)
  `(read-from-string (format nil ":~S" ,color)))

(defmacro system-color->symbol (color)
  `(read-from-string (symbol-name ,color)))

(defmacro radio-button-pushed-p (button)
  `(capi:button-selected ,button))

(defmacro check-box-checked-p (button)
  `(capi:button-selected ,button))

(defmacro window-container (item)
  `(capi:element-interface ,item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------------------------------------
;; Define an ellipse pinboard object
;;----------------------------------------------------------------------------
;; WITH-XOR makes all graphics-port operations done within its body be drawn
;; using exclusive-or.
(defmacro with-xor ((port) &body body)
  `(gp:with-graphics-state (,port
                            :foreground (gp:compute-xor-pixel ,port)
			    :operation boole-xor)
    ,@body))

(defclass ellipse (capi:pinboard-object)
  ((foreground :accessor foreground :initform nil :initarg :foreground)
   (filled :accessor filled :initform nil :initarg :filled))
  (:default-initargs
   :visible-min-width 10
   :visible-min-height 10))

(defmethod capi:draw-pinboard-object (pinboard (ellipse ellipse) &key)
  (capi:with-geometry ellipse
    (let ((x-radius (floor (1- capi:%width%) 2))
          (y-radius (floor (1- capi:%height%) 2)))
(with-xor (pinboard)
      (gp:draw-ellipse pinboard
		       (+ capi:%x% x-radius)
                       (+ capi:%y% y-radius)
                       x-radius
                       y-radius
		       :foreground (or (foreground ellipse)
                                       (capi:simple-pane-foreground pinboard))
                       :filled (filled ellipse))))) )

(defmethod draw-object-outline (pinboard (ellipse ellipse) x y width height)
  (let ((x-radius (floor width 2))
        (y-radius (floor height 2)))
    (with-xor (pinboard)
      (gp:draw-ellipse pinboard 
                       (+ x x-radius)
                       (+ y y-radius)
                       x-radius
                       y-radius))))

(capi:define-interface window ()
  ((pen-position :initform (vector 0 0) :initarg :pen-position :accessor pen-position)
   (eye :initform nil :accessor eye)
   (ring :initform nil  :accessor ring))
  (:layouts
   (pinboard capi:pinboard-layout  ()
    :draw-pinboard-objects :local-buffer :accessor pinboard 
    :input-model '(((:button-1 :press)   view-click-event-handler)
                   (:character key-callback)
#-:win32                   ((:key :press) key-callback)
        ) ))                
  (:default-initargs
   :layout 'pinboard))

(defmethod key-callback ((self capi:pinboard-layout) x y  key)
  (declare (ignore x y)) 
  (view-key-event-handler (capi:element-interface self) key))

(defmethod add-object-to-screen  ((scr window) obj col)
  (when (null (slot-value scr obj))
    (setf (slot-value scr obj) (make-instance 'ellipse :foreground col))
    (let ((pinboard (pinboard scr)))
      (capi:apply-in-pane-process pinboard
                           #'(lambda(x)
                               (capi:manipulate-pinboard pinboard x :add-top)
                               (setf (capi:pinboard-pane-position x)  (values 0 0))
                               (setf (capi:pinboard-pane-size x) (values 10  10)))
                           (slot-value scr obj))
    )))

(defmethod move-pb-obj ((scr window) obj  x y)
   (let ((pinboard (pinboard scr)))
     (capi:apply-in-pane-process pinboard
                           #'(lambda(obj x y)
                               (setf (capi:pinboard-pane-position obj) (values (- x 6) (- y 0))))
                           obj x y)))

(defmethod view-position ((view capi:simple-pane))
  (capi:with-geometry view (vector capi:%x% capi:%y%)))

(defmethod view-size ((view capi:simple-pane))
  (capi:with-geometry view (vector capi:%width% capi:%height%)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; device.lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *crosshair-cursor* nil "not supported")

(defvar *attn-tracker* nil "Holds the view for the focus ring.")
(defparameter *last-update* (get-internal-real-time))

;;;; RPM-LINE-TO is meant to be called in the VIEW-DRAW-CONTENTS method for
;;;; window, which should be called after PM-PROC-DISPLAY.  I know this is
;;;; is stupid, but I'm having trouble getting this to work right with views.

;;; RPM-LINE-TO      [Function]
;;; Date        : 97.03.31
;;; Description : For use in building interfaces.  If there is to be a lot of
;;;             : drawing, then there will probably be a lot of LINE-TO 
;;;             : commands.  After making the window in question the current
;;;             : RPM window (via RPM-INSTALL-WINDOW), then call this function
;;;             : instead.  Note that the point should be in the coordinate
;;;             : system of the window.

(defun rpm-line-to (the-view h &optional v)
  "Adds a line to the icon and draws in in the default window. Designed to be called by the VIEW-DRAW-CONTENTS method for the window."
  (let* (;DAN (the-wind (device (device-interface *mp*)))
         (the-wind (current-device))
         (start-point (pen-position the-wind))
         (end-point (if v (make-point h v) h))
         (the-offset (view-position the-view)))
    (line-to the-view end-point)
    (rpm-build-line (p2vpt (add-points start-point the-offset))
                    (p2vpt (add-points the-offset end-point)))
    end-point))


;;; RPM-BUILD-LINE      [Function]
;;; Date        : 97.03.31
;;; Description : Called by RPM-LINE-TO, this creates an icon feature based 
;;;             : on the beginning and end points.

(defun rpm-build-line (start-pt end-pt)
  "Build a line feature in the visual interface"
  ;  (declare (point start-pt) (point end-pt))
  (push
   (make-instance 'line-feature
     :end1-x (px start-pt) :end1-y (py start-pt)
     :end2-x (px end-pt) :end2-y (py end-pt)
     :x (loc-avg (px start-pt) (py end-pt))
     :y (loc-avg (px start-pt) (py end-pt))
     :width (abs (- (px start-pt) (py end-pt)))
     :height (abs (- (px start-pt) (py end-pt))))
   ; DAN (visicon (vis-m *mp*))))
   (visicon (get-module :vision ))))

(defun loc-avg (x y)
  "Return the 'location' (integer) average of <x> and <y>."
  (declare (fixnum x) (fixnum y))
  (floor (/ (+ x y) 2)))


;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; LW screen-to-icon interface
;;;; ---------------------------------------------------------------------- ;;;;;;;


;;; BUILD-FEATURES-FOR      [Method]
;;; Description : For an MCL window, just walk the sub-objects (which by
;;;             : default are subviews) and build features.

(defmethod build-features-for ((self window) (vis-mod vision-module))  
  (let ((base-ls (flatten
                    (mapcar #'(lambda (obj) (build-features-for obj vis-mod))
                            (get-sub-objects self)))))
    (dolist (feat base-ls)
      (fill-default-dimensions feat))
    base-ls))


(defgeneric get-sub-objects (window)
  (:documentation  "Grabbing the sub-objects of a view by default returns the subviews."))

(defmethod get-sub-objects ((v window))
  (capi:layout-description (pinboard  v)))

#|
;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 97.04.02
;;; Description : Builder of features for a generic view.  Doesn't do anything
;;;             : based on the view itself, but it will call the 
;;;             : feature-builder on all subviews.

(defmethod build-features-for ((self view) (vis-mod vision-module))
  (let ((subs (get-sub-objects self))
        (outlis nil))
    (dolist (sub subs outlis)
      (push (build-features-for sub vis-mod) outlis))))

|#

;;; BUILD-FEATURES-FOR      [Method]
;;; Description : The basic method for building features, returns an object
;;;             : of the ICON-FEATURE class with the location and ISA fields
;;;             : set.

(defmethod build-features-for ((self capi:simple-pane) (vis-mod vision-module))
  (declare (ignore vis-mod))
  (make-instance 'icon-feature
    :x (px (view-loc self))
    :y (py (view-loc self))
    :kind 'visual-object
    :value 'unknown
    :screen-obj self))




;;; BUILD-FEATURES-FOR      [Method]
;;; Description : Same as for EDIT-BOXes.

(defmethod build-features-for ((self capi:text-input-pane) ;;editable-text-dialog-item
                                  (vis-mod vision-module))
  (let ((font-spec (view-font self))
        (text (capi:text-input-pane-text self)))  ;dialog-item-text
    (cons
     (make-instance 'rect-feature 
       :x (px (view-loc self))
       :y (py (view-loc self))
       :screen-obj self)
     (unless (equal text "")
       (multiple-value-bind (ascent descent)
                            (font-info font-spec)
         (build-string-feats vis-mod :text text
                             :start-x (1+ (point-h (view-position self)))
                             :y-pos (+ (point-v (view-position self))
                                       descent (round ascent 2))
                             :width-fct #'(lambda (str)
                                            (string-width str font-spec))
                             :height ascent :obj self))))))


;;; BUILD-FEATURES-FOR      [Method]
;;; Description : A button dialog item is a lot like a static text item,
;;;             : except there's an oval associated with it and the text
;;;             : is centered both horizontally and vertically.

(defmethod build-features-for ((self capi:push-button)   ;;button-dialog-item
                                  (vis-mod vision-module))
  (let ((btn-width (point-h (view-size self)))
        (btn-height (point-v (view-size self)))
        (text (cdialog-item-text  self)))  

    (cons
     (make-instance 'oval-feature 
       :x (px (view-loc self)) :y (py (view-loc self))
       :width btn-width :height btn-height :color 'light-gray
       :screen-obj self) 
     (unless (equal text "")
       (let* ((font-spec (view-font self))
              (start-y nil)
              (accum nil)
              (textlines (string-to-lines text))
              (width-fct #'(lambda (str) (string-width str font-spec))))
         (multiple-value-bind (ascent descent) (font-info font-spec)
           (setf start-y (+ (point-v (view-position self))
                            (round (- btn-height (* (length textlines)
                                                    (+ ascent descent))) 2)))
           (dolist (item textlines (nreverse accum))
             (push
              (build-string-feats vis-mod :text item
                                  :start-x 
                                  (+ (point-h (view-position self))
                                     (round 
                                      (- btn-width (funcall width-fct item))
                                      2))
                                  :y-pos 
                                  (+ start-y (round (+ ascent descent) 2))
                                  :width-fct width-fct 
                                  :height (min ascent btn-height)
                                  :obj self)
              accum)
             (incf start-y (+ ascent descent)))))))))



;;; BUILD-FEATURES-FOR      [Method]
;;; Description : A radio button is like a regular button, except that the
;;;             : oval is small and might be gray [unselected] or gray
;;;             : [selected].  Text is also not horizontally centered.

(defmethod build-features-for ((self capi:radio-button)  ;;radio-button-dialog-item
                                  (vis-mod vision-module))
  (let* ((btn-height (point-v (view-size self)))
         (text (cdialog-item-text self)))
    (cons
     (make-instance 'oval-feature 
       :x (+ 7 (point-h (view-position self))) :y (py (view-loc self))
       :width 11 :height 11 :screen-obj self
       :color (if (radio-button-pushed-p self)
                'black
                'light-gray))
     (unless (equal text "")
       (let* ((font-spec (view-font self))
              (start-y nil)
              (accum nil)
              (textlines (string-to-lines text))
              (width-fct #'(lambda (str) (string-width str font-spec))))
         (multiple-value-bind (ascent descent) (font-info font-spec)
           (setf start-y (+ (point-v (view-position self))
                            (round (- btn-height (* (length textlines)
                                                    (+ ascent descent))) 2)))
           (dolist (item textlines (nreverse accum))
             (push
              (build-string-feats vis-mod :text item
                                  :start-x 
                                  (+ (point-h (view-position self))
                                     17)
                                  :y-pos 
                                  (+ start-y (round (+ ascent descent) 2))
                                  :width-fct width-fct 
                                  :height (min ascent btn-height)
                                  :obj self)
              accum)
             (incf start-y (+ ascent descent)))))))))


;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 02.04.16
;;; Description : Very much like radio buttons, but if checked add an 
;;;             : "X" to the output.

(defmethod build-features-for ((self capi:check-button) ;;check-box-dialog-item
                                  (vis-mod vision-module))
  (let ((btn-height (point-v (view-size self)))
        (text (cdialog-item-text self))
        (feats nil))
    (setf feats
          (cons
           (make-instance 'rect-feature 
             :x (+ 8 (point-h (view-position self))) :y (py (view-loc self))
             :width 11 :height 11 :color 'light-gray
             :screen-obj self)
           (unless (equal text "")
             (let* ((font-spec (view-font self))
                    (start-y nil)
                    (accum nil)
                    (textlines (string-to-lines text))
                    (width-fct #'(lambda (str) (string-width str font-spec))))
               (multiple-value-bind (ascent descent) (font-info font-spec)
                 (setf start-y (+ (point-v (view-position self))
                                  (round (- btn-height (* (length textlines)
                                                          (+ ascent descent))) 2)))
                 (dolist (item textlines (nreverse accum))
                   (push
                    (build-string-feats vis-mod :text item
                                        :start-x 
                                        (+ (point-h (view-position self))
                                           17)
                                        :y-pos 
                                        (+ start-y (round (+ ascent descent) 2))
                                        :width-fct width-fct 
                                        :height (min ascent btn-height)
                                        :obj self)
                    accum)
                   (incf start-y (+ ascent descent))))))))
    (when (check-box-checked-p self)
      (setf feats
            (cons
             (make-instance 'icon-feature
               :x (+ 8 (point-h (view-position self)))
               :y (py (view-loc self))
               :kind 'visual-object
               :value 'check
               :screen-obj self
               :height 11
               :width 11)               
             
             feats)))
    feats
    ))


(defmethod approach-width ((feat text-feature) (theta number))
  (let ((screen-obj (screen-obj feat)))
    (if (button-p screen-obj)
      (let ((new-feat 
             (find-if #'(lambda (f) (and (eq (screen-obj f) screen-obj)
                                          (eq (kind f) 'oval)))
                      ;DAN (visicon (vis-m *mp*)))))
                      (visicon (get-module :vision)))))
        (if new-feat
          (approach-width new-feat theta)
          (call-next-method)))
      (call-next-method))))


(defmethod button-p (obj)
  (declare (ignore obj))
  nil)

(defmethod button-p ((obj capi:button)) ;;button-dialog-item
  (declare (ignore obj))
  t)

;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 99.04.02
;;; Description : A static text dialog item is really just text, so just
;;;             : build the string features for it.

(defmethod build-features-for ((self capi:title-pane) ;;static-text-dialog-item
                               (vis-mod vision-module))
  (let ((font-spec (view-font self))
        (text (capi:title-pane-text self))) ;;;dialog-item-text
    (unless (equal text "")
      (multiple-value-bind (ascent descent)
          (font-info  font-spec self)
        (build-string-feats vis-mod :text text
                            :start-x (1+ (point-h (view-position self)))  ;;;;(xstart self)
                            :y-pos (+ (point-v (view-position self))
                                      descent (round ascent 2))
                            :width-fct #'(lambda (str)
                                           (string-width str font-spec))
                            :height ascent :obj self)))))



#|
(defmethod xstart ((self static-text-dialog-item))
  (let ((left-x (point-h (view-position self)))
        (text-width (string-width (dialog-item-text self)
                                  (view-font self)))
        (text-just
         (or (cdr (assq (slot-value self 'ccl::text-justification)
                        '((:left . #.#$tejustleft)
                          (:center . #.#$tejustcenter)
                          (:right . #.#$tejustright))))
             (require-type (slot-value self 'ccl::text-justification) 'fixnum)))
        )
    (ecase text-just
      (#.#$teJustLeft (1+ left-x))
      (#.#$teJustCenter (+ 1 left-x (round (/ (- (width self) text-width) 2))))
      (#.#$teJustRight (+ 1 left-x (- (width self) text-width))))))
 

;;; CURSOR-TO-FEATURE      [Function]
;;; Date        : 97.06.16
;;; Description : Returns a feature representing the current state and shape
;;;             : of the cursor.

(defmethod cursor-to-feature ((the-window window))
  (let ((pos (view-mouse-position the-window))
        (shape (window-cursor the-window)))
    (when (cursor-in-window-p the-window)
      (make-instance 'cursor-feature
        :x (point-h pos) :y (point-v pos)
        :value (case shape
                 (*i-beam-cursor* 'I-BEAM)
                 (*crosshair-cursor* 'CROSSHAIR)
                 (*watch-cursor* 'WATCH)
                 (otherwise 'POINTER))))))


(defgeneric cursor-in-window-p (wind)
  (:documentation  "Returns T if the cursor is over <wind>, NIL otherwise."))

(defmethod cursor-in-window-p ((tw window))
  (when (window-shown-p tw)
    (rlet ((the-rect rect))
      (points-to-rect (view-position tw)
                      (add-points (view-position tw) (view-size tw))
                      the-rect)
      (point-in-rect-p the-rect 
                       (local-to-global tw (view-mouse-position tw))))))

|#

(defmethod view-loc ((self capi:element))  ;;view
  (let ((pos (view-position self))
        (size (view-size self)))
    (vector (round (+ (point-h pos) (/ (point-h size) 2)))
            (round (+ (point-v pos) (/ (point-v size) 2))))))


(defmethod view-loc ((self capi:interface))  ;simple-view
  (let ((pos (view-position self))
        (size (view-size self)))
    (vector (round (+ (point-h pos) (/ (point-h size) 2)))
            (round (+ (point-v pos) (/ (point-v size) 2))))))


(defmethod view-loc ((self symbol))
  (if (eq self :cursor)
    ;DAN (get-mouse-coordinates (device (device-interface *mp*)))
    (get-mouse-coordinates (current-device))
    (error "!! Can't find location of ~S" self)))


(defmethod width ((self capi:element))
  (point-h (view-size self)))


(defmethod height ((self capi:element))
  (point-v (view-size self)))



;;; FILL-DEFAULT-DIMENSIONS      [Method]
;;; Date        : 99.04.02
;;; Description : The base methods for most MCL views don't set the height or
;;;             : width attributes of the features they generate, nor their
;;;             : size.  Set that up if necessary.

(defgeneric fill-default-dimensions (feat)
  (:documentation  "Fill in the width, height, and size of an icon feature."))

(defmethod fill-default-dimensions ((feat icon-feature))
  (aif (simple-size feat)
    (setf (size feat) it)
    (if (null (screen-obj feat))
      (setf (size feat) 1.0)              ; should be default size, eh
      (progn
        (unless (width feat) (setf (width feat) (width (screen-obj feat))))
        (unless (height feat) (setf (height feat) (height (screen-obj feat))))
        (setf (size feat) (simple-size feat))))))


;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; The view based line drawing classes and methods
;;;; ---------------------------------------------------------------------- ;;;;;;;

;;; LINER      [Class]
;;; Description : The base class for the view based lines.  
;;;             : All it adds to a simple-view is a color slot that defaults
;;;             : to black.

(defclass liner (capi:line-pinboard-object)
  ((color :accessor color :initarg :color :initform :black)  ;;;*black-color*
  (view-position :accessor view-position :initarg :view-position :initform nil)
  (view-size :accessor view-size  :initarg :view-size :initform nil)))
 
;;; POINT-IN-CLICK-REGION-P      [Method]
;;; Description : Override this method so that lines don't handle mouse clicks.

(defmethod point-in-click-region-p ((self liner) where)
  (declare (ignore where))
  nil)


;;; TD-LINER      [Class]
;;; Description : A view that represents a line which is drawn top-down 
;;;             : i.e. from the view-position (upper-left) to the 
;;;             : [view-size - (1,1)] (lower-right) in the container window

(defclass td-liner (liner)
  ())

;;;  A view that represents a line which is drawn bottom-up i.e. from the
;;;  view's lower-left to the view's upper-right in the container window.

;;; BU-LINER      [Class]
;;; Description : A view that represents a line which is drawn bottom-up 
;;;             : i.e. from the view's lower-left to the view's upper-rignt
;;;             : in the container window

(defclass bu-liner (liner)
  ())

;;; Description : A td-liner is just a line-feature located "at" it's mid-point.

(defmethod build-features-for ((lnr td-liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let ((start-pt (view-position lnr))
        (end-pt (subtract-points (add-points (view-position lnr) (view-size lnr)) 
                                  (make-point 1 1))))
    (make-instance 'line-feature
      :color (system-color->symbol (color lnr))
      :end1-x (point-h start-pt) 
      :end1-y (point-v start-pt)
      :end2-x (point-h end-pt) 
      :end2-y (point-v end-pt)
      :x (loc-avg (point-h start-pt) (point-h end-pt))
      :y (loc-avg (point-v start-pt) (point-v end-pt))
      :width (abs (- (point-h start-pt) (point-h end-pt)))
      :height (abs (- (point-v start-pt) (point-v end-pt))))))

(defmethod build-features-for ((lnr bu-liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let ((start-pt (add-points (view-position lnr)
                                (make-point 0 (1- (point-v (view-size lnr))))))
        
        
        (end-pt (add-points (view-position lnr) 
                             (make-point (1- (point-h (view-size lnr))) 0))))
    (make-instance 'line-feature
      :color (system-color->symbol (color lnr))
      :end1-x (point-h start-pt) 
      :end1-y (point-v start-pt)
      :end2-x (point-h end-pt) 
      :end2-y (point-v end-pt)
      
      :x (loc-avg (point-h start-pt) (point-h end-pt))
      :y (loc-avg (point-v start-pt) (point-v end-pt))
      :width (abs (- (point-h start-pt) (point-h end-pt)))
      :height (abs (- (point-v start-pt) (point-v end-pt))))))

;;; RPM-VIEW-LINE [Function]
;;; Description : Add a view to the window that displays a line defined by
;;;             : the start and end points in the color supplied (an MCL
;;;             : system style color).

(defun rpm-view-line (wind start-pt end-pt &optional (color :black))  ;;*black-color*
  "Adds a view in the specified window which draws a line from the start-pt to the end-pt
   using the optional color specified (defaulting to black).  This view will add features 
   to the icon on PM-PROC-DISPLAY."
  (let* ((gx (> (point-h end-pt) (point-h start-pt)))
         (gy (> (point-v end-pt) (point-v start-pt)))
         (vs (subtract-points start-pt end-pt)))
    (setf vs (make-point (+ 1 (abs (point-h vs)))
                         (+ 1 (abs (point-v vs)))))
    (add-subviews wind (cond ((and gx gy)
                              (make-instance 'td-liner
                                :color color
                                :view-position start-pt 
                                :view-size vs))
                             ((and (not gx) (not gy))
                              (make-instance 'td-liner
                                :color color
                                :view-position end-pt 
                                :view-size vs))
                             ((and gx (not gy))
                              (make-instance 'bu-liner
                                :color color
                                :view-position (make-point (point-h start-pt) (point-v end-pt))
                                :view-size vs))
                             (t
                              (make-instance 'bu-liner
                                :color color
                                :view-position (make-point (point-h end-pt) (point-v start-pt))
                                :view-size vs))))))
;;;End Line Stuff

(defmethod cursor-to-feature ((obj capi:interface))
  (let ((pos (mouse-position)))
     (make-instance 'cursor-feature
        :x (first pos) :y (second pos)
        :value 'arrow)))

(defmethod get-border-height ((dev capi:interface))
  (+
  (if (member :borderless (capi::simple-pane-window-styles dev)) 0 
#+:mac 22
#+:win32 33
    )
#+:mac 22 ;height of menubar
#+:win32 0
   ))
;;;
;;; Device Interface
;;;
(defmethod device-handle-click ((device capi:interface))
 (lw-click-mouse))
  
(defmethod device-handle-keypress ((device capi:interface) key)
  (capi:activate-pane device) 
  (capi:execute-with-interface device #'key-press (if (stringp key) key (format nil "~c" key))))

(defmethod device-move-cursor-to ((device capi:interface) xyloc)
  (multiple-value-bind (x y) (capi:convert-relative-position device (capi:element-screen device) (svref xyloc 0) (svref xyloc 1))
      (lw-move-cursor x  (+ y  (get-border-height device)))  ;;add height of menu borders
    ))
 
(defmethod device-speak-string ((device capi:interface) string)
  (mp:process-run-function
   "Voice"
   nil
   'lw-speak
   string)
  )

(defmethod get-mouse-coordinates ((device capi:interface))
  (let ((pos (mouse-position)))
    (vector (first pos) (second pos))
    )
  )

(defmethod device-update-attended-loc ((device capi:interface) xyloc)
  (multiple-value-bind (x y) (capi:convert-relative-position device (capi:element-screen device) (svref xyloc 0) (svref xyloc 1))
   (update-me *attn-tracker* device (vector x (+ y (get-border-height device))))) ;;add height of menu bar
  )
;;; End Device Interface


(defmethod device-update :after ((device capi:interface) time)
  (declare (ignore device time))
  (mp:process-allow-scheduling)
  )



;;;
;;; Focus Ring
;;;
(defclass rpm-overlay ()
  ((offset :accessor offset :initarg :offset :initform nil)
   (view-size :initarg :view-size :accessor view-size :initform nil)))

(defclass focus-ring (rpm-overlay capi:output-pane)
  ((color :accessor color :initarg :color :initform :red))
  (:default-initargs 
    :view-size (vector 15 15)
    :offset (vector 20 -10)))

;;; make the focus ring

(defun display-circle (self x y width height)
  (declare (ignore x y width height))
  (gp:draw-circle self 20 20 10 :filled t :foreground  (color:make-rgb 0.8 0.2 0.2 1.0)))


(capi:define-interface focus-win ()
  ((focus-x :initform 0 :accessor focus-x)
   (focus-y :initform 0 :accessor focus-y)
   (destroy-func :initform nil :accessor destroy-func))
  (:panes
   (attn-ring focus-ring :accessor attn-ring :display-callback 'display-circle  ))
  (:layouts
   (main capi:simple-layout '(attn-ring) :visible-min-width 40 :visible-min-height 40))
  #+:LISPWORKS5 (:default-initargs
                    :transparency .5
                  :window-styles '(:always-on-top :borderless))
  #-:LISPWORKS5 (:default-initargs
                  :window-styles '(:always-on-top :borderless))
  )


(defmethod draw-focus ((xyloc vector) (win focus-win))
 (with-slots (focus-x focus-y attn-ring) win
     (let ((x (svref xyloc 0)) (y (svref xyloc 1)))
       (when (or (not (= x focus-x )) (not (= y focus-y )))
         (setf focus-x x focus-y  y)
           (capi:set-top-level-interface-geometry win :x (+ x   (svref (offset attn-ring) 0))
                                                  :y (+ y   (svref (offset attn-ring) 1)))))))

(eval-when (load eval)
  (setf *attn-tracker* (attn-ring (capi:display (make-instance 'focus-win))))
  (capi:hide-interface *attn-tracker* nil))



(defmethod update-me ((obj focus-ring) (device capi:interface) (xyloc vector))
  (awhen (capi:element-interface obj)
    (when (eql (capi:top-level-interface-display-state it) :hidden)
      (let ((dev (device (current-device-interface))))
        (when dev
          (setf (destroy-func it) (capi:interface-destroy-callback dev))
          (setf (capi:interface-destroy-callback dev) (lambda(win) (capi:interface-destroy-callback dev) (capi:hide-interface it nil) 
                                                                   ))))
      (capi:show-interface it))
    (draw-focus xyloc it)))
;;; End Focus Ring



;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Windows Stuff;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

#+:win32
(progn
(fli:define-foreign-function
    (SetCursorPosition "SetCursorPos")
    ((X (:unsigned :int)) (Y (:unsigned :int)))
  :result-type :boolean)

;; define Mouse Constant
(defconstant Mouse_Event_Left_Button_Down #x00000002)
(defconstant Mouse_Event_Left_Button_Up #x00000004)

;; define mouse_move
(fli:define-foreign-function
    (mouse_move "SetCursorPos")
    ((X (:unsigned :int)) (Y (:unsigned :int)))
  :result-type :boolean)

;; define mouse_event
(fli:define-foreign-function 
    (mouse_event "mouse_event" )
    ((dwFlag (:unsigned :long))
     (dx_ (:unsigned :long)) (dy_ (:unsigned :long))
     (dwData (:unsigned :long))(ptr (:unsigned :long)))
  :result-type :boolean)

;;define key press
(fli:define-foreign-function
    (keybd_event "keybd_event" )
    ((keycode :byte) (scancode :byte) (flags (:unsigned :long))
     (noop (:unsigned :long)))
  :result-type :boolean)

(fli:define-c-typedef bool (:boolean :int))

(fli:define-c-typedef long :long)

(fli:define-c-struct tagpoint
  (x long)
  (y long))

(fli:define-c-typedef point (:struct tagpoint))

(fli:define-c-typedef lppoint (:pointer point))

(fli:define-foreign-function (get-cursor-position "GetCursorPos")
  ((lp-point lppoint))
  :result-type bool)


(defun key-press (key)
  (keybd_event (char-code (char-upcase key)) 0 0 0) ;down
  (keybd_event (char-code (char-upcase key)) 0 2 0) ;up
)

(defun mouse-position ()
  (let ((loc (fli:allocate-foreign-object :type 'point)))
    (get-cursor-position loc)
    (list (fli:foreign-slot-value loc 'x) (fli:foreign-slot-value loc 'y))))

(defun mouse-move(x y)
  (mouse_move x y))

(defun mouse-left-click()
  (mouse_event Mouse_Event_Left_Button_Down 0 0 0 0)
  (mouse_event Mouse_Event_Left_Button_Up 0 0 0 0))

(defun mouse-left-click-at(x y)
  (mouse-move x y)
  (mouse-left-click))

;;;
;;;  Device Interface for windows
;;;

(defmethod device-handle-click ((device capi:interface))
  (mouse-left-click))
  
(defmethod device-handle-keypress ((device capi:interface) key)
  (key-press key)  )

(defmethod device-move-cursor-to ((device capi:interface) xyloc)
  (multiple-value-bind (x y) (capi:convert-relative-position device (capi:element-screen device) (svref xyloc 0) (svref xyloc 1))
    (mouse-move x (+ y (get-border-height device)))    )  ) ;;add height of lispworks window title bar

(defmethod device-speak-string ((device capi:interface) string)  )

  
)



