;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2015 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : load-act-r.lisp
;;; Version     : 1.0
;;; 
;;; Description : Top level loader for the whole ACT-R 7 system.
;;; 
;;; Bugs        : ???
;;;
;;; To do       : [-] Test in a variety of Lisps for issues with the
;;;             :     logical hostname stuff.
;;;             : [ ] Now, look into using the clisp version in other
;;;             :     lisps because it seems cleaner/more generic than
;;;             :     the ones I put toghether...
;;;             : [x] Use compile-file-pathname instead of always adding a new
;;;             :     entry for *.fasl-pathname*.
;;; 
;;; ----- History -----
;;;
;;; 2004.10.26 Dan
;;;             : Creation.
;;;             :
;;;             : Realized that require doesn't compile things automatically
;;;             : in all cases, so added my own require-compiled that does.
;;; 2004.12.10 Dan
;;;             : Fixed the make-package for the packaged version (for use
;;;             : with ACL at least).
;;;             : Reduced the lines to max of 80 chars.
;;; 2005.01.02 Dan
;;;             : Changed it so that it loads the "core modules" in a specific
;;;             : order and then all other modules.
;;; 2005.01.12 Dan
;;;             : * Added the tools directory to the set.
;;; 2005.01.23 Dan
;;;             : * Fixed the Lispworks binary extension check. Don't think it
;;;             :   still needs the old one...
;;; 2005.01.29 Dan
;;;             : * Added a feature check into compile-and-load to force it
;;;             :   to recompile if :actr-recompile is on the features list.
;;; 2005.02.01 Dan
;;;             : * This time, the Lispworks feature checks should be set
;;;             :   properly for OSX (thanks to Chris Sims).
;;; 2005.02.25 Dan
;;;             : * Removed the ~\newline usages because that causes problems
;;;             :   when a Lisp only wants to see native new lines there.
;;; 2005.04.14 Dan
;;;             : * Changed compile-and-load so that it throws an error if the
;;;             :   file it is passed has a non-"lisp" extension. - need to
;;;             :   verify that in other Lisps to make sure it works right.
;;; 2005.07.07 Dan
;;;             : * Fixed the packaged loading for Lispworks now too.
;;; 2005.08.10 Dan
;;;             : * Added a new directory to the set (commands) in place of
;;;             :   where modules was and then moved modules to after the
;;;             :   devices.
;;;             : * Now, there's basically a directory to auto-load in all
;;;             :   resonable locations, and I can better distribute files 
;;;             :   that were all jammed into tools.
;;;             : * Updated the version to 1.0.
;;; 2005.08.16 Dan
;;;             : * Added a flag to indicate whether things have been loaded
;;;             :   previously or not and actually throw an error if this
;;;             :   file is attempted to be loaded more than once.
;;; 2005.09.16 Dan
;;;             : * Added the appropriate feature checks to work "right" with
;;;             :   ACL 7's IDE i.e. load the devices and package things in
;;;             :   cg-user when necessary.
;;; 2005.10.18 Dan
;;;             : * Added the logical host setup for CMUCL.
;;;             : * Moved the smart-load function here and generalized it so
;;;             :   that framework and core-modules don't need to have 
;;;             :   their own special versions.
;;;             : * Also converted those specific loaders to essentially just
;;;             :   file lists now.
;;; 2005.11.01 Dan
;;;             : * Added a new compile-and-load so that things can be loaded
;;;             :   into MCL 5/5.1 (the versions that have the split open/load
;;;             :   Mac/Unix file menu options) without having to convert all
;;;             :   the files first.  This file needs to be loaded as a Unix
;;;             :   file and the rest should take care of itself.
;;; 2005.11.07 Dan
;;;             : * Realized that since the environment is loaded from tools
;;;             :   that there's no way to add patches to the environment
;;;             :   in an "auto load" directory because things in tools may
;;;             :   be loaded before the environment.  So, I've added yet
;;;             :   another directory from which files are loaded automatically.
;;;             :   The other-files directory is now scanned and .lisp files
;;;             :   are loaded as the last step of the load process.
;;; 2005.12.13 Dan
;;;             : * Changed the logical host setup for ACL because it turns
;;;             :   out that the host-namestring always ends up nil and doesn't
;;;             :   actually capture the drive info which causes problems if
;;;             :   the ACT-R sources are on a different drive than the ACL
;;;             :   assumed default.
;;; 2006.01.04 Dan
;;;             : * Added the switches so that it'll load under CMUCL in OS X
;;;             :   (with ppc).
;;; 2006.06.29 Dan
;;;             : * Added components provided by Don Morrison to allow it to be 
;;;             :   loaded into CLisp v2.38 - the CLisp logical host, tighter 
;;;             :   handling of the logical pathnames in general (other Lisps
;;;             :   didn't mind logical namestrings in places where a pathname
;;;             :   designator was required), and a shadowing of the CLisp
;;;             :   execute function.
;;; 2006.08.31 Dan
;;;             : * Replaced the *already-loaded-act-r-6-files* variable as
;;;             :   the reloading test with a feeature check for :act-r-6.0
;;;             :   which is now placed on the *features* list.
;;; 2007.01.17 Dan
;;;             : * Added the support necessary to load into SBCL.
;;;             : * Required changing all calls to logical-pathname in the
;;;             :   directory calls to translate-logical-pathname which should
;;;             :   work for all Lisps (SHOULD!).
;;;             : * NOTE that this doesn't work with SBCL 1.0 under Windows
;;;             :   because a bug in their directory command doesn't
;;;             :   recognize the wildcards but they've addressed that so future
;;;             :   versions of SBCL for Windows should work.
;;; 2007.02.02 Dan
;;;             : * Changed the uses for the packaged-actr so that all Lisps
;;;             :   use "COMMON-LISP" - should work, but this is still an
;;;             :   an experimental feature.
;;; 2007.02.05 Dan
;;;             : * Added the hack for the broken directory command in SBCL for
;;;             :   Windows so that it can load all of the files now.
;;; 2007.02.26 Dan
;;;             : * Added an appropriate fasl extension for LispWorks 5 on
;;;             :   x86 Macs.
;;; 2007.07.24 Dan
;;;             : * Finally added the right extension for LispWorks 5 for
;;;             :   Windows.
;;; 2007.08.03 Dan
;;;             : * Fixed the feature checks so that LispWorks 5 gets the
;;;             :   right value for Windows...
;;; 2007.09.10 Dan
;;;             : * Putting the LispWorks device file pointers in to allow use
;;;             :   of the beta device interface.
;;; 2008.06.09 Dan
;;;             : * Added yet another LispWorks entry for compiled file types
;;;             :   to cover the LW 5 unix names.  Probably about time to try
;;;             :   getting something that's robust using compile-file-pathname
;;;             :   instead...
;;; 2008.06.10 Dan
;;;             : * Trying a version that uses compile-file-pathname to try
;;;             :   to set the fasl name "automatically".
;;; 2010.03.09 Dan
;;;             : * Changed the openmcl logical setting to the 'generic' one
;;;             :   to fix problems with different "drives" under Windows.
;;; 2010.03.11 mdb
;;;             : * Changed MCL-specific code to not load CFBundle when running
;;;             :   under MCL 5.2.
;;; 2010.11.02 Dan
;;;             : * Added ABCL to the list for logical translation.
;;; 2011.04.28 Dan
;;;             : * Define *file-list* here and just set it in the other loaders.
;;; 2012.01.04 Dan
;;;             : * Added another directory to the tree: user-loads.  That will
;;;             :   always go last and there will never be any distributed files
;;;             :   in it.  The .lisp files in it will be loaded in order based
;;;             :   on file name and is for users to add initialization or model
;;;             :   files to as needed.
;;;             : * Removed the old info under general docs.
;;; 2012.05.07 Dan
;;;             : * Make the ACL device support dependent on the Windows version
;;;             :   only and print a warning for the others.
;;; 2012.08.24 Dan
;;;             : * Added the switches to load the new ccl-cocoa device and
;;;             :   removed the mcl device.
;;; 2013.08.09 Dan
;;;             : * Fixed a problem with the shadowing of functions in SBCL and
;;;             :   CLisp when the current package wasn't :cg-user.
;;; 2013.10.07 Dan
;;;             : * Added another feature switch to be tested :actr-fast.  When
;;;             :   it's on the features list set the optimize settings for faster
;;;             :   compiled code.
;;; 2013.12.19 Dan
;;;             : * Added an :ecl switch to the logical path definitions.
;;; 2014.02.21 Dan
;;;             : * Big warnings that this shouldn't be used!
;;; 2014.05.29 Dan
;;;             : * Define the variable used to test for applying the hacks to
;;;             :   work more like ACT-R 6.0.
;;; 2014.06.16 Dan
;;;             : * Added a use :ccl to the :act-r package for CCL.
;;; 2014.07.14 Dan
;;;             : * Changed the feature switch from :ACT-R-6.0 to :ACT-R-6.1
;;;             :   and added a separate :ACT-R switch which can be used in
;;;             :   the future for testing that "some" ACT-R is loaded.
;;; 2014.08.28 Dan
;;;             : * Changed the warning from don't use to pre-release and let me
;;;             :   know about issues.
;;; 2014.08.29 Dan
;;;             : * Double check for the :act-r being on *features* because
;;;             :   the compiler switch isn't sufficient if the file gets 
;;;             :   compiled when the switch isn't set.
;;;             : * Removed the second "do not use" warning as well.
;;; 2014.09.11 Dan
;;;             : * Changed the final load print out to say 6.1.
;;; 2014.09.29 Dan
;;;             : * Added the actr-load function which does a translate on the
;;;             :   parameter it's passed so that I can use that in the test
;;;             :   models and have them work with SBCL and any others which
;;;             :   don't accept logical to load.
;;; 2014.09.30 Dan
;;;             : * Changed the warning to beta instead of pre-release.
;;; 2014.12.01 Dan
;;;             : * Removed the beta warning at load time.
;;; 2014.12.02 Dan
;;;             : * Added a translate-logical-pathname to the directory passed
;;;             :   to smart-load so it can be used with logicals in Lisps that
;;;             :   don't like logicals as pathnames.
;;; 2015.07.28 Dan 
;;;             : * Renamed it to load-act-r-7.lisp.
;;;             : * Changed the logical from "ACT-R6" to "ACT-R" and use the
;;;             :   general form for all Lisps.
;;;             : * Removed all the special code for MCL.
;;;             : * Get rid of the 6.0 compatibility hack variable.
;;;             : * Changed the feature to :act-r-7.
;;;             : * Changed the output to say ACT-R 7.
;;;             : * Added an "ACT-R-support" logical for use in other places.
;;; 2015.08.03 Dan
;;;             : * Change the package shadowing for CLisp because it seems to
;;;             :   need the nicknames now.
;;; 2015.08.05 Dan
;;;             : * Changed the name again to load-act-r.lisp because I might as 
;;;             :   well do so now while I'm changing everything else...
;;; 2015.10.26 Dan
;;;             : * The hack for defconstant in SBCL doesn't work for the newer
;;;             :   versions so replaced that with something that doesn't depend
;;;             :   on the internals and just ignores any error in the "rebinding"
;;;             :   situation.  This works for both the old and new versions and
;;;             :   should continue to work with future versions as well.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Using logical pathnames a directory structure for ACT-R 7 can be created
;;; that allows users to add or remove files from a specific directory within
;;; the system, and through the use of require and provide also remove the
;;; need to edit a "load order" file.
;;;
;;; See the reference manual for details about the directories provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; The logical hostname "ACT-R" can be used as a relative reference for the
;;; directory where the ACT-R 7 folders are located.
;;;
;;;
;;; require-compiled (code-module pathname)
;;;
;;; code-module is a string that designates some code that needs to be loaded
;;;             which should have a corresponding (provide code-module)
;;; pathname is the pathname to where code-module can be found.
;;;
;;; Similar to the function require this will determine if the requested
;;; code-module has been loaded and if not will compile and load the file
;;; specified by pathname.  This differs from the normal require function
;;; in that the pathname is mandatory and it does not search through any
;;; implementation defaults to find the code-module.  However, it does still
;;; depend on a provide call existing in the code-module file so that
;;; it only loads the necessary file the first time it is required.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; The idea is for a system where people can just drop in new modules without
;;; having to edit or change any of the existing code.  In practice, that
;;; may not work all the time (with things like name conflicts) but should
;;; be useable.  Name conflicts could probably be eliminated through some
;;; sort of module packaging scheme, but that seems to complicate module
;;; creation and could lead to some potentially nasty debugging issues.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (make-package :act-r 
                               :use '("COMMON-LISP-USER" 
                                      "COMMON-LISP"
                                      #+:ccl "CCL"
                                      #+:allegro "EXCL"
                                      #+:allegro-ide "COMMON-GRAPHICS-USER"
                                      #+:common-graphics "COMMON-GRAPHICS-USER"))


;;; Basically a hack for ACL 7 so that I don't have to touch every file!

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  #+(and :allegro :ide (not :allegro-ide))
    (push :allegro-ide *features*))

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


#+:act-r (error "Only one version of ACT-R should be loaded at a time.")
#-:act-r (if (find :act-r *features*)
             (error "Only one version of ACT-R should be loaded at a time.")
           (progn
             (pushnew :act-r-7 *features*) 
             (pushnew :act-r *features*)))


;; Clisp has an implementation-specific function execute that conflicts with
;; the generic function execute in ACT-R, so shadow it
#+:clisp (eval `(defpackage ,(package-name *package*) (:nicknames "CL-USER" "USER") (:shadow "EXECUTE")))

;; SBCL has a function called reset we need to shadow and there's an issue
;; with their defconstat because it throws an error if you compile and then
;; load a file (it's fine with the compiled file later, but that first time
;; is a problem).

#+(and :sbcl (not :win32))
  (eval `(defpackage ,(package-name *package*) (:shadow "RESET" "DEFCONSTANT")))
#+(and :sbcl :win32) 
  (eval `(defpackage ,(package-name *package*) (:shadow "RESET" "DEFCONSTANT" "DIRECTORY")))

#+:sbcl (defmacro defconstant (name value &optional documentation)
          `(if (boundp ',name)
               (ignore-errors (cl::defconstant ,name ,value ,documentation))
             (cl::defconstant ,name ,value ,documentation)))

;;; The Windows version of SBCL doesn't properly handle wild cards in the
;;; directory command so this hacks around that for now sufficiently to load
;;; the ACT-R files...

#+(and :sbcl :win32) 
(eval-when (:load-toplevel :execute)
  (defun directory (pathname &key)
    ;(format t "Calling the new directory for ~S~%" pathname)
    (if (not (string= (pathname-type pathname) "*"))
        (let* ((new-path (make-pathname :host (pathname-host pathname)
                                        :device (pathname-device pathname)
                                        :directory (pathname-directory pathname)
                                        :defaults "*.*"))
               (new-val (cl::directory new-path))
               (res (remove-if-not (lambda (x) 
                                     (string-equal (pathname-type x) (pathname-type pathname)))
                                   new-val)))
          ;(format t "Returning ~S from directory of new-path: ~s which returns ~s.~%" res new-path new-val)
          res)
      (cl::directory pathname))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the logical host "ACT-R" relative to the current location

(setf (logical-pathname-translations "ACT-R")
  `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

(setf (logical-pathname-translations "ACT-R-support")
  `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" (translate-logical-pathname "ACT-R:support;"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define the file extension (the pathname type) for compiled and source files
;;; in the currently supported systems

(unless (boundp '*.lisp-pathname*)
  (defvar *.lisp-pathname* 
      (make-pathname :type "lisp")))

(unless (boundp '*.fasl-pathname*)
  (defvar *.fasl-pathname* 
      (let ((type (pathname-type (compile-file-pathname "dummy.lisp"))))
        (if (and type (not (string-equal type "lisp"))) 
          (make-pathname :type type)
        
        ;; If it can't figure it out automatically resort to predefined value
        
        #+:allegro (make-pathname :type "fasl")
        #+:sbcl (make-pathname :type "fasl")
        #+:clisp (make-pathname  :type "fas")
        #+(and :linux :cmu) (make-pathname :type "x86f")
        #+(and :ppc :cmu) (make-pathname :type "ppcf")
        #+(and :lispworks :win32 (not :lispworks5)) (make-pathname :type "fsl")
        #+(and :lispworks :win32 :lispworks5) (make-pathname :type "ofasl")
        #+(and :lispworks :unix (not :macosx) (not :lispworks5)) (make-pathname :type "ufsl")
        #+(and :lispworks :unix (not :macosx) :lispworks5) (make-pathname :type "ufasl")
        #+(and :lispworks :macosx (not :x86)) (make-pathname :type "nfasl")
        #+(and :lispworks :macosx :x86) (make-pathname :type "xfasl")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; When :actr-fast is on the features list then set the switches for fastest
;;; compiled code.

#+:actr-fast (eval-when (:compile-toplevel :load-toplevel :execute)
               (proclaim '(optimize (speed 3) (safety 1) (space 0) (debug 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define some functions for compiling and loading files

;;; compile-and-load (pathname)
;;;
;;; pathname a file pathname (or pathname string) if the file already
;;;          has a type specified, then it is ignored and the defaults
;;;          of lisp for source and system-dependent binary types are
;;;          used.
;;; 
;;; If a source file (.lisp) exists for the specified pathname then if there
;;; is no binary file (determined by *.fasl-pathname*), the binary is
;;; older than the source file, or the feature :act-r-recompile is set then 
;;; compile the source file into a binary and load it.  
;;;
;;; Based on the smart-load function from the ACT-R loader.


(defun compile-and-load (pathname)
  (when (pathname-type pathname) ;; throw away the type to allow for
                                 ;; the merging with a binary type
    (if (string-equal (pathname-type pathname) "lisp")
        (setf pathname (make-pathname :host (pathname-host pathname)
                                      :directory (pathname-directory pathname)
                                      :device (pathname-device pathname)
                                      :name (pathname-name pathname)))
      (error "To compile a file it must have a .lisp extension")))
  
  (let* ((srcpath (merge-pathnames pathname *.lisp-pathname*))
         (binpath (merge-pathnames pathname *.fasl-pathname*)))
    (unless (probe-file srcpath)
      (error "File ~S does not exist" srcpath))
    (when (or (member :actr-recompile *features*)
              (not (probe-file binpath))
              (> (file-write-date srcpath) (file-write-date binpath)))
      (compile-file srcpath :output-file binpath))
    (load binpath)))
  


;;; SMART-LOAD      [Function]
;;; Date        : 99.12.21
;;; Description : Loads binary version of a specified file.  Of course, the 
;;;             : said binary version might not exist or be older than the 
;;;             : source version, in which case the source file is compiled 
;;;             : before loading.
;;;             : Updated to add an option parameter to determine whether
;;;             : to just warn of a missing file or to throw an error.


(defun smart-load (this-files-dir file &optional (error? nil))
  "Loads binary <file> in directory <this-files-dir> or compiles and loads 
   source version"
  (let* ((true-dir (translate-logical-pathname this-files-dir))
         (srcpath (merge-pathnames  
                   (merge-pathnames file *.lisp-pathname*)
                   true-dir)))
    (if (not (probe-file srcpath))
        (if error? 
            (error "File ~S does not exist" srcpath)
          (format *error-output* "File ~S does not exist" srcpath)))
    (compile-and-load srcpath)))


(defun actr-load (file)
  "Loads the file specified after translating the pathname"
  (let ((path (translate-logical-pathname file)))
    (if (not (probe-file path))
        (format *error-output* "#|Warning: File ~S does not exist.|#~%" path)
      (load path))))



;;; require-compiled (code-module pathname)
;;;
;;; code-module is a string that designates some code that needs to be loaded
;;;             which should have a corresponding (provide code-module) in it
;;; pathname is the pathname to where that code-module can be found (including
;;;          the file's name).
;;;
;;; Similar to the function require this will determine if the requested
;;; code-module has been loaded and if not will compile and load the file
;;; specified by pathname.  This differs from the normal require function
;;; in that the pathname is mandatory and it does not search through any
;;; implementation defaults to find the code-module.  However, it does still
;;; depend on a provide call existing in the code-module file so that
;;; it only loads the necessary file the first time it is required.

(defmacro require-compiled (code-module pathname)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (member ,code-module *modules* :test #'string=)
       (compile-and-load (translate-logical-pathname ,pathname)))))                       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load any special system support files here

;; Warn about the case sensitive mode in ACL because things like t vs T shouldn't
;; matter (and both probably occur in the code), and won't work in the case sensitive
;; versions of ACL.

#+:allegro (when (or (eq :case-sensitive-lower *current-case-mode*)
                     (eq :case-sensitive-upper *current-case-mode*))
             (unless 
                 (yes-or-no-p 
                  "WARNING: you are using a case sensitive Lisp.  ACT-R may not load or run correctly.  Continue anyway?")
               (break)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load the framework's loader file (it is order dependent)

(defvar *file-list*)

(smart-load (translate-logical-pathname "ACT-R:framework;") "framework-loader.lisp")

(dolist (the-file *file-list*)
  (smart-load (translate-logical-pathname "ACT-R:framework;") the-file t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load the core modules 

(smart-load (translate-logical-pathname "ACT-R:core-modules;") "core-loader.lisp")

(dolist (the-file *file-list*)
  (smart-load (translate-logical-pathname "ACT-R:core-modules;") the-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; First, load any additional extensions.

(dolist (file (directory (translate-logical-pathname "ACT-R:commands;*.lisp")))
  (compile-and-load file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; When a new device is added it should be included with a switch below so that it can be loaded 

(defvar *device-interface-pathname* nil)

;;; Here are the devices that are defined

#+(and :allegro-ide :mswindows) (setf *device-interface-pathname* "ACT-R:devices;acl;")

#+(and :allegro-ide (not :mswindows)) (print-warning "Native ACL device not available for Mac or Linux versions because~%they lack the commands for controlling the mouse and keyboard as described here~%http://www.franz.com/support/documentation/6.2/doc/cggtk-relnotes.html#2.3~%")

#+:lispworks (setf *device-interface-pathname* "ACT-R:devices;lw;")

#+(and :clozure :darwin :apple-objc :ccl-1.8) (setf *device-interface-pathname* "ACT-R:devices;ccl-cocoa;")

;;; Load the virtual device

(compile-and-load (translate-logical-pathname "ACT-R:devices;virtual;device.lisp"))
(compile-and-load (translate-logical-pathname "ACT-R:devices;virtual;uwi.lisp"))

;;; Load any Lisp specific device that's defined

(when *device-interface-pathname*
  (if (probe-file (merge-pathnames *device-interface-pathname* "device.lisp"))
      (compile-and-load (merge-pathnames *device-interface-pathname* 
                                         "device.lisp"))
    (format t 
        "################~%#### No Device file found in ~S ####~%##############"
      *device-interface-pathname*))
  (if (probe-file (merge-pathnames *device-interface-pathname* "uwi.lisp"))
      (compile-and-load (merge-pathnames *device-interface-pathname* 
                                         "uwi.lisp"))
    (format t 
        "#################~%#### No uwi file found in ~S ####~%################"
      *device-interface-pathname*)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; After the modules and devices files are done load any files in the 
;;; modules, tools and then finally the other-files drectories.

(dolist (file (directory (translate-logical-pathname "ACT-R:modules;*.lisp")))
  (compile-and-load file))

(dolist (file (directory (translate-logical-pathname "ACT-R:tools;*.lisp")))
  (compile-and-load file))

(dolist (file (directory (translate-logical-pathname "ACT-R:other-files;*.lisp")))
  (compile-and-load file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print a confirmation message to let the user know ACT-R has been loaded
;;; along with the version numbers of all the modules.

(format t "~%##################################~%")
(mp-print-versions)
(format t "~%######### Loading of ACT-R 7 is complete #########~%")


(let ((d (directory (translate-logical-pathname "ACT-R:user-loads;*.lisp"))))
  (when d
    (format t "~%######### Loading user files #########~%")
    (dolist (file (sort d 'string< :key (lambda (x) (string (pathname-name x)))))
      (compile-and-load file))
    (format t "~%######### User files loaded #########~%")))


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
