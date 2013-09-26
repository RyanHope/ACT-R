
These two files need to be loaded to provide ACT-R with the ability to
interact with real windows in MCL.  This interface is no longer supported,
but is included in extras for those that may still need it.  It has not
been updated with the changes necessary to provide support for multiple
windows or multiple models sharing the same window.

To have them loaded automatically the best thing to do would be place 
them into a directory called mcl in the actr6/devices directory and edit
the load-act-r-6.lisp file uncommenting this line:

;; #+:digitool (setf *device-interface-pathname* "ACT-R6:devices;mcl;")

found near the bottom of the file.