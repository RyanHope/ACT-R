Threaded Cognition
ACT-R Implementation
Dario Salvucci & Niels Taatgen

This directory contains the code that implements the threaded cognition theory of concurrent 
multitasking.  To install the code into your ACT-R setup, place the file "new-threads.lisp" 
into the "actr6/modules/" directory.  The code will compile and load automatically during 
the next startup of the ACT-R system.  

The "legacy-threads.lisp" file is the original implementation of the code for reference.
It will not work correctly with the current procedural module.

The provided sample model can then be loaded and run.

For more information on using threading in your own models, please see the user guide 
"threads.pdf" also found in this directory.

For more information about the theory, please see: Salvucci, D. D., & Taatgen, N. A. (2007).  
Threaded cognition: An integrated theory of concurrent multitasking.  Under revision for
Psychological Review.
