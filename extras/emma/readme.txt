
The emma.lisp file implements the EMMA extension to ACT-R as a replacement for
the default vision module.  To use EMMA load the emma.lisp file after loadng
the main ACT-R system, or put emma.lisp into the other-files directory to have
it loaded automatically with the rest of the ACT-R system.  No other changes are
necessary to use EMMA, but you may want to change the default object
frequency to adjust the attention shift times.  You can do that by setting the 
:vis-obj-freq parameter or set the frequency for text using the register-string-
frequency command which takes a string and its frequency value as parameters.