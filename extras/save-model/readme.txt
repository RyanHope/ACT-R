The save-chunks-and-productions.lisp file defines a new command called 
save-chunks-and-productions that can be used to write a model's current 
declarative memory, set of productions, general parameters related to 
declarative and procedural, the appropriate chunk and proceduction 
parameters, and the chunks currently in buffers out to a model file.

The save-chunks-and-productions.lisp file can be put into the tools 
directory of the distribution to be loaded automatically, or it can be 
explicitly loaded as needed.

To use it call the save-chunks-and-productions function passing it
a string which indicates a file to open and write the model information
into.

More details on exactly what is saved and how to call the new command
can be found in the comments of the save-chunks-and-productions.lisp
file.
