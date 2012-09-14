This directory contains the components necessary to add a chunk viewer to the 
Environment which works similarly to the one developed by Andrea Heiberg, 
Jack Harris, and Jerry Ball as described in the abstract which is included
here: HeibergHarrisBall.pdf.

To enable the new viewer copy the chunk-tree.lisp file to the actr6/other-files
directory and the 35a-declarative-tree.tcl file to the 
actr6/environment/GUI/dialogs directory.

In this implementation slot names are shown in italics in blue at the end of
the line out of the parent chunk.  Chunks are shown in green unless it's a
circular reference in which case it's shown as red, and any other slot value 
is shown in black.  If the chunk is light green then it is a chunk which
exists in the model's DM and clicking on that chunk will bring up a new
"normal" declarative viewer showing the details of that chunk.

The "show nil slots" check box toggles whether empty slots of the chunks
are shown in the tree display or not.

The save button will create an Encapsulated PostScript image of the tree.
