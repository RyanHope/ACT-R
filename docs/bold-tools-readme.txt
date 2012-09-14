These are new controls for the ACT-R environment for
use with the BOLD prediction module which are now
included by default.  The are implemented in the
following files found in the environment/GUI/dialogs 
directory, and if you do not want them they can be
deleted or renamed to not have the .tcl extension.


80-ctrl-panel-bold-label.tcl

  This control just provides a text title of "BOLD tools"
  on the control panel to separate the BOLD tools.

81-bold-browser.tcl 

  This control provides a button called "Buffer graphs".
  That button opens up a window which will graph the bold 
  response data as returned by predict-bold-response for
  any buffer.  Multiple copies of the tool may be opened to
  view different graphs.  By default each buffer's graph has 
  its data scaled to the 0-1 range independent of the others.
  However, if the "Scale across regions" box is checked then
  all regions are scaled with the same multiplier.

82-bold-brain.tcl

  This control provides a button called "2D brain".  This
  viewer shows pictures of fMRI brain slices with the regions 
  to which the ACT-R modules are mapped displayed and allows 
  one to see the activity of those regions at the intervals 
  recorded.
  Note, only the manual, goal, aural, imaginal, production, 
  retrieval, visual, and vocal buffers have a mapping at 
  this time.  The slider at the bottom of the window lets
  you advance the model through time to see the activity
  predicted in each region (the ticks on the slider 
  correspond to the :bold-inc value).

83-bold-brain-3d.tcl

  This control adds a button called "3D brain". This viewer 
  shows data similar to the previous one, but instead of 
  showing the data on the individual slice images it creates 
  a very crude 3d brain (created from the outline of the slices) 
  into which the modules' regions of intrest are mapped.  The 
  image can be rotated by clicking and dragging the left mouse 
  button, zoomed in and out by clicking and dragging the right 
  mouse button, and translated by clicking and dragging the
  middle mouse button.  The slider at the bottom advances 
  through the time samples like the previous control.

  The viewer is very crude, but may provide some perspective
  as to where the regions are located.  

  There are other groups working on more sophisticated 
  viewers, and perhaps at some point a better tool
  set will be available for displaying the BOLD data.

  The 3d rendering is produced using code found on the 
  Tcl/Tk Wiki: 3dviewer.tcl by Mark B. Stucky. 

84-bold-brain-3d-real-time.tcl

  This control adds a button called "Run-time 3D brain".
  This viewer is just like the previous one except
  that it will update the BOLD data as the model is
  running instead of allowing one to browse through
  the data after a run.  This should only be used if the
  model is running in real time or slower because otherwise 
  it is very likely to fall behind, become nonresponsive,
  and possibly break the connection between ACT-R and the
  environment tools.
