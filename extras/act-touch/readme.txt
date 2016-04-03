ACT-Touch is a set of manual motor request extensions for ACT-R. These manual
request extensions constitute theoretical claims predicting motor preparation
and execution times for certain manual gestures commonly used with multitouch
computer displays. These manual request extensions follow ACT-R's theoretical
claims about how cognition interfaces with action to output information from 
the human to the environment, which in turn originated with the EPIC 
architecture.

As ACT-Touch extends ACT-R's framework with additional manual movement 
vocabulary, many movement styles are analogous to extant ACT-R movement styles
in the sense that a movement is composed of a certain set of features which 
specify the movement such as a hand, a finger, a direction and a distance. 
Consequently ACT-Touch's movement styles are subject to the same constraints
and caveats as ACT-R's, e.g., finger positions that would be physically 
impossible for any physical human hand to attain are specifiable for a cognitive
model, and so it is up to the modeler to consider such things.

ACT-Touch is implemented as Lisp code that is meant to be loaded with the
ACT-R software.  There are 5 files included in this directory (in addition
to this readme file):
 - act-touch.lisp, which is the set of manual request extensions.
 - virtual-multitouch-device.lisp, implements a demonstration ACT-R device 
   to handle ACT-Touch's manual requests.
 - act-touch-demo-model.lisp, a demonstration model which uses the demonstration
   device. 
 - act-touch-reference-manual.pdf, a reference manual for ACT-Touch.
 - LGPL.txt, a text file of the LGPL license under which this software is
   distributed.


To use ACT-Touch the act-touch.lisp file must be loaded after loading ACT-R.
The easiest way to do that is to place the act-touch.lisp file into the ACT-R
user-loads directory so that it will be loaded automatically after the main
ACT-R files have been loaded.  The virtual-multitouch-device.lisp file may be
placed there as well if one wants to use that device for model interactions.

With act-touch.lisp and virtual-multitouch-device.lisp loaded one can load
the demonstration model file act-touch-demo-model.lisp and then call the 
function run-touch-demo with no parameters to run the demo.

ACT-Touch can also be downloaded from Cogscent, LLC's website:
http://cogscent.com/.

Direct technical support inquiries regarding ACT-Touch to Frank Tamborello:
frank.tamborello@cogscent.com.

This version of ACT-Touch is only compatible with ACT-R 6.1.  A version that
that can be used with ACT-R 6.0 is available on Cogscent, LLC's website.