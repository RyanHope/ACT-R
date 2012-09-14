This directory contains a module which can be used to provide models with the short 
term base-level activation component described in:

   Lebiere, C., & Best, B. J. (2009). Balancing Long-Term Reinforcement and
   Short-Term Inhibition.  Proceedings of the 31st Annual Conference of the
   Cognitive Science Society (pp. 2378-2383).  Austin, TX: Cognitive Science Society.

----------------------------------------------------------

The files in this directory are:

readme.txt - this file
bl-inhibition.lisp - the module definition
free-recall.lisp - an example model that runs with or without inhibition enabled 
Cogsci 2009 Lebiere Final.pdf - a pdf of Christian's slides from the Cogsci. presentation
Cogsci09-Lebiere-Best.pdf - a pdf of an updated version of the Cogsci. the paper 

----------------------------------------------------------

To use this extension put the bl-inhibition.lisp file into the modules directory 
before loading the main ACT-R load file, or load it once after loading ACT-R and 
before defining any models.

That will then add the module for this mechanism to the system and add these
three new parameters: :enable-inhibition, :inhibition-scale and :inhibition-decay.

With this module added the declarative chunk activations now have an additional
term added to the equation when the :enable-inhibition parameter is set to t
(its default value) and base-level learning is enabled with at least one past 
reference being included (either :ol nil or a number).  The new term is:

                    - log [1 + (Tn/ts)^-ds]

  Tn is the time since the most recent reference of the chunk
  ds is the value of the :inhibition-decay parameter
  ts is the value of the :inhibition-scale parameter

----------------------------------------------------------

The example model is a simple free recall task where the model retrieves 
chunks from DM.  The model has 10 chunks in DM with equal reference histories
and the productions simply retrieve a chunk and then harvest that retrieval.

To run the example model load the free-recall.lisp file and then call the 
function  free-recall.  That will run the model through a number of trials
(100 by default) of recalling items with a delay (1 second by default) between
each trial.  The function will return a list of the frequencies of recall for
each of the 10 items.  With no parameters given, free-recall runs without the
base-level inhibition enabled.  To enable it you need to provide the values for
the scale and decay parameters using the keywords :scale and :decay.  

After runnig the free-recall task you can use the functions display-all-base-levels
and graph-all-base-levels to see what the activations of the chunks looked like
during the last free-recall run.  display-all-base-levels will print out a table
with the activation values.  If you are using the ACT-R Environment or a Lisp with 
a GUI that has a supported ACT-R device (ACL, LispWorks, or MCL) then the graph-all-
base-levels function will draw a graph of those activation values.