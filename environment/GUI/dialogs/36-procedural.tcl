# This is the file that contains the procedural memory viewer dialog
# definition.  It's copied almost directly from the declarative viewer


proc make_procedural_viewer {} {
  # create a new toplevel window with a unique name
  # not specifing anything for it - placement, size, fonts, etc.
  # thats going to be done later from a config system
  
   if {[currently_selected_model] == "nil"} {

    tk_messageBox -icon info -type ok -title "Procedural" \
                  -message "Inspector tools require a current model."
  } else {
 
  set win [toplevel [new_variable_name .procedural]]
  
  # hide the window for speed and aesthetic reasons
  
  wm withdraw $win

  record_new_window $win $win

  wm geometry $win [get_configuration .procedural $win]
  
  # make the label of the filter - just the text item "Filter:"
  
  set filter_label [label $win.filter_label -text "Filter:" -justify left \
                          -font label_font]

  # On the Mac a scrollbar doesn't work unless it's in a separate "subview"
  # (Mac terminology) with the item to be scrolled, so I've got to do that
  # for everything I want to scroll...

  set list_frame [frame $win.list_frame -borderwidth 2]  
  
  # create the listbox that uses a special variable <name>.var for its list
  # so that the list-box-handler can ensure that the selection remains the
  # same over updates, tie it to a vertical scroll bar, set it so that
  # there is only a single selection possible, and don't export the selection
  # otherwise there's only every going to be 1 "selected" chunk in all the
  # list boxes 
  
  set list_box [listbox $list_frame.list_box \
                        -listvar $list_frame.list_box.var \
                        -yscrollcommand "$list_frame.list_scrl set" \
                        -selectmode single -font list_font\
                        -exportselection 0 -bd 0]

  # tell the Lisp system to create a handler for sending a list 
  # that will set the list variable to the results of eval on 
  # (no-output (pp)) which is the list of all productions.
  # Have it update on reset and after every cycle hooks.
  
  send_environment_cmd "create list-box-handler $list_box $list_box \
                        (lambda (x) (declare (ignore x)) (no-output (pp))) (post) [send_model_name]"
  
  # Make sure that when this window is closed that the Lisp side
  # handler gets removed as well
  
  bind $list_box <Destroy> {
    remove_handler %W
  }

  # create the scroll bar for the listbox
  
  set list_scroll_bar [scrollbar $list_frame.list_scrl \
                                 -command "$list_box yview"]

  # here's the frame for the production display

  set text_frame [frame $win.text_frame -borderwidth 0]  

  # create a text box to display the production and set it so that it has a
  # scroll bar attached to it and make it noneditable (that may need to change,
  # but for now the viewer windows are going to be static)
  
  set text_box [text $text_frame.text -yscrollcommand \
                     "$text_frame.text_scrl set" -state disabled \
                     -font text_font]
  
  # tell Lisp to create a handler that will send the results of
  # capturing the output stream when the update command is executed
  # but for now there is no update command because there isn't a production
  # selected but it should update after every cycle and on a reset.

  send_environment_cmd \
    "create text-output-handler $text_box $text_box (lambda (x) (declare (ignore x))) (post) [send_model_name]"

  # make sure that when the window is closed the Lisp handler gets removed

  bind $text_box <Destroy> {
    remove_handler %W
  }
  
  # Make the window useable for copy operations on Windows
  
  bind $text_box <1> {focus %W}


  # Here's where the production displayed gets tied to the selection in the 
  # list box.  Whenever an item is selected in the list it's going to
  # call select_production with that production and the text box.  The 
  # select_production procedure is defined below

  bind $list_box <<ListboxSelect>> "select_production %W $text_box"

  # create the scroll bar for the text box
  
  set text_scroll_bar [scrollbar $text_frame.text_scrl \
                                 -command "$text_box yview"]

  # make the whynot button which will open a window and explain why or why not
  # the currently selected production can fire

  set why_not [button $win.whynot_button -text "Why not?" -font button_font \
                            -command "show_why_not $list_box [send_model_name]"]

  
  # here the items are placed into the window using the relative
  # options for those items that need to resize if the window gets resized 
  # could get this from the Visual Tcl/tk GUI builder, or just work it out

  place $why_not -x 2 -y 5 -width 75 -height 24

  place $list_frame -relx 0.0 -y 30 -relheight 1.0 -height -30 -relwidth .4
  place $text_frame -relx .4 -y 30 -relheight 1.0 -height -30 -relwidth .6
     
  pack $list_scroll_bar -side right -fill y 
  pack $list_box -side left -expand 1 -fill both
  pack $text_scroll_bar -side right -fill y
  pack $text_box -side left -expand 1 -fill both

  # now show the window 

  wm deiconify $win
  focus $win

  # make sure that every time this window is selected it updates
  # it's contents - both the list box and the message box
  
  bind $win <FocusIn> "updater $list_box
                       updater $text_box"

  return $win
}
}

# select_production
# Given a list widget (listwin) and a tcl window (target_win)
# if there is a Lisp handler for target_win and something selected
# in the listwin send a command to Lisp to request an update of
# target_win with the "pprint" of the production selected in listwin.
#
# There's probably something more general that should be done here
# and moved up to the server.tcl file for general comsumption, but
# at this point I'm not sure what that should look like yet...

proc select_production {listwin target_win} {
  if [valid_handler_name $target_win] {
    set selections [$listwin curselection]
    if {[llength $selections] != 0} {
      set prod [$listwin get [lindex $selections 0]]

      send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) \
                (declare (ignore x)) \
                   (when (car (no-output (sgp :esc))) \
                     (spp $prod) \
                     (format t \"~%~%\")) \
                   (pp $prod)))"
    } else {
      send_environment_cmd "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))"
    }
  }
}


# show_why_not
# This is the procedure that gets called when the Whynot? button is pressed.
# It gets 1 parameter which is the corresponding window's production list box
# of productions.  If one of those productions is selected a new window is
# created with the title "Whynot X" where X is the name of that production and
# the contents of that window are the current time, a note as to whether or not
# this production matches because this confuses beginners, and then the act-r
# whynot report on the production.

proc show_why_not {prods model} {
  # get the current selection - knowing it's a single selection list box
  set selection [$prods curselection]

  if {$selection != "" } {
      
    # get the production name
    set prod [$prods get $selection]
    
    # make a new window 
    set top [toplevel [new_variable_name ".whynot"]]

    wm geometry $top [get_configuration .whynot $top]

    record_new_window $top "Whynot $prod"
    
    # create a text_box to hold the output and put it in the window
    # also need a frame to hold it and the scroll bar

    frame $top.frame -borderwidth 0  
    
    set text_box [text $top.frame.text -font text_font -state disabled \
                                       -yscrollcommand "$top.frame.scrl set" ]

    set scrl_bar [scrollbar $top.frame.scrl -command "$top.frame.text yview"]
   
    place $top.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
    pack $scrl_bar -side right -fill y 
    pack $text_box -side left -expand 1 -fill both
 
 
    # create a handler for this window to generate an update 
    # but won't need to ever request another update
    send_environment_cmd \
      "create text-output-handler $text_box $text_box \
        (lambda (x) \
           (declare (ignore x)) \
           (format t \"Time: ~s~%\" (mp-time)) \
           (whynot $prod)) \
        nil $model"

    # make sure that when the window is closed the Lisp handler gets removed

    bind $text_box <Destroy> {
      remove_handler %W
    }
  }
}


# Make a button for the control panel that will open a new procedural viewer

button [control_panel_name].procedural -command {make_procedural_viewer} \
                                 -text "Procedural viewer" -font button_font

# put that button on the control panel

pack [control_panel_name].procedural
