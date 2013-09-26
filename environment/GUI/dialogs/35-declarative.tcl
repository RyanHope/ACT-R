# This is the file that contains the declarative memory viewer dialog
# definition.  This is the basic chunk viewer from the environment - the
# button to graph the chunk doesn't do anything yet because there's no 
# graphing tool...

# I'm going to document this one pretty heavily because this is the
# prototype dialog which should allow people to create their own 
# inspectors by analogy to this one without needing to know too much
# Tcl/Tk.  There will also be 3 very simple examples that cover the 
# individual components in the dialogs folder that don't automatically
# put a button on the control panel, but will be available from the options
# menu (at some point).

# The one big complication is that there isn't a "drop-down" selection
# widget in Tk to use for the filter, so I have to roll my own (there's
# an example one in the book I'm using which I've copied from mostly).
# I could use an option menu (which is how the current Mac environment does 
# it) but that's really tricky because there'd need to be some kind of 
# tkwait used for the picking and a special updater type that could get
# things right...


proc make_declarative_viewer {} {
  # create a new toplevel window with a unique name
  # not specifing anything for it - placement, size, fonts, etc.
  # thats going to be done later from a config system
  
  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Declarative" -message "Inspector tools require a current model."
  } else {

    set win [toplevel [new_variable_name .declarative]]
  
    # hide the window for speed and aesthetic reasons
  
    wm withdraw $win

    record_new_window $win $win

    wm geometry $win [get_configuration .declarative $win]
  
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
  
    set list_box [listbox $list_frame.list_box -listvar \
                          $list_frame.list_box.var \
                          -yscrollcommand "$list_frame.list_scrl set" \
                          -selectmode single \
                          -exportselection 0 -font list_font -bd 0]
  
    # tell the Lisp system to create a handler for sending a list 
    # that will set the list variable to the results of eval on 
    # (no-output (wm)) which is the list of all chunks.
    # Have it update on reset and after every cycle
    
    send_environment_cmd "create list-box-handler $list_box $list_box \
                          (lambda (x) (declare (ignore x)) (no-output (dm))) (post) [send_model_name]"
    
    # Make sure that when this window is closed that the Lisp side
    # handler gets removed as well
  
    bind $list_box <Destroy> {
      remove_handler %W
    }

    # create the scroll bar for the listbox
  
    set list_scroll_bar [scrollbar $list_frame.list_scrl \
                                   -command "$list_box yview"]

    # here's the frame for the chunk display

    set text_frame [frame $win.text_frame -borderwidth 0]  
 
    # create a text box to display the chunk and set it so that it also has a
    # scroll bar attached to it and make it noneditable (that may need to change,
    # but for now the viewer windows are going to be static)
  
    set text_box [text $text_frame.text -yscrollcommand \
                       "$text_frame.text_scrl set" -state disabled \
                       -font text_font]
    
    # tell Lisp to create a handler that will send the results of
    # capturing the output stream when the update command is executed
    # but for now there is no update command because there isn't a chunk
    # selected, but it should update after every cycle and on reset.

    send_environment_cmd "create text-output-handler $text_box $text_box \
                          (lambda (x)(declare (ignore x))) (post) [send_model_name]"

    # make sure that when the window is closed the Lisp handler gets removed

    bind $text_box <Destroy> {
      remove_handler %W
    }
  
    # Make the window useable for copy operations on Windows
  
    bind $text_box <1> {focus %W}

    # Here's where the chunk displayed gets tied to the selection in the 
    # list box.  Whenever an item is selected in the list it's going to
    # call select_chunk with that chunk and the text box.  The select_chunk
    # procedure is defined below

    bind $list_box <<ListboxSelect>> "select_chunk %W $text_box"

    # create the scroll bar for the text box
  
    set text_scroll_bar [scrollbar $text_frame.text_scrl \
                                   -command "$text_box yview"]

    set why_not [button $win.whynot_button -text "Why not?" -font button_font \
                            -command "show_why_not_dm $list_box [send_model_name]"]

    # Here is where I create my own drop-down (combobox, whatever you want to 
    # call it) widget for the filter.  This one is actually going to be a button
    # that when pressed displays the list of items to choose from.  I guess 
    # that's slightly outside of the style guidelines, but it doesn't seem too 
    # bad to me but if people don't like it maybe I'll fix it then.

    # first create a toplevel window that will hold the selection list
    # it's got to be a separate window so that things like leaving it's focus
    # can easily be trapped

    set top [toplevel $win.top]

    # remove the titlebar and border and hide it

    wm overrideredirect $top 1
    wm withdraw $top 

    # if the user moves the focus outside of this window then hide it

    bind $top <FocusOut> "wm withdraw $top"

    # now create a listbox to hold the list that drop down
    # as above, give it the special variable for the list, add 
    # scrollbar support, and make it not export the selection

    # start with the frame so it can be scrolled on a Mac

    set drop_frame [frame $top.frame -borderwidth 0] 

    set drop_box [listbox $drop_frame.drop_box \
                          -listvar $drop_frame.drop_box.var \
                          -yscrollcommand "$drop_frame.drop_scrl set" \
                          -exportselection 0 -font list_font -bd 0] 

  
    # tell the Lisp system to create a handler for sending a list 
    # that will set the list variable to the results of eval on 
    # (cons 'none (no-output (chunk-type)) which is the list of all 
    # chunk-types plus the symbol none and only send automatic updates
    # when the system is reset 
  
    send_environment_cmd \
      "create list-box-handler $drop_box $drop_box \
        (lambda (x) \
          (declare (ignore x)) \
          (cons 'none (sort (mapcan (lambda (x) (copy-list (chunk-type-supertypes-fct x))) (hash-table-keys (dm-chunks (get-module-fct 'declarative)))) #'string< :key 'symbol-name))) \
        () [send_model_name]"
  
    # Make sure that when this window is closed that the Lisp side
    # handler gets removed as well
  
    bind $drop_box <Destroy> {
      remove_handler %W
    }
  
    # Now tie the events to this listbox so that it behaves "properly".
    # the setchoice procedure is defined in the server.tcl file because it
    # is used by multiple dialogs.  It basically sets a 
    # global variable to the selection becuase when the widget opens for
    # a selection it waits until something is selected
  
    # Only take the selection when the button is released
  
    bind $drop_box <ButtonRelease-1> "setchoice %W 1"
  
    # take hitting return as a selection
  
    bind $drop_box <Key-Return> "setchoice %W 1"

    # have the esc key close down the interaction
  
    bind $drop_box <Key-Escape> "wm withdraw $top"

    # when the window gets hidden (the focusout event on the container window 
    # above or the esc key on this window) register that as a null selection

    bind $drop_box <Unmap> "setchoice %W 0"
  
    # create a scroll bar for the dropbox list
  
    set drop_scroll_bar [scrollbar $drop_frame.drop_scrl \
                                   -command "$drop_box yview"]

    # now, make a button that will serve as the trigger to drop down the list  
    # with the original filter of "NONE"

    set filter_button [button $win.filter_button -text "none" -borderwidth 2 \
                              -relief sunken -font button_font\
                              -command "declarative_drop_list \
                                                    $win.filter_button \
                                                    $top $drop_frame \
                                                    $drop_box \
                                                    $drop_scroll_bar \
                                                    $list_box"]

    # Add a checkbox to allow specifying which chunk-types to filter on

    set cbox [checkbutton $win.check \
                -text "Current types only" \
                -font checkbox_font \
                -variable $drop_box.check \
                -onvalue 1 -offvalue 0]

    $cbox select

    # just for completeness, create the add-to-graph button
    # that doesn't do anything yet

    #  set add_button [button $win.add_to_graph -text "Add to Graph" \
    #                         -font button_font]

    # here the items are placed into the window using the relative
    # options for those items that need to resize if the window gets resized 
    # could get this from the Visual Tcl/tk GUI builder, or just work it out
    # sizes are based on the default fonts, so if you change that it could
    # look a little "off"

    place $filter_label -x 80 -y 5 -width 40 -height 20
    place $filter_button -x 124 -y 5 -relwidth 1.0 -width -275 -height 24
    place $cbox -relx 1.0 -x -150 -y 5 -width 150 -height 20

#  place $add_button -relx 1.0 -x -92 -y 5 -width 90 -height 20
  
    place $why_not  -x 2 -y 5 -width 75 -height 20

    place $list_frame -relx 0.0 -y 35 -relheight 1.0 -height -35 -relwidth .4
    place $text_frame -relx .4 -y 35 -relheight 1.0 -height -35 -relwidth .6
     
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

# select_chunk
# Given a list widget (listwin) and a tcl window (target_win)
# if there is a Lisp handler for target_win and something selected
# in the listwin send a command to Lisp to request an update of
# target_win with the "pprint" of the chunk selected in listwin.
#
# There's probably something more general that should be done here
# and moved up to the server.tcl file for general comsumption, but
# at this point I'm not sure what that should look like yet...

proc select_chunk {listwin target_win} {
  if [valid_handler_name $target_win] {
    set selections [$listwin curselection]
    if {[llength $selections] != 0} {
      set chunk [$listwin get [lindex $selections 0]]

      send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) \
                (declare (ignore x)) \
                (when (no-output (dm $chunk)) \
                  (when (car (no-output (sgp :esc))) \
                    (suppress-declarative-noise (sdp $chunk)) \
                    (format t \"~%~%\")) \
                  (dm $chunk)))"
    } else {
        send_environment_cmd \
          "update [get_handler_name $target_win] (lambda (x) (Declare (ignore x)))" 
    }
  }
}

# declarative_drop_list
# This is the procedure that implements the interaction of a drop-down
# widget triggered by pressing a button.  Its parameters are that button (but)
# the window (top) that holds a frame (drop_frame) which contains 
# the listbox (drop_box) and the scrollbar (drop_scroll_bar), and the chunk 
# list widget (list_box).  
# It displays the selection listbox below the button and waits for either a 
# selection or the dismissal of the selection box. If there is a selection 
# different from the current one it updates the button to reflect the new 
# choice and issues an update request for the chunk list for chunks of only 
# the requested type.

proc declarative_drop_list {but top drop_frame drop_box \
                            drop_scroll_bar list_box} {
 
  # record the current filter

  set current_filter [$but cget -text]

  # get the new list of chunks in the selection box

  #  it was just this
  #  updater $drop_box
  #  which also does a validity check on $drop_box
  #  but now be sensitive to the check box setting
  #  and I don't think I need the validity check

  upvar $drop_box.check check

  if {$check == 1} {
    send_environment_cmd "update [get_handler_name $drop_box] \
       (lambda (x) \
          (declare (ignore x)) \
          (cons 'none (sort (remove-duplicates (mapcan (lambda (x) (copy-list (chunk-type-supertypes-fct x))) (hash-table-keys (dm-chunks (get-module-fct 'declarative))))) #'string< :key 'symbol-name)))"
  } else {
    send_environment_cmd "update [get_handler_name $drop_box] \
       (lambda (x) \
          (declare (ignore x)) \
          (cons 'none (sort (no-output (chunk-type)) #'string< :key 'symbol-name)))"
  }


  # get a pointer to the list of chunk_types

  upvar $drop_box.var cur_list

  # figure out where the current selection is in the list
  # so that it can be made visible

  set choice [lsearch -exact $cur_list $current_filter]

  # make the selection window as wide as the button and 5 lines high

  set f_height [font metrics list_font -linespace]

  $top configure -width [winfo width $but] -height [expr 5*$f_height]

  # put the selection components into the top level window  

  place $drop_frame -relx 0.0 -rely 0.0 -relheight 1.0  -relwidth 1.0
     
  pack $drop_scroll_bar -side right -fill y 
  pack $drop_box -side left -expand 1 -fill both

  # move the top level window so that it's directly below the button

  set x [winfo rootx $but]
  set y [winfo rooty $but]
  set y [expr $y + [winfo height $but]]

  wm geometry $top "+$x+$y"

  # make it visible and ensure it's on top

  wm deiconify $top
  raise $top

  # set the selected item in the list to the current filter choice and
  # make sure it's visible in the list

  $drop_box activate $choice
  $drop_box see $choice

  # make the selection box the current focus

  focus $drop_box 

  # wait for the user to pick something or dismiss the window (selecting "")

  tkwait variable $drop_box.choice

  # get pointers to the index of the item the user picked and the
  # current_list of chunks (I'm not sure I need to reupvar the list,
  # but since it may have changed since the last time it seems safer to
  # do it this way, but that may just be my lack of understanding of Tcl
  # showing)

  upvar $drop_box.choice new_choice
  upvar $drop_box.var new_list 

  # if the user picked something that was different from
  # what was chosen before then change the button's text and
  # issue an update for the chunk list of the appropriate
  # chunk-types

  if {$new_choice != -1} {
    set new_filter [lindex $new_list $new_choice]
    if {$new_filter != $current_filter} {
      $but configure -text $new_filter
      if {$new_filter == "none"} {
        send_environment_cmd \
          "update [get_handler_name $list_box] (lambda (x) (declare (ignore x)) (no-output (dm)))"
      } else {
        send_environment_cmd \
          "update [get_handler_name $list_box] \
            (lambda (x) \
               (declare (ignore x)) (no-output (sdm isa $new_filter)))"
      }
    }
  } 
 
  # hide the selection window

  wm withdraw $top   
}



proc show_why_not_dm {chunks model} {
  # get the current selection - knowing it's a single selection list box
  set selection [$chunks curselection]

  if {$selection != "" } {
      
    # get the chunk name
    set chunk [$chunks get $selection]
    
    # make a new window 
    set top [toplevel [new_variable_name ".whynotdm"]]

    wm geometry $top [get_configuration .whynotdm $top]

    record_new_window $top "Whynot-dm $chunk"
    
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
           (whynot-dm $chunk)) \
        nil $model"

    # make sure that when the window is closed the Lisp handler gets removed

    bind $text_box <Destroy> {
      remove_handler %W
    }
  }
}


# Make a button for the control panel that will open a new declarative viewer

button [control_panel_name].declarative -command {make_declarative_viewer} \
       -text "Declarative viewer" -font button_font

# put that button on the control panel

pack [control_panel_name].declarative
