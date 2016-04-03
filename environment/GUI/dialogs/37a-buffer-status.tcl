

proc make_buffer_status_viewer {} {
  # create a new toplevel window with a unique name
  # not specifing anything for it - placement, size, fonts, etc.
  # thats going to be done later from a config system
  
  if {[currently_selected_model] == "nil"} {

    tk_messageBox -icon info -type ok -title "Buffer Status" \
                  -message "Inspector tools require a current model."
  } else {
 
  set win [toplevel [new_variable_name .bufferstatus]]
  
  # hide the window for speed and aesthetic reasons
  
  wm withdraw $win

  record_new_window $win $win

  wm geometry $win [get_configuration .bufferstatus $win]
  
    
  # On the Mac a scrollbar doesn't work unless it's in a separate "subview"
  # (Mac terminology) with the item to be scrolled, so I've got to do that
  # for everything I want to scroll...

  set list_frame [frame $win.list_frame -borderwidth 2]  
  
  # create the listbox that uses a special variable <name>.var for its list
  # so that the list-box-handler can ensure that the selection remains the
  # same over updates, tie it to a vertical scroll bar, set it so that
  # there is only a single selection possible, and don't export the selection
  # otherwise there's only every going to be 1 "selected" buffer in all the
  # list boxes 
  
  set list_box [listbox $list_frame.list_box \
                        -listvar $list_frame.list_box.var \
                        -yscrollcommand "$list_frame.list_scrl set" \
                        -selectmode single \
                        -exportselection 0 -font list_font -bd 0]

  # tell the Lisp system to create a handler for sending a list 
  # that will set the list variable for the available buffers
 

  send_environment_cmd \
    "create list-box-handler $list_box $list_box buffer-list () [send_model_name]"
  
  # Make sure that when this window is closed that the Lisp side
  # handler gets removed as well
  
  bind $list_box <Destroy> {
    remove_handler %W
  }

  # create the scroll bar for the listbox
  
  set list_scroll_bar [scrollbar $list_frame.list_scrl -command "$list_box yview"]

  # here's the frame for the buffer's chunk display

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

  send_environment_cmd \
    "create text-output-handler $text_box $text_box (lambda (x) (declare (ignore x))) (post) [send_model_name]"

  # make sure that when the window is closed the Lisp handler gets removed

  bind $text_box <Destroy> {
    remove_handler %W
  }

  # Make the window useable for copy operations on Windows
  
  bind $text_box <1> {focus %W}
  
  # Here's where the chunk displayed gets tied to the selection in the 
  # list box.  Whenever an item is selected in the list it's going to
  # call select_buffer with that buffer and the text box.  The select_buffer
  # procedure is defined below

  set m [currently_selected_model]
  
  bind $list_box <<ListboxSelect>> "select_buffer_status %W $text_box $m"

  # create the scroll bar for the text box
  
  set text_scroll_bar [scrollbar $text_frame.text_scrl -command "$text_box yview"]

  # here the items are placed into the window using the relative
  # options for those items that need to resize if the window gets resized 
  # could get this from the Visual Tcl/tk GUI builder, or just work it out
  # sizes are based on the default fonts, so if you change that it could
  # look a little "off"

  place $list_frame -relx 0.0 -rely 0.0 -relheight 1.0 -relwidth .4
  place $text_frame -relx .4 -rely 0.0 -relheight 1.0 -relwidth .6
     
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
}
}
# select_buffer_status
# Given a list widget (listwin) and a tcl window (target_win)
# if there is a Lisp handler for target_win and something selected
# in the listwin send a command to Lisp to request an update of
# target_win with the result of the buffer_contents command.

proc select_buffer_status {listwin target_win model_name} {

  global options_array 

  if [valid_handler_name $target_win] {
    set selections [$listwin curselection]
    set top_win [winfo parent [winfo parent $listwin]]

    if {[llength $selections] != 0} {
      set buffer [$listwin get [lindex $selections 0]]

      send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) (declare (ignore x)) (buffer-status-fct (list '$buffer)))"
      
      if {$options_array(multiple_models) == 1} {
        wm title $top_win "$buffer Buffer Status viewer ($model_name)"
      } else {
        wm title $top_win "$buffer Buffer Status viewer"
      }

    } else {
      send_environment_cmd "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))"

      if {$options_array(multiple_models) == 1} {
        wm title $top_win "$top_win ($model_name)"
      } else {
        wm title $top_win $top_win
      }
    }
  }
}

# Make a button for the control panel that will open a new declarative viewer

button [control_panel_name].buffer_status -command {make_buffer_status_viewer} \
                    -text "Buffer Status viewer" -font button_font

# put that button on the control panel

pack [control_panel_name].buffer_status
