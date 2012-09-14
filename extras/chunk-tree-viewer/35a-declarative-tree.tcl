

proc make_declarative_tree_viewer {} {
  # create a new toplevel window with a unique name
  # not specifing anything for it - placement, size, fonts, etc.
  # thats going to be done later from a config system
  
   if {[currently_selected_model] == "nil"} {

    tk_messageBox -icon info -type ok -title "DM Tree Viewer" \
                  -message "Inspector tools require a current model."
  } else {
  set win [toplevel [new_variable_name .declarative_tree]]
  
  # hide the window for speed and aesthetic reasons
  
  wm withdraw $win
 
  record_new_window $win $win

  global $win.dm_viewer
  
  set $win.dm_viewer 0

  wm geometry $win [get_configuration .declarative_tree $win]
  
  # make the label of the filter - just the text item "Filter:"
  
  set filter_label [label $win.filter_label -text "Filter:" -justify left \
                                            -font label_font]
  
  # On the Mac a scrollbar doesn't work unless it's in a separate "subview"
  # (Mac terminology) with the item to be scrolled, so I've got to do that
  # for everything I want to scroll...

  set list_frame [frame $win.list_frame -borderwidth 0]  
  
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
                        -exportselection 0 -font list_font]

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

  set frame [frame $win.frame -borderwidth 0]  
 
  
  canvas $win.frame.canvas  \
         -xscrollcommand "$win.frame.scrlx set" \
         -yscrollcommand "$win.frame.scrly set" \
         -bg white
   
       
    scrollbar $win.frame.scrlx \
              -command "$win.frame.canvas xview" -orient horizontal

    scrollbar $win.frame.scrly \
              -command "$win.frame.canvas yview" -orient vertical



  send_environment_cmd "create list-handler $win.frame.canvas $win.return \
                        (lambda (x)(declare (ignore x))) () [send_model_name]"

  # make sure that when the window is closed the Lisp handler gets removed

  bind $win.frame.canvas <Destroy> {
    remove_handler %W
  }
  
  # Here's where the chunk displayed gets tied to the selection in the 
  # list box.  Whenever an item is selected in the list it's going to
  # call select_chunk with that chunk and the text box.  The select_chunk
  # procedure is defined below

  bind $list_box <<ListboxSelect>> "select_chunk_tree %W $win.frame.canvas $win"

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
                        -exportselection 0 -font list_font] 

  
  # tell the Lisp system to create a handler for sending a list 
  # that will set the list variable to the results of eval on 
  # (cons 'none (no-output (chunk-type)) which is the list of all 
  # chunk-types plus the symbol none and only send automatic updates
  # when the system is reset 
  
  send_environment_cmd \
    "create list-box-handler $drop_box $drop_box \
      (lambda (x) \
        (declare (ignore x)) \
        (cons 'none (no-output (chunk-type)))) \
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
                            -command "declarative_tree_drop_list \
                                                    $win.filter_button \
                                                    $top $drop_frame \
                                                    $drop_box \
                                                    $drop_scroll_bar \
                                                    $list_box"]


  # here the items are placed into the window using the relative
  # options for those items that need to resize if the window gets resized 
  # could get this from the Visual Tcl/tk GUI builder, or just work it out
  # sizes are based on the default fonts, so if you change that it could
  # look a little "off"


  set save_button [button $win.save -text "Save" -font button_font -command "save_declarative_tree $win.frame.canvas"]

  checkbutton $win.check -text "Show nil slots" -variable $win.hide_nil -onvalue nil -offvalue t -font checkbox_font -command "event generate $list_box <<ListboxSelect>>"

  place $filter_label -x 2 -y 5 -width 40 -height 20
  place $filter_button -x 42 -y 5 -relwidth 1.0 -width -140 -height 20
  place $win.check -x 0 -y 25 -relwidth 1.0 -height 20
  place $save_button -relx 1.0 -x -92 -y 5 -width 90 -height 20


    pack $win.frame.scrlx -side bottom -fill x
    pack $win.frame.scrly -side right -fill y
    place $win.frame.canvas -relx 0 -rely 0 -relwidth 1.0 -relheight 1.0


  place $list_frame -relx 0.0 -y 50 -relheight 1.0 -height -50 -relwidth .4
  place $win.frame -relx .4 -y 50 -relheight 1.0 -height -50 -relwidth .6
     
  pack $list_scroll_bar -side right -fill y 
  pack $list_box -side left -expand 1 -fill both

  wm deiconify $win
  focus $win

  # make sure that every time this window is selected it updates
  # it's contents - both the list box and the message box
  
  bind $win <FocusIn> "updater $list_box
                       updater $win.frame.canvas"
  }
}


proc save_declarative_tree {c} {
  set fname [tk_getSaveFile -title "Save graphic trace as"\
                                  -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  if {$fname != ""} {
    $c postscript -file $fname -width [$c cget -width] -height [$c cget -height] -x 0 -y 0  -pageanchor nw 
  }
}


# select_chunk_tree
# Given a list widget (listwin) and a tcl window (target_win)
# if there is a Lisp handler for target_win and something selected
# in the listwin send a command to Lisp to request an update of
# target_win with the "pprint" of the chunk selected in listwin.
#
# There's probably something more general that should be done here
# and moved up to the server.tcl file for general comsumption, but
# at this point I'm not sure what that should look like yet...

proc select_chunk_tree {listwin target_win win} {

  if [valid_handler_name $target_win] {
    set selections [$listwin curselection]
    if {[llength $selections] != 0} {
      set chunk [$listwin get [lindex $selections 0]]

      $target_win delete all

      global $win.show_nil
      upvar $win.hide_nil hide_nil

      global $win.return
      set $win.return ""

      send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) \
                (declare (ignore x)) \
                (parse-chunk-tree-for-env '$chunk $hide_nil))"
       
       wait_for_non_null $win.return

       upvar $win.return result

       foreach x $result {
         switch [lindex $x 0] {
           size { 
             $target_win configure -width [lindex $x 1]
             $target_win configure -height [lindex $x 2]
             $target_win configure -scrollregion "0 0 [lindex $x 1] [lindex $x 2]"
        
           }
           line {
             $target_win create line [lindex $x 1] [lindex $x 2] \
                         [lindex $x 3] [lindex $x 4] \
                         -fill [lindex $x 5] -width 2
           }
           text {
             $target_win create text [lindex $x 2] [lindex $x 3] \
                              -font env_window_font -fill [lindex $x 4] \
                              -text [lindex $x 1] -anchor n

           
           }
           chunk {
             $target_win create text [lindex $x 2] [lindex $x 3] \
                              -font env_window_font -fill [lindex $x 4] \
                              -text [lindex $x 1] -anchor n -tag [lindex $x 1]

             $target_win bind [lindex $x 1] <ButtonPress> "show_dm_view $listwin $win [lindex $x 1]"

           }
           slot {
             $target_win create text [lindex $x 2] [lindex $x 3] \
                              -font dm_tree_slot_name -fill [lindex $x 4] \
                              -text [lindex $x 1] -anchor n
           }
         }
       } 
      } else {

        $target_win delete all

        send_environment_cmd \
          "update [get_handler_name $target_win] (lambda (x) (Declare (ignore x)))" 
    }
  }

 # focus $target_win
}


proc show_dm_view {list_win win chunk} {
 
  if {[lsearch -exact [$list_win get 0 end] $chunk] != -1} {

  global $win.dm_viewer

  upvar $win.dm_viewer viewer

  if {$viewer == 0 || [winfo exists $viewer] != 1} {

    set win [make_declarative_viewer]

    set box "$win.list_frame.list_box"

    global  $win.list_frame.list_box.var
    wait_for_non_null $win.list_frame.list_box.var

    set index [lsearch -exact [$box get 0 end] $chunk] 

    $box selection set $index

    event generate $box <<ListboxSelect>>

    set viewer $win

  } else {
    
    wm deiconify $viewer
    raise $viewer

    set box "$viewer.list_frame.list_box"

    set index [lsearch -exact [$box get 0 end] $chunk] 

    $box selection clear 0 end

    event generate $box <<ListboxSelect>>

    $box selection set $index

    event generate $box <<ListboxSelect>>


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

proc declarative_tree_drop_list {but top drop_frame drop_box \
                            drop_scroll_bar list_box} {
 
  # record the current filter

  set current_filter [$but cget -text]

  # get the new list of chunks in the selection box

  updater $drop_box
  
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


# Make a button for the control panel that will open a new declarative viewer

button [control_panel_name].declarative_tree -command {make_declarative_tree_viewer} \
       -text "DM Tree viewer" -font button_font

# put that button on the control panel

pack [control_panel_name].declarative_tree
