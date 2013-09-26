
proc make_declarative_history_viewer {} {
  
  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Retrieval History" -message "Tracing tools require a current model."
  } else {

  set win [toplevel [new_variable_name .retrieval_history]]
  
  wm withdraw $win

  record_new_window $win $win

  wm geometry $win [get_configuration .retrieval_history $win]
  
  # frame and list box for times

  set list_frame_1 [frame $win.list_frame_1 -borderwidth 0]  
  
  set list_box_1 [listbox $list_frame_1.list_box -listvar \
                        $list_frame_1.list_box.var \
                        -yscrollcommand "$list_frame_1.list_scrl set" \
                        -selectmode single \
                        -exportselection 0 -font list_font -bd 0]

  
  send_environment_cmd "create list-box-handler $list_box_1 $list_box_1 \
                        (lambda (x) (declare (ignore x))) nil [send_model_name]"
  
  bind $list_box_1 <Destroy> {
    remove_handler %W
  }
  
  set list_scroll_bar_1 [scrollbar $list_frame_1.list_scrl \
                                 -command "$list_box_1 yview"]

  # Frame and list box for chunks

  set list_frame_2 [frame $win.list_frame -borderwidth 0]  
  
  set list_box_2 [listbox $list_frame_2.list_box -listvar \
                        $list_frame_2.list_box.var \
                        -yscrollcommand "$list_frame_2.list_scrl set" \
                        -selectmode single \
                        -exportselection 0 -font list_font -bd 0]

  
  send_environment_cmd "create list-box-handler $list_box_2 $list_box_2 \
                        (lambda (x) (declare (ignore x))) nil [send_model_name]"
  
  bind $list_box_2 <Destroy> {
    remove_handler %W
  }
  
  set list_scroll_bar_2 [scrollbar $list_frame_2.list_scrl \
                                 -command "$list_box_2 yview"]




  # The lables for the sections

  set l1 [label $win.l1 -text "Times" -justify left -font label_font]
  set l2 [label $win.l2 -text "Matching Chunks" -justify left -font label_font]
  set l3 [label $win.l3 -text "Details" -justify left -font label_font]
  set l4 [label $win.l4 -text "Request" -justify left -font label_font]
  set l5 [label $win.l5 -text "Activation" -justify left -font label_font]


  send_environment_cmd \
      "create list-handler $l1 $win.dummy \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-dm-history t :sact t)) nil) (reset) [send_model_name]"


    bind $l1 <Destroy> "remove_handler $l1"


  # frame for the chunk display

  set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
  set text_box_1 [text $text_frame_1.text -yscrollcommand \
                     "$text_frame_1.text_scrl set"  \
                     -xscrollcommand "$text_frame_1.text_scrl_x set" \
                     -font text_font -wrap none]
  
  send_environment_cmd "create text-output-handler $text_box_1 $text_box_1 \
                        (lambda (x)(declare (ignore x))) nil [send_model_name]"

  bind $text_box_1 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_1 <1> {focus %W}


  # create the scroll bar for the text box
  
  set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl \
                                 -command "$text_box_1 yview"]

  set text_scroll_bar_1a [scrollbar $text_frame_1.text_scrl_x \
                                 -command "$text_box_1 xview" -orient horizontal]


  

  # frame for the request display

  set text_frame_2 [frame $win.text_frame_2 -borderwidth 0]  
 
  set text_box_2 [text $text_frame_2.text -yscrollcommand \
                     "$text_frame_2.text_scrl set"  \
                     -font text_font]
  
  send_environment_cmd "create text-output-handler $text_box_2 $text_box_2 \
                        (lambda (x)(declare (ignore x)) \"HI\") nil [send_model_name]"

  bind $text_box_2 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_2 <1> {focus %W}


  # create the scroll bar for the text box
  
  set text_scroll_bar_2 [scrollbar $text_frame_2.text_scrl \
                                 -command "$text_box_2 yview"]



  # bind the selection of a time to the updating of the chunks list and
  # the request box

  bind $list_box_1 <<ListboxSelect>> "select_dm_history_time %W $list_box_2 $text_box_2"

  button $win.get -text "Get History" -font button_font -command "get_dm_history_times $list_box_1"



 # frame for the activation display

  set text_frame_3 [frame $win.text_frame_3 -borderwidth 0]  
 
  set text_box_3 [text $text_frame_3.text -yscrollcommand \
                     "$text_frame_3.text_scrl set"  \
                     -xscrollcommand "$text_frame_3.text_scrl_x set" \
                     -font text_font -wrap none]
  
  send_environment_cmd "create text-output-handler $text_box_3 $text_box_3 \
                        (lambda (x)(declare (ignore x))) nil [send_model_name]"

  bind $text_box_3 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_3 <1> {focus %W}


  # create the scroll bar for the text box
  
  set text_scroll_bar_3 [scrollbar $text_frame_3.text_scrl \
                                 -command "$text_box_3 yview"]

  set text_scroll_bar_3a [scrollbar $text_frame_3.text_scrl_x \
                                 -command "$text_box_3 xview" -orient horizontal]

  # make chunk selection call the both display updaters 


  bind $list_box_2 <<ListboxSelect>> "select_dm_history_chunk %W $list_box_1 $text_box_1; \
                                      select_dm_history_trace %W $list_box_1 $text_box_3"


    
  pack $list_scroll_bar_1 -side right -fill y 
  pack $list_box_1 -side left -expand 1 -fill both
  pack $text_scroll_bar_1 -side right -fill y
  pack $text_scroll_bar_1a -side bottom -fill x
  pack $text_box_1 -side left -expand 1 -fill both

  pack $list_scroll_bar_2 -side right -fill y 
  pack $list_box_2 -side left -expand 1 -fill both
  pack $text_scroll_bar_2 -side right -fill y
  pack $text_box_2 -side left -expand 1 -fill both

  pack $text_scroll_bar_3 -side right -fill y
  pack $text_scroll_bar_3a -side bottom -fill x
  pack $text_box_3 -side left -expand 1 -fill both


  place $win.get -relx 0 -y 0 -height 25 -relwidth .12
  place $l1 -relx 0.0 -y 25 -height 25 -relwidth .12
  place $list_frame_1 -relx 0.0 -y 50 -relheight .6 -height -50 -relwidth .12

  place $l2 -relx .12 -y 0 -height 25 -relwidth .28
  place $list_frame_2 -relx .12 -y 25 -relheight .6 -height -25 -relwidth .28

  place $l3 -relx .4 -y 0 -height 25 -relwidth .60
  place $text_frame_1 -relx .4 -y 25 -relheight .45 -height -25 -relwidth .60
  place $l4 -relx .4 -rely .45 -height 25 -relwidth .60
  place $text_frame_2 -relx .4 -rely .45 -y 25 -relheight .15 -height -25 -relwidth .60
  
  place $l5 -relx .0 -rely .6 -height 25 -relwidth 1.0
  place $text_frame_3 -relx .0 -rely .6 -y 25 -relheight .4 -height -25 -relwidth 1.0


  # now show the window 

  wm deiconify $win
  focus $win

  return $win
  }
}


proc get_dm_history_times {timelst} {

  send_environment_cmd "update [get_handler_name $timelst] \
    (lambda (x) \
      (declare (ignore x)) \
      (dm-history-get-time-list))"
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

proc select_dm_history_chunk {chunkwin timewin target_win} {
  if [valid_handler_name $target_win] {
    set selections [$chunkwin curselection]
    if {[llength $selections] != 0} {
      set chunk [$chunkwin get [lindex $selections 0]]
      
      set selections [$timewin curselection]
      if {[llength $selections] != 0} {
        set time [$timewin get [lindex $selections 0]]
      
 
        send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) \
                (declare (ignore x)) \
                (dm-history-chunk-display $time '$chunk))"
      } else {
        send_environment_cmd \
          "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))" 
      }
    } else {
      send_environment_cmd \
        "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))" 
    }
  }
}


proc select_dm_history_trace {chunkwin timewin target_win} {
  if [valid_handler_name $target_win] {
    set selections [$chunkwin curselection]
    if {[llength $selections] != 0} {
      set chunk [$chunkwin get [lindex $selections 0]]
      
      set selections [$timewin curselection]
      if {[llength $selections] != 0} {
        set time [$timewin get [lindex $selections 0]]
      
 
        send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) \
                (declare (ignore x)) \
                (dm-history-trace-display $time '$chunk))"
      } else {
        send_environment_cmd \
          "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))" 
      }
    } else {
      send_environment_cmd \
        "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))" 
    }
  }
}


proc select_dm_history_time {timewin chunklist request} {
  if [valid_handler_name $request] {
    set selections [$timewin curselection]
    if {[llength $selections] != 0} {
      set time [$timewin get [lindex $selections 0]]

      send_environment_cmd "update [get_handler_name $chunklist] \
            (lambda (x) \
                (declare (ignore x)) \
                (dm-history-chunk-list $time))"

      send_environment_cmd "update [get_handler_name $request] \
            (lambda (x) \
                (declare (ignore x)) \
                (dm-history-request-text $time))"

    } else {
        send_environment_cmd \
          "update [get_handler_name $chunklist] (lambda (x) (Declare (ignore x)))" 

        send_environment_cmd \
          "update [get_handler_name $request] (lambda (x) (Declare (ignore x)))" 

    }
  }
}

# Make a button for the control panel that will open a new declarative viewer

button [control_panel_name].declarative_history -command {make_declarative_history_viewer} \
       -text "Retrieval History" -font button_font

# put that button on the control panel

pack [control_panel_name].declarative_history
