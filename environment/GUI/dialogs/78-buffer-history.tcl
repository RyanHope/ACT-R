
proc make_buffer_history_viewer {} {
  
  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Buffer History" -message "Tracing tools require a current model."
  } else {

  set win [toplevel [new_variable_name .buffer_history]]
  
  wm withdraw $win

  record_new_window $win $win

  wm geometry $win [get_configuration .buffer_history $win]
  
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
  set l2 [label $win.l2 -text "Buffers" -justify left -font label_font]
  set l3 [label $win.l3 -text "Details" -justify left -font label_font]

  send_environment_cmd \
      "create list-handler $l1 $win.dummy \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-buffer-history t)) nil) (reset) [send_model_name]"


    bind $l1 <Destroy> "remove_handler $l1"


  # frame for the chunk display

  set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
  set text_box_1 [text $text_frame_1.text -yscrollcommand \
                     "$text_frame_1.text_scrl set"  \
                     -font text_font]
  
  send_environment_cmd "create text-handler $text_box_1 $text_box_1 \
                        (lambda (x)(declare (ignore x)) \" \") nil [send_model_name]"

  bind $text_box_1 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_1 <1> {focus %W}


  # create the scroll bar for the text box
  
  set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl \
                                 -command "$text_box_1 yview"]


  # make both selection lists call the buffer display updater 

  bind $list_box_1 <<ListboxSelect>> "select_buffer_history %W $list_box_2 $text_box_1"
  bind $list_box_2 <<ListboxSelect>> "select_buffer_history $list_box_1 %W $text_box_1"


  button $win.get -text "Get History" -font button_font -command "get_buffer_history_data $list_box_1 $list_box_2"

    
  pack $list_scroll_bar_1 -side right -fill y 
  pack $list_box_1 -side left -expand 1 -fill both
  pack $text_scroll_bar_1 -side right -fill y
  pack $text_box_1 -side left -expand 1 -fill both

  pack $list_scroll_bar_2 -side right -fill y 
  pack $list_box_2 -side left -expand 1 -fill both

  place $win.get -relx 0 -y 0 -height 25 -relwidth .15
  place $l1 -relx 0.0 -y 25 -height 25 -relwidth .15
  place $list_frame_1 -relx 0.0 -y 50 -relheight 1.0 -height -50 -relwidth .15

  place $l2 -relx .15 -y 0 -height 25 -relwidth .3
  place $list_frame_2 -relx .15 -y 25 -relheight 1.0 -height -25 -relwidth .3

  place $l3 -relx .45 -y 0 -height 25 -relwidth .55
  place $text_frame_1 -relx .45 -y 25 -relheight 1.0 -height -25 -relwidth .55
  
  # now show the window 

  wm deiconify $win
  focus $win

  return $win
  }
}


proc get_buffer_history_data {timelst bufferlst} {

  send_environment_cmd "update [get_handler_name $timelst] \
    (lambda (x) \
      (declare (ignore x)) \
      (buffer-history-time-list))"

  send_environment_cmd "update [get_handler_name $bufferlst] \
    (lambda (x) \
      (declare (ignore x)) \
      (buffer-history-buffer-list))"

}



proc select_buffer_history {timewin bufferwin target_win} {
  if [valid_handler_name $target_win] {
    set selections [$bufferwin curselection]
    if {[llength $selections] != 0} {
      set buffer [$bufferwin get [lindex $selections 0]]
      
      set selections [$timewin curselection]
      if {[llength $selections] != 0} {
        set time [$timewin get [lindex $selections 0]]
      
 
        send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) \
                (declare (ignore x)) \
                (buffer-history-text $time '$buffer))"
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

# Make a button for the control panel that will open a new buffer history viewer

button [control_panel_name].buffer_history -command {make_buffer_history_viewer} \
       -text "Buffer History" -font button_font

# put that button on the control panel

pack [control_panel_name].buffer_history
