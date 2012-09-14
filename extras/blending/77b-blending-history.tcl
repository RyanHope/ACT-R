
proc make_blending_history_viewer {} {
  
  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Blending History" -message "Tracing tools require a current model."
  } else {

  set win [toplevel [new_variable_name .blend_history]]
  
  wm withdraw $win

  record_new_window $win $win

  wm geometry $win [get_configuration .blend_history $win]
  
  # frame and list box for times

  set list_frame_1 [frame $win.list_frame_1 -borderwidth 0]  
  
  set list_box_1 [listbox $list_frame_1.list_box -listvar \
                        $list_frame_1.list_box.var \
                        -yscrollcommand "$list_frame_1.list_scrl set" \
                        -selectmode single \
                        -exportselection 0 -font list_font]

  
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
                        -exportselection 0 -font list_font]

  
  send_environment_cmd "create list-box-handler $list_box_2 $list_box_2 \
                        (lambda (x) (declare (ignore x))) nil [send_model_name]"
  
  bind $list_box_2 <Destroy> {
    remove_handler %W
  }
  
  set list_scroll_bar_2 [scrollbar $list_frame_2.list_scrl \
                                 -command "$list_box_2 yview"]




  # The lables for the sections

  set l1 [label $win.l1 -text "Times" -justify left -font label_font]
  set l2 [label $win.l2 -text "Blended Chunks" -justify left -font label_font]
  set l3 [label $win.l3 -text "Chunk" -justify left -font label_font]
  set l4 [label $win.l4 -text "Request" -justify left -font label_font]
  set l5 [label $win.l5 -text "Result" -justify left -font label_font]
  set l6 [label $win.l6 -text "Blending" -justify left -font label_font]
  set l7 [label $win.l7 -text "Activation" -justify left -font label_font]



  send_environment_cmd \
      "create list-handler $l1 $win.dummy \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-blending-history t :sact t :sblt t)) nil) (reset) [send_model_name]"


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

  set text_scroll_bar_2 [scrollbar $text_frame_2.text_scrl \
                                 -command "$text_box_2 yview"]


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

  
  set text_scroll_bar_3 [scrollbar $text_frame_3.text_scrl \
                                 -command "$text_box_3 yview"]

  set text_scroll_bar_3a [scrollbar $text_frame_3.text_scrl_x \
                                 -command "$text_box_3 xview" -orient horizontal]


 # frame for the blending-trace display

  set text_frame_4 [frame $win.text_frame_4 -borderwidth 0]  
 
  set text_box_4 [text $text_frame_4.text -yscrollcommand \
                     "$text_frame_4.text_scrl set"  \
                     -xscrollcommand "$text_frame_4.text_scrl_x set" \
                     -font text_font -wrap none]
  
  send_environment_cmd "create text-output-handler $text_box_4 $text_box_4 \
                        (lambda (x)(declare (ignore x))) nil [send_model_name]"

  bind $text_box_4 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_4 <1> {focus %W}

  
  set text_scroll_bar_4 [scrollbar $text_frame_4.text_scrl \
                                 -command "$text_box_4 yview"]

  set text_scroll_bar_4a [scrollbar $text_frame_4.text_scrl_x \
                                 -command "$text_box_4 xview" -orient horizontal]

 # frame for the result display

  set text_frame_5 [frame $win.text_frame_5 -borderwidth 0]  
 
  set text_box_5 [text $text_frame_5.text -yscrollcommand \
                     "$text_frame_5.text_scrl set"  \
                     -font text_font]
  
  send_environment_cmd "create text-output-handler $text_box_5 $text_box_5 \
                        (lambda (x)(declare (ignore x)) \"HI\") nil [send_model_name]"

  bind $text_box_5 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_5 <1> {focus %W}

  set text_scroll_bar_5 [scrollbar $text_frame_5.text_scrl \
                                 -command "$text_box_5 yview"]





  # bind the selection of a time to the updating of the chunks list, request box,
  # result box, and blending trace

  bind $list_box_1 <<ListboxSelect>> "select_blend_history_time %W $list_box_2 $text_box_2 $text_box_5 $text_box_4"

  # make chunk selection call two display updaters 
  # Why those aren't together is unclear since the previous ones are all together...

  bind $list_box_2 <<ListboxSelect>> "select_blend_history_chunk %W $list_box_1 $text_box_1; \
                                      select_blend_history_trace %W $list_box_1 $text_box_3"



  button $win.get -text "Get History" -font button_font -command "get_blend_history_times $list_box_1"
    
  pack $list_scroll_bar_1 -side right -fill y 
  pack $list_box_1 -side left -expand 1 -fill both
  pack $list_scroll_bar_2 -side right -fill y 
  pack $list_box_2 -side left -expand 1 -fill both

  pack $text_scroll_bar_1 -side right -fill y
  pack $text_scroll_bar_1a -side bottom -fill x
  pack $text_box_1 -side left -expand 1 -fill both

  pack $text_scroll_bar_2 -side right -fill y
  pack $text_box_2 -side left -expand 1 -fill both

  pack $text_scroll_bar_3 -side right -fill y
  pack $text_scroll_bar_3a -side bottom -fill x
  pack $text_box_3 -side left -expand 1 -fill both

  pack $text_scroll_bar_4 -side right -fill y
  pack $text_scroll_bar_4a -side bottom -fill x
  pack $text_box_4 -side left -expand 1 -fill both

  pack $text_scroll_bar_5 -side right -fill y
  pack $text_box_5 -side left -expand 1 -fill both


  place $win.get -relx 0 -y 0 -height 25 -relwidth .10
  place $l1 -relx 0.0 -y 25 -height 25 -relwidth .10
  place $list_frame_1 -relx 0.0 -y 50 -relheight .5 -height -50 -relwidth .10

  place $l4 -relx .10 -y 0 -height 25 -relwidth .2
  place $text_frame_2 -relx .10 -y 25 -relheight .25 -height -25 -relwidth .2
  place $l5 -relx .10 -rely .25 -height 25 -relwidth .2
  place $text_frame_5 -relx .10 -rely .25 -y 25 -relheight .25 -height -25 -relwidth .2
  
  place $l2 -relx .3 -y 0 -height 25 -relwidth .2
  place $list_frame_2 -relx .3 -y 25 -relheight .5 -height -25 -relwidth .2

  place $l3 -relx .5 -y 0 -height 25 -relwidth .5
  place $text_frame_1 -relx .5 -y 25 -relheight .25 -height -25 -relwidth .5
  place $l7 -relx .5 -rely .25 -height 25 -relwidth .5
  place $text_frame_3 -relx .5 -rely .25 -y 25 -relheight .25 -height -25 -relwidth .5
  
  place $l6 -relx 0 -rely .5 -height 25 -relwidth 1.0
  place $text_frame_4 -relx 0 -rely .5 -y 25 -relheight .5 -height -25 -relwidth 1.0

  
  # now show the window 

  wm deiconify $win
  focus $win

  return $win
  }
}


proc get_blend_history_times {timelst} {

  send_environment_cmd "update [get_handler_name $timelst] \
    (lambda (x) \
      (declare (ignore x)) \
      (blend-history-get-time-list))"
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

proc select_blend_history_chunk {chunkwin timewin target_win} {
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
                (blend-history-chunk-display $time '$chunk))"
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


proc select_blend_history_trace {chunkwin timewin target_win} {
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
                (blend-history-chunk-trace-display $time '$chunk))"
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


# needs to update request, result, blending trace and chunks list now...
# chunks list, request box, result box, and blending trace

proc select_blend_history_time {timewin chunklist request result blend} {
  if [valid_handler_name $request] {
    set selections [$timewin curselection]
    if {[llength $selections] != 0} {
      set time [$timewin get [lindex $selections 0]]

      send_environment_cmd "update [get_handler_name $chunklist] \
            (lambda (x) \
                (declare (ignore x)) \
                (blend-history-chunk-list $time))"

      send_environment_cmd "update [get_handler_name $request] \
            (lambda (x) \
                (declare (ignore x)) \
                (blend-history-request-text $time))"

      send_environment_cmd "update [get_handler_name $result] \
            (lambda (x) \
                (declare (ignore x)) \
                (blend-history-result-display $time))"

      send_environment_cmd "update [get_handler_name $blend] \
            (lambda (x) \
                (declare (ignore x)) \
                (blend-history-trace-display $time))"


    } else {
        send_environment_cmd \
          "update [get_handler_name $chunklist] (lambda (x) (Declare (ignore x)))" 

        send_environment_cmd \
          "update [get_handler_name $request] (lambda (x) (Declare (ignore x)))" 

        send_environment_cmd \
          "update [get_handler_name $result] (lambda (x) (Declare (ignore x)))" 

        send_environment_cmd \
          "update [get_handler_name $blend] (lambda (x) (Declare (ignore x)))" 
    }
  }
}

# Make a button for the control panel that will open a new declarative viewer

button [control_panel_name].blending_history -command {make_blending_history_viewer} \
       -text "Blending History" -font button_font

# put that button on the control panel

pack [control_panel_name].blending_history
