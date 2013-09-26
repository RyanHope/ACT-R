
proc make_bold_multi_graphs {} {
  
  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "BOLD Graphs" -message "BOLD tools require a current model."
  } else {

  set win [toplevel [new_variable_name .bold_graphs]]

  global $win.scale

  upvar $win.scale scale

  set scale 1.0
  
  # hide the window for speed and aesthetic reasons
  
  wm withdraw $win

  record_new_window $win $win

  wm geometry $win [get_configuration .bold_graphs $win]
  
  set list_frame [frame $win.list_frame -borderwidth 0]  
  
  set list_box [listbox $list_frame.list_box -listvar \
                        $list_frame.list_box.var \
                        -yscrollcommand "$list_frame.list_scrl set" \
                        -selectmode multiple \
                        -exportselection 0 -font list_font -bd 0]

  send_environment_cmd "create list-box-handler $list_box $list_box \
                        (lambda (x) (declare (ignore x)) (no-output (buffers))) () [send_model_name]"
  
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

 ## Create a dummy handler to set the :save-buffer-trace parameter to t
 ## whenever a bold-browser window is open.

    send_environment_cmd \
      "create simple-handler $frame $win.dummy \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-buffer-trace t)) nil) (reset) [send_model_name]"


    bind $frame <Destroy> "remove_handler $win.frame"


  send_environment_cmd "create list-handler $win.frame.canvas $win.return \
                        (lambda (x)(declare (ignore x))) () [send_model_name]"

  # make sure that when the window is closed the Lisp handler gets removed

  bind $win.frame.canvas <Destroy> "kill_bold_multi_window $win"
  
  bind $list_box <<ListboxSelect>> "select_multi_buffer_graph %W $win.frame.canvas $win"

  checkbutton $win.check -text "Scale across regions" -variable $win.checkbox -onvalue nil -offvalue t -font checkbox_font -command "select_multi_buffer_graph $list_box $win.frame.canvas $win"

  button $win.zoom_in -command "bold_multi_zoom_in $win" -text "+" -font button_font

  button $win.zoom_out -command  "bold_multi_zoom_out $win" -text "-" -font button_font

  button $win.redisplay -command  "select_multi_buffer_graph $list_box $win.frame.canvas $win" -text "Redisplay" -font button_font

  label $win.start -font text_font -text "Start"
  label $win.stop -font text_font -text "Stop"
 
  entry $win.start_box -textvariable $win.start_time -font text_font
  entry $win.stop_box -textvariable $win.stop_time -font text_font


  pack $win.frame.scrlx -side bottom -fill x
  pack $win.frame.scrly -side right -fill y
  place $win.frame.canvas -relx 0 -rely 0 -relwidth 1.0 -relheight 1.0


  place $win.check -x 0 -y 0 -height 25 -relwidth .18

  place $win.redisplay -relx .18 -y 0 -height 25 -relwidth .15
  place $win.start -relx .33 -y 0 -height 25 -relwidth .10
  place $win.start_box -relx .43 -y 0 -height 25 -relwidth .13
  place $win.stop -relx .56 -y 0 -height 25 -relwidth .10
  place $win.stop_box -relx .66 -y 0 -height 25 -relwidth .13

  place $win.zoom_in -relx 0.8 -y 0 -height 25 -relwidth .09
  place $win.zoom_out -relx 0.9 -y 0 -height 25 -relwidth .09
  

  place $list_frame -relx 0.0 -y 25 -relheight 1.0 -height -25 -relwidth .25
  place $win.frame -relx .25 -y 25 -relheight 1.0 -height -25 -relwidth .75
     
  pack $list_scroll_bar -side right -fill y 
  pack $list_box -side left -expand 1 -fill both

  wm deiconify $win
  focus $win
  }
}

proc bold_multi_zoom_out {win} {

  global $win.scale
  upvar $win.scale scale

  if {[$win.frame.canvas cget -scrollregion] != ""} {
    set scale [expr .5 * $scale]
    resize_window $win .5
  }
}

proc resize_window {win scale} {

  $win.frame.canvas scale all 0 0 $scale 1.0
  $win.frame.canvas configure -scrollregion "0 0 [expr $scale * [lindex [$win.frame.canvas cget -scrollregion] 2]] [$win.frame.canvas cget -height]"
  $win.frame.canvas configure -width [expr $scale * [$win.frame.canvas cget -width]]
}

proc bold_multi_zoom_in {win} {

  global $win.scale
  upvar $win.scale scale

  if {[$win.frame.canvas cget -scrollregion] != ""} {
    set scale [expr 2 * $scale]
    resize_window $win 2
  }    
}


proc kill_bold_multi_window {win} {

  # global $win.return

  send_environment_cmd "update [get_handler_name $win.frame.canvas] \
    (lambda (x) (declare (ignore x)) \
        (uncache-bold-data (cons '[get_handler_name $win.frame.canvas] '$win.frame.canvas)))" 
   
   # Don't want to wait for the update since that's problematic
   # if the model is no longer defined, and it shouldn't matter
   # anyway since the remove can't be handled until the update 
   # finishes.
   # wait_for_non_null $win.return 

   remove_handler $win.frame.canvas
}


proc select_multi_buffer_graph {listwin target_win win} {

  if [valid_handler_name $target_win] {
    set selections [$listwin curselection]
    if {[llength $selections] != 0} {
      
      $target_win delete all

      global $win.checkbox
      global $win.start_time 
      global $win.stop_time 
  
      upvar $win.checkbox local
      upvar $win.start_time start
      upvar $win.stop_time stop

      foreach index $selections {

        set chunk [$listwin get $index]
       
        set starts [scan $start "%f" st]
        set ends [scan $stop "%f" et]

        if {$starts == 0 || $starts == -1} {set st -1}
        if {$ends == 0 || $ends == -1} {set et -1}

        $target_win configure -state disabled

        global $win.return
        set $win.return ""
      
        send_environment_cmd "update [get_handler_name $target_win] \
             (lambda (x) (declare (ignore x))\
                 (parse-bold-predictions-for-graph (cons '[get_handler_name $target_win] '$target_win) '$chunk $local $st $et))"
       
        wait_for_non_null $win.return

        $target_win configure -state normal

        upvar $win.return result

        foreach x $result {
          switch [lindex $x 0] {
            color {
              $listwin itemconfigure $index -selectbackground [lindex $x 1]
            }
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
                          -text [lindex $x 1] -anchor nw 
            }
            text_x {
              $target_win create text [lindex $x 2] [lindex $x 3] \
                          -font env_window_font -fill [lindex $x 4] \
                          -text [lindex $x 1] -anchor n 
            }
            text_y {
              $target_win create text [lindex $x 2] [lindex $x 3] \
                          -font env_window_font -fill [lindex $x 4] \
                          -text [lindex $x 1] -anchor se
            }
          }
        } 
      }

      global $win.scale
      upvar $win.scale scale
      
      if {$scale != 1.0} {resize_window $win $scale}

    } else {

      $target_win delete all

      send_environment_cmd \
          "update [get_handler_name $target_win] (lambda (x) (Declare (ignore x)))" 
    }
  }
}


button [control_panel_name].bold_multi_graphs -command {make_bold_multi_graphs} \
       -text "Buffer graphs" -font button_font

# put that button on the control panel

pack [control_panel_name].bold_multi_graphs
