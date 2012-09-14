global reload_return
global during_reload

set during_reload 0

frame [control_panel_name].r_frame -borderwidth 0

button [control_panel_name].r_frame.reset -text "Reset" -font button_font -command {
  send_environment_cmd \
    "update [get_handler_name [control_panel_name].r_frame.reset] \
        reset-model-env"
} 

button [control_panel_name].r_frame.reload -text "Reload" -font button_font -command {
  global options_array
  global reload_return
  global during_reload

  set reload_return ""

  set during_reload 1

  global save_return
  global current_open_model

  if {[info exists save_return] == 1 && [info exists current_open_model] == 1 && $options_array(save_before_reload) == 1 && $current_open_model != ""} {
    save_model
  }

  
  send_environment_cmd \
    "update [get_handler_name [control_panel_name].r_frame.reload] \
        (lambda (x) \
            (declare (ignore x)) \
            (reload-model $options_array(use_smart_load)))"

  wait_for_non_null reload_return

  set win [toplevel [new_variable_name .reload_response]]
  
  # hide the window for speed and aesthetic reasons
  
  wm withdraw $win

  wm geometry $win [get_configuration .reload_response $win]


 set text_frame [frame $win.text_frame -borderwidth 0]  
 
 set text_box [text $text_frame.text -yscrollcommand \
                     "$text_frame.text_scrl set" -state normal \
                     -font text_font]
  
  
  
  set text_scroll_bar [scrollbar $text_frame.text_scrl \
                                 -command "$text_box yview"]


  set the_button [button $win.but -text "Ok" -command "destroy $win" -font button_font]

  place $text_frame -x 0 -y 0 -relheight 1.0 -height -30 -relwidth 1.0
  place $the_button -relx .5 -x -30 -width 60 -rely 1.0 -y -30 -height 30

  pack $text_scroll_bar -side right -fill y
  pack $text_box -side left -expand 1 -fill both


  if {[lindex $reload_return 0] == 0} {
    wm title $win "ERROR Reloading"
    $text_box insert 1.0 "Error Reloading:\n[lindex $reload_return 1]"
  } else {
    wm title $win "SUCCESSFUL Reload"
    $text_box insert 1.0 "Successful Reload:\n[lindex $reload_return 1]"
  }

  set during_reload 0

  wm deiconify $win
  focus $win
}
      
pack [control_panel_name].r_frame.reset -side left
pack [control_panel_name].r_frame.reload -side right

pack [control_panel_name].r_frame

send_environment_cmd \
  "create simple-handler [control_panel_name].r_frame.reset ignore_returns \
     (lambda (x) (declare (ignore x))) ()"

send_environment_cmd \
  "create list-handler [control_panel_name].r_frame.reload reload_return \
    (lambda (x) (declare (ignore x))) ()"

bind [control_panel_name].r_frame.reset <Destroy> {
  remove_handler %W
}

bind [control_panel_name].r_frame.reload <Destroy> {
  remove_handler %W
}
