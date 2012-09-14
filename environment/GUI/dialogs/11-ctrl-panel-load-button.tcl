# The use_smart_load is going to be on the options panel, and it
# determines whether or not the file is just reloaded with the reload
# command (when it's nil) or whether the smart-loader function
# is used.  For now the default is t - use smart-loader
 

global load_return 

button [control_panel_name].load -text "Load Model" -font button_font -command {
  global load_return
  global options_array
  global local_connection

  set fname ""

  if {$local_connection == 0} {
    tk_messageBox -icon warning -type ok -title "Load warning" \
                  -message "You cannot use the Load Model button if the\
                            environment is not running on the same machine\
                            as ACT-R at this time, but if you want such a\
                            feature let Dan know because there are a couple\
                            of ways such a feature could be added."  
  } else {
    set fname [tk_getOpenFile -title "Model to load" \
                              -filetypes {{"All Files" *}} -initialdir $top_dir]
  

    if {$fname != ""} {
      set load_return ""

      send_environment_cmd \
        "update [get_handler_name [control_panel_name].load] \
           (lambda (x) \
              (declare (ignore x)) \
                (if (or (stepper-open-p) (environment-busy-p)) \
                  (list 0 \"ACT-R currently running or the stepper is open.\") \
                (unwind-protect \
                  (progn \
                   (set-environment-busy) \
                   (let ((result (safe-load \"$fname\" $options_array(use_smart_load)))) \
                       (set-environment-free) \
                       (format t (if (= 1 (first result)) \
                               \"~%#|## Model $fname loaded. ##|#~%\" \
                               \"~%#|## Failed loading model $fname. ##|#~%\")) \     
                       result)) \
                  (set-environment-free))))"
  
      wait_for_non_null load_return
 
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


  set the_button [button $win.but -text "Ok" -font button_font -command "destroy $win"]

  place $text_frame -x 0 -y 0 -relheight 1.0 -height -30 -relwidth 1.0
  place $the_button -relx .5 -x -30 -width 60 -rely 1.0 -y -30 -height 30

  pack $text_scroll_bar -side right -fill y
  pack $text_box -side left -expand 1 -fill both


      if {[lindex $load_return 0] == 0} {
        wm title $win "ERROR Loading"
    $text_box insert 1.0 "Error Loading:\n[lindex $load_return 1]"
  } else {
     wm title $win "SUCCESSFUL Load"
    $text_box insert 1.0 "Successful Load:\n[lindex $load_return 1]"
      }

  wm deiconify $win
  focus $win
      
    }
  }              
}

pack [control_panel_name].load

send_environment_cmd \
  "create list-handler [control_panel_name].load load_return (lambda (x)) ()"

bind [control_panel_name].load <Destroy> {
  remove_handler %W
}

