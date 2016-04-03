
proc make_parameter_viewer {} {
  
  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Parameter Viewer" -message "The parameter viewer requires a current model."
  } else {

  set win [toplevel [new_variable_name .param_viewer]]
  
  wm withdraw $win

  record_new_window $win $win

  wm geometry $win [get_configuration .param_viewer $win]
  
  # frame and list box for modules

  set list_frame_1 [frame $win.list_frame_1 -borderwidth 2]  
  
  set list_box_1 [listbox $list_frame_1.list_box -listvar \
                        $list_frame_1.list_box.var \
                        -yscrollcommand "$list_frame_1.list_scrl set" \
                        -selectmode single \
                        -exportselection 0 -font list_font -bd 0]

  
  send_environment_cmd "create list-box-handler $list_box_1 $list_box_1 \
                        (lambda (x) (declare (ignore x)) (sort (all-module-names) #'string<  :key 'symbol-name)) nil [send_model_name]"
  
  bind $list_box_1 <Destroy> {
    remove_handler %W
  }
  
  set list_scroll_bar_1 [scrollbar $list_frame_1.list_scrl -command "$list_box_1 yview"]

  # Frame and list box for parameters

  set list_frame_2 [frame $win.list_frame -borderwidth 2]  
  
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

  set l1 [label $win.l1 -text "Modules" -justify left -font label_font]
  set l2 [label $win.l2 -text "Parameters" -justify left -font label_font]
 

  # frame for the chunk display

  set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
  set text_box_1 [text $text_frame_1.text -xscrollcommand \
                     "$text_frame_1.text_scrl set"  \
                     -font text_font -width 200 -wrap none]
  
  send_environment_cmd "create text-output-handler $text_box_1 $text_box_1 \
                        (lambda (x)(declare (ignore x)) \" \") nil [send_model_name]"

  bind $text_box_1 <Destroy> {
    remove_handler %W
  }
  
  bind $text_box_1 <1> {focus %W}


  # create the scroll bar for the text box
  
  set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl -command "$text_box_1 xview" -orient horizontal]


  # make selection lists call the appropriate updater 

  bind $list_box_1 <<ListboxSelect>> "select_param_module %W $list_box_2 $text_box_1"
  bind $list_box_2 <<ListboxSelect>> "select_param_value $list_box_1 %W $text_box_1"

  pack $list_scroll_bar_1 -side right -fill y 
  pack $list_box_1 -side left -expand 1 -fill both
  pack $text_scroll_bar_1 -side bottom -fill x
  pack $text_box_1 -side left -expand 1 -fill both

  pack $list_scroll_bar_2 -side right -fill y 
  pack $list_box_2 -side left -expand 1 -fill both

  place $l1 -relx 0.0 -y 0 -height 25 -relwidth .5
  place $list_frame_1 -relx 0.0 -y 25 -relheight 1.0 -height -75 -relwidth .5

  place $l2 -relx .5 -y 0 -height 25 -relwidth .5
  place $list_frame_2 -relx .5 -y 25 -relheight 1.0 -height -75 -relwidth .5

  place $text_frame_1 -relx 0.0 -rely 1.0 -y -45 -height 45 -relwidth 1.0
  
  # now show the window 

  wm deiconify $win
  focus $win

  return $win
  }
}

proc select_param_module {modulewin paramswin target_win} {
  if [valid_handler_name $paramswin] {
    if [valid_handler_name $target_win] {
      set selections [$modulewin curselection]
      if {[llength $selections] != 0} {
        set module [$modulewin get [lindex $selections 0]]
      
        send_environment_cmd "update [get_handler_name $paramswin] \
              (lambda (x) \
                  (declare (ignore x)) \
                  (let ((params nil)) \
                    (maphash (lambda (key value) \
                       (when (eq (act-r-parameter-owner value) '$module) \
                          (push key params))) \
                       *act-r-parameters-table*) \
                  (sort params 'string< :key 'symbol-name)))"
      } else {
          send_environment_cmd "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))" 
      }
    } else {
       send_environment_cmd "update [get_handler_name $paramswin] (lambda (x) (declare (ignore x)))"
       send_environment_cmd "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))" 
    }
  }
}

proc select_param_value {modulewin paramswin target_win} {
  if [valid_handler_name $target_win] {
    set selections [$paramswin curselection]
    if {[llength $selections] != 0} {
      set param [$paramswin get [lindex $selections 0]]
      
      send_environment_cmd "update [get_handler_name $target_win] \
            (lambda (x) \
                (declare (ignore x)) \
                (sgp $param))"
    } else {
        send_environment_cmd "update [get_handler_name $target_win] (lambda (x) (declare (ignore x)))" 
    }
  }
}

# Make a button for the control panel that will open a new buffer history viewer

button [control_panel_name].param_viewer -command {make_parameter_viewer} \
       -text "Parameters" -font button_font

# put that button on the control panel

pack [control_panel_name].param_viewer
