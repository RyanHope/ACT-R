label [control_panel_name].current_load -text "Current Model" -font label_font

tk_optionMenu [control_panel_name].current_model_name global_current_model_name "No Current Model"

[control_panel_name].current_model_name configure -font label_font

[[control_panel_name].current_model_name cget -menu] configure -font button_font


pack [control_panel_name].current_load
pack [control_panel_name].current_model_name

send_environment_cmd \
  "create simple-funcall-handler [control_panel_name].current_model_name set_current_model_value \
    (lambda (x) (case (environment-control-which-hook *environment-control*) \
                   (create (format nil \"create ~(~a~)\" x)) \
                   (delete (format nil \"delete ~(~a~)\" (current-model))) \
                   (t      (format nil \"check \\\"~{ ~(~a~)~}\\\" \" (mp-models))))) \
    (create delete)"

bind [control_panel_name].current_model_name <Destroy> {
  remove_handler %W
}


proc currently_selected_model {} {
  global global_current_model_name
  
  if {$global_current_model_name == "No Current Model" } {
    return "nil"
  } else {
    return $global_current_model_name
  }
}



proc model_in_current_list {model} {

  set menu [[control_panel_name].current_model_name cget -menu]
  set last [$menu index last]

  
  for {set i 0} {$i <= $last} {incr i} {
    if {$model == [$menu entrycget $i -value]} {
      return 1
    }
  }
  return 0
}
 
proc set_currently_selected_model {model} {
  global global_current_model_name

  if [model_in_current_list $model] {
    set global_current_model_name $model
    return 1
  } else {
    return 0
  }
}

proc set_current_model_value {action name} {
  global global_current_model_name
  global options_array
  global during_reload
  global tcl_env_dir

  set menu [[control_panel_name].current_model_name cget -menu]
  set top [$menu entrycget 0 -value]

  switch $action {
    create {
      if {$top == "No Current Model"} {
        $menu delete "No Current Model"
      } elseif {$options_array(multiple_models) == 0} {
        tk_messageBox -icon warning -message "You must enable multiple model support in the Options before the tools will work correctly when more than one model is defined." \
                  -type ok -title "Multiple models not enabled"
      }      

      $menu add radiobutton -label $name -variable global_current_model_name
      set global_current_model_name $name
    }

    delete {
      $menu delete $name 
      
      if {[$menu entrycget 0 -value] == ""} {
        $menu add radiobutton -label "No Current Model" -variable global_current_model_name
      }      
      
      if {$global_current_model_name == $name} {
        set global_current_model_name [$menu entrycget 0 -value]
      }

      if {$options_array(multiple_models) == 1 && $during_reload == 0} {
      
        if {$options_array(kill_model_windows) == 1} {
          kill_all_model_windows $name
        } 
        #   don't show the dialog anymore because it isn't really modal and runs into 
        #   problems if there are multiple models being deleted which will always be
        #   the case for a clear-all with multiple models defined...
        #   Instead moving it to the options settings.
        #   
        #   elseif {$options_array(kill_model_windows) == -1} {
        #  switch [my_tk_dialog .multi_model_choice "Close deleted model windows?" "Should the Environment close inspector windows for a deleted model?" \
        #                    warning 0 "Yes" "No" "Always" "Never"] {
        #    0 { kill_all_model_windows $name}
        #    1 { #nothing
        #    }
        #    2 { kill_all_model_windows $name
        #        set options_array(kill_model_windows) 1
        #        # write that out to the end of hte userconfig file
        #        
        #        set file [file join $tcl_env_dir init "99-userconfig.tcl"]
        #
        #        append_data "set options_array(kill_model_windows) 1\n" $file
        #    }
        #    3 { set options_array(kill_model_windows) 0
        #        # write that out to the end of hte userconfig file
        #        
        #        set file [file join $tcl_env_dir init "99-userconfig.tcl"]
        #
        #        append_data "set options_array(kill_model_windows) 0\n" $file
        #    }
        #  }
        #}
      }
    }
    check {
      if {$top == "No Current Model" && [llength $name] != 0} {
        $menu delete "No Current Model"
      }
      if {$options_array(multiple_models) == 0 && [llength $name] > 1} {
        tk_messageBox -icon warning -message "You must enable multiple model support under Options before the ACT-R Environment tools will work correctly when more than one model is defined." \
                  -type ok -title "Multiple models not enabled"
      }      

      foreach model $name {
       $menu add radiobutton -label $model -variable global_current_model_name
       set global_current_model_name $model
      }
    }     
  }
}
    
global window_to_model
global model_to_windows

proc send_model_name {} {
  global options_array
  
  if {$options_array(multiple_models) == 1} {
    return [currently_selected_model]
  } else {
    return nil
  }
}

proc kill_all_model_windows {name} {

  global model_to_windows

  if {[array names model_to_windows -exact $name] != ""} {

    set windows $model_to_windows($name) 

    foreach x $windows {
      catch {
        destroy $x
      }
    }

    array unset model_to_windows $name
  }
}

bind model_window <Destroy> {
  global window_to_model
  global model_to_windows

  set model $window_to_model(%W)
  array unset window_to_model %W

  set old $model_to_windows($model) 
  set index [lsearch -exact $old $model]
  if {$index != -1} {
    set model_to_windows($model) [lreplace $old $index $index]
  }
}

proc record_new_window {win_name title} {
  
  global window_to_model
  global model_to_windows
  global options_array

  if {$options_array(multiple_models) == 1} {
    set model [currently_selected_model]
    set window_to_model($win_name) $model

    wm title $win_name "$title ($model)"

    if {[array names model_to_windows -exact $model] == ""} {
      set model_to_windows($model) [list $win_name]
    } else {
      set model_to_windows($model) [concat $model_to_windows($model) $win_name]
    }
  } else {
    wm title $win_name $title
  }
}

  
