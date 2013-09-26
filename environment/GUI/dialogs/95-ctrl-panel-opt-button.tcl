button [control_panel_name].options -text "Options" -font button_font -command {
  select_options
}                    

global current_options

pack [control_panel_name].options

proc select_options {} {
  global options_array
  global current_options

  array set current_options [array get options_array]

  if {[winfo exists .options] == 1} {
    wm deiconify .options
    raise .options
  } else {
    # make it now

    toplevel .options
    wm withdraw .options
    wm title .options "Options"

    wm geometry .options [get_configuration .options]

    frame .options.but_frame

    button .options.but_frame.save -text "Save" -font button_font -command {
      save_options
      destroy .options
    }

    button .options.but_frame.apply -text "Apply" -font button_font -command {
      apply_options
    }

    button .options.but_frame.cancel -text "Cancel" -font button_font -command {
      destroy .options
    }

    button .options.but_frame.revert -text "Revert" -font button_font -command {
      array set current_options [array get options_array]
    }

    pack .options.but_frame.save -side left
    pack .options.but_frame.apply -side left
    pack .options.but_frame.cancel -side left
    pack .options.but_frame.revert -side left

    checkbutton .options.use_env_window \
                -text "Use an environment window for the Experiment display" \
                -font checkbox_font -variable current_options(use_env_window) \
                -onvalue t -offvalue nil

    checkbutton .options.use_smart_load \
                -text "Compile definitions when model opened or reloaded" \
                -font checkbox_font -variable current_options(use_smart_load) \
                -onvalue t -offvalue nil

    checkbutton .options.save_before_reload \
                -text "Automatically save an open model when reload pressed" \
                -font checkbox_font \
                -variable current_options(save_before_reload) \
                -onvalue 1 -offvalue 0

    checkbutton .options.show_copyrights \
                -text "Show the ACT-R copyrights screen" \
                -font checkbox_font \
                -variable current_options(show_copyrights) \
                -onvalue 1 -offvalue 0

    checkbutton .options.save_backups \
                -text "Save a backup every time" \
                -font checkbox_font \
                -variable current_options(save_backups) \
                -onvalue 1 -offvalue 0


    checkbutton .options.multiple_models \
                -text "Allow the environment to work with multiple models" \
                -font checkbox_font \
                -variable current_options(multiple_models) \
                -onvalue 1 -offvalue 0

    checkbutton .options.multiple_models_close \
                -text "Close inspector windows for deleted models when multiple models are defined" \
                -font checkbox_font \
                -variable current_options(kill_model_windows) \
                -onvalue 1 -offvalue 0


    pack .options.use_env_window -anchor w -expand 1 -fill x
    pack .options.use_smart_load -anchor w -expand 1 -fill x
    pack .options.save_before_reload -anchor w -expand 1 -fill x
    pack .options.show_copyrights -anchor w -expand 1 -fill x
    pack .options.save_backups -anchor w -expand 1 -fill x
    pack .options.multiple_models -anchor w -expand 1 -fill x
    pack .options.multiple_models_close -anchor w -expand 1 -fill x

    pack .options.but_frame

    # now show the window 

    wm deiconify .options
  }
}

proc apply_options {} {
  global options_array
  global current_options


  # If multi-models is enabled warn that a restart is needed

  if {$options_array(multiple_models) != $current_options(multiple_models)} {
    tk_messageBox -icon warning -message "You must save the settings and then reconnect ACT-R to the environment (by calling stop-environment and then start-environment) to change multiple model support." \
                  -type ok -title "Changing multiple models support"
  }


  array set options_array [array get current_options]
  
  # take care of any special updating necessary

  catch {
    send_environment_cmd \
      "update [get_handler_name .env_window] \
         (lambda (x) \
            (declare (ignore x)) \
            (setf (environment-control-use-env-windows *environment-control*) $options_array(use_env_window) ) \
            (list 'ignore))"
  }
}

proc save_options {} {
  global options_array
  global tcl_env_dir

  apply_options

  set file [file join $tcl_env_dir init "99-userconfig.tcl"]

  write_data "# this file generated from saving the options\n" $file
 
  foreach name [array names options_array] {
    append_data "set options_array($name) $options_array($name)\n" $file
  }
}

