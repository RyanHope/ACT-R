global wait_for_create
global stop_type
global stepper_tutor
global stepper_waiter
global stepper_temp
global instan_view
global instan_parsed
global tutor_temp
global tutor_ans
global tutor_bindings
global initial_wait

proc stepper_button_state_control {state} {
 .stepper.step configure -state $state
 .stepper.stop configure -state $state
 .stepper.run_until configure -state $state
} 


proc select_stepper {} {
  global wait_for_create
  global stop_type
  global instan_parsed
  global tutor_temp
  global initial_wait

  global stepper_tutor

  global stepper_temp

  global next_stepper_event
  global .stepper.current_text.var

  if {[winfo exists .stepper] == 1} {
    wm deiconify .stepper
    raise .stepper
  } else {
    # make it now if not running

    set stepper_tutor 0

    set instan_parsed 0

    set stepper_temp ""

    send_environment_cmd \
      "create simple-handler .stepper stepper_temp \
         (lambda (x) (declare (ignore x)) (if (or (mp-running?) (environment-busy-p) (environment-control-stepper-open *environment-control*)) 1 (if (environment-control-pre-hook *environment-control*) 0 (if (mp-models) 2 3)))) ()"

    wait_for_non_null stepper_temp

    if {$stepper_temp == 1} {
      set stepper_temp ""

    tk_messageBox -icon info -type ok -title "Stepper" \
                  -message "You cannot open the stepper while the model is running or there is already a stepper open."

      remove_handler .stepper
    } elseif {$stepper_temp == 2} {

    tk_messageBox -icon info -type ok -title "Stepper" \
                  -message "One or more models were loaded before the environment was connected.  You must reset or reload to synchronize them."

      remove_handler .stepper
    } elseif {$stepper_temp == 3} {

    tk_messageBox -icon info -type ok -title "Stepper" \
                  -message "There is no model defined thus the stepper cannot be used."

      remove_handler .stepper
    } else {

      remove_handler .stepper


    toplevel .stepper
    wm withdraw .stepper
    wm title .stepper "Stepper"

    wm geometry .stepper [get_configuration .stepper]


    tk_optionMenu .stepper.run_until_type stop_type Time Production Module

    .stepper.run_until_type configure -font button_font

    [.stepper.run_until_type cget -menu] configure -font menu_font

    button .stepper.run_until -text "Run Until:" -font button_font \
           -command {
       global stepper_tutor

      if $stepper_tutor {
       tk_messageBox -icon info -type ok -title "Tutoring" \
                    -message "Only the Step button can be used when in tutor mode."
      } else {
        send_environment_cmd "update [get_handler_name .stepper.run_until] \
         (lambda (x) \
          (declare (ignore x)) \
          (set-stepper-skip-time '($stop_type $run_until_time)) \
          (stepper-step-button nil))"
      }
    }                    

    send_environment_cmd \
      "create simple-handler .stepper.run_until ignore_returns \
       (lambda (x) (declare (ignore x))) ()"

    bind .stepper.run_until <Destroy> {
      remove_handler .stepper.run_until
    }

    entry .stepper.run_until_time -width 6 -font text_font \
      -textvariable run_until_time 


    label .stepper.current -text "Next Event:" -justify left -font label_font

     set stepper_temp ""

     send_environment_cmd \
        "create simple-handler .stepper.current stepper_temp \
           (lambda (x) (declare (ignore x)) \
              (init-stepper)) (pre)"

     wait_for_non_null stepper_temp

     send_environment_cmd \
        "update [get_handler_name .stepper.current] \
           stepper-step"


    bind .stepper.current <Destroy> {
      global stepper_temp
 
      set stepper_temp ""

      send_environment_cmd \
        "update [get_handler_name .stepper.current] \
           (lambda (x) (declare (ignore x)) \
              (remove-stepper))"

      wait_for_non_null stepper_temp

      remove_handler .stepper.current
     }


   button .stepper.step -text "Step" -font button_font -command {stepper_step_button}
   button .stepper.stop -text "Stop" -font button_font -command {stepper_stop_button}

    send_environment_cmd \
      "create simple-handler .stepper.step ignore_returns \
         stepper-step-button ()"

    bind .stepper.step <Destroy> {
      remove_handler .stepper.step
    }
    
    label .stepper.current_text -font text_font \
         -textvar .stepper.current_text.var -justify left -anchor nw

    set .stepper.current_text.var ""

    send_environment_cmd \
      "create simple-handler .stepper.current_text .stepper.current_text.var \
          next-event-display nil"

    bind .stepper.current_text <Destroy> {
      remove_handler .stepper.current_text
    }

    frame .stepper.prod_frame -borderwidth 0

    frame .stepper.prod_frame.f4 -borderwidth 0  
  
    label .stepper.prod_frame.f4.list_title -textvar .stepper.prod_frame.f4.list_title.var \
           -anchor nw -justify left -font label_font

    set .stepper.prod_frame.f4.list_title.var ""

    send_environment_cmd \
      "create simple-handler .stepper.prod_frame.f4.list_title .stepper.prod_frame.f4.list_title.var \
          stepper-list-name nil"
    
    bind .stepper.prod_frame.f4.list_title <Destroy> {
      remove_handler .stepper.prod_frame.f4.list_title
    }

    frame .stepper.prod_frame.f4.f -borderwidth 0  
  

    listbox .stepper.prod_frame.f4.f.list -listvar .stepper.prod_frame.f4.f.list.var \
            -yscrollcommand ".stepper.prod_frame.f4.f.scrl set" \
            -selectmode single -exportselection 0 -font list_font -bd 0
     
    send_environment_cmd \
      "create select-first-list-box-handler .stepper.prod_frame.f4.f.list \
        .stepper.prod_frame.f4.f.list stepper-list-values nil"

    bind .stepper.prod_frame.f4.f.list <Destroy> {
      remove_handler .stepper.prod_frame.f4.f.list
    }
   
    bind .stepper.prod_frame.f4.f.list <<ListboxSelect>> {
 
       update_instantiation_viewers %W
    }

    scrollbar .stepper.prod_frame.f4.f.scrl \
              -command ".stepper.prod_frame.f4.f.list yview"



   frame .stepper.prod_frame.f3 -borderwidth 0
   frame .stepper.prod_frame.f3.f -borderwidth 0

    label .stepper.prod_frame.f3.production \
          -textvariable .stepper.prod_frame.f3.production.var  \
          -justify left -font label_font

     send_environment_cmd \
      "create simple-handler .stepper.prod_frame.f3.production \
              .stepper.prod_frame.f3.production.var \
              stepper-prod_frame-name nil"

     bind .stepper.prod_frame.f3.production <Destroy> {
      remove_handler .stepper.prod_frame.f3.production
     }

    text .stepper.prod_frame.f3.f.text -font text_font \
         -yscrollcommand ".stepper.prod_frame.f3.f.scrl set" \
         -state disabled 

    send_environment_cmd \
      "create special-simple-output-handler .stepper.prod_frame.f3.f.text \
         instan_view (lambda (x) (declare (ignore x))) nil"

    send_environment_cmd \
      "create simple-handler .tutor tutor_temp (lambda (x) (declare (ignore x))) nil"

    bind .stepper.prod_frame.f3.f.text <Destroy> {
      remove_handler .stepper.prod_frame.f3.f.text
      remove_handler .tutor
    }

    # Make the window useable for copy operations on Windows
  
    bind .stepper.prod_frame.f3.f.text <1> {focus %W}


    scrollbar .stepper.prod_frame.f3.f.scrl \
              -command ".stepper.prod_frame.f3.f.text yview"


    frame .stepper.prod_frame.f2 -borderwidth 0
    frame .stepper.prod_frame.f2.f -borderwidth 0

    label .stepper.prod_frame.f2.bindings \
          -textvariable .stepper.prod_frame.f2.bindings.var \
          -justify left -font label_font

     send_environment_cmd \
      "create simple-handler .stepper.prod_frame.f2.bindings \
              .stepper.prod_frame.f2.bindings.var \
              stepper-bindings-name nil"

     bind .stepper.prod_frame.f2.bindings <Destroy> {
      remove_handler .stepper.prod_frame.f2.bindings
     }

    text .stepper.prod_frame.f2.f.text -font text_font \
         -yscrollcommand ".stepper.prod_frame.f2.f.scrl set" \
         -state disabled

    send_environment_cmd \
      "create text-output-handler .stepper.prod_frame.f2.f.text \
        .stepper.prod_frame.f2.f.text (lambda (x) (declare (ignore x))) nil"

    bind .stepper.prod_frame.f2.f.text <Destroy> {
      remove_handler .stepper.prod_frame.f2.f.text
    }

    # Make the window useable for copy operations on Windows
  
    bind .stepper.prod_frame.f2.f.text <1> {focus %W}

    scrollbar .stepper.prod_frame.f2.f.scrl \
              -command ".stepper.prod_frame.f2.f.text yview"


   checkbutton .stepper.stepper_tutor -text "Tutor Mode" -font checkbox_font \
                -variable stepper_tutor -command {select_tutor_mode} 

    set instan_parsed 0

    send_environment_cmd \
      "create simple-handler .stepper.stepper_tutor stepper_waiter \
         (lambda (x) (declare (ignore x)) (setf (stepper-control-mode (environment-control-stepper *environment-control*)) :production)) nil"

     bind .stepper.stepper_tutor <Destroy> {
      remove_handler .stepper.stepper_tutor
     }


    .stepper.stepper_tutor deselect


   frame .stepper.prod_frame.f5 -borderwidth 0
   frame .stepper.prod_frame.f5.f -borderwidth 0

    label .stepper.prod_frame.f5.production \
          -textvariable .stepper.prod_frame.f5.production.var  \
          -justify left -font label_font

     send_environment_cmd \
      "create simple-handler .stepper.prod_frame.f5.production \
              .stepper.prod_frame.f5.production.var \
              stepper-parameter-frame-name nil"

     bind .stepper.prod_frame.f5.production <Destroy> {
      remove_handler .stepper.prod_frame.f5.production
     }

    text .stepper.prod_frame.f5.f.text -font text_font \
         -yscrollcommand ".stepper.prod_frame.f5.f.scrl set" \
         -state disabled 


    send_environment_cmd \
      "create text-output-handler .stepper.prod_frame.f5.f.text \
        .stepper.prod_frame.f5.f.text (lambda (x) (declare (ignore x))) nil"

    bind .stepper.prod_frame.f5.f.text <Destroy> {
      remove_handler .stepper.prod_frame.f5.f.text
    }

    # Make the window useable for copy operations on Windows
  
    bind .stepper.prod_frame.f5.f.text <1> {focus %W}


    scrollbar .stepper.prod_frame.f5.f.scrl \
              -command ".stepper.prod_frame.f5.f.text yview"



    send_environment_cmd \
      "create simple-funcall-handler .stepper \"stepper_button_state_control\" \
         (lambda (x) (if (eq x :start) 'normal 'disabled)) (run-start run-end)"


    bind .stepper <Destroy> {remove_handler .stepper}

    place .stepper.step -x 2 -y 2 -width 50 -height 25
    place .stepper.stop -x 55 -y 2 -width 50 -height 25
 
    place .stepper.run_until -x 108 -y 2 -width 80 -height 25
    place .stepper.run_until_type -x 190 -y 2 -width 100 -height 25
    place .stepper.run_until_time -x 292 -y 2 -width -394 -relwidth 1.0 -height 25
    place .stepper.stepper_tutor -x -100 -relx 1.0 -y 5 -height 25 -width 100


    place .stepper.current -x 0 -y 30 -height 25 -width 70 
 
    place .stepper.current_text -x 75 -y 32 -relwidth 1.0 -width -75 -height 23 

    place .stepper.prod_frame -x 0 -y 60 -relwidth 1.0 -relheight 1.0 \
                              -height -60


    place .stepper.prod_frame.f5 -relx 0.0 -rely .5 \
                                 -relwidth .4 -relheight .5


    pack .stepper.prod_frame.f5.production -side top 

    pack .stepper.prod_frame.f5.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f5.f.text -side left -expand 1 -fill both

    pack .stepper.prod_frame.f5.f -side top -expand 1 -fill both



    place .stepper.prod_frame.f4 -relx 0.0 -rely 0.0 \
                                 -relwidth .4 -relheight .5


    pack .stepper.prod_frame.f4.list_title -side top 

    pack .stepper.prod_frame.f4.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f4.f.list -side left -expand 1 -fill both

    pack .stepper.prod_frame.f4.f -side top -expand 1 -fill both


    place .stepper.prod_frame.f3 -relx 0.4 -rely 0.4 \
                                 -relwidth .6 -relheight .6


    pack .stepper.prod_frame.f3.production -side top 

    pack .stepper.prod_frame.f3.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f3.f.text -side left -expand 1 -fill both

    pack .stepper.prod_frame.f3.f -side top -expand 1 -fill both


    place .stepper.prod_frame.f2 -relx 0.4 -rely 0.0 \
                                 -relwidth .6 -relheight .4


    pack .stepper.prod_frame.f2.bindings -side top 

    pack .stepper.prod_frame.f2.f.scrl -side right -fill y 
    pack .stepper.prod_frame.f2.f.text -side left -expand 1 -fill both

    pack .stepper.prod_frame.f2.f -side top -expand 1 -fill both

    stepper_button_state_control disabled
    wm deiconify .stepper
    
  }
}




proc select_tutor_mode {} {
  global stepper_waiter 
  global tutor_bindings 
  global instan_parsed 
  global stepper_tutor
  
  set stepper_waiter "" 
            
  send_environment_cmd \
    "update [get_handler_name .stepper.stepper_tutor] \
            (lambda (x) (declare (ignore x)) \
                    (let ((stepper (environment-control-stepper *environment-control*))) \
                      (if (= $stepper_tutor 1) \
                       (progn \
                          (setf (stepper-control-mode stepper) :tutor) \
                          (setf (stepper-control-tutor-bindings stepper) nil) \
                          (setf (stepper-control-tutor-responses stepper) nil)) \
                       (setf (stepper-control-mode stepper) :production))))"

  set instan_parsed 0
  if [array exists tutor_bindings] {
    array unset tutor_bindings 
  }  

  wait_for_non_null stepper_waiter
  update_instantiation_viewers .stepper.prod_frame.f4.f.list
}

proc stepper_step_button {} {
  global stepper_tutor
  global step_return
  global tutor_bindings
  global instan_parsed

  if {!$stepper_tutor || [array names tutor_bindings] == ""} { 
    send_environment_cmd "update [get_handler_name .stepper.step] stepper-step-button"
    set instan_parsed 0
  } else {
    tk_messageBox -icon info -type ok -title "Tutoring" \
            -message "You must complete the instantiation before continuing in tutor mode."
  }
}    


proc stepper_stop_button {} {
  global stepper_tutor
  global step_return
  global tutor_bindings
  global instan_parsed

  if {!$stepper_tutor || [array names tutor_bindings] == ""} { 
    send_environment_cmd "update [get_handler_name .stepper.step] stepper-stop-button"
    set instan_parsed 0
    destroy .stepper
  } else {
    tk_messageBox -icon info -type ok -title "Tutoring" \
            -message "You must complete the instantiation before stopping in tutor mode."
  }
}  

proc update_instantiation_viewers {list} {
  global instan_view
  global stepper_tutor
  global instan_parsed
  global stepper_temp
  global tutor_temp
  global tutor_bindings
  global stepper_waiter

  if {[$list curselection] != ""} {
    set prod [$list get [$list curselection]]

      send_environment_cmd \
      "update [get_handler_name .stepper.prod_frame.f2.f.text ] \
        (lambda (x) (declare (ignore x)) (stepper-instan-binding '$prod ))"

      send_environment_cmd \
      "update [get_handler_name .stepper.prod_frame.f5.f.text ] \
        (lambda (x) (declare (ignore x)) (stepper-parameter-frame-values '$prod ))"



    if {!$stepper_tutor || !$instan_parsed} {
 
      set instan_view ""

      send_environment_cmd \
        "update [get_handler_name .stepper.prod_frame.f3.f.text] \
          (lambda (x) (declare (ignore x)) (stepper-instan-production '$prod))"

      wait_for_non_null instan_view

      .stepper.prod_frame.f3.f.text configure -state normal
      .stepper.prod_frame.f3.f.text delete 1.0 end

      if {$instan_view != "EMPTY_ENV_STRING"} {
        .stepper.prod_frame.f3.f.text insert 1.0 $instan_view

        if $stepper_tutor {
  
        set stepper_waiter ""

          send_environment_cmd "update [get_handler_name .stepper.stepper_tutor] \
            (lambda (x) (declare (ignore x)) (tutored-step))"

          wait_for_non_null stepper_waiter

          if {$stepper_waiter != "nil" && !$instan_parsed} {
       
            set instan_parsed 1
    
            set strt 1.0
            set side lhs
            set buf ""

            while {[set indx [.stepper.prod_frame.f3.f.text search "=" $strt end]] != ""} { 

              set word_end [.stepper.prod_frame.f3.f.text index "$indx + 1 chars wordend"]
              set var [.stepper.prod_frame.f3.f.text get $indx $word_end]

              if {$var == "=="} {
                set side rhs
              } else {
                if {[string compare -nocase $var "=goal"] == 0} {
                  set buf "=goal"
                } elseif {[string compare -nocase $var "=retrieval"] == 0} {
                  set buf "=retrieval"
                }
            
                set t_name [new_variable_name tag]

                .stepper.prod_frame.f3.f.text tag add $t_name $indx $word_end

                .stepper.prod_frame.f3.f.text tag configure $t_name -background black -foreground white 
          
                set tutor_bindings($t_name) [list $indx $word_end $var $side $buf]

                .stepper.prod_frame.f3.f.text tag bind $t_name <1> {
                  global tutor_bindings
                  global tutor_ans

                  set t_name [%W tag names @%x,%y]
                  
                  if {[llength $t_name] == 1} {
                    set strt [lindex $tutor_bindings($t_name) 0]
                    set w_end [lindex $tutor_bindings($t_name) 1]
                    set var [lindex $tutor_bindings($t_name) 2]
                    set side [lindex $tutor_bindings($t_name) 3]
                    set buf [lindex $tutor_bindings($t_name) 4]
  
                    if {[get_tutor_response $var $side $buf $strt $w_end $t_name]} {
               
                      wait_for_non_null tutor_ans
                
                      if {$tutor_ans != "NoAnswer"} {

                        array unset tutor_bindings [lindex $tutor_ans 3]
                   
                        .stepper.prod_frame.f3.f.text configure -state normal
                        .stepper.prod_frame.f3.f.text delete [lindex $tutor_ans 1] [lindex $tutor_ans 2]
                        .stepper.prod_frame.f3.f.text tag delete [lindex $tutor_ans 3]
                        .stepper.prod_frame.f3.f.text insert [lindex $tutor_ans 1] [lindex $tutor_ans 0]
                        .stepper.prod_frame.f3.f.text configure -state disabled
                      }
                    }
                  } else {
                   tk_messageBox -icon warning -title "Invalid Selection" \
                    -message "There was additional text selected when clicking on a variable.  Please try again." -type ok
        

                  }
                }
              }
            
              set strt $word_end
            }
          }
        }
      } 



      .stepper.prod_frame.f3.f.text configure -state disabled
    }

  } else {
    .stepper.prod_frame.f2.f.text configure -state normal
    .stepper.prod_frame.f2.f.text delete 1.0 end  
    .stepper.prod_frame.f2.f.text configure -state disabled
    .stepper.prod_frame.f3.f.text configure -state normal
    .stepper.prod_frame.f3.f.text delete 1.0 end
    .stepper.prod_frame.f3.f.text configure -state disabled
    .stepper.prod_frame.f5.f.text configure -state normal
    .stepper.prod_frame.f5.f.text delete 1.0 end  
    .stepper.prod_frame.f5.f.text configure -state disabled

  }
}
}


button [control_panel_name].step_button \
       -command {select_stepper} -text "Stepper" -font button_font

pack [control_panel_name].step_button


proc get_tutor_response {word side buf start end name} {
  if {[winfo exists .tutor_response] == 1} {
    tk_messageBox -icon info -type ok -title "Tutoring" \
                  -message "You haven't completed the previous binding yet."
    wm deiconify .tutor_response
    raise .tutor_response

    return 0
  } else {
    # make it now

    global tutor_help
    global tutor_entry
    global tutor_ans

    toplevel .tutor_response
    wm withdraw .tutor_response
    wm title .tutor_response "Tutor Response"

    wm geometry .tutor_response [get_configuration .tutor_response]


    label .tutor_response.label \
          -text "What is the binding for $word?" \
          -justify left -font label_font


    entry .tutor_response.entry \
          -width 50 -textvariable tutor_entry -font text_font

    set tutor_entry ""

    set tutor_ans ""

    bind .tutor_response.entry <Key-Return> "accept_tutor_response $word $start $end $name"
    
    label .tutor_response.help \
          -textvariable tutor_help \
          -justify left -font label_font \
          -height 2
    
    set tutor_help ""

    button .tutor_response.help_button -text "Help" -font button_font -command "tutor_help $word"
    button .tutor_response.hint_button -text "Hint" -font button_font -command "tutor_hint $word $side $buf"

    bind .tutor_response.entry <Destroy> {
      if {$tutor_ans == ""} {
        set tutor_ans "NoAnswer"
      }
    }    

    pack .tutor_response.label -anchor w
    pack .tutor_response.entry -anchor w
    pack .tutor_response.help -anchor w
    pack .tutor_response.hint_button -side left
    pack .tutor_response.help_button -side left

    wm deiconify .tutor_response

    after idle {focus .tutor_response.entry}

    return 1
  }
}

proc accept_tutor_response {word start end name} {
  global tutor_entry
  global stepper_temp    
  global tutor_help
  global tutor_ans
  global stepper_waiter

  if {$tutor_entry != ""} {

    set stepper_waiter ""

    send_environment_cmd "update [get_handler_name .stepper.stepper_tutor] (lambda (x) (declare (ignore x)) \
                         (tutor-check '$word '$tutor_entry))"

    wait_for_non_null stepper_waiter

    if $stepper_waiter {
      set tutor_ans [list $tutor_entry $start $end $name]
      destroy .tutor_response
      update_instantiation_viewers .stepper.prod_frame.f4.f.list
        
    } else {
      set tutor_help "Incorrect.\n$tutor_entry is not the binding for $word in this instantiation."
      set tutor_entry ""
    }
  }
}

proc tutor_hint {word side buf} {
  global tutor_help
  global stepper_waiter

  set stepper_waiter ""

  send_environment_cmd "update [get_handler_name .stepper.stepper_tutor] (lambda (x) (declare (ignore x)) (tutor-completed '$word))"

  wait_for_non_null stepper_waiter

  if $stepper_waiter {
    set tutor_help "Look in the bindings section of the stepper window\nto see the current binding for $word."
  } elseif {$side == "rhs"} {
    set tutor_help "You should find the binding for $word on\nthe left hand side of the production first."
  } elseif {[string compare -nocase $word "=goal"] == 0 || [string compare -nocase $word "=retrieval"] == 0} {
    set tutor_help "Use the buffer viewer to determine the chunk in the [string range $word 1 end] buffer."
  } elseif {$side == "lhs" && $buf != ""} { 
    set tutor_help "$word is in a slot of the [string range $buf 1 end] buffer.\nYou can find its value using the buffer viewer."
  } else {
    set tutor_help "No hint is available for this variable. (contact Dan because this is an error)"
  }
}

proc tutor_help {word} {
  global tutor_help
  global stepper_waiter

  set stepper_waiter ""

  send_environment_cmd "update [get_handler_name .stepper.stepper_tutor] (lambda (x) (declare (ignore x)) (tutor-answer '$word))"

  wait_for_non_null stepper_waiter
  set tutor_help "The binding of $word is $stepper_waiter"
}
