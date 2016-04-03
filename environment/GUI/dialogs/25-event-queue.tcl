proc select_event_queue {} {

  if {[winfo exists .event_queue] == 1} {
    wm deiconify .event_queue
    raise .event_queue
  } else {
    # make it now
    toplevel .event_queue

    wm withdraw .event_queue

    wm geometry .event_queue [get_configuration .event_queue]

    set f [frame .event_queue.frame -borderwidth 0]  
    
    set t [text $f.text -font text_font -yscrollcommand "$f.scrl set" -state disabled]
          
    set s [scrollbar $f.scrl -command "$t yview"]

    send_environment_cmd \
      "create text-output-handler $t $t \
          (lambda (x) (declare (ignore x)) (mp-show-queue t)(model-output \"\")(mp-show-waiting)) (reset post) [send_model_name]"

    bind $t <Destroy> {
      remove_handler %W
    }

    # Make the window useable for copy operations on Windows
   
    bind $t <1> {focus %W}
  
    pack $s -side right -fill y 
    pack $t -side left -expand 1 -fill both
  
    place $f -x 0 -y 0 -relwidth 1.0 -relheight 1.0 

    # now show the window 

    wm deiconify .event_queue
  }
}


button [control_panel_name].event_queue_button \
       -command {select_event_queue} -text "Event Queue" -font button_font

pack [control_panel_name].event_queue_button

