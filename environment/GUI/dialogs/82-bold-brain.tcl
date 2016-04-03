image create photo brain -file [file join $tcl_env_dir dialogs ref-brain.gif]


global brain_data 

proc make_bold_brains {} {
  
  if {[currently_selected_model] == "nil"} {

    tk_messageBox -icon info -type ok -title "BOLD Brain" -message "BOLD tools require a current model."
  } else {

    set win ".bold_slices_[currently_selected_model]"

  if {[winfo exists $win] == 1} {
    wm deiconify $win
    focus -force $win
  } else {


  toplevel $win
  
  # hide the window for speed and aesthetic reasons
  
  wm withdraw $win

  record_new_window $win "BOLD Brain"

  wm geometry $win [get_configuration .bold_slices $win]
  
  # here's the frame for the brain display

  set frame [frame $win.frame -borderwidth 0]  
 
  
  canvas $win.frame.canvas  \
         -xscrollcommand "$win.frame.scrlx set" \
         -yscrollcommand "$win.frame.scrly set" \
         -width 740 -height 640 -scrollregion {0 0 740 640}
          
    scrollbar $win.frame.scrlx \
              -command "$win.frame.canvas xview" -orient horizontal

    scrollbar $win.frame.scrly \
              -command "$win.frame.canvas yview" -orient vertical


    $win.frame.canvas create image 100 0 -image brain -anchor nw

$win.frame.canvas create rectangle 262 68 272 78  -width 0 -fill "" -outline "" -tag {box manual}
$win.frame.canvas create rectangle 390 68 400 78  -width 0 -fill "" -outline "" -tag {box manual} 
$win.frame.canvas create rectangle 518 68 528 78  -width 0 -fill ""  -outline "" -tag {box manual}
$win.frame.canvas create rectangle 646 68 656 78  -width 0 -fill ""  -outline "" -tag {box manual}
$win.frame.canvas create rectangle 316 68 326 78  -width 0 -fill ""  -outline "" -tag {box manual}
$win.frame.canvas create rectangle 444 68 454 78  -width 0 -fill ""  -outline "" -tag {box manual}
$win.frame.canvas create rectangle 572 68 582 78  -width 0 -fill ""  -outline "" -tag {box manual}
$win.frame.canvas create rectangle 700 68 710 78  -width 0 -fill ""  -outline "" -tag {box manual}
$win.frame.canvas create rectangle 390 602 400 612  -width 0 -fill ""  -outline "" -tag {box visual}
$win.frame.canvas create rectangle 518 602 528 612  -width 0 -fill ""  -outline "" -tag {box visual}
$win.frame.canvas create rectangle 646 602 656 612 -width 0 -fill ""  -outline "" -tag {box visual}
$win.frame.canvas create rectangle 444 602 454 612 -width 0 -fill ""  -outline "" -tag {box visual}
$win.frame.canvas create rectangle 572 602 582 612 -width 0 -fill ""  -outline "" -tag {box visual}
$win.frame.canvas create rectangle 700 602 710 612 -width 0 -fill ""  -outline "" -tag {box visual}
$win.frame.canvas create rectangle 542 46 548 56 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 670 46 676 56 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 158 174 164 184 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 286 174 292 184 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 550 46 556 56 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 678 46 684 56 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 166 174 172 184 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 294 174 300 184 -width 0 -fill ""  -outline "" -tag {box goal}
$win.frame.canvas create rectangle 260 190 270 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 388 190 398 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 516 190 526 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 644 190 654 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 316 190 326 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 444 190 454 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 572 190 582 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 700 190 710 200 -width 0 -fill ""  -outline "" -tag {box vocal}
$win.frame.canvas create rectangle 274 224 284 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 402 224 412 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 530 224 540 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 658 224 668 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 304 224 314 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 432 224 442 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 560 224 570 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 688 224 698 234 -width 0 -fill ""  -outline "" -tag {box imaginal}
$win.frame.canvas create rectangle 388 164 398 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 516 164 526 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 644 164 654 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 132 292 142 302 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 444 164 454 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 572 164 582 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 700 164 710 174 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 188 292 198 302 -width 0 -fill ""  -outline "" -tag {box retrieval}
$win.frame.canvas create rectangle 536 300 544 308 -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 664 300 672 308 -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 152 428 160 436 -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 280 428 288 436 -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 554 300 562 308 -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 682 300 690 308 -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 170 428 178 436 -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 298 428 306 436   -width 0 -fill ""  -outline "" -tag {box procedural}
$win.frame.canvas create rectangle 642 322 652 332   -width 0 -fill ""  -outline "" -tag {box aural}
$win.frame.canvas create rectangle 130 450 140 460   -width 0 -fill ""  -outline "" -tag {box aural}
$win.frame.canvas create rectangle 258 450 268 460   -width 0 -fill ""  -outline "" -tag {box aural}
$win.frame.canvas create rectangle 386 450 396 460   -width 0 -fill ""  -outline "" -tag {box aural}
$win.frame.canvas create rectangle 702 322 712 332   -width 0 -fill ""  -outline "" -tag {box aural}
$win.frame.canvas create rectangle 190 450 200 460   -width 0 -fill ""  -outline "" -tag {box aural}
$win.frame.canvas create rectangle 318 450 328 460   -width 0 -fill ""  -outline "" -tag {box aural}
$win.frame.canvas create rectangle 446 450 456 460   -width 0 -fill ""  -outline "" -tag {box aural}


  global brain_borders

  set brain_borders 0

  checkbutton $win.show -text "Show box borders" -variable brain_borders  -font checkbox_font -command "toggle_borders $win"

 global brain_data

 set brain_data ""

  send_environment_cmd \
      "create list-handler $win.frame brain_data \
         (lambda (x) (declare (ignore x)) (list \"XXX\" \"XXX\")) () [send_model_name]"

  
  wait_for_non_null brain_data

  pack $win.frame.scrlx -side bottom -fill x
  pack $win.frame.scrly -side right -fill y
  place $win.frame.canvas -relx 0 -rely 0 -relwidth 1.0 -relheight 1.0

  place $win.show -x 0 -y 0 -height 25 
  place $win.frame -x 0 -y 25 -height -100 -relheight 1.0 -relwidth 1.0


  wm deiconify $win
  focus $win

  set brain_data ""

  send_environment_cmd "update [get_handler_name $win.frame] (lambda (x) (declare (ignore x)) (bold-brain-data-results (cons '[get_handler_name $win.frame] '$win.frame) t))"

  wait_for_non_null brain_data

  

  set save_data $brain_data

# for now just kill it immediately, but may want to keep it around later

  set brain_data ""

  send_environment_cmd "update [get_handler_name $win.frame] (lambda (x) (declare (ignore x)) (uncache-bold-data (cons '[get_handler_name $win.frame] '$win.frame)))"

  wait_for_non_null brain_data

  remove_handler $win.frame

  set brain_data $save_data


    if {[lindex [lindex $brain_data 0] 1] != {} } {
       $win.frame.canvas create text  10  120 -text "manual" -fill "#f00" -anchor w -font checkbox_font 
       $win.frame.canvas itemconfigure manual -outline "#f00"
    }
    if {[lindex [lindex $brain_data 1] 1] != {} } {
       $win.frame.canvas create text  10  140 -text "goal" -fill "#090" -anchor w  -font checkbox_font
       $win.frame.canvas itemconfigure goal -outline "#090"
    }

    if {[lindex [lindex $brain_data 2] 1] != {} } {
       $win.frame.canvas create text  10  160 -text "vocal" -fill "#00f" -anchor w -font checkbox_font 
       $win.frame.canvas itemconfigure vocal -outline "#00f"
    }
    if {[lindex [lindex $brain_data 3] 1] != {} } {
       $win.frame.canvas create text  10  180 -text "imaginal" -fill "#aa0" -anchor w -font checkbox_font
       $win.frame.canvas itemconfigure imaginal -outline "#aa0"
    }
    if {[lindex [lindex $brain_data 4] 1] != {} } {
       $win.frame.canvas create text  10  200 -text "retrieval" -fill "#0aa" -anchor w -font checkbox_font
       $win.frame.canvas itemconfigure retrieval -outline "#0aa"
    }

    if {[lindex [lindex $brain_data 5] 1] != {} } {
       $win.frame.canvas create text  10  400 -text "procedural" -fill "#a0a" -anchor w -font checkbox_font
       $win.frame.canvas itemconfigure procedural -outline "#a0a"
    }
    if {[lindex [lindex $brain_data 6] 1] != {} } {
       $win.frame.canvas create text  10  420 -text "aural" -fill "#0e0" -anchor w -font checkbox_font 
       $win.frame.canvas itemconfigure aural -outline "#0e0"
    }
    if {[lindex [lindex $brain_data 7] 1] != {} } {
       $win.frame.canvas create text  10  440 -text "visual" -fill "#00a" -anchor w -font checkbox_font 
       $win.frame.canvas itemconfigure visual -outline "#00a"
    }


  scale $win.s -orient horizontal -command "report_scale_brain $win.frame.canvas" -from 0 -to [expr [llength [lindex $brain_data 0]]-1] -label "Scan #"  -font checkbox_font -tickinterval [expr [llength [lindex $brain_data 0]]-1]
  
  $win.s set -1

  place $win.s -relwidth 1.0 -rely 1.0 -y -75
}
}     
}


proc toggle_borders {win} {
  global brain_borders

  if {$brain_borders == 1} {
   $win.frame.canvas itemconfigure box -width 1
  } else {
   $win.frame.canvas itemconfigure box -width 0
  }
}

proc report_scale_brain {w s} {

  global brain_data

   $w itemconfigure manual -fill "[lindex [lindex $brain_data 0] $s]"
   $w itemconfigure goal -fill "[lindex [lindex $brain_data 1] $s]"
   $w itemconfigure vocal -fill "[lindex [lindex $brain_data 2] $s]"
   $w itemconfigure imaginal -fill "[lindex [lindex $brain_data 3] $s]"
   $w itemconfigure retrieval -fill "[lindex [lindex $brain_data 4] $s]"
  $w itemconfigure procedural -fill "[lindex [lindex $brain_data 5] $s]"
  $w itemconfigure aural -fill "[lindex [lindex $brain_data 6] $s]"
  $w itemconfigure visual -fill "[lindex [lindex $brain_data 7] $s]"

}


button [control_panel_name].bold_brains -command {make_bold_brains} \
       -text "2D brain" -font button_font

# put that button on the control panel

pack [control_panel_name].bold_brains
