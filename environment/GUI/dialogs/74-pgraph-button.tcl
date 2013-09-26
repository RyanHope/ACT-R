

proc select_pgraph {} {

  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Production Graph" -message "Tracing tools require a current model."
  } else {

    set win [toplevel [new_variable_name .pgraph]]

    global $win.scale

    global $win.p_viewer
    set $win.p_viewer 0

    wm withdraw $win
    record_new_window $win $win

    wm geometry $win [get_configuration .pgraph $win]


    frame $win.frame -borderwidth 0  
    
    canvas $win.frame.canvas  \
         -xscrollcommand "$win.frame.scrlx set" \
         -yscrollcommand "$win.frame.scrly set" \
         -width 900 -height 300 -scrollregion {0 0 900 300} -bg white
   
       
    scrollbar $win.frame.scrlx \
              -command "$win.frame.canvas xview" -orient horizontal

    scrollbar $win.frame.scrly \
              -command "$win.frame.canvas yview" -orient vertical

    global $win.return

    send_environment_cmd \
      "create list-handler $win.frame.canvas $win.return \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-p-history t)) nil) (reset) [send_model_name]"

    bind $win.frame.canvas <Destroy> "clear_p_history_data $win
                                      remove_handler $win.frame.canvas"

    global $win.which
    global $win.count
    global $win.max

    button $win.update -command "set $win.type \"All Transitions\"
                                 set $win.which :all
                                 set $win.count 0
                                 $win.next configure -state disabled
                                 $win.previous configure -state disabled
                                 draw_p_graph $win [send_model_name] t" -text "All Transitions" -font button_font

    label $win.text -font text_font  -textvariable $win.textvar
  
    set $win.textvar ""

    label $win.notes -font text_font  -textvariable $win.notesvar -anchor nw
  
    set $win.notesvar ""

    button $win.grid -command "set $win.type Frequencies
                               set $win.which :freq
                               set $win.count 0
                               $win.next configure -state normal
                               $win.previous configure -state normal
                               draw_p_graph $win [send_model_name] t" -text "Frequencies" -font button_font

    button $win.grid1 -command "set $win.type Cycles
                                set $win.which :cycle
                               set $win.count 0
                               $win.next configure -state normal
                               $win.previous configure -state normal
                               draw_p_graph $win [send_model_name] t" -text "Cycles" -font button_font

    button $win.grid2 -command "set $win.type \"Unique Cycles\"
                                set $win.which :color
                                set $win.count 0
                                $win.next configure -state normal
                                $win.previous configure -state normal
                                draw_p_graph $win [send_model_name] t" -text "Unique Cycles" -font button_font

    button $win.grid3 -command "set $win.type Runs
                                set $win.which :run
                                set $win.count 0
                                $win.next configure -state normal
                                $win.previous configure -state normal
                                draw_p_graph $win [send_model_name] t" -text "Runs" -font button_font

    button $win.grid4 -command "set $win.type \"Unique Runs\"
                                set $win.which :run-color
                                set $win.count 0
                                $win.next configure -state normal
                                $win.previous configure -state normal
                                draw_p_graph $win [send_model_name] t" -text "Unique Runs" -font button_font

    button $win.grid5 -command "set $win.type Utilities
                                set $win.which :utility
                                set $win.count 0
                                $win.next configure -state normal
                                $win.previous configure -state normal
                                draw_p_graph $win [send_model_name] t" -text "Utilities" -font button_font


    label $win.cur_d -font text_font  -textvariable $win.current_display -anchor n
    label $win.of -font text_font  -text "of" -anchor n
    label $win.max_d -font text_font  -textvariable $win.max_display -anchor n
  
    button $win.next -command "increment_p_graph_count $win
                               draw_p_graph $win [send_model_name] nil" -text "+" -font button_font -state disable 

    button $win.previous -command "decrement_p_graph_count $win
                                   draw_p_graph $win [send_model_name] nil" -text "-" -font button_font -state disable
 

    label $win.type_box -font text_font  -textvariable $win.type -anchor nw

    button $win.save -command "save_phistory_graph $win" -text "Save as .eps" -font button_font
    button $win.save2 -command "save_phistory_graph_dot $win" -text "Save as .dot" -font button_font

    checkbutton $win.check \
                -text "Hide unused productions" \
                -font checkbox_font \
                -variable $win.check_val \
                -onvalue nil -offvalue t



    global $win.scale
    set $win.scale 1.0

#    global $win.grid_state
#    set $win.grid_state 1


    global $win.labels
    global $win.boxes
    global $win.links
    global $win.graph_name 

    set $win.graph_name "\"[currently_selected_model]\""

#    global $win.c_width

#    global $win.label_offset

    pack $win.frame.scrlx -side bottom -fill x
    pack $win.frame.scrly -side right -fill y
    pack $win.frame.canvas -side left -fill both
 
    place $win.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -height -75
    
    place $win.notes -x 500 -rely 1.0 -y -25 -relwidth 1.0 -width -500 -height 24
    place $win.text -x 0 -rely 1.0 -y -25 -width 100 -height 24

    place $win.update -x 0 -rely 1.0 -y -50 -width 100 -height 24
    place $win.grid -x 100 -rely 1.0 -y -50 -width 100 -height 24
    place $win.grid1 -x 200 -rely 1.0 -y -50 -width 100 -height 24
    place $win.grid2 -x 300 -rely 1.0 -y -50 -width 100 -height 24
    place $win.grid3 -x 400 -rely 1.0 -y -50 -width 100 -height 24
    place $win.grid4 -x 500 -rely 1.0 -y -50 -width 100 -height 24
    place $win.grid5 -x 600 -rely 1.0 -y -50 -width 100 -height 24

    place $win.type_box -relx 0 -relwidth .5 -width -135 -rely 1.0 -y -75 -height 24
    place $win.of -relx .5 -x -15 -width 30 -rely 1.0 -y -75 -height 24
    place $win.cur_d -relx .5 -x -95 -width 80 -rely 1.0 -y -75 -height 24
    place $win.max_d -relx .5 -x 15 -width 80 -rely 1.0 -y -75 -height 24
    place $win.next -relx .5 -x 95 -width 40 -rely 1.0 -y -75 -height 24
    place $win.previous -relx .5 -x -135 -width 40 -rely 1.0 -y -75 -height 24

    place $win.save -x 100 -rely 1.0 -y -25 -width 100 -height 24
    place $win.save2 -x 200 -rely 1.0 -y -25 -width 100 -height 24
    place $win.check -x 300 -rely 1.0 -y -25 -width 175 -height 24

    wm deiconify $win
  }
} 

proc clear_p_history_data {win} {
 
  send_environment_cmd "update [get_handler_name $win.frame.canvas] (lambda (x) (declare (ignore x)) (remove-p-history-entry (cons '[get_handler_name $win.frame.canvas] '$win)) (list t))"
}
                                     

proc increment_p_graph_count {win} {
  global $win.count
  upvar $win.count count
  global $win.max
  upvar $win.max max

  if {$count != $max} {
    incr count 1
  }
}

proc decrement_p_graph_count {win} {
  global $win.count
  upvar $win.count count
  if {$count != 0} {
    incr count -1
  }
}


proc draw_p_graph {win model clear_all} {

  if {[$win.next cget -state] == "normal"} {
    set enable_next 1
  } else {
    set enable_next 0
  }

  $win.update configure -state disabled
  $win.grid configure -state disabled
  $win.grid1 configure -state disabled
  $win.grid2 configure -state disabled
  $win.grid3 configure -state disabled
  $win.grid4 configure -state disabled
  $win.grid5 configure -state disabled
  $win.next configure -state disabled
  $win.previous configure -state disabled
  $win.save configure -state disabled
  $win.save2 configure -state disabled

  global $win.return
  
  if {$clear_all == "nil"} {
    $win.frame.canvas delete transient
  } else {
    $win.frame.canvas delete all 
  }

  upvar $win.textvar display
  upvar $win.notesvar notes
  upvar $win.current_display current_display
  upvar $win.max_display max_display

              
  upvar $win.which kind
  upvar $win.count count
  upvar $win.max max

  upvar $win.check_val show_unused


  set display "Busy"

  global $win.scale
#  global $win.grid_state
#  global $win.c_height
#  global $win.c_width
#  global $win.label_offset


  upvar $win.scale scale
#  upvar $win.grid_state grid
#  upvar $win.c_height g_c_height
#  upvar $win.c_width g_c_width
#  upvar $win.label_offset label_offset

  set c_height 0
  set c_width 0
  set t_height 0
  set t_width 0
  set n_width 0
  set x_display 0
   

  global $win.labels
  global $win.boxes
  global $win.links

  upvar $win.labels labels
  upvar $win.boxes boxes
  upvar $win.links links

  set links [list]
  set boxes [list]
  if {$clear_all == "t"} {
    set labels [list]
  }

  set done 0

  set min_time ""
  set max_time ""
  set max 0

  while {$done == 0} {
             
  set $win.return ""
                  
  send_environment_cmd "update [get_handler_name $win.frame.canvas] (lambda (x) (declare (ignore x)) (create-production-graph-coords (cons '[get_handler_name $win.frame.canvas] '$win) $kind $count $clear_all $show_unused))"

  wait_for_non_null $win.return

  upvar $win.return result

  foreach x $result {
    switch [lindex $x 0] {
      label { 
        
          set p [lindex $x 1]

          if {[lindex $x 8] == "gray"} {
            lappend labels "\"$p\" \[ color = gray \]"
          } else {
            lappend labels "\"$p\""
          }

          $win.frame.canvas create text [lindex $x 2] [lindex $x 3] -anchor center -font text_font -text $p -tag $p
          $win.frame.canvas bind $p <ButtonPress> "p_graph_p_view $win $p $model"
          $win.frame.canvas create rectangle [lindex $x 4] [lindex $x 5] [lindex $x 6] [lindex $x 7] -outline [lindex $x 8] -width 2
        
      }

      box { 
        
         lappend boxes "\"[lindex $x 1]\" \[ color = [lindex $x 6] \]"

         $win.frame.canvas create rectangle [lindex $x 2] [lindex $x 3] [lindex $x 4] [lindex $x 5] -outline [lindex $x 6] -width 2 -tag transient
        
      }


      link {
         if {[lindex $x 3] == "gray"} {
           lappend links "\"[lindex $x 1]\" -> \"[lindex $x 2]\" \[ style=dashed color=gray \]"
           $win.frame.canvas create line [lrange $x 5 end] -fill [lindex $x 3] -arrow last -smooth 1 -width [lindex $x 4] -dash ".-" -tag transient
         } else {
           lappend links "\"[lindex $x 1]\" -> \"[lindex $x 2]\""
           $win.frame.canvas create line [lrange $x 5 end] -fill [lindex $x 3] -arrow last -smooth 1 -width [lindex $x 4] -tag transient
         }

      }  
      min_time {
        set min_time [lindex $x 1]
      }
      max_time {
        set max_time [lindex $x 1]
      }
      cycles {
        set max [lindex $x 1]
      }
      size { 
       
       $win.frame.canvas configure -width [lindex $x 1] -height [lindex $x 2] -scrollregion "0 0 [lindex $x 1] [lindex $x 2]"
      }

      done {
       set done 1
      }
    }
  }
  }

  send_environment_cmd "update [get_handler_name $win.frame.canvas]  \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-p-history t)) nil)"

  
  $win.update configure -state normal
  $win.grid configure -state normal
  $win.grid1 configure -state normal
  $win.grid2 configure -state normal
  $win.grid3 configure -state normal
  $win.grid4 configure -state normal
  $win.grid5 configure -state normal

  if $enable_next {
    $win.next configure -state normal
    $win.previous configure -state normal
  }

   $win.save configure -state normal
   $win.save2 configure -state normal

   set display "Done"
 
   if {$min_time != "" && $max_time != ""} {
     set notes "$min_time - $max_time"
   } else {
     set notes ""
   }

   set current_display [expr $count + 1]
   set max_display [expr $max + 1]

}



proc p_graph_p_view {win prod model} {
 
  global $win.p_viewer

  upvar $win.p_viewer viewer

  if {$viewer == 0 || [winfo exists $viewer] != 1} {

    if {$model == "nil" || [set_currently_selected_model $model] != 0} {
      set win [make_procedural_viewer]

      set box "$win.list_frame.list_box"

      global  $win.list_frame.list_box.var
      wait_for_non_null $win.list_frame.list_box.var

      set index [lsearch -exact [$box get 0 end] $prod] 

      $box selection set $index

      event generate $box <<ListboxSelect>>

      set viewer $win
    } else {
      tk_messageBox -icon warning -title "No Model" \
                    -message "Model $model is not currently defined so procedural viewer unavailable" -type ok
    }

  } else {
    
    wm deiconify $viewer
    raise $viewer

    set box "$viewer.list_frame.list_box"

    set index [lsearch -exact [$box get 0 end] $prod] 

    $box selection clear 0 end

    event generate $box <<ListboxSelect>>

    $box selection set $index

    event generate $box <<ListboxSelect>>
  }
}



proc save_phistory_graph {win} {
  set fname [tk_getSaveFile -title "Save production graph as"\
                                  -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  if {$fname != ""} {
    $win.frame.canvas postscript -file $fname -width [$win.frame.canvas cget -width] -height [$win.frame.canvas cget -height] -x 0 -y 0  -pageanchor nw -pagex 0.0 -pagey [$win.frame.canvas cget -height] -pagewidth [$win.frame.canvas cget -width]
  }
}


proc save_phistory_graph_dot {win} {
  set fname [tk_getSaveFile -title "Save production graph as"\
                                  -filetypes {{"DOT file" "*.dot"}}]

  if {$fname != ""} {
  global $win.labels
  global $win.boxes
  global $win.links
  global $win.graph_name

  upvar $win.labels labels
  upvar $win.boxes boxes
  upvar $win.links links
  upvar $win.graph_name name
 
  write_data "digraph $name \{\n" $fname

  foreach label $labels {
    append_data "  $label ;\n" $fname
  }

  foreach box $boxes {
    append_data "  $box ;\n" $fname
  }

  foreach link $links {
    append_data "  $link ;\n" $fname
  }

  append_data "\}" $fname

}}

button [control_panel_name].pgraph_button \
       -command {select_pgraph} -text "Production Graph" -font button_font

pack [control_panel_name].pgraph_button
