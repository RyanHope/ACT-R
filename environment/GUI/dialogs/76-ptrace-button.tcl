
global ptrace_array

proc select_ptrace {} {

  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Production Selection History" -message "Tracing tools require a current model."
  } else {

    set win [toplevel [new_variable_name .ptrace]]

    global $win.scale
    global ptrace_array

    global $win.p_viewer
    set $win.p_viewer 0

    set ptrace_array($win,0) 0
    set ptrace_array($win,1) 0

    wm withdraw $win
    record_new_window $win $win

    wm geometry $win [get_configuration .ptrace $win]


    frame $win.frame -borderwidth 0  
    
    canvas $win.frame.canvas  \
         -xscrollcommand "$win.frame.scrlx set" \
         -yscrollcommand "$win.frame.scrly set" \
         -width 900 -height 300 -scrollregion {0 0 900 300} -bg white
   
    canvas $win.frame.canvas1  \
         -yscrollcommand "$win.frame.scrly set" \
         -width 150 -height 300 -scrollregion {0 0 150 300} -bg white
   
       
    scrollbar $win.frame.scrlx \
              -command "$win.frame.canvas xview" -orient horizontal

    scrollbar $win.frame.scrly \
              -command "scroll_ptraces_canvas $win" -orient vertical


    set $win.scale 1.0
          
    send_environment_cmd \
      "create list-handler $win.frame.canvas $win.return \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-p-history t)) nil) (reset) [send_model_name]"


    bind $win.frame.canvas <Destroy> "remove_handler $win.frame.canvas"


    button $win.update -command "draw_p_trace $win [send_model_name]" -text "Get history" -font button_font

    label $win.text -font text_font  -textvariable $win.textvar
  
    set $win.textvar ""

    label $win.notes -font text_font  -textvariable $win.notesvar -anchor nw
  
    set $win.notesvar ""

    button $win.grid -command "p_trace_grid $win" -text "Grid" -font button_font

    button $win.zoom_in -command "p_trace_zoom_in $win" -text "+" -font button_font

    button $win.zoom_out -command  "p_trace_zoom_out $win" -text "-" -font button_font

    button $win.save -command "save_phistory_trace $win" -text "Save 1P" -font button_font
    button $win.save2 -command "save_phistory_trace_multi $win" -text "Save Multi." -font button_font


    # Add a checkbox to allow removing the empty columns to the tool

    checkbutton $win.check \
                -text "Hide empty columns" \
                -font checkbox_font \
                -variable $win.check_val \
                -command "draw_p_trace $win [send_model_name]" \
                -onvalue 1 -offvalue 0


    global $win.scale
    set $win.scale 1.0

    global $win.grid_state
    set $win.grid_state 1

    global $win.c_height
    global $win.c_width

    global $win.label_offset

    pack $win.frame.scrlx -side bottom -fill x
    pack $win.frame.scrly -side right -fill y
    pack $win.frame.canvas1 -side left -fill y
    pack $win.frame.canvas -side left -fill both
 
    place $win.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -height -50
    

    place $win.update -x 0 -rely 1.0 -y -50 -width 85 -height 24
    place $win.grid -x 85 -rely 1.0 -y -50 -width 60 -height 24
    place $win.save -x 145 -rely 1.0 -y -50 -width 85 -height 24
    place $win.notes -x 240 -rely 1.0 -y -50 -relwidth 1.0 -height 25

    place $win.text -x 0 -rely 1.0 -y -25 -width 85 -height 25
    place $win.zoom_in -x 85 -rely 1.0 -y -25 -width 30 -height 24
    place $win.zoom_out -x 115 -rely 1.0 -y -25 -width 30 -height 24
    place $win.save2 -x 145 -rely 1.0 -y -25 -width 85 -height 24
    place $win.check -x 230 -rely 1.0 -y -25 -width 175 -height 24

    wm deiconify $win
  }
} 


proc scroll_ptraces_canvas {win args} {

   set ignore ""
   eval [append ignore $win.frame.canvas " " yview " " $args]
   set ignore ""
   eval [append ignore $win.frame.canvas1 " " yview " " $args]
}

proc draw_p_trace {win model} {

  global $win.return

  $win.frame.canvas delete all 
  $win.frame.canvas1 delete all

  upvar $win.textvar display
    
  upvar $win.check_val hide_empty
            
  set display "Busy"

  global $win.scale
  global $win.grid_state
  global $win.c_height
  global $win.c_width
  global $win.label_offset


  upvar $win.scale scale
  upvar $win.grid_state grid
  upvar $win.c_height g_c_height
  upvar $win.c_width g_c_width
  upvar $win.label_offset label_offset

  set c_height 0
  set c_width 0
  set t_height 0
  set t_width 0
  set n_width 0
  set x_display 0
   
  $win.update configure -state disabled
  $win.grid configure -state disabled
  $win.zoom_in configure -state disabled
  $win.zoom_out configure -state disabled
  $win.save configure -state disabled
  $win.save2 configure -state disabled
  $win.check configure -state disabled

  set done 0

  while {$done == 0} {
             
  set $win.return ""
                  
  if $hide_empty {
    send_environment_cmd "update [get_handler_name $win.frame.canvas] (lambda (x) (with-parameters (:draw-blank-columns nil) (production-history-chart-data x)))"
  } else {
    send_environment_cmd "update [get_handler_name $win.frame.canvas] production-history-chart-data"
  }

  wait_for_non_null $win.return

  upvar $win.return result

  foreach x $result {
    switch [lindex $x 0] {
      labels { 
        set y $c_height
        
        foreach p [lrange $x 1 end] {
          set box_name [new_variable_name box]
 
          $win.frame.canvas create text [expr $label_offset + 5] [expr $y + 5] -anchor nw -font text_font -text $p 
  
          $win.frame.canvas1 create text 5 $y -anchor nw -font text_font -text $p -tag $box_name
          $win.frame.canvas1 create line 0 $y $n_width $y -width 1 -f gray

          $win.frame.canvas1 bind $box_name <ButtonPress> "p_history_p_view $win $p $model"
          
          incr y $c_height
        }
      }

      colors {
          set colors [lrange $x 1 end]
      }

      reasons {
          set reasons [lrange $x 1 end]
      }
      
      size { 
       set t_height [lindex $x 1]
       set c_height [lindex $x 2]
       set n_width [lindex $x 3]
       set c_width [lindex $x 4]
       set t_width [lindex $x 5]

       set label_offset [expr -$n_width]

       $win.frame.canvas configure -width $t_width -height $t_height
       $win.frame.canvas1 configure -width $n_width -height $t_height
       $win.frame.canvas1 configure -scrollregion "0 0 $n_width $t_height"
       $win.frame.canvas configure -scrollregion "0 0 $t_width $t_height"
      }

      done {
       set done 1
      }

      column {
        
        $win.frame.canvas create text [expr $x_display + ($c_width / 2)] 0 -anchor n -font graphic_trace_font -text [lindex $x 1] -tag zoom

        set y $c_height

        foreach {index value uofn} [lrange $x 2 end] {

          set box_name [new_variable_name box]
 
          $win.frame.canvas create rectangle $x_display $y [expr $x_display + $c_width] [expr $y + $c_height] -width 0 -fill [lindex $colors $index] -tag [list $box_name zoom]
  
          switch $index {
              0 {
               $win.frame.canvas bind $box_name <Enter> "set $win.notesvar \"Utility: $value U(n): $uofn\""
              }
              1 {
                $win.frame.canvas bind $box_name <Enter> "set $win.notesvar \"Utility: $value U(n): $uofn\""
              }
              2 {
                $win.frame.canvas bind $box_name <Enter> "set $win.notesvar {Whynot: [lindex $reasons $value]}"
              }
          }

          $win.frame.canvas bind $box_name <Leave>  "set $win.notesvar \"\""
          incr y $c_height
        }
        incr x_display $c_width
      }
    }
  }
  }

  send_environment_cmd "update [get_handler_name $win.frame.canvas]  \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-p-history t)) nil)"


  for {set x 0} {$x < $t_width} {incr x $c_width} {
     $win.frame.canvas create line $x 0 $x $t_height -width 1 -f black -tag [list grid grid_vert zoom]
  }

  for {set y $c_height} {$y < $t_height} {incr y $c_height} {
     $win.frame.canvas create line 0 $y $t_width $y -width 1 -f black -tag [list grid zoom]
  }

  set scale 1.0
  set grid black
  set g_c_height $c_height
  set g_c_width $c_width
  
  $win.update configure -state normal
  $win.grid configure -state normal
  $win.zoom_in configure -state normal
  $win.zoom_out configure -state normal
  $win.save configure -state normal
  $win.save2 configure -state normal
  $win.check configure -state normal

  set display "Done"
}

proc p_trace_zoom_out {win} {

   global $win.scale
   upvar $win.scale scale

   set scale [expr .5 * $scale]
 
   $win.frame.canvas scale zoom 0 0 0.5 1.0
   $win.frame.canvas configure -width [expr .5 * [$win.frame.canvas cget -width]]
   $win.frame.canvas configure -scrollregion "0 0 [$win.frame.canvas cget -width] [$win.frame.canvas cget -height]"
  

#   foreach x [$win.frame.canvas find withtag trace_text] {
#      $win.frame.canvas itemconfigure $x -width [expr .5 * [$win.frame.canvas itemcget $x -width]]
#   }
}

proc p_trace_zoom_in {win} {

   global $win.scale
   upvar $win.scale scale

   if {$scale < 16} {
      set scale [expr 2 * $scale]

      $win.frame.canvas scale zoom 0 0 2.0 1.0
   $win.frame.canvas configure -width [expr 2.0 * [$win.frame.canvas cget -width]]
   $win.frame.canvas configure -scrollregion "0 0 [$win.frame.canvas cget -width] [$win.frame.canvas cget -height]"
      
#      foreach x [$win.frame.canvas find withtag trace_text] {
#         $win.frame.canvas itemconfigure $x -width [expr 2 * [$win.frame.canvas itemcget $x -width]]
#      }
   }
}


proc p_trace_grid {win} {

   global $win.grid_state
   upvar $win.grid_state grid

   if {$grid == ""} {
    $win.frame.canvas itemconfigure grid -f black
    set grid black
   } elseif {$grid == "vert"} {
    $win.frame.canvas itemconfigure grid -f ""
    set grid ""
   } else {
    $win.frame.canvas itemconfigure grid_vert -f ""
    set grid vert
   }

#   foreach x [$win.frame.canvas find withtag trace_text] {
#      $win.frame.canvas itemconfigure $x -width [expr .5 * [$win.frame.canvas itemcget $x -width]]
#   }
}



proc p_history_p_view {win prod model} {
 
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



proc save_phistory_trace {win} {
  set fname [tk_getSaveFile -title "Save production history as"\
                                  -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  global $win.label_offset
  upvar $win.label_offset label_offset

  if {$fname != ""} {
    $win.frame.canvas postscript -file $fname -width [expr -$label_offset + [$win.frame.canvas cget -width]] -height [$win.frame.canvas cget -height] -x $label_offset -y 0  -pageanchor nw -pagex 0.0 -pagey [$win.frame.canvas cget -height] -pagewidth [expr -$label_offset + [$win.frame.canvas cget -width]]
  }
}

proc save_phistory_trace_multi {win} {
  set fname [tk_getSaveFile -title "Save production history as" -filetypes {{"PostScript" "*.ps"}}]

  global $win.label_offset
  upvar $win.label_offset label_offset


  if {$fname != ""} {  
 
   set width 1400.0
   set height 400

   set xMax [expr -$label_offset + [$win.frame.canvas cget -width]]
   set NOP [expr ceil ($xMax / $width)]   
  
# The following code was modified from code written by Robert Heller
# in a file called bridge.tcl which was posted to comp.lang.tcl as
# an example of producing multi-page ps files.

   set prFile [open $fname w]

  puts $prFile "%!PS-Adobe-2.0"
  puts $prFile "%%Creator: ACT-R Environment Copyright 2011 Dan Bothell"
  puts $prFile "%%Title: Production History Chart"
  puts -nonewline $prFile "%%CreationDate: "
  global tcl_version
  if {$tcl_version >= 7.6} {
    puts $prFile "[clock format [clock seconds]]"
  } else {
    puts $prFile "[exec date]"
  }
  puts $prFile "%%Pages: $NOP $xMax $width [expr ceil($xMax / $width)]"
  puts $prFile "%%EndComments"
  puts $prFile "/EncapDict 200 dict def EncapDict begin"
  puts $prFile "/showpage {} def /erasepage {} def /copypage {} def end"
  puts $prFile "/BeginInclude {0 setgray 0 setlinecap 1 setlinewidth"
  puts $prFile "0 setlinejoin 10 setmiterlimit \[\] 0 setdash"
  puts $prFile "/languagelevel where {"
  puts $prFile "  pop"
  puts $prFile "  languagelevel 2 ge {"
  puts $prFile "    false setoverprint"
  puts $prFile "    false setstrokeadjust"
  puts $prFile "  } if"
  puts $prFile "} if"
  puts $prFile "newpath"
  puts $prFile "save EncapDict begin} def"
  puts $prFile "/EndInclude {restore end} def"
  puts $prFile "%%EndProlog"
  set pageNo 1


  for {set xoff 0} {$xoff < $xMax} {set xoff [expr $xoff + $width]} {

      puts $prFile "%%Page: $pageNo $pageNo"
      puts $prFile "BeginInclude"

      # this one works set eps "[$win.frame.canvas postscript -height $height -width $width -x [expr $xoff - 150] -y 0 -pageanchor nw -pagex 0.25i -pagey 7.5i -pagewidth 8.0i]"

      set eps "[$win.frame.canvas postscript -height $height -width $width -x [expr $xoff + $label_offset] -y 0 -pageanchor nw -pagex 2.0i -pagey 0.5i -pagewidth 10.0i -rotate 1]"

 
      set EOC [string first "%%BeginProlog\n" "$eps"]
      set EOF [expr [string first "%%EOF\n" "$eps"] - 1]

      puts $prFile "[phistory_StripPSComments [string range $eps $EOC $EOF]]"
      puts $prFile "EndInclude showpage"
      incr pageNo
  }
  puts $prFile "%%EOF"
  close $prFile
  }
}

proc phistory_StripPSComments {PSString} {
  set result {}
  foreach l [split "$PSString" "\n"] {
    set i [string first "%" "$l$"]
    if {$i == 0} {
      set result "$result\n"
    } elseif {$i > 0 && [regexp {(^.*[^\\])(%.*$)} "$l" whole prefix comment]} {
      set result "$result$prefix\n"
    } else {
      set result "$result$l\n"
    }
  }
  return "$result"
}



button [control_panel_name].ptrace_button \
       -command {select_ptrace} -text "Production History" -font button_font

pack [control_panel_name].ptrace_button

