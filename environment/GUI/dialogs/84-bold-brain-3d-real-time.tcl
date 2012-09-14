 # The 3d code in this file was found on the Tcl/Tk Wiki (wiki.tcl.tk).
 # It's been stripped down and modified specifically for 
 # use with the brain viewer, but the original file name
 # and author are:
 #
 # 3dviewer.tcl
 #
 # Author       : Mark B. Stucky

global gvar

 proc bb_Init {config name title init} {

   if {[winfo exists $name] == 1} {
      wm deiconify $name
      focus -force $name
    } else {


      set win [toplevel $name]
 


 ## Create a dummy handler to set the :save-buffer-trace parameter to t
 ## whenever a bold-browser window is open.

    send_environment_cmd \
      "create simple-handler $win $win.dummy \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-buffer-trace t)) nil) (reset) [send_model_name]"


    bind $win <Destroy> "remove_handler $win"

      wm geometry $win [get_configuration $config $win]

        record_new_window $win $title

     global gvar


     set gvar($win,Xscreen) 750
     set gvar($win,Yscreen) 500

     set gvar($win,rho)   5.
     set gvar($win,theta) 0.0
     set gvar($win,phi)   0.0
     set gvar($win,dist)  1200.0

     set gvar($win,x,translate)   0.0
     set gvar($win,y,translate)   0.0

     set gvar($win,look_at) [list 0.0 0.0 0.0]

     set pi    [expr {4.0 * atan(1.0)}]
     set mpi   [expr {-1.0 * $pi}]

     set gvar($win,pi)     $pi
     set gvar($win,mpi)    $mpi
     set gvar($win,twopi)  [expr {2.0 * $pi}]

     
     set gvar($win,theta_light) 0.0
     set gvar($win,phi_light)   0.0

     set gvar($win,colors) [list none none none none none none none none]

     canvas $win.c -width 650 -height 500 -bg black -bd 0
     place $win.c -x 0 -y 0

     set gvar($win,canv) $win.c

     bind $win.c <ButtonPress-1>   "canvas_rotate_start $win  %W %x %y"
     bind $win.c <B1-Motion>       "canvas_rotate_drag $win   %W %x %y"
     bind $win.c <ButtonRelease-1> "canvas_rotate_end $win    %W %x %y"

     bind $win.c <ButtonPress-2>   "canvas_pan_start $win  %W %x %y"
     bind $win.c <B2-Motion>       "canvas_pan_drag $win %W %x %y"
     bind $win.c <ButtonRelease-2> "canvas_pan_end $win     %W %x %y"

     bind $win.c <ButtonPress-3>   "canvas_zoom_start $win  %W %x %y"
     bind $win.c <B3-Motion>       "canvas_zoom_drag $win   %W %x %y"
     bind $win.c <ButtonRelease-3> "canvas_zoom_end $win    %W %x %y"

     eval $init $win

   }
}

proc create_brain_real_time_view {win} {

   send_environment_cmd \
     "create simple-funcall-handler $win.c \"update_brain_view $win\" start-brain-scan (reset) [send_model_name]"
  
     
    bind $win.c <Destroy> "kill_brain_view $win"
    
}
 
proc kill_brain_view {win} {
     global gvar

     set gvar($win,colors) ""

     send_environment_cmd "update [get_handler_name $win.c] remove-brain-scan"

     wait_for_non_null gvar($win,colors)
     
     remove_handler $win.c
}

proc update_brain_view {win colors} {
 
  global gvar
 
  if {$colors == "close"} {
    set gvar($win,colors) 1
  } else {
    set gvar($win,colors) $colors
    bb_redraw $win
  }
}

 proc bb_min { val1 val2 } {
     if { $val1 <= $val2 } {
         return $val1
     } else {
         return $val2
     }
 }

 proc bb_max { val1 val2 } {
     if { $val1 >= $val2 } {
         return $val1
     } else {
         return $val2
     }
 }


 #******************************* MatrixVectorProduct *****************

 proc MatrixVectorProduct {M V} {
     set x [lindex $V 0]
     set y [lindex $V 1]
     set z [lindex $V 2]
     set w [lindex $V 3]
     return [list \
        [expr {[lindex [lindex $M 0] 0]*$x+[lindex [lindex $M 1] 0]*$y+[lindex [lindex $M 2] 0]*$z+[lindex [lindex $M 3] 0]*$w}] \
        [expr {[lindex [lindex $M 0] 1]*$x+[lindex [lindex $M 1] 1]*$y+[lindex [lindex $M 2] 1]*$z+[lindex [lindex $M 3] 1]*$w}] \
        [expr {[lindex [lindex $M 0] 2]*$x+[lindex [lindex $M 1] 2]*$y+[lindex [lindex $M 2] 2]*$z+[lindex [lindex $M 3] 2]*$w}] \
        [expr {[lindex [lindex $M 0] 3]*$x+[lindex [lindex $M 1] 3]*$y+[lindex [lindex $M 2] 3]*$z+[lindex [lindex $M 3] 3]*$w}] ]
 }

 #******************************* bb_d_move ********************************

 proc bb_d_move {win x y z w } {
 
global gvar

     set tmp [MatrixVectorProduct $gvar($win,Curnt_Trans) [list $x $y $z $w]]

     foreach {xt yt zt wt} $tmp {break}

     if {$zt == 0.0} {
         set zt 0.000001
     }

     set x1 [expr {($gvar($win,Xscreen)/2.) + round($gvar($win,dist)*($xt/$zt)) }]
     set y1 [expr {($gvar($win,Yscreen)/2.) - round($gvar($win,dist)*($yt/$zt)) }]

     set x1 [expr {$x1 + $gvar($win,x,translate) }]
     set y1 [expr {$y1 - $gvar($win,y,translate) }]

     set gvar($win,Curnt_X_position) $x1
     set gvar($win,Curnt_Y_position) $y1

     return [list $x1 $y1]
 }

 #******************************* bb_d_draw ********************************

 proc bb_d_draw {win x y z w {color white} {arrow none} } {

global gvar

     set tmp [MatrixVectorProduct $gvar($win,Curnt_Trans) [list $x $y $z $w]]

     foreach {xt yt zt} $tmp {break}

     if {$zt == 0.0} {
         set zt 0.000001
     }

     set x2 [expr {($gvar($win,Xscreen)/2.) + round($gvar($win,dist)*($xt/$zt)) }]
     set y2 [expr {($gvar($win,Yscreen)/2.) - round($gvar($win,dist)*($yt/$zt)) }]

     set x2 [expr {$x2 + $gvar($win,x,translate) }]
     set y2 [expr {$y2 - $gvar($win,y,translate) }]

     $gvar($win,canv) create line [list $gvar($win,Curnt_X_position) $gvar($win,Curnt_Y_position) $x2 $y2] \
         -fill $color -arrow $arrow -tag brain

     set gvar($win,Curnt_X_position) $x2
     set gvar($win,Curnt_Y_position) $y2

     return [list $x2 $y2]
 }

 #******************************* bb_draw_shaded ********************************

 proc bb_draw_shaded {win clist {fillcolor blue} {edgecolor white} } {

global gvar
     set poly {}
     foreach {x y z} $clist {
         set w 1.0
         set tmp [MatrixVectorProduct $gvar($win,Curnt_Trans) [list $x $y $z $w]]
         foreach {xt yt zt} $tmp {break}
         if {$zt == 0.0} {
             puts "zt = 0.0"
             set zt 0.001
         }
         set x2 [expr {($gvar($win,Xscreen)/2.) + round($gvar($win,dist)*($xt/$zt)) }]
         set y2 [expr {($gvar($win,Yscreen)/2.) - round($gvar($win,dist)*($yt/$zt)) }]

         set x2 [expr {$x2 + $gvar($win,x,translate) }]
         set y2 [expr {$y2 - $gvar($win,y,translate) }]

         lappend poly $x2 $y2
     }

     if {$edgecolor == "none"}  {
         $gvar($win,canv) create polygon $poly -fill $fillcolor -tag brain
     } else {
         $gvar($win,canv) create polygon $poly -fill $fillcolor -outline $edgecolor -tag brain -width 3
     }
 }

 #*********************** Set_Viewing_Transform ********************************
 #

 proc Set_Viewing_Transform {win rho theta phi dist } {
global gvar
     set sinth [expr {sin($theta)}]
     set costh [expr {cos($theta)}]
     set sinph [expr {sin($phi)}]
     set cosph [expr {cos($phi)}]

     
     set l1 [list [expr {-$sinth}] [expr {-$costh * $cosph}] [expr {-$costh * $sinph}] 0.0]
     set l2 [list [expr {$costh}] [expr {-$sinth * $cosph}] [expr {-$sinth * $sinph}] 0.0]
     set l3 [list 0.0 $sinph [expr {-$cosph}] 0.0]
     set l4 [list 0.0 0.0 $rho 1.0]

     set Ct [list $l1 $l2 $l3 $l4]

     set gvar($win,Curnt_Trans) $Ct

     set eye_x [expr {$rho * $sinph * $costh}]
     set eye_y [expr {$rho * $sinph * $sinth}]
     set eye_z [expr {$rho * $cosph}]

     set gvar($win,eye_point) [list $eye_x $eye_y $eye_z]
 }


 proc Conc_Trans { a b c } {

   for {set i 0} {$i < 4} {incr i} {
       set xa [lindex $a $i 0]
       set ya [lindex $a $i 1]
       set za [lindex $a $i 2]
       set wa [lindex $a $i 3]
       for {set j 0} {$j < 4} {incr j} {
           set xb [lindex $b 0 $j]
           set yb [lindex $b 1 $j]
           set zb [lindex $b 2 $j]
           set wb [lindex $b 3 $j]
           lset c $i $j [expr {$xa*$xb + $ya*$yb + $za*$zb + $wa*$wb}]
       }
   }
   return $c
 }

 #********************************* Get_Vector **********************

 proc Get_Vector { v1 v2 } {

     set x1  [lindex $v1 0]
     set y1  [lindex $v1 1]
     set z1  [lindex $v1 2]

     set x2  [lindex $v2 0]
     set y2  [lindex $v2 1]
     set z2  [lindex $v2 2]

     set ux  [expr {$x2  - $x1 }]
     set uy  [expr {$y2  - $y1 }]
     set uz  [expr {$z2  - $z1 }]

     return [list $ux $uy $uz]
 }

 #********************************* CrossProduct ****************

 proc Vector_CrossProduct {v1 v2} {
     set x1  [lindex $v1 0]
     set y1  [lindex $v1 1]
     set z1  [lindex $v1 2]

     set x2  [lindex $v2 0]
     set y2  [lindex $v2 1]
     set z2  [lindex $v2 2]

     set normal [list [expr {$y1*$z2 - $y2*$z1}] \
                      [expr {$z1*$x2 - $z2*$x1}] \
                      [expr {$x1*$y2 - $x2*$y1}]]
     return $normal
 }

 #********************************* CrossProduct ****************

 proc CrossProduct {x1 y1 z1 x2 y2 z2} {
     set normal [list [expr {$y1*$z2 - $y2*$z1}] \
                      [expr {$z1*$x2 - $z2*$x1}] \
                      [expr {$x1*$y2 - $x2*$y1}]]
     return $normal
 }

 #********************************* DotProduct ****************

 proc Vector_DotProduct {v1 v2} {
     set x1  [lindex $v1 0]
     set y1  [lindex $v1 1]
     set z1  [lindex $v1 2]

     set x2  [lindex $v2 0]
     set y2  [lindex $v2 1]
     set z2  [lindex $v2 2]

     set mag [expr {$x1*$x2 + $y1*$y2 + $z1*$z2}]
     return  $mag
 }

 proc DotProduct {x1 y1 z1 x2 y2 z2} {
     set mag [expr {$x1*$x2 + $y1*$y2 + $z1*$z2}]
     return  $mag
 }

 proc Normal_Vector { v1 v2 } {

     set n [Vector_CrossProduct $v1 $v2]

     set mag [Vector_DotProduct $n $n]
     set mag [expr {sqrt($mag)}]

     foreach {x y z} $n {break}

     set xn [expr {$x / $mag }]
     set yn [expr {$y / $mag }]
     set zn [expr {$z / $mag }]

     set norm [list $xn $yn $zn]

     return $norm

 }

 #***************************** R E A D D A T A ******************************

 proc bb_ReadData {win} {

global gvar
     set lvtx {{ 0 0 0.5120} { 0.2969 0.0000 0.4800} { 0.3047 0.2187 0.4800} { 0.1562 0.4844 0.4800}
               { -.1484 0.4609 0.4800} { -.2578 0.1875 0.4800} { -.2500 0.0000 0.4800} { -.1797 -.1250 0.4800}
               { -.0703 -.2187 0.4800} { 0.0703 -.2266 0.4800} { 0.2109 -.1484 0.4800} { 0.3203 0.0000 0.4480}
               { 0.3281 0.2422 0.4480} { 0.1797 0.5547 0.4480} { -.1797 0.5547 0.4480} { -.3281 0.2422 0.4480}
               { -.3203 0.0000 0.4480} { -.2266 -.1641 0.4480} { -.0859 -.2578 0.4480} { 0.0859 -.2578 0.4480}
               { 0.2266 -.1641 0.4480} { 0.3750 0.0000 0.4160} { 0.3906 0.2812 0.4160} { 0.1953 0.6016 0.4160}
               { -.1875 0.5859 0.4160} { -.3594 0.2656 0.4160} { -.3437 0.0000 0.4160} { -.2734 -.1953 0.4160}
               { -.1250 -.3906 0.4160} { 0.1328 -.3984 0.4160} { 0.2891 -.2109 0.4160} { 0.4453 0.0000 0.3840}
               { 0.4453 0.3203 0.3840} { 0.2031 0.6250 0.3840} { -.1953 0.6016 0.3840} { -.4141 0.2969 0.3840}
               { -.4062 0.0000 0.3840} { -.3125 -.2266 0.3840} { -.1484 -.4453 0.3840} { 0.1562 -.4766 0.3840}
               { 0.3437 -.2500 0.3840} { 0.4844 0.0000 0.3520} { 0.4922 0.3594 0.3520} { 0.2109 0.6406 0.3520}
               { -.2031 0.6328 0.3520} { -.4531 0.3281 0.3520} { -.4375 0.0000 0.3520} { -.3594 -.2656 0.3520}
               { -.1641 -.5000 0.3520} { 0.1719 -.5234 0.3520} { 0.3906 -.2812 0.3520} { 0.5312 0.0000 0.3200}
               { 0.5391 0.3906 0.3200} { 0.2187 0.6641 0.3200} { -.2109 0.6562 0.3200} { -.5078 0.3672 0.3200}
               { -.4844 0.0000 0.3200} { -.3984 -.2891 0.3200} { -.1797 -.5469 0.3200} { 0.1875 -.5781 0.3200}
               { 0.4297 -.3125 0.3200} { 0.5625 0.0000 0.2880} { 0.5547 0.4062 0.2880} { 0.2266 0.6875 0.2880}
               { -.2187 0.6641 0.2880} { -.5156 0.3750 0.2880} { -.5156 0.0000 0.2880} { -.4297 -.3125 0.2880}
               { -.1953 -.5937 0.2880} { 0.2031 -.6172 0.2880} { 0.4609 -.3359 0.2880} { 0.5781 0.0000 0.2560}
               { 0.5859 0.4297 0.2560} { 0.2266 0.7031 0.2560} { -.2266 0.6953 0.2560} { -.5625 0.4062 0.2560}
               { -.5469 0.0000 0.2560} { -.4453 -.3281 0.2560} { -.2031 -.6172 0.2560} { 0.2031 -.6250 0.2560}
               { 0.4687 -.3359 0.2560} { 0.6094 0.0000 0.2240} { 0.5859 0.4297 0.2240} { 0.2344 0.7109 0.2240}
               { -.2266 0.6875 0.2240} { -.5547 0.4062 0.2240} { -.5703 0.0000 0.2240} { -.4531 -.3281 0.2240}
               { -.2187 -.6719 0.2240} { 0.2266 -.6875 0.2240} { 0.4766 -.3437 0.2240} { 0.6172 0.0000 0.1920}
               { 0.5781 0.4219 0.1920} { 0.2500 0.7578 0.1920} { -.2422 0.7500 0.1920} { -.5625 0.4062 0.1920}
               { -.5937 0.0000 0.1920} { -.4922 -.3594 0.1920} { -.2266 -.6875 0.1920} { 0.2266 -.7031 0.1920}
               { 0.5156 -.3750 0.1920} { 0.6406 0.0000 0.1600} { 0.5937 0.4297 0.1600} { 0.2500 0.7734 0.1600}
               { -.2500 0.7656 0.1600} { -.5781 0.4219 0.1600} { -.6172 0.0000 0.1600} { -.5078 -.3672 0.1600}
               { -.2422 -.7344 0.1600} { 0.2422 -.7500 0.1600} { 0.5234 -.3828 0.1600} { 0.6562 0.0000 0.1280}
               { 0.6094 0.4453 0.1280} { 0.2578 0.7891 0.1280} { -.2500 0.7734 0.1280} { -.5937 0.4297 0.1280}
               { -.6328 0.0000 0.1280} { -.5312 -.3828 0.1280} { -.2500 -.7656 0.1280} { 0.2500 -.7812 0.1280}
               { 0.5469 -.3984 0.1280} { 0.6875 0.0000 0.0960} { 0.6406 0.4609 0.0960} { 0.2578 0.8047 0.0960}
               { -.2578 0.7891 0.0960} { -.6016 0.4375 0.0960} { -.6406 0.0000 0.0960} { -.5469 -.3984 0.0960}
               { -.2500 -.7812 0.0960} { 0.2578 -.8047 0.0960} { 0.5781 -.4219 0.0960} { 0.6953 0.0000 0.0640}
               { 0.6484 0.4687 0.0640} { 0.2656 0.8203 0.0640} { -.2578 0.7969 0.0640} { -.6094 0.4375 0.0640}
               { -.6484 0.0000 0.0640} { -.5469 -.3984 0.0640} { -.2578 -.7891 0.0640} { 0.2656 -.8203 0.0640}
               { 0.5859 -.4297 0.0640} { 0.6953 0.0000 0.0320} { 0.6484 0.4687 0.0320} { 0.2656 0.8203 0.0320}
               { -.2578 0.7969 0.0320} { -.6172 0.4531 0.0320} { -.6562 0.0000 0.0320} { -.5469 -.3984 0.0320}
               { -.2656 -.8125 0.0320} { 0.2656 -.8281 0.0320} { 0.5781 -.4141 0.0320} { 0.6875 0.0000 0.0000}
               { 0.6406 0.4609 0.0000} { 0.2812 0.8516 0.0000} { -.2734 0.8359 0.0000} { -.6172 0.4531 0.0000}
               { -.6562 0.0000 0.0000} { -.5625 -.4062 0.0000} { -.2656 -.8203 0.0000} { 0.2734 -.8359 0.0000}
               { 0.5859 -.4297 0.0000} { 0.6875 0.0000 -.0320} { 0.6484 0.4687 -.0320} { 0.2812 0.8516 -.0320}
               { -.2734 0.8359 -.0320} { -.6172 0.4531 -.0320} { -.6484 0.0000 -.0320} { -.5547 -.4062 -.0320}
               { -.2656 -.8125 -.0320} { 0.2734 -.8359 -.0320} { 0.5859 -.4297 -.0320} { 0.6797 0.0000 -.0640}
               { 0.6562 0.4766 -.0640} { 0.2812 0.8672 -.0640} { -.2812 0.8516 -.0640} { -.6250 0.4531 -.0640}
               { -.6328 0.0000 -.0640} { -.5469 -.3984 -.0640} { -.2656 -.8125 -.0640} { 0.2734 -.8359 -.0640}
               { 0.5859 -.4297 -.0640} { 0.6953 0.0000 -.0960} { 0.6484 0.4687 -.0960} { 0.2812 0.8750 -.0960}
               { -.2812 0.8594 -.0960} { -.6172 0.4531 -.0960} { -.6641 0.0000 -.0960} { -.5703 -.4141 -.0960}
               { -.2734 -.8359 -.0960} { 0.2734 -.8437 -.0960} { 0.5937 -.4297 -.0960} { 0.6719 0.0000 -.1280}
               { 0.6328 0.4609 -.1280} { 0.2812 0.8594 -.1280} { -.2812 0.8516 -.1280} { -.6250 0.4531 -.1280}
               { -.6562 0.0000 -.1280} { -.5625 -.4062 -.1280} { -.2734 -.8359 -.1280} { 0.2734 -.8359 -.1280}
               { 0.5781 -.4141 -.1280} { 0.6562 0.0000 -.1600} { 0.6406 0.4609 -.1600} { 0.2812 0.8750 -.1600}
               { -.2812 0.8672 -.1600} { -.6328 0.4609 -.1600} { -.6484 0.0000 -.1600} { -.5625 -.4062 -.1600}
               { -.2656 -.8125 -.1600} { 0.2656 -.8125 -.1600} { 0.5703 -.4141 -.1600} { 0.6719 0.0000 -.1920}
               { 0.6484 0.4687 -.1920} { 0.2891 0.8906 -.1920} { -.2812 0.8750 -.1920} { -.6250 0.4531 -.1920}
               { -.6484 0.0000 -.1920} { -.5469 -.3984 -.1920} { -.2578 -.8047 -.1920} { 0.2656 -.8125 -.1920}
               { 0.5703 -.4141 -.1920} { 0.6641 0.0000 -.2240} { 0.6328 0.4609 -.2240} { 0.2812 0.8750 -.2240}
               { -.2812 0.8672 -.2240} { -.6250 0.4531 -.2240} { -.6484 0.0000 -.2240} { -.5234 -.3828 -.2240}
               { -.2578 -.7969 -.2240} { 0.2578 -.8047 -.2240} { 0.5391 -.3906 -.2240} { 0.6797 0.0000 -.2560}
               { 0.6406 0.4609 -.2560} { 0.2891 0.8828 -.2560} { -.2812 0.8750 -.2560} { -.6250 0.4531 -.2560}
               { -.6562 0.0000 -.2560} { -.5156 -.3750 -.2560} { -.2578 -.7891 -.2560} { 0.2578 -.7969 -.2560}
               { 0.5312 -.3828 -.2560} { 0.6562 0.0000 -.2880} { 0.6250 0.4531 -.2880} { 0.2812 0.8594 -.2880}
               { -.2812 0.8516 -.2880} { -.6172 0.4531 -.2880} { -.6484 0.0000 -.2880} { -.5078 -.3672 -.2880}
               { -.2422 -.7500 -.2880} { 0.2500 -.7578 -.2880} { 0.5156 -.3750 -.2880} { 0.6797 0.0000 -.3200}
               { 0.6406 0.4609 -.3200} { 0.2891 0.8984 -.3200} { -.2891 0.8828 -.3200} { -.6172 0.4531 -.3200}
               { -.6562 0.0000 -.3200} { -.5078 -.3672 -.3200} { -.2422 -.7422 -.3200} { 0.2266 -.6953 -.3200}
               { 0.5234 -.3828 -.3200} { 0.6641 0.0000 -.3520} { 0.6250 0.4531 -.3520} { 0.2891 0.8828 -.3520}
               { -.2812 0.8516 -.3520} { -.5859 0.4297 -.3520} { -.6094 0.0000 -.3520} { -.4766 -.3437 -.3520}
               { -.2266 -.7031 -.3520} { 0.2187 -.6641 -.3520} { 0.5156 -.3750 -.3520} { 0.6641 0.0000 -.3840}
               { 0.6328 0.4609 -.3840} { 0.2891 0.8828 -.3840} { -.2734 0.8437 -.3840} { -.5703 0.4141 -.3840}
               { -.5859 0.0000 -.3840} { -.4609 -.3359 -.3840} { -.1875 -.5859 -.3840} { 0.1953 -.5937 -.3840}
               { 0.5156 -.3750 -.3840} { 0.6641 0.0000 -.4160} { 0.6094 0.4375 -.4160} { 0.2891 0.8828 -.4160}
               { -.2812 0.8516 -.4160} { -.5703 0.4141 -.4160} { -.6016 0.0000 -.4160} { -.4766 -.3437 -.4160}
               { -.1797 -.5391 -.4160} { 0.1797 -.5391 -.4160} { 0.5156 -.3750 -.4160} { 0.6719 0.0000 -.4480}
               { 0.6016 0.4375 -.4480} { 0.2812 0.8672 -.4480} { -.2812 0.8516 -.4480} { -.5859 0.4297 -.4480}
               { -.6484 0.0000 -.4480} { -.4922 -.3594 -.4480} { -.0937 -.2891 -.4480} { 0.0937 -.2891 -.4480}
               { 0.5156 -.3750 -.4480} { 0.6562 0.0000 -.4800} { 0.6016 0.4375 -.4800} { 0.2656 0.8281 -.4800}
               { -.2656 0.8125 -.4800} { -.5859 0.4297 -.4800} { -.6328 0.0000 -.4800} { -.4687 -.3359 -.4800}
               { -.0937 -.2969 -.4800} { 0.0937 -.2969 -.4800} { 0.4766 -.3516 -.4800} { 0.6328 0.0000 -.5120}
               { 0.5859 0.4297 -.5120} { 0.2500 0.7656 -.5120} { -.2500 0.7578 -.5120} { -.5547 0.4062 -.5120}
               { -.6094 0.0000 -.5120} { -.4766 -.3437 -.5120} { -.0937 -.2891 -.5120} { 0.0937 -.2891 -.5120}
               { 0.4766 -.3437 -.5120} { 0.6094 0.0000 -.5440} { 0.5703 0.4141 -.5440} { 0.2422 0.7344 -.5440}
               { -.2422 0.7422 -.5440} { -.5391 0.3906 -.5440} { -.5859 0.0000 -.5440} { -.4609 -.3359 -.5440}
               { -.0391 -.1250 -.5440} { 0.0391 -.1250 -.5440} { 0.4766 -.3437 -.5440} { 0.1719 0.0000 -.5760}
               { 0.5703 0.4141 -.5760} { 0.2422 0.7344 -.5760} { -.2422 0.7422 -.5760} { -.5625 0.4062 -.5760}
               { -.1484 0.0000 -.5760} { -.4219 -.3047 -.5760} { -.0391 -.1172 -.5760} { 0.0391 -.1172 -.5760}
               { 0.4297 -.3125 -.5760} { 0.1719 0.0000 -.5760} { 0.5703 0.4141 -.5760} { 0.2422 0.7344 -.5760}
               { -.2422 0.7422 -.5760} { -.5625 0.4062 -.5760} { -.1484 0.0000 -.5760} { -.4219 -.3047 -.5760}
               { -.0391 -.1172 -.5760} { 0.0391 -.1172 -.5760} { 0.4297 -.3125 -.5760}

               { -.4609 0.0703 0.2880} { -.3281 0.0703 0.2880} { -.4609 0.2031 0.2880} { -.3281 0.2031 0.2880} { -.4609 0.0703 0.1920} { -.3281 0.0703 0.1920} { -.4609 0.2031 0.1920} { -.3281 0.2031 0.1920} { 0.3750 0.0703 0.2880} { 0.5078 0.0703 0.2880} { 0.3750 0.2031 0.2880} { 0.5078 0.2031 0.2880} { 0.3750 0.0703 0.1920} { 0.5078 0.0703 0.1920} { 0.3750 0.2031 0.1920} { 0.5078 0.2031 0.1920}
               { -.0859 -.2734 0.2240} { -.0156 -.2734 0.2240} { -.0859 -.1406 0.2240} { -.0156 -.1406 0.2240} { -.0859 -.2734 0.1280} { -.0156 -.2734 0.1280} { -.0859 -.1406 0.1280} { -.0156 -.1406 0.1280} { 0.0547 -.2734 0.2240} { 0.1250 -.2734 0.2240} { 0.0547 -.1406 0.2240} { 0.1250 -.1406 0.2240} { 0.0547 -.2734 0.1280} { 0.1250 -.2734 0.1280} { 0.0547 -.1406 0.1280} { 0.1250 -.1406 0.1280}
               { -.4922 -.0234 0.1280} { -.3594 -.0234 0.1280} { -.4922 0.1094 0.1280} { -.3594 0.1094 0.1280} { -.4922 -.0234 0.0320} { -.3594 -.0234 0.0320} { -.4922 0.1094 0.0320} { -.3594 0.1094 0.0320} { 0.3828 -.0234 0.1280} { 0.5156 -.0234 0.1280} { 0.3828 0.1094 0.1280} { 0.5156 0.1094 0.1280} { 0.3828 -.0234 0.0320} { 0.5156 -.0234 0.0320} { 0.3828 0.1094 0.0320} { 0.5156 0.1094 0.0320}
               { -.2734 0.5078 0.1280} { -.1406 0.5078 0.1280} { -.2734 0.6406 0.1280} { -.1406 0.6406 0.1280} { -.2734 0.5078 0.0320} { -.1406 0.5078 0.0320} { -.2734 0.6406 0.0320} { -.1406 0.6406 0.0320} { 0.1641 0.5078 0.1280} { 0.2969 0.5078 0.1280} { 0.1641 0.6406 0.1280} { 0.2969 0.6406 0.1280} { 0.1641 0.5078 0.0320} { 0.2969 0.5078 0.0320} { 0.1641 0.6406 0.0320} { 0.2969 0.6406 0.0320}
               { -.4922 -.4297 0.0960} { -.3594 -.4297 0.0960} { -.4922 -.2969 0.0960} { -.3594 -.2969 0.0960} { -.4922 -.4297 0.0000} { -.3594 -.4297 0.0000} { -.4922 -.2969 0.0000} { -.3594 -.2969 0.0000} { 0.4062 -.4297 0.0960} { 0.5391 -.4297 0.0960} { 0.4062 -.2969 0.0960} { 0.5391 -.2969 0.0960} { 0.4062 -.4297 0.0000} { 0.5391 -.4297 0.0000} { 0.4062 -.2969 0.0000} { 0.5391 -.2969 0.0000}
               { -.1797 -.3047 -.0960} { -.0781 -.3047 -.0960} { -.1797 -.2031 -.0960} { -.0781 -.2031 -.0960} { -.1797 -.3047 -.1920} { -.0781 -.3047 -.1920} { -.1797 -.2031 -.1920} { -.0781 -.2031 -.1920} { 0.1094 -.3047 -.0960} { 0.2109 -.3047 -.0960} { 0.1094 -.2031 -.0960} { 0.2109 -.2031 -.0960} { 0.1094 -.3047 -.1920} { 0.2109 -.3047 -.1920} { 0.1094 -.2031 -.1920} { 0.2109 -.2031 -.1920}
               { -.5234 0.0391 -.1280} { -.3906 0.0391 -.1280} { -.5234 0.1719 -.1280} { -.3906 0.1719 -.1280} { -.5234 0.0391 -.2240} { -.3906 0.0391 -.2240} { -.5234 0.1719 -.2240} { -.3906 0.1719 -.2240} { 0.4062 0.0391 -.1280} { 0.5391 0.0391 -.1280} { 0.4062 0.1719 -.1280} { 0.5391 0.1719 -.1280} { 0.4062 0.0391 -.2240} { 0.5391 0.0391 -.2240} { 0.4062 0.1719 -.2240} { 0.5391 0.1719 -.2240}
               { -.4375 0.4141 -.3840} { -.3047 0.4141 -.3840} { -.4375 0.5469 -.3840} { -.3047 0.5469 -.3840} { -.4375 0.4141 -.4480} { -.3047 0.4141 -.4480} { -.4375 0.5469 -.4480} { -.3047 0.5469 -.4480} { 0.3828 0.4141 -.3840} { 0.5156 0.4141 -.3840} { 0.3828 0.5469 -.3840} { 0.5156 0.5469 -.3840} { 0.3828 0.4141 -.4480} { 0.5156 0.4141 -.4480} { 0.3828 0.5469 -.4480} { 0.5156 0.5469 -.4480}
     }

     set lcnx {{ 0 10 1 none} { 0 11 12 none} { 0 2 3 none} { 0 13 14 none} { 0 4 5 none} { 0 15 16 none} { 0 6 7 none}
               { 0 17 18 none} { 0 8 9 none} { 0 19 20 none} { 10 30 21 1 none} { 2 22 23 3 none} { 4 24 25 5 none}
               { 6 26 27 7 none} { 8 28 29 9 none} { 11 31 32 12 none} { 13 33 34 14 none} { 15 35 36 16 none} { 17 37 38 18 none}
               { 19 39 40 20 none} { 30 50 41 21 none} { 22 42 43 23 none} { 24 44 45 25 none} { 26 46 47 27 none}
               { 28 48 49 29 none} { 31 51 52 32 none} { 33 53 54 34 none} { 35 55 56 36 none} { 37 57 58 38 none}
               { 39 59 60 40 none} { 50 70 61 41 none} { 42 62 63 43 none} { 44 64 65 45 none} { 46 66 67 47 none}
               { 48 68 69 49 none} { 51 71 72 52 none} { 53 73 74 54 none} { 55 75 76 56 none} { 57 77 78 58 none}
               { 59 79 80 60 none} { 70 90 81 61 none} { 62 82 83 63 none} { 64 84 85 65 none} { 66 86 87 67 none}
               { 68 88 89 69 none} { 71 91 92 72 none} { 73 93 94 74 none} { 75 95 96 76 none} { 77 97 98 78 none}
               { 79 99 100 80 none} { 90 110 101 81 none} { 82 102 103 83 none} { 84 104 105 85 none} { 86 106 107 87 none}
               { 88 108 109 89 none} { 91 111 112 92 none} { 93 113 114 94 none} { 95 115 116 96 none} { 97 117 118 98 none}
               { 99 119 120 100 none} { 110 130 121 101 none} { 102 122 123 103 none} { 104 124 125 105 none}
               { 106 126 127 107 none} { 108 128 129 109 none} { 111 131 132 112 none} { 113 133 134 114 none}
               { 115 135 136 116 none} { 117 137 138 118 none} { 119 139 140 120 none} { 130 150 141 121 none}
               { 122 142 143 123 none} { 124 144 145 125 none} { 126 146 147 127 none} { 128 148 149 129 none}
               { 131 151 152 132 none} { 133 153 154 134 none} { 135 155 156 136 none} { 137 157 158 138 none}
               { 139 159 160 140 none} { 150 170 161 141 none} { 142 162 163 143 none} { 144 164 165 145 none}
               { 146 166 167 147 none} { 148 168 169 149 none} { 151 171 172 152 none} { 153 173 174 154 none}
               { 155 175 176 156 none} { 157 177 178 158 none} { 159 179 180 160 none} { 170 190 181 161 none}
               { 162 182 183 163 none} { 164 184 185 165 none} { 166 186 187 167 none} { 168 188 189 169 none}
               { 171 191 192 172 none} { 173 193 194 174 none} { 175 195 196 176 none} { 177 197 198 178 none}
               { 179 199 200 180 none} { 190 210 201 181 none} { 182 202 203 183 none} { 184 204 205 185 none}
               { 186 206 207 187 none} { 188 208 209 189 none} { 191 211 212 192 none} { 193 213 214 194 none}
               { 195 215 216 196 none} { 197 217 218 198 none} { 199 219 220 200 none} { 210 230 221 201 none}
               { 202 222 223 203 none} { 204 224 225 205 none} { 206 226 227 207 none} { 208 228 229 209 none}
               { 211 231 232 212 none} { 213 233 234 214 none} { 215 235 236 216 none} { 217 237 238 218 none}
               { 219 239 240 220 none} { 230 250 241 221 none} { 222 242 243 223 none} { 224 244 245 225 none}
               { 226 246 247 227 none} { 228 248 249 229 none} { 231 251 252 232 none} { 233 253 254 234 none}
               { 235 255 256 236 none} { 237 257 258 238 none} { 239 259 260 240 none} { 250 270 261 241 none}
               { 242 262 263 243 none} { 244 264 265 245 none} { 246 266 267 247 none} { 248 268 269 249 none}
               { 251 271 272 252 none} { 253 273 274 254 none} { 255 275 276 256 none} { 257 277 278 258 none}
               { 259 279 280 260 none} { 270 290 281 261 none} { 262 282 283 263 none} { 264 284 285 265 none}
               { 266 286 287 267 none} { 268 288 289 269 none} { 271 291 292 272 none} { 273 293 294 274 none}
               { 275 295 296 276 none} { 277 297 298 278 none} { 279 299 300 280 none} { 290 310 301 281 none}
               { 282 302 303 283 none} { 284 304 305 285 none} { 286 306 307 287 none} { 288 308 309 289 none}
               { 291 311 312 292 none} { 293 313 314 294 none} { 295 315 316 296 none} { 297 317 318 298 none}
               { 299 319 320 300 none} { 310 330 321 301 none} { 302 322 323 303 none} { 304 324 325 305 none}
               { 306 326 327 307 none} { 308 328 329 309 none} { 311 331 332 312 none} { 313 333 334 314 none}
               { 315 335 336 316 none} { 317 337 338 318 none} { 319 339 340 320 none} { 330 350 341 321 none}
               { 322 342 343 323 none} { 324 344 345 325 none} { 326 346 347 327 none} { 328 348 349 329 none}

               { 351 355 356 352 0} { 351 353 357 355 0} { 351 352 354 353 0} { 358 354 352 356 0} { 358 357 353 354 0} { 358 356 355 357 0}
               { 359 363 364 360 0} { 359 361 365 363 0} { 359 360 362 361 0} { 366 362 360 364 0} { 366 365 361 362 0} { 366 364 363 365 0}
               { 367 371 372 368 1} { 367 369 373 371 1} { 367 368 370 369 1} { 374 370 368 372 1} { 374 373 369 370 1} { 374 372 371 373 1}
               { 375 379 380 376 1} { 375 377 381 379 1} { 375 376 378 377 1} { 382 378 376 380 1} { 382 381 377 378 1} { 382 380 379 381 1}
               { 383 387 388 384 2} { 383 385 389 387 2} { 383 384 386 385 2} { 390 386 384 388 2} { 390 389 385 386 2} { 390 388 387 389 2}
               { 391 395 396 392 2} { 391 393 397 395 2} { 391 392 394 393 2} { 398 394 392 396 2} { 398 397 393 394 2} { 398 396 395 397 2}
               { 399 403 404 400 3} { 399 401 405 403 3} { 399 400 402 401 3} { 406 402 400 404 3} { 406 405 401 402 3} { 406 404 403 405 3}
               { 407 411 412 408 3} { 407 409 413 411 3} { 407 408 410 409 3} { 414 410 408 412 3} { 414 413 409 410 3} { 414 412 411 413 3}
               { 415 419 420 416 4} { 415 417 421 419 4} { 415 416 418 417 4} { 422 418 416 420 4} { 422 421 417 418 4} { 422 420 419 421 4}
               { 423 427 428 424 4} { 423 425 429 427 4} { 423 424 426 425 4} { 430 426 424 428 4} { 430 429 425 426 4} { 430 428 427 429 4}
               { 431 435 436 432 5} { 431 433 437 435 5} { 431 432 434 433 5} { 438 434 432 436 5} { 438 437 433 434 5} { 438 436 435 437 5}
               { 439 443 444 440 5} { 439 441 445 443 5} { 439 440 442 441 5} { 446 442 440 444 5} { 446 445 441 442 5} { 446 444 443 445 5}
               { 447 451 452 448 6} { 447 449 453 451 6} { 447 448 450 449 6} { 454 450 448 452 6} { 454 453 449 450 6} { 454 452 451 453 6}
               { 455 459 460 456 6} { 455 457 461 459 6} { 455 456 458 457 6} { 462 458 456 460 6} { 462 461 457 458 6} { 462 460 459 461 6}
               { 463 467 468 464 7} { 463 465 469 467 7} { 463 464 466 465 7} { 470 466 464 468 7} { 470 469 465 466 7} { 470 468 467 469 7}
               { 471 475 476 472 7} { 471 473 477 475 7} { 471 472 474 473 7} { 478 474 472 476 7} { 478 477 473 474 7} { 478 476 475 477 7}
     }     

     set obj  [list $lvtx $lcnx]

     set gvar($win,display) [list $obj]

     ####################################################################
     # Calculate the bounds (xmin,xmax,ymin,ymax,zmin,zmax)
     #
     set xmin  9.9999e10
     set xmax -9.9999e10
     set ymin $xmin
     set ymax $xmax
     set zmin $xmin
     set zmax $xmax
     foreach obj $gvar($win,display) {
         set verts [lindex $obj 0]
         foreach v $verts {
             foreach {x y z} $v {
                 set xmin [bb_min $x $xmin]
                 set ymin [bb_min $y $ymin]
                 set zmin [bb_min $z $zmin]
                 set xmax [bb_max $x $xmax]
                 set ymax [bb_max $y $ymax]
                 set zmax [bb_max $z $zmax]
             }
         }
     }
     set gvar($win,display,bounds) [list $xmin $xmax $ymin $ymax $zmin $zmax]

     ####################################################################
     # Calculate Normal for each face
     #
     set newdisp {}
     foreach obj $gvar($win,display) {
         set verts [lindex $obj 0]
         set conn  [lindex $obj 1]

         set faceNorm {}
         set newconn  {}
         foreach edge $conn {
             set i1 [lindex $edge 0]
             set i2 [lindex $edge 1]
             set i3 [lindex $edge 2]

             set v1 [lindex $verts $i1]
             set v2 [lindex $verts $i2]
             set v3 [lindex $verts $i3]

             set u_vector   [Get_Vector $v1 $v2]
             set v_vector   [Get_Vector $v1 $v3]

             set normal     [Normal_Vector $u_vector $v_vector]

             lappend newconn  [lrange $edge 0 [expr [llength $edge] - 2]] $normal [lindex $edge end]
         }
         lappend newdisp [list $verts $newconn]
     }
     set gvar($win,display) $newdisp
 }


 #********************************* Draw_Face ************************

 proc Draw_Face {win verts edge fNorm {color white} } {
global gvar

    if { [llength $edge] < 3} {
         return
     }

     set nedges [expr {[llength $edge] - 1}]
     set ilast  [lindex $edge $nedges]
     set vtx    [lindex $verts $ilast]
     foreach {x y z} $vtx {break}
     bb_d_move $win $x $y $z 1

     for {set n 0} {$n < [llength $edge]} {incr n} {
         set i    [lindex $edge $n]
         set vtx  [lindex $verts $i]
         foreach {x y z} $vtx {break}
         bb_d_draw $win $x $y $z 1 $color
     }

 }

 #********************************* Shade_Face ************************

 proc Shade_Face {win verts edge fNorm {fillcolor blue} {edgecolor white} } {

global gvar
    if { [llength $edge] < 3} {
         return
     }

     set nedges [expr {[llength $edge] - 1}]
     set ilast  [lindex $edge $nedges]
     set vtx    [lindex $verts $ilast]
     foreach {x y z} $vtx {break}

     set coordlst [list $x $y $z]

     for {set n 0} {$n < [llength $edge]} {incr n} {
         set i    [lindex $edge $n]
         set vtx  [lindex $verts $i]
         foreach {x y z} $vtx {break}
         lappend coordlst $x $y $z
     }

     bb_draw_shaded $win $coordlst $fillcolor $edgecolor
 }

 proc cull_backfaces {win verts face } {
global gvar
    set retval {}

     foreach {edge faceNorm draw} $face {

         set i1 [lindex $edge 0]
         set i2 [lindex $edge 1]
         set i3 [lindex $edge 2]

         set v1 [lindex $verts $i1]
         set v2 [lindex $verts $i2]
         set v3 [lindex $verts $i3]

         set u_vector       [Get_Vector $v1 $v2]
         set v_vector       [Get_Vector $v1 $v3]
         set line_of_sight  [Get_Vector $v1 $gvar($win,eye_point)]

         set normal         [Vector_CrossProduct $u_vector $v_vector]
         set mag            [Vector_DotProduct $faceNorm $line_of_sight]

         if {$mag < 0.0 && $draw == "none"} {
             set visible 0
         } else {
             set visible 1
             lappend retval $edge $faceNorm $draw
         }
     }

     return $retval
 }

 #  sort each face based on it's distance from the "eye_point"
 #  This is to implement the "Painter's Algorithm" hidden surface
 #  removal
 #
 proc sortconn {win verts face } {
global gvar
    set sconn {}

     foreach {xe ye ze} $gvar($win,eye_point) {break}

     foreach {edge faceNorm draw} $face {
         set d 0.0
         set nedge [llength $edge]

         for {set n 0} {$n < $nedge} {incr n} {
             set i    [lindex $edge $n]
             set vtx  [lindex $verts $i]
             foreach {x y z} $vtx {break}

             set dx    [expr {$x - $xe}]
             set dy    [expr {$y - $ye}]
             set dz    [expr {$z - $ze}]
             set vdist [expr {sqrt($dx*$dx + $dy*$dy + $dz*$dz)}]

             set d [expr {$d + $vdist}]
         }
         set d [expr {$d / $nedge}]
         lappend sconn [list $d $edge $faceNorm $draw]
     }

     set sconn [lsort -decreasing -index 0 $sconn]

     set newface {}
     foreach s $sconn {
         lappend newface [lindex $s 1] [lindex $s 2] [lindex $s 3]
     }

     return $newface
 }

 proc canvas_rotate_start {win w x y} {
global gvar
     set gvar($win,canvas,rotate,coords) [list $x $y]
 }

 proc canvas_rotate_drag {win w x y} {
global gvar
     foreach {newtheta newphi} [canvas_rotate_calc $win $x $y] {break}

     set gvar($win,theta) $newtheta
     set gvar($win,phi)   $newphi

     set gvar($win,canvas,rotate,coords) [list $x $y]
     bb_redraw $win
 }

 proc canvas_rotate_end {win w x y} {
global gvar
     foreach {newtheta newphi} [canvas_rotate_calc $win $x $y] {break}

     set gvar($win,theta) $newtheta
     set gvar($win,phi)   $newphi

     set gvar($win,canvas,rotate,coords) [list $x $y]
     bb_redraw $win
 }

 proc canvas_rotate_calc {win x y} {

global gvar
    foreach {xold yold} $gvar($win,canvas,rotate,coords) {break}
     set dx [expr { ($x - $xold) * 0.01}]
     set dy [expr {-($y - $yold) * 0.01}]

     set newtheta    [expr {$gvar($win,theta) - $dx}]
     set newphi      [expr {$gvar($win,phi)   + $dy}]

     if {$newtheta > $gvar($win,pi)} {
         set newtheta [expr {$newtheta - $gvar($win,twopi)}]
     }     
     if {$newtheta < $gvar($win,mpi)} {
         set newtheta [expr {$newtheta + $gvar($win,twopi)}]
     }
     if {$newphi > $gvar($win,pi)} {
         set newphi [expr {$newphi - $gvar($win,twopi)}]
     }
     if {$newphi < $gvar($win,mpi)} {
         set newphi [expr {$newphi + $gvar($win,twopi)}]
     }

     set retval [list $newtheta $newphi]
     return $retval
 }

 proc canvas_pan_start {win w x y} {

     global gvar
     set gvar($win,canvas,pan,coords) [list $x $y]
 }

 proc canvas_pan_drag {win w x y} {

     global gvar
     foreach {xold yold} $gvar($win,canvas,pan,coords) {break}

     set dx  [expr { ($x - $xold) * 1.0}]
     set dy  [expr {-($y - $yold) * 1.0}]

     set gvar($win,x,translate)  [expr {$gvar($win,x,translate) + $dx}]
     set gvar($win,y,translate)  [expr {$gvar($win,y,translate) + $dy}]

     set gvar($win,canvas,pan,coords) [list $x $y]

     bb_redraw $win
 }

 proc canvas_pan_end {win w x y} {

     global gvar
     foreach {xold yold} $gvar($win,canvas,pan,coords) {break}

     set dx  [expr { ($x - $xold) * 0.1}]
     set dy  [expr {-($y - $yold) * 0.1}]

     set gvar($win,x,translate)  [expr {$gvar($win,x,translate) + $dx}]
     set gvar($win,y,translate)  [expr {$gvar($win,y,translate) + $dy}]

     set gvar($win,canvas,pan,coords) [list $x $y]

     bb_redraw $win
 }

 proc canvas_zoom_start {win w x y} {

global gvar
     set gvar($win,canvas,zoom,coords) [list $x $y]
 }

 proc canvas_zoom_drag {win w x y} {

global gvar
     foreach {xold yold} $gvar($win,canvas,zoom,coords) {break}

     set dy [expr {-($y - $yold)}]

     if {$dy != 0} {
         set gvar($win,dist) [expr {$gvar($win,dist) + $dy * 2}]
     }
     set gvar($win,canvas,zoom,coords) [list $x $y]
     bb_redraw $win
 }

 proc canvas_zoom_end {win w x y} {
global gvar
     foreach {xold yold} $gvar($win,canvas,zoom,coords) {break}

     set dy [expr {-($y - $yold)}]

     if {$dy != 0} {
         set gvar($win,dist) [expr {$gvar($win,dist) + $dy * 2}]
     }
     set gvar($win,canvas,zoom,coords) [list $x $y]
     bb_redraw $win
 }

 #************************** bb_Draw_Objects *************************

 proc bb_Draw_Objects {win} {

global gvar
     global voxel_colors
     global voxel_shades
     global voxel_shade_colors

     foreach obj $gvar($win,display) {
         set verts    [lindex $obj 0]
         set face     [lindex $obj 1]


          set face [cull_backfaces $win $verts $face]

          set face [sortconn $win $verts $face]

          foreach {edge fNorm draw} $face {
              
                if {$draw == "none"} { 
                     set colr "none" 
                } else {
                  set colr [lindex $voxel_colors $draw]
                  set shade [lindex $gvar($win,colors) $draw] 
                  if {$shade == "none"} {
                     set fill_color "#000"
                  } else {
                    if {$shade < 0} {
                      set shade 0
                    } else {
                      set shade [expr round($shade * 20)] 
                    }
                    set fill_color "#[lindex $voxel_shade_colors $shade][lindex $voxel_shade_colors $shade][lindex $voxel_shade_colors $shade]"
                  }
                }
              
                if {$colr != "none"} {    
                  Shade_Face $win $verts $edge $fNorm $fill_color $colr
                } else {
                  Draw_Face $win $verts $edge $fNorm "#8f8f8f"
                }
             }
     }

     $gvar($win,canv) delete values    

     set y 200
     foreach {color} $gvar($win,colors) {

       if {$color == "none"} {
         $gvar($win,canv) create text 70  $y -text "---" -fill "#ccc" -anchor w -tag values -font checkbox_font
       } else {
         $gvar($win,canv) create text 70  $y -text [format "%1.3f" $color] -fill "#ccc" -anchor w -tag values -font checkbox_font
       } 
       incr y 40
     }  
 }


 # Update the display
 #
 proc bb_redraw {win} {

global gvar
     $gvar($win,canv) delete brain
     Set_Viewing_Transform $win $gvar($win,rho) $gvar($win,theta) $gvar($win,phi) $gvar($win,dist)
     bb_Draw_Objects $win
 }

 
 #******************************* M A I N ************************************


set voxel_shade_colors [list 23 2d 37 41 4b 55 5f 69 73 7d 87 91 9b a5 af b9 c3 cd d7 e1 eb]
set voxel_colors [list "#f00" "#090" "#00f" "#aa0" "#0aa" "#a0a" "#0e0" "#36a"]

 proc main_bold_brains {config win title init} {

     global voxel_colors
     global voxel_shade_colors

     global gvar

     bb_Init $config $win $title $init

     $gvar($win,canv) create text 10  200 -text "manual" -fill [lindex $voxel_colors 0] -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  240 -text "goal" -fill [lindex $voxel_colors 1] -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  280 -text "vocal" -fill [lindex $voxel_colors 2] -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  320 -text "imaginal" -fill [lindex $voxel_colors 3] -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  360 -text "retrieval" -fill [lindex $voxel_colors 4] -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  400 -text "procedural" -fill [lindex $voxel_colors 5] -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  440 -text "aural" -fill [lindex $voxel_colors 6] -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  480 -text "visual" -fill [lindex $voxel_colors 7] -anchor w -font checkbox_font

     $gvar($win,canv) create text 10  10 -text "0.0" -fill "#ccc" -anchor w -font checkbox_font
     $gvar($win,canv) create text 10  170 -text "1.0" -fill "#ccc" -anchor w -font checkbox_font

     set y 5
     foreach x $voxel_shade_colors {
       $gvar($win,canv) create rectangle 30 $y 40 [expr $y + 8] -outline "" -fill "#$x$x$x"
       incr y 8
     }

     bb_ReadData $win

     Set_Viewing_Transform $win $gvar($win,rho) $gvar($win,theta) $gvar($win,phi) $gvar($win,dist)

     bb_Draw_Objects $win
 }

proc start_realtime_brain_view {} {
  if {[currently_selected_model] == "nil"} {

    tk_messageBox -icon info -type ok -title "3d BOLD viewer" -message "BOLD tools require a current model."
  } else {
    main_bold_brains .bold_brain_3d_real ".bold_brain_3d_real_[currently_selected_model]" "3d BOLD run-time viewer" create_brain_real_time_view
  }
}

button [control_panel_name].bold_brains_3d_real -command {start_realtime_brain_view} \
       -text "Run-time 3D brain" -font button_font

# put that button on the control panel

pack [control_panel_name].bold_brains_3d_real
