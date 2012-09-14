# This file creates windows on the Environment side for showing
# Experiments generated with the AGI.
# There can be any number of experiment windows on the environment side,
# but there's only one "main" handler which isn't visible but is used
# as the target for all the communication: .env_window since I need 
# something to setup the handler on the Lisp side.

toplevel .env_window
wm withdraw .env_window

send_environment_cmd \
  "create env-window-handler .env_window .env_window \
    (lambda (x) \
      (setf (environment-control-use-env-windows *environment-control*) $options_array(use_env_window) ) \
      (list 'ignore)) ()"

bind .env_window <Destroy> {
  remove_handler [get_handler_name .env_window]
}



proc send_keypress_to_actr {win key} {
  send_environment_cmd "update [get_handler_name .env_window] \
                        (lambda (x) \
                          (declare (ignore x)) \
                          (env-window-key-pressed \"$win\" #\\$key) \
                          (list 'ignore))"
  }

proc send_button_press_to_actr {win but} {
  send_environment_cmd "update [get_handler_name .env_window] \
                        (lambda (x) \
                          (declare (ignore x)) \
                          (env-button-pressed \"$win\" \"$but\") \
                               (list 'ignore))"
  }

proc create_env_window {win title width height x y} {


  toplevel $win
  wm withdraw $win

  # don't really close it when the close button pressed
  wm protocol $win WM_DELETE_WINDOW "wm withdraw $win"

  # send all the keypresses to ACT-R

  bind $win <KeyPress> "send_keypress_to_actr $win %K"

  wm title $win $title
  wm geometry $win [format "%dx%d+%d+%d" $width $height $x $y]

  canvas $win.can
  place $win.can -x 0 -y 0 -relwidth 1.0 -relheight 1.0

  wm deiconify $win
  focus -force $win
}

 


global id_references
global button_references

set cursor_id ""
set attention_id ""
set eyeloc_id ""

proc process_env_window {dummy cmd} {
  global id_references
  global cursor_id
  global attention_id
  global eyeloc_id
  global button_references
 

  set win ".[lindex $cmd 1]"
 
  switch [lindex $cmd 0] {
    ignore {
    }
    select {
      wm deiconify $win
      raise $win
      focus -force $win
    }
    close {
      $win.can delete all
      destroy $win 
    }
    clear {
      $win.can delete line button text
    }
    open {
      create_env_window $win [lindex $cmd 2] [lindex $cmd 5] [lindex $cmd 6] [lindex $cmd 3] [lindex $cmd 4]  
    }
    remove {
      $win.can delete [lindex $cmd 2]
    }

    attention {
      $win.can delete attention
      set x [lindex $cmd 2]
      set y [lindex $cmd 3]
      $win.can create oval [expr $x - 10] [expr $y - 10] [expr $x + 10] [expr $y + 10] -outline red -width 4 -tag attention
    }

    clearattention {
      $win.can delete attention
    }

    eyeloc {
      $win.can delete eyeloc
      set x [lindex $cmd 2]
      set y [lindex $cmd 3]
      $win.can create oval [expr $x - 5] [expr $y - 5] [expr $x + 5] [expr $y + 5] -outline blue -width 3 -tag eyeloc
    }

    cleareyeloc {
      $win.can delete eyeloc
    }


    cursor {
      $win.can delete cursor
      set x [lindex $cmd 2]
      set y [lindex $cmd 3]
      $win.can create polygon $x $y $x [expr $y + 15] [expr $x + 5] [expr $y + 12] [expr $x + 9] [expr $y + 20] \
                              [expr $x + 13] [expr $y + 19] [expr $x + 9] [expr $y + 12] [expr $x + 15] [expr $y + 12] \
                              -fill white -outline black -tag cursor
    }

    line {
      $win.can create line [lindex $cmd 3] [lindex $cmd 4] \
                         [lindex $cmd 6] [lindex $cmd 7] \
                         -fill [lindex $cmd 5] -width 2 -tags [list [lindex $cmd 2] line]
    }
    text {
      $win.can create text [lindex $cmd 3] [lindex $cmd 4] \
                              -font env_window_font -fill [lindex $cmd 5] \
                              -text [lindex $cmd 6] -anchor nw -tags [list [lindex $cmd 2] text]
    }
    button {
      set but \
       [button [new_variable_name $win.can.but] -text [lindex $cmd 7] -bg [lindex $cmd 8] -font button_font\
               -command "send_button_press_to_actr $win [lindex $cmd 2]"]

      $win.can create window [lindex $cmd 3] [lindex $cmd 4] \
                                -window $but -anchor nw \
                                -height [lindex $cmd 6] -width [lindex $cmd 5] -tags [list [lindex $cmd 2] button]
    }
    click {
      set but [$win.can itemcget [$win.can find withtag [lindex $cmd 2]] -window]
      set old "[$but cget -command]"
      $but configure -command ""
      event generate $but <ButtonPress> -button 1
      after 100 "event generate $but <ButtonRelease> -button 1;\
                 $but configure -command {$old}"
    }
  }
}
