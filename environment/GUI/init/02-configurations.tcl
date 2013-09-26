# These strings set the default positions and sizes to use for the 
# dialogs so named. They are formatted such that they can be passed to
# wm geometry.  The plan is to actually change them when the window 
# is moved or resized, so that the next one appears where the last one was 
# (could be an option setting) and then it gets written to the config file
# on exit so that next time things "are as they were the last time".

set screen_width [winfo screenwidth .]
set screen_height [winfo screenheight .]

set screen_center_x [expr $screen_width / 2]
set screen_center_y [expr $screen_height / 2]

global window_config

set window_config(.copyright) \
    "400x466+[expr $screen_center_x - 200]+[expr $screen_center_y - 233]"

if {$standalone_mode == 0} {
  set window_config(.control_panel) \
      "170x700+[expr $screen_width - 190]+[expr $screen_center_y - 350]"
} else {
  set window_config(.control_panel) \
      "235x700+[expr $screen_width - 255]+[expr $screen_center_y - 350]"
}


set window_config(.reload_response) \
    "500x230+[expr $screen_center_x - 250]+[expr $screen_center_y - 115]"

set window_config(.stepper) \
    "500x550+[expr $screen_center_x - 250]+[expr $screen_center_y - 275]"

set window_config(.tutor_response) \
    "480x130+[expr $screen_center_x - 240]+[expr $screen_center_y - 65]"


set window_config(.declarative) \
    "420x300+[expr $screen_center_x - 210]+[expr $screen_center_y - 150]"

set window_config(.whynotdm) \
    "200x300+[expr $screen_center_x - 100]+[expr $screen_center_y - 150]"

set window_config(.procedural) \
    "500x400+[expr $screen_center_x - 250]+[expr $screen_center_y - 200]"

set window_config(.whynot) \
    "200x300+[expr $screen_center_x - 100]+[expr $screen_center_y - 150]"

set window_config(.buffers) \
    "350x240+[expr $screen_center_x - 175]+[expr $screen_center_y - 120]"

set window_config(.bufferstatus) \
    "350x240+[expr $screen_center_x - 175]+[expr $screen_center_y - 120]"

set window_config(.visicon) \
    "660x150+[expr $screen_center_x - 330]+[expr $screen_center_y - 100]"

set window_config(.audicon) \
    "870x150+[expr $screen_center_x - 435]+[expr $screen_center_y - 100]"

set window_config(.param_viewer) \
    "400x330+[expr $screen_center_x - 200]+[expr $screen_center_y - 150]"


set window_config(.graphic_trace) \
    "870x500+[expr $screen_center_x - 435]+[expr $screen_center_y - 250]"

set window_config(.vert_graphic_trace) \
    "870x500+[expr $screen_center_x - 435]+[expr $screen_center_y - 250]"

set window_config(.pgraph) \
    "700x400+[expr $screen_center_x - 350]+[expr $screen_center_y - 200]"


set window_config(.ptrace) \
    "410x370+[expr $screen_center_x - 205]+[expr $screen_center_y - 150]"

set window_config(.retrieval_history) \
    "670x380+[expr $screen_center_x - 335]+[expr $screen_center_y - 150]"

set window_config(.buffer_history) \
    "530x290+[expr $screen_center_x - 265]+[expr $screen_center_y - 100]"


set window_config(.bold_graphs) \
    "660x250+[expr $screen_center_x - 330]+[expr $screen_center_y - 120]"

set window_config(.bold_slices) \
    "766x388+[expr $screen_center_x - 383]+[expr $screen_center_y - 150]"

set window_config(.bold_brain_3d) \
    "658x586+[expr $screen_center_x - 329]+[expr $screen_center_y - 260]"

set window_config(.bold_brain_3d_real) \
    "658x508+[expr $screen_center_x - 329]+[expr $screen_center_y - 250]"


set window_config(.options) \
    "450x274+[expr $screen_center_x - 225]+[expr $screen_center_y - 137]"


set window_config(.model) \
    "400x500+0+20"

proc check_for_configuration {w} {
   global window_config
   return [llength [array names window_config $w]] 
}

proc get_configuration {w args} {
   global window_config
   global screen_center_x
   global screen_center_y

   if [llength $args] {
     set_window_config_name [lindex $args 0] $w
   }

   if {[check_for_configuration $w] == 0} {
     set window_config($w) "200x200+[expr $screen_center_x - 100]+[expr $screen_center_y - 100]"
   } 

   return $window_config($w)
}


global window_crossreference
global changed_window_list

proc set_window_config_name {w v} {
  global window_crossreference

  set window_crossreference($w) $v 
}

proc check_for_crossreference {w} {
  global window_crossreference
  return [llength [array names window_crossreference $w]] 
}

bind all <Configure> {
  global window_config
  global window_crossreference
  global changed_window_list

  if [check_for_crossreference %W] {
    set name $window_crossreference(%W)
  } else {
    set name %W
  } 
  if [check_for_configuration $name] {
    set window_config($name) [wm geometry %W]
    set changed_window_list($name) 1
  }
}

proc save_window_positions {} {
  global tcl_env_dir
  global changed_window_list
  global window_config

  set file [file join $tcl_env_dir init "10-userguisettings.tcl"]

  write_data "# this file generated when environment is closed\n# refresh . to make sure sizes are right\n\nwm deiconify .\nupdate\nwm withdraw .\n" $file
 
  append_data "if {\[winfo screenwidth .\] != [winfo screenwidth .] || \[winfo screenheight .\] != [winfo screenheight .] || \[lindex \[wm maxsize .\] 0\] != [lindex [wm maxsize .] 0] || \[lindex \[wm maxsize .\] 1\] != [lindex [wm maxsize .] 1]} {\n" $file
  append_data "  set size_mismatch 1\n} else {\n  set size_mismatch 0\n}\n\n" $file

 
  append_data "if \$size_mismatch {\n" $file
  append_data "  set reset_window_sizes \[tk_messageBox -icon warning -title \"Screen resolution changed\" -type yesno \\\n" $file
  append_data "                                         -message \"The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?\"\]\n" $file
  append_data "} else { set reset_window_sizes 0}\n" $file
  

  append_data "if {\$reset_window_sizes != \"yes\"} {\n" $file

  foreach name [array names changed_window_list] {
    append_data "  set window_config($name) $window_config($name)\n" $file
    append_data "  set changed_window_list($name) 1\n" $file
  }

  append_data "}\n" $file
}
