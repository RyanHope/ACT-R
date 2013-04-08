# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 1440 || [winfo screenheight .] != 900 || [lindex [wm maxsize .] 0] != 1402 || [lindex [wm maxsize .] 1] != 856} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.bufferstatus) 200x200+620+350
  set changed_window_list(.bufferstatus) 1
  set window_config(.visicon) 715x319+390+350
  set changed_window_list(.visicon) 1
  set window_config(.stepper) 933x603+458+221
  set changed_window_list(.stepper) 1
  set window_config(.control_panel) 175x700+1204+108
  set changed_window_list(.control_panel) 1
  set window_config(.declarative) 420x300+510+300
  set changed_window_list(.declarative) 1
  set window_config(.buffers) 1124x340+35+436
  set changed_window_list(.buffers) 1
  set window_config(.copyright) 400x466+520+217
  set changed_window_list(.copyright) 1
  set window_config(.reload_response) 1052x440+255+118
  set changed_window_list(.reload_response) 1
}
