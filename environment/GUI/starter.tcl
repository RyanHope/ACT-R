#!/bin/sh
# start with wish instead \
 exec wish "$0"

# This was formerly in the splash-screen.tcl init file
# This script opens a window with the Powered by ONR Logo in it
# centered on the screen for 3 seconds or until it's clicked 

# read the image from the file
image create photo onr_logo -file [file join [pwd] "logo.gif"]

#create a window for it with no border and hide it
toplevel .splash
wm overrideredirect .splash 1
wm withdraw .splash

# make a label to hold the image and put it in the window
label .splash.image -image onr_logo
pack .splash.image

# create a variable that will trigger the destruction of the window
# after 3 seconds or when the window is clicked

set clear_splash 0
global clear_splash


# compute the window position given that the
# logo is 579x288 and set it

set x [expr ([winfo screenwidth .]/2) - 290]
set y [expr ([winfo screenheight .]/2) - 144] 

wm geometry .splash +$x+$y

# show the window
wm deiconify .splash
focus -force .splash

# set the variable that triggers destruction after 3 seconds
# or as soon as a button is pressed on the window

after 3000 {set clear_splash 1}
bind .splash <ButtonPress> {set clear_splash 1}

# wait for one of the triggering events to happen
tkwait variable clear_splash

# close the window
destroy .splash

# Here's where the real work takes place...

global quit_now
set quit_now ""

# close the console window if it's open 

if {$tcl_platform(platform) != "unix"} {console hide}

# Start by hiding .

wm withdraw .

proc set_quit_now {val} {
  global quit_now
  set quit_now $val
}

file delete [file join [pwd] "error.log"]

while {$quit_now != 1} {

  global quit_now

  set quit_now ""
  interp create env_instance
  load {} Tk env_instance
  env_instance alias set_return_result set_quit_now
  env_instance alias console console
  env_instance eval {source "server.tcl"}
  # this is the same race condition that I had
  # problems with in the main code, but here
  # it's not really a problem because the chance
  # that a user could close the control panel
  # between the if and the then is virtually
  # impossible particularly because the setting
  # of quit_now happens for each of the items
  # on the control panel...
  if {$quit_now == ""} {tkwait variable quit_now}
  interp delete env_instance
}

# chances are that it'll never get to here, but
# always safest to have a default case

exit

# when debugging through the console need to
# make sure that I deal with the sub interpreter:
#
# 4 % env_instance eval {winfo children .}
# .control_panel .copyright
