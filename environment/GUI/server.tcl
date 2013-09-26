# 
# This is the main environment file.
# It contains the procedures and variables for the network
# communication, sources all of the other files, creates the
# control panel window as .control_panel, and opens the listening socket.

# This variable is used to conditionalize code that
# operates differently when the environment is being
# built into a standalone application.

global standalone_mode

# Use 0 for not standalne
# Use 1 for Windows standalone
# Use 2 for Mac standalone

set standalone_mode 0

# This variable is an array that will be used to hold the
# table of Lisp handler names indexed by Tcl window name.
# These get entered by the register command coming from Lisp.

global handler_names


# valid_handler_name
# Given a tcl window (w) return true if there is 
# currently an entry in the table of that window's 
# Lisp handler and 0 if there is not.

proc valid_handler_name {w} {
  global handler_names
  return [llength [array names handler_names $w]] 
}

# get_handler_name
# Given a tcl window (w) return the name of the Lisp handler
# associated with that window.  There is no check to see if
# that window has a handler - that must be done prior to calling
# this procedure.

proc get_handler_name {w} {
  global handler_names
  return $handler_names($w)
}

# set_handler_name 
# Given a tcl window (w) and the name of a Lisp handler (h)
# set the handler entry of w to be h in the global lookup table.

proc set_handler_name {w h} {
  global handler_names
  set handler_names($w) $h
}


# This variable and the next procedure are used to implement
# basically a gentemp in Tcl.  The reason I need it is because every 
# dialog that comunicates with Lisp needs a unique name and 
# probably a variable to be set to return values.  This allows
# me to easily create multiple instances of the "standard" 
# environment dialogs.

global variable_count_array


global time_to_exit 
set time_to_exit 1

# new_variable_name
# Given any variable name prefix (prefix) return that 
# prefix with the next number appended to it.  Doesn't 
# gurantee uniqueness though because if you use name#
# variables elswhere this could end up generating that
# same name.
 
proc new_variable_name {prefix} {
  global variable_count_array
  if [llength [array names variable_count_array $prefix]] {
    return "$prefix[incr variable_count_array($prefix)]" 
  } else {
    return "$prefix[set variable_count_array($prefix) 1]"
  }
}

# record the Environment's directory
global top_dir
global tcl_env_dir

set tcl_env_dir [pwd]
cd ..
if {$standalone_mode == 0} {cd ..}

set top_dir [pwd]

if {$standalone_mode == 0} {cd "environment"}

cd "GUI"


# To make sure everything is the "same size" on all systems
# I need to set a consistent scaling - most important for the
# Environment display of virtual windows otherwise the fixation
# cursor isn't in the 'right' place ...

tk scaling 1.0


# Define some general file handling procedures.
# Taken pretty much right from the Tcl/Tk book I've got

proc read_file { filename } {
  set data ""
  if {[file readable $filename]} {
    set fileid [open $filename "r"]
    set data [read $fileid]
    close $fileid
  }

  return $data
}

proc save_text {textwidget filename} {
  set data [$textwidget get 1.0 {end -1c}]
  write_data $data $filename
}

proc write_data {data filename} {
  set fileid [open $filename "w"]
  puts -nonewline $fileid $data
  # I know I shouldn't have to flush before closing, but I've
  # got a weird synchronization problem that seems to be caused
  # by just this type of thing
  flush $fileid
  close $fileid
}

proc append_data {data filename} {
  set fileid [open $filename "a"]
  puts -nonewline $fileid $data
  # I know I shouldn't have to flush before closing, but I've
  # got a weird synchronization problem that seems to be caused
  # by just this type of thing
  flush $fileid
  close $fileid
}


# Start by hiding .

wm withdraw .

# source all the init folder .tcl files.
# They're sourced in sorted order, so prepending a # to the
# beginning of the name helps enforce an order on them.

cd init

global init_error
global init_error_msg

set init_error 0

foreach f [lsort [glob -nocomplain *.tcl]] {if { [catch {source $f} init_error_msg] } {
    append_data "Error during init of $f: $init_error_msg\n" [file join $tcl_env_dir "error.log"]
    set init_error 1
}}


cd ..

# Create the Control Panel here...

toplevel .control_panel

wm title .control_panel "Control Panel"

wm geometry .control_panel [get_configuration .control_panel]

# for now just have a label to report errors.

global global_status

label .control_panel.status -textvariable global_status -text "" -borderwidth 1 -font intro_s_font 
pack .control_panel.status

# Then it needs a "scrollable frame" for the controls since the window is 
# getting too big for small (laptop) displays.  Because of issues with
# including the libraries in the executables I'm not going to use the BWidgets or
# iwidgets extensions and just use a simple hack which is based on code
# found at http://www.tek-tips.com/viewthread.cfm?qid=372792 by 
# Ken Jones of Avia Training and Consulting, www.avia-training.com


# Create a "hull" frame to contain all other widgets 
# composing the scrollable frame.

frame .control_panel.frame
 
canvas .control_panel.frame.c  -height 100 -width 50 -yscrollcommand ".control_panel.frame.ybar set"
scrollbar .control_panel.frame.ybar -orient vertical -command ".control_panel.frame.c yview"
 
# Create the frame that will actually
# hold all children of the scrollable
# frame. All children should be packed
# or gridded into this frame, *not* the
# hull frame or the canvas!

frame .control_panel.frame.c.contents
 
# Tell the canvas to display the frame,
# anchoring the upper-left hand corner
# of the frame in the upper-left hand
# corner of the canvas
 
.control_panel.frame.c create window 0 0 -anchor nw -window .control_panel.frame.c.contents
 
# Use grid to display the canvas and its
# scrollbars. Handle resizing properly.
 
grid .control_panel.frame.c -row 0 -column 0 -sticky nsew -pady 4 -padx 2
grid .control_panel.frame.ybar -row 0 -column 1 -sticky ns -pady 4 -padx 2
grid columnconfigure .control_panel.frame 0 -weight 1
grid rowconfigure .control_panel.frame 0 -weight 1
 
# Detect <Configure> events on the frame
# to detect when it is first displayed
# and any time is is resized. In either
# case, recompute the visible bounds of
# the canvas and update its -scrollregion
# attribute.
   
bind .control_panel.frame.c.contents <Configure> {.control_panel.frame.c configure -scrollregion [.control_panel.frame.c bbox all]}

pack .control_panel.frame -expand yes -fill both

# Now instead of packing the buttons directly on the control panel one has 
# to get the name of the correct parent window with the control_panel_name
# procedure something like this:
# button [control_panel_name].tool ...
# pack [control_panel_name].tool ...

proc control_panel_name {} {return ".control_panel.frame.c.contents"}


# report_status
# Display the provided string (s) on the status label of the control Panel.

#proc report_status {s} {
##  global global_status
#  set global_status "$global_status $s"
#}


# To avoid overwriting on big reports, instead
# dump it to an error log

#file delete [file join $tcl_env_dir "error.log"]

proc report_status {s} {
  global global_status
  global tcl_env_dir
  set global_status "ERROR Logged"
  append_data $s [file join $tcl_env_dir "error.log"]
}




# Will need to expand this later, but for now make sure
# that the socket gets closed when the program ends 

bind .control_panel <Destroy> {
  global time_to_exit

  save_window_positions

  catch {
    global environment_socket
    global connection_socket
    if {$environment_socket != ""} {
      close $environment_socket
    }
  }
  catch {
    if {$connection_socket != ""} {
      close $connection_socket
    }
  }
  
  set_return_result $time_to_exit
}

# For debugging I need access to the console window, so 
# here's how to get it ->  shift-control-L

bind .control_panel <Shift-Control-Key-L> {console show}

wm protocol .control_panel WM_DELETE_WINDOW {
  shut_it_down 0
}

proc shut_it_down {now} {
  global current_open_model 
  global local_connection
  global standalone_mode

  set destroy 1
         
  # there are other ways to test for a closed socket, but trying to
  # send a command seems easy and robust enough...

  if {$now == 0 && $local_connection == 1 && \
      [check_connection] == 0} {
    if $standalone_mode {
      if {$standalone_mode == 2} { # Macs exit this way...
        tk_messageBox -icon warning -title "Open connection" \
                      -message "To quit the environment close the Listener window." -type ok
      } else { # Windows version needs to kill the Lisp explicitly

        set answer [tk_messageBox -icon warning -title "Exit the Environment?" \
                   -message "Closing this window exits the environment. Do you want to quit now?" \
                   -type yesno]

        switch -- $answer {
          yes {
            if {$current_open_model != ""} {
              set answer [tk_messageBox -icon warning -title "Model still open" \
                  -message "There is a model open.  Should the environment \
                            close the model before quitting (all unsaved\
                            changes will be lost if it is not closed)?" \
                  -type yesno]

              switch -- $answer {
                yes {
                  close_model
                }
              }
            }

            # Just force it to go away...

           send_environment_cmd "create simple-handler .control_panel copyrightlab30var (lambda (x) (quit)) nil"

            catch {
              global environment_socket
              global connection_socket

              if {$environment_socket != ""} {
                close $environment_socket
              }
            }
            catch {
              if {$connection_socket != ""} {
                close $connection_socket
              }
            }
    
            exit
          }
        }
      }
    } else {
      tk_messageBox -icon warning -title "Open connection" \
                    -message "You must close the connection from the Lisp side first." -type ok
    }

    set destroy 0
  } elseif {$now == 0} { # I know there should be a better order here, but
                         # but this simple setup isn't too bad for now
    #tk_messageBox -icon warning -title "Closing Environment" \
    #              -message "ACT-R Environment exiting" -type ok
    exit
    
  } elseif {$current_open_model != ""} {
    set answer [tk_messageBox -icon warning -title "Model still open" \
                  -message "There is a model open.  Should the environment \
                            close the model before quitting (all unsaved\
                            changes will be lost if it is not closed)?" \
                  -type yesno]

    switch -- $answer {
      yes {
        close_model
      }
    }
  }

  if $destroy {destroy .control_panel}
}

proc check_connection {} {
  global environment_socket
  return [catch {puts $environment_socket "(k-a)<end>"}] 
}

# These are the procedures for handling the communications.
# There is no handshaking on the commands, because it uses TCP
# sockets which are supposed to do that stuff for you...

# accept_socket_commands
# This procdure gets called everytime there is something to read
# on the socket connected to ACT-R.  The parameter passed is the
# socket.
# It reads lines from the socket until a whole command has been read
# (there is an explicit <end> marker now sent to handle new-lines 
# properly).  Then it passes that command to handle_socket_command.

# Just ignoring the case where there "isn't" something for now...

set last_command ""

proc accept_socket_commands {sock} {
  global last_command
  global time_to_exit
  global standalone_mode
 global up_and_running

  if {[eof $sock]} {

    close $sock

    switch -- $standalone_mode {
      0 { #Not standalone mode so ask if wait or quit
        if {$up_and_running == 1} {
         
         switch [tk_messageBox -title "ACT-R connection lost" -message "The environment has lost contact with ACT-R.  Press retry to return to waiting for a new connection or cancel to quit." \
                            -icon warning -type retrycancel] {
          retry { 
              set time_to_exit 0
              shut_it_down 1
          }
          cancel { 
              shut_it_down 0
          }
        }
       } else {
         tk_messageBox -title "ACT-R connection error" -message "There was a problem starting the environment and it must exit." -icon warning -type ok
         exit}
     
      }
      1 { #windows standalone should ask if restart or quit
        
         if {$up_and_running == 1} {
          switch [tk_messageBox -title "ACT-R connection lost" -message "The environment has lost contact with ACT-R.  Press retry to start a new listener or cancel to quit." \
                            -icon warning -type retrycancel] {
          retry { 
              set time_to_exit 0
              shut_it_down 1
              after 2000 { #wait a little for things to restart
                global top_dir
                cd $top_dir
                if [catch {exec "actr6s-64.exe" -V}] {
                  if [catch {exec "actr6s-32.exe" -V}] {
                    tk_messageBox -icon warning -title "No ACT-R available" \
                      -message "Cannot run the ACT-R application. Contact Dan for help." -type ok
                    exit
                  } else {
                    exec "./run-32.bat" &
                  }
                } else {
                  exec "./run-64.bat" &
                }
              }
          }
          cancel { 
              shut_it_down 0
          }
        }
        } else {
           tk_messageBox -title "ACT-R connection error" -message "There was a problem starting the environment and it must exit." -icon warning -type ok
           exit}
      }  
      2 { #mac standalone should just quit
       tk_messageBox -icon warning -title "ACT-R connection lost" -message "The environment has lost contact with ACT-R and will close now." -type ok
       shut_it_down 0
      }
      default {}
    }
  } elseif {-1 != [gets $sock line]} {
    if [regexp {(.*)<end>$} $line dummy cmd] {
      handle_socket_command "$last_command$cmd"
      set last_command ""
    } else { 
      set last_command "$last_command$line\n"
    }
  } 
}


# handle_socket_command
# This procedure takes one parameter which is a string that represents
# a command sent from Lisp.
# The commands that can be accepted must be of the form:
# {command} {arg1} {rest}
# command must be either update or register 
# arg1 and rest depend on the command, but must be non-empty.
# "bad" commands get reported in the status of the control panel.
    
proc handle_socket_command {data} {
  global time_to_exit

  if [regexp -nocase {^([a-z]+)([ ]+)([^ ]+)([ ]+)(.+)$} \
             $data dummy cmd blank arg1 blank rest] {
    switch $cmd {
      update {
        if [regexp -nocase \
                   {^([^ ]+)[ ]([ ]*)(.+)$} $rest dummy target blank value] {
          switch $arg1 {
            simple {
              global $target
              if {$value == "EMPTY_ENV_STRING"} {
                set $target ""
              } else {
                set $target $value
              }
            }
            special {
              global $target
                set $target $value
            }
            simple_funcall {
             
              set eval_cmd ""
              if {$value == "EMPTY_ENV_STRING"} {
                eval [append eval_cmd $target " " "\"\""]
              } else {
                eval [append eval_cmd $target " " $value]
              }
            }
            text {
              catch {
                if [valid_handler_name $target] {
                  $target configure -state normal
                  $target delete 1.0 end
                  if {$value != "EMPTY_ENV_STRING"} {
                    $target insert 1.0 $blank
                    $target insert end $value
                    $target configure -state disabled
                  }
                }
              }
            }
            simpletext {
              catch {
                if [valid_handler_name $target] {
                  # $target configure -state normal
                  if {$value != "EMPTY_ENV_STRING"} {
                    $target insert end $blank
                   $target insert end $value
                   $target yview -pickplace end
                  }
                  # $target configure -state disabled
                }
              }
            }      
            list_box {
              catch {
                if [valid_handler_name $target] {
                  set selection [$target curselection]
                  global $target.var
                  if {$value == "EMPTY_ENV_STRING"} {
                    set $target.var ""
                    $target selection set 0
                    event generate $target <<ListboxSelect>>
                  } elseif {$selection != ""} {
                    set selected [$target get $selection]
                    set $target.var $value
                    set newpos [lsearch -exact $value $selected]
                    if {$newpos == -1} {
                      $target selection clear 0 end
                      event generate $target <<ListboxSelect>>
                    } else {
                      $target selection clear 0 end
                      $target selection set $newpos
                    }
                  } else {
                    set $target.var $value
                  }
                }
              }    
            }
            select_first_list_box {
              catch {
                if [valid_handler_name $target] {
                  global $target.var
                  if {$value == "EMPTY_ENV_STRING"} {
                    set $target.var ""
                    $target selection set 0
                    event generate $target <<ListboxSelect>>
                  } else {
                    set $target.var $value
                    $target selection clear 0 end
                    $target selection set 0
                    event generate $target <<ListboxSelect>>
                  }
                }
              }    
            }
            env_window {
              catch {
                process_env_window $target $value
              }
            }
            default {
              report_status "unhandled update $data"
            }
          }
        } else {
          report_status "invalid update: $data"
        }
      }
      register {
        set_handler_name $arg1 $rest
      } 
      close {
        set time_to_exit 0
        shut_it_down 1
      }
      ka {
      }
      sync {
        send_environment_cmd "sync"
      }
      default {
        report_status "Invalid command : $data"
      }
    }
  } else {
       report_status "Malformed command: $data"
  }
}  

# send_environment_cmd
# This procedure sends the command (cmd) passed in to the Lisp
# server.  There is no error checking on the command sent.

proc send_environment_cmd {cmd} {
  # don't know how to test if the socket is still active
  # before trying this, perhaps just wrapping it in a catch
  # is good enough, but it doesn't seem to help if the socket goes
  # down.

  global environment_socket
         
  catch {
    puts $environment_socket "($cmd)<end>"
  }
}

# accept_connection
# This function gets called when a socket connects to the listening
# socket. 
# The parameters are the socket for communicating (sock), and the
# address (addr) and port (port) of the remote connection.  Since 
# there is no handshaking the address and port of the remote aren't
# used, but perhaps for a secure server such checks may be necessary.
# The listening socket is closed, because the environment only needs
# one connection.  Then the real communication socket is configured
# so that it doesn't block or buffer and that the accept_socket_commands
# function gets called everytime there is data to read from that socket.

proc accept_connection {sock addr port} {
  global connection_socket
  global environment_socket
  fconfigure $sock -blocking no
  fconfigure $sock -buffering none
  fileevent $sock readable "accept_socket_commands $sock"
  close $connection_socket
  set connection_socket ""
  set environment_socket $sock
}

# environment_socket is the global variable that holds the socket
# connected to Lisp.

set environment_socket ""

# This is where the listening server gets started.
# Just create a server that calls accept_connection when
# a connection is made on the port specified in the net-config file.
# That's all there is to it!

set connection_socket [socket -server accept_connection $tcl_port]


# These procedures are ones that I needed for the declarative viewer
# and it seemed like they'd be generally useful for other dialogs 
# as well so I've included them here.

# updater
# Given a tcl window (w) if that window has a Lisp handler
# associated with it then send a command to that handler to 
# update this window.

proc updater {w} {
  if [valid_handler_name $w] {
    send_environment_cmd "update [get_handler_name $w]"
  }
}


# remove_handler
# Given a tcl window (w) if that window has a Lisp handler
# send the command to remove that handler and remove the 
# entry for this window from the global handler table.
# You can also pass the optional removal function (f)
# which will be sent as part of the remove command.

proc remove_handler {w {f ""}} {
  global handler_names
   
  if [valid_handler_name $w] {
    send_environment_cmd "remove [get_handler_name $w] $f"
    array unset handler_names $w
  }
}

# setchoice
# This function takes 2 parameters, which are a drop_down listbox (win)
# and a flag to indicate whether or not this choice is a real one (valid)
# If valid is true then the global variable associated with this listbox
# is set to the index of the currently selected item and if valid is false
# the global is set to -1.  The global variable is the target of a tkwait, 
# which is why it needs to be set even for a "nonchoice"

proc setchoice {win valid} {
  global $win.choice
     
  if $valid {
    if {[$win curselection] == ""} {
      set $win.choice -1
    } else {
      set $win.choice [$win curselection]
    }
  } else {
    set $win.choice -1
  }
}


# The following is a hack that gets around a race condition
# in an idiom that I was using and don't have a better solution for.
# the race condition was this:
# if {$var == ""} {tkwait variable var}
# where var was going to get set by the socket handlers
# it became a real problem on a 100MHz Pentium rig running Linux
# and maybe it's causing some of my Mac troubles, who knows.
# the hack is to replace that with a polling loop that blocks for
# 100 ms at a crack so as to not grind the processor checking

global hack_ticker

proc set_hack_ticker {} {
  global hack_ticker
  set hack_ticker 1
  after 100 {set_hack_ticker}
}

after 100 {set_hack_ticker}

proc wait_for_non_null {var} {
  global hack_ticker  
  
 # report_status "waiting for $var"

  upvar $var check
  while {$check == ""} {
    tkwait var hack_ticker
  }
  # report_status "-done  "
}

set up_and_running 0

# source all the dialog folder .tcl files.
# They're sourced in sorted order, so prepending a number to the
# beginning of the name helps enforce an order on them.


cd dialogs

foreach f [lsort [glob -nocomplain *.tcl]] {if { [catch {source $f} init_error_msg] } {
    append_data "Error during dialog loading of $f: $init_error_msg\n" [file join $tcl_env_dir "error.log"]
    set global_status "dialog error"
}}


cd ..

set up_and_running 1

# there's got to be a connection at this point since the init dialog requires 
# it, so now we just need to send the keep alive periodically to make sure
# that the socket doesn't timeout (seems to only be a problem in MCL because
# the active sockets don't allow a timeout parameter so they die if inactive
# for more than 30 seconds, but it can't really hurt to do it every where - 
# one small message every 20 seconds seems pretty insignificant)


proc keep_alive {} { 
  send_environment_cmd "k-a"
  after 20000 keep_alive 
}         

# Get things rolling with the keep_alives...

after 20000 {keep_alive}


# Below is a modified version of the tk_dilog command which I've
# named my_tk_dialog.  It uses my text and button fonts to build
# the display.  I've included all the original copyrights and 
# license info along with it (instead of including the license
# file separately).
#
# dialog.tcl --
#
# This file defines the procedure tk_dialog, which creates a dialog
# box containing a bitmap, a message, and one or more buttons.
#
# RCS: @(#) $Id: dialog.tcl,v 1.14.2.3 2006/01/25 18:21:41 dgp Exp $
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# Copyright (c) 1994-1997 Sun Microsystems, Inc.
#
# This software is copyrighted by the Regents of the University of
# California, Sun Microsystems, Inc., Scriptics Corporation, ActiveState
# Corporation and other parties.  The following terms apply to all files
# associated with the software unless explicitly disclaimed in
# individual files.
#
# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.
# 
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# 
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
# 
# GOVERNMENT USE: If you are acquiring this software on behalf of the
# U.S. government, the Government shall have only "Restricted Rights"
# in the software and related documentation as defined in the Federal 
# Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
# are acquiring the software on behalf of the Department of Defense, the
# software shall be classified as "Commercial Computer Software" and the
# Government shall have only "Restricted Rights" as defined in Clause
# 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
# authors grant the U.S. Government and others acting in its behalf
# permission to use and distribute the software in accordance with the
# terms specified in this license. 
#

#
# ::tk_dialog:
#
# This procedure displays a dialog box, waits for a button in the dialog
# to be invoked, then returns the index of the selected button.  If the
# dialog somehow gets destroyed, -1 is returned.
#
# Arguments:
# w -		Window to use for dialog top-level.
# title -	Title to display in dialog's decorative frame.
# text -	Message to display in dialog.
# bitmap -	Bitmap to display in dialog (empty string means none).
# default -	Index of button that is to display the default ring
#		(-1 means none).
# args -	One or more strings to display in buttons across the
#		bottom of the dialog box.

proc ::my_tk_dialog {w title text bitmap default args} {
    global tcl_platform
    variable ::tk::Priv

    # Check that $default was properly given
    if {[string is integer -strict $default]} {
	if {$default >= [llength $args]} {
	    return -code error "default button index greater than number of\
		    buttons specified for tk_dialog"
	}
      # Never call if -strict option is omitted in previous test !
    } elseif {"" eq $default} {
	set default -1
    } else {
	set default [lsearch -exact $args $default]
    }

    # 1. Create the top-level window and divide it into top
    # and bottom parts.

    destroy $w
    toplevel $w -class Dialog
    wm title $w $title
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }

    # Dialog boxes should be transient with respect to their parent,
    # so that they will always stay on top of their parent window.  However,
    # some window managers will create the window as withdrawn if the parent
    # window is withdrawn or iconified.  Combined with the grab we put on the
    # window, this can hang the entire application.  Therefore we only make
    # the dialog transient if the parent is viewable.
    #
    if {[winfo viewable [winfo toplevel [winfo parent $w]]] } {
	wm transient $w [winfo toplevel [winfo parent $w]]
    }

    set windowingsystem [tk windowingsystem]

    if {$tcl_platform(platform) eq "macintosh" || $windowingsystem eq "aqua"} {
	::tk::unsupported::MacWindowStyle style $w dBoxProc
    }

    frame $w.bot
    frame $w.top
    if {$windowingsystem eq "x11"} {
	$w.bot configure -relief raised -bd 1
	$w.top configure -relief raised -bd 1
    }
    pack $w.bot -side bottom -fill both
    pack $w.top -side top -fill both -expand 1

    # 2. Fill the top part with bitmap and message (use the option
    # database for -wraplength and -font so that they can be
    # overridden by the caller).

    option add *Dialog.msg.wrapLength 3i widgetDefault
    if {$tcl_platform(platform) eq "macintosh" || $windowingsystem eq "aqua"} {
	option add *Dialog.msg.font system widgetDefault
    } else {
	option add *Dialog.msg.font {Times 12} widgetDefault
    }

    label $w.msg -justify left -text $text -font label_font
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {$bitmap ne ""} {
	if {($tcl_platform(platform) eq "macintosh"
	     || $windowingsystem eq "aqua") && ($bitmap eq "error")} {
	    set bitmap "stop"
	}
	label $w.bitmap -bitmap $bitmap
	pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $args {
	button $w.button$i -text $but -font button_font -command [list set ::tk::Priv(button) $i]
	if {$i == $default} {
	    $w.button$i configure -default active
	} else {
	    $w.button$i configure -default normal
	}
	grid $w.button$i -in $w.bot -column $i -row 0 -sticky ew \
		-padx 10 -pady 4
	grid columnconfigure $w.bot $i
	# We boost the size of some Mac buttons for l&f
	if {$tcl_platform(platform) eq "macintosh" || $windowingsystem eq "aqua"} {
	    set tmp [string tolower $but]
	    if {$tmp eq "ok" || $tmp eq "cancel"} {
		grid columnconfigure $w.bot $i -minsize [expr {59 + 20}]
	    }
	}
	incr i
    }

    # 4. Create a binding for <Return> on the dialog if there is a
    # default button.

    if {$default >= 0} {
	bind $w <Return> "
	[list $w.button$default] configure -state active -relief sunken
	update idletasks
	after 100
	set ::tk::Priv(button) $default
	"
    }

    # 5. Create a <Destroy> binding for the window that sets the
    # button variable to -1;  this is needed in case something happens
    # that destroys the window, such as its parent window being destroyed.

    bind $w <Destroy> {set ::tk::Priv(button) -1}

    # 6. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set x [expr {[winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]}]
    set y [expr {[winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]}]
    # Make sure that the window is on the screen and set the maximum
    # size of the window is the size of the screen.  That'll let things
    # fail fairly gracefully when very large messages are used. [Bug 827535]
    if {$x < 0} {
	set x 0
    }
    if {$y < 0} {
	set y 0
    }
    wm maxsize $w [winfo screenwidth $w] [winfo screenheight $w]
    wm geometry $w +$x+$y
    wm deiconify $w

    tkwait visibility $w

    # 7. Set a grab and claim the focus too.

    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab ne ""} {
	set grabStatus [grab status $oldGrab]
    }
    grab $w
    if {$default >= 0} {
	focus $w.button$default
    } else {
	focus $w
    }

    # 8. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    vwait ::tk::Priv(button)
    catch {focus $oldFocus}
    catch {
	# It's possible that the window has already been destroyed,
	# hence this "catch".  Delete the Destroy handler so that
	# Priv(button) doesn't get reset by it.

	bind $w <Destroy> {}
	destroy $w
    }
    if {$oldGrab ne ""} {
	if {$grabStatus ne "global"} {
	    grab $oldGrab
	} else {
	    grab -global $oldGrab
	}
    }
    return $Priv(button)
}


