#!/usr/bin/env sh

# only start process if it's not already running

# this is a bashism, so it won't work on dash/sh,
# "pgrep" gives the PID of a  process, -f means list name instead of PID,
# "$1" is 1.st argument starting from 0
#
# function run {
#   if  ! pgrep -f $1 ;
#   then
#     $@&
#   fi
# }

#run() in posix shell
run () {
  if  [  "" = "$(pgrep -f $1))" ]
  then $@&
  fi

}
#
# enviroment varialbes
# let gtk and qt handle fcitx compose key. else it breaks
export XMODIFIERS=@im="ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"

SE=$(loginctl show-session $(awk '/tty/ {print $1}' <(loginctl)) -p Type | awk -F= '{print $2}')
if [ "$SE" = "wayland" ]; then
# wayland statusbar
 sh -c waybar
else
  #get id  number for touchpad
  ID=$(xinput list | grep Touch | fold -w 8 | grep id | sed "s/id=//")
  #bruger ID som nummer for device touchpad
  xinput set-prop ${ID} 308 1

  #also set touchpad sensetivity.
  xinput --set-prop ${ID} 'libinput Accel Speed' 0.25
  #disable mouse accelaration
  xinput --set-prop ${ID}  "Coordinate Transformation Matrix" 2.4 0 0 0 2.4 0 0 0 1

  #java does not work in dwm because it is non-parenting, this does something that  this fixes it(found on arch wiki)
  #export AWT_TOOLKIT=MToolkit

  #DPMS, screen blankout time, to 60 seconds or 1 minute
  # WORKS ONLY FOR X11, YOU HAVE TO USE A DIFFERENT UTILITY/PROGRAM ON WAYLAND
  xset s 120
#dwm statusbar
fi

#get id  number for touchpad
#######################################################################################################################
# ID=$(xinput list | grep Touch | fold -w 8 | grep id | sed "s/id=//")                                                #
# #bruger ID som nummer for device touchpad                                                                           #
# xinput set-prop ${ID} 308 1                                                                                         #
#                                                                                                                     #
# #also set touchpad sensetivity.                                                                                     #
# xinput --set-prop ${ID} 'libinput Accel Speed' 0.25                                                                 #
# #disable mouse accelaration                                                                                         #
# xinput --set-prop ${ID}  "Coordinate Transformation Matrix" 2.4 0 0 0 2.4 0 0 0 1                                   #
#                                                                                                                     #
# #java does not work in dwm because it is non-parenting, this does something that  this fixes it(found on arch wiki) #
# #export AWT_TOOLKIT=MToolkit                                                                                        #
#                                                                                                                     #
# #DPMS, screen blankout time, to 60 seconds or 1 minute                                                              #
# # WORKS ONLY FOR X11, YOU HAVE TO USE A DIFFERENT UTILITY/PROGRAM ON WAYLAND                                        #
# xset s 120                                                                                                          #
#######################################################################################################################



#####################
# statusbar applets #
#####################

run nm-applet
run redshift-gtk 
run indicator-cpufreq




# startup programs
	# web browser
	run firefox
	#mail
	#run thunderbird &
# syncronises files between pc's decentralised, privately and securely this is better (in my opinion) than google drive and nextcloud as it's very simple to set up.but it kills battery.... normally i have [6-8]*45min battery, but with this  it barely had 4*45 min.... it takes a 1/3 battery... pathetic
# run syncthing --no-browser

########################
# services and daemons #
########################

#set option of swapping escape and capslog, usefull when using vim, and can be disabled by passing having only "setxkbmap -option"
# first option lets me swap layout with shift + alt
# exec setxkbmap -option caps:swapescape
# executing them stops the script
 #run setxkbmap -layout "dk,us" -option "grp:alt_shift_toggle" -option caps:swapescape &

# daemons letting program run in background
run thunar --daemon
#emacs started as a service, this speeds up emacs startup by alot much
run emacs --daemon 
# key rebinder and some macros
run kmonad ~/.config/kmonad/config.kbd


