#!/usr/bin/env bash

# only start process if it's not already running

function run {
  if  ! pgrep -f $1 ;
  then
    $@&
  fi
}

# enviroment varialbes
# let gtk and qt handle fcitx compose key. else it breaks
export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE="fcitx"
export QT_IM_MODULE="fcitx"

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
xset s 60

#dwm statusbar 
#run dwmblocks &

#startup programs
# web browser
run firefox &
#mail
#run thunderbird &

#statusbar applets
run nm-applet &
run redshift-gtk &

## services and daemons

#start "Hawck-inputd" on this specific keyboard
# this is a daemon macro scripting language, I use it for keyrebinding, as it reads from /dev/input, so it works for wayland,xorg and console!
#run hawck-inputd --kbd-device /dev/input/by-path/platform-i8042-serio-0-event-kbd 
#run hawck-macrod

#set option of swapping escape and capslog, usefull when using vim, and can be disabled by passing having only "setxkbmap -option"
# first option lets me swap layout with shift + alt
# exec setxkbmap -option caps:swapescape  
# executing them stops the script
 #run setxkbmap -layout "dk,us" -option "grp:alt_shift_toggle" -option caps:swapescape &

# daemons letting program run in background
run thunar --daemon 


#emacs started as a systemd service, the file is in ~/.config
run emacs --daemon 


# kmonad ~/.config/kmonad/config.kbd
