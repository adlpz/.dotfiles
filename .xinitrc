
# Custom XINITRC
#xsetroot -cursor_name left_ptr

# Set keymap to spanish
setxkbmap es
# Set desktop background (not needed anymore, defined in the theme)
WALLPAPER="Images/linuxwallpaper42.png"
awsetbg -u feh -f $WALLPAPER

# Tell QT which GTK+ Config
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Execute WM
exec awesome -c ~/.config/awesome/rc.lua 
#exec startkde
