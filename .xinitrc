
# Custom XINITRC

# Tell QT which GTK+ Config
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Remap CapsLock to Control
xmodmap -e "clear Lock"
xmodmap -e "keycode 66 = Control_R"
xmodmap -e "add Control = Control_R"
# Right Control (Caps Lock) acts as Escape on single press
# AUR: xcape-git
xcape -e "Control_R=Escape"

# Synaptics Touchpad: disable when typing
syndaemon -i 1 -t -k -d

# Execute WM
exec awesome -c ~/.awesome/rc.lua
