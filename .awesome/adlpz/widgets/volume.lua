local wibox = require("wibox")
local awful = require("awful")
local lain = require("lain")
local beautiful = require("beautiful")
local helpers = require("adlpz.helpers")
local gears = require("gears")

volicon = wibox.widget.imagebox(beautiful.widget_vol)
volwidget = wibox.widget.textbox()

function set_volume_text(volume)
    volwidget:set_text(volume .. "% ")
end

function volume_up ()
    helpers.run_command("~/tools/volumecontrol/volumecontrol.py volume-up", set_volume_text)
end

function volume_down ()
    helpers.run_command("~/tools/volumecontrol/volumecontrol.py volume-down", set_volume_text)
end

function refresh_volume ()
    helpers.run_command("~/tools/volumecontrol/volumecontrol.py show-volume", set_volume_text)
end

volwidget:buttons(
   awful.util.table.join(
   awful.button({}, 1, function()
       awful.util.spawn("volumecontrol")
   end),
   awful.button({}, 3, function ()
       awful.util.spawn("pavucontrol")
   end),
   awful.button({}, 4, function()
       volume_up()
   end),
   awful.button({}, 5, function()
       volume_down()
   end)
))

refresh_volume()
gears.timer.start_new(5, function() 
    refresh_volume()
    return true
end)

return {widget=volwidget,icon=volicon}
