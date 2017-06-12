local wibox = require("wibox")
local awful = require("awful")
local lain = require("lain")
local beautiful = require("beautiful")

volicon = wibox.widget.imagebox(beautiful.widget_vol)
function change_volume (step)
    awful.util.spawn_with_shell("amixer sset DAC0 " .. step)
    volumewidget:update()
end
volicon:buttons(
   awful.util.table.join(
      awful.button({}, 1, function ()
            awful.util.spawn_with_shell("pavucontrol")
   end),
   awful.button({}, 4, function()
         change_volume("1+")
   end),
   awful.button({}, 5, function()
         change_volume("1-")
   end)
))
volumewidget = lain.widget.alsa({
    channel = "DAC0",
    settings = function()
        if volume_now.status == "off" then
            volicon:set_image(beautiful.widget_vol_mute)
        elseif tonumber(volume_now.level) == 0 then
            volicon:set_image(beautiful.widget_vol_no)
        elseif tonumber(volume_now.level) <= 50 then
            volicon:set_image(beautiful.widget_vol_low)
        else
            volicon:set_image(beautiful.widget_vol)
        end

        widget:set_text(" " .. volume_now.level .. "% ")
        widget:buttons(
            awful.util.table.join(
            awful.button({}, 1, function ()
                awful.util.spawn_with_shell("pavucontrol")
            end),
            awful.button({}, 4, function()
                change_volume("1+")
            end),
            awful.button({}, 5, function()
                change_volume("1-")
           end)
         ))
    end
})

return {widget=volumewidget,icon=volicon}
