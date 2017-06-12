local lain = require("lain")
local awful = require("awful")
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local beautiful = require("beautiful")

clockicon = wibox.widget.imagebox(beautiful.widget_clock)
mytextclock = awful.widget.watch(
  "bash -c \"LANG=ca_ES.UTF-8 date +'%a%d/%H%M'\"",
  27,
  function(widget, output)
    widget:set_text(output)
  end
)
-- calendar
lain.widget.calendar({
    attach_to = mytextclock,
    notification_preset = {
      font = "Consolas 12"
    },
    cal = "LANG=ca_ES.UTF-8 /usr/bin/cal"
})


return {widget=mytextclock,icon=clockicon}
