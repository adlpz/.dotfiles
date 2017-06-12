
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")

cpuicon = wibox.widget.imagebox(beautiful.widget_cpu)
cpuwidget = lain.widget.cpu({
    settings = function()
      widget:set_text(" " .. string.format("%02d", cpu_now.usage) .. "% ")
    end
})

helpers.add_hover_notification(cpuwidget.widget, "~/scripts/top.py cpu")


return {widget=cpuwidget,icon=cpuicon}
