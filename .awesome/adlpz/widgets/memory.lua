
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")

-- MEM
memicon = wibox.widget.imagebox(beautiful.widget_mem)
memwidget = lain.widget.mem({
    settings = function()
      local text = string.format("%4.2f", mem_now.used/1024)
      widget:set_text(" " .. text .. "GB ")
    end
})

helpers.add_hover_notification(memwidget.widget, "~/scripts/top.py mem")

return {widget=memwidget, icon=memicon}
