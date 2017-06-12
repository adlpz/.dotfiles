
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")

fsicon = wibox.widget.imagebox(beautiful.widget_hdd)
fswidget = lain.widget.fs({
    settings  = function()
      widget:set_text(" " .. fs_now.used .. "% ")
    end,
    notification_preset = {
      font = "Consolas 13"
    }
})

return {widget=fswidget,icon=fsicon}
