
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")
local awful = require("awful")

blicon = wibox.widget.imagebox(beautiful.widget_backlight)
blwidget = wibox.widget.textbox()
function change_backlight (step)
  if step then
    os.execute("~/scripts/brightness.sh " .. step)
  end
  local handle = io.popen("~/scripts/brightness.sh")
  local result = handle:read("*a"):gsub("^%s*(.-)%s*$", "%1")
  handle:close()
  blwidget:set_text(" " .. result .. "% ")
end
change_backlight()
blwidget:buttons(awful.util.table.join(
                   awful.button({}, 4, function() change_backlight("+20") end),
                   awful.button({}, 5, function() change_backlight("-20") end)
))

return {widget=blwidget,icon=blicon}
