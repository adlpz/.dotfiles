
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")
local awful = require("awful")

cafficon = wibox.widget.imagebox(beautiful.widget_caffeine)
caffwidget = wibox.widget.textbox()

function format_caffeine (value)
  if tonumber(value) >= 3200 then
    return "<span color='#ff0000' font_weight='bold'>" .. value .. "</span>"
  end
  return "<b>" .. value .. "</b>"
end

function set_current_caffeine ()
  helpers.run_command(
    "~/scripts/caffeine.sh current",
    function(output)
      caffwidget:set_markup(format_caffeine(output))
    end
  )
end

function next_caffeine ()
  helpers.run_command("~/scripts/caffeine.sh next")
  set_current_caffeine()
end

set_current_caffeine()

caffwidget:buttons(awful.util.table.join(
                     awful.button({}, 1, function() next_caffeine() end)
))

return {widget=caffwidget,icon=cafficon}
