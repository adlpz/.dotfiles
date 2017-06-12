
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")
local gears = require("gears")
beautiful.init(gears.filesystem.get_dir("config") .. "themes/adlpz/theme.lua")

function batreader (value)
  local handle = io.popen("~/scripts/power.sh " .. value)
  local result = handle:read("*a")
  handle:close()
  return result:gsub("^%s*(.-)%s*$", "%1")
end
function update_batwidget ()
  local charge = batreader("charge")
  charge = tonumber(charge)
  if (charge >= 100) then
    charge = string.format("%5.1f", charge) .. "%"
  else
    charge = string.format("%5.2f", charge) .. "%"
  end
  local power = batreader("power")
  power = string.format("%05.2f", tonumber(power))
  local status = batreader("status")
  local sign = (status == "Discharging") and "-" or "+"
  local string = charge .. " " .. sign ..  power .. "W "
  batwidget:set_text(string)
end

baticon = wibox.widget.imagebox(beautiful.widget_battery)
batwidget = wibox.widget.textbox()
battimer = gears.timer({ timeout = 5 })
battimer:connect_signal("timeout", update_batwidget)
battimer:start()
update_batwidget()

helpers.add_hover_notification(batwidget, "acpi -bt")

return {widget=batwidget,icon=baticon}
