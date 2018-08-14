
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

function get_baticon_image (sign, charge)
    if (sign == "+") then
        return beautiful.widget_battery_charging
    end

    if charge >= 90 then
        return beautiful.widget_battery_full
    end

    if charge >= 60 then
        return beautiful.widget_battery
    end

    if charge >= 20 then
        return beautiful.widget_battery_low
    end

    return beautiful.widget_battery_critical
end

function update_batwidget ()
  local charge = batreader("charge")
  charge = tonumber(charge)
  local charge_str = ""
  if (charge >= 100) then
    charge_str = string.format("%5.1f", charge) .. "%"
  else
    charge_str = string.format("%5.2f", charge) .. "%"
  end
  local power = batreader("power")
  power = string.format("%05.2f", tonumber(power))
  local status = batreader("status")
  local sign = (status == "Discharging") and "-" or "+"
  local string = charge_str .. " " .. sign ..  power .. "W "
  batwidget:set_text(string)
  local new_image = get_baticon_image(sign, charge)
  if (new_image ~= baticon_image) then
      baticon_image = new_image
      baticon.image = baticon_image
  end
end

baticon_image = beautiful.widget_battery
baticon = wibox.widget.imagebox(baticon_image)
batwidget = wibox.widget.textbox()
battimer = gears.timer({ timeout = 5 })
battimer:connect_signal("timeout", update_batwidget)
battimer:start()
update_batwidget()

helpers.add_hover_notification(batwidget, "acpi -bt")

return {widget=batwidget,icon=baticon}
