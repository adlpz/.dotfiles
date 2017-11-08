
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")
local awful = require("awful")
local gears = require("gears")

markup = lain.util.markup

terminal = "urxvt"
iptraf = terminal .. " -g 180x54-20+34 -e sudo iptraf-ng -i all "


neticon = wibox.widget.imagebox(beautiful.widget_net)
network = "LOADING"
netwidget = lain.widget.net({
    settings = function()
      widget:set_markup(network ..
                          markup("#7AC82E", " " .. string.format("%6.1f", net_now.received)) .. 
                          " " ..
                          markup("#46A8C3", " " .. string.format("%6.1f", net_now.sent) .. " "))
    end
})

gears.timer {
  timeout = 5,
  autostart = true,
  callback = function()
    helpers.run_command(
      'iw dev wlp2s0 link | grep SSID | cut -d" " -f2-',
      function(output)
        network = output:gsub("%&", "")
      end
    )
  end
}

netwidget.widget:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.util.spawn_with_shell(terminal .. " -e nmtui") end),
                           awful.button({ }, 2, function () awful.util.spawn_with_shell(iptraf) end)
))

return {widget=netwidget,icon=neticon}
