
local helpers = require("adlpz.helpers")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")
local awful = require("awful")
local naughty = require("naughty")

kbicon = wibox.widget.imagebox(beautiful.widget_keymap)
kbwidget = wibox.widget.textbox()

function notify_keymap_change(keymap)
  naughty.notify({text = "Switched to keymap " .. keymap})
end

function set_keymap_text(keymap)
  kbwidget:set_text(keymap .. " ")
end

function next_kb ()
  helpers.run_command(
    "sleep 1 && ~/scripts/keymaps.py next",
    function(stdout)
      notify_keymap_change(stdout)
      set_keymap_text(stdout)
    end
  )
end

-- On start, se the text async
helpers.run_command(
  "sleep 1 && ~/scripts/keymaps.py current",
  function(stdout)
    set_keymap_text(stdout)
  end
)

-- Change keymap on click
kbwidget:buttons(awful.util.table.join(
                   awful.button({}, 1, function()
                       next_kb()
end)))

return {widget=kbwidget,icon=kbicon,next_kb=next_kb}
