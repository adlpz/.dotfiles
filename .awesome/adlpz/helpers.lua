--
-- Utilities for Awesome WM
-- by @adlpz <adria@prealfa.com>
--

local naughty = require("naughty")
local awful = require("awful")
local gears = require("gears")
local object = require("gears.object")
local beautiful = require("beautiful")
local socket = require("socket")

function print_r(arr, indentLevel)
  local str = ""
  local indentStr = "#"

  if(indentLevel == nil) then
    print(print_r(arr, 0))
    return
  end

  for i = 0, indentLevel do
    indentStr = indentStr.."\t"
  end

  for index,value in pairs(arr) do
    if type(value) == "table" then
      str = str..indentStr..index..": \n"..print_r(value, (indentLevel + 1))
    else 
      str = str..indentStr..index..": "..value.."\n"
    end
  end
  return str
end

function run_command(command, callback)
  awful.spawn.easy_async(
    "bash -c '" .. command .. "'",
    function(stdout, stderr)
        local trimmed = stderr:gsub("^%s*(.-)%s*$", "%1")
        if trimmed ~= "" then
          naughty.notify({text = "Error: " .. trimmed})
          return
        end
        if callback then
          callback(stdout:gsub("^%s*(.-)%s*$", "%1"))
        end
    end
  )
end

function add_hover_notification(widget, command)
  local show = function(self)
    if self._private.visible then
      return
    end

    self._private.visible = true

    run_command(
      command,
      function(output)
        if not self._private.visible then
          -- Somehow the input left before the async call finished
          return
        end
        self._private.notification = naughty.notify({
            text = output,
            font = "Consolas 13"
        })
      end
    )
  end

  local hide = function(self)
    if not self._private.visible then
      return
    end

    naughty.destroy(self._private.notification)
    self._private.notification = nil
    self._private.visible = false;
  end

  local self = object {
    enable_properties = true
  }

  rawset(self, "_private", {})

  self._private.visible = false
  self._private.notification = nil

  widget:connect_signal('mouse::enter', function() show(self) end)
  widget:connect_signal('mouse::leave', function() hide(self) end)
end

function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

function get_hostname()
    return socket.dns.gethostname()
end

local hostname = get_hostname()
local is_traveler = hostname == "traveler"
local is_barbacode = hostname == "barbacode.prealfa.com"

function add_to_table(t, elements)
	for _, v in pairs(elements) do
		table.insert(t, v)
	end
end

local utilities = {}
utilities.print_r = print_r
utilities.run_command = run_command
utilities.add_hover_notification = add_hover_notification
utilities.set_wallpaper = set_wallpaper
utilities.hostname = hostname
utilities.is_traveler = is_traveler
utilities.is_barbacode = is_barbacode
utilities.add_to_table = add_to_table

return utilities
