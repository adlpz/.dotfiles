--
-- My Awesome WM 4.1 Configuration
-- by @adlpz <adria@prealfa.com>
--

-----------------------------------------------------------------------------
-- Awesome Libraries
-----------------------------------------------------------------------------

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
-- Enable VIM help for hotkeys widget when client with matching name is opened:
require("awful.hotkeys_popup.keys.vim")

-----------------------------------------------------------------------------
-- Theme
-----------------------------------------------------------------------------

beautiful.init(gears.filesystem.get_dir("config") .. "themes/adlpz/theme.lua")

-----------------------------------------------------------------------------
-- Custom Widgets
-----------------------------------------------------------------------------

local w_memory = require("adlpz.widgets.memory")
local w_backlight = require("adlpz.widgets.backlight")
local w_battery = require("adlpz.widgets.battery")
local w_caffeine = require("adlpz.widgets.caffeine")
local w_clock = require("adlpz.widgets.clock")
local w_cpu = require("adlpz.widgets.cpu")
local w_fs = require("adlpz.widgets.fs")
local w_keymap = require("adlpz.widgets.keymap")
local w_net = require("adlpz.widgets.net")
local w_volume = require("adlpz.widgets.volume")

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------

local helpers = require("adlpz.helpers")

-----------------------------------------------------------------------------
-- Error Handling
-----------------------------------------------------------------------------

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end

-----------------------------------------------------------------------------
-- Global Variables
-----------------------------------------------------------------------------

terminal = "termite"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

modkey = "Mod4"
altkey = "Mod1"

-----------------------------------------------------------------------------
-- Layouts
-----------------------------------------------------------------------------

awful.layout.layouts = {
  awful.layout.suit.tile,
  awful.layout.suit.fair,
  awful.layout.suit.floating,
  -- awful.layout.suit.tile.left,
  -- awful.layout.suit.tile.bottom,
  -- awful.layout.suit.tile.top,
  -- awful.layout.suit.fair.horizontal,
  -- awful.layout.suit.spiral,
  -- awful.layout.suit.spiral.dwindle,
  -- awful.layout.suit.max,
  -- awful.layout.suit.max.fullscreen,
  -- awful.layout.suit.magnifier,
  -- awful.layout.suit.corner.nw,
  -- awful.layout.suit.corner.ne,
  -- awful.layout.suit.corner.sw,
  -- awful.layout.suit.corner.se,
}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-----------------------------------------------------------------------------
-- Menu and Menu Icon (top-left corner)
-----------------------------------------------------------------------------

screensmenu = {
  {"single", function() helpers.run_command("~/.screenlayout/single.sh") end},
  {"oficina", function() helpers.run_command("~/.screenlayout/oficina-2-h.sh") end}
}

tasksmenu = {
  {"kill spotify", function() helpers.run_command("killall -TERM spotify") end},
  {"bluetooth", terminal .. " -e bluetoothctl"}
}

awesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

mainmenu = awful.menu({ items = {
                            {"screen", screensmenu},
                            {"tasks", tasksmenu},
                            { "awesome", awesomemenu}}})

mylauncher = awful.widget.button({ image = beautiful.awesome_icon })
mylauncher:buttons(gears.table.join(
                     awful.button({}, 1, nil, function() mainmenu:toggle() end),
                     awful.button({}, 3, nil, function() helpers.run_command("~/scripts/lock.sh") end)))

-----------------------------------------------------------------------------
-- Tag List Bindings
-----------------------------------------------------------------------------

local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end))

-----------------------------------------------------------------------------
-- Task List Bindings
-----------------------------------------------------------------------------

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()))



-----------------------------------------------------------------------------
-- Generate Screens
-----------------------------------------------------------------------------

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1 W", "2 C1", "3 C2", "4 C3", "5 T1", "6 T2", "7 T3", "8", "9", "0", "Comm", "Music" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(),
            w_net.icon,
            w_net.widget,
            w_memory.icon,
            w_memory.widget,
            w_cpu.icon,
            w_cpu.widget,
            -- The FS widget fails due to a bug in Lain (lain/widget/fs.lua)
            -- w_fs.icon,
            -- w_fs.widget,
            w_battery.icon,
            w_battery.widget,
            w_volume.icon,
            w_volume.widget,
            w_backlight.icon,
            w_backlight.widget,
            w_caffeine.icon,
            w_caffeine.widget,
            w_keymap.icon,
            w_keymap.widget,
            w_clock.icon,
            w_clock.widget,
            wibox.widget.textbox(" "),
            s.mylayoutbox,
            mylauncher,
        },
    }
end)

-----------------------------------------------------------------------------
-- Mouse and Keyboard bindings
-----------------------------------------------------------------------------

require("adlpz.bindings")

-----------------------------------------------------------------------------
-- Rules
-----------------------------------------------------------------------------

require('adlpz.rules')

-----------------------------------------------------------------------------
-- Signals
-----------------------------------------------------------------------------

require('adlpz.signals')
