local awful   = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local w_keymap = require("adlpz.widgets.keymap")
local gears = require("gears")
local menubar = require("menubar")
local w_backlight = require("adlpz.widgets.backlight")
local w_volume = require("adlpz.widgets.volume")
menubar.utils.terminal = terminal

globalkeys = {}
clientkeys = {}

local function addkeyfn(map)
    return function(mods, key, fn, description, group)
        local key = awful.key(mods, key, fn, {description=description, group=group})
        for k,v in pairs(key) do
            table.insert(map, v)
        end
    end
end

local globalkey = addkeyfn(globalkeys)
local clientkey = addkeyfn(clientkeys)

-- {{{ GLOBAL KEY BINDINGS }}}

-- Move through Tags

globalkey({ modkey, "shift" }, "Left", awful.tag.viewprev, "Previous tag", "tag")
globalkey({ modkey, "shift" }, "Right", awful.tag.viewnext, "Next tag", "tag")
globalkey({ modkey, "shift" }, "Escape", awful.tag.history.restore, "Last used tag", "tag")

-- Move through Clients

globalkey({ modkey }, "h", function() awful.client.focus.bydirection("left") end, "Left client", "client")
globalkey({ modkey }, "j", function() awful.client.focus.bydirection("down") end, "Down client", "client")
globalkey({ modkey }, "k", function() awful.client.focus.bydirection("up") end, "Up client", "client")
globalkey({ modkey }, "l", function() awful.client.focus.bydirection("right") end, "Right client", "client")

globalkey({ modkey }, "Tab", function() awful.spawn("rofi -show window -kb-row-down 'Super_L+Tab' -kb-accept-entry '!Super_L+Tab,Return'") end, "Cycle client", "client")

globalkey({ modkey }, "u", awful.client.urgent.jumpto, "Urgent client", "client")

-- Move through screens

globalkey({ modkey }, "Down", function() awful.screen.focus_bydirection("right") end, "Right screen", "screen")
globalkey({ modkey }, "Up", function() awful.screen.focus_bydirection("left") end, "Right screen", "screen")

-- Layout manipulation

globalkey({ modkey, "Control" }, "l", function() awful.tag.incmwfact(0.05) end, "Increase width factor", "layout")
globalkey({ modkey, "Control" }, "h", function() awful.tag.incmwfact(-0.05) end, "Decrease width factor", "layout")

globalkey({ modkey, "Control" }, "k", function() awful.tag.incnmaster(1, nil, true) end, "More master clients", "layout")
globalkey({ modkey, "Control" }, "j", function() awful.tag.incnmaster(-1, nil, true) end, "Fewer master clients", "layout")

globalkey({ modkey, "Shift" }, "k", function() awful.tag.incncol(1, nil, true) end, "More columns", "layout")
globalkey({ modkey, "Shift" }, "j", function() awful.tag.incncol(-1, nil, true) end, "Fewer columns", "layout")

globalkey({ modkey }, "space", function() awful.layout.inc(1) end, "Next layout", "layout")

globalkey({ modkey }, "o", function() awful.client.movetoscreen() end, "Move client to next screen", "layout")

-- Screen manipulation

globalkey({ modkey, "Control" }, "p", function() awful.util.spawn_with_shell("~/scripts/commands/switch_screen_layout.sh") end, "Screen layout", "screen")

-- Applications and Menus

globalkey({ modkey }, "Return", function() awful.spawn(terminal) end, "Terminal", "applications")

-- Prompt and Launcher

globalkey({ modkey }, "r", function () awful.screen.focused().mypromptbox:run() end, "Run", "launcher")
globalkey({ modkey }, "x", function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end, "Execute Lua", "launcher")
globalkey({ modkey }, "p", function() awful.spawn("rofi -combi-modi drun,run,ssh -show combi") end, "Menu Bar", "launcher")

-- Control AwesomeWM

globalkey({ modkey }, "s", hotkeys_popup.show_help, "Help", "awesome")
globalkey({ modkey, "Control" }, "r", awesome.restart, "Restart", "awesome")
globalkey({ modkey, "Control" }, "q", awesome.quit, "Quit", "awesome")

-- System

globalkey({ modkey, altkey }, "space", function() w_keymap.next_kb() end, "Next Keymap", "system")
globalkey({ modkey, "Control" }, "v", function() awful.spawn("volumecontrol") end, "Volume Control UI", "system")
globalkey({ modkey, "Control" }, "b", function() awful.util.spawn_with_shell(terminal .. " -e bluetoothctl") end, "Bluetooth Control UI", "system")
globalkey({ modkey, "Control" }, "s", function() awful.spawn("keeweb") end, "KeePass UI", "system")
globalkey({ modkey, "Control" }, "f", function() awful.spawn("nemo") end, "NEMO File Manager", "system")

-- Media Keys

globalkey({}, "#233", function() w_backlight.change("+60") end, "Brightness Up", "media") -- Brightness UP
globalkey({}, "#232", function() w_backlight.change("-60") end, "Brightness Down", "media") -- Brightness DOWN
globalkey({"Shift"}, "#233", function() w_backlight.change("+10") end, "Brightness Up", "media") -- Brightness UP
globalkey({"Shift"}, "#232", function() w_backlight.change("-10") end, "Brightness Down", "media") -- Brightness DOWN

globalkey({}, "#121", w_volume.mute, "Mute", "media") -- Mute
globalkey({}, "#122", w_volume.down, "Volume Down", "media") -- Vol Down
globalkey({}, "#123", w_volume.up, "Volume Up", "media") -- Vol Up

-- {{{ CLIENT KEY BINDINGS }}}

clientkey({ modkey }, "f", function(c) c.fullscreen = not c.fullscreen; c:raise() end, "Fullscreen", "client")
clientkey({ modkey }, "d", function(c) c.floating = not c.floating  end, "Detach (Float)", "client")
clientkey({ modkey }, "m", function(c) c.maximized = not c.maximized end, "Maximized", "client")
clientkey({ modkey }, "n", function(c) c.minimized = true end, "Minimize", "client")
clientkey({ modkey }, "q", function(c) c:kill() end, "Close", "client")
clientkey({ modkey }, "c", function (c)
        c.maximized = false
        c.maximized_horizontal = false
        c.maximized_vertical = false
        awful.client.floating.set(c, false)
      end, "Clear modifiers", "client")

-- {{{ TAG AUTOMATIC BINDINGS }}}

local tag_keys = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "="}
for i = 1, 12 do
    local key = tag_keys[i];
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, key,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag "..key, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, key,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag " .. key, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, key,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag "..key, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, key,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag " .. key, group = "tag"})
    )
end

-- {{{ MOUSE BINDINGS }}}

-- Client buttons. This is used later on rules.lua
clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- {{{ APPLY }}}

root.keys(globalkeys)
