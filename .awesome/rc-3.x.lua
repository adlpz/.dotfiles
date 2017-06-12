--
-- My awesome config
-- by @adlpz <adria@prealfa.com>
--

local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
              require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local drop = require("scratchdrop")
local lain = require("lain")
local menubar = require("menubar")

-- Utilities

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

-- Helper Functions

function run_command (command)
    local handle = io.popen(command)
    local text = handle:read("*a"):gsub("^%s*(.-)%s*$", "%1")
    handle:close()
    return text
end

function add_hover_notification(widget, command)
    local notification = nil
    local hide = function()
        if notification ~= nil then
            naughty.destroy(notification)
            notification = nil
        end
    end
    local show = function()
        if notification ~= nil then
            return
        end
        local text = run_command(command)
        notification = naughty.notify({
            text = text,
            font = "Consolas 13"
        })
    end

    widget:connect_signal('mouse::enter', function() show() end)
    widget:connect_signal('mouse::leave', function() hide() end)
end

-- common
modkey     = "Mod4"
altkey     = "Mod1"
terminal   = "urxvt"
editor     = "~/tools/em"

-- user defined
browser    = "chromium"
gui_editor = editor
graphics   = "gimp"
mail       = terminal .. " -e mutt "
iptraf     = terminal .. " -g 180x54-20+34 -e sudo iptraf-ng -i all "
musicplr   = terminal .. " -g 130x34-320+16 -e ncmpcpp "

beautiful.init(os.getenv("HOME") .. "/.awesome/themes/powerarrow-darker/theme.lua")

local layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.fair,
    awful.layout.suit.floating,
}
-- }}}

-- {{{ Tags
tags = {
   names = { "1 Web", "2 Code", "3 T1", "4 T2", "5 Comm", "6", "7", "8", "9 Music" },
   layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] , layouts[1], layouts[1], layouts[1] }
}

for s = 1, screen.count() do
   tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Wibox
markup = lain.util.markup

-- Textclock
clockicon = wibox.widget.imagebox(beautiful.widget_clock)

mytextclock = lain.widgets.abase({
    timeout  = 27,
    cmd      = "LANG=ca_ES.UTF-8 date +'%a%d/%H%M'",
    settings = function()
        widget:set_text(" " .. output)
    end
})

-- calendar
lain.widgets.calendar:attach(mytextclock, {
    font = "Consolas", 
    font_size = 12,
    cal = "LANG=ca_ES.UTF-8 /usr/bin/cal"
})

-- MPD
mpdicon = wibox.widget.imagebox(beautiful.widget_music)
mpdicon:buttons(awful.util.table.join(awful.button({ }, 1, function () awful.util.spawn_with_shell(musicplr) end)))
mpdwidget = lain.widgets.mpd({
    settings = function()
        if mpd_now.state == "play" then
            artist = " " .. mpd_now.artist .. " "
            title  = mpd_now.title  .. " "
            mpdicon:set_image(beautiful.widget_music_on)
        elseif mpd_now.state == "pause" then
            artist = " mpd "
            title  = "paused "
        else
            artist = ""
            title  = ""
            mpdicon:set_image(beautiful.widget_music)
        end

        widget:set_markup(markup("#EA6F81", artist) .. title)
    end
})

-- MEM
memicon = wibox.widget.imagebox(beautiful.widget_mem)
memwidget = lain.widgets.mem({
      settings = function()
         local text = string.format("%4.2f", mem_now.used/1024)
        widget:set_text(" " .. text .. "GB ")
    end
})

add_hover_notification(memwidget, "~/scripts/top.py mem")

-- CPU
cpuicon = wibox.widget.imagebox(beautiful.widget_cpu)
cpuwidget = lain.widgets.cpu({
    settings = function()
       widget:set_text(" " .. string.format("%02d", cpu_now.usage) .. "% ")
    end
})

add_hover_notification(cpuwidget, "~/scripts/top.py cpu")

-- Battery

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
battimer = timer({ timeout = 5 })
battimer:connect_signal("timeout", update_batwidget)
battimer:start()
update_batwidget()


-- Backlight

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

-- Keyboard

kbwidget = wibox.widget.textbox()
function next_kb ()
    local handle = io.popen("~/scripts/keymaps.py next")
    local current = handle:read("*a"):gsub("^%s*(.-)%s*$", "%1")
    handle:close()
    kbwidget:set_text(" " .. current .. " ")
end
function current_kb ()
    local handle = io.popen("~/scripts/keymaps.py current")
    local current = handle:read("*a"):gsub("^%s*(.-)%s*$", "%1")
    handle:close()
    return current
end
kbwidget:set_text(" " .. current_kb() .. " ")
kbwidget:buttons(awful.util.table.join(
    awful.button({}, 1, function() next_kb() end)
))

-- Caffeine


caffwidget = wibox.widget.textbox()
function set_current_caffeine ()
    local current = run_command("~/scripts/caffeine.sh current")
    local text = current .. 's'
    if tonumber(current) >= 3200 then
        text = "<span color='#ff0000' font_weight='bold'>" .. text .. "</span>"
    elseif tonumber(current) > 300 then
        text = "<b>" .. text .. "</b>"
    end
    caffwidget:set_markup(text)
end

function next_caffeine ()
    run_command("~/scripts/caffeine.sh next")
    set_current_caffeine()
end
set_current_caffeine()
caffwidget:buttons(awful.util.table.join(
    awful.button({}, 1, function() next_caffeine() end)
))

-- / fs
fsicon = wibox.widget.imagebox(beautiful.widget_hdd)
fswidget = lain.widgets.fs({
    settings  = function()
        widget:set_text(" " .. fs_now.used .. "% ")
    end,
    notification_preset = {
        font = "Consolas 13"
    }
})

-- ALSA volume
volicon = wibox.widget.imagebox(beautiful.widget_vol)
function change_volume (step)
    awful.util.spawn_with_shell("amixer sset DAC0 " .. step)
    volumewidget:update()
end
volicon:buttons(
   awful.util.table.join(
      awful.button({}, 1, function ()
            awful.util.spawn_with_shell("pavucontrol")
   end),
   awful.button({}, 4, function()
         change_volume("1+")
   end),
   awful.button({}, 5, function()
         change_volume("1-")
   end)
))
volumewidget = lain.widgets.alsa({
    channel = "DAC0",
    settings = function()
        if volume_now.status == "off" then
            volicon:set_image(beautiful.widget_vol_mute)
        elseif tonumber(volume_now.level) == 0 then
            volicon:set_image(beautiful.widget_vol_no)
        elseif tonumber(volume_now.level) <= 50 then
            volicon:set_image(beautiful.widget_vol_low)
        else
            volicon:set_image(beautiful.widget_vol)
        end

        widget:set_text(" " .. volume_now.level .. "% ")
        widget:buttons(
            awful.util.table.join(
            awful.button({}, 1, function ()
                awful.util.spawn_with_shell("pavucontrol")
            end),
            awful.button({}, 4, function()
                change_volume("1+")
            end),
            awful.button({}, 5, function()
                change_volume("1-")
           end)
         ))
    end
})

-- Net
neticon = wibox.widget.imagebox(beautiful.widget_net)
netwidget = lain.widgets.net({
    settings = function()
        widget:set_markup(
            run_command("iw dev wlp2s0 link | grep SSID | cut -d' ' -f2-"):gsub("%&", "") ..
               markup("#7AC82E", " " .. string.format("%6.1f", net_now.received)) .. 
            " " ..
               markup("#46A8C3", " " .. string.format("%6.1f", net_now.sent) .. " ")
        )
    end
})
netwidget:buttons(awful.util.table.join(
    awful.button({ }, 1, function () awful.util.spawn_with_shell(terminal .. " -e nmtui") end),
    awful.button({ }, 2, function () awful.util.spawn_with_shell(iptraf) end)
    ))

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

mylauncher = wibox.widget.imagebox(beautiful.submenu_icon)
mylauncher:buttons(awful.util.table.join(awful.button({ }, 3, function () awful.util.spawn_with_shell("~/scripts/lock.sh") end)))

for s = 1, screen.count() do

    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()

    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                            awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                            awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                            awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                            awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))

    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s, height = 18 })

    -- Widgets that are aligned to the upper left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the upper right
    local function right_layout_add (...)
        local arg = {...}
        for i, n in pairs(arg) do
            right_layout:add(n)
        end
    end

    right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout_add(neticon,netwidget)
    right_layout_add(mpdicon, mpdwidget)
    right_layout_add(volicon, volumewidget)
    right_layout_add(memicon, memwidget)
    right_layout_add(cpuicon, cpuwidget)
    right_layout_add(fsicon, fswidget)
    right_layout_add(baticon, batwidget)
    right_layout_add(blwidget)
    right_layout_add(caffwidget)
    right_layout_add(mytextclock)
    right_layout_add(kbwidget)
    right_layout_add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)
    mywibox[s]:set_widget(layout)

end
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Up",  function() awful.screen.focus_relative(1) end),
    awful.key({ modkey,           }, "Down",  function() awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),
    awful.key({ modkey, "Shift" }, "Up", function() awful.client.movetoscreeen(client.focus) end),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey,           }, "e", function () awful.util.spawn(editor) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.client.incwfact(-0.05)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.client.incwfact( 0.05)      end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end),
    -- Backlight and Audio
    awful.key({ }, "#233", function() change_backlight("+60") end),
    awful.key({ }, "#232", function() change_backlight("-60") end),
    awful.key({ }, "#121", function()
        awful.util.spawn_with_shell("pactl set-sink-mute 1 toggle")
        volumewidget:update()
    end),
    awful.key({ }, "#122", function() change_volume("1-") end),
    awful.key({ }, "#123", function() change_volume("1+") end),
    awful.key({ modkey, altkey }, "#49", nil, function()
          next_kb()
          naughty.notify({
                text = "Changed Keyboard Map",
                timeout = 1
          })
    end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
