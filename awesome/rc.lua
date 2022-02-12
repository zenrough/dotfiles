-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

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
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

-- battery widget battery widget https://github.com/streetturtle/awesome-wm-widgets
-- local batteryarc_widget = require("awesome-wm-widgets.batteryarc-widget.batteryarc")
local battery_widget = require("awesome-wm-widgets.battery-widget.battery")

-- volume widget
local volume_widget = require('awesome-wm-widgets.volume-widget.volume')

-- {{{ Error handling
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
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- set master size bigger than equal
beautiful.master_width_factor = 0.57

-- add transparent gaps between all clients, with a value of anything 0.2 if you want ? it's broken
-- gaps are usefull, if you to visually seperate windows,
-- and if using mouse you can move the mouse outside the window and scroll to change workspace.
 beautiful.useless_gap = 1


-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others programs as mod1 is the "alt" key.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    -- I do not need a layout where everything is floating
    -- awful.layout.suit.floating,
    awful.layout.suit.tile.right,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
     -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

local menu_awesome = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", terminal }

if has_fdo then
    mymainmenu = freedesktop.menu.build({
        before = { menu_awesome },
        after =  { menu_terminal }
    })
else
    mymainmenu = awful.menu({
        items = {
                  menu_awesome,
                  { "Debian", debian.menu.Debian_menu.Debian },
                  menu_terminal,
                }
    })
end


mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
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
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
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

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table. aux is auxillary, meaning helping programs for system configuration things, changing sound, media player, settings so on.
    -- awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])
    local names = { "E", "D", ">_", "W", "aux", "gimp", "comms", "office", "www" }
    local l = awful.layout.suit  -- Just to save some typing: use an alias.
    local layouts = {   l.tile.right , l.tile.right , l.tile.right , l.tile.right , l.tile.right , l.tile.right , l.tile.right , l.tile.right , l.max }
awful.tag(names, s, layouts)

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,

        -- i like the arc one more aestethicly but it's not very practical, (this plugin external, not built into awesomwm)
        -- volume_widget{
        --     widget_type = 'arc'
        -- },
         volume_widget(),
            mykeyboardlayout,

            -- batteryarc_widget({
            --     show_current_level = true,
            --     arc_thickness = 2, }),
            -- normal battery, you need the awesomwwm widgets collection for arc and normal
            battery_widget({
                show_current_level = true,
                 }),
            wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings keybindings
globalkeys = gears.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    -- rebound from "w" to "æ" which is good if you think it is useless, like i think
    -- awful.key({ modkey,           }, "æ", function () mymainmenu:show() end,
    --          {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),
    -- Menubar, applicaiton launcher
    -- awful.key({ modkey }, "p", function() menubar.show() end,
    awful.key({ modkey }, "p",
      function()
            menubar.show()
        -- rofi.launch {
          -- local handle = io.popen("rofi -modi drun -show drun -matching fuzzy")
          -- local result = handle:read("a*")
          -- handle:close()


      end, {description = "show the menubar", group = "launcher"}) )

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    -- awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
    awful.key({ modkey, }, "q",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
        -- awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
        -- awful.key({ modkey, }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

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
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "gimp",
          "signal",
          "xtightvncviewer"},

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = true }
    },

    -- Set Firefox to always map on the tag named "www" on screen 1.
    { rule = {
        class = "Firefox"

    },
      properties = { screen = 1, tag = "www" }
    },

    -- all terminals on 3. tag, "">_" is used because then you can change if oyu wan't it on 3. , or 2. , or 4.. or any tag
    { rule =
        { class = ".*terminal.*"
    -- kitty, plus alacritty

        },
      properties = { screen = 1, tag = ">_" }
    },

    -- kitty is a wayland and X11 terminal
    -- light resource usage
    { rule =
        { class = "kitty"

        },
      properties = { screen = 1, tag = ">_" }
    },

    -- alacritty the fastest terminal,
    -- but also ressource heavy
    { rule =
        { class = "alacritty"

        },
      properties = { screen = 1, tag = ">_" }
    },


    { rule =
        { class = "Zathura"

        },
      properties = { screen = 1, tag = "D" }
    },

    -- anything office on 8
    { rule = {
        class = ".*office.*"

    },
      properties = { screen = 1, tag = "office" }
    },

    { rule = {
       class = "java-lang-Thread"
        -- name = ".*Maple.*" ,
    },
      properties = { screen = 1, tag = "W" }
    },
    -- emacs as editor, smarter to use easy to acces this for things you toggle often, as i toggle windows instead of just viewing one at a time
    { rule = { class = ".*Emacs.*" },
      properties = { screen = 1, tag = "E" }
    },


}


-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

 -- autostart programs, startup programs,
 -- statup is implemented via a shitty hack
 -- on exit of awesomewm, it makes a emty file at /tmp/
 -- then detects if the file is there
 --
 awesome.connect_signal(
     'exit',
     function(args)
         awful.util.spawn('touch /tmp/.awesome-restart')
     end

 )
 
 awesome.connect_signal(
     'startup',
    function(args)
         awful.util.spawn('sh -c "rm /tmp/.awesome-restart || ~/.config/autorun.sh || ~/.config/autostart.sh "')
    end,

-- https://awesomewm.org/doc/api/libraries/awesome.html
    function (args)
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
            awful.tag.viewtoggle(tag)
    end
)

 -- My own layout as the defaults are shite,
 -- only expand horizontally, but 1 is still master
 -- this is layout is only bad for terminals as line lenght is comprimesed, this is not a problem
 -- this is not a problem because using a terminal multiplexer like tmux, then you only need 1 terminal window
 --  --        (1)                (2)                (3)
        --   +---+---+---+      +---+---+---+      +---+---+---+
        --   |   |   |   |      |      |    |      |       | | |
        --   | 1 |   |   |  ->  | 2    | 1  |  ->  | 1     |2|3|
        --   |   |   |   |      |      |    |      |       | | |
        --   +---+---+---+      +---+---+---+      +---+---+---+
        --
        --
--[[
this is a fork of
https://github.com/lcpz/lain/blob/master/layout/termfair.lua
--     Licensed under GNU General Public License v2
      * (c) 2014,      projektile
      * (c) 2013,      Luca CPZ
      * (c) 2010,      Nicolas Estibals
      * (c) 2010-2012, Peter Hofmann

--]]

-- local math     = math
-- local screen   = screen
-- local tonumber = tonumber

-- local termfair  = { name = "termfair" }
-- termfair.center = { name = "centerfair" }
-- termfair.stable = { name = "stablefair" }

-- local function do_fair(p, orientation)
--     local t = p.tag or screen[p.screen].selected_tag
--     local wa = p.workarea
--     local cls = p.clients

--     if #cls == 0 then return end

--     -- How many vertical columns? Read from nmaster on the tag.
--     local num_x = tonumber(termfair.nmaster) or t.master_count
--     local ncol  = tonumber(termfair.ncol) or t.column_count
--     if num_x <= 2 then num_x = 2 end
--     if ncol  <= 1 then ncol  = 1 end
--     local width = math.floor(wa.width/num_x)

--     if orientation == "west" then
--         -- Layout with fixed number of vertical columns (read from nmaster).
--         -- New windows align from left to right. When a row is full, a new
--         -- one above it is created. Like this:

--         --        (1)                (2)                (3)
--         --   +---+---+---+      +---+---+---+      +---+---+---+
--         --   |   |   |   |      |   |   |   |      |   |   |   |
--         --   | 1 |   |   |  ->  | 1 | 2 |   |  ->  | 1 | 2 | 3 |  ->
--         --   |   |   |   |      |   |   |   |      |   |   |   |
--         --   +---+---+---+      +---+---+---+      +---+---+---+

--         --        (4)                (5)                (6)
--         --   +---+---+---+      +---+---+---+      +---+---+---+
--         --   | 1 |   |   |      | 1 | 2 |   |      | 1 | 2 | 3 |
--         --   +---+---+---+  ->  +---+---+---+  ->  +---+---+---+
--         --   | 2 | 3 | 4 |      | 3 | 4 | 5 |      | 4 | 5 | 6 |
--         --   +---+---+---+      +---+---+---+      +---+---+---+

--         local num_y     = math.max(math.ceil(#cls / num_x), ncol)
--         local height    = math.floor(wa.height/num_y)
--         local cur_num_x = num_x
--         local at_x      = 0
--         local at_y      = 0

--         local remaining_clients = #cls

--         -- We start the first row. Left-align by limiting the number of
--         -- available slots.
--         if remaining_clients < num_x then
--             cur_num_x = remaining_clients
--         end

--         -- Iterate in reversed order.
--         for i = #cls,1,-1 do
--             -- Get x and y position.
--             local c = cls[i]
--             local this_x = cur_num_x - at_x - 1
--             local this_y = num_y - at_y - 1

--             -- Calculate geometry.
--             local g = {}
--             if this_x == (num_x - 1) then
--                 g.width = wa.width - (num_x - 1)*width
--             else
--                 g.width = width
--             end

--             if this_y == (num_y - 1) then
--                 g.height = wa.height - (num_y - 1)*height
--             else
--                 g.height = height
--             end

--             g.x = wa.x + this_x*width
--             g.y = wa.y + this_y*height

--             if g.width  < 1 then g.width  = 1 end
--             if g.height < 1 then g.height = 1 end

--             p.geometries[c] = g

--             remaining_clients = remaining_clients - 1

--             -- Next grid position.
--             at_x = at_x + 1
--             if at_x == num_x then
--                 -- Row full, create a new one above it.
--                 at_x = 0
--                 at_y = at_y + 1

--                 -- We start a new row. Left-align.
--                 if remaining_clients < num_x then
--                     cur_num_x = remaining_clients
--                 end
--             end
--         end
--     elseif orientation == "stable" then
--         -- Layout with fixed number of vertical columns (read from nmaster).
--         -- New windows align from left to right. When a row is full, a new
--         -- one below it is created. Like this:

--         --        (1)                (2)                (3)
--         --   +---+---+---+      +---+---+---+      +---+---+---+
--         --   |   |   |   |      |   |   |   |      |   |   |   |
--         --   | 1 |   |   |  ->  | 1 | 2 |   |  ->  | 1 | 2 | 3 |  ->
--         --   |   |   |   |      |   |   |   |      |   |   |   |
--         --   +---+---+---+      +---+---+---+      +---+---+---+

--         --        (4)                (5)                (6)
--         --   +---+---+---+      +---+---+---+      +---+---+---+
--         --   | 1 | 2 | 3 |      | 1 | 2 | 3 |      | 1 | 2 | 3 |
--         --   +---+---+---+      +---+---+---+      +---+---+---+
--         --   | 4 |   |   |      | 4 | 5 |   |      | 4 | 5 | 6 |
--         --   +---+---+---+  ->  +---+---+---+  ->  +---+---+---+

--         local num_y     = math.max(math.ceil(#cls / num_x), ncol)
--         local height    = math.floor(wa.height/num_y)

--         for i = #cls,1,-1 do
--             -- Get x and y position.
--             local c = cls[i]
--             local this_x = (i - 1) % num_x
--             local this_y = math.floor((i - this_x - 1) / num_x)

--             -- Calculate geometry.
--             local g = {}
--             if this_x == (num_x - 1) then
--                 g.width = wa.width - (num_x - 1)*width
--             else
--                 g.width = width
--             end

--             if this_y == (num_y - 1) then
--                 g.height = wa.height - (num_y - 1)*height
--             else
--                 g.height = height
--             end

--             g.x = wa.x + this_x*width
--             g.y = wa.y + this_y*height

--             if g.width  < 1 then g.width  = 1 end
--             if g.height < 1 then g.height = 1 end

--             p.geometries[c] = g
--         end
--
--     elseif orientation == "center" then
--         -- Layout with fixed number of vertical columns (read from nmaster).
--         -- Cols are centerded until there is nmaster columns, then windows
--         -- are stacked in the slave columns, with at most ncol clients per
--         -- column if possible.

--         -- with nmaster=3 and ncol=1 you'll have
--         --        (1)                (2)                (3)
--         --   +---+---+---+      +-+---+---+-+      +---+---+---+
--         --   |   |   |   |      | |   |   | |      |   |   |   |
--         --   |   | 1 |   |  ->  | | 1 | 2 | | ->   | 1 | 2 | 3 |  ->
--         --   |   |   |   |      | |   |   | |      |   |   |   |
--         --   +---+---+---+      +-+---+---+-+      +---+---+---+

--         --        (4)                (5)
--         --   +---+---+---+      +---+---+---+
--         --   |   |   | 3 |      |   | 2 | 4 |
--         --   + 1 + 2 +---+  ->  + 1 +---+---+
--         --   |   |   | 4 |      |   | 3 | 5 |
--         --   +---+---+---+      +---+---+---+

--         if #cls < num_x then
--             -- Less clients than the number of columns, let's center it!
--             local offset_x = wa.x + (wa.width - #cls*width) / 2
--             for i = 1, #cls do
--                 local g = { y = wa.y }
--                 g.width  = width
--                 g.height = wa.height
--                 if g.width < 1 then g.width = 1 end
--                 if g.height < 1 then g.height = 1 end
--                 g.x = offset_x + (i - 1) * width
--                 p.geometries[cls[i]] = g
--             end
--         else
--             -- More clients than the number of columns, let's arrange it!
--             -- Master client deserves a special treatement
--             local g = {}
--             g.width = wa.width - (num_x - 1)*width
--             g.height = wa.height
--             if g.width < 1 then g.width = 1 end
--             if g.height < 1 then g.height = 1 end
--             g.x = wa.x
--             g.y = wa.y
--             p.geometries[cls[1]] = g

--             -- Treat the other clients

--             -- Compute distribution of clients among columns
--             local num_y = {}
--             local remaining_clients = #cls-1
--             local ncol_min = math.ceil(remaining_clients/(num_x-1))

--             if ncol >= ncol_min then
--                 for i = (num_x-1), 1, -1 do
--                     if (remaining_clients-i+1) < ncol then
--                         num_y[i] = remaining_clients-i + 1
--                     else
--                         num_y[i] = ncol
--                     end
--                     remaining_clients = remaining_clients - num_y[i]
--                 end
--             else
--                 local rem = remaining_clients % (num_x-1)
--                 if rem == 0 then
--                     for i = 1, num_x-1 do
--                         num_y[i] = ncol_min
--                     end
--                 else
--                     for i = 1, num_x-1 do
--                         num_y[i] = ncol_min - 1
--                     end
--                     for i = 0, rem-1 do
--                         num_y[num_x-1-i] = num_y[num_x-1-i] + 1
--                     end
--                 end
--             end

--             -- Compute geometry of the other clients
--             local nclient = 2 -- we start with the 2nd client
--             local wx = g.x + g.width
--             for i = 1, (num_x-1) do
--                 local height = math.floor(wa.height / num_y[i])
--                 local wy = wa.y
--                 for _ = 0, (num_y[i]-2) do
--                     g = {}
--                     g.x = wx
--                     g.y = wy
--                     g.height = height
--                     g.width = width
--                     if g.width < 1 then g.width = 1 end
--                     if g.height < 1 then g.height = 1 end
--                     p.geometries[cls[nclient]] = g
--                     nclient = nclient + 1
--                     wy = wy + height
--                 end
--                 g = {}
--                 g.x = wx
--                 g.y = wy
--                 g.height = wa.height - (num_y[i] - 1)*height
--                 g.width = width
--                 if g.width < 1 then g.width = 1 end
--                 if g.height < 1 then g.height = 1 end
--                 p.geometries[cls[nclient]] = g
--                 nclient = nclient + 1
--                 wx = wx + width
--             end
--         end
--     end
-- end

-- function termfair.center.arrange(p)
--     return do_fair(p, "center")
-- end

-- function termfair.stable.arrange(p)
--     return do_fair(p, "stable")
-- end

-- function termfair.arrange(p)
--     return do_fair(p, "west")
-- end

-- return termfair
