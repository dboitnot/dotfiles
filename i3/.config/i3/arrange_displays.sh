#!/usr/bin/env bash

# TOP_LEFT=HDMI-1-3
# TOP_RIGHT=HDMI-1-1
# BOT_LEFT=DisplayPort-2
# BOT_RIGHT=HDMI-A-4


# xrandr --auto
# xrandr --output $TOP_LEFT --pos 0x0
# xrandr --output $TOP_RIGHT --right-of $TOP_LEFT
# xrandr --output $BOT_LEFT --below $TOP_LEFT
# xrandr --output $BOT_RIGHT --right-of $BOT_LEFT

autorandr --change main
