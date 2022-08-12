#!/usr/bin/env bash

for TP in $(xinput list |grep -i touchpad |grep -o 'id=[0-9][0-9]*' |cut -d = -f 2); do
    echo "Configuring Touchpad $TP"
    xinput set-prop $TP "libinput Tapping Enabled" 1
    xinput set-prop $TP "libinput Tapping Enabled Default" 1
done
