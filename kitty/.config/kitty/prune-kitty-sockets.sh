#!/usr/bin/env bash

for F in $HOME/.kitty.sock-*; do
    P=$(basename $F |cut -d - -f 2)
    if [ "$(ps -q $P -o cmd=)" == "kitty" ]; then
        echo "$P is active"
    else
        echo "$P is gone, pruning socket file"
        rm -f $F
    fi
done
