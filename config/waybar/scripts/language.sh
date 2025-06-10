#!/bin/bash

status=$(fcitx5-remote -n 2>/dev/null)

if [[ $status == *"mozc"* ]] || [[ $status == *"japanese"* ]]; then
    echo "JP"
elif [[ $status == *"keyboard"* ]]; then
    echo "EN"
else
    echo "EN"
fi
