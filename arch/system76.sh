#!/bin/bash

# these are the instructions to install the system76 driver on arch linux.
# https://support.system76.com/articles/system76-driver/#arch---using-an-aur-helper
if [ -f /etc/arch-release ]; then
    paru -S --needed --noconfirm system76-firmware-daemon-git
    sudo systemctl enable --now system76-firmware-daemon
    sudo gpasswd -a $USER adm

    paru -S --needed --noconfirm firmware-manager-git

    paru -S --needed --noconfirm system76-driver
    # choose the first software option listed
    sudo systemctl enable --now system76
fi
