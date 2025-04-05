#!/bin/bash
# install.sh

echo "Installing core packages..."
sudo pacman -S --needed --noconfirm - < arch/packages.txt

echo "Installing AUR helper..."
if ! command -v paru &> /dev/null; then
    echo "Installing paru..."
    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si --noconfirm
    cd ..
    rm -rf paru
fi

echo "Installing AUR packages..."
paru -S --needed --noconfirm - < arch/aur-packages.txt

echo "Setting up dotfiles..."
./scripts/symlink.sh

echo "Installation complete!"
