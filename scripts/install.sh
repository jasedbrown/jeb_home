#!/bin/bash
# install.sh

# this is my long-time dev directory. make it it exists and is owned by me.
export SRC_DIR=/opt/dev
sudo mkdir -p $SRC_DIR
sudo chown $(whoami):$(whoami) $SRC_DIR

# Detect OS
if [ -f /etc/arch-release ]; then
    echo "Detected Arch Linux"
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

    echo "Installing system76 drivers..."
    ./arch/system76.sh

elif [ -f /etc/os-release ] && grep -q "Pop!_OS" /etc/os-release; then
    echo "Detected Pop!_OS"
    ./pop_os/pop_os.sh
else
    echo "Unsupported operating system"
    exit 1
fi

echo "Setting default shell to zsh..."
sudo chsh -s /usr/bin/zsh $(whoami)

echo "Running common setup..."
./scripts/setup_common.sh

echo "Setting up dotfiles..."
./scripts/symlink.sh

echo "Installation complete!"
