#!/bin/bash
# install.sh

# this is my long-time dev directory. make sure it exists and is owned by me.
export SRC_DIR=/opt/dev
sudo mkdir -p $SRC_DIR
sudo chown $(whoami):$(whoami) $SRC_DIR

# Detect OS
if [ -f /etc/arch-release ]; then
    echo "Detected Arch Linux"
    ./arch/arch.sh
elif [ -f /etc/os-release ] && grep -q "Pop!_OS" /etc/os-release; then
    echo "Detected Pop!_OS"
    ./pop_os/pop_os.sh
else
    echo "Unsupported distro!"
    exit 1
fi

# Only change shell if not already zsh
if [ "$(getent passwd $(whoami) | cut -d: -f7)" != "/usr/bin/zsh" ]; then
    sudo chsh -s /usr/bin/zsh $(whoami)
fi

echo "Running common setup..."
./scripts/setup_common.sh

echo "Setting up dotfiles..."
./scripts/symlink.sh

echo "Installation complete!"
