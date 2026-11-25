#!/bin/bash
# install.sh
#
# Installation script for jasobrown's dotfiles.
#
# Example usage:
#   # Install only public dotfiles
#   ./scripts/install.sh

# Ensure we're in the correct directory
if [[ ! -f "$(dirname "$0")/setup_common.sh" ]]; then
    echo "Error: This script must be run from the root of the dotfiles repository"
    echo "Current directory: $(pwd)"
    echo "Expected to find: scripts/setup_common.sh"
    exit 1
fi

if [ "$(uname -s)" = "Darwin" ]; then
    ./macos/install.sh
    exit 0
fi

# this is my long-time dev directory. make sure it exists and is owned by me.
export SRC_HOME=/opt/dev
if [ ! -d "$SRC_HOME" ]; then
    sudo mkdir -p $SRC_HOME
    sudo chown $(whoami):$(whoami) $SRC_HOME
fi

# Detect OS
if command -v omarchy >/dev/null 2>&1; then
    bash ./omarchy/omarchy.sh
elif [ -f /etc/arch-release ]; then
    ./arch/arch.sh
elif [ -f /etc/os-release ] && grep -q "Pop!_OS" /etc/os-release; then
    ./pop_os/pop_os.sh
elif [ -f /proc/device-tree/model ] && grep -q "Raspberry Pi" /proc/device-tree/model; then
    ./pi_os/pi_os.sh
else
    echo "Unsupported distro!"
    exit 1
fi

# Only change shell if not already zsh
if [ "$(getent passwd $(whoami) | cut -d: -f7)" != "/usr/bin/zsh" ]; then
    sudo chsh -s /usr/bin/zsh $(whoami)
fi

./scripts/setup_common.sh

./npm/install.sh

./scripts/symlink.sh
