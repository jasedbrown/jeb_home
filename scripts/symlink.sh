#!/bin/bash
# symlink.sh - Create symlinks for dotfiles

set -euo pipefail

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Set dotfiles directory
DOTFILES_DIR="$HOME/jeb_home"
cd "$DOTFILES_DIR" || { echo -e "${RED}Error: Cannot change to dotfiles directory${NC}"; exit 1; }
OS_NAME="$(uname -s)"

# Create necessary directories.
# Pre-create dirs that other tools write into so stow creates per-file
# symlinks instead of folding the whole directory into the repo.
echo -e "${BLUE}Creating necessary directories...${NC}"
mkdir -p "$HOME/.config"
mkdir -p "$HOME/.cache"
mkdir -p "$HOME/.local/bin"
mkdir -p "$HOME/.local/share"
mkdir -p "$HOME/.local/state"
mkdir -p "$HOME/.cargo"

if [ "$OS_NAME" = "Linux" ]; then
    mkdir -p "$HOME/.config/systemd/user/default.target.wants"
    mkdir -p "$HOME/.config/systemd/user/sockets.target.wants"
fi

if ! command -v stow >/dev/null 2>&1; then
    echo -e "${RED}GNU Stow not found. Install stow before running this script.${NC}"
    exit 1
fi

echo -e "${BLUE}Using GNU Stow to create symlinks...${NC}"

# Stow XDG config directories
echo -e "${GREEN}Symlinking XDG config files...${NC}"
stow -t "$HOME/.config" config

# Stow local scripts
echo -e "${GREEN}Symlinking scripts to ~/.local/...${NC}"
stow -t "$HOME/.local" local

# Stow home directory files
echo -e "${GREEN}Symlinking files to home directory...${NC}"
stow -t "$HOME" home

ln -sf "$DOTFILES_DIR/config/zsh/.zshenv" "$HOME/.zshenv"

if [ "$OS_NAME" = "Linux" ] && [ -f /etc/arch-release ]; then
    echo -e "${GREEN}Applying system-wide configurations - Arch specific...${NC}"
    sudo cp "$DOTFILES_DIR/system/vconsole.conf" /etc/vconsole.conf
    sudo cp "$DOTFILES_DIR/system/sddm.conf" /etc/sddm.conf

    # setup ssh keys, this allows arch to add a key to ssh-agent.
    # need to reenter pw on every login; else try `gnome-keyring`.
    mkdir -p ~/.ssh
    ln -sf "$DOTFILES_DIR/home/ssh/config" ~/.ssh/config
    chmod 700 ~/.ssh
    chmod 600 ~/.ssh/config

    sudo ln -fs "$DOTFILES_DIR/local/bin/grimblast" /usr/local/bin/grimblast

    # keep the mold consistent between arch and pop :shrug:
    sudo ln -fs /usr/bin/mold /usr/local/bin/mold
fi

if [ "$OS_NAME" = "Linux" ] && [ -d /etc/udev/rules.d ]; then
    echo -e "${GREEN}Applying udev rules...${NC}"
    sudo cp "$DOTFILES_DIR/system/etc/udev/rules.d/50-zsa.rules" /etc/udev/rules.d/50-zsa.rules
    if command -v udevadm >/dev/null 2>&1; then
        sudo udevadm control --reload-rules
    fi
fi


# Symlink config files
ln -sf "$(pwd)/config/cargo/config.toml" "$HOME/.cargo/config.toml"

# # symlink Claude config
# mkdir -p ~/.claude
# ln -sf "$(pwd)/config/claude/commands" ~/.claude/commands
# ln -sf "$(pwd)/config/claude/skills" ~/.claude/skills

echo -e "${GREEN}Symlinks created successfully!${NC}"
