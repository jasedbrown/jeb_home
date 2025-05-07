#!/bin/bash
# symlink.sh - Create symlinks for dotfiles

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Set dotfiles directory
DOTFILES_DIR="$HOME/jeb_home"
cd "$DOTFILES_DIR" || { echo -e "${RED}Error: Cannot change to dotfiles directory${NC}"; exit 1; }

# Create necessary directories
echo -e "${BLUE}Creating necessary directories...${NC}"
mkdir -p "$HOME/.config"
mkdir -p "$HOME/.local"

# Symlink using stow if available, otherwise manually
if command -v stow &> /dev/null; then
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
else
    echo -e "${RED}GNU Stow not found, ignoring...${NC}"
fi

# system-wide settings
echo -e "${GREEN}Applying system-wide configuraitons...${NC}"
sudo ln -sf "$DOTFILES_DIR/system/profile.d/jasobrown-init.sh" /etc/profile.d/jasobrown-init.sh

if [ -f /etc/arch-release ]; then
    echo -e "${GREEN}Applying system-wide configuraitons - arch specific...${NC}"
    sudo cp "$DOTFILES_DIR/system/vconsole.conf" /etc/vconsole.conf
    sudo cp "$DOTFILES_DIR/system/sddm.conf" /etc/sddm.conf
fi

# Symlink config files
ln -sf "$(pwd)/config/cargo/config.toml" ~/.cargo/config.toml

# by this time, kitty should be installed (at least the symlink'd dir in ~/.config)
if [ ! -d "$SRC_HOME/kitty-themes" ]; then
    git clone https://github.com/dexpota/kitty-themes.git $SRC_HOME/kitty-themes
    ln -s $SRC_HOME/kitty-themes/themes/Afterglow.conf $XDG_CONFIG_HOME/kitty/theme.conf
fi

echo -e "${GREEN}Symlinks created successfully!${NC}"
