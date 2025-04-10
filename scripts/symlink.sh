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
    stow -t "$HOME/.local/bin" local/bin
    mkdir -p "$HOME/.local/share"
    stow -t "$HOME/.local/share/wallpapers" local/share/wallpapers
    
    # Stow home directory files
    echo -e "${GREEN}Symlinking files to home directory...${NC}"
    stow -t "$HOME" home
else
    echo -e "${YELLOW}GNU Stow not found, ignoring...${NC}"
fi

# VS Code specific setup
# if [ -d "$DOTFILES_DIR/config/Code" ]; then
#     echo -e "${GREEN}Setting up VS Code...${NC}"
#     if [ -f "$DOTFILES_DIR/scripts/vscode-setup.sh" ]; then
#         bash "$DOTFILES_DIR/scripts/vscode-setup.sh"
#     else
#         mkdir -p "$HOME/.config/Code/User"
#         ln -sf "$DOTFILES_DIR/config/Code/User/keybindings.json" "$HOME/.config/Code/User/keybindings.json"
        
#         if [ -f "$DOTFILES_DIR/config/Code/extensions.txt" ] && command -v code &> /dev/null; then
#             echo -e "${BLUE}Installing VS Code extensions...${NC}"
#             cat "$DOTFILES_DIR/config/Code/extensions.txt" | xargs -L 1 code --install-extension
#         fi
#     fi
# fi

# Arch-specific setup
if [ -f /etc/arch-release ]; then
    echo -e "${GREEN}Applying Arch-specific configurations...${NC}"
    # Add any Arch-specific symlinks here
fi

# system-wide settings
echo "${GREEN}Applying system-wide configuraitons...${NC}"
sudo ln -sf "$DOTFILES_DIR/system/profile.d/jasobrown-init.sh" /etc/profile.d/jasobrown-init.sh

if [ -f /etc/arch-release ]; then
    echo "${GREEN}Applying system-wide configuraitons - arch ...${NC}"
    sudo cp "$DOTFILES_DIR/system/vconsole.conf" /etc/vconsole.conf
    sudo cp "$DOTFILES_DIR/system/sddm.conf" /etc/sddm.conf
fi

# Symlink config files
ln -sf "$(pwd)/config/cargo/config.toml" ~/.cargo/config.toml

echo -e "${GREEN}Symlinks created successfully!${NC}"
