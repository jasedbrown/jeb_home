#!/bin/bash

# install_work.sh - Install work-specific configurations
# This script should be run after the main dotfiles installation
#
# Environment variables:
#   WORK_DOTFILES_DIR: Path to the work-specific dotfiles repository
#                      This must be set by the parent script (install.sh)
#
# This script is not meant to be run directly. It should be called by install.sh
# which will set the required environment variables.

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Check if work dotfiles repo path is provided via environment variable
if [ -z "$WORK_DOTFILES_DIR" ]; then
    echo -e "${RED}Error: WORK_DOTFILES_DIR environment variable not set${NC}"
    echo "This script should be called by install.sh which sets the required environment variables."
    exit 1
fi

# Verify the work dotfiles directory exists
if [ ! -d "$WORK_DOTFILES_DIR" ]; then
    echo -e "${RED}Error: Work dotfiles directory not found at $WORK_DOTFILES_DIR${NC}"
    exit 1
fi

echo -e "${BLUE}Installing work-specific configurations...${NC}"

# Run work-specific installation script if it exists
if [ -f "$WORK_DOTFILES_DIR/install.sh" ]; then
    echo -e "${BLUE}Running work-specific installation script...${NC}"
    bash "$WORK_DOTFILES_DIR/install.sh"
fi

echo -e "${GREEN}Work-specific configurations installed successfully!${NC}" 