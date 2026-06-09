#!/bin/bash
# macos/install.sh - Bootstrap macOS development tools and dotfile symlinks.

set -euo pipefail

DOTFILES_HOME="${DOTFILES_HOME:-$HOME/jeb_home}"
BREWFILE="$DOTFILES_HOME/macos/Brewfile"

if [ "$(uname -s)" != "Darwin" ]; then
    echo "Error: macos/install.sh is only intended for macOS."
    exit 1
fi

if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -x /usr/local/bin/brew ]; then
    eval "$(/usr/local/bin/brew shellenv)"
fi

if ! command -v brew >/dev/null 2>&1; then
    echo "Error: Homebrew is not installed or not on PATH."
    echo "Install it from https://brew.sh, then rerun this script."
    exit 1
fi

if [ -d /Applications/Emacs.app ] && ! brew list --cask emacs-plus-app >/dev/null 2>&1; then
    echo "Error: /Applications/Emacs.app already exists, but emacs-plus-app is not installed by Homebrew."
    echo "Move or remove the existing Emacs.app before running this installer."
    echo "For example: mv /Applications/Emacs.app /Applications/Emacs-formacosx.app"
    exit 1
fi

if [ ! -f "$BREWFILE" ]; then
    echo "Error: cannot find Brewfile at $BREWFILE"
    exit 1
fi

echo "Installing macOS packages from $BREWFILE..."
brew bundle --file "$BREWFILE"

echo "Creating user config directories..."
mkdir -p "$HOME/.config"
mkdir -p "$HOME/.cache"
mkdir -p "$HOME/.local/bin"
mkdir -p "$HOME/.local/share"
mkdir -p "$HOME/.local/state"
mkdir -p "$HOME/.cargo"

if [ -L /usr/local/bin/emacs ]; then
    old_emacs_target="$(readlink /usr/local/bin/emacs)"
    case "$old_emacs_target" in
        /Applications/Emacs.app/*)
            echo "Note: /usr/local/bin/emacs still points at $old_emacs_target"
            echo "      /opt/homebrew/bin should come first after Homebrew shellenv is loaded."
            ;;
    esac
fi

echo "Setting up dotfile symlinks..."
"$DOTFILES_HOME/scripts/symlink.sh"

echo "macOS setup complete."
