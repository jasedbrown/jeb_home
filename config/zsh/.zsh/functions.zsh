# A collection of my simple functions to make zsh life easier
#
# tip: when you need to `cd` and don't want to change your cwd,
# use parentheses to create a subshell as it will just change
# directories for the duration of the function execution.
# you need to include all relevant functionality in those parens.

# Function to add SSH key if not already added
function ensure_ssh_key() {
  eval "$(ssh-agent -s)" > /dev/null

  # Check if key is already added
  ssh-add -l | grep -q "$(ssh-keygen -lf ~/.ssh/id_ed25519 | awk '{print $2}')" 2>/dev/null
  if [ $? -ne 0 ]; then
    ssh-add ~/.ssh/id_ed25519
  fi
}

# Function to start Cursor editor
cursor() {
  local appimage_dir="$HOME/.local/bin"
  local latest_appimage=$(ls -v "$appimage_dir"/cursor*.AppImage | tail -1)

  if [ -z "$latest_appimage" ]; then
    echo "No AppImage found in $appimage_dir"
    return 1
  fi

  chmod +x "$latest_appimage"

  if [ -f /etc/arch-release ]; then
    export ELECTRON_OZONE_PLATFORM_HINT=wayland
    export GDK_BACKEND=wayland
    export XDB_SESSION_TYPE=wayland
    export ENABLE_SANDBOX=0
      
    nohup "$latest_appimage" \
      --enable-features=UseOzonePlatform \
      --ozone-platform=wayland \
      "$@" > /dev/null 2>&1 & disown
  else
    nohup "$latest_appimage" \
      "$@" > /dev/null 2>&1 & disown
  fi
}
# Function to start zoom video conferencing
zoom-launch() {
  if [ -f /etc/arch-release ]; then
    export ELECTRON_OZONE_PLATFORM_HINT=wayland
    export GDK_BACKEND=wayland
    export XDB_SESSION_TYPE=wayland
    export ENABLE_SANDBOX=0
      
    nohup zoom \
      --enable-features=UseOzonePlatform \
      --ozone-platform=wayland \
      "$@" > /dev/null 2>&1 & disown
  else
    nohup zoom \
      "$@" > /dev/null 2>&1 & disown
  fi
}

restart() {
    sudo /sbin/shutdown -r 0
}

sysupdate() {
    if [ -f /etc/arch-release ]; then
        sudo pacman -Syu
    else
        sudo apt update
        sudo apt upgrade
    fi
}

# tmux attach-or-create from tmuxp template
tdev() {
  local name="$1"
  local tmpl_path

  if [[ -z "$name" ]]; then
    echo "Usage: tdev <session-name>"
    return 1
  fi

  # Try to attach if session exists
  if tmux has-session -t "$name" 2>/dev/null; then
    tmux attach -t "$name"
  else
    # Look for template in ~/.tmuxp or ~/.config/tmuxp
    if [[ -f "$XDG_CONFIG_HOME/tmuxp/$name.yaml" ]]; then
      tmpl_path="$XDG_CONFIG_HOME/tmuxp/$name.yaml"
    else
      echo "Template for '$name' not found in $XDG_CONFIG_HOME/tmuxp"
      return 1
    fi
    tmuxp load "$tmpl_path"
  fi
}

# Run the basic cargo sanity commands: check, fmt, and clippy
ccheck() {
    echo "cargo check"
    cargo check
    echo "cargo fmt"
    cargo fmt
    echo "cargo clippy"
    cargo clippy
}
