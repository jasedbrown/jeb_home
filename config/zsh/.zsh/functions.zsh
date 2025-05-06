# A collection of my simple functions to make zsh life easier
#
# tip: when you need to `cd` and don't want to change your cwd,
# use parentheses to create a subshell as it will just change
# directories for the duration of the function execution.
# you need to include all relevant functionality in those parens.

# Function to start docker compose
dockerup() {
  (cd $SRC_DIR/jeb-configs/docker && docker compose up -d)
}

# Function to shut down docker compose
dockerdown() {
  (cd $SRC_DIR/jeb-configs/docker && docker compose down)
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
  "$latest_appimage" > /tmp/app_output.log 2>&1 &
}
