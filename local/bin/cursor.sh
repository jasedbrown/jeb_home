#!/bin/bash

# Directory where AppImages are stored
APPIMAGE_DIR="$HOME/.local/bin"

# Find the latest AppImage (sorted by version number)
LATEST_APPIMAGE=$(ls -v "$APPIMAGE_DIR"/cursor*.AppImage | tail -1)

# Check if an AppImage was found
if [ -z "$LATEST_APPIMAGE" ]; then
    echo "No AppImage found in $APPIMAGE_DIR"
    exit 1
fi

# Make sure the AppImage is executable
chmod +x "$LATEST_APPIMAGE"

# Run the latest AppImage in the background and log output
"$LATEST_APPIMAGE" > /tmp/app_output.log 2>&1 &
