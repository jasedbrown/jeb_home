#!/bin/bash
# Install or update global npm command-line applications from packages.txt.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGES_FILE="$SCRIPT_DIR/packages.txt"

if ! command -v npm >/dev/null 2>&1; then
    echo "npm is required to install packages from $PACKAGES_FILE"
    exit 1
fi

mapfile -t packages < <(grep -Ev '^[[:space:]]*(#|$)' "$PACKAGES_FILE")

if [ "${#packages[@]}" -eq 0 ]; then
    exit 0
fi

echo "Installing/updating global npm packages..."
npm install --global "${packages[@]}"
