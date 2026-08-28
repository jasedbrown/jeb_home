#!/bin/bash

set -euo pipefail

if ! command -v omarchy >/dev/null 2>&1; then
    echo "Omarchy is required to run this installer." >&2
    exit 1
fi

echo "Installing Omarchy repository packages..."
grep -v '^[[:space:]]*#' ./omarchy/packages.txt | xargs -r omarchy pkg add

echo "Installing Omarchy AUR packages..."
grep -v '^[[:space:]]*#' ./omarchy/aur-packages.txt | xargs -r omarchy pkg aur add
