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

# npm 12+ blocks dependency install scripts (preinstall/install/postinstall) by
# default as a supply-chain-risk mitigation. These packages rely on a
# postinstall step to fetch/link their platform-specific native binary, so
# without an explicit allowlist entry they silently install without a working
# binary (npm exits 0 and only prints a warning).
SCRIPT_ALLOWED_PACKAGES="@anthropic-ai/claude-code,opencode-ai"
npm config set allow-scripts="$SCRIPT_ALLOWED_PACKAGES" --location=user

echo "Installing/updating global npm packages..."
npm install --global "${packages[@]}"
