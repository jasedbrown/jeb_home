#!/bin/bash
# Install or update global npm command-line applications from packages.txt.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGES_FILE="$SCRIPT_DIR/packages.txt"

if ! command -v npm >/dev/null 2>&1; then
    echo "npm is required to install packages from $PACKAGES_FILE"
    exit 1
fi

# Not mapfile: macOS ships bash 3.2, which lacks it.
packages=()
while IFS= read -r package; do
    packages+=("$package")
done < <(grep -Ev '^[[:space:]]*(#|$)' "$PACKAGES_FILE")

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

# `npm install --global` re-downloads every package it's given, even ones that
# are already current, so only pass it packages that are missing or outdated.
# `npm outdated` exits 1 when anything is outdated, hence the `|| true`.
installed_json="$(npm ls --global --depth=0 --json 2>/dev/null || true)"
outdated_json="$(npm outdated --global --json 2>/dev/null || true)"

to_install=()
while IFS= read -r package; do
    [ -n "$package" ] && to_install+=("$package")
done < <(
    INSTALLED_JSON="$installed_json" OUTDATED_JSON="$outdated_json" node -e '
        const parse = (s) => { try { return JSON.parse(s || "{}"); } catch { return {}; } };
        const installed = new Set(Object.keys(parse(process.env.INSTALLED_JSON).dependencies || {}));
        const outdated = new Set(Object.keys(parse(process.env.OUTDATED_JSON)));
        for (const p of process.argv.slice(1)) {
            if (!installed.has(p) || outdated.has(p)) console.log(p);
        }
    ' "${packages[@]}"
)

if [ "${#to_install[@]}" -eq 0 ]; then
    exit 0
fi

echo "Installing/updating: ${to_install[*]}"
npm install --global "${to_install[@]}"
