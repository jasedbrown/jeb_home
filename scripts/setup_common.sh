#!/bin/bash
# setup_common.sh

echo "Configuring system settings..."
# Lower the paranoia level for better flamegraphs
sudo sh -c 'echo -1 > /proc/sys/kernel/perf_event_paranoid'
sudo sh -c 'echo 0 > /proc/sys/kernel/kptr_restrict'

echo "Setting up development tools..."
# install rust and libraries
if [ ! -d "$HOME/.rustup" ] || ! command -v rustc &> /dev/null; then
    echo "Installing rust..."

    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    # export to run the `cargo install` commands below
    export PATH=$PATH:$HOME/.cargo/bin

    rustup component add rust-src
    rustup component add rust-analyzer
    cargo install --locked sccache
    cargo install --locked inferno
    cargo install --locked cargo-nextest
fi


#################################
# java

# Install SDKMAN for Java/Maven
if [ ! -d "$HOME/.sdkman" ]; then
    echo "Installing sdkman (java)..."

    curl -s "https://get.sdkman.io" | bash
    source "$HOME/.sdkman/bin/sdkman-init.sh"
    sdk install java 21.0.5-zulu
    sdk install maven
fi

# Install/update JDTLS (Java LSP server)
# my wrapper script will be symlinked
JDTLS_DIR="$HOME/.local/share/eclipse.jdt.ls"
JDTLS_SNAPSHOTS="https://download.eclipse.org/jdtls/snapshots"
# latest.txt names the current snapshot tarball, e.g. jdt-language-server-1.62.0-202609111927.tar.gz.
# Record it after install and only re-download when it changes.
JDTLS_LATEST=$(curl -fsSL "$JDTLS_SNAPSHOTS/latest.txt" | tr -d '[:space:]')
JDTLS_VERSION_FILE="$JDTLS_DIR/.installed-version"

if [ -z "$JDTLS_LATEST" ]; then
    echo "Could not determine latest JDTLS version, skipping"
elif [ "$(cat "$JDTLS_VERSION_FILE" 2>/dev/null)" != "$JDTLS_LATEST" ]; then
    echo "Installing JDTLS $JDTLS_LATEST..."
    mkdir -p "$JDTLS_DIR"
    if curl -fsSL "$JDTLS_SNAPSHOTS/$JDTLS_LATEST" -o /tmp/jdtls.tar.gz; then
        rm -rf "$JDTLS_DIR"/* "$JDTLS_VERSION_FILE"
        tar -xzf /tmp/jdtls.tar.gz -C "$JDTLS_DIR" && echo "$JDTLS_LATEST" > "$JDTLS_VERSION_FILE"
    fi
    rm -f /tmp/jdtls.tar.gz
fi


#################################
# python

# Install pyenv
if [ ! -d "$HOME/.pyenv" ]; then
    echo "Installing pyenv..."

    curl https://pyenv.run | bash
    source "$HOME/.pyenv/bin/pyenv"
    pyenv update
    pyenv install 3.14
fi

# Make sure pipx is initialized
pipx ensurepath

# Install global Python LSP + helpers if not already installed
if ! pipx list | grep -q "python-lsp-server"; then
    pipx install python-lsp-server
    pipx inject python-lsp-server pylsp-mypy python-lsp-black python-lsp-ruff pylsp-rope
fi


#################################
# Install golang
# the official docs on supporting multiple installed SDKs version is a fucking joke:
# https://go.dev/doc/manage-install
# GO_DIR="/usr/local/go"
# if [ ! -d "$GO_DIR" ]; then
#     echo "Installing golang..."
#     sudo mkdir -p $GO_DIR
#     sudo chown $(whoami):$(whoami) $GO_DIR

#     # Download from https://go.dev/doc/install
#     # install to $GO_DIR, unzip in version-name directory

#     # create a symlink
    
# fi
