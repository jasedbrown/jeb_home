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
    # add $HOME/.cargo/bin to PATH
    export PATH=$PATH:$HOME/.cargo/bin
    echo "\nexport PATH=$PATH:$HOME/.cargo/bin" >> ~/.bashrc

    rustup component add rust-src
    rustup component add rust-analyzer
    cargo install --locked sccache
    cargo install --locked bottom
    cargo install --locked critcmp
    cargo install --locked flamegraph
    cargo install --locked cargo-deny
    cargo install --locked mdbook-toc mdbook
    cargo install --locked cargo-nextest
fi

# Install SDKMAN for Java/Maven
if [ ! -d "$HOME/.sdkman" ]; then
    echo "Installing sdkman (java)..."

    curl -s "https://get.sdkman.io" | bash
    source "$HOME/.sdkman/bin/sdkman-init.sh"
    sdk install java 21.0.5-zulu
    sdk install maven
fi

# Install pyenv
if [ ! -d "$HOME/.pyenv" ]; then
    echo "Installing pyenv..."

    curl https://pyenv.run | bash
    source "$HOME/.pyenv/bin/pyenv"
    pyenv update
    pyenv install 3.13

    # Install Python LSP packages
    python3 -m pip install python-lsp-server pylsp-mypy python-lsp-black python-lsp-ruff pylsp-rope 
fi

# Install golang
# the official docs on supporting multiple installed SDKs version is a fucking joke:
# https://go.dev/doc/manage-install
GO_DIR="/usr/local/go"
if [ ! -d "$GO_DIR" ]; then
    echo "Installing golang..."
    sudo mkdir -p $GO_DIR
    sudo chown $(whoami):$(whoami) $GO_DIR

    # download from https://go.dev/doc/install
    # install to $GO_DIR, unzip in version-name directory

    # create a symlink
    
fi
