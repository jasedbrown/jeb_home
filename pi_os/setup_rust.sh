#!/bin/bash
# setup_rust.sh - Rust development environment for Raspberry Pi OS

echo "Configuring system settings for profiling..."
# Lower the paranoia level for better flamegraphs
sudo sh -c 'echo -1 > /proc/sys/kernel/perf_event_paranoid'
sudo sh -c 'echo 0 > /proc/sys/kernel/kptr_restrict'

echo "Setting up Rust development tools..."
# Install rust and libraries
if [ ! -d "$HOME/.rustup" ] || ! command -v rustc &> /dev/null; then
    echo "Installing rust..."

    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    # Export to run the cargo install commands below
    export PATH=$PATH:$HOME/.cargo/bin

    rustup component add rust-src
    rustup component add rust-analyzer

    echo "Installing Rust development tools (this may take a while on ARM)..."
    cargo install --locked sccache
    cargo install --locked inferno
    cargo install --locked cargo-nextest
fi

echo "Rust development setup complete!"
