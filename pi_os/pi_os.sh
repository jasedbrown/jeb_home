#!/bin/bash
# pi_os.sh - Raspberry Pi OS specific setup

echo "Updating package lists..."
sudo apt update > /dev/null

echo "Upgrading existing packages..."
sudo apt upgrade -y > /dev/null

echo "Installing packages..."
# Install core packages from packages.txt
grep -v "^#" ./pi_os/packages.txt | xargs sudo apt install -y > /dev/null

if [ ! -f "/usr/local/bin/mold" ]; then
    echo "Installing mold linker from source..."
    ./pop_os/build_mold.sh 
fi

echo "Raspberry Pi OS specific setup complete!"
