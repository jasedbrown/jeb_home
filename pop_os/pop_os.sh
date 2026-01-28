#!/bin/bash
# pop_os.sh

echo "Adding additional repositories..."
# Add Wireshark PPA if not already present
if ! grep -q "wireshark-dev/stable" /etc/apt/sources.list.d/*.list; then
    sudo add-apt-repository -y ppa:wireshark-dev/stable
fi

echo "Updating package lists..."
sudo apt update > /dev/null

echo "Installing packages..."
# Install core packages from packages.txt
grep -v "^#" ./pop_os/packages.txt | xargs sudo apt install -y > /dev/null

if [ ! -f "/usr/local/bin/mold" ]; then
    echo "Installing mold linker from source..."
    ./build_mold.sh 
fi

# Build and install Emacs
if [ ! -f "/usr/local/bin/emacs" ]; then
    echo "Building Emacs from source..."
    ./build_emacs.sh
fi

# pop_os has some aggressively low vm.dirty_bytes settings (tuned for low-end hardware).
# override those settings
sudo cp ./system/etc/sysctl.d/99-dirty-pages.conf /etc/sysctl.d/

echo "Pop!_OS specific setup complete!" 
