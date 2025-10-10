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

MOLD_VERSION="v2.40.4"
MOLD_DIR="/tmp/mold"
if [ ! -f "/usr/local/bin/mold" ]; then
    echo "Installing mold linker from source..."

    # Install mold linker dependencies
    # there used to be/still exist known problems with mold, ubuntu, and clang/gcc:
    # https://github.com/rui314/mold/issues/1025
    # https://github.com/michaelsproul/mold-stdcpp/blob/main/.cargo/config.toml
    #
    # but according to this SO, just installing an updated version of gcc's libstdc++ solves it:
    # https://stackoverflow.com/questions/67712376/after-updating-gcc-clang-cant-find-libstdc-anymore
    # might also want to install an updated version of llvm, as well (see ./setup-llvm.sh)
    sudo apt install -y libstdc++-12-dev

    git clone https://github.com/rui314/mold.git "$MOLD_DIR"
    cd "$MOLD_DIR"
    git checkout "$MOLD_VERSION"
    mkdir build
    cd build
    sudo ../install-build-deps.sh
    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=c++ ..
    cmake --build . -j $(nproc)
    sudo cmake --install .
    cd /tmp
    rm -rf "$MOLD_DIR"
fi

# Build and install Emacs
if [ ! -f "/usr/local/bin/emacs" ]; then
    echo "Building Emacs from source..."
    ./build_emacs.sh
fi

echo "Pop!_OS specific setup complete!" 
