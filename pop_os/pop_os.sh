#!/bin/bash
# pop_os.sh

echo "Adding additional repositories..."
# Add Wireshark PPA
sudo add-apt-repository -y ppa:wireshark-dev/stable

# Add Docker's official GPG key and repository
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg
echo \
  "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

echo "Updating package lists..."
sudo apt update

echo "Installing packages..."
# Install core packages from packages.txt
xargs -a packages.txt sudo apt install -y

# Install mold linker dependencies
# there used to be/still exist known problems with mold, ubuntu, and clang/gcc:
# https://github.com/rui314/mold/issues/1025
# https://github.com/michaelsproul/mold-stdcpp/blob/main/.cargo/config.toml
#
# but according to this SO, just installing an updated version of gcc's libstdc++ solves it:
# https://stackoverflow.com/questions/67712376/after-updating-gcc-clang-cant-find-libstdc-anymore
# might also want to install an updated version of llvm, as well (see ./setup-llvm.sh)
sudo apt install -y libstdc++-12-dev

echo "Installing mold linker from source..."
MOLD_VERSION="v2.37.1"
MOLD_DIR="/tmp/mold"
if [ ! -f "/usr/local/bin/mold" ]; then
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

echo "Building Emacs from source..."
# Install build dependencies
sudo apt build-dep -y emacs
sudo apt install -y libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev \
    gnutls-bin libtree-sitter-dev gcc-10 imagemagick libmagick++-dev \
    libwebp-dev webp libxft-dev libxft2

# Build and install Emacs
EMACS_DIR="/opt/dev/emacs-30"
if [ ! -f "/usr/local/bin/emacs" ]; then
    sudo mkdir -p /opt/dev
    sudo chown $(whoami):$(whoami) /opt/dev
    git clone --depth 1 --branch emacs-30.1 git://git.savannah.gnu.org/emacs.git "$EMACS_DIR"
    cd "$EMACS_DIR"
    
    export CC=/usr/bin/gcc-10
    export CXX=/usr/bin/gcc-10
    ./autogen.sh
    ./configure --with-native-compilation=aot --with-imagemagick --with-json \
                --with-tree-sitter --with-xft
    make -j$(nproc)
    sudo make install
fi

echo "Pop!_OS specific setup complete!" 
