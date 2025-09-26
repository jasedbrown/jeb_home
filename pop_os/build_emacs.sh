#!/bin/bash

# build emacs from a distro tarball. this site was helpful:
# https://ryanfleck.ca/2025/compiling-emacs-30/

EMACS_VERSION="30.2"
TREE_SITTER_VERSION="v0.25.10"

Install build dependencies
sudo apt build-dep -y emacs
sudo apt install -y libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev \
     gnutls-bin libtree-sitter-dev gcc-10 imagemagick libmagick++-dev \
     libwebp-dev webp libxft-dev libxft2

####################
# fwiw, pop_os 22.04 *and* 24.04 only have tree-sitter v0.20.x in the package repo.
# of course, the rust support under tree-sitter is available after v0.21 :facepalm:
# so build tree-sitter ourselves (god dammit)

version_lt() {
    printf '%s\n%s\n' "$1" "$2" | sort -V -C
}

SYSTEM_TS_VERSION=$(pkg-config --modversion tree-sitter 2>/dev/null || echo "0.0.0")

if version_lt "$SYSTEM_TS_VERSION" "$TREE_SITTER_VERSION"; then
    TS_DIR="/tmp/tree-sitter"
    rm -rf $TS_DIR
    git cl git@github.com:tree-sitter/tree-sitter.git $TS_DIR
    cd /tmp/tree-sitter
    git co "$TREE_SITTER_VERSION"
    make
    sudo make install
    sudo ldconfig
fi


###################
# now to the main event
rm -rf /tmp/emacs*

tar_file="emacs-$EMACS_VERSION.tar.gz"
wget https://ftpmirror.gnu.org/emacs/$tar_file -O /tmp/$tar_file
cd /tmp
tar zxf $tar_file
cd /tmp/emacs-"$EMACS_VERSION"

export CC=/usr/bin/gcc-10
export CXX=/usr/bin/gcc-10
./autogen.sh
./configure \
    --with-native-compilation=aot \
    --with-imagemagick \
    --with-tree-sitter \
    --with-xft \
    --without-xaw3d
make -j$(nproc)
sudo make install
