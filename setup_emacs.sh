#!/bin/bash

###############################
# a script for building emacs from source.
# instructions came from theses links:
# https://arnesonium.com/2023/07/emacs-29-1-on-ubuntu-22-04-lts
# https://practical.li/blog/posts/build-emacs-from-source-on-ubuntu-linux/

set -euo pipefail
set -x

# get the build deps first - yeah, it needs gcc-10. not sure if
# that exact version, but multiple sites listed it.
sudo apt build-dep emacs
sudo apt install -y libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev \
    gnutls-bin libtree-sitter-dev gcc-10 imagemagick libmagick++-dev \
    libwebp-dev webp libxft-dev libxft2

cd /opt/dev
git clone --depth 1 --branch emacs-29 git://git.savannah.gnu.org/emacs.git emacs-29
cd emacs-29

export CC=/usr/bin/gcc-10
export CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-native-compilation=aot --with-imagemagick --with-json \
            --with-tree-sitter --with-xft
make -j$(nproc)

sudo make install
