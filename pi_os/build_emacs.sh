#!/bin/bash
set -euo pipefail

EMACS_VERSION="30.2"

sudo apt update

# Minimal deps for terminal Emacs + native-comp + tree-sitter on Debian/RPiOS Trixie
sudo apt install -y \
  build-essential \
  wget ca-certificates \
  libncurses-dev \
  libgnutls28-dev \
  libjansson-dev \
  libtree-sitter-dev \
  zlib1g-dev \
  libxml2-dev \
  libsqlite3-dev

# Native-comp: on Trixie, prefer the matching gccjit dev package
sudo apt install -y libgccjit-12-dev libgccjit0 || sudo apt install -y libgccjit-dev

rm -rf /tmp/emacs-"$EMACS_VERSION" /tmp/emacs-"$EMACS_VERSION".tar.gz

tar_file="emacs-$EMACS_VERSION.tar.gz"
wget -O /tmp/"$tar_file" "https://ftpmirror.gnu.org/emacs/$tar_file"

cd /tmp
tar zxf "$tar_file"
cd /tmp/emacs-"$EMACS_VERSION"

# Release tarball: no autogen.sh
rm -f config.cache config.log config.status

./configure \
  --with-native-compilation=aot \
  --with-tree-sitter \
  --with-x-toolkit=no \
  --without-x \
  --without-imagemagick

make -j"$(nproc)"
sudo make install
