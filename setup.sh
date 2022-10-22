#!/bin/bash

sudo apt update
sudo apt install -y \
     zsh \
     emacs \
     cmake \
     gcc \
     dstat \
     htop \
     ninja-build \
     build-essentials \
     libssl-dev \
     net-tools \
     autoconf \
     libtool \
     curl \
     libcurl4-openssl-dev \
     liblzma-dev \
     python-dev-is-python3 \
     libssl-dev \
     python3-pip \
     libgflags-dev \
     libzstd-dev \
     bzip2 \
     liblz4-dev \
     liblz4-tool \
     zstd \
     libbz2-dev \
     libsnappy-dev \
     zlib1g-dev \
     libgflags-dev 
     python3-pip


cp -v -R home/.* home/* ~

# install oh-my-zsh *yeah, i still like it)
if [ ! -d "~/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# install rust/rustup
if [ ! -d "~/.rustup" ]; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi
