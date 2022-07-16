#!/bin/bash

sudo apt update
sudo apt install -y \
     zsh \
     emacs \
     gcc \
     ninja-build \
     build-essentials \
     libssl-dev \
     net-tools \
     autoconf \
     libtool \
     curl \
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
