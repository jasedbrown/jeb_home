#!/bin/bash

sudo apt update
sudo apt install \
     zsh \
     emacs \
     gcc \
     ninja-build \
     build-essentials \
     libssl-dev \
     net-tools \
     autoconf \
     libtool \
     curl

cp -v -R home/.* home/* ~

if [ ! -d "~/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

