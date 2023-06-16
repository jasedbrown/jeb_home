#!/bin/bash
# add repo for updated versions of emacs
sudo add-apt-repository ppa:kelleyk/emacs

sudo apt update
sudo apt install -y \
     zsh \
     cmake \
     gcc \
     dstat \
     htop \
     ninja-build \
     build-essential \
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
<<<<<<< HEAD
     libgflags-dev \ 
     python3-pip \
     emacs28 \
     libgflags-dev  
=======
     libgflags-dev \
     linux-tools-$(uname -r)
>>>>>>> e222d57 (add linux perf to install)


# install oh-my-zsh *yeah, i still like it)
if [ ! -d "~/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# install rust/rustup
if [ ! -d "~/.rustup" ]; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    rustup component add rust-src
    rustup component add rust-analyzer
fi

# copy over home directory files *after* installing oh-my-zsh
cp -v -R home/.* home/* ~

# install semgrep (free, OSS engine: https://semgrep.dev/docs/getting-started/)
python3 -m pip install semgrep
# add to $PATH
