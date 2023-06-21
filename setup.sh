#!/bin/bash
# add repo for updated versions of emacs
sudo add-apt-repository -y ppa:kelleyk/emacs

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
     libgflags-dev \ 
     python3-pip \
     emacs28 \
     libgflags-dev  \
     libgflags-dev \
     linux-tools-$(uname -r) \
     ncal

# install oh-my-zsh (yeah, i still like it)
if [ ! -d "~/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi


#######################################
# install rust/rustup
if [ ! -d "~/.rustup" ]; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    # add $HOME/.cargo/bin to PATH
    export PATH=$PATH:$HOME/.cargo/bin
    echo "\nexport PATH=$PATH:$HOME/.cargo/bin" >> ~/.bashrc

    cargo install flamegraph
    cargo install --locked cargo-deny
    rustup component add rust-src
    rustup component add rust-analyzer
fi

#############################
# install mold compiler. instructions taken from 
# https://github.com/rui314/mold
# only for linux

MOLD_HOME=$SRC_HOME/mold
if [ ! -d "$MOLD_HOME" ]; then
    cd $SRC_HOME
    git clone https://github.com/rui314/mold.git
    mkdir mold/build
    cd mold/build
    git checkout v1.11.0
    sudo ../install-build-deps.sh
    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=c++ ..
    cmake --build . -j $(nproc)
    sudo cmake --install .

    # now add to rust toolchain
    mkdir -p $HOME/.cargo
    cat <<EOF >> $HOME/.cargo/config.toml
[target.x86_64-unknown-linux-gnu]
linker = "clang"
rustflags = ["-C", "link-arg=-fuse-ld=/usr/local/bin/mold/mold"]
EOF

fi

# copy over home directory files *after* installing oh-my-zsh
cp -v -R home/.* home/* ~

# install semgrep (free, OSS engine: https://semgrep.dev/docs/getting-started/)
python3 -m pip install semgrep
# add to $PATH
