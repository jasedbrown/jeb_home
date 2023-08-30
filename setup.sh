#!/bin/bash
# make sure to ssh with -A flag

set -euo pipefail
set -x

#############################
# update system

# add repo for updated versions of emacs
sudo add-apt-repository -y ppa:kelleyk/emacs

sudo apt update
sudo apt install -y \
     autoconf \
     build-essential \
     bzip2 \
     cmake \
     curl \
     dstat \
     gcc \
     gdb \
     gnuplot \
     htop \
     libbz2-dev \
     libclang-dev \
     libclang-cpp-dev \
     libcurl4-openssl-dev \
     libgflags-dev \
     liblzma-dev \
     liblz4-dev \
     liblz4-tool \
     libsnappy-dev \
     libssl-dev \
     libssl-dev \
     libtool \
     libzstd-dev \
     linux-tools-$(uname -r) \
     linux-tools-common \
     linux-tools-generic \
     mysql-client \
     ncal \
     net-tools \
     ninja-build \
     pkg-config \
     postgresql-client \
     python-dev-is-python3 \
     python3-pip \
     python3-venv \
     sysbench \
     tmux \
     wireshark \
     zlib1g-dev \
     zsh \
     zstd 


# install oh-my-zsh (yeah, i still like it)
if [ ! -d "~/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi


##############################
# install docker (https://docs.docker.com/engine/install/ubuntu/)

if [ ! -f "/usr/bin/docker" ]; then
    # Add Docker’s official GPG key
    sudo install -m 0755 -d /etc/apt/keyrings
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
    sudo chmod a+r /etc/apt/keyrings/docker.gpg

    # set up the repository
    echo \
  "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

    sudo apt-get update
    sudo apt-get -y install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
fi


#######################################
# install rust/rustup
if [ ! -d "~/.rustup" ]; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    # add $HOME/.cargo/bin to PATH
    export PATH=$PATH:$HOME/.cargo/bin
    echo "\nexport PATH=$PATH:$HOME/.cargo/bin" >> ~/.bashrc

    rustup component add rust-src
    rustup component add rust-analyzer
    cargo install critcmp
    cargo install flamegraph
    cargo install --locked cargo-deny
fi

# lower the paranoia level so we can get reasonable flamegraphs
echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid

SRC_HOME=~/src
mkdir -p $SRC_HOME

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
    git checkout v2.1.0
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


#############################
# install readyset and do a basic build of readyset,
# this will update cargo, grab dependent crates, etc.

READYSET_HOME=$SRC_HOME/readyset
if [ ! -d "$READYSET_HOME" ]; then
    cd $SRC_HOME
    git clone https://github.com/readysettech/readyset.git 
#    cd $READYSET_HOME
#    cargo build --bin readyset
#    cargo build --release --bin readyset
fi


#############################
## install flamegraphs

if [ ! -d "$SRC_HOME/FlameGraph" ]; then
    cd $SRC_HOME
    git clone git@github.com:brendangregg/FlameGraph.git
fi

# copy over home directory files *after* installing oh-my-zsh
cp -v -R home/.* home/* ~

# install semgrep (free, OSS engine: https://semgrep.dev/docs/getting-started/)
python3 -m pip install semgrep
# add to $PATH
