#!/bin/bash

set -x

SCRIPT_DIR=$(pwd)

#############################
# update system

#sudo add-apt-repository ppa:wireshark-dev/stable
sudo apt update
sudo DEBIAN_FRONTEND=noninteractive apt install -y \
     alacritty \
     autoconf \
     build-essential \
     bzip2 \
     cmake \
     curl \
     dstat \
     feh \
     gcc \
     gdb \
     gnuplot \
     graphviz \
     htop \
     libbz2-dev \
     libcurl4-openssl-dev \
     libgflags-dev \
     liblzma-dev \
     liblz4-dev \
     liblz4-tool \
     libnss3-tools \
     libreadline-dev \
     libsnappy-dev \
     libsqlite3-dev \
     libssl-dev \
     libssl-dev \
     libtool \
     libtree-sitter-dev \
     libudev-dev \
     libzstd-dev \
     linux-tools-common \
     linux-tools-generic \
     mkcert \
     ncal \
     net-tools \
     ninja-build \
     pkg-config \
     postgresql-client \
     python-dev-is-python3 \
     python3-pip \
     python3-venv \
     sqlite3 \
     tk \
     tmux \
     wireshark \
     zlib1g-dev \
     zsh \
     zstd 


# install oh-my-zsh (yeah, i still like it)
# if [ ! -d "~/.oh-my-zsh" ]; then
#     sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# fi

# copy over home directory files *after* installing oh-my-zsh
cd $SCRIPT_DIR
cp -v -R home/.gitconfig ~
cp -v -R home/.zshrc ~
cp -v -R home/bin ~

# symlink to emacs files
mkdir -p ~/.emacs.d/straight/version
ln -s $SCRIPT_DIR/home/.emacs.d/init.el ~/.emacs.d/init.el
ln -s $SCRIPT_DIR/home/.emacs.d/early-init.el ~/.emacs.d/early-init.el
ln -s $SCRIPT_DIR/home/.emacs.d/straight/version/default.el  ~/.emacs.d/straight/version/default.el

sudo chsh -s /usr/bin/zsh $(whoami)

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
    cargo install --locked sccache
    cargo install --locked bottom
    cargo install --locked critcmp
    cargo install --locked flamegraph
    cargo install --locked cargo-deny
fi

# lower the paranoia level so we can get reasonable flamegraphs
echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid
echo 0 | sudo tee /proc/sys/kernel/kptr_restrict

SRC_HOME=~/src
mkdir -p $SRC_HOME

#############################
# install mold compiler. instructions taken from 
# https://github.com/rui314/mold
# only for linux

# there used to be/still exist known problems with mold, ubuntu, and clang/gcc:
# https://github.com/rui314/mold/issues/1025
# https://github.com/michaelsproul/mold-stdcpp/blob/main/.cargo/config.toml
#
# but according to this SO, just installing an updated version of gcc's libstdc++ solves it:
# https://stackoverflow.com/questions/67712376/after-updating-gcc-clang-cant-find-libstdc-anymore
# might also want to install an updated version of llvm, as well (see ./setup-llvm.sh)
sudo apt install libstdc++-12-dev

MOLD_HOME=$SRC_HOME/mold
if [ ! -d "$MOLD_HOME" ]; then
    cd $SRC_HOME
    git clone https://github.com/rui314/mold.git
    mkdir mold/build
    cd mold/build
    git checkout v2.35.1
    sudo ../install-build-deps.sh
    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=c++ ..
    cmake --build . -j $(nproc)
    sudo cmake --install .

    # now add to rust toolchain
    mkdir -p $HOME/.cargo
    cat <<EOF >> $HOME/.cargo/config.toml
[target.x86_64-unknown-linux-gnu]
# apparently i need this as-per `flamegraph-rs`, the `--no-resegment` flag, for mold
rustflags = ["-C", "link-arg=-fuse-ld=mold", "-C", "link-arg=-Wl,--no-rosegment"]

# safe version that uses gcc
# rustflags = ["-C", "link-arg=-fuse-ld=mold"]
EOF

fi


#############################
## install flamegraphs

if [ ! -d "$SRC_HOME/FlameGraph" ]; then
    cd $SRC_HOME
    git clone git@github.com:brendangregg/FlameGraph.git
fi


#############################
# install other programming languages (or at least the tools to make it bearable)

# java and maven
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk install java 21.0.5-zulu
sdk install maven


# python
curl https://pyenv.run | bash
exec "$SHELL"
pyenv update
pyenv install 3.13

# install python lsp packages
python3 -m pip install python-lsp-server pylsp-mypy python-lsp-black python-lsp-ruff pylsp-rope

# add to $PATH
