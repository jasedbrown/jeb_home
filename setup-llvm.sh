#!/bin/bash

##########################
# install an updated version of the llvm toolchain.
# you _might_ need to 'apt remove llvm' any system-default install
# of llvm as i've seen older versions completely eff up
# the lldb-mi build (terribly).

set -eu pipefail
set -x

LLVM_DIR="/opt/dev/llvm"
LLVM_VERSION="19"

mkdir -p $LLVM_DIR
cd $LLVM_DIR

# download llvm binaries via a nice script (https://apt.llvm.org/)
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh $LLVM_VERSION all

sudo apt install liblldb-$LLVM_VERSION liblldb-$LLVM_VERSION-dev

# change all the system syslinks to the new llvm
sudo bash /home/jasobrown/bin/update_alternatives_llvm.sh $LLVM_VERSION 1

# now clone the llvm-mi repo and build
git clone git@github.com:lldb-tools/lldb-mi.git
cd $LLVM_DIR/lldb-mi

mkdir build
cd build
cmake ..
make

# copy the binary to a location that will be on the emacs exec-path
sudo cp src/lldb-mi /usr/local/bin

# update the cargo build instuctions
