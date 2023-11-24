#!/bin/bash
# make sure to ssh with -A flag

set -eu pipefail
set -x

LLVM_DIR="/opt/dev/llvm"
LLVM_VERSION="16"

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
cmake .
cmake --build .

# update the cargo build instuctions
