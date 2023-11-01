#!/bin/bash
# make sure to ssh with -A flag

set -euo pipefail
set -x

LLVM_DIR="/opt/dev/llvm"
LLVM_VERSION="16"

mkdir $LLVM_DIR
cd $LLVM_DIR

# download llvm binaries via a nice script (https://apt.llvm.org/)
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh $LLVM_VERSION all

# change all the system syslinks to the new llvm
source ~/bin/update_alternatives_llvm.sh

# now clone the llvm-mi repo and build
git clone git@github.com:lldb-tools/lldb-mi.git
cd llvm-mi
cmake .
cmake --build .

# update the cargo build instuctions
