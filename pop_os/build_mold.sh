#!/bin/bash
# pop_os.sh

MOLD_VERSION="v2.40.4"
MOLD_DIR="/tmp/mold"
if [ ! -f "/usr/local/bin/mold" ]; then
    echo "Installing mold linker from source..."

    # Install mold linker dependencies
    # there used to be/still exist known problems with mold, ubuntu, and clang/gcc:
    # https://github.com/rui314/mold/issues/1025
    # https://github.com/michaelsproul/mold-stdcpp/blob/main/.cargo/config.toml
    #
    # but according to this SO, just installing an updated version of gcc's libstdc++ solves it:
    # https://stackoverflow.com/questions/67712376/after-updating-gcc-clang-cant-find-libstdc-anymore
    # might also want to install an updated version of llvm, as well (see ./setup-llvm.sh)
    sudo apt install -y libstdc++-12-dev

    git clone https://github.com/rui314/mold.git "$MOLD_DIR"
    cd "$MOLD_DIR"
    git checkout "$MOLD_VERSION"
    mkdir build
    cd build
    sudo ../install-build-deps.sh
    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=c++ ..
    cmake --build . -j $(nproc)
    sudo cmake --install .
    cd /tmp
    rm -rf "$MOLD_DIR"
fi
