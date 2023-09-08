#!/bin/bash
# make sure to ssh with -A flag

set -euo pipefail
set -x

SCRIPT_DIR=$(pwd)

##############################
# install extra applications for benchmarking

sudo DEBIAN_FRONTEND=noninteractive apt install -y \
     memcached \
     pgbouncer \
     redis-server

## TODO: we need our own custom pgbouncer config file,
## which will be copied over here.

SRC_HOME=~/src


#############################
## install pgcat. we need to clone the repo and build ourselves.

PGCAT_HOME=$SRC_HOME/pgcat
if [ ! -d "$PGCAT_HOME" ]; then
    cd $SRC_HOME
    git clone git@github.com:postgresml/pgcat.git
    cd pgcat
    git checkout v1.1.1
    cargo build --release

    # copy over our hand-built config
    cp $SCRIPT_DIR/home/configs/pgcat/pgcat.benching-config.toml ./pgcat.toml
fi


