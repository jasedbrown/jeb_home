#!/bin/bash

DATA_ROOT="/var/lib/mongo"

sudo mkdir -p $DATA_ROOT
sudo chown -R jasobrown: $DATA_ROOT

MONGO_BIN="./build/install/bin"
if [ ! -d $MONGO_BIN ]; then
    # Mongo 4.0 or lower
    MONGO_BIN="./build.opt/mongo"
fi

MONGOD="${MONGO_BIN}/mongod"

for i in {1..3}
do
    REPL_HOME="${DATA_ROOT}/repl${i}"
    ${MONGOD} --shutdown --dbpath $REPL_HOME/data
done

