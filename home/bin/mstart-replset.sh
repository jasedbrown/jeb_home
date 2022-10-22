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

# this doesn't work with Mongo 6.0+
MONGO_SHELL="${MONGO_BIN}/mongo"

for i in {1..3}
do
    REPL_HOME="${DATA_ROOT}/repl${i}"
    mkdir -p $REPL_HOME/log
    mkdir -p $REPL_HOME/data

    REPL_IP="127.0.0.${i}"

    ${MONGOD} --fork --profile=1 --slowms=100000 replSet=shard_jasobrown --noscripting --depath $REPL_HOME/data --bindIp $REPL_IP --port 9902 --logpath $REPL_HOME/log/mongo.log --logRotate --logappend 
done

${MONGO_SHELL} 127.0.0.1:9902 ~/bin/initiate.js

