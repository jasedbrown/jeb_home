#!/bin/bash

for i in pgbouncer.service redis-server.service memcached.service; do sudo systemctl stop $i; done
