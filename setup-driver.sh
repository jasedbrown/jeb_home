#!/bin/bash
# make sure to ssh with -A flag

set -euo pipefail
set -x

SCRIPT_DIR=$(pwd)


#############################
## setup some directories for container output

sudo mkdir /var/lib/grafana
sudo chown $(whoami)  /var/lib/grafana

sudo mkdir /var/lib/prometheus
sudo chown $(whoami)  /var/lib/prometheus
