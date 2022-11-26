#!/bin/bash

#-------------------------------------------------------------
# installs gemmini
#
# run location: circle ci docker image
#-------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

source $SCRIPT_DIR/enable-conda.sh

cd $HOME
rm -rf chipyard
git clone --progress --verbose https://github.com/ucb-bar/chipyard.git chipyard
cd $LOCAL_CHIPYARD_DIR

git fetch
git checkout $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)

./build-setup.sh esp-tools

source env.sh

cd toolchains/esp-tools/riscv-isa-sim/build
git checkout $(cat $LOCAL_CHECKOUT_DIR/SPIKE.hash)
make && make install

cd $LOCAL_CHECKOUT_DIR
chown -R $(whoami) .
git config --global --add safe.directory $LOCAL_CHECKOUT_DIR
git submodule update --init --recursive software/gemmini-rocc-tests
rm -rf $LOCAL_CHIPYARD_DIR/generators/gemmini/* $LOCAL_CHIPYARD_DIR/generators/gemmini/.git*
mv -f $LOCAL_CHECKOUT_DIR/* $LOCAL_CHECKOUT_DIR/.git* $LOCAL_CHIPYARD_DIR/generators/gemmini/

