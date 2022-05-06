#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Make sure the cardano node socket is available 
# eg. ssh -L  /home/lawrence/src/cardano-lottery/node.socket:/home/lawrence/.cardano-testnet-node/db/node.socket cardano-server


# Define export variables
export BASE=
export WORK=$BASE/work

# PROJECT_ID is for blockfrost datum query, get your account here
# https://blockfrost.io/
export PROJECT_ID=$(cat $BASE/scripts/cardano-cli/testnet/blockfrost.id)
export CARDANO_CLI=
export CARDANO_NODE_SOCKET_PATH=
export ADMIN_VKEY=
export ADMIN_SKEY=
export ADMIN_PKH=
export PLAYER_VKEY=
export PLAYER_SKEY=
export PLAYER_PKH=
export SPONSOR_VKEY=
export SPONSOR_SKEY=
export SPONSOR_PKH=
export MIN_ADA_OUTPUT_TX=2000000
export COLLATERAL_ADA=10000000
export ADMIN_COLLATERAL=
export PLAYER_COLLATERAL=
export SPONSOR_COLLATERAL=



