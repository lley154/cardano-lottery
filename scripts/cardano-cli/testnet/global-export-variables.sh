#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Make sure the cardano node socket is available 
# eg. ssh -L  /home/lawrence/src/cardano-lottery/node.socket:/home/lawrence/.cardano-testnet-node/db/node.socket cardano-server


# Define export variables
export BASE=/home/lawrence/src/cardano-lottery
export WORK=$BASE/work

# PROJECT_ID is for blockfrost datum query, get your account here
# https://blockfrost.io/
export PROJECT_ID=$(cat $BASE/scripts/cardano-cli/testnet/blockfrost.id)
export CARDANO_CLI=/home/lawrence/.local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/src/cardano-lottery/node.socket
export TESTNET_MAGIC=1097911063
export ADMIN_VKEY=/home/lawrence/.local/keys/01.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/01.skey
export ADMIN_PKH=/home/lawrence/.local/keys/01.pkh
export PLAYER_VKEY=/home/lawrence/.local/keys/02.vkey
export PLAYER_SKEY=/home/lawrence/.local/keys/02.skey
export PLAYER_PKH=/home/lawrence/.local/keys/02.pkh
export SPONSOR_VKEY=/home/lawrence/.local/keys/03.vkey
export SPONSOR_SKEY=/home/lawrence/.local/keys/03.skey
export SPONSOR_PKH=/home/lawrence/.local/keys/03.pkh
export MIN_ADA_OUTPUT_TX=2000000
export COLLATERAL_ADA=10000000
export ADMIN_COLLATERAL=fd5cc20668b84aaf4fe87b01d7075fc250e93f790664fd95d3ce45b703db2ad0#1
export PLAYER_COLLATERAL=ffd0511a84b2e10936e4a4129429224302cb55f6c4b2ca4a7a509b67c839cc54#1
export SPONSOR_COLLATERAL=bb156c1bd9027d8dc3d3e07af194826d9ea156bc5ac4deed3082bca42e6fc422#1



