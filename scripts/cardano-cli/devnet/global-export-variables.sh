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
export CARDANO_CLI=/home/lawrence/.local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/src/cardano-node/run/current/node-0/node.socket
export TESTNET_MAGIC=42
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
export ADMIN_COLLATERAL=6ac3b15cd5b82fabbb5bbaa3bc25933f43ba50422cf2268ea522e563050529f4#2
export PLAYER_COLLATERAL=6ac3b15cd5b82fabbb5bbaa3bc25933f43ba50422cf2268ea522e563050529f4#4
export SPONSOR_COLLATERAL=6ac3b15cd5b82fabbb5bbaa3bc25933f43ba50422cf2268ea522e563050529f4#6



