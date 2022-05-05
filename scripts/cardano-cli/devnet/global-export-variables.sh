#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Make sure the cardano node socket is available 
# ssh -L  /home/lawrence/src/cardano-lottery-private/node.sock:/home/lawrence/.cardano-testnet-node/db/node.socket cardano-server


# Define export variables

# PROJECT_ID is for blockfrost datum query, get your account here
# https://blockfrost.io/
export PROJECT_ID=testnetOo1ri1RaUAfnslati2pZDrLPdnURwaBu
export BASE=/home/lawrence/src/cardano-lottery-private
export WORK=$BASE/work
export CARDANO_CLI=/home/lawrence/.local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/src/cardano-node/run/current/node-0/node.socket
export TESTNET_MAGIC=42
export ADMIN_VKEY=/home/lawrence/.local/keys/01.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/01.skey
export ADMIN_PKH=/home/lawrence/.local/keys/01.pkh
export PLAYER_VKEY=/home/lawrence/.local/keys/04.vkey
export PLAYER_SKEY=/home/lawrence/.local/keys/04.skey
export PLAYER_PKH=/home/lawrence/.local/keys/04.pkh
export SPONSOR_VKEY=/home/lawrence/.local/keys/03.vkey
export SPONSOR_SKEY=/home/lawrence/.local/keys/03.skey
export SPONSOR_PKH=/home/lawrence/.local/keys/03.pkh
export TICKET_MIN_VALUE=1500000
export MIN_ADA_OUTPUT_TX=2000000
export COLLATERAL_ADA=10000000
export ADMIN_COLLATERAL=a1bd084c298ac62cd16bdc1a0a2f48242e3b0c6dd1dcd89295ba49c0bed6dc8f#2
export PLAYER_COLLATERAL=a1bd084c298ac62cd16bdc1a0a2f48242e3b0c6dd1dcd89295ba49c0bed6dc8f#6
export SPONSOR_COLLATERAL=a1bd084c298ac62cd16bdc1a0a2f48242e3b0c6dd1dcd89295ba49c0bed6dc8f#4



