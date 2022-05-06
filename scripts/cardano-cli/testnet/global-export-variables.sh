#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Make sure the cardano node socket is available 
# eg. ssh -L  /home/lawrence/src/cardano-lottery/node.socket:/home/lawrence/.cardano-testnet-node/db/node.socket cardano-server


# Define export variables

# PROJECT_ID is for blockfrost datum query, get your account here
# https://blockfrost.io/
export PROJECT_ID=testnetOo1ri1RaUAfnslati2pZDrLPdnURwaBu
export BASE=/home/lawrence/src/cardano-lottery
export WORK=$BASE/work
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
export ADMIN_COLLATERAL=4bc07d353abd7ef6bca96162cab6d21e498791d82fcf6880a6440429f426308a#0
export PLAYER_COLLATERAL=53f53c2c6cd6b17db4c6b9c359bfc49e70a9b661d247d2988347fd2aa227ad67#0
export SPONSOR_COLLATERAL=c248d582e29c03aa80d2288c0bad795f24abc5c027c078edef3a34890e5b3054#0



