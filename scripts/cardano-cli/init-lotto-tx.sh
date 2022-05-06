#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "init-lotto-tx-.sh:  Invalid script arguments"
    echo "Usage: init-lotto-tx.sh [devnet|testnet|mainnet]"
    exit 1
fi
ENV=$1

# Pull in global export variables
MY_DIR=$(dirname $(readlink -f $0))
source $MY_DIR/$ENV/global-export-variables.sh

if [ "$ENV" == "mainnet" ];
then
    network="--mainnet"
else
    network="--testnet-magic $TESTNET_MAGIC"
fi

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"

ls -al "$CARDANO_NODE_SOCKET_PATH"

mkdir -p $WORK
mkdir -p $WORK-backup
rm -f $WORK/*
rm -f $WORK-backup/*

########################################################################################
# Lotto Settings - START
########################################################################################
# Please note that changes to admin_pkh & ticket_cost will also
# required to update Deploy.hs then compile and deploy new a new buy script

#deadline=1596109091999
ticket_cost=20000
initial_funding=10000000
fee_percentage=2


########################################################################################
# Lotto Settings - END
########################################################################################

# generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters $network --out-file $WORK/pparms.json

# load in local variable values
lotto_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lotto-validator.plutus"
minting_script="$BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.plutus"
lotto_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.hash | jq -r '.bytes')
lotto_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lotto_validator_script" $network)
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-thread-token-mint.json"
lotto_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-token-name.json | jq -r '.bytes')
buy_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-name.json | jq -r '.bytes')
admin_pkh=$(cat $ADMIN_PKH)
sponsor_pkh=$(cat $SPONSOR_PKH)


echo "starting lotto init"
echo "Script: $lotto_validator_script"

# Step 1: Get UTXOs from lotto admin
# There needs to be at least 2 utxos that can be consumed
admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json
cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')

cp $BASE/scripts/cardano-cli/$ENV/data/lotto-datum-init.datum $WORK/lotto-datum-out.json


# Step 2: Build and submit the transaction
$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  $network \
  --tx-in "$admin_utxo_in" \
  --tx-in-collateral "$ADMIN_COLLATERAL" \
  --mint-script-file "$minting_script" \
  --mint-redeemer-file "$redeemer_file_path" \
  --tx-out "$lotto_validator_script_addr+$initial_funding + 1 $lotto_mph.$lotto_token_name + 1 $lotto_mph.$buy_token_name" \
  --tx-out-datum-embed-file "$WORK/lotto-datum-out.json"  \
  --mint "1 $lotto_mph.$lotto_token_name + 1 $lotto_mph.$buy_token_name" \
  --change-address "$admin_utxo_addr" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/init-tx-alonzo.body
  
# --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/init-alonzo.costs"

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/init-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/init-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/init-tx-alonzo.tx $network



