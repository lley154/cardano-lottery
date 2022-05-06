#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enable debugging
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "close-lotto-tx-.sh:  Invalid script arguments"
    echo "Usage: close-lotto-tx.sh [devnet|testnet|mainnet]"
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
cp -f $WORK/* $WORK-backup


# generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters $network --out-file $WORK/pparms.json

# load in local variable values
lotto_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lotto-validator.plutus"
lotto_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lotto_validator_script" $network)
lotto_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-token-name.json | jq -r '.bytes')
buy_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-name.json | jq -r '.bytes')
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.hash | jq -r '.bytes')
redeemer_file="$BASE/scripts/cardano-cli/$ENV/data/redeemer-lotto-close.json"
admin_pkh=$(cat $ADMIN_PKH)

echo "starting lotto close"

# Step 1: Get UTXOs from lotto admin
# There needs to be at least 2 utxos that can be consumed
admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json
cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')


# Step 2: Get the UTXOs from the scrpit address
$CARDANO_CLI query utxo --address $lotto_validator_script_addr $network --out-file $WORK/lotto-validator-utxo.json

# pull out the utxo that has the lotto thread token in it
lotto_validator_utxo_tx_in=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$lotto_token_name'") 
| .key' $WORK/lotto-validator-utxo.json)


# Step 3: Get the current datum from the utxo at the script address
# TODO - filter for only utxo with thread token
if [ "$ENV" == "devnet" ];
then
    cp $WORK/lotto-datum-out.json $WORK/lotto-datum-in.json
elif [ "$ENV" == "testnet" ]; 
then
    curl -H "project_id: $PROJECT_ID" "https://cardano-testnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
    datum_hash=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$datum_hash" | jq -c .json_value > $WORK/lotto-datum-in.json
elif [ "$ENV" == "mainnet" ];
then
    curl -H "project_id: $PROJECT_ID" "https://cardano-mainnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
    datum_hash=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/$datum_hash" | jq -c .json_value > $WORK/lotto-datum-in.json
else
    echo "No environment selected"
    exit 1
fi

# upate the datum accordingly for the close state
cat $WORK/lotto-datum-in.json | \
jq -c ' .fields[7].int  |= 2 ' > $WORK/lotto-datum-out.json

lotto_value=$(jq -r '.fields[2].int + .fields[4].int + .fields[5].int' $WORK/lotto-datum-out.json)


# Step 4: Build and submit the transaction
$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  $network \
  --tx-in "$admin_utxo_in" \
  --tx-in "$lotto_validator_utxo_tx_in" \
  --tx-in-script-file "$lotto_validator_script" \
  --tx-in-datum-file "$WORK/lotto-datum-in.json" \
  --tx-in-redeemer-file "$redeemer_file" \
  --tx-in-collateral "$ADMIN_COLLATERAL" \
  --tx-out "$lotto_validator_script_addr+$lotto_value + 1 $thread_token_mph.$lotto_token_name + 1 $thread_token_mph.$buy_token_name" \
  --tx-out-datum-embed-file "$WORK/lotto-datum-out.json" \
  --change-address "$admin_utxo_addr" \
  --required-signer-hash "$admin_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/close-tx-alonzo.body

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/close-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/close-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/close-tx-alonzo.tx $network



