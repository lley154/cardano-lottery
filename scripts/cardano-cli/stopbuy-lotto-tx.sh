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
    echo "stopbuy-lotto-tx-.sh:  Invalid script arguments"
    echo "Usage: stopbuy-lotto-tx.sh [devnet|testnet|mainnet]"
    exit 1
fi
ENV=$1

# Pull in global export variables
MY_DIR=$(dirname $(readlink -f $0))
source $MY_DIR/$ENV/global-export-variables.sh

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"

ls -al "$CARDANO_NODE_SOCKET_PATH"
mkdir -p $WORK
mkdir -p $WORK-backup
cp -f $WORK/* $WORK-backup


# generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters --testnet-magic $TESTNET_MAGIC --out-file $WORK/pparms.json

# load in local variable values
# You will need to re-run Deploy.hs if adminPkh has changed   This is because both the lotto 
# and buy validator scripts are parameterized which include adminPkh.  When adminPKH changes
# so does the validator hashs and addresses.
lotto_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lotto-validator.plutus"
lotto_validator_hash=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-validator.hash)
buy_validator_script="$BASE/scripts/cardano-cli/$ENV/data/buy-validator.plutus"
lotto_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lotto_validator_script"  --testnet-magic "$TESTNET_MAGIC")
buy_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$buy_validator_script"  --testnet-magic "$TESTNET_MAGIC")
lotto_redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-lotto-stopbuy.json"
lotto_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-token-name.json | jq -r '.bytes')
lotto_token_value=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-token-value.json)
buy_redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-buy-transfertoken.json"
buy_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-name.json | jq -r '.bytes')
buy_token_value=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-value.json)
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.hash | jq -r '.bytes')
admin_pkh=$(cat $ADMIN_PKH)


echo "starting lotto stop buy"

# Step 1: Get UTXOs from lotto admin
admin_utxo_addr=$($CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/admin-utxo.json
cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')


# Step 2: Get the UTXOs from the script address
$CARDANO_CLI query utxo --address $lotto_validator_script_addr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/lotto-validator-utxo.json

lotto_validator_utxo_tx_in_0=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$lotto_token_name'") 
| .key' $WORK/lotto-validator-utxo.json)

lotto_validator_utxo_tx_in_1=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$buy_token_name'") 
| .key' $WORK/lotto-validator-utxo.json)

buy_total_value=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$buy_token_name'") 
| .value.value.lovelace' $WORK/lotto-validator-utxo.json)



# Step 3: Get the current datum from the utxo at the script address
# TODO - filter for only utxo with thread token
if [ "$ENV" == "devnet" ];
then
    cp $WORK/lotto-datum-out.json $WORK/lotto-datum-in.json
    cp $WORK/buy-datum-out.json $WORK/buy-datum-in.json
elif [ "$ENV" == "testnet"]; 
then
    curl -H "project_id: $PROJECT_ID" "https://cardano-testnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
    datum_hash_0=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$datum_hash_0" | jq -c .json_value > $WORK/lotto-datum-in.json

    datum_hash_1=$(jq -r '.[1].data_hash' $WORK/lotto-datum-in.hash)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$datum_hash_1" | jq -c .json_value > $WORK/buy-datum-in.json

elif [ "$ENV" == "mainnet"];
then
    curl -H "project_id: $PROJECT_ID" "https://cardano-mainnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
    datum_hash_0=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/$datum_hash_0" | jq -c .json_value > $WORK/lotto-datum-in.json

    datum_hash_1=$(jq -r '.[1].data_hash' $WORK/lotto-datum-in.hash)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/$datum_hash_1" | jq -c .json_value > $WORK/buy-datum-in.json
else
    echo "No environment selected"
    exit 1
fi


jackpot=$(jq -r '.fields[2].int' $WORK/lotto-datum-in.json)
treasury=$(jq -r '.fields[4].int' $WORK/lotto-datum-in.json)
fees=$(jq -r '.fields[5].int' $WORK/lotto-datum-in.json)
percentFees=$(jq -r '.fields[0].fields[7].int' $WORK/lotto-datum-in.json) 
new_jackpot=$(($jackpot + ($buy_total_value * ((100 - $percentFees)/2))/100))
new_treasury=$(($treasury + ($buy_total_value * ((100 - $percentFees)/2))/100))
new_fees=$(($fees + ($buy_total_value * $percentFees)/100))


# upate the datum accordingly for the open state
cat $WORK/lotto-datum-in.json | \
jq -c '
  .fields[2].int                |= '$new_jackpot'
| .fields[4].int                |= '$new_treasury'
| .fields[5].int                |= '$new_fees'' > $WORK/lotto-datum-out.json

lotto_value=$(jq -r '.fields[2].int + .fields[4].int + .fields[5].int' $WORK/lotto-datum-out.json)


# Step 4: Build and submit the transaction
$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$admin_utxo_in" \
  --tx-in "$lotto_validator_utxo_tx_in_0" \
  --tx-in-script-file "$lotto_validator_script" \
  --tx-in-datum-file "$WORK/lotto-datum-in.json" \
  --tx-in-redeemer-file "$lotto_redeemer_file_path" \
  --tx-in "$lotto_validator_utxo_tx_in_1" \
  --tx-in-script-file "$lotto_validator_script" \
  --tx-in-datum-file "$WORK/lotto-datum-in.json" \
  --tx-in-redeemer-file "$lotto_redeemer_file_path" \
  --tx-in-collateral "$ADMIN_COLLATERAL" \
  --tx-out "$lotto_validator_script_addr+$lotto_value + 1 $thread_token_mph.$lotto_token_name + 1 $thread_token_mph.$buy_token_name" \
  --tx-out-datum-embed-file "$WORK/lotto-datum-out.json" \
  --change-address "$admin_utxo_addr" \
  --required-signer-hash "$admin_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/stopbuy-tx-alonzo.body
  

# --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/stopbuy-alonzo.costs"

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/stopbuy-tx-alonzo.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/stopbuy-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/stopbuy-tx-alonzo.tx --testnet-magic "$TESTNET_MAGIC"



