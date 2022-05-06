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
    echo "payout-lotto-tx-.sh:  Invalid script arguments"
    echo "Usage: payout-lotto-tx.sh [devnet|testnet|mainnet] [p|s]    where p = player, s = sponsor"
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
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-lotto-payout.json"
lotto_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-token-name.json | jq -r '.bytes')
lotto_token_value=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-token-value.json)
buy_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-name.json | jq -r '.bytes')
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.hash | jq -r '.bytes')


echo "starting lotto payout"

if [ "$2" == "p" ];
then
    # Step 1: Get UTXOs from player 
    user_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$PLAYER_VKEY")
    $CARDANO_CLI query utxo --address $user_addr $network --out-file $WORK/player-utxo.json
    cat $WORK/player-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/player-utxo-valid.json
    readarray player_utxo_valid_array < $WORK/player-utxo-valid.json
    user_utxo_in=$(echo $player_utxo_valid_array | tr -d '\n')
    user_collateral=$PLAYER_COLLATERAL
    user_pkh=$(cat $PLAYER_PKH)
    user_skey=$PLAYER_SKEY

elif [ "$2" == "s" ]; 
then
    # Step 1: Get UTXOs from sponsor
    user_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$SPONSOR_VKEY")
    $CARDANO_CLI query utxo --address $user_addr $network --out-file $WORK/sponsor-utxo.json
    cat $WORK/sponsor-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/sponsor-utxo-valid.json
    readarray sponsor_utxo_valid_array < $WORK/sponsor-utxo-valid.json
    user_utxo_in=$(echo $sponsor_utxo_valid_array | tr -d '\n')
    user_collateral=$SPONSOR_COLLATERAL
    user_pkh=$(cat $SPONSOR_PKH)
    user_skey=$SPONSOR_SKEY
else
     echo "Neither (p)layer nor (s)ponsor was selected"
     exit 1
fi

# Step 2: Get the UTXOs from the script address
# TODO - filter for only utxo with thread token
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

# get the first pkh in the datum map
pkh0=$(jq -r '.fields[6].map[0].k.bytes' $WORK/lotto-datum-in.json)
pkh1=$(jq -r '.fields[6].map[1].k.bytes' $WORK/lotto-datum-in.json)

jackpot=$(jq -r '.fields[2].int' $WORK/lotto-datum-in.json)

# determine payout for player
if [ "$2" == "p" ];
then
    if [ $pkh0 == $(cat $PLAYER_PKH) ];
    then
        win_amount=$(jq '.fields[6].map[0].v.int' $WORK/lotto-datum-in.json)
        sponsor_amount=$(jq '.fields[6].map[1].v.int' $WORK/lotto-datum-in.json)
        new_jackpot=$((jackpot - win_amount))

        cat $WORK/lotto-datum-in.json | \
        jq -c '
          .fields[6].map                |= [{"k":{"bytes":"'$pkh1'"},"v":{"int":'$sponsor_amount'}}]
        | .fields[2].int                |= '$new_jackpot' 
        ' > $WORK/lotto-datum-out.json
    else
        echo "No player pkh found in beneficary map"
    fi

fi


# determine payout for sponsor
if [ "$2" == "s" ];
then
    if [ $pkh0 == $(cat $SPONSOR_PKH) ];
    then
        win_amount=$(jq '.fields[6].map[0].v.int' $WORK/lotto-datum-in.json)
        new_jackpot=$((jackpot - win_amount))

        cat $WORK/lotto-datum-in.json | \
        jq -c '
        .fields[6].map                  |= []
        | .fields[2].int                |= '$new_jackpot' 
        ' > $WORK/lotto-datum-out.json
    elif [ $pkh1 == $(cat $SPONSOR_PKH) ];
    then
        win_amount=$(jq '.fields[6].map[1].v.int' $WORK/lotto-datum-in.json)
        new_jackpot=$((jackpot - win_amount))
        player_amount=$(jq '.fields[6].map[0].v.int' $WORK/lotto-datum-in.json)

        cat $WORK/lotto-datum-in.json | \
        jq -c '
        .fields[6].map                  |= [{"k":{"bytes":"'$pkh0'"},"v":{"int":'$player_amount'}}]
        | .fields[2].int                |= '$new_jackpot' 
        ' > $WORK/lotto-datum-out.json
    else
        echo "Sponsor public key has not found in the lottery"
        exit 1
    fi
fi

lotto_value=$(jq -r '.fields[2].int + .fields[4].int + .fields[5].int' $WORK/lotto-datum-out.json)


# Step 4: Build and submit the transaction
$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  $network \
  --tx-in "$user_utxo_in" \
  --tx-in "$lotto_validator_utxo_tx_in" \
  --tx-in-script-file "$lotto_validator_script" \
  --tx-in-datum-file "$WORK/lotto-datum-in.json" \
  --tx-in-redeemer-file "$redeemer_file_path" \
  --tx-in-collateral "$user_collateral" \
  --tx-out "$lotto_validator_script_addr+$lotto_value + 1 $thread_token_mph.$lotto_token_name + 1 $thread_token_mph.$buy_token_name" \
  --tx-out-datum-embed-file "$WORK/lotto-datum-out.json" \
  --tx-out "$user_addr+$win_amount" \
  --change-address "$user_addr" \
  --required-signer-hash "$user_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/payout-tx-alonzo.body
  

# --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/payout-alonzo.costs"

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/payout-tx-alonzo.body \
  $network \
  --signing-key-file "${user_skey}" \
  --out-file $WORK/payout-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/payout-tx-alonzo.tx $network



