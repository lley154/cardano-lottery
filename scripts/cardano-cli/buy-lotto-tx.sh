#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# Enabled debug flag for bash shell
set -x

# Check if command line argument is empty or not present
if [ -z $1 ]; then
    echo "buy-lotto-tx-.sh:  Invalid script arguments"
    echo "Usage: buy-lotto-tx.sh [devnet|testnet|mainnet] [n]    where n = ticket number "
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


# Generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters --testnet-magic $TESTNET_MAGIC --out-file $WORK/pparms.json
lotto_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lotto-validator.plutus"
buy_validator_script="$BASE/scripts/cardano-cli/$ENV/data/buy-validator.plutus"
minting_script="$BASE/scripts/cardano-cli/$ENV/data/ticket-policy.plutus"
lotto_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lotto_validator_script"  --testnet-magic "$TESTNET_MAGIC")
buy_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$buy_validator_script"  --testnet-magic "$TESTNET_MAGIC")
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-buy-ticket.json"
redeemer_mint_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-ticket-mint.json"
buy_validator_addr=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-val-addr.json)
buy_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-name.json | jq -r '.bytes')
buy_token_value=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-value.json)
ticket_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/ticket-policy.hash | jq -r '.bytes')
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.hash | jq -r '.bytes')
player_pkh=$(cat $PLAYER_PKH)

echo "starting lotto buy"

# Step 1: Get UTXOs from player 
player_addr=$($CARDANO_CLI address build --testnet-magic $TESTNET_MAGIC --payment-verification-key-file $PLAYER_VKEY)
$CARDANO_CLI query utxo --address $player_addr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/player-utxo.json
cat $WORK/player-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/player-utxo-valid.json
readarray player_utxo_valid_array < $WORK/player-utxo-valid.json
player_utxo_in=$(echo $player_utxo_valid_array | tr -d '\n')

# Step 2: Get the UTXOs from the script address
$CARDANO_CLI query utxo --address $buy_validator_script_addr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/buy-validator-utxo.json

# Pull the utxo with the buy token in it
buy_validator_utxo_tx_in=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$buy_token_name'") 
| .key' $WORK/buy-validator-utxo.json)


# Step 3: Get the current datums from the utxo at the script addresses
# TODO - filter for only utxo with thread token
if [ "$ENV" == "devnet" ];
then
    cp $WORK/lotto-datum-out.json $WORK/lotto-datum-in.json
    cp $WORK/buy-datum-out.json $WORK/buy-datum-in.json
elif [ "$ENV" == "testnet" ]; 
then
    curl -H "project_id: $PROJECT_ID" "https://cardano-testnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
    datum_hash=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$datum_hash" | jq -c .json_value > $WORK/lotto-datum-in.json

    curl -H "project_id: $PROJECT_ID" "https://cardano-testnet.blockfrost.io/api/v0/addresses/$buy_validator_script_addr/utxos" > $WORK/buy-utxo-in.json
    buy_datum_hash=$(jq -r '.[0].data_hash' $WORK/buy-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$buy_datum_hash" | jq -c .json_value > $WORK/buy-datum-in.json
    
    # Check if this is the first buy, if so, then need to copy buy datum from prior output
    # This is needed because the startbuy only sends a datum hash and not the actual datum
    buy_datum_test=$(cat $WORK/buy-datum-in.json)
    if [ "$buy_datum_test" == "null" ];
    then 
        cp $WORK/buy-datum-out.json $WORK/buy-datum-in.json
    fi
elif [ "$ENV" == "mainnet" ];
then
    curl -H "project_id: $PROJECT_ID" "https://cardano-mainnet.blockfrost.io/api/v0/addresses/$lotto_validator_script_addr/utxos" > $WORK/lotto-utxo-in.json
    datum_hash=$(jq -r '.[0].data_hash' $WORK/lotto-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/$datum_hash" | jq -c .json_value > $WORK/lotto-datum-in.json

    curl -H "project_id: $PROJECT_ID" "https://cardano-mainnet.blockfrost.io/api/v0/addresses/$buy_validator_script_addr/utxos" > $WORK/buy-utxo-in.json
    buy_datum_hash=$(jq -r '.[0].data_hash' $WORK/buy-utxo-in.json)
    curl -H "project_id: $PROJECT_ID" \
    "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/$buy_datum_hash" | jq -c .json_value > $WORK/buy-datum-in.json
    
    # Check if this is the first buy, if so, then need to copy buy datum from prior output
    # This is needed because the startbuy only sends a datum hash and not the actual datum
    buy_datum_test=$(cat $WORK/buy-datum-in.json)
    if [ "$buy_datum_test" == "null" ];
    then 
        cp $WORK/buy-datum-out.json $WORK/buy-datum-in.json
    fi
else
    echo "No environment selected"
    exit 1
fi

# Step 4: Get values from the lotto and buy script datums  
ticket_cost=$(jq -r '.fields[0].fields[8].int' $WORK/lotto-datum-in.json)
seq_num=$(jq -r '.fields[3].int' $WORK/lotto-datum-in.json)
ticket_num="$seq_num$2"
ticket_num_hex=$(echo -n "$ticket_num" | xxd -ps)
ticket_total=$(jq -r '.fields[0].int' $WORK/buy-datum-in.json)
total_value=$(jq -r '.fields[1].int' $WORK/buy-datum-in.json)
new_ticket_total=$(($ticket_total + 1))
new_total_value=$(($total_value + ($ticket_cost * 100)))


# Upate the buy datum accordingly
cat $WORK/buy-datum-in.json | \
jq -c '
  .fields[0].int   |= '$new_ticket_total'
| .fields[1].int   |= '$new_total_value'' > $WORK/buy-datum-out.json
 
# Upate the redeemer mint with the ticket number to be purchased
cat $redeemer_mint_file_path | \
jq -c '
  .fields[0].constructor    |= '1'
| .fields[1].bytes          |= "'$ticket_num_hex'"' > $WORK/redeemer-ticket-mint.json



# Step 5: Build and submit the transaction
$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$player_utxo_in" \
  --tx-in "$buy_validator_utxo_tx_in" \
  --tx-in-script-file "$buy_validator_script" \
  --tx-in-datum-file "$WORK/buy-datum-in.json" \
  --tx-in-redeemer-file "$redeemer_file_path" \
  --mint-script-file "$minting_script" \
  --mint-redeemer-file "$WORK/redeemer-ticket-mint.json" \
  --tx-in-collateral "$PLAYER_COLLATERAL" \
  --tx-out "$buy_validator_script_addr+$new_total_value + 1 $thread_token_mph.$buy_token_name" \
  --tx-out-datum-embed-file "$WORK/buy-datum-out.json" \
  --tx-out "$player_addr+$MIN_ADA_OUTPUT_TX + 1 $ticket_mph.$ticket_num_hex" \
  --change-address "$player_addr" \
  --mint "1 $ticket_mph.$ticket_num_hex" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/buy-tx-alonzo.body
  

# --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/buy-alonzo.costs"

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/buy-tx-alonzo.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${PLAYER_SKEY}" \
  --out-file $WORK/buy-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/buy-tx-alonzo.tx --testnet-magic "$TESTNET_MAGIC"



