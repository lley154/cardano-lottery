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
    echo "redeem-lotto-tx-.sh:  Invalid script arguments"
    echo "Usage: redeem-lotto-tx.sh [devnet|testnet|mainnet]"
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
lotto_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lotto_validator_script"  --testnet-magic "$TESTNET_MAGIC")
minting_script="$BASE/scripts/cardano-cli/$ENV/data/ticket-policy.plutus"
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-lotto-redeem.json"
redeemer_mint_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-ticket-burn.json"
lotto_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-token-name.json | jq -r '.bytes')
buy_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/buy-token-name.json | jq -r '.bytes')
ticket_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/ticket-policy.hash | jq -r '.bytes')
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lotto-thread-token-policy.hash | jq -r '.bytes')
player_pkh=$(cat $PLAYER_PKH)


echo "starting lotto redeem"

# Step 1: Get the UTXOs from the script address
$CARDANO_CLI query utxo --address $lotto_validator_script_addr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/lotto-validator-utxo.json

# pull out the utxo that has the lotto thread token in it
lotto_validator_utxo_tx_in=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$lotto_token_name'") 
| .key' $WORK/lotto-validator-utxo.json)


# Step 2: Get the current datum from the utxo at the script address
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


winners=$(jq -r '.fields[1]' $WORK/lotto-datum-in.json)
difficulty=$(jq -r '.fields[0].fields[9].int' $WORK/lotto-datum-in.json)
winning_numbers=$(jq -r -c '.fields[8].list ' $WORK/lotto-datum-in.json | jq -c 'limit('$difficulty';.[])')
win_num=$(echo $winning_numbers | jq '.int' | tr -d '\n')
sponsor_pkh=$(jq -r '.fields[0].fields[1].bytes' $WORK/lotto-datum-in.json)
seq_num=$(jq -r '.fields[3].int' $WORK/lotto-datum-in.json)
ticket_num="$seq_num$win_num"
ticket_num_hex=$(echo -n "$ticket_num" | xxd -ps)

# upate the datum accordingly for the redeem state
winning_numbers_array=$(jq -r -c '.fields[8].list ' $WORK/lotto-datum-in.json | jq -c ['limit('$difficulty';.[])'])
cat $WORK/lotto-datum-in.json | \
jq -c '
.fields[1].list               |= [{"fields":[{"bytes":"'$sponsor_pkh'"},{"list":[]}], "constructor": 0}
                                 ,{"fields":[{"bytes":"'$player_pkh'"},{"list":'$winning_numbers_array'}], "constructor": 0}
                                 ]' > $WORK/lotto-datum-out.json

lotto_value=$(jq -r '.fields[2].int + .fields[4].int + .fields[5].int' $WORK/lotto-datum-out.json)

# upate the redeemer ticket to be validated 
cat $redeemer_file_path | \
jq -c '
.fields[0].bytes          |= "'$ticket_num_hex'"' > $WORK/redeemer-lotto-redeem.json


# upate the redeemer ticket with the ticket number to be burned
cat $redeemer_mint_file_path | \
jq -c '
  .fields[0].constructor    |= '0'
| .fields[1].bytes          |= "'$ticket_num_hex'"' > $WORK/redeemer-ticket-burn.json


# Step 4: Now, get UTXO from player with matching ticket number 
player_addr=$($CARDANO_CLI address build --testnet-magic $TESTNET_MAGIC --payment-verification-key-file $PLAYER_VKEY)
$CARDANO_CLI query utxo --address $player_addr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/player-utxo.json
cat $WORK/player-utxo.json | jq -r 'to_entries[] | {"txid":'.key',"mph":'.value.value'} ' > $WORK/player-utxo-valid.out
tx_in_win_num=$(cat $WORK/player-utxo-valid.out | jq -r 'select(.mph."'$ticket_mph'"."'$ticket_num_hex'") | .txid')


# Step 5: Get additional UTXOs from player to cover the cost of the transaction 
cat $WORK/player-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/player-utxo-valid.json
readarray player_utxo_valid_array < $WORK/player-utxo-valid.json
player_utxo_in=$(echo $player_utxo_valid_array | tr -d '\n')


# Step 6: Build and submit the transaction
$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$player_utxo_in" \
  --tx-in "$tx_in_win_num" \
  --tx-in "$lotto_validator_utxo_tx_in" \
  --tx-in-script-file "$lotto_validator_script" \
  --tx-in-datum-file "$WORK/lotto-datum-in.json" \
  --tx-in-redeemer-file "$WORK/redeemer-lotto-redeem.json" \
  --tx-in-collateral "$PLAYER_COLLATERAL" \
  --tx-out "$lotto_validator_script_addr+$lotto_value + 1 $thread_token_mph.$lotto_token_name + 1 $thread_token_mph.$buy_token_name" \
  --tx-out-datum-embed-file "$WORK/lotto-datum-out.json" \
  --tx-out "$player_addr+$MIN_ADA_OUTPUT_TX + 1 $ticket_mph.$ticket_num_hex" \
  --change-address "$player_addr" \
  --required-signer-hash "$player_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/redeem-tx-alonzo.body
  

# --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/redeem-alonzo.costs"


# have to disable mint burn on redeem due to execeeding max tx size limits UtxoFailure (MaxTxSizeUTxO 18136 16384)))]
# this is nice to have feature for only one winner, but is required for multiple winners
# once vasil hard fork is live, the max tx size will reduce significantly and should be able to renable
#  --mint-script-file "$minting_script" \
#  --mint-redeemer-file "$WORK/redeemer-ticket-burn.json" \
#  --mint "-1 $ticket_mph.$ticket_num_hex" \


echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/redeem-tx-alonzo.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${PLAYER_SKEY}" \
  --out-file $WORK/redeem-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/redeem-tx-alonzo.tx --testnet-magic "$TESTNET_MAGIC"



