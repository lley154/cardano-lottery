#! /usr/bin/bash

# start the pab instance prior to running this script
# nix-shell

# cabal exec -- cardano-lottery-pab-run --config cardano-lottery-pab.yaml migrate
# cabal exec -- cardano-lottery-pab-run --config cardano-lottery-pab.yaml webserver --passphrase cardano-wallet
#
# if you need to proxy from local host to cardano-server for caradano-wallet & cardano-node node.sock
# ssh -L 8090:localhost:8090 cardano-server
# ssh -L  /home/lawrence/src/cardano-lottery/node.sock:/home/lawrence/Downloads/plutus-apps/plutus-pab/test-node/testnet/node.sock cardano-server


wait_with_backoff() {
    cmd="$1"
    msg="$2"
    final_cmd="${3:-$cmd}"
    up=0
    for ((i=0;i<=10;i++))
    do
        if eval "$cmd"
        then
            up=1
            break
        fi
        let delay=2**i
        echo "$msg, sleeping for ${delay} seconds then trying again" >&2
        sleep "${delay}"
    done
    if [[ "$up" == "0" ]]
    then
        eval "$final_cmd"
    fi
}

wait_with_backoff "/usr/bin/curl --fail http://localhost:9080/api/contract/definitions &>/dev/null" "Connecting to PAB failed" "/usr/bin/curl --fail http://localhost:9080/api/contract/definitions"

create_wallets() {

    # list current wallets
    curl http://localhost:8090/v2/wallets/

    WALLET1_ID=$(/usr/bin/curl --request POST \
    --url http://localhost:8090/v2/wallets \
    --header 'Content-Type: application/json' \
    --data '{
        "name": "test_wallet_1",
        "mnemonic_sentence": ["before", "supply", "air", "shrimp", "awkward", "cousin", "alert", "race", "base", "side", "agree", "assume", "coast", "soft", "poet", "symbol", "muscle", "impose", "say", "raccoon", "fault", "concert", "middle", "congress"],
        "passphrase": "cardano-wallet"
    }'| jq -r '.id')


    WALLET2_ID=$(/usr/bin/curl --request POST \
    --url http://localhost:8090/v2/wallets \
    --header 'Content-Type: application/json' \
    --data '{
        "name": "test_wallet_2",
        "mnemonic_sentence": ["coin", "embrace", "brown", "before", "custom", "leaf", "source", "neck", "dizzy", "olympic", "lava", "kitchen", "govern", "home", "trigger", "risk", "buffalo", "any", "faint", "build", "kidney", "list", "lemon", "long"],
        "passphrase": "cardano-wallet"
    }'| jq -r '.id')


    ADDR1=$(/usr/bin/curl --silent --request GET --url 'http://localhost:8090/v2/wallets/'$WALLET1_ID'/addresses?state=unused' | jq -r '.[0].id')

    ADDR2=$(/usr/bin/curl --silent --request GET --url 'http://localhost:8090/v2/wallets/'$WALLET2_ID'/addresses?state=unused' | jq -r '.[0].id')
    

sleep 2
}


use_wallets() {

    # list current wallets
    curl http://localhost:8090/v2/wallets/

    WALLET1_ID=f6b3948d73f5e317ac130419ada6047262bfbb22
    WALLET2_ID=b6ac58e44f232e1fd863b7da0520b3c99d18bab5

    WALLET3_ID=00d3b1fc7af1236ee7f6226a83ed2ad360b91013
    ADDR3=$(curl --silent --request GET --url 'http://localhost:8090/v2/wallets/'$WALLET3_ID'/addresses?state=unused' | jq -r '.[0].id')


    curl --request POST \
    --url http://localhost:8090/v2/wallets/$WALLET1_ID/transactions \
    --header 'Content-Type: application/json' \
    --data '{
            "passphrase": "cardano-wallet",
            "payments": [
                {
                "address": "'$ADDR3'",
                "amount": {
                    "quantity": 900000000,
                    "unit": "lovelace"
                    }
                }
            ]
    }'

sleep 2
}

init() {

    echo "Wallets:"
    echo $WALLET1_ID
    echo $WALLET2_ID
    
    echo "staring init..."

    wait_with_backoff '[[ $(/usr/bin/curl --fail --silent http://localhost:8090/wallet/$WALLET1_ID/total-funds | jq '\''.getValue|length'\'') != "0" ]]' "Wallet not populated"

    CONTRACT_ID=$(/usr/bin/curl --silent \
                       --header "Content-Type: application/json" \
                       --data '{"caID": "InitLottoContract", "caWallet":{"getWalletId": "'$WALLET2_ID'"}}' \
                       http://localhost:9080/api/contract/activate | jq -r '.unContractInstanceId')

    log_count=0
    echo "Contract ID:"
    echo $CONTRACT_ID
    
    wait_for_contract() {
	wait_with_backoff '/usr/bin/curl --fail --silent http://localhost:9080/api/contract/instance/'$CONTRACT_ID'/status > status.json && [[("$(jq -r .cicStatus < status.json)" == "Active") ]]' "Contract not ready"
    log_count="$(jq -r '.cicCUrrentState.logs | length' <status.json)"
    }

    wait_for_contract

    # call init endpoint
    echo "Calling init endpoint"
    /usr/bin/curl --silent --header "Content-Type: application/json" --data '[]' http://localhost:9080/api/contract/instance/$CONTRACT_ID/endpoint/init

    wait_for_contract

    wait_with_backoff '/usr/bin/curl --fail --silent http://localhost:9080/api/contract/instance/'$CONTRACT_ID'/status > status.json && [[ -n "$(jq -r .cicCurrentState.observableState.ttLotery.ttCurrencySymbol.unCurrencySymbol < status.json)" ]]' "Observable state not ready"
    
    sleep 5
    LOTTERY=$(/usr/bin/curl --silent http://localhost:9080/api/contract/instance/$CONTRACT_ID/status | jq -r '.cicCurrentState.observableState')

    jq -r < status.json

    echo "Lottery:"
    echo $LOTTERY
}

buy() {

    wait_with_backoff '[[ $(/usr/bin/curl --fail --silent http://localhost:8090/wallet/$WALLET2_ID/total-funds | jq '\''.getValue|length'\'') != "0" ]]' "Wallet not populated"

    CONTRACT_ID=$(/usr/bin/curl --silent \
                       --header "Content-Type: application/json" \
                       --data '{"caID": "UseLottoContract", "caWallet":{"getWalletId": "'$WALLET2_ID'"}}' \
                       http://localhost:9080/api/contract/activate | jq -r '.unContractInstanceId')

    echo "Contract ID:"
    echo $CONTRACT_ID
    log_count=0

    wait_for_contract() {
	wait_with_backoff '/usr/bin/curl --fail --silent http://localhost:9080/api/contract/instance/'$CONTRACT_ID'/status > status.json && [[("$(jq -r .cicStatus < status.json)" == "Active") ]]' "Contract not ready"
    log_count="$(jq -r '.cicCUrrentState.logs | length' <status.json)"
    }

    wait_for_contract

    # call buy endpoint
    echo "Calling buy endpoint"
    /usr/bin/curl  --request POST \
    --header "Content-Type: application/json" \
    --data '['"$LOTTERY"',{"unTokenName": "123"}]' \
    http://localhost:9080/api/contract/instance/$CONTRACT_ID/endpoint/buy

    wait_for_contract

    jq -r < status.json
}

#create_wallets
use_wallets
init
buy

