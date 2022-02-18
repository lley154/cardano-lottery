#!/bin/bash

# default wallet for local devnet named 'plutus-wallet'
WALLET_DEF_ID=2d4cc31a4b3116ab86bfe529d30d9c362acd0b44


WALLET1_ID=$(curl --request POST \
  --url http://localhost:46493/v2/wallets \
  --header 'Content-Type: application/json' \
  --data '{
    "name": "test_wallet_1",
    "mnemonic_sentence": ["before", "supply", "air", "shrimp", "awkward", "cousin", "alert", "race", "base", "side", "agree", "assume", "coast", "soft", "poet", "symbol", "muscle", "impose", "say", "raccoon", "fault", "concert", "middle", "congress"],
    "passphrase": "cardano-wallet"
}'| jq -r '.id')


WALLET2_ID=$(curl --request POST \
  --url http://localhost:46493/v2/wallets \
  --header 'Content-Type: application/json' \
  --data '{
    "name": "test_wallet_2",
    "mnemonic_sentence": ["coin", "embrace", "brown", "before", "custom", "leaf", "source", "neck", "dizzy", "olympic", "lava", "kitchen", "govern", "home", "trigger", "risk", "buffalo", "any", "faint", "build", "kidney", "list", "lemon", "long"],
    "passphrase": "cardano-wallet"
}'| jq -r '.id')


ADDR1=$(curl --silent --request GET --url 'http://localhost:46493/v2/wallets/'$WALLET1_ID'/addresses?state=unused' | jq -r '.[0].id')

ADDR2=$(curl --silent --request GET --url 'http://localhost:46493/v2/wallets/'$WALLET2_ID'/addresses?state=unused' | jq -r '.[0].id')



curl --request POST \
   --url http://localhost:46493/v2/wallets/$WALLET_DEF_ID/transactions \
   --header 'Content-Type: application/json' \
   --data '{
        "passphrase": "cardano-wallet",
        "payments": [
            {
            "address": "'$ADDR1'",
            "amount": {
                "quantity": 1000000000,
                "unit": "lovelace"
                }
            }
        ]
}'



curl --request POST \
   --url http://localhost:46493/v2/wallets/$WALLET_DEF_ID/transactions \
   --header 'Content-Type: application/json' \
   --data '{
        "passphrase": "cardano-wallet",
        "payments": [
            {
            "address": "'$ADDR2'",
            "amount": {
                "quantity": 1000000000,
                "unit": "lovelace"
                }
            }
        ]
}'

sleep 5
curl http://localhost:46493/v2/wallets/ | jq



