#!/bin/bash
curl -H "Content-Type: application/json" --request POST -d @wallet-test.json  http://localhost:46493/v2/wallets

curl --request POST \
  --url http://localhost:46493/v2/wallets \
  --header 'Content-Type: application/json' \
  --data '{
    "name": "test_wallet_1",
    "mnemonic_sentence": ["shift", "badge", "heavy", "action", "tube", "divide", "course", "quality", "capable", "velvet", "cart", "marriage", "vague", "aware", "maximum", "exist", "crime", "file", "analyst", "great", "cabbage", "course", "sad", "apology"],
    "passphrase": "test123456"
}' | jq

curl --request POST \
  --url http://localhost:46493/v2/wallets \
  --header 'Content-Type: application/json' \
  --data '{
    "name": "test_wallet_2",
    "mnemonic_sentence": ["before", "supply", "air", "shrimp", "awkward", "cousin", "alert", "race", "base", "side", "agree", "assume", "coast", "soft", "poet", "symbol", "muscle", "impose", "say", "raccoon", "fault", "concert", "middle", "congress"],
    "passphrase": "test123456"
}' | jq


curl --request POST \
  --url http://localhost:46493/v2/wallets \
  --header 'Content-Type: application/json' \
  --data '{
    "name": "test_wallet_3",
    "mnemonic_sentence": ["cook", "prevent", "among", "spend", "ill", "great", "balcony", "mix", "measure", "ahead", "link", "loyal", "once", "live", "light", "limit", "type", "income", "decorate", "kid", "warm", "scene", "left", "asset"],
    "passphrase": "test123456"
}' | jq



