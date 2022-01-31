#!/bin/bash

curl http://localhost:46493/v2/wallets/ | jq

curl http://cardano-server:8090/v2/wallets/ | jq


curl --request GET \
     --url 'http://cardano-server:8090/v2/wallets/92d1ad83159ce811e76c5cf1780222bb72678c8e/addresses?state=unused'



curl --request GET \
     --url 'http://localhost:46493/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/addresses?state=unused'


curl --request POST \
   --url http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions \
   --header 'Content-Type: application/json' \
   --data '{
        "passphrase": "cardano-wallet",
        "payments": [
            {
            "address": "addr1qx8z7gya3jkg9mn9lcmc68xakzc0wfjqve4pe89ctnf29rc9wqfjx6pydmpyzrz6rg2mu0y7s2vh53emnpzqh6sahg9ss22vym",
            "amount": {
                "quantity": 1000000000,
                "unit": "lovelace"
                }
            }
        ]
}'


curl --request GET \
     --url 'http://localhost:46493/v2/wallets/f6b3948d73f5e317ac130419ada6047262bfbb22/addresses?state=unused'



curl --request POST \
   --url http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions \
   --header 'Content-Type: application/json' \
   --data '{
        "passphrase": "cardano-wallet",
        "payments": [
            {
            "address": "addr1q83p25z992dc5fe9l4hmxma06y4rs6xapkf3d0guzrkk53glrar9en6as6v75jj327curlt32ysxv2r76l50e9s52shs7yxp5j",
            "amount": {
                "quantity": 1000000000,
                "unit": "lovelace"
                }
            }
        ]
}'

curl --request GET \
     --url 'http://localhost:46493/v2/wallets/00d3b1fc7af1236ee7f6226a83ed2ad360b91013/addresses?state=unused'



curl --request POST \
   --url http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions \
   --header 'Content-Type: application/json' \
   --data '{
        "passphrase": "cardano-wallet",
        "payments": [
            {
            "address": "addr1q99zckeytnde39wnlfa6n7umvu8skaxjmja62yls5j2gu5dypaqgsn59x9njp2pj7e4mgjtslmksvwjw0he6l0tdf6usxmwu08",
            "amount": {
                "quantity": 1000000000,
                "unit": "lovelace"
                }
            }
        ]
}'

curl --request POST \
   --url http://localhost:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions \
   --header 'Content-Type: application/json' \
   --data '{
        "passphrase": "test123456",
        "payments": [
            {
            "address": "addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x",
            "amount": {
                "quantity": 20000000,
                "unit": "lovelace"
                }
            }
        ]
}'