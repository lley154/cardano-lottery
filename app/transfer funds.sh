#!/bin/bash
curl --request GET \
     --url 'http://localhost:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/addresses?state=unused'


curl --request POST \
   --url http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions \
   --header 'Content-Type: application/json' \
   --data '{
        "passphrase": "cardano-wallet",
        "payments": [
            {
            "address": "addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7",
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