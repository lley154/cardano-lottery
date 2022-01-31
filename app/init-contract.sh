#!/bin/bash

curl http://127.0.0.1:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/addresses

curl http://127.0.0.1:46493/v2/addresses/addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7

curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init



curl http://127.0.0.1:46493/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/addresses


curl http://127.0.0.1:46493/v2/addresses/addr1qx8z7gya3jkg9mn9lcmc68xakzc0wfjqve4pe89ctnf29rc9wqfjx6pydmpyzrz6rg2mu0y7s2vh53emnpzqh6sahg9ss22vym | jq

curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init




curl http://cardano-server:8090/v2/wallets/92d1ad83159ce811e76c5cf1780222bb72678c8e/addresses


curl http://cardano-server:8090/v2/addresses/addr_test1qz5gks3j8ac4kz6ls2vr5dtkndcul9w60v54pcnledef3r25ckj9c9u0ud4z5etpqacth5m2228p92vty0qwdu4706wsya900t | jq

curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init


lawrence@lawrence-MacBookAir:~/src/cardano-lottery/app$ curl http://cardano-server:8090/v2/addresses/addr_test1qz5gks3j8ac4kz6ls2vr5dtkndcul9w60v54pcnledef3r25ckj9c9u0ud4z5etpqacth5m2228p92vty0qwdu4706wsya900t | jq
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   422    0   422    0     0  10550      0 --:--:-- --:--:-- --:--:-- 10550
{
  "stake_reference": "by value",
  "address_style": "Shelley",
  "network_tag": 0,
  "stake_key_hash_bech32": "stake_vkh12nz6ghqh3l3k52n9vyrhpw7ndffguy4f3v3upehjhelf6ytdhpc",
  "stake_key_hash": "54c5a45c178fe36a2a65610770bbd36a528e12a98b23c0e6f2be7e9d",
  "spending_key_hash_bech32": "addr_vkh14z95yv3lw9dskhuznqar2a5mw88etknm99gwyl7tw2vg68u0vrc",
  "spending_key_hash": "a88b42323f715b0b5f82983a35769b71cf95da7b2950e27fcb72988d",
  "address_type": 0
}

