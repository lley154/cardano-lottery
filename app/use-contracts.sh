#!/bin/bash


curl http://127.0.0.1:46493/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/addresses


curl http://127.0.0.1:46493/v2/addresses/addr1qx8z7gya3jkg9mn9lcmc68xakzc0wfjqve4pe89ctnf29rc9wqfjx6pydmpyzrz6rg2mu0y7s2vh53emnpzqh6sahg9ss22vym | jq

curl -H "Content-Type: application/json" --request POST -d @start.json localhost:9080/api/contract/instance/$INSTANCE_ID2/endpoint/start

