#!/bin/bash

curl http://127.0.0.1:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/addresses

curl http://127.0.0.1:46493/v2/addresses/addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7

curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init



curl http://127.0.0.1:46493/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/addresses


curl http://127.0.0.1:46493/v2/addresses/addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x

curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init

