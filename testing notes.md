
[nix-shell:~/Downloads/plutus-apps/plutus-pab-executables]$ cabal build plutus-pab-examples

nix-shell:~/Downloads/plutus-apps/plutus-pab-executables]$ cabal exec plutus-pab-examples -- migrate --config plutus-pab.yaml.sample

[nix-shell:~/Downloads/plutus-apps/plutus-pab-executables]$ cabal exec plutus-pab-examples -- all-servers --config plutus-pab.yaml.sample


export DIR=~/Downloads/cardano-wallet

export SHELLEY_TEST_DATA=~/Downloads/plutus-apps/plutus-pab/local-cluster/cluster-data/cardano-node-shelley

$ cabal exec plutus-pab-examples -- all-servers --config plutus-pab.yaml.sample



[pab:Info:13] [2022-01-22 14:19:20.75 UTC] Starting PAB Server on port 9082
Starting: MockWallet
Started: MockWallet
[pab:Info:15] [2022-01-22 14:19:20.75 UTC] Processing chain event TxnValidate 09604d638153f5182cb588fc3d9779a9e0962964e9110e8eae561d88819b554b
Starting: PABWebserver
[pab:Info:25] [2022-01-22 14:19:20.82 UTC] Starting wallet server on port 9081
Started: PABWebserver
[pab:Info:59] [2022-01-22 14:19:20.91 UTC] Restoring PAB state ...
[pab:Info:59] [2022-01-22 14:19:20.91 UTC] No contract instance were restored in the PAB state.
[pab:Info:59] [2022-01-22 14:19:20.91 UTC] Starting PAB backend server on port 9080
Starting: ChainIndex
Started: ChainIndex
[pab:Info:116] [2022-01-22 14:19:20.91 UTC] Starting node client thread
[pab:Info:116] [2022-01-22 14:19:20.91 UTC] Starting chain index on port 9083


export WALLET_ID_1=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`
export WALLET_ID_2=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`

# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWalletId": '$WALLET_ID_1'}}' \
  http://localhost:9080/api/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWalletId": '$WALLET_ID_2'}}' \
  http://localhost:9080/api/contract/activate | jq

$ echo $WALLET_ID_1
"969f6a9871439d9071c053619cba9891ec3f0452"
lawrence@lawrence-MacBookAir:~$ echo $WALLET_ID_2
"ddf1a2a5806f356d8f881c73eb6ff5e0b592349f"

curl -H "Content-Type: application/json" -v \
       -X POST \
       -d '{"caID":{"tag":"IntegrationTest"},"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
       localhost:9080/api/contract/activate

    


{
  "caID": {
    "tag": "UniswapInit"
  },
  "caWallet": {
    "getWalletId": {
      "unWalletId": "string"
    }
  }
}

ads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"60013552-8797-4975-9e7b-c8a812807d12"}


{
    "caID": {
        "tag": "UniswapInit"
    },
    "caWallet": {
        "getWalletId": "969f6a9871439d9071c053619cba9891ec3f0452"
    }
}


[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- migrate --config cardano-lottery-pab.yaml

[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- all-servers --config cardano-lottery-pab.yaml
Starting: StartNode
Started: StartNode
Starting: MockWallet
[pab:Info:10] [2022-01-22 16:36:08.45 UTC] Starting slot coordination thread. Initial slot time: 2020-07-29T21:44:51Z Slot length: 1000ms
[pab:Info:10] [2022-01-22 16:36:08.45 UTC] Starting PAB Server on port 9082
Started: MockWallet
[pab:Info:12] [2022-01-22 16:36:08.48 UTC] Processing chain event TxnValidate 09604d638153f5182cb588fc3d9779a9e0962964e9110e8eae561d88819b554b
Starting: PABWebserver
[pab:Info:25] [2022-01-22 16:36:08.49 UTC] Starting wallet server on port 9081
Started: PABWebserver
[pab:Info:41] [2022-01-22 16:36:08.57 UTC] Restoring PAB state ...
[pab:Info:41] [2022-01-22 16:36:08.57 UTC] No contract instance were restored in the PAB state.
[pab:Info:41] [2022-01-22 16:36:08.57 UTC] Starting PAB backend server on port 9080
Starting: ChainIndex
Started: ChainIndex
[pab:Info:87] [2022-01-22 16:36:08.57 UTC] Starting node client thread
[pab:Info:87] [2022-01-22 16:36:08.57 UTC] Starting chain index on port 9083

lawrence@lawrence-MacBookAir:~$ curl -s http://localhost:9080/api/contract/definitions | jq


    ],
    "csrDefinition": {
      "tag": "InitLottoContract"
    }


lawrence@lawrence-MacBookAir:~$ export WALLET_ID_1=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`
lawrence@lawrence-MacBookAir:~$ echo $WALLET_ID_1
"2ae8ea1050ae679390f6e8946962a4e534007174"
lawrence@lawrence-MacBookAir:~$ export WALLET_ID_2=`curl -s -d '' http://localhost:9081/create | jq '.wiWallet.getWalletId'`
lawrence@lawrence-MacBookAir:~$ echo $WALLET_ID_2
"7bc049579c6db571b7e1ad4d37a100e81a16f464"

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b"}lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 

Current block: 3. Current slot: 46810964
[pab:Info:148] [2022-01-22 16:47:36.33 UTC] Initialising contract InitLottoContract with ID a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b
[pab:Info:148] [2022-01-22 16:47:36.40 UTC] Activated instance a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b on W2ae8ea1
Current block: 3. Current slot: 46810965

export INSTANCE_ID=a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq
{
  "cicCurrentState": {
    "observableState": null,
    "logs": [],
    "hooks": [
      {
        "rqID": 1,
        "itID": 1,
        "rqRequest": {
          "aeMetadata": null,
          "aeDescription": {
            "getEndpointDescription": "init"
          }
        }
      }
    ],
    "lastLogs": [],
    "err": null
  },
  "cicYieldedExportTxs": [],
  "cicContract": {
    "unContractInstanceId": "a2ad3d59-7a05-46c4-afef-44bcf5fb2c8b"
  },
  "cicStatus": "Active",
  "cicDefinition": {
    "tag": "InitLottoContract"
  },
  "cicWallet": {
    "getWalletId": "2ae8ea1050ae679390f6e8946962a4e534007174"
  }
}




        sp = StartParams
                { spAdmin          = pkh
                , spBenAddress     = pkh
                , spJackpot        = jackpot'
                , spTicket         = ticket'
                , spDeadline       = deadline'
                }


export INSTANCE_ID=c753ff28-f998-4680-9fa5-d3fac466b87d


[INFO] sp parms: 
[INFO] "{\"spAdmin\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spBenAddress\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spJackpot\":10000000,\"spTicket\":20000,\"spDeadline\":1596064091999}"
[INFO] useTT : 
[INFO] "true"


[INFO] params : 
[INFO] "[{\"spAdmin\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spBenAddress\":{\"unPaymentPubKeyHash\":{\"getPubKeyHash\":\"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\"}},\"spJackpot\":10000000,\"spTicket\":20000,\"spDeadline\":1596064091999},true]"


######################################## Sunday Jan 23 ##################################


[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- migrate --config cardano-lottery-pab.yaml

[nix-shell:~/Downloads/cardano-lottery]$ cabal exec cardano-lottery-pab-run -- all-servers --config cardano-lottery-pab.yaml
Starting: StartNode
Started: StartNode
[pab:Info:10] [2022-01-23 17:28:19.97 UTC] Starting slot coordination thread. Initial slot time: 2020-07-29T21:44:51Z Slot length: 1000ms
Starting: MockWallet
Started: MockWallet
[pab:Info:10] [2022-01-23 17:28:19.98 UTC] Starting PAB Server on port 9082
[pab:Info:12] [2022-01-23 17:28:20.00 UTC] Processing chain event TxnValidate 09604d638153f5182cb588fc3d9779a9e0962964e9110e8eae561d88819b554b
Starting: PABWebserver
[pab:Info:26] [2022-01-23 17:28:20.02 UTC] Starting wallet server on port 9081
Started: PABWebserver
[pab:Info:41] [2022-01-23 17:28:20.10 UTC] Restoring PAB state ...
[pab:Info:41] [2022-01-23 17:28:20.10 UTC] No contract instance were restored in the PAB state.
[pab:Info:41] [2022-01-23 17:28:20.10 UTC] Starting PAB backend server on port 9080
Starting: ChainIndex
Started: ChainIndex
[pab:Info:88] [2022-01-23 17:28:20.10 UTC] Starting node client thread
[pab:Info:88] [2022-01-23 17:28:20.10 UTC] Starting chain index on port 9083




[nix-shell:~/Downloads]$ curl -s -d '' http://localhost:9081/create
{"wiWallet":{"getWalletId":"bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"},"wiPaymentPubKeyHash":{"unPaymentPubKeyHash":{"getPubKeyHash":"4484759c2c3fbc9c9c2704901b010b3b1c51bca942413b299bbe0cf3"}}}

[nix-shell:~/Downloads]$ export WALLET_ID_1='"bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"'

[nix-shell:~/Downloads]$ echo $WALLET_ID_1
"bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"



[nix-shell:~/Downloads]$ curl -s -d '' http://localhost:9081/create
{"wiWallet":{"getWalletId":"da8f13c8f9d13215e77d7c37aa4444b888cedb7e"},"wiPaymentPubKeyHash":{"unPaymentPubKeyHash":{"getPubKeyHash":"d42e8cbb1f44177a1109ca624e94bdfcfdd13d9170ce9d5c0c815710"}}}
[nix-shell:~/Downloads]$ export WALLET_ID_2='"da8f13c8f9d13215e77d7c37aa4444b888cedb7e"'

[nix-shell:~/Downloads]$ echo $WALLET_ID_2
"da8f13c8f9d13215e77d7c37aa4444b888cedb7e"


[nix-shell:~/Downloads/cardano-lottery/app]$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"7c6bb925-af3e-4934-95c1-e30cbb5317f6"}

[pab:Info:182] [2022-01-23 17:43:01.27 UTC] Initialising contract InitLottoContract with ID 7c6bb925-af3e-4934-95c1-e30cbb5317f6
[pab:Info:182] [2022-01-23 17:43:01.33 UTC] Activated instance 7c6bb925-af3e-4934-95c1-e30cbb5317f6 on Wbdf5e8b

[nix-shell:~/Downloads/cardano-lottery/app]$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
[]


[pab:Error:184] [2022-01-23 17:51:00.86 UTC] 7c6bb925-af3e-4934-95c1-e30cbb5317f6: "OtherError \"Error in $: parsing (a, b) failed, expected Array, but encountered Object\""

[nix-shell:~/Downloads/cardano-lottery/app]$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq
{
  "cicCurrentState": {
    "observableState": null,
    "logs": [
      {
        "_logMessageContent": "OtherError \"Error in $: parsing (a, b) failed, expected Array, but encountered Object\"",
        "_logLevel": "Error"
      }
    ],
    "hooks": [
      {
        "rqID": 1,
        "itID": 2,
        "rqRequest": {
          "aeMetadata": null,
          "aeDescription": {
            "getEndpointDescription": "init"
          }
        }
      }
    ],
    "lastLogs": [
      {
        "_logMessageContent": "OtherError \"Error in $: parsing (a, b) failed, expected Array, but encountered Object\"",
        "_logLevel": "Error"
      }
    ],
    "err": null
  },
  "cicYieldedExportTxs": [],
  "cicContract": {
    "unContractInstanceId": "7c6bb925-af3e-4934-95c1-e30cbb5317f6"
  },
  "cicStatus": "Active",
  "cicDefinition": {
    "tag": "InitLottoContract"
  },
  "cicWallet": {
    "getWalletId": "bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382"
  }
}


lot: 46902873
[pab:Error:184] [2022-01-23 18:19:24.98 UTC] 7c6bb925-af3e-4934-95c1-e30cbb5317f6: "OtherError \"Error in $[0].spAdmin: parsing Ledger.Address.PaymentPubKeyHash(PaymentPubKeyHash) failed, key \\\"unPaymentPubKeyHash\\\" not found\""
Current block: 5. Current slot: 46902874


Current block: 5. Current slot: 46903571
Wallet.Emulator.Wallet.walletPubKey: Wallet Wallet bdf5e8b409a77cb5d1d13fd6aff9356e3bb06382 is not a mock wallet
CallStack (from HasCallStack):
  error, called at src/Wallet/Emulator/Wallet.hs:144:22 in plutus-contract-0.1.0.0-2f17c7bdd61646ab0c6fe498f5ce48773511927703d743a2d774b3ad5e1ff438:Wallet.Emulator.Wallet
Current block: 5. Current slot: 46903572



############################# JAN 23 #####################################

The tip of the local node: SlotNo 180
Connecting to the node using socket: /run/user/1000/test-cluster192560/node/node.socket
Starting webserver on port 9083
A Swagger UI for the endpoints are available at http://localhost:9083/swagger/swagger-ui


[pab:Info:1500] [2022-01-23 20:15:05.36 UTC] Restoring PAB state ...
[pab:Info:1500] [2022-01-23 20:15:05.36 UTC] No contract instance were restored in the PAB state.
[pab:Info:1500] [2022-01-23 20:15:05.36 UTC] Starting PAB backend server on port 9080

Restored wallet: WalletId {getWalletId = 2d4cc31a4b3116ab86bfe529d30d9c362acd0b44}
Passphrase: cardano-wallet


[cluster:Notice:1384] [2022-01-23 20:14:47.91 UTC] {"string":"Wallet url: http://127.0.0.1:46493/, EKG url: none, Prometheus url:none"}


curl -s -d '' http://localhost:46493/create



http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys

http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/1852H


lawrence-MacBookAir:~$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/1852
"addr_vk1hd5qqfrktfqr29fy0jmz2vnj2fxey7avvul4j7nyjqykv8sxsrms7szx9a"

lawrence@lawrence-MacBookAir:~$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/1852?hash=true
"addr_vkh1eq0mkwyqm7muef2sjcw0vun5rhk7u56d8w0ua5u0xu92xeqam9m"lawrence@lawrence-MacBookAir:~$ 


curl http://127.0.0.1:46493/v2/shared-wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys?format=extended


[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/utxo
{"entries":[{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]},{"ada":{"unit":"lovelace","quantity":100000000000},"ada_minimum":{"unit":"lovelace","quantity":999978},"assets":[]}]}


[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/addresses[{"derivation_path":["1852H","1815H","0H","0","0"],"id":"addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x","state":"used"},{"derivation_path":["1852H","1815H","0H","0","1"],"id":"addr1qxvy2tutkjmrs7gyehmgeq6qnzhjjys98kweanhp3c69r2ufl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqgk0wgx","state":"used"},{"derivation_path":["1852H","1815H","0H","0","2"],"id":"addr1qy39m0dz3pg0l6unlpmn6pwt3tagp0qkfl0kxlq5wk8h76yfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqjnm7j8","state":"used"},{"derivation_path":["1852H","1815H","0H","0","3"],"id":"addr1q9l9xu3h6q692glfmlfz76rrkj63y33qv0xs0kfsdkl4x8yfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqtw4aa5","state":"used"}



[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/addresses/addr1q9l9xu3h6q692glfmlfz76rrkj63y33qv0xs0kfsdkl4x8yfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqtw4aa5
{"address_style":"Shelley","spending_key_hash_bech32":"addr_vkh10efhyd7sx32j86wl6ghksca5k5fyvgrre5rajvrdhaf3cpdjw73","network_tag":1,"stake_key_hash_bech32":"stake_vkh1387xr5saml9af4pk22lst3qvx3h609y8zs3mv5zj6as5c62hmun","stake_reference":"by value","spending_key_hash":"7e537237d0345523e9dfd22f6863b4b512462063cd07d9306dbf531c","stake_key_hash":"89fc61d21ddfcbd4d43652bf05c40c346fa794871423b65052d7614c"}


curl http://127.0.0.1:46493/v2/addresses/addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x

[nix-shell:~/Downloads/cardano-lottery/app]$ curl http://127.0.0.1:46493/v2/addresses/addr1qx0d0kyppx3qls8laq5jvpq0qa52d0gahm8tsyj2jpg0lpvfl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqfyq43x
{"address_style":"Shelley","spending_key_hash_bech32":"addr_vkh1nmta3qgf5g8upllg9ynqgrc8dznt68d7e6upyj5s2rlc2x3fpx5","network_tag":1,"stake_key_hash_bech32":"stake_vkh1387xr5saml9af4pk22lst3qvx3h609y8zs3mv5zj6as5c62hmun","stake_reference":"by value","spending_key_hash":"9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85","stake_key_hash":"89fc61d21ddfcbd4d43652bf05c40c346fa794871423b65052d7614c"}




lawrence@lawrence-MacBookAir:~$ curl http://localhost:46493/v2/wallets/
[{"address_pool_gap":20,"passphrase":{"last_updated_at":"2022-01-23T22:13:40.087168596Z"},"balance":{"available":{"unit":"lovelace","quantity":1000000000000},"total":{"unit":"lovelace","quantity":1000000000000},"reward":{"unit":"lovelace","quantity":0}},"id":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44","state":{"status":"ready"},"name":"plutus-wallet","assets":{"available":[],"total":[]},"tip":{"height":{"unit":"block","quantity":5270},"slot_number":66,"absolute_slot_number":10766,"epoch_number":107,"time":"2022-01-23T22:48:56.2Z"},"delegation":{"next":[],"active":{"status":"not_delegating"}}}]lawrence@lawrence-MacBookAir:~$ 


curl http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44


[nix-shell:~/Downloads/cardano-lottery/app]$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"b4b2703f-31d3-4edc-9e89-df86d2fb4c5d"}

export INSTANCE_ID=b4b2703f-31d3-4edc-9e89-df86d2fb4c5d


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq


curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init


############################### Jan 24 #################################


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request POST \
>   --url http://localhost:46493/v2/wallets \
>   --header 'Content-Type: application/json' \
>   --data '{
>     "name": "test_cf_1",
>     "mnemonic_sentence": ["shift", "badge", "heavy", "action", "tube", "divide", "course", "quality", "capable", "velvet", "cart", "marriage", "vague", "aware", "maximum", "exist", "crime", "file", "analyst", "great", "cabbage", "course", "sad", "apology"],
>     "passphrase": "test123456"
> }'
{"address_pool_gap":20,"passphrase":{"last_updated_at":"2022-01-24T16:02:51.141839196Z"},"balance":{"available":{"unit":"lovelace","quantity":0},"total":{"unit":"lovelace","quantity":0},"reward":{"unit":"lovelace","quantity":0}},"id":"5076b34c6949dbd150eb9c39039037543946bdce","state":{"status":"syncing","progress":{"unit":"percent","quantity":0}},"name":"test_cf_1","assets":{"available":[],"total":[]},"tip":{"height":{"unit":"block","quantity":0},"slot_number":0,"absolute_slot_number":0,"epoch_number":0,"time":"2022-01-24T01:05:51Z"},"delegation":{"next":[],"active":{"status":"not_delegating"}}}l

awrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request GET \
>   --url 'http://localhost:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/addresses?state=unused'
[{"derivation_path":["1852H","1815H","0H","0","0"],"id":"addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","1"],"id":"addr1q9g2eglv9gf2rksvdj53t6ajfgzkycaadlt2fatjyn4etpze0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qepwhvl","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","2"],"id":"addr1q9fk9lkc7qnh94ta84w7g8wexzg8k84m5rcj8hzpez58ntje0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qk8k7ld","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","3"],"id":"addr1q9cyzrh222u9udjvdm2a6dmsyar5qqasqzyksv6038gzvy2e0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00quvf760","state":"unused"}, ...

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request POST \
>   --url http://localhost:46493/v2/wallets \
>   --header 'Content-Type: application/json' \
>   --data '{
>     "name": "test_wallet_1",
>     "mnemonic_sentence": ["shift", "badge", "heavy", "action", "tube", "divide", "course", "quality", "capable", "velvet", "cart", "marriage", "vague", "aware", "maximum", "exist", "crime", "file", "analyst", "great", "cabbage", "course", "sad", "apology"],
>     "passphrase": "test123456"
> }'
{"address_pool_gap":20,"passphrase":{"last_updated_at":"2022-01-24T17:05:29.755656006Z"},"balance":{"available":{"unit":"lovelace","quantity":0},"total":{"unit":"lovelace","quantity":0},"reward":{"unit":"lovelace","quantity":0}},"id":"5076b34c6949dbd150eb9c39039037543946bdce","state":{"status":"syncing","progress":{"unit":"percent","quantity":0}},"name":"test_wallet_1","assets":{"available":[],"total":[]},"tip":{"height":{"unit":"block","quantity":0},"slot_number":0,"absolute_slot_number":0,"epoch_number":0,"time":"2022-01-24T17:02:55Z"},"delegation":{"next":[],"active":{"status":"not_delegating"}}}


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl --request POST \
>    --url http://localhost:46493/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions \
>    --header 'Content-Type: application/json' \
>    --data '{
>         "passphrase": "cardano-wallet",
>         "payments": [
>             {
>             "address": "addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7",
>             "amount": {
>                 "quantity": 1000000000,
>                 "unit": "lovelace"
>                 }
>             }
>         ]
> }'
{"outputs":[{"amount":{"unit":"lovelace","quantity":1000000000},"address":"addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7","assets":[]},{"amount":{"unit":"lovelace","quantity":98999869000},"address":"addr1qydz7tcs8wy4m0nn3zkvnnqslyxuftd98arvsswj43zxxpufl3say8wle02dgdjjhuzugrp5d7nefpc5ywm9q5khv9xqnt8cj6","assets":[]}],"status":"pending","amount":{"unit":"lovelace","quantity":1000131000},"mint":[],"fee":{"unit":"lovelace","quantity":131000},"deposit":{"unit":"lovelace","quantity":0},"id":"114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2","direction":"outgoing","metadata":null,"script_validity":"valid","pending_since":{"height":{"unit":"block","quantity":1524},"slot_number":59,"absolute_slot_number":3159,"epoch_number":31,"time":"2022-01-24T17:13:26.8Z"},"withdrawals":[],"collateral":[],"inputs":[{"amount":{"unit":"lovelace","quantity":100000000000},"address":"addr1v9fvyz26pnc7jftnezn784my73y8r8ylmhunl6j8mypdddgtqhp4x","id":"d3e4ff652ffb1bdda69bd60ca331cf2444cd49149255f1aad50564593c830bbf","index":4,"assets":[]}],"expires_at":{"slot_number":61,"absolute_slot_number":39161,"epoch_number":391,"time":"2022-01-24T19:13:27.


lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl http://localhost:46493/v2/wallets/ | jq
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  2365    0  2365    0     0   230k      0 --:--:-- --:--:-- --:--:--  256k
[
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:03:32.045280301Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 996999607000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 996999607000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "2d4cc31a4b3116ab86bfe529d30d9c362acd0b44",
    "state": {
      "status": "ready"
    },
    "name": "plutus-wallet",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  },
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:05:29.755656006Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "5076b34c6949dbd150eb9c39039037543946bdce",
    "state": {
      "status": "ready"
    },
    "name": "test_wallet_1",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  },
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:06:01.597936395Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "f6b3948d73f5e317ac130419ada6047262bfbb22",
    "state": {
      "status": "ready"
    },
    "name": "test_wallet_2",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  },
  {
    "address_pool_gap": 20,
    "passphrase": {
      "last_updated_at": "2022-01-24T17:06:22.797119755Z"
    },
    "balance": {
      "available": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "total": {
        "unit": "lovelace",
        "quantity": 1000000000
      },
      "reward": {
        "unit": "lovelace",
        "quantity": 0
      }
    },
    "id": "00d3b1fc7af1236ee7f6226a83ed2ad360b91013",
    "state": {
      "status": "ready"
    },
    "name": "test_wallet_3",
    "assets": {
      "available": [],
      "total": []
    },
    "tip": {
      "height": {
        "unit": "block",
        "quantity": 2162
      },
      "slot_number": 33,
      "absolute_slot_number": 4433,
      "epoch_number": 44,
      "time": "2022-01-24T17:17:41.6Z"
    },
    "delegation": {
      "next": [],
      "active": {
        "status": "not_delegating"
      }
    }
  }
]



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl http://127.0.0.1:46493/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/addresses
[{"derivation_path":["1852H","1815H","0H","0","0"],"id":"addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7","state":"used"},{"derivation_path":["1852H","1815H","0H","0","1"],"id":"addr1q9g2eglv9gf2rksvdj53t6ajfgzkycaadlt2fatjyn4etpze0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qepwhvl","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","2"],"id":"addr1q9fk9lkc7qnh94ta84w7g8wexzg8k84m5rcj8hzpez58ntje0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qk8k7ld","state":"unused"},{"derivation_path":["1852H","1815H","0H","0","3"],"id":"addr1q9cyzrh222u9udjvdm2a6dmsyar5qqasqzyksv6038gzvy2e0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00quvf760","state":"unused"},

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl http://127.0.0.1:46493/v2/addresses/addr1qxf9q3qjcaf6kxshwjfw9ge29njtm56r2a08g49l79xgt4je0592agqpwraqajx2dsu2sxj64uese5s4qum293wuc00qay8vu7 | jq
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   405    0   405    0     0   395k      0 --:--:-- --:--:-- --:--:--  395k
{
  "address_style": "Shelley",
  "spending_key_hash_bech32": "addr_vkh1jfgygyk82w4359m5jt32x23vuj7axs6hte69f0l3fjzavprjvsq",
  "network_tag": 1,
  "stake_key_hash_bech32": "stake_vkh1t97s4t4qq9c05rkgefkr32q6t2hnxrxjz5rndgk9mnpau9fwk40",
  "stake_reference": "by value",
  "spending_key_hash": "92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d6",
  "stake_key_hash": "597d0aaea00170fa0ec8ca6c38a81a5aaf330cd2150736a2c5dcc3de"
}
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 




lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @activation.json localhost:9080/api/contract/activate
{"unContractInstanceId":"4323d3dd-0552-44f7-b098-4862e0db6e74"}

lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ export INSTANCE_ID=4323d3dd-0552-44f7-b098-4862e0db6e74
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ echo $INSTANCE_ID
4323d3dd-0552-44f7-b098-4862e0db6e74
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init


[pab:Warning:3177] [2022-01-24 17:36:52.77 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-sign\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 403, statusMessage = \"Forbidden\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Mon, 24 Jan 2022 17:36:52 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"The given encryption passphrase doesn't match the one I use to encrypt the root private key of the given wallet: 5076b34c6949dbd150eb9c39039037543946bdce\\\",\\\"code\\\":\\\"wrong_encryption_passphrase\\\"}\"})"


cardano-wallet.api-server:Info:3966] [2022-01-24 17:36:52.70 UTC] {"string":"[RequestId 21] POST /v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-balance 202 Accepted in 0.069304522s"}
[cardano-wallet.api-server:Info:3966] [2022-01-24 17:36:52.75 UTC] {"string":"[RequestId 22] [POST] /v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-sign"}
[cardano-wallet.api-server:Info:3966] [2022-01-24 17:36:52.77 UTC] {"string":"[RequestId 22] POST /v2/wallets/5076b34c6949dbd150eb9c39039037543946bdce/transactions-sign 403 Forbidden in 0.019715701s"}



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
[]lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ 
lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -s http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq
{
  "cicCurrentState": {
    "observableState": null,
    "logs": [
      {
        "_logMessageContent": {
          "mkTxLogResult": {
            "Right": {
              "unBalancedTxTx": {
                "txData": [
                  [
                    "47e742b2b5c960e8cb8dcb7a76110e73864f9ec31abcd3d7c550586d15fce8d5",
                    "d8799fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d61b000001739cd54f5f194e2041309fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d64130ffff581cecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae1a00989680000000a1581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d600ffff"
                  ]
                ],
                "txInputs": [
                  {
                    "txInType": {
                      "tag": "ConsumePublicKeyAddress"
                    },
                    "txInRef": {
                      "txOutRefId": {
                        "getTxId": "114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2"
                      },
                      "txOutRefIdx": 0
                    }
                  }
                ],
                "txRedeemers": [
                  [
                    [
                      "Mint",
                      0
                    ],
                    "d8799f581c72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5d87980ff"
                  ]
                ],
                "txOutputs": [
                  {
                    "txOutValue": {
                      "getValue": [
                        [
                          {
                            "unCurrencySymbol": ""
                          },
                          [
                            [
                              {
                                "unTokenName": ""
                              },
                              10000000
                            ]
                          ]
                        ],
                        [
                          {
                            "unCurrencySymbol": "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e"
                          },
                          [
                            [
                              {
                                "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                              },
                              1
                            ]
                          ]
                        ]
                      ]
                    },
                    "txOutAddress": {
                      "addressStakingCredential": null,
                      "addressCredential": {
                        "contents": "72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5",
                        "tag": "ScriptCredential"
                      }
                    },
                    "txOutDatumHash": "47e742b2b5c960e8cb8dcb7a76110e73864f9ec31abcd3d7c550586d15fce8d5"
                  }
                ],
                "txValidRange": {
                  "ivTo": [
                    {
                      "tag": "PosInf"
                    },
                    true
                  ],
                  "ivFrom": [
                    {
                      "tag": "NegInf"
                    },
                    true
                  ]
                },
                "txMint": {
                  "getValue": [
                    [
                      {
                        "unCurrencySymbol": "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e"
                      },
                      [
                        [
                          {
                            "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                          },
                          1
                        ]
                      ]
                    ]
                  ]
                },
                "txFee": {
                  "getValue": []
                },
                "txCollateral": [],
                "txMintScripts": [
                  {
                    "getMintingPolicy": "590a9901000..."
                  }
                ],
                "txSignatures": []
              },
              "unBalancedTxRequiredSignatories": [],
              "unBalancedTxValidityTimeRange": {
                "ivTo": [
                  {
                    "tag": "PosInf"
                  },
                  true
                ],
                "ivFrom": [
                  {
                    "tag": "NegInf"
                  },
                  true
                ]
              },
              "unBalancedTxUtxoIndex": []
            }
          },
          "mkTxLogTxConstraints": {
            "txOwnInputs": [],
            "txOwnOutputs": [
              {
                "ocValue": {
                  "getValue": [
                    [
                      {
                        "unCurrencySymbol": ""
                      },
                      [
                        [
                          {
                            "unTokenName": ""
                          },
                          10000000
                        ]
                      ]
                    ],
                    [
                      {
                        "unCurrencySymbol": "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e"
                      },
                      [
                        [
                          {
                            "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                          },
                          1
                        ]
                      ]
                    ]
                  ]
                },
                "ocDatum": "d8799fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d61b000001739cd54f5f194e2041309fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d64130ffff581cecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae1a00989680000000a1581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d600ffff"
              }
            ],
            "txConstraints": [
              {
                "contents": "d8799fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d61b000001739cd54f5f194e2041309fd8799f581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d64130ffff581cecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae1a00989680000000a1581c92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d600ffff",
                "tag": "MustIncludeDatum"
              },
              {
                "contents": [
                  "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e",
                  "d8799f581c72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5d87980ff",
                  {
                    "unTokenName": "\u00000x72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
                  },
                  1
                ],
                "tag": "MustMintValue"
              },
              {
                "contents": {
                  "txOutRefId": {
                    "getTxId": "114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2"
                  },
                  "txOutRefIdx": 0
                },
                "tag": "MustSpendPubKeyOutput"
              }
            ]
          },
          "mkTxLogLookups": {
            "slOtherScripts": [],
            "slPaymentPubKeyHashes": [],
            "slOwnPaymentPubKeyHash": null,
            "slOwnStakePubKeyHash": null,
            "slOtherData": [],
            "slTypedValidator": {
              "tvForwardingMPSHash": "ecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae",
              "tvForwardingMPS": {
                "getMintingPolicy": "59092701000033233..."
              },
              "tvValidator": {
                "getValidator": "5927ac0100003323322..."
              },
              "tvValidatorHash": "72bc5beeb8c8bd1ff61d8c1c71c6904315299b182b3ed7fdd13a19d5"
            },
            "slTxOutputs": [
              [
                {
                  "txOutRefId": {
                    "getTxId": "114cd6d5f27a8fea1476ccccb4c06c400ecdbb0d08acfd68f38680d250b0f9d2"
                  },
                  "txOutRefIdx": 0
                },
                {
                  "_ciTxOutAddress": {
                    "addressStakingCredential": {
                      "contents": {
                        "contents": {
                          "getPubKeyHash": "597d0aaea00170fa0ec8ca6c38a81a5aaf330cd2150736a2c5dcc3de"
                        },
                        "tag": "PubKeyCredential"
                      },
                      "tag": "StakingHash"
                    },
                    "addressCredential": {
                      "contents": {
                        "getPubKeyHash": "92504412c753ab1a177492e2a32a2ce4bdd343575e7454bff14c85d6"
                      },
                      "tag": "PubKeyCredential"
                    }
                  },
                  "_ciTxOutValue": {
                    "getValue": [
                      [
                        {
                          "unCurrencySymbol": ""
                        },
                        [
                          [
                            {
                              "unTokenName": ""
                            },
                            1000000000
                          ]
                        ]
                      ]
                    ]
                  },
                  "tag": "PublicKeyChainIndexTxOut"
                }
              ]
            ],
            "slMPS": [
              [
                "073d077f9a5202d15f1993827ad5ddcafb6f6ad5bfaea4c1cf0ed22e",
                {
                  "getMintingPolicy": "590a990100003..."
                }
              ],
              [
                "ecee78512960beb877c2237195ec68dafb35a75f05d4071be8ebe0ae",
                {
                  "getMintingPolicy": "59092701000...
                }
              ]
            ]
          }
        },
        "_logLevel": "Debug"
      }
    ],
    "hooks": [],
    "lastLogs": [],
    "err": null
  },
  "cicYieldedExportTxs": [],
  "cicContract": {
    "unContractInstanceId": "4323d3dd-0552-44f7-b098-4862e0db6e74"
  },
  "cicStatus": "Active",
  "cicDefinition": {
    "tag": "InitLottoContract"
  },
  "cicWallet": {
    "getWalletId": "5076b34c6949dbd150eb9c39039037543946bdce"
  }
}



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
EndpointCallError (EndpointNotAvailable (ContractInstanceId {unContractInstanceId = fc05158c-c660-43a4-bcb9-c797d4ca604a}) (EndpointDescription {getEndpointDescription = "init"}))


[pab:Warning:10018] [2022-01-24 19:27:03.40 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 400, statusMessage = \"Bad Request\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Mon, 24 Jan 2022 19:27:03 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"I was unable to assign execution units to one of your redeemers: minting(f9fdc049adaa45b1043d31193dceb6127b31a8394cd0133f3231ef53); Its execution is failing with the following error: ValidationFailedV1 (CekError An error has occurred:  User error: The provided Plutus code called 'error'.) [\\\\\\\"S8\\\\\\\",\\\\\\\"PT5\\\\\\\"].\\\",\\\"code\\\":\\\"redeemer_script_failure\\\"}\"})"



[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.08 UTC] {"string":"[RequestId 32] [GET] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0?hash=true"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.08 UTC] {"string":"[RequestId 32] GET /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0 200 OK in 0.002541562s"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.12 UTC] {"string":"[RequestId 33] [POST] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance"}
[cardano-wallet.wallet-engine:Info:10934] [2022-01-24 19:27:03.12 UTC] {"string":"2d4cc31a: Selection report (summarized):\nSelectionReportSummarized:\n  computedFee: 0.131700\n  adaBalanceOfSelectedInputs: 20.000000\n  adaBalanceOfExtraCoinSource: 0.000000\n  adaBalanceOfExtraCoinSink: 0.000000\n  adaBalanceOfRequestedOutputs: 2.000000\n  adaBalanceOfGeneratedChangeOutputs: 17.868300\n  numberOfSelectedInputs: 1\n  numberOfSelectedCollateralInputs: 0\n  numberOfRequestedOutputs: 1\n  numberOfGeneratedChangeOutputs: 1\n  numberOfUniqueNonAdaAssetsInSelectedInputs: 0\n  numberOfUniqueNonAdaAssetsInRequestedOutputs: 0\n  numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs: 0\n"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.12 UTC] {"string":"[RequestId 33] POST /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance 202 Accepted in 0.005562668s"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.17 UTC] {"string":"[RequestId 34] [GET] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0?hash=true"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.17 UTC] {"string":"[RequestId 34] GET /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/keys/utxo_external/0 200 OK in 0.000887215s"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.38 UTC] {"string":"[RequestId 35] [POST] /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance"}
[cardano-wallet.wallet-engine:Info:10934] [2022-01-24 19:27:03.39 UTC] {"string":"2d4cc31a: Selection report (summarized):\nSelectionReportSummarized:\n  computedFee: 1.491600\n  adaBalanceOfSelectedInputs: 100000.000000\n  adaBalanceOfExtraCoinSource: 0.000000\n  adaBalanceOfExtraCoinSink: 0.000000\n  adaBalanceOfRequestedOutputs: 10.000000\n  adaBalanceOfGeneratedChangeOutputs: 99988.508400\n  numberOfSelectedInputs: 1\n  numberOfSelectedCollateralInputs: 1\n  numberOfRequestedOutputs: 1\n  numberOfGeneratedChangeOutputs: 1\n  numberOfUniqueNonAdaAssetsInSelectedInputs: 0\n  numberOfUniqueNonAdaAssetsInRequestedOutputs: 1\n  numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs: 0\n"}
[cardano-wallet.api-server:Info:10934] [2022-01-24 19:27:03.40 UTC] {"string":"[RequestId 35] POST /v2/wallets/2d4cc31a4b3116ab86bfe529d30d9c362acd0b44/transactions-balance 400 Bad Request in 0.01885629s"}



lawrence@lawrence-MacBookAir:~/Downloads/cardano-lottery/app$ curl -H "Content-Type: application/json" --request POST -d @init.json localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/init
EndpointCallError (EndpointNotAvailable (ContractInstanceId {unContractInstanceId = fc05158c-c660-43a4-bcb9-c797d4ca604a}) (EndpointDescription {getEndpointDescription = "init"}))



[cardano-wallet.wallet-engine:Error:10369] [2022-01-26 21:25:24.01 UTC] {"string":"Transaction 53f0c705 failed: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (WrongNetwork Mainnet (fromList [Addr Testnet (ScriptHashObj (ScriptHash \"870e8957cd9567e515cad1e5d91d5f7688421c6a34a30c3fad7a99ba\")) StakeRefNull]))))])) AlonzoEraInCardanoMode"}
[cardano-wallet.api-server:Error:10369] [2022-01-26 21:25:24.02 UTC] {"string":"[RequestId 17] POST /v2/proxy/transactions 500 Internal Server Error in 0.017427987s"}


[pab:Info:399] [2022-01-26 21:19:07.66 UTC] Initialising contract InitLottoContract with ID 704ee014-8819-4207-96c4-ed56298312e3
[pab:Info:399] [2022-01-26 21:19:07.71 UTC] Activated instance 704ee014-8819-4207-96c4-ed56298312e3 on Wb6ac58e
[pab:Warning:401] [2022-01-26 21:25:24.02 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"localhost\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/proxy/transactions\"), requestQueryString = fromList [], requestBody = Just ((),application/octet-stream), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Wed, 26 Jan 2022 21:25:23 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"The submitted transaction was rejected by the local node. Here's an error message that may help with debugging: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (WrongNetwork Mainnet (fromList [Addr Testnet (ScriptHashObj (ScriptHash \\\\\\\"870e8957cd9567e515cad1e5d91d5f7688421c6a34a30c3fad7a99ba\\\\\\\")) StakeRefNull]))))])) AlonzoEraInCardanoMode\\\",\\\"code\\\":\\\"created_invalid_transaction\\\"}\"})"


pab:Warning:1918] [2022-01-26 21:49:12.81 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-sign\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 403, statusMessage = \"Forbidden\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Wed, 26 Jan 2022 21:49:12 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"The given encryption passphrase doesn't match the one I use to encrypt the root private key of the given wallet: b6ac58e44f232e1fd863b7da0520b3c99d18bab5\\\",\\\"code\\\":\\\"wrong_encryption_passphrase\\\"}\"})"



##################################### JAN 26 ########################################33
pab:Info:2077] [2022-01-26 22:07:40.93 UTC] Initialising contract InitLottoContract with ID 89180b7e-d84a-41a7-a5c4-09a04a6dac2c
[pab:Info:2077] [2022-01-26 22:07:40.93 UTC] Activated instance 89180b7e-d84a-41a7-a5c4-09a04a6dac2c on Wb6ac58e
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1480}, tipBlockId = 86d57ff039b1cdd7dcfeeb760a4a7683bfefdf37ad0ade7ddc6ef737582b3dbf, tipBlockNo = BlockNumber {unBlockNumber = 668}}, targetPoint = Point {pointSlot = Slot {getSlot = 2085}, pointBlockId = f884df88dda0bd736c19c6f2c439763671cb48ccd0b7e09919bda86452e2eea6}})
[pab:Info:2079] [2022-01-26 22:10:56.86 UTC] 89180b7e-d84a-41a7-a5c4-09a04a6dac2c: "lotto has been intialized Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = 4db08b8c449cf658469eded2414924ddab35fa38599006f0fe308711b5050c98, txOutRefIdx = 0}, ttCurrencySymbol = 25b3ec827aa736b7a98d25d40092b421810eb27407939acf01c0024e})}"
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 2170}, tipBlockId = 1404b16bf526dd94a93213bf4c7bdf2b8a698096e2d1f84f499b6caa66fcf602, tipBlockNo = BlockNumber {unBlockNumber = 1031}}, targetPoint = Point {pointSlot = Slot {getSlot = 2883}, pointBlockId = dec2200cbea540de58b693f205d3cc1cabe8d0fa374009230ebb8a58ec770efe}


curl -s http://localhost:9080/api/contract/definitions | jq


[nix-shell:~/Downloads/cardano-lottery/app]$ curl -s http://localhost:9080/api/contract/definitions | jq
[
  {
    "csrSchemas": [
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "spAdmin",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spBenAddress",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spDeadline",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spTicket",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spJackpot",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "tag": "FormSchemaBool"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "init"
        }
      }
    ],
    "csrDefinition": "InitLottoContract"
  },
  {
    "csrSchemas": [
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "lToken",
                  {
                    "contents": {
                      "contents": [
                        [
                          "ttOutRef",
                          {
                            "contents": [
                              [
                                "txOutRefId",
                                {
                                  "contents": [
                                    [
                                      "getTxId",
                                      {
                                        "tag": "FormSchemaString"
                                      }
                                    ]
                                  ],
                                  "tag": "FormSchemaObject"
                                }
                              ],
                              [
                                "txOutRefIdx",
                                {
                                  "tag": "FormSchemaInteger"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ],
                        [
                          "ttCurrencySymbol",
                          {
                            "contents": [
                              [
                                "unCurrencySymbol",
                                {
                                  "tag": "FormSchemaString"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ]
                      ],
                      "tag": "FormSchemaObject"
                    },
                    "tag": "FormSchemaMaybe"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "tag": "FormSchemaInteger"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "buy"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "calc-payout"
        }
      },
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "lToken",
                  {
                    "contents": {
                      "contents": [
                        [
                          "ttOutRef",
                          {
                            "contents": [
                              [
                                "txOutRefId",
                                {
                                  "contents": [
                                    [
                                      "getTxId",
                                      {
                                        "tag": "FormSchemaString"
                                      }
                                    ]
                                  ],
                                  "tag": "FormSchemaObject"
                                }
                              ],
                              [
                                "txOutRefIdx",
                                {
                                  "tag": "FormSchemaInteger"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ],
                        [
                          "ttCurrencySymbol",
                          {
                            "contents": [
                              [
                                "unCurrencySymbol",
                                {
                                  "tag": "FormSchemaString"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ]
                      ],
                      "tag": "FormSchemaObject"
                    },
                    "tag": "FormSchemaMaybe"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "tag": "FormSchemaInteger"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "close"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "collect"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "payout"
        }
      },
      {
        "argument": {
          "contents": [
            [
              "lToken",
              {
                "contents": {
                  "contents": [
                    [
                      "ttOutRef",
                      {
                        "contents": [
                          [
                            "txOutRefId",
                            {
                              "contents": [
                                [
                                  "getTxId",
                                  {
                                    "tag": "FormSchemaString"
                                  }
                                ]
                              ],
                              "tag": "FormSchemaObject"
                            }
                          ],
                          [
                            "txOutRefIdx",
                            {
                              "tag": "FormSchemaInteger"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ],
                    [
                      "ttCurrencySymbol",
                      {
                        "contents": [
                          [
                            "unCurrencySymbol",
                            {
                              "tag": "FormSchemaString"
                            }
                          ]
                        ],
                        "tag": "FormSchemaObject"
                      }
                    ]
                  ],
                  "tag": "FormSchemaObject"
                },
                "tag": "FormSchemaMaybe"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        },
        "endpointDescription": {
          "getEndpointDescription": "redeem"
        }
      },
      {
        "argument": {
          "contents": [
            {
              "contents": [
                [
                  "lToken",
                  {
                    "contents": {
                      "contents": [
                        [
                          "ttOutRef",
                          {
                            "contents": [
                              [
                                "txOutRefId",
                                {
                                  "contents": [
                                    [
                                      "getTxId",
                                      {
                                        "tag": "FormSchemaString"
                                      }
                                    ]
                                  ],
                                  "tag": "FormSchemaObject"
                                }
                              ],
                              [
                                "txOutRefIdx",
                                {
                                  "tag": "FormSchemaInteger"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ],
                        [
                          "ttCurrencySymbol",
                          {
                            "contents": [
                              [
                                "unCurrencySymbol",
                                {
                                  "tag": "FormSchemaString"
                                }
                              ]
                            ],
                            "tag": "FormSchemaObject"
                          }
                        ]
                      ],
                      "tag": "FormSchemaObject"
                    },
                    "tag": "FormSchemaMaybe"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            },
            {
              "contents": [
                [
                  "spAdmin",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spBenAddress",
                  {
                    "contents": [
                      [
                        "unPaymentPubKeyHash",
                        {
                          "contents": [
                            [
                              "getPubKeyHash",
                              {
                                "tag": "FormSchemaString"
                              }
                            ]
                          ],
                          "tag": "FormSchemaObject"
                        }
                      ]
                    ],
                    "tag": "FormSchemaObject"
                  }
                ],
                [
                  "spDeadline",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spTicket",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ],
                [
                  "spJackpot",
                  {
                    "tag": "FormSchemaInteger"
                  }
                ]
              ],
              "tag": "FormSchemaObject"
            }
          ],
          "tag": "FormSchemaTuple"
        },
        "endpointDescription": {
          "getEndpointDescription": "start"
        }
      }
    ],
    "csrDefinition": "UseLottoContract"
  }
]

[pab:Info:1768] [2022-01-27 14:59:34.04 UTC] 7655417b-eb56-48c0-913f-56e235296dc5: "lotto has been intialized Lottery {lToken = Just (ThreadToken {ttOutRef = TxOutRef {txOutRefId = ede42dfb0526a4acc97088958648fdf56aed125cd633860aca977e3d2f193ff3, txOutRefIdx = 0}, ttCurrencySymbol = a291967c430b0a3a63b19463ae815f59c31b38685de69c704dcf5976})}"
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 1426}, pointBlockId = c9beab0836f0d37498501472aaf145093a1a9cf274ba854402338dbfdd3f4f88}})
[pab:Info:2051] [2022-01-27 15:00:38.96 UTC] Initialising contract UseLottoContract with ID 4be96adf-f991-4fa2-ba0a-986217842056
[pab:Info:2051] [2022-01-27 15:00:38.96 UTC] Activated instance 4be96adf-f991-4fa2-ba0a-986217842056 on Wb6ac58e
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 1924}, pointBlockId = 904ad009892f4db9de19c491b960ca9fc17f1c8181b73ddb171c8291d67abd88}})
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 2757}, pointBlockId = 90bfa028ac296359359d84ff2ba4579694df66eaa150af059349cab15a7d203f}})
[pab:Info:2054] [2022-01-27 15:04:25.80 UTC] 4be96adf-f991-4fa2-ba0a-986217842056: "setting lotto sequence to start of winning ticket number "
[cardano-wallet.api-server:Error:2304] [2022-01-27 15:04:25.94 UTC] {"string":"[RequestId 11] POST /v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-balance 500 Internal Server Error in 0.053316004s"}
[pab:Warning:2054] [2022-01-27 15:04:25.94 UTC] WalletClientError "FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"127.0.0.1\", baseUrlPort = 46493, baseUrlPath = \"\"},\"/v2/wallets/b6ac58e44f232e1fd863b7da0520b3c99d18bab5/transactions-balance\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Thu, 27 Jan 2022 15:04:25 GMT\"),(\"Server\",\"Warp/3.3.17\"),(\"Content-Type\",\"application/json;charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"{\\\"message\\\":\\\"What was supposed to be an initial overestimation of fees turned out to be an underestimation, and I cannot recover. This is a cardano-wallet bug.\\\",\\\"code\\\":\\\"created_invalid_transaction\\\"}\"})"
handleSyncAction failed with: RollbackFailure (TipMismatch {foundTip = Tip {tipSlot = Slot {getSlot = 1261}, tipBlockId = 8fcc5bc373c78a689c9e0d443a49a3a5f67f7da37f464c7ba39bb6fedfcb95a9, tipBlockNo = BlockNumber {unBlockNumber = 579}}, targetPoint = Point {pointSlot = Slot {getSlot = 4048}, pointBlockId = 80621bed6744bd709359e77a5569f2b3d52429365a0fd405d6665b9a0973576c}})

