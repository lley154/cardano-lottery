
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
