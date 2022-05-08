# Table Of Contents
- [The Open Source Cardano Lottery](#the-open-source-cardano-lottery)
  - [Why](#why)
  - [Get Involved](#get-involved)
  - [Lotto State Machine](#lotto-state-machine)
- [How To Play The Lottery](#how-to-play-the-lottery)
- [How To Run The Lottery](#how-to-run-the-lottery)
- [The Open Source Lottery Design](#the-open-source-lottery-design)
  - [3 Key Threat Vectors](#3-key-threat-vectors)
  - [High Level Design](#high-level-design)
  - [Funding Model](#funding-model)
- [Roadmap Items](#roadmap-items)
- [Developer Testing](#developer-testing)
- [Coding Style](#coding-style)
- [Tips & Tricks](#tips--tricks)
- [System Requirements](#system-requirements)
- [Legal Notice](#legal-notice)

  

# The Open Source Cardano Lottery
The Open Source Cardano Lottery is a smart contract lottery built using plutus for the Cardano Blockchain.   It is a non-custodial lottery where all interactions with the lottery are done directly with the smart contracts and do not require any 3rd party to manage and hold any lottery funds.  Equally important is that the lotto administrator only has the ability to run the lottery and does not have access to the jackpot or treasury to prevent any unauthorized access.

## Why
The motivation for creating a blockchain lottery is to help provide fund raising for "GreenFi" and "SoGood" Cardano blockchain projects.  The reason for making this an open source project is that everyone can see the code and provide feedback, comments, suggestions and contributions which will make it a secure and trustworthy blockchain lottery.


## Get Involved
Feel free to reach out if you have any questions, comments, suggestions or contributions.

- [Discord](https://discord.gg/KPwBvVH2ZN) 
- [YouTube](https://youtu.be/q55nU9mwZYg)


Please note that I will never direct message you first or ask you for any ADA. 

## Lotto State Machine
The Lottery uses a state machine design pattern which follows the states below during a typical lottery lifecycle.
 
 
| State | Name | Description |
|:---|:---|:---|
|0| Init | Initialize the lottery|
|1| Open | Open the lottery (only in this state can tickets be purchased) |
|2| Closed | Close the lottery |
|3| Draw | Draw the winning lottery numbers |
|4| Redeem | Player redeems their winning number if applicable|
|5| Payout | Both the player and sponsor claim their payout if there is a winner|
|6| End | End the lottery. 

To run another lottery cycle, the lottery goes back to Open State again 
 
 
## How To Play The Lottery
The current Beta 0.1.0.0 release has only a basic bash script for playing the lotto which requires the player to run the bash script on a computer with the cardano node running.   Future roadmap items include browser and/or mobile app wallet integration.
 
 
### Setup
1. Download the cardano-lottery repository
```
git clone https://github.com/lley154/cardano-lottery.git
```
2. Go into the scripts directory for either testnet or mainnet
```
cd cardano-lottery/scripts/cardano-cli/[testnet|mainnet]/
```
3. Edit the global-export-variables.sh file and set the correct path for the following:

   ```
   PROJECT_ID = get ID from blockfrost (https://blockfrost.io/)
   BASE=path-to-cardano-lottery
   CARDANO_CLI=path-to-cardano-cli
   CARDANO_NODE_SOCKET_PATH=path-to-node-socket
   TESTNET_MAGIC=1097911063 or 
   PLAYER_VKEY=path-to-vkey
   PLAYER_SKEY=path-to-skey
   PLAYER_PKH=path-to-pkh
   PLAYER_COLLATERAL=TxId#TxIdx
   ```
   
#### Buy
The buy script will take the number in the arguments, look up the current lotto sequence number and submit the transaction with the required Ada for the cost of a ticket.   If successful, a ticket with your number will be minted and will be sent to your wallet.   The sequence number is appended to the token name so you know what lotto cycle a ticket is for.  A ticket is only valid for the lotto cycle in which it was purchased.
 
 ```
./buy-lotto-tx.sh [devnet|testnet|mainnet] [n]    where n = ticket number
 ```
 
#### Redeem
The redeem script will take your ticket token from your wallet and if there is a match with the winning numbers, then the token will be burned and the public key hash of the wallet will be recorded in the lotto datum.  
 
 ```
./redeem-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
#### Payout
The payout script will try to claim the funds available to your public key hash.   It is important that you use the same wallet that was used during the redeem script.  If successful, then the Ada from the jackpot that is to be rewarded to you will be sent to your wallet.
 
 ```
./payout-lotto-tx.sh [devnet|testnet|mainnet] [p|s]    where p = player, s = sponsor
 ```
 
## How To Run The Lottery
 
### Setup
1. Download the cardano-lottery repository
```
git clone https://github.com/lley154/cardano-lottery.git
```
2. Determine what unspent transaction (UTXO) from your wallet that can be used.  We will use this for a lottery ID to avoid any collisions with other lotteries.
```
export CARDANO_NODE_SOCKET_PATH=path-to-your-cardano node.socket
cardano-cli query utxo --address <<your address here>> --cardano-mode --testnet-magic 1097911063
```

3. Update cardano-lottery/src/Deploy.hs and save with correct values for:

|Key|Description|Can Be Changed After Init|
|:---|:---|:---|
|lottoAdminPubKeyHashBS=|Lotto Admin Public Key Hash|Yes|
|sponsorPubKeyHashBS=|Sponsor Public Key Hash|No|
|txIdBS=|A UTXO (unspent transaction output) that will be spent during the init tx from step 2|No|
|txIdIdxInt=|The index of the UTXO above|No|
|difficultyInt=|The difficulty of the lottery|No|
|jackpotSplitInt=|The % allocation of the jackpot that will go to the sponsor|No|
|ticketCostInt=|This is the base amount which is multiplied by 100 (eg. 20,000 * 100 = 2,000,000 or 2 ADA)|Yes|
|percentFeesInt=|The percentage of ticket sales that goes to the lotto admin (0-100)|Yes|

4. Download the corresponding plutus-app source code here: https://github.com/input-output-hk/plutus-apps (checkout tag e4d852ffcf6622e0c8359b73170a28b6e5cefc46)
5. Go into the plutus-app directory and run nix-shell - this may take a while and 30GB+ disk space
6. You should see the ```[nix-shell:~/src/plutus-apps]$```  beside your command prompt which confirms that are you in a nix shell
7. Go to the cardano-lottery directoy while still in the nix-shell
8. ```[nix-shell:~/src/cardano-lottery]$ cabal repl cardano-lottery```    (this compiles the lottery)
9. ```Prelude Types> Deploy.main```          (this will create the updated data files we will need)
10. You can quit the repl by typing :q
11. You can also exit the nix-shell since we only needed it to run the Deploy.main
12. Copy the generated plutus scripts and data into the scripts data directory 
```
~/src/cardano-lottery$ cp deploy/* scripts/cardano-cli/[choose your env]/data/
```
13. Go to the scripts directory ```~/src/cardano-lottery-private/scripts/cardano-cli/[choose your env]/```
14. Update the following values in global-export-variables.sh.

```
# PROJECT_ID is for blockfrost datum query, get your account here
# https://blockfrost.io/
export PROJECT_ID=
export BASE=path-to cardano-lottery
export CARDANO_CLI=path-to cardano-cli
export CARDANO_NODE_SOCKET_PATH=path-to node.socket
export TESTNET_MAGIC=update if required
export ADMIN_VKEY=path-to vkey
export ADMIN_SKEY=path-to skey
export ADMIN_PKH=path-to pkh
export PLAYER_VKEY=path-to vkey
export PLAYER_SKEY=path-to skey
export PLAYER_PKH=path-to pkh
export SPONSOR_VKEY=path-to vkey
export SPONSOR_SKEY=path-to skey
export SPONSOR_PKH=path-to pkh
export TICKET_MIN_VALUE=1500000  (default, set as required)
export ADMIN_COLLATERAL=txId#TxIdIdx
export PLAYER_COLLATERAL=txId#TxIdIdx
export SPONSOR_COLLATERAL=txId#TxIdIdx
```

 
#### Initialize
The initialize state creates the lotto token (thread token) and the buy token.  Each of these tokens are unique and can only exist at most once because they are minted using an unspent transaction that is spent as part of the init transaction.   These 2 tokens are key to confirming what inputs and outputs belong to the the lotto and buy validator scripts.   The init script only needs to be run once.

```
 ./init-lotto-tx.sh [devnet|testnet|mainnet]
 
Estimated transaction fee: Lovelace 423404
tx has been built
tx has been signed
Transaction successfully submitted.

 ```
 If successful, you will see something like this using cardano-cli
 ```
 [nix-shell:~/src/cardano-lottery]$ cardano-cli address build --payment-script-file $BASE/scripts/cardano-cli/testnet/data/lotto-validator.plutus --testnet-magic 1097911063
addr_test1wzpw0uuxz974za6vs970299ujfh378fdqdfrrktnfh8lp5cky59qg
[nix-shell:~/src/cardano-lottery]$ cardano-cli query utxo --address addr_test1wzpw0uuxz974za6vs970299ujfh378fdqdfrrktnfh8lp5cky59qg --cardano-mode --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
985b372e8c5affd7302656dff95cb7e9e993b99a12b966ed115e5589849b15b1     1        10000000 lovelace + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.07b3514f9a8068cd067d4be15d87b19eb51f340a0a408eebc9a0513742279262 + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.5011ddb9bb5e8b389b215624618b1252c33a2e1597d0e66ce5a58ec568538a37 + TxOutDatumHash ScriptDataInAlonzoEra "4199db6fbdc9030b601a1fd95895308fd762aae43ae1da0cf46c573c41e835e5"
```
 
#### Open
The open script allows the lotto admin to change some key settings defined during the lotto initialization or from prior lotto cycles.  As part of the open transaction, the lotto admin can change the public key hash of the lotto admin wallet, the cost of the ticket and the % allocation of new ticket sales that goes towards fees.   If the lotto admin public key is changing, it will still require the key that was used during the previous state to sign the open transaction.  The new admin key will be used going forward once the open transaction is completed.
 
 ```
./open-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 You will need to wait and verify that the transaction is completed on the network, so you should see somthing like the following.
 ```
 [nix-shell:~/src/cardano-lottery]$ cardano-cli query utxo --address addr_test1wzpw0uuxz974za6vs970299ujfh378fdqdfrrktnfh8lp5cky59qg --cardano-mode --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5a6f42a83bd61034a2bcdd2ab69d732721920e7f7b327913c31ad2fff8e331a2     1        20000000 lovelace + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.07b3514f9a8068cd067d4be15d87b19eb51f340a0a408eebc9a0513742279262 + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.5011ddb9bb5e8b389b215624618b1252c33a2e1597d0e66ce5a58ec568538a37 + TxOutDatumHash ScriptDataInAlonzoEra "fcec9d0bf8b4f391c6e7a1626dc936ab1111673ca40ebc5536ac6f3773be566e"
```
 
#### StartBuy
The start-buy script will move the buy token to the buy validator script which allows people to buy lotto tickets.   If this does not occur, then people will be unable to buy and mint the lotto tickets.
 
 ```
./start-buy-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 Once the transaction is submitted, you should see the following.
 ```
 lotto_validator address, and notice that the buy token is no longer locked.
 [nix-shell:~/src/cardano-lottery]$ cardano-cli query utxo --address addr_test1wzpw0uuxz974za6vs970299ujfh378fdqdfrrktnfh8lp5cky59qg --cardano-mode --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
308395bbd1ea297ef3fb44b3f79a24151d4c240a1ec2c71fccd77612be87fc04     1        20000000 lovelace + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.5011ddb9bb5e8b389b215624618b1252c33a2e1597d0e66ce5a58ec568538a37 + TxOutDatumHash ScriptDataInAlonzoEra "fcec9d0bf8b4f391c6e7a1626dc936ab1111673ca40ebc5536ac6f3773be566e"

buy_validator address:
[nix-shell:~/src/cardano-lottery]$ cardano-cli query utxo --address addr_test1wzgetu74nu2vgu9uhrhjv6lxw5vuh5dayctasctr9lpuyjc80jakp --cardano-mode --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
308395bbd1ea297ef3fb44b3f79a24151d4c240a1ec2c71fccd77612be87fc04     2        2000000 lovelace + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.07b3514f9a8068cd067d4be15d87b19eb51f340a0a408eebc9a0513742279262 + TxOutDatumHash ScriptDataInAlonzoEra "8083c2a15e2830de23226540e3765aacb3a54a8e8eb7b92ed9ef128349997418"
```

 
#### Buy
The players can now buy lotto tickets.
 
 ```
./buy-lotto-tx.sh [devnet|testnet|mainnet] [n]    where n = ticket number
 ```
You should then see the minted ticket in your wallet.  
```
[nix-shell:~/src/cardano-lottery]$ cardano-cli query utxo --address addr_test1vrdlyzsmw2fpjlw5fe2cpaemcm63vde48c6ntvl303r6kdqqw0njq --cardano-mode --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
00d54a08acc6987667da2740dd4bb0628822594d0b7bddb4e4347d2d0fbf37ce     0        984981969 lovelace + TxOutDatumNone
00d54a08acc6987667da2740dd4bb0628822594d0b7bddb4e4347d2d0fbf37ce     2        2000000 lovelace + 1 ee96db7254eef0b18f318d8a12055b8c577bf31dad6d28a90f13c264.3138 + TxOutDatumNone
45207af30d9eee5b841f662be91fe8be33ebd72eac7cb080655656a804ff9899     1        10000000 lovelace + TxOutDatumNone
```
The ticket number is encoded in Hex format so in this example, the minted ticket is:
```
sequence = 1
ticket number = 8
hex conversion of 18 = 3138
```
 
#### StopBuy
The stop-buy script will move the buy token back to the lotto validator script and also move all collected funds back to the lotto script as well.   Once this is completed, no one will be able to buy a lotto ticket until the next lotto cycle.
 
 ```
./stop-buy-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
#### Close
The close transaction is very important because it will transition the lotto state to close and provide the close tx hash in the lotto datum which will be used for the winning number generation. 
 
 ```
./close-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
#### Draw
The draw transaction will then calculate the winning numbers based on the closed tx hash provided during the previous step.  These pseudo random numbers are validated both on and off chain to ensure they are correct.  Since the winning numbers are based on the close tx hash and it is not possible to buy lotto tickets during the close state, this ensures that no one will be able to predict the winning lotto numbers during the buy phase.
 
 ```
./draw-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
#### Redeem
The Player with a winning number can now redeem their ticket.
 
 ```
./redeem-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
#### CalcPayout
Once you have provided sufficient notice and time for players to redeem, you can then move forward to calculate the payouts.  The payout is determined by the amount of Ada in the jackpot and then the amount of the jackpot that goes to the sponsor vs the player.  This is calculated by the % jackpot split defined during the lotto Init phase.   

 NOTE: Once the lotto moves to CalcPayout state, a player can no longer redeem a winning ticket.  
 
 ```
./calcpayout-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
#### Payout
The player who previously redeemed their ticket can now claim his payout.  The sponsor can also claim a payout if there was a winner during this lotto cycle.
 
 ```
./payout-lotto-tx.sh [devnet|testnet|mainnet] [p|s]    where p = player, s = sponsor
 ```
 
#### Collect
The lotto admin can collect fees anytime, but be sure there is more than the minimum amount Ada required for a cardano transaction otherwise this transaction will be done at a loss.
 
 ```
./collect-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
#### End
If there are no winners or if all the winnings have been claimed, then you close the lotto.  To start another lottery cycle, go back to Open step and repeat the steps above.
 
Note: You can also close the lotto if there are unclaimed funds and these will get recycled back into the jackpot for the next lotto cycle.   
 
 ```
./end-lotto-tx.sh [devnet|testnet|mainnet]
 ```
 
## The Open Source Lottery Design
 
### 3 Key Threat Vectors
- Players can't predict the winning number in advance
- Players have to pay for the lotto ticket and can't steal any funds
- Lotto admin can't access jackpot and treasury funds
 
### High Level Design
![high level design](/images/lotto-high-level-design.png)
 
This is a simplified diagram that shows some of the transactions involved during a lottery cycle

 
### Funding Model
![lottery funding model](/images/lotto-funding.png)
 
When a lotto ticket is purchased, the ticket cost will get split into three buckets
1. jackpot
2. treasury
3. admin fees
 
The % amount that goes to admin fees is configurable and can be changed at the start of each lotto cycle.  The remaining amount will be split between the jackpot and the treasury equally. 
 
The sponsor will get a % of the jackpot rewards if there is a winner during the current lotto cycle.   The % is also configurable but only at the lotto initialization state. 
 
 
## Roadmap Items
- Mobile app integration
- Plutus V2 updates/integration
- Browser wallet integration
- Staking rewards of Ada locked in Lotto contract will go to the sponsor
- Governance model/contract that allows for changing of the lotto parameters
- Multi-player winners
- Jackpot sweeteners (eg adding NFT and/or custom currencies)
- Automated & property based testing 
 
## Developer Testing
If you want to run the lottery locally for testing, the best way to do this is to use the PAB simluator.  This can be done by doing the following.
 
1. Download the latest stable source code release here:  https://github.com/lley154/cardano-lottery
2. Download the corresponding plutus-app source code here: https://github.com/input-output-hk/plutus-apps
3. go into the plutus-app directory and run nix-shell - this will take a while and 30GB+ disk space
4. you should see the [nix-shell:~/src/plutus-apps]$ beside your command prompt which confirms that are you in a nix shell
5. cd to the cardano-lottery directoy while still in the nix-shell
6. [nix-shell:~/src/cardano-lottery]$ cabal build cardano-lottery-pab-sim
7. [nix-shell:~/src/cardano-lottery]$ cabal exec cardano-lottery-pab-sim
 
This will run the PAB simulator using the cardano-lottery/app/Main-sim.hs file
 
 ## Coding Style
- I have chosen readability over advanced haskel programing techniques 
- Plutus scripts must operate in a resource constrained environment so this is why there is minimal logging onchain
 
 ## Tips & Tricks
 
#### How To Run cardano-cli When The Cardano Node Is Running On A Different Server
Log into the server with the cardano-node running with ssh and use the -L to forward traffic from the socket on the server to your computer locally.

```
ssh -L  /path-to-node-socket-on-your-local-computer/node.socket:/path-to-the-node-socket-on-your-server/node.socket cardano-server

export $CARDANO_NODE_SOCKET_PATH=/path-to-node-socket-on-your-local-computer/node.socket
cardano-cli --version
cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
git rev 0000000000000000000000000000000000000000
```
 
 #### How To Create A Verification, Signing Key, Public Key Hash and Address
```
cardano-cli address key-gen --verification-key-file /path-to-key/01.vkey --signing-key-file /path-to-key/01.skey

cardano-cli address key-hash --payment-verification-key-file=/path-to-key/01.vkey --out-file=/path-to-key/01.pkh

cardano-cli address build --testnet-magic 1097911063 --payment-verification-key-file /path-to-key/01.vkey

addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v
```

 #### How To Determine Your Unspent UTXO
This example is using the testnet.
```
cardano-cli query utxo --address addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v --cardano-mode --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f1c2000121a328a97926948a7482cf44f0cc565b67f52ac046a26a713e8a09e4     0        810635607 lovelace + TxOutDatumNone
fd5cc20668b84aaf4fe87b01d7075fc250e93f790664fd95d3ce45b703db2ad0     1        10000000 lovelace + TxOutDatumNone
```

#### How To See the UTXOs At A Script Address
```
cardano-cli address build --payment-script-file $BASE/scripts/cardano-cli/testnet/data/lotto-validator.plutus  --testnet-magic 1097911063
addr_test1wzkzcpa90clq9k6yfetpxye0ce56xgvm48tk3genzlymksc5qfgu4

cardano-cli query utxo --address addr_test1wzkzcpa90clq9k6yfetpxye0ce56xgvm48tk3genzlymksc5qfgu4 --cardano-mode --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c004a997c86687d56560f677568eb5564ae5d1818db46c8f66f9fecec558ffcb     1        10000000 lovelace + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.23c9c05e3411eff787eaf1198c734cf31d1e5cfe3f90ad2eeff6d93d9f4a32c7 + 1 a46b817b3ce8bf9a3a3c7b4b9d13b8801bdc72f2b4c30838c54a7b12.91af2c15473a5335e080984dda0142f877a7ccadc8fd91a38a0727f9f8332251 + TxOutDatumHash ScriptDataInAlonzoEra "89f0d93585d8be7c600ca22544b759982d31372cbbf40a9d9a2fbf04a5f18719"

```
#### How To Transfer ADA To Your Wallet To Setup Collateral UTXO

```
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "dcf0277523ad02943c1ff3a4d5f58632500770c86fee10288cb80a46318dd45c#0" \
  --tx-out "addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v+10000000" \
  --change-address "addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v" \
  --out-file "$WORK/transfer-tx-alonzo.body"

cardano-cli transaction sign \
  --tx-body-file "$WORK/transfer-tx-alonzo.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "/path-to-key/01.skey" \
  --out-file "$WORK/transfer-tx-alonzo.tx"
  
cardano-cli transaction submit --tx-file $WORK/transfer-tx-alonzo.tx --testnet-magic 1097911063  
```
#### How To Use The Local Devnet
- Download the cardano-node here https://github.com/input-output-hk/cardano-node
- cd into ```cardno-node``` and type ```nix-shell```
- once this is completed, you will see the following lines 
```
  Commands:
    * nix flake lock --update-input <iohkNix|haskellNix> - update nix build input
    * cardano-cli - used for key generation and other operations tasks
    * wb - cluster workbench
    * start-cluster - start a local development cluster
    * stop-cluster - stop a local development cluster
    * restart-cluster - restart the last cluster run (in 'run/current')
                        (WARNING: logs & node DB will be wiped clean)
```
- run the ```start-cluster``` command to start a local cluster on your computer
- once this is complete, set your ```CARDANO_NODE_SOCKET_PATH``` to one of the node sockets
```
export CARDANO_NODE_SOCKET_PATH=/path-to-cardano-node/cardano-node/run/current/node-0/node.socket
```
The cluster settings can be found in the run directory in the cardano-node folder.
```
[nix-shell:~/src/cardano-node/run/current]$ ls -l
total 64
-rw-r--r-- 1 lawrence lawrence  309 Apr 29 20:42 env.json
drwxr-xr-x 2 lawrence lawrence 4096 Apr 29 20:42 generator
lrwxrwxrwx 1 lawrence lawrence   73 Apr 29 20:42 genesis -> /path-to-home-directory/.cache/cardano-workbench/genesis/k3-d1-0.003kD-0kU-9082bbc
-rw------- 1 lawrence lawrence 8402 Apr 29 20:42 genesis.alonzo.json
-rw------- 1 lawrence lawrence 3682 Apr 29 20:42 genesis-shelley.json
-rw-r--r-- 1 lawrence lawrence 3667 Apr 29 20:42 meta.json
drwxr-xr-x 3 lawrence lawrence 4096 Apr 29 20:42 node-0
drwxr-xr-x 3 lawrence lawrence 4096 Apr 29 20:42 node-1
drwxr-xr-x 3 lawrence lawrence 4096 Apr 29 20:42 node-2
drwxr-xr-x 3 lawrence lawrence 4096 Apr 29 20:42 node-3
-rw-r--r-- 1 lawrence lawrence  468 Apr 29 20:42 node-specs.json
-rw-r--r-- 1 lawrence lawrence 2782 Apr 29 20:42 profile.json
drwxr-xr-x 2 lawrence lawrence 4096 Apr 29 20:42 supervisor
drwxr-xr-x 2 lawrence lawrence 4096 Apr 29 20:42 topology

```
Build the address and get the utxo for a gensis utxo
```
[nix-shell:~/src/cardano-node]$  cardano-cli address build --testnet-magic 42 --payment-verification-key-file /path-to-home-directory/.cache/cardano-workbench/genesis/k3-d1-0.003kD-0kU-9082bbc/utxo-keys/utxo1.vkey
addr_test1vz5s4fjz95vljmw9qyywntq0gzz98ysyp80dt7a9lkre4zgnsgfpc

[nix-shell:~/src/cardano-node]$  cardano-cli query utxo --address addr_test1vz5s4fjz95vljmw9qyywntq0gzz98ysyp80dt7a9lkre4zgnsgfpc --cardano-mode --testnet-magic 42
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8fb372a25f7b3508e9bed7a84de9e3f4c45356859e92c2b5f7408d57c55a0422     0        90000000000000 lovelace + TxOutDatumNone
```
Then get the addresses of your pkh for the lotto admin, sponsor and player.
```

[nix-shell:~/src/cardano-node]$  cardano-cli address build --testnet-magic 42 --payment-verification-key-file /path-to-keys/01.vkey
addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v
[nix-shell:~/src/cardano-node]$  cardano-cli address build --testnet-magic 42 --payment-verification-key-file /path-to-keys/02.vkey
addr_test1vptq4mev2egfu6p8nk0yh20uh2yg3eak0z36pcqynh5rkqsdnhhv3
[nix-shell:~/src/cardano-node]$  cardano-cli address build --testnet-magic 42 --payment-verification-key-file /path-to-keys/03.vkey
addr_test1vrdlyzsmw2fpjlw5fe2cpaemcm63vde48c6ntvl303r6kdqqw0njq
```
Now create a transaction to send Ada to each of those addreses, including the collateral that will be required.
```
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 42 \
  --tx-in "8f08c8680dd85a6b9b8225cc1ab7079168a35c651d30d230552c6541455d8e60#0" \
  --tx-out "addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v+1000000000" \
  --tx-out "addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v+10000000" \
  --tx-out "addr_test1vptq4mev2egfu6p8nk0yh20uh2yg3eak0z36pcqynh5rkqsdnhhv3+1000000000" \
  --tx-out "addr_test1vptq4mev2egfu6p8nk0yh20uh2yg3eak0z36pcqynh5rkqsdnhhv3+10000000" \
  --tx-out "addr_test1vrdlyzsmw2fpjlw5fe2cpaemcm63vde48c6ntvl303r6kdqqw0njq+1000000000" \
  --tx-out "addr_test1vrdlyzsmw2fpjlw5fe2cpaemcm63vde48c6ntvl303r6kdqqw0njq+10000000" \
  --change-address "addr_test1vz5s4fjz95vljmw9qyywntq0gzz98ysyp80dt7a9lkre4zgnsgfpc" \
  --out-file "$WORK/transfer-tx-alonzo.body"

cardano-cli transaction sign \
  --tx-body-file "$WORK/transfer-tx-alonzo.body" \
  --testnet-magic 42 \
  --signing-key-file "/home/lawrence/.cache/cardano-workbench/genesis/k3-d1-0.003kD-0kU-9082bbc/utxo-keys/utxo1.skey" \
  --out-file "$WORK/transfer-tx-alonzo.tx"

cardano-cli transaction submit --tx-file $WORK/transfer-tx-alonzo.tx --testnet-magic 42 
```
 
#### Devnet Blockchain TX Monitoring
```
[nix-shell:~/src/cardano-node/run/current/node-0]$ tail -f stdout | grep tip
``` 

#### How To Change The Lotto Ticket Cost Or Admin Public Key Hash
 - update open-lotto-tx.sh bash shell script accordingly
 - update Deploy.hs and then:
 ```
[nix-shell:~/src/cardano-lottery]$ cabal repl cardano-lottery    (this compiles the lottery)
Prelude Types> Deploy.main          (this will create the updated data files we will need)
Prelude Types> :q
[nix-shell:~/src/cardano-lottery]$ cp plutus-scripts/* scripts/cardano-cli/[choose your env]/data/
 ```
 
 
 #### How To Undo A Devnet Lotto State Machine Step 
Only do this if the ```cardano-cli``` transaction failed and you need to try it again
```
[nix-shell:~/src/cardano-lottery-private]$ cd work-backup/
[nix-shell:~/src/cardano-lottery-private/work-backup]$ cp * ../work
```


## System Requirements
- Recommended and tested OS: Linux Ubuntu 20.04.1
- 8 GB Memory
- 30+ GB Disk  



## Legal Notice
```
Copyright 2022 Context Solutions Inc. 

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 ```
