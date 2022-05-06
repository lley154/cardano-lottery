#!/usr/bin/env bash

set -e
set -o pipefail

##########################################################################
# Please run before running this script
# 1) Ensure there is lotto admin utxo greater than 10 Ada
# 2) Use that utxo and update txIdBS & txIdIdxInt in Deploy.hs
# 3) then run 
#       [nix-shell:~/src/cardano-lottery]$ cabal repl cardano-lottery
# 4) then run 
#       Prelude Types> Deploy.main
# 5) finally, copy over the newly created data files
#       [nix-shell:~/src/cardano-lottery]$ cp deploy/* ../scripts/cardano-cli/[testnet|testnet|mainnet]/data/
#
###########################################################################

#./init-lotto-tx.sh testnet
#sleep 5
#printf "\n please confirm before proceeding that you see the init tx on the blockchain"
#read

#./open-lotto-tx.sh testnet
#sleep 5
#printf "\n please confirm before proceeding that you see the open tx on the blockchain"
#read

./startbuy-lotto-tx.sh testnet
sleep 5
printf "\n please confirm before proceeding that you see the startbuy tx on the blockchain"
read

for i in {0..30}
    do
        ./buy-lotto-tx.sh testnet $i
        sleep 60
        #printf "\n please confirm before proceeding that you see the buy tx on the blockchain"
        #read
    done


./transfertoken-lotto-tx.sh testnet 2
sleep 5
printf "\n please confirm before proceeding that you see the buy tx on the blockchain"
read

./stopbuy-lotto-tx.sh testnet
sleep 5
printf "\n please confirm before proceeding that you see the stopbuy tx on the blockchain"
read

./close-lotto-tx.sh testnet
sleep 5
printf "\n please confirm before proceeding that you see the close tx on the blockchain"
read

./draw-lotto-tx.sh testnet
sleep 5
printf "\n please confirm before proceeding that you see the draw tx on the blockchain"
read

./redeem-lotto-tx.sh testnet
sleep 5
printf "\n please confirm before proceeding that you see the redeem tx on the blockchain"
read

./calc-lotto-tx.sh testnet
sleep 5
printf "\n please confirm before proceeding that you see the calc tx on the blockchain"
read

./payout-lotto-tx.sh testnet p
sleep 5
printf "\n please confirm before proceeding that you see the payout tx on the blockchain"
read

./payout-lotto-tx.sh testnet s
sleep 5
printf "\n please confirm before proceeding that you see the payout tx on the blockchain"
read

./end-lotto-tx.sh testnet
sleep 5
printf "\n please confirm before proceeding that you see the end tx on the blockchain"
read






