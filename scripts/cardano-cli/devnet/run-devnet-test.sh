#!/usr/bin/env bash

set -e
set -o pipefail

##########################################################################
# Please run before running this script
# 1) Get a lotto admin UTXO greater than 10 Ada
#       [nix-shell:~/src/cardano-lottery]$ cardano-cli query utxo --address addr_test1vznkvztpdrp3ww03k5hw9p74kfadpa5t5ajxyvq4v4qxgxgxzrs4v --cardano-mode --testnet-magic 42
# 2) Grab the UTXO and update txIdBS & txIdIdxInt in Deploy.hs
# 3) then run 
#       [nix-shell:~/src/cardano-lottery]$ cabal repl cardano-lottery
# 4) then run 
#       Prelude Types> Deploy.main
# 5) finally, copy over the newly created data files
#       [nix-shell:~/src/cardano-lottery]$ cd plutus-scripts/
#       [nix-shell:~/src/cardano-lottery/plutus-scripts]$ cp * ../scripts/cardano-cli/[devnet|testnet|mainnet]/data/
#
###########################################################################

./init-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the init tx on the blockchain"
read

./open-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the open tx on the blockchain"
read

./startbuy-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the startbuy tx on the blockchain"
read

for i in {0..9}
    do
        ./buy-lotto-tx.sh devnet $i
        sleep 5
        printf "\n please confirm before proceeding that you see the buy tx on the blockchain"
        read
    done


./transfertoken-lotto-tx.sh devnet 2
sleep 5
printf "\n please confirm before proceeding that you see the buy tx on the blockchain"
read

./stopbuy-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the stopbuy tx on the blockchain"
read

./close-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the close tx on the blockchain"
read

./draw-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the draw tx on the blockchain"
read

./redeem-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the redeem tx on the blockchain"
read

./calc-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the calc tx on the blockchain"
read

./payout-lotto-tx.sh devnet p
sleep 5
printf "\n please confirm before proceeding that you see the payout tx on the blockchain"
read

./end-lotto-tx.sh devnet
sleep 5
printf "\n please confirm before proceeding that you see the end tx on the blockchain"
read






