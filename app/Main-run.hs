{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}


module Main
    ( main
    ) where

import          LottoContract                                    (StarterContracts(..))
import          Plutus.PAB.Effects.Contract.Builtin qualified as Builtin

-- enable localcluster (devnet) by uncommenting the following line
--import          Plutus.PAB.LocalCluster.Run         (runWith)

-- enable testnet/mainnet by using the following line, comment out if using local cluster (devnet)
import Plutus.PAB.Run (runWith)


main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)


