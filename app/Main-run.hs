{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}


module Main
    ( main
    ) where

import          LottoContract                                    (StarterContracts(..))
import          Plutus.PAB.Effects.Contract.Builtin qualified as Builtin

-- enable if using localcluster (devnet) 
import          Plutus.PAB.LocalCluster.Run         (runWith)

-- enable if using testnet, comment out if using local cluster (devnet)
--import Plutus.PAB.Run (runWith)


main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)


