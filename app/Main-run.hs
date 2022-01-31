{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}


module Main
    ( main
    ) where

import          LottoContract                                    (StarterContracts(..))
import          Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
--import          Plutus.PAB.LocalCluster.Run         (runWith)
import Plutus.PAB.Run (runWith)


main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)


