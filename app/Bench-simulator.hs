{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main
    ( main
    ) where
        

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), fromJSON, encode)
import qualified Data.ByteString.Lazy                as LB
import           Data.Default                        (def)
import qualified Data.Monoid                         as Monoid
import           Ledger.Address                      (Address, PaymentPubKeyHash, pubKeyHashAddress)
import           Ledger.CardanoWallet   qualified as CW
import           Benchmark
import qualified Ledger.TimeSlot                     as TimeSlot
import           BenchContract                       (StarterContracts(..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.V1.Ledger.Slot               as Slot 
import           Wallet.Emulator.Wallet              (Wallet, knownWallet)


defaultWallet1 :: Wallet
defaultWallet1 = knownWallet 1

defaultWallet2 :: Wallet
defaultWallet2 = knownWallet 2


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    

    cidInit1 <- Simulator.activateContract defaultWallet1 BenchContract
    cidInit2 <- Simulator.activateContract defaultWallet2 BenchContract

    void $ Simulator.callEndpointOnInstance cidInit1 "give" (5000000::Integer)
    Simulator.waitNSlots 5

    void $ Simulator.callEndpointOnInstance cidInit2 "grab" ()
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Simulator end"
    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers = Simulator.mkSimulatorHandlers def def $ interpret (contractHandler Builtin.handleBuiltin)



