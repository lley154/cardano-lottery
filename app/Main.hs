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

module Main
    ( main
    ) where


import           Control.Monad                       (forM_, void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), encode, fromJSON)
import           Data.Default                        (def)
import           Data.Monoid                         (Last (..))
import qualified Plutus.V1.Ledger.Slot               as Slot 
import           Lottery
import qualified Ledger.TimeSlot                     as TimeSlot
import           LottoContract                       (StarterContracts(..))
import qualified Data.Monoid                         as Monoid
import           Network.HTTP.Req
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Wallet              (Wallet, knownWallet, knownWallets)
import           Wallet.Types                        (ContractInstanceId (..))
import           Wallet.API                          (WalletAPIError, ownPubKeyHash)


{-
main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)
-}

defaultWallet :: Wallet
defaultWallet = knownWallet 1

slotCfg :: TimeSlot.SlotConfig
slotCfg = def

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract defaultWallet InitLottoContract
    adminPkh <- Simulator.handleAgentThread defaultWallet ownPubKeyHash
    Simulator.logString @(Builtin StarterContracts) $ "adminPkh = " ++ show adminPkh

    let jackpot'   = 10000000
        ticket'    = 20000
        deadline'  = TimeSlot.slotToEndPOSIXTime slotCfg (Slot.Slot 50)
        sp         = StartParams
            { spAdmin       = adminPkh
            , spBenAddress  = adminPkh
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            }
        useTT = True
    
    void $ Simulator.callEndpointOnInstance cidInit "init" (sp, useTT)
    Simulator.waitNSlots 5

    lotToken <- flip Simulator.waitForState cidInit $ \json -> case (fromJSON json :: Result (Monoid.Last Lottery.Lottery)) of
                    Success (Monoid.Last (Just lotToken))   -> Just lotToken
                    _                                       -> Nothing
    Simulator.logString @(Builtin StarterContracts) $ "Lotto found: " ++ show lotToken

    cid1 <- Simulator.activateContract defaultWallet $ UseLottoContract lotToken
    void $ Simulator.callEndpointOnInstance cid1 "start" sp
    Simulator.waitNSlots 5

    cid2 <- Simulator.activateContract (knownWallet 2) $ UseLottoContract lotToken
    void $ Simulator.callEndpointOnInstance cid2 "buy" (123 :: Integer)
    Simulator.waitNSlots 5

    
    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers = Simulator.mkSimulatorHandlers def def $ interpret (contractHandler Builtin.handleBuiltin)

