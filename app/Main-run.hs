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


import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), fromJSON)
import           Data.Default                        (def)
import qualified Plutus.V1.Ledger.Slot               as Slot 
import           Lottery
import qualified Ledger.TimeSlot                     as TimeSlot
import           LottoContract                       (StarterContracts(..))
import qualified Data.Monoid                         as Monoid
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
--import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
--import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Core                     as Core
import           Plutus.PAB.Run                      (runWith)
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
--import           Wallet.Emulator.Wallet              (Wallet, knownWallet)
--import           Wallet.API                          (ownPubKeyHash)
import           Control.Monad.Freer.Extras.Log      (LogMessage, LogMsg, LogObserve, logDebug, logInfo)


slotCfg :: TimeSlot.SlotConfig
slotCfg = def

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)
--    shutdown <- PAB.Server.startServerDebug
    
--    cidInit <- Core.activateContract (Wallet "xfs") InitLottoContract
--    adminPkh <- Core.handleAgentThread (Wallet "xfs")  ownPubKeyHash
--    logInfo @(Builtin StarterContracts) $ "adminPkh = " ++ show adminPkh


--defaultWallet :: Wallet
--defaultWallet = knownWallet 1

    logString @(Builtin StarterContracts) "********* PAB Server is running *********"
    logString @(Builtin StarterContracts) "Activate Init Contract then press return"
    void $ liftIO getLine



{-

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
    
    Simulator.logString @(Builtin StarterContracts) "Lotto init contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cidInit "init" (sp, useTT)
    Simulator.waitNSlots 5

    lotToken <- flip Simulator.waitForState cidInit $ \json -> case (fromJSON json :: Result (Monoid.Last Lottery.Lottery)) of
                    Success (Monoid.Last (Just lotToken))   -> Just lotToken
                    _                                       -> Nothing
    Simulator.logString @(Builtin StarterContracts) $ "Lotto found: " ++ show lotToken

    cid1 <- Simulator.activateContract defaultWallet $ UseLottoContract lotToken
    cid2 <- Simulator.activateContract (knownWallet 2) $ UseLottoContract lotToken
    cid3 <- Simulator.activateContract (knownWallet 3) $ UseLottoContract lotToken
    cid4 <- Simulator.activateContract (knownWallet 4) $ UseLottoContract lotToken

    benPkh <- Simulator.handleAgentThread (knownWallet 4) ownPubKeyHash

    let sp' = StartParams
            { spAdmin       = adminPkh
            , spBenAddress  = benPkh
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            }

    Simulator.logString @(Builtin StarterContracts) "Lotto start contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "start" sp'
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto buy 123 contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "buy" (123 :: Integer)
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto buy 789 contract wallet 3 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid3 "buy" (789 :: Integer)
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto close 123 contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "close" (123 :: Integer)
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto redeem contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "redeem" ()
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto calc-payout contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "calc-payout" ()
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto payout contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "payout" ()
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto payout contract wallet 4 (sponsor)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid4 "payout" ()
    Simulator.waitNSlots 5

    Simulator.logString @(Builtin StarterContracts) "Lotto collect contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "collect" ()
    Simulator.waitNSlots 5


    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown
    

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers = Simulator.mkSimulatorHandlers def def $ interpret (contractHandler Builtin.handleBuiltin)

-}

