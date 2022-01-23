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
import           Data.ByteString.Lazy.Char8          as BSL
import           Data.Default                        (def)
import qualified Plutus.V1.Ledger.Slot               as Slot 
import           Ledger.Address                      (Address, PaymentPubKeyHash, pubKeyHashAddress)
import           Ledger.CardanoWallet   qualified as CW
import           Lottery
import qualified Ledger.TimeSlot                     as TimeSlot
import           LottoContract                       (StarterContracts(..))
import qualified Data.Monoid                         as Monoid
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Wallet              (Wallet, knownWallet)
--import           Wallet.API                          (WalletAPIError, ownPaymentPubKeyHash)
--import           Wallet.API                          (ownPubKeyHash)


{-
main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)
-}

defaultWallet :: Wallet
defaultWallet = knownWallet 1

defaultWalletPaymentPubKeyHash :: PaymentPubKeyHash
defaultWalletPaymentPubKeyHash = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 1)

defaultWalletPaymentPubKeyHashAddress :: Address
defaultWalletPaymentPubKeyHashAddress = pubKeyHashAddress defaultWalletPaymentPubKeyHash Nothing

benWalletPaymentPubKeyHash :: PaymentPubKeyHash
benWalletPaymentPubKeyHash = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 4)


slotCfg :: TimeSlot.SlotConfig
slotCfg = def

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    

    Simulator.logString @(Builtin StarterContracts) "********* PAB Server is running *********"
    Simulator.logString @(Builtin StarterContracts) "Activate Init Contract then press return"
    void $ liftIO getLine

    cidInit <- Simulator.activateContract defaultWallet InitLottoContract

    --adminPkh <- defaultWalletPaymentPubKeyHash
    --adminPkh <- pubKeyHashAddress <$> Simulator.handleAgentThread defaultWallet (Just cid) ownPaymentPubKeyHash
    --adminPkh <- Simulator.handleAgentThread defaultWallet (Just cidInit) ownPaymentPubKeyHash
    --    where
    --        ownPaymentPubKeyHashH :: (Member (State WalletState) effs) => Eff effs PaymentPubKeyHash
    --        ownPaymentPubKeyHashH = gets (CW.paymentPubKeyHash . _mockWallet)

    Simulator.logString @(Builtin StarterContracts) $ "adminPkh = " ++ show defaultWalletPaymentPubKeyHash
    Simulator.logString @(Builtin StarterContracts) $ "adminPkhAddress = " ++ show defaultWalletPaymentPubKeyHashAddress


    let jackpot'   = 10000000 -- 10 Ada
        ticket'    = 2000000  -- 2 Ada
        deadline'  = TimeSlot.slotToEndPOSIXTime slotCfg (Slot.Slot 5000)
        sp         = StartParams
            { spAdmin       = defaultWalletPaymentPubKeyHash
            , spBenAddress  = defaultWalletPaymentPubKeyHash
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            }
        useTT = True
    
    Simulator.logString @(Builtin StarterContracts) "sp parms: "
    Simulator.logString @(Builtin StarterContracts) (show $ encode sp)
    Simulator.logString @(Builtin StarterContracts) "useTT : "
    Simulator.logString @(Builtin StarterContracts) (show $ encode useTT)

    Simulator.logString @(Builtin StarterContracts) "Lotto init contract wallet 1 (lotto admin)"
    void $ liftIO getLine

    --Simulator.logString @(Builtin StarterContracts) "Enter lotto admin contract ID"
    --cidInit <- readCommandIO
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

    --benPkh <- Simulator.handleAgentThread (knownWallet 4) ownPaymentPubKeyHash

    let sp' = StartParams
            { spAdmin       = defaultWalletPaymentPubKeyHash
            , spBenAddress  = benWalletPaymentPubKeyHash
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



