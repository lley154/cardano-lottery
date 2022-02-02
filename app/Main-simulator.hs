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
import           Lottery
import qualified Ledger.TimeSlot                     as TimeSlot
import           LottoContract                       (StarterContracts(..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.V1.Ledger.Slot               as Slot 
import           Wallet.Emulator.Wallet              (Wallet, knownWallet)


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
    Simulator.logString @(Builtin StarterContracts) $ "adminPkh = " ++ show defaultWalletPaymentPubKeyHash
    Simulator.logString @(Builtin StarterContracts) $ "adminPkhAddress = " ++ show defaultWalletPaymentPubKeyHashAddress


    let jackpot'   = 10000000 -- 10 Ada
        ticket'    = 20000    -- 0.02 Ada (base amount * 100 = 2 Ada)
        deadline'  = TimeSlot.slotToEndPOSIXTime slotCfg (Slot.Slot 50000)
        sp         = StartParams
            { spAdmin       = defaultWalletPaymentPubKeyHash
            , spBenAddress  = defaultWalletPaymentPubKeyHash
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            }
        useTT = True


    let params = (sp, useTT)
    Simulator.logString @(Builtin StarterContracts) "params : "
    Simulator.logString @(Builtin StarterContracts) $ show params

    Simulator.logString @(Builtin StarterContracts) "Lotto init contract wallet 1 (lotto admin)"
    void $ liftIO getLine

    void $ Simulator.callEndpointOnInstance cidInit "init" params
    Simulator.waitNSlots 5

    lot <- flip Simulator.waitForState cidInit $ \json -> case (fromJSON json :: Result (Monoid.Last Lottery.Lottery)) of
                    Success (Monoid.Last (Just lot))   -> Just lot
                    _                                       -> Nothing
    Simulator.logString @(Builtin StarterContracts) $ "Lotto found: " ++ show lot

    liftIO $ LB.writeFile "lotto.json" $ encode lot

    cid1 <- Simulator.activateContract defaultWallet $ UseLottoContract
    cid2 <- Simulator.activateContract (knownWallet 2) $ UseLottoContract
    cid3 <- Simulator.activateContract (knownWallet 3) $ UseLottoContract
    cid4 <- Simulator.activateContract (knownWallet 4) $ UseLottoContract

    let sp' = StartParams
            { spAdmin       = defaultWalletPaymentPubKeyHash
            , spBenAddress  = benWalletPaymentPubKeyHash
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            }

    let startParams = (lot, sp')
    Simulator.logString @(Builtin StarterContracts) "start params : "
    Simulator.logString @(Builtin StarterContracts) $ show startParams
    liftIO $ LB.writeFile "start.json" $ encode startParams
    
    Simulator.logString @(Builtin StarterContracts) "Lotto start contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "start" startParams
    Simulator.waitNSlots 5


    let buyParams = (lot, 123::Integer)
    Simulator.logString @(Builtin StarterContracts) "buy params : "
    Simulator.logString @(Builtin StarterContracts) $ show buyParams
    liftIO $ LB.writeFile "buy.json" $ encode buyParams

    Simulator.logString @(Builtin StarterContracts) "Lotto buy 123 contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams
    Simulator.waitNSlots 5


    let buyParams' = (lot, 789::Integer)
    Simulator.logString @(Builtin StarterContracts) "buy params : "
    Simulator.logString @(Builtin StarterContracts) $ show buyParams'

    Simulator.logString @(Builtin StarterContracts) "Lotto buy 789 contract wallet 3 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid3 "buy" buyParams'
    Simulator.waitNSlots 5

    let closeParams = (lot, 123::Integer)
    Simulator.logString @(Builtin StarterContracts) "close params : "
    Simulator.logString @(Builtin StarterContracts) $ show closeParams
    liftIO $ LB.writeFile "close.json" $ encode closeParams

    Simulator.logString @(Builtin StarterContracts) "Lotto close 123 contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "close" closeParams
    Simulator.waitNSlots 5

    
    let redeemParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "redeem params : "
    Simulator.logString @(Builtin StarterContracts) $ show redeemParams
    liftIO $ LB.writeFile "redeem.json" $ encode redeemParams
    
    Simulator.logString @(Builtin StarterContracts) "Lotto redeem contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "redeem" redeemParams
    Simulator.waitNSlots 5


    let calcParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "calc params : "
    Simulator.logString @(Builtin StarterContracts) $ show calcParams
    liftIO $ LB.writeFile "calc.json" $ encode calcParams

    Simulator.logString @(Builtin StarterContracts) "Lotto calc-payout contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "calc-payout" calcParams
    Simulator.waitNSlots 5

    
    let payoutParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "payout params : "
    Simulator.logString @(Builtin StarterContracts) $ show payoutParams
    liftIO $ LB.writeFile "payout.json" $ encode payoutParams

    Simulator.logString @(Builtin StarterContracts) "Lotto payout contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "payout" payoutParams
    Simulator.waitNSlots 5


    let payParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "payout params : "
    Simulator.logString @(Builtin StarterContracts) $ show payParams

    Simulator.logString @(Builtin StarterContracts) "Lotto payout contract wallet 4 (sponsor)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid4 "payout" payParams
    Simulator.waitNSlots 5

    --let collectParams = (lot)
    --Simulator.logString @(Builtin StarterContracts) "collect params : "
    --Simulator.logString @(Builtin StarterContracts) $ show collectParams
    
    --Note: only collect fees if more than minimum amount of ada has been collected
    --Simulator.logString @(Builtin StarterContracts) "Lotto collect contract wallet 1 (lotto admin)"
    --void $ liftIO getLine
    --void $ Simulator.callEndpointOnInstance cid1 "collect" ()
    --Simulator.waitNSlots 5

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers = Simulator.mkSimulatorHandlers def def $ interpret (contractHandler Builtin.handleBuiltin)



