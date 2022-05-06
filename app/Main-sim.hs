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
import           Data.Aeson                          (Result (..), fromJSON)
import           Data.Default                        (def)
import qualified Data.Monoid                         as Monoid
import           Ledger.Address                      (Address, PaymentPubKeyHash, pubKeyHashAddress)
import           Ledger.CardanoWallet   qualified as CW
import           OffChain
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
    --setLocaleEncoding utf8
    Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    

    Simulator.logString @(Builtin StarterContracts) "********* PAB Server is running *********"
    Simulator.logString @(Builtin StarterContracts) "Activate Init Contract then press return"
    void $ liftIO getLine

    cidInit <- Simulator.activateContract defaultWallet InitLottoContract
    Simulator.logString @(Builtin StarterContracts) $ "adminPkh = " ++ show defaultWalletPaymentPubKeyHash
    Simulator.logString @(Builtin StarterContracts) $ "adminPkhAddress = " ++ show defaultWalletPaymentPubKeyHashAddress


    let jackpot'   = 10000000 -- jackpot increments of 10 Ada
        ticket'    = 20000    -- ticket cost of 0.02 Ada (base amount * 100 = 2 Ada)
        deadline'  = TimeSlot.slotToEndPOSIXTime slotCfg (Slot.Slot 50000)
        fees       = 2       -- % of ticket sales goes to fees (0-100)
        benWeight  = 50       -- % share of jackpot will go to the beneficiary (0-100)
        sp         = StartParams
            { spDifficulty  = 1 
            , spAdmin       = defaultWalletPaymentPubKeyHash
            , spSpon         = benWalletPaymentPubKeyHash
            , spSponWeight   = benWeight
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            , spFees        = fees
            }
   

    Simulator.logString @(Builtin StarterContracts) "params : "
    Simulator.logString @(Builtin StarterContracts) $ show sp

    Simulator.logString @(Builtin StarterContracts) "Lotto init contract wallet 1 (lotto admin)"
    void $ liftIO getLine

    void $ Simulator.callEndpointOnInstance cidInit "init" sp
    Simulator.waitNSlots 2

    lot <- flip Simulator.waitForState cidInit $ \json -> case (fromJSON json :: Result (Monoid.Last OffChain.Lottery)) of
                    Success (Monoid.Last (Just lot))   -> Just lot
                    _                                       -> Nothing
    Simulator.logString @(Builtin StarterContracts) $ "Lotto found: " ++ show lot
    --liftIO $ LB.writeFile "lotto.json" $ encode lot

    cid1 <- Simulator.activateContract defaultWallet $ UseLottoContract    -- lotto admin
    cid2 <- Simulator.activateContract (knownWallet 2) $ UseLottoContract  -- lotto player 1
    cid3 <- Simulator.activateContract (knownWallet 3) $ UseLottoContract  -- lotto player 2
    cid4 <- Simulator.activateContract (knownWallet 4) $ UseLottoContract  -- lotto sponsor

    let sp' = StartParams
            { spDifficulty  = 1 
            , spAdmin       = defaultWalletPaymentPubKeyHash
            , spSpon         = benWalletPaymentPubKeyHash
            , spSponWeight   = benWeight
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            , spFees        = fees
            }


    let openParams = (lot, sp')
    Simulator.logString @(Builtin StarterContracts) "open params : "
    Simulator.logString @(Builtin StarterContracts) $ show openParams
    --liftIO $ LB.writeFile "open.json" $ encode openParams

    
    Simulator.logString @(Builtin StarterContracts) "Lotto open contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "open" openParams
    Simulator.waitNSlots 2


    let startBuyParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "start Buy params : "
    Simulator.logString @(Builtin StarterContracts) $ show startBuyParams
    --liftIO $ LB.writeFile "startBuy.json" $ encode startBuyParams

    Simulator.logString @(Builtin StarterContracts) "Lotto startbuy contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "startbuy" startBuyParams
    Simulator.waitNSlots 2

    let buyParams0 = (lot, 0::Integer)
        buyParams1 = (lot, 1::Integer)
        buyParams2 = (lot, 2::Integer)
        buyParams3 = (lot, 3::Integer)
        buyParams4 = (lot, 4::Integer)
        buyParams5 = (lot, 5::Integer)
        buyParams6 = (lot, 6::Integer)
        buyParams7 = (lot, 7::Integer)
        buyParams8 = (lot, 8::Integer)
        buyParams9 = (lot, 9::Integer)

    Simulator.logString @(Builtin StarterContracts) "buy params : "
    Simulator.logString @(Builtin StarterContracts) $ show buyParams0
    --liftIO $ LB.writeFile "buy.json" $ encode buyParams0

    Simulator.logString @(Builtin StarterContracts) "Lotto buy 0-9 tickets contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams0
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams1
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams2
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams3
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams4
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams5
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams6
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams7
    Simulator.waitNSlots 2
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams8
    Simulator.waitNSlots 2    
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams9
    Simulator.waitNSlots 2

    let buyParams' = (lot, 789::Integer)
    Simulator.logString @(Builtin StarterContracts) "buy params : "
    Simulator.logString @(Builtin StarterContracts) $ show buyParams'

    Simulator.logString @(Builtin StarterContracts) "Lotto buy 789 contract wallet 3 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid3 "buy" buyParams'
    Simulator.waitNSlots 2

    let transferTokenParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "transfertoken params : "
    Simulator.logString @(Builtin StarterContracts) $ show transferTokenParams
    --liftIO $ LB.writeFile "stopBuy.json" $ encode transferTokenParams

    Simulator.logString @(Builtin StarterContracts) "Lotto transfertoken contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "transfertoken" transferTokenParams
    Simulator.waitNSlots 2


    let stopBuyParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "stop Buy params : "
    Simulator.logString @(Builtin StarterContracts) $ show stopBuyParams
    --liftIO $ LB.writeFile "stopBuy.json" $ encode stopBuyParams

    Simulator.logString @(Builtin StarterContracts) "Lotto stopbuy contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "stopbuy" stopBuyParams
    Simulator.waitNSlots 2

    let closeParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "close params : "
    Simulator.logString @(Builtin StarterContracts) $ show closeParams
    --liftIO $ LB.writeFile "close.json" $ encode closeParams

    Simulator.logString @(Builtin StarterContracts) "Lotto close contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "close" closeParams
    Simulator.waitNSlots 2


    let drawParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "draw params : "
    Simulator.logString @(Builtin StarterContracts) $ show drawParams
    --liftIO $ LB.writeFile "close.json" $ encode drawParams

    Simulator.logString @(Builtin StarterContracts) "Lotto draw contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "draw" drawParams
    Simulator.waitNSlots 2

    
    let redeemParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "redeem params : "
    Simulator.logString @(Builtin StarterContracts) $ show redeemParams
    --liftIO $ LB.writeFile "redeem.json" $ encode redeemParams
    
    Simulator.logString @(Builtin StarterContracts) "Lotto redeem contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "redeem" redeemParams
    Simulator.waitNSlots 2

    {-
    -- invalid redemption scneario
    let redeemParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "redeem params : "
    Simulator.logString @(Builtin StarterContracts) $ show redeemParams
    liftIO $ LB.writeFile "redeem.json" $ encode redeemParams
    
    Simulator.logString @(Builtin StarterContracts) "Lotto redeem contract wallet 3 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid3 "redeem" redeemParams
    Simulator.waitNSlots 2
    -}

    -- calc payout
    let calcParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "calc params : "
    Simulator.logString @(Builtin StarterContracts) $ show calcParams
    --liftIO $ LB.writeFile "calc.json" $ encode calcParams

    Simulator.logString @(Builtin StarterContracts) "Lotto calc contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "calc" calcParams
    Simulator.waitNSlots 2

    -- winning player payout
    let payoutParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "payout params : "
    Simulator.logString @(Builtin StarterContracts) $ show payoutParams
    --liftIO $ LB.writeFile "payout.json" $ encode payoutParams

    Simulator.logString @(Builtin StarterContracts) "Lotto payout contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "payout" payoutParams
    Simulator.waitNSlots 2

    -- sponsor (beneficiary) payout
    let payParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "payout params : "
    Simulator.logString @(Builtin StarterContracts) $ show payParams

    Simulator.logString @(Builtin StarterContracts) "Lotto payout contract wallet 4 (lotto sponsor)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid4 "payout" payParams
    Simulator.waitNSlots 2
    

    let endParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "end params : "
    Simulator.logString @(Builtin StarterContracts) $ show endParams
    --liftIO $ LB.writeFile "calc.json" $ encode endParams

    Simulator.logString @(Builtin StarterContracts) "Lotto end contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "end" calcParams
    Simulator.waitNSlots 2

    {-
    -- collect admin fee scenario
    -- Note: only collect fees if more than the tx minimum amount of ada has been collected
    let collectParams = (lot)
    Simulator.logString @(Builtin StarterContracts) "collect params : "
    Simulator.logString @(Builtin StarterContracts) $ show collectParams
    
    Simulator.logString @(Builtin StarterContracts) "Lotto collect contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "collect" ()
    Simulator.waitNSlots 5
    -}


    {-
    -- reopen another lotter scenario
    Simulator.logString @(Builtin StarterContracts) "Lotto open contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "open" openParams
    Simulator.waitNSlots 2


    Simulator.logString @(Builtin StarterContracts) "Lotto startbuy contract wallet 1 (lotto admin)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid1 "startbuy" startBuyParams
    Simulator.waitNSlots 2

    Simulator.logString @(Builtin StarterContracts) "buy params : "
    Simulator.logString @(Builtin StarterContracts) $ show buyParams0
    liftIO $ LB.writeFile "buy.json" $ encode buyParams0

    Simulator.logString @(Builtin StarterContracts) "Lotto buy 0-9 tickets contract wallet 2 (lotto player)"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance cid2 "buy" buyParams0
    Simulator.waitNSlots 2

    -}

    -- Pressing enter results in the balances being printed
    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    void $ liftIO getLine

    balances <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) balances

    shutdown

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers = Simulator.mkSimulatorHandlers def def $ interpret (contractHandler Builtin.handleBuiltin)


