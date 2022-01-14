{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TestLottery
    ( test
    , myTrace
    ) where

--import           Control.Lens                (view)
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
--import           Data.Default               (Default (..))
import           Data.Monoid                (Last (..))
import           Data.Default               (Default (def))
--import           Ledger
import           Ledger.Value()
import           Ledger.Ada                 as Ada()
import qualified Ledger.TimeSlot            as TimeSlot
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
--import           Plutus.Trace               as Trace
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))
import           Lottery
--import           Wallet.Emulator            (Wallet (..), knownWallet, knownWallets, walletPubKeyHash)

{-
t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

theContract :: POSIXTime -> Contract () LottoSchema ContractError ()
theContract startTime = crowdfunding $ theCampaign startTime
-}

slotCfg :: TimeSlot.SlotConfig
slotCfg = def

test :: IO ()
test = runEmulatorTraceIO $ myTrace 

myTrace :: EmulatorTrace ()
myTrace = do
    logInfo @String "starting lotto"
    
    -- lotto admin wallet    
    h <- Emulator.activateContractWallet (knownWallet 1) initEndpoint
    --slotCfg <- Emulator.getSlotConfig
    --now <- view Emulator.currentSlot <$> Emulator.chainState
    
    let pkh      = walletPubKeyHash (knownWallet 1)
        jackpot'   = 10000000
        ticket'    = 20000
        --deadline'  = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 50
        deadline' = TimeSlot.scSlotZeroTime slotCfg + 50

        sp = StartParams
                { spAdmin          = pkh
                , spBenAddress     = pkh
                , spJackpot        = jackpot'
                , spTicket         = ticket'
                , spDeadline       = deadline'
                }
        useTT = True

    Emulator.callEndpoint @"init" h (sp, useTT)    
    void $ Emulator.waitNSlots 5
                 
    Last m <- Emulator.observableState h
    case m of
        Nothing -> Extras.logError @String "error finding lottery"
        Just lot -> do
            Extras.logInfo $ "found lottery " ++ show lot

            -- lotto admin wallet
            h1 <- Emulator.activateContractWallet (knownWallet 1) $ useEndpoints lot
            
            -- lotto player wallet
            h2 <- Emulator.activateContractWallet (knownWallet 2) $ useEndpoints lot
            
            -- lotto player wallet
            h3 <- Emulator.activateContractWallet (knownWallet 3) $ useEndpoints lot
            
            -- lotto sponsor wallet
            h4 <- Emulator.activateContractWallet (knownWallet 4) $ useEndpoints lot
            
            let sponsor_pkh = walletPubKeyHash (knownWallet 4)            
                sp' = StartParams
                          { spAdmin          = pkh
                          , spBenAddress     = sponsor_pkh
                          , spJackpot        = jackpot'
                          , spTicket         = ticket'
                          , spDeadline       = deadline'
                          }

            -- start lotto 
            callEndpoint @"start" h1 sp'
            void $ Emulator.waitNSlots 5
            let pkh      = walletPubKeyHash (knownWallet 1)

            Extras.logInfo $ "wallet 1 pkh " ++ show pkh
            Extras.logInfo $ "wallet 1 " ++ show (knownWallet 1)
            
            
            -- lotto play to buy lotto ticket with number 123
            callEndpoint @"buy" h2 123
            void $ Emulator.waitNSlots 5
            let pkh'      = walletPubKeyHash (knownWallet 2)

            Extras.logInfo $ "wallet 2 pkh " ++ show pkh'
            Extras.logInfo $ "wallet 2 " ++ show (knownWallet 2)
            
            
            {-
            
            
            callEndpoint @"buy" h3 789
            void $ Emulator.waitNSlots 5
            
            -- try to close but not lotto admin            
            -- callEndpoint @"close" h2 123
            -- void $ Emulator.waitNSlots 5
            
            -- lotto admin to close lotto with number 123
            callEndpoint @"close" h1 123
            void $ Emulator.waitNSlots 5
         
            -- lotto player to redeem ticket with winning number 123
            callEndpoint @"redeem" h2 ()
            void $ Emulator.waitNSlots 5
            
            -- lotto player try to redeem but does not have a winning ticket
            --callEndpoint @"redeem" h3 () 
            --void $ Emulator.waitNSlots 5
            
            
            -- calculate payout
            callEndpoint @"calc_payout" h1 () 
            void $ Emulator.waitNSlots 5
                        
            -- claim jackpot for lucky winner
            callEndpoint @"payout" h2 () 
            void $ Emulator.waitNSlots 5
            
            -- claim 50% of jackpot for the sponsor
            callEndpoint @"payout" h4 () 
            void $ Emulator.waitNSlots 5
            
            
            -- collect admin fees 
            callEndpoint @"collect" h1 ()
            void $ Emulator.waitNSlots 5

            {-
         
            
            -- ****************************** 
            -- **** start the next lotto ****
            callEndpoint @"start" h1 sp'
            void $ Emulator.waitNSlots 5
            
            -- lotto player to buy lotto ticket with number 123
            --callEndpoint @"buy" h2 123
            --void $ Emulator.waitNSlots 5
            
            -- lotto player to buy with ticket number 456
            callEndpoint @"buy" h3 456
            void $ Emulator.waitNSlots 5
            
            
            -- lotto admin to close lotto with number 456
            callEndpoint @"close" h1 456
            void $ Emulator.waitNSlots 5
         
         
            -- lotto player try to redeem and has winning ticket
            callEndpoint @"redeem" h3 () 
            void $ Emulator.waitNSlots 5
            

            -- calculate payout
            callEndpoint @"calc_payout" h1 () 
            void $ Emulator.waitNSlots 5
                                                
            -- claim jackpot 
            callEndpoint @"payout" h3 () 
            void $ Emulator.waitNSlots 5
            
             -- claim 50% of jackpot for sponsor 
            callEndpoint @"payout" h4 () 
            void $ Emulator.waitNSlots 5
            
             -- collect admin fees
            callEndpoint @"collect" h1 ()
            void $ Emulator.waitNSlots 5
                

           
            -}
            
            -}
            
            
            
            
            
            



