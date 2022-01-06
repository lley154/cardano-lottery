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


import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Data.Monoid                (Last (..))
import           Ledger
import           Ledger.Value()
import           Ledger.Ada                 as Ada()
import           Ledger.TimeSlot
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))
import           Lottery

test :: IO ()
test = runEmulatorTraceIO $ myTrace 

myTrace :: EmulatorTrace ()
myTrace = do
    logInfo @String "starting lotto"
    
    -- lotto admin wallet    
    h <- activateContractWallet (Wallet 1) initEndpoint
    
    let pkh      = pubKeyHash $ walletPubKey $ Wallet 1
        jackpot'   = 10000000
        ticket'    = 2000000
        deadline'  = slotToEndPOSIXTime def 50

        sp = StartParams
                { spAdmin          = pkh
                , spJackpot        = jackpot'
                , spTicket         = ticket'
                , spDeadline       = deadline'
                }
        useTT = True

    callEndpoint @"init" h (sp, useTT)    
    void $ Emulator.waitNSlots 5
                 
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String "error finding lottery"
        Just lot -> do
            Extras.logInfo $ "found lottery " ++ show lot

            -- lotto admin wallet
            h1 <- activateContractWallet (Wallet 1) $ useEndpoints lot
            
            -- lotto player wallet
            h2 <- activateContractWallet (Wallet 2) $ useEndpoints lot
            
            -- lotto player wallet
            h3 <- activateContractWallet (Wallet 3) $ useEndpoints lot

            -- start lotto 
            callEndpoint @"start" h1 sp
            void $ Emulator.waitNSlots 5
            
            -- lotto play to buy lotto ticket with number 123
            callEndpoint @"buy" h2 123
            void $ Emulator.waitNSlots 5
            
            -- callEndpoint @"buy" h2 456
            -- void $ Emulator.waitNSlots 5
            
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
            
            -- claim jackpot 
            callEndpoint @"payout" h2 () 
            void $ Emulator.waitNSlots 5
            
            {-
            -- **** start the next lotto ****
            callEndpoint @"start" h1 sp
            void $ Emulator.waitNSlots 5
            
            -- lotto play to buy lotto ticket with number 123
            callEndpoint @"buy" h2 123
            void $ Emulator.waitNSlots 5
            
            -- callEndpoint @"buy" h2 456
            -- void $ Emulator.waitNSlots 5
            
            callEndpoint @"buy" h3 789
            void $ Emulator.waitNSlots 5
            
            -- try to close but not lotto admin            
            --callEndpoint @"close" h2 123
            --void $ Emulator.waitNSlots 5
            
            -- lotto admin to close lotto with number 123
            callEndpoint @"close" h1 789
            void $ Emulator.waitNSlots 5
         
            -- lotto player to redeem ticket with winning number 123
            --callEndpoint @"redeem" h2 ()
            --void $ Emulator.waitNSlots 5
            
            -- lotto player try to redeem but does not have a winning ticket
            callEndpoint @"redeem" h3 () 
            void $ Emulator.waitNSlots 5
            
            -- claim jackpot 
            callEndpoint @"payout" h3 () 
            void $ Emulator.waitNSlots 5
            -}
            
            
            
            
            



