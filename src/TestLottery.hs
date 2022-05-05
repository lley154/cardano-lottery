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
import           Data.Monoid                (Last (..))
import           Data.Default               (Default (def))
import           Ledger                     (Slot (..))           
import           Ledger.Ada                 as Ada()
import qualified Ledger.TimeSlot            as TimeSlot
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))
import           OffChain


slotCfg :: TimeSlot.SlotConfig
slotCfg = def

test :: IO ()
test = runEmulatorTraceIO $ myTrace 

myTrace :: EmulatorTrace ()
myTrace = do
    logInfo @String "starting lotto"
    
    -- lotto admin wallet    
    handle <- Emulator.activateContractWallet (knownWallet 1) initEndpoint

    let admin_pkh       = mockWalletPaymentPubKeyHash (knownWallet 1)
        sponsor_pkh     = mockWalletPaymentPubKeyHash (knownWallet 4)
        jackpot'        = 5000000  -- 10 Ada
        ticket'         = 20000     -- 0.02 Ada (base amount * 100 = 2 ADA)
        deadline'       = TimeSlot.slotToEndPOSIXTime slotCfg (Slot 5000)
        fees            = 2       -- % of ticket sales goes to fees
        benWeigth       = 50

        sp = StartParams
            { spDifficulty    = 1 
            , spAdmin       = admin_pkh
            , spSpon         = sponsor_pkh
            , spSponWeight   = benWeigth
            , spJackpot     = jackpot'
            , spTicket      = ticket'
            , spDeadline    = deadline'
            , spFees        = fees
            }
    
    Emulator.callEndpoint @"init" handle (sp)   
    void $ Emulator.waitNSlots 5

    Last state <- Emulator.observableState handle
    case state of
        Nothing -> Extras.logError @String "error finding lottery"
        Just lot -> do
            Extras.logInfo $ "found lottery " ++ show lot

            -- lotto admin wallet
            h1 <- Emulator.activateContractWallet (knownWallet 1) useEndpoint

            callEndpoint @"open" h1 (lot, sp)
            void $ Emulator.waitNSlots 5

            callEndpoint @"startbuy" h1 (lot)
            void $ Emulator.waitNSlots 5
            
            -- lotto player 1 wallet
            h2 <- Emulator.activateContractWallet (knownWallet 2) useEndpoint
            h3 <- Emulator.activateContractWallet (knownWallet 3) useEndpoint

            -- lotto sponsor
            h4 <- Emulator.activateContractWallet (knownWallet 4) useEndpoint
            
            callEndpoint @"buy" h2 (lot, 0::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 1::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 2::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 3::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 4::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 5::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 6::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 7::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 8::Integer)
            void $ Emulator.waitNSlots 2
            callEndpoint @"buy" h2 (lot, 9::Integer)
            void $ Emulator.waitNSlots 2
            
            callEndpoint @"buy" h3 (lot, 789::Integer)
            void $ Emulator.waitNSlots 5

            callEndpoint @"transfertoken" h1 (lot)
            void $ Emulator.waitNSlots 5

            callEndpoint @"stopbuy" h1 (lot)
            void $ Emulator.waitNSlots 5

            callEndpoint @"close" h1 (lot)
            void $ Emulator.waitNSlots 5

            callEndpoint @"draw" h1 (lot)
            void $ Emulator.waitNSlots 5

            callEndpoint @"redeem" h2 (lot)
            void $ Emulator.waitNSlots 5

            callEndpoint @"calc" h1 (lot)
            void $ Emulator.waitNSlots 5

            callEndpoint @"payout" h2 (lot) -- winning player
            void $ Emulator.waitNSlots 5

            callEndpoint @"payout" h4 (lot) -- lotto sponsor
            void $ Emulator.waitNSlots 5

            callEndpoint @"end" h1 (lot)
            void $ Emulator.waitNSlots 5

            --callEndpoint @"collect" h1 (lot)
            --void $ Emulator.waitNSlots 5

            --callEndpoint @"open" h1 (lot, sp)
            --void $ Emulator.waitNSlots 5

            --callEndpoint @"buy" h2 (lot, 123::Integer)
            --void $ Emulator.waitNSlots 5
  

                

           
            
            
            
            
            
            
            
            



