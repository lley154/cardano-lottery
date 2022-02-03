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


module TestBenchmark
    ( test
    , myTrace
    ) where


import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
--import           Data.Monoid                (Last (..))
--import           Data.Default               (Default (def))
--import           Ledger                     (Slot (..))
import           Ledger.Value()
import           Ledger.Ada                 as Ada()
--import qualified Ledger.TimeSlot            as TimeSlot
import           Benchmark
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String)




test :: IO ()
test = runEmulatorTraceIO $ myTrace 

myTrace :: EmulatorTrace ()
myTrace = do
    logInfo @String "starting benchmark"
    
    -- wallet 1   
    h1 <- Emulator.activateContractWallet (knownWallet 1) endpoints
    
    -- wallet 2
    h2 <- Emulator.activateContractWallet (knownWallet 2) endpoints

    Emulator.callEndpoint @"give" h1 5000000  
    void $ Emulator.waitNSlots 5
    
    Emulator.callEndpoint @"grab" h2 ()
    void $ Emulator.waitNSlots 5
    
    Emulator.callEndpoint @"give" h1 5000000  
    void $ Emulator.waitNSlots 5

    Emulator.callEndpoint @"grab" h2 ()
    void $ Emulator.waitNSlots 5

    Emulator.callEndpoint @"give" h1 5000000  
    void $ Emulator.waitNSlots 5

    Emulator.callEndpoint @"grab" h2 ()
    void $ Emulator.waitNSlots 5

    Emulator.callEndpoint @"give" h1 5000000  
    void $ Emulator.waitNSlots 5

    Emulator.callEndpoint @"grab" h2 ()
    void $ Emulator.waitNSlots 5

    Emulator.callEndpoint @"give" h1 5000000  
    void $ Emulator.waitNSlots 5

    Emulator.callEndpoint @"grab" h2 ()
    void $ Emulator.waitNSlots 5

    
  
    
    
            
            
            
            
            
            
            
            



