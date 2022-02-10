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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}


module TestLottery
    ( test
    , myTrace
    ) where


import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.ByteString            as BS (ByteString)
import           Data.ByteString.Char8      as C8 (pack)
import           Data.Monoid                (Last (..))
import           Data.Default               (Default (def))
import           Ledger                     (Slot (..))
import           Ledger.Value               
import           Ledger.Ada                 as Ada()
import qualified Ledger.TimeSlot            as TimeSlot
import           Lottery
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))



slotCfg :: TimeSlot.SlotConfig
slotCfg = def

test :: IO ()
test = runEmulatorTraceIO $ myTrace 

strToBS :: String -> BS.ByteString
strToBS = C8.pack 

lottoToken::String -> TokenName
lottoToken tn = tokenName $ strToBS tn


myTrace :: EmulatorTrace ()
myTrace = do
    logInfo @String "starting lotto"
    
    -- lotto player wallet    
    h <- Emulator.activateContractWallet (knownWallet 1) initEndpoint

    let sp = StartParams 
                {
                    spSeq = 5000000
                }
    
    Emulator.callEndpoint @"init" h ()   
    void $ Emulator.waitNSlots 5

    Last m <- Emulator.observableState h
    case m of
        Nothing -> Extras.logError @String "error finding lottery"
        Just lot -> do
            Extras.logInfo $ "found lottery " ++ show lot

            -- lotto player 1 wallet
            h1 <- Emulator.activateContractWallet (knownWallet 2) useEndpoint

            callEndpoint @"buy" h1 (lot, lottoToken "123")
            void $ Emulator.waitNSlots 5


            

                

           
            
            
            
            
            
            
            
            



