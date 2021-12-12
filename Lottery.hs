------------------------------------------------------------------------
-- Lotto.hs is a lottery built using plutus for the Cardano Blockchain
-- Author: L Ley 
-- First published date: Dec 8, 2021
-- Licenses: Apache-2.0 License
-- Credits: 1) This could not have been created without the amazing  
--          tutorials by Lars Brünjes for the plutus pioneer program
--          https://github.com/input-output-hk/plutus-pioneer-program
--          2) The plutus use cases, in particular https://github.com/input-output-hk/plutus/blob/f7466c86fe3afc593746e44257adbf7785f7cedb/plutus-use-cases/src/Plutus/Contracts/Governance.hs
-------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Lottery
    ( Lottery (..)
    , LottoRedeemer (..)
    , LottoDatum (..)
    , LottoStartSchema
    , LottoUseSchema
    , StartParams (..)
    , startEndpoint
    , useEndpoints
    ) where

import           Control.Monad                hiding (fmap)
import qualified Data.Map                     as Map
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           Data.ByteString              as BS (ByteString)
import           Data.ByteString.Char8        as C8 (pack)
import           Data.Maybe                   as Maybe
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified Ledger.Constraints           as Constraints()
import qualified Ledger.Interval              as Interval
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 as Value
--import qualified PlutusTx.AssocMap            as AssocMap
import           Prelude                      (String, Semigroup (..), Show (..))
import qualified Prelude

data Lottery = Lottery
    { lToken          :: !(Maybe ThreadToken)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Lottery

data LottoRedeemer = 
       Open 
     | Buy TokenName PubKeyHash
     | Close TokenName 
     | Redeem PubKeyHash 
     | Payout 
    deriving Show
      
PlutusTx.unstableMakeIsData ''LottoRedeemer
    
    
data LottoDatum = LottoDatum
    { admin        :: PubKeyHash
    , deadline     :: POSIXTime
    , ticket       :: Integer
    , winNum       :: TokenName
    , winner       :: PubKeyHash
    , mph          :: MintingPolicyHash
    , jackpot      :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''LottoDatum


{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = PlutusTx.Prelude.any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1 
        _                -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn


{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE lottoValue #-}
lottoValue :: MintingPolicyHash -> TokenName -> Value.Value
lottoValue mph' tokenName' =
    Value.singleton (Value.mpsSymbol mph') tokenName' 1
    
    
{-# INLINABLE ownsLottoToken #-}
ownsLottoToken :: MintingPolicyHash -> TokenName -> TxConstraints Void Void
ownsLottoToken mph'' tokenName'' = Constraints.mustSpendAtLeast (lottoValue mph'' tokenName'')

-- LottoDatum fields
-- a = admin  
-- d = deadline
-- t = ticket
-- n = winning number
-- w = the winner
-- m = mph of minting script
-- j = jackpot

{-# INLINABLE transition #-}
transition :: Lottery -> State (Maybe LottoDatum) -> LottoRedeemer -> Maybe (TxConstraints Void Void, State (Maybe LottoDatum))
transition lot s r = case (stateValue s, stateData s, r) of
    (_, Just(LottoDatum a d t n w m j), Open)           ->   Just ( Constraints.mustBeSignedBy (a)
                                                         , State (Just (LottoDatum a d t n w m j)) (lovelaceValueOf j)
                                                         )
    (v, Just (LottoDatum a d t n w m j), Buy tn pkh)  
        | n /= tn                                       ->  let constraints = Constraints.mustPayToPubKey pkh (lottoValue m tn)
                                                                             <> Constraints.mustValidateIn (Interval.to d)
                                                                             <> Constraints.mustMintValue (lottoValue m tn)  
                                                             in Just ( constraints, State (Just (LottoDatum a d t n w m (j + t))) $ v   <>
                                                             (lovelaceValueOf t)
                                                         )
    (v, Just(LottoDatum a d t n w m j), Close tn) 
        | n /= tn                                       ->   Just ( Constraints.mustBeSignedBy a
                                                         , State (Just (LottoDatum a d t tn w m j)) $ v
                                                         )
    -- TODO add winner to wins map
    (v, Just(LottoDatum a d t n _ m j), Redeem pkh)     ->  let constraints = ownsLottoToken m n  
                                                                              <> Constraints.mustValidateIn (Interval.to d)   
                                                              in Just ( constraints
                                                         , State (Just (LottoDatum a d t n pkh m j)) $ v
                                                         )
    -- TODO payout logic for all winners                                                     
    (v, Just(LottoDatum a d t n w m j), Payout)         -> Just (Constraints.mustBeSignedBy (w)
                                                         , State (Just (LottoDatum a d t n w m j)) $ v                                  <>
                                                         (lovelaceValueOf (negate j))
                                                         )
    _                                                   ->   Nothing


{-# INLINABLE lottoStateMachine #-}
lottoStateMachine :: Lottery -> StateMachine (Maybe LottoDatum) LottoRedeemer
lottoStateMachine lot = mkStateMachine (lToken lot) (transition lot) PlutusTx.Prelude.isNothing

{-# INLINABLE mkLottoValidator #-}
mkLottoValidator :: Lottery -> Maybe LottoDatum -> LottoRedeemer -> ScriptContext -> Bool
mkLottoValidator = mkValidator . lottoStateMachine

type LT = StateMachine (Maybe LottoDatum) LottoRedeemer

lottoTypedValidator :: Lottery -> Scripts.TypedValidator LT
lottoTypedValidator lot = Scripts.mkTypedValidator @LT
    ($$(PlutusTx.compile [|| mkLottoValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode lot)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe LottoDatum) @LottoRedeemer

lottoValidator :: Lottery -> Validator
lottoValidator = Scripts.validatorScript . lottoTypedValidator

lottoAddress :: Lottery -> Ledger.Address
lottoAddress = scriptAddress . lottoValidator

lottoClient :: Lottery -> StateMachineClient (Maybe LottoDatum) LottoRedeemer
lottoClient lot = mkStateMachineClient $ StateMachineInstance (lottoStateMachine lot) (lottoTypedValidator lot)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ Data.Text.pack . show

data StartParams = StartParams
    { spAdmin          :: !PubKeyHash
    , spJackpot        :: !Integer
    , spTicket         :: !Integer
    , spDeadline       :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON)

startLotto :: StartParams -> Bool -> Contract (Last Lottery) s Text ()
startLotto sp useTT = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
    
    let lottery = Lottery
            { lToken          = tt
            }
            
        v = lovelaceValueOf (spJackpot sp)
        mph' = Scripts.forwardingMintingPolicyHash (lottoTypedValidator lottery)
        ticketNum = tokenName(strToBS("0")) -- intialize winning ticket to 0    
        lDatum = LottoDatum 
              {  admin      = spAdmin sp
              ,  deadline   = spDeadline sp
              ,  ticket     = spTicket sp
              ,  winNum     = ticketNum
              ,  winner     = pkh
              ,  mph        = mph'
              ,  jackpot    = spJackpot sp 
              }
        client = lottoClient lottery
        
    void $ mapErrorSM $ runInitialise client (Just lDatum) v
    tell $ Last $ Just lottery
    logInfo $ "lotto has started " ++ show lottery

strToBS :: String -> BS.ByteString
strToBS = C8.pack
    
buyTicket :: Lottery -> Integer -> Contract w s Text ()
buyTicket lot num = do
       
    pkh' <- pubKeyHash <$> Contract.ownPubKey
    let ticketNum = tokenName(strToBS (show num)) 
    
    void $ mapErrorSM $ runStep (lottoClient lot) $ Buy ticketNum pkh'
  

closeLotto :: Lottery -> Integer -> Contract w s Text ()
closeLotto lot num = do
        
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let ticketNum = tokenName(strToBS(show num)) 
                val       = Value.singleton (curSymbol oref ticketNum) ticketNum 1
                lookups   = Constraints.mintingPolicy (policy oref ticketNum) <> Constraints.unspentOutputs utxos
            logInfo $ "value: " ++ show val
            logInfo $ "lookups: " ++ show lookups

    let ticketNum = tokenName(strToBS (show num)) 
    void $ mapErrorSM $ runStep (lottoClient lot) $ Close ticketNum


redeemLotto :: Lottery -> Contract w s Text ()
redeemLotto lot = do 
    
     pkh' <- pubKeyHash <$> Contract.ownPubKey      
     void $ mapErrorSM $ runStep (lottoClient lot) $ Redeem pkh'

payoutLotto :: Lottery -> Contract w s Text ()
payoutLotto lot = void $ mapErrorSM $ runStep (lottoClient lot) $ Payout

type LottoStartSchema =
        Endpoint "start"       (StartParams, Bool)
type LottoUseSchema =
        Endpoint "buy"         Integer
    .\/ Endpoint "close"       Integer
    .\/ Endpoint "redeem"      ()
    .\/ Endpoint "payout"      ()
    

startEndpoint :: Contract (Last Lottery) LottoStartSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ \(sp, useTT) -> startLotto sp useTT

useEndpoints :: Lottery -> Contract () LottoUseSchema Text ()
useEndpoints lot = forever $ handleError logError $ awaitPromise $ buy `select` close `select` redeem `select` payout
  where
    buy       = endpoint @"buy"        $ buyTicket lot
    close     = endpoint @"close"      $ closeLotto lot
    redeem    = endpoint @"redeem"     $ const $ redeemLotto lot
    payout    = endpoint @"payout"     $ const $ payoutLotto lot
    
    
    
