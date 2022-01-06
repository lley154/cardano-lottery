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
    , LottoInitSchema
    , LottoUseSchema
    , StartParams (..)
    , initEndpoint
    , useEndpoints
    ) where

import           Control.Monad                hiding (fmap)
import qualified Data.Map                     as Map
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           Data.ByteString              as BS (ByteString, append)
import           Data.ByteString.Char8        as C8 (pack)
import           Data.Maybe                   as Maybe
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified Ledger.Constraints           as Constraints()
import qualified Ledger.Interval              as Interval
import           Ledger.Typed.Tx
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

data StartParams = StartParams
    { spAdmin          :: !PubKeyHash
    , spDeadline       :: !POSIXTime
    , spTicket         :: !Integer
    , spJackpot        :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)
    
PlutusTx.unstableMakeIsData ''StartParams


data LottoRedeemer = 
       Init 
     | Start StartParams ByteString
     | Buy ByteString PubKeyHash
     | Close ByteString 
     | Redeem PubKeyHash 
     | Payout 
    deriving Show
      
PlutusTx.unstableMakeIsData ''LottoRedeemer

    
data LottoDatum = LottoDatum
    { admin        :: PubKeyHash
    , deadline     :: POSIXTime
    , cost         :: Integer
    , winNum       :: ByteString
    , winner       :: PubKeyHash
    , mph          :: MintingPolicyHash
    , jackpot      :: Integer
    , seqNum       :: Integer
    , treasury     :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''LottoDatum

--{-# INLINABLE lottoNumbers #-}
--lottoNumbers :: ByteString
--lottoNumbers = "0123456789"


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
lottoValue :: MintingPolicyHash -> ByteString -> Value.Value
lottoValue mph' tn' = Value.singleton (Value.mpsSymbol mph') lottoToken 1
    where 
        lottoToken::TokenName
        lottoToken = tokenName(tn')
    
    
{-# INLINABLE ownsLottoToken #-}
ownsLottoToken :: MintingPolicyHash -> ByteString -> TxConstraints Void Void
ownsLottoToken mph'' tn'' = Constraints.mustSpendAtLeast (lottoValue mph'' tn'')


-- LottoDatum fields
-- a = admin  
-- d = deadline
-- c = ticket cost
-- n = the winning number
-- w = the winner(s)
-- m = mph of minting script
-- j = jackpot
-- s = lotto sequence number
-- t = lotto treasury

{-# INLINABLE transition #-}
transition :: Lottery -> State (Maybe LottoDatum) -> LottoRedeemer -> Maybe (TxConstraints Void Void, State (Maybe LottoDatum))
transition _ s' r = case (stateValue s', stateData s', r) of
    (_, Just(LottoDatum a d c n w m j s t), Init)          
        | j > 0                                           ->   Just ( Constraints.mustBeSignedBy (a)
                                                         , State (Just (LottoDatum a d c n w m j s t)) (lovelaceValueOf j)
                                                         )
    (v, Just(LottoDatum a _ _ _ w m j s t), Start sp n') 
        | (spJackpot sp > 0)                              ->     let a' = spAdmin sp
                                                                     d' = spDeadline sp
                                                                     c' = spTicket sp
                                                                     j' = spJackpot sp
                                                                 in Just ( Constraints.mustBeSignedBy (a)
                                                         , State (Just (LottoDatum a' d' c' n' w m (j + j') (s + 1) t)) $ v                 <> 
                                                             (lovelaceValueOf j')
                                                         )                                                         
    (v, Just (LottoDatum a d c n w m j s t), Buy tn pkh)  
        | n /= tn                                       ->  let constraints = Constraints.mustPayToPubKey pkh (lottoValue m tn)
                                                                             <> Constraints.mustValidateIn (Interval.to d)
                                                                             <> Constraints.mustMintValue (lottoValue m tn)  
                                                             in Just ( constraints, State (Just (LottoDatum a d t n w m (j + c) s t)) $ v   <>
                                                             (lovelaceValueOf c)
                                                         )
    (v, Just(LottoDatum a d c n w m j s t), Close tn) 
        | n /= tn                                       ->   Just ( Constraints.mustBeSignedBy a
                                                         , State (Just (LottoDatum a d c tn w m j s t)) $ v
                                                         )
    -- TODO add winner to wins map
    (v, Just(LottoDatum a d c n _ m j s t), Redeem pkh)     ->  let constraints = ownsLottoToken m n  
                                                                              <> Constraints.mustValidateIn (Interval.to d)   
                                                              in Just ( constraints
                                                         , State (Just (LottoDatum a d c n pkh m j s t)) $ v
                                                         )
    -- TODO payout logic for all winners                                                     
    (v, Just(LottoDatum a d c n w m j s t), Payout)         -> Just (Constraints.mustBeSignedBy (w)
                                                         , State (Just (LottoDatum a d c n w m 0 s t)) $ v                                 <>
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


initLotto :: StartParams -> Bool -> Contract (Last Lottery) s Text ()
initLotto sp useTT = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
    
    let lottery = Lottery
            { lToken          = tt
            }
            
        v = lovelaceValueOf (spJackpot sp)
        mph' = Scripts.forwardingMintingPolicyHash (lottoTypedValidator lottery)
        ticketNum = strToBS("0") -- intialize winning ticket to 0    
        lDatum = LottoDatum 
              {  admin      = spAdmin sp
              ,  deadline   = spDeadline sp
              ,  cost       = spTicket sp
              ,  winNum     = ticketNum
              ,  winner     = pkh
              ,  mph        = mph'
              ,  jackpot    = spJackpot sp
              ,  seqNum     = 0 
              ,  treasury   = 0
              }
        client = lottoClient lottery
        
    void $ mapErrorSM $ runInitialise client (Just lDatum) v
    tell $ Last $ Just lottery
    logInfo $ "lotto has been intialized " ++ show lottery


startLotto :: Lottery -> StartParams -> Contract w s Text ()
startLotto lot sp' = do

    let lClient = lottoClient lot
    
    l <- mapErrorSM $ getOnChainState lClient
    case l of
        Nothing             -> throwError "lottery not found"
        Just ((o, _), _) -> case tyTxOutData o of

            Just(LottoDatum _ _ _ _ _ _ _ s' _) -> do
                logInfo @String "setting lotto sequence to start of winning ticket number"
                let tn' = strToBS((show s') ++ "-") -- intialize winning ticket with lotto seq
                
                void $ mapErrorSM $ runStep (lottoClient lot) $ Start sp' tn'
                logInfo $ "lotto has started " ++ show lot
            _ -> logInfo @String "no lotto datum found"
   
    

strToBS :: String -> BS.ByteString
strToBS = C8.pack
    
buyTicket :: Lottery -> Integer -> Contract w s Text ()
buyTicket lot num = do
       
    let lClient = lottoClient lot
    
    l <- mapErrorSM $ getOnChainState lClient
    case l of
        Nothing             -> throwError "lottery not found"
        Just ((o, _), _) -> case tyTxOutData o of

            Just(LottoDatum _ _ _ n' _ _ _ _ _) -> do
                logInfo @String "appending lotto sequence to lotto ticket: "
     
                -- TODO num > 0
                pkh' <- pubKeyHash <$> Contract.ownPubKey
                let ticketNum  = strToBS(show num)
                    ticketNum' = BS.append n' ticketNum
    
                void $ mapErrorSM $ runStep (lottoClient lot) $ Buy ticketNum' pkh'
            _ -> logInfo @String "no lotto datum found"
            
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
  

closeLotto :: Lottery -> Integer -> Contract w s Text ()
closeLotto lot num = do
        
    let lClient = lottoClient lot
    
    l <- mapErrorSM $ getOnChainState lClient
    case l of
        Nothing             -> throwError "lottery not found"
        Just ((o, _), _) -> case tyTxOutData o of

            Just(LottoDatum _ _ _ n' _ _ _ _ _) -> do
                logInfo @String "appending lotto sequence to lotto ticket: "
     
                -- TODO num > 0
                let ticketNum  = strToBS(show num)
                    ticketNum' = BS.append n' ticketNum
    
                void $ mapErrorSM $ runStep (lottoClient lot) $ Close ticketNum'
            _ -> logInfo @String "no lotto datum found"


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
            

redeemLotto :: Lottery -> Contract w s Text ()
redeemLotto lot = do 
    
     pkh' <- pubKeyHash <$> Contract.ownPubKey      
     void $ mapErrorSM $ runStep (lottoClient lot) $ Redeem pkh'

payoutLotto :: Lottery -> Contract w s Text ()
payoutLotto lot = void $ mapErrorSM $ runStep (lottoClient lot) $ Payout

type LottoInitSchema =
        Endpoint "init"       (StartParams, Bool)
type LottoUseSchema =
        Endpoint "start"       StartParams
    .\/ Endpoint "buy"         Integer
    .\/ Endpoint "close"       Integer
    .\/ Endpoint "redeem"      ()
    .\/ Endpoint "payout"      ()
    

initEndpoint :: Contract (Last Lottery) LottoInitSchema Text ()
initEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"init" $ \(sp, useTT) -> initLotto sp useTT

useEndpoints :: Lottery -> Contract () LottoUseSchema Text ()
useEndpoints lot = forever $ handleError logError $ awaitPromise $ start `select` buy `select` close `select` redeem `select` payout
  where
    start     = endpoint @"start"      $ startLotto lot
    buy       = endpoint @"buy"        $ buyTicket lot
    close     = endpoint @"close"      $ closeLotto lot
    redeem    = endpoint @"redeem"     $ const $ redeemLotto lot
    payout    = endpoint @"payout"     $ const $ payoutLotto lot
    
    
    
    
