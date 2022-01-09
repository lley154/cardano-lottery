------------------------------------------------------------------------
-- Lotto.hs is a lottery built using plutus for the Cardano Blockchain
-- Author: Lawrence Ley, Bs.C. 
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
import qualified PlutusTx.AssocMap            as AssocMap
import           Prelude                      (String, Semigroup (..), Show (..))
import qualified Prelude

data Lottery = Lottery
    { lToken          :: !(Maybe ThreadToken)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Lottery

data StartParams = StartParams
    { spAdmin          :: !PubKeyHash
    , spBenAddress     :: !PubKeyHash
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
     | CollectFees
     | CalcPayout
     | Payout PubKeyHash

    deriving Show
      
PlutusTx.unstableMakeIsData ''LottoRedeemer

    
data LottoDatum = LottoDatum
    { admin         :: PubKeyHash
    , deadline      :: POSIXTime
    , cost          :: Integer
    , winNum        :: ByteString
    , winners       :: [(PubKeyHash, ByteString)]
    , mph           :: MintingPolicyHash
    , jackpot       :: Integer
    , seqNum        :: Integer
    , treasury      :: Integer
    , adminFees     :: Integer
    , beneficiaries :: AssocMap.Map PubKeyHash Integer
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


{-# INLINABLE calcPayouts #-}
calcPayouts :: Integer -> [(PubKeyHash, ByteString)] -> AssocMap.Map PubKeyHash Integer
calcPayouts j' w' = finalPayout
    where         
        pkhOnly :: [(PubKeyHash, ByteString)] -> [PubKeyHash]
        pkhOnly [] = []
        pkhOnly (x:xs) = fst x : pkhOnly xs
        
        pkhOnlyList  = pkhOnly w'
        halfPot = PlutusTx.Prelude.divide j' 2
        totalWinners = (length pkhOnlyList) - 1  -- exclude the sponsor address from the winner count
        jackpotSplit = PlutusTx.Prelude.divide halfPot totalWinners  -- half of jackpot goes to winners, other half goes to the sponsor
        
        count :: Eq a => a -> [a] -> Integer
        count x  = length . filter (==x)
        
        calcPayout :: [PubKeyHash] -> [(PubKeyHash, Integer)]
        calcPayout []     = []
        calcPayout (x:xs) = (x , (count x pkhOnlyList) * jackpotSplit) : calcPayout xs
        
        -- remove duplicate pkh's if any since they have already been included in the payout calc above
        removeDups :: [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
        removeDups []     = []
        removeDups (x:xs) = x : filter (/= x) (removeDups xs) 
        
        totalPayout :: AssocMap.Map PubKeyHash Integer
        totalPayout = AssocMap.fromList $ removeDups $ calcPayout pkhOnlyList
        
        -- update sponsor beneficiary with 50% of the jackpot
        finalPayout :: AssocMap.Map PubKeyHash Integer
        finalPayout = AssocMap.insert (pkhOnlyList!!0) halfPot totalPayout
      
      
{-# INLINABLE getPayout #-}
getPayout :: Maybe Integer -> Integer
getPayout p = case p of
    Nothing -> 0
    Just x -> x
    

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
-- f = lotto admin fees
-- b = beneficiaries

{-# INLINABLE transition #-}
transition :: Lottery -> State (Maybe LottoDatum) -> LottoRedeemer -> Maybe (TxConstraints Void Void, State (Maybe LottoDatum))
transition _ s' r = case (stateValue s', stateData s', r) of
    (_, Just(LottoDatum a d c n w m j s t f b), Init)          
        | j > 0                                              ->   Just ( Constraints.mustBeSignedBy (a)
                                                             , State (Just (LottoDatum a d c n w m j s t f b)) (lovelaceValueOf j)
                                                             )
    (v, Just(LottoDatum a _ _ _ _ m j s t f _), Start sp tn) 
        | (spJackpot sp > 0)                                 ->     let a' = spAdmin sp
                                                                        d' = spDeadline sp
                                                                        c' = spTicket sp
                                                                        j' = spJackpot sp
                                                                        b' = spBenAddress sp
                                                                    in Just ( Constraints.mustBeSignedBy (a)
                                                             , State (Just (LottoDatum a' d' c' tn [(b', tn)] m (j + j') (s + 1) t f (AssocMap.singleton b' 0))) $ v <> 
                                                             (lovelaceValueOf j')
                                                             )                                                         
    (v, Just (LottoDatum a d c n w m j s t f b), Buy tn pkh)  
        | n /= tn                                            ->  let constraints = Constraints.mustPayToPubKey pkh (lottoValue m tn)
                                                                             <> Constraints.mustValidateIn (Interval.to d)
                                                                             <> Constraints.mustMintValue (lottoValue m tn)  
                                                                 in Just ( constraints, State (Just (LottoDatum a d c n w m (j + 49 * c) s (t + 49 * c) (f + 2 * c) b)) $ v          <>
                                                             (lovelaceValueOf (c * 100))
                                                             )
    (v, Just(LottoDatum a d c n w m j s t f b), Close tn) 
        | n /= tn                                            ->   Just ( Constraints.mustBeSignedBy a
                                                             , State (Just (LottoDatum a d c tn w m j s t f b)) $ v
                                                             )
    (v, Just(LottoDatum a d c n w m j s t f b), Redeem pkh)  ->  let constraints = ownsLottoToken m n  
                                                                              <> Constraints.mustValidateIn (Interval.to d)   
                                                                 in Just ( constraints
                                                             , State (Just (LottoDatum a d c n (w ++ [(pkh, n)]) m j s t f b)) $ v
                                                             )
    (v, Just(LottoDatum a d c n w m j s t f _), CalcPayout)  
        | length w > 1                                        ->  let b' = calcPayouts j w
                                                                  in Just (Constraints.mustBeSignedBy (a)
                                                             , State (Just (LottoDatum a d c n w m j s t f b')) $ v
                                                             )    
    (v, Just(LottoDatum a d c n w m j s t f b), Payout pkh)      
        | length w > 1 && AssocMap.member pkh b              -> let payout = getPayout(AssocMap.lookup pkh b) 
                                                                 in Just (Constraints.mustBeSignedBy pkh
                                                             , State (Just (LottoDatum a d c n w m (j - payout) s t f (AssocMap.delete pkh b))) $ v                                                                   <>
                                                             (lovelaceValueOf (negate payout))
                                                             )
    (v, Just(LottoDatum a d c n w m j s t f b), CollectFees) ->   Just (Constraints.mustBeSignedBy (a)
                                                             , State (Just (LottoDatum a d c n w m j s t 0 b)) $ v                                                                   <>
                                                             (lovelaceValueOf (negate f))
                                                             )    
    _                                                        ->   Nothing


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
    --pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
    
    let lottery = Lottery
            { lToken          = tt
            }
            
        v = lovelaceValueOf (spJackpot sp)
        mph' = Scripts.forwardingMintingPolicyHash (lottoTypedValidator lottery)
        ticketNum = strToBS("0") -- intialize winning ticket to 0    
        lDatum = LottoDatum 
              {  admin         = spAdmin sp
              ,  deadline      = spDeadline sp
              ,  cost          = spTicket sp
              ,  winNum        = ticketNum
              ,  winners       = [(spBenAddress sp, ticketNum)] -- the sponsor pkh is always the 1st element in list
              ,  mph           = mph'
              ,  jackpot       = spJackpot sp
              ,  seqNum        = 0 
              ,  treasury      = 0
              ,  adminFees     = 0
              ,  beneficiaries = AssocMap.singleton (spBenAddress sp) 0
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

            Just(LottoDatum _ _ _ _ _ _ _ s' _ _ _) -> do
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

            Just(LottoDatum _ _ _ n' _ _ _ _ _ _ _) -> do
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

            Just(LottoDatum _ _ _ n' _ _ _ _ _ _ _) -> do
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
     
     pk    <- Contract.ownPubKey
     utxos <- utxoAt (pubKeyAddress pk)
     logInfo $ "utxos: " ++ show utxos
     

collectFees :: Lottery -> Contract w s Text ()
collectFees lot = do
    
    void $ mapErrorSM $ runStep (lottoClient lot) $ CollectFees
    
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    logInfo $ "utxos: " ++ show utxos

    
calcPayoutLotto :: Lottery -> Contract w s Text ()
calcPayoutLotto lot = do
    
    void $ mapErrorSM $ runStep (lottoClient lot) $ CalcPayout   
    
     
payoutLotto :: Lottery -> Contract w s Text ()
payoutLotto lot = do
    
    pkh' <- pubKeyHash <$> Contract.ownPubKey    
    void $ mapErrorSM $ runStep (lottoClient lot) $ Payout pkh'
    
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    logInfo $ "utxos: " ++ show utxos
    

type LottoInitSchema =
        Endpoint "init"       (StartParams, Bool)
type LottoUseSchema =
        Endpoint "start"       StartParams
    .\/ Endpoint "buy"         Integer
    .\/ Endpoint "close"       Integer
    .\/ Endpoint "redeem"      ()
    .\/ Endpoint "collect"     ()
    .\/ Endpoint "calc_payout" ()
    .\/ Endpoint "payout"      ()

    

initEndpoint :: Contract (Last Lottery) LottoInitSchema Text ()
initEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"init" $ \(sp, useTT) -> initLotto sp useTT

useEndpoints :: Lottery -> Contract () LottoUseSchema Text ()
useEndpoints lot = forever $ handleError logError $ awaitPromise $ start `select` buy `select` close `select` redeem `select` collect `select` calc_payout `select` payout
  where
    start        = endpoint @"start"       $ startLotto lot
    buy          = endpoint @"buy"         $ buyTicket lot
    close        = endpoint @"close"       $ closeLotto lot
    redeem       = endpoint @"redeem"      $ const $ redeemLotto lot
    collect      = endpoint @"collect"     $ const $ collectFees lot
    calc_payout  = endpoint @"calc_payout" $ const $ calcPayoutLotto lot
    payout       = endpoint @"payout"      $ const $ payoutLotto lot
    
    
    
    
