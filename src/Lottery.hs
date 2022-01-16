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
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase #-}

module Lottery
    ( Lottery (..)
    , LottoRedeemer (..)
    , LottoDatum (..)
    , LottoInitSchema
    , LottoUseSchema
    , StartParams (..)
    , LottoSMError (..)
    , initEndpoint
    , useEndpoints
    ) where

import           Control.Lens                 (makeClassyPrisms)
import           Control.Monad                (forever, void)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.ByteString              as BS (ByteString, append)
import           Data.ByteString.Char8        as C8 (pack)
import           Data.Text                    (Text, pack)
import qualified Data.Map                     as Map
import           Data.Monoid                  (Last (..))
import           Data.OpenApi.Schema qualified as OpenApi
import           GHC.Generics                 (Generic)
import           Ledger                       (POSIXTime, PubKeyHash, TxOutRef, ScriptContext, TxInfo, CurrencySymbol, 
                                               Validator, Address, scriptContextTxInfo, txInInfoOutRef, txInfoInputs, 
                                               txInfoMint, ownCurrencySymbol, mkMintingPolicyScript, scriptCurrencySymbol,
                                               scriptAddress, pubKeyHashAddress)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           (TxConstraints)
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.Interval              as Interval
import           Ledger.Scripts               (MintingPolicyHash)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import           Ledger.Value                 (TokenName, Value, flattenValue)
import qualified Ledger.Value                 as Value
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine (OnChainState (..), SMContractError, State (..),
                                               StateMachine, StateMachineClient (..), Void, ThreadToken)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import qualified PlutusTx.AssocMap            as AssocMap
--import           PlutusTx.Prelude
import           PlutusTx.Prelude      hiding (Monoid (..), Semigroup (..))
import           Prelude                      (Semigroup (..))
import qualified Prelude                      as Haskell

newtype Lottery = Lottery
    { lToken          :: (Maybe ThreadToken)
    } deriving stock    (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)
      deriving newtype  (Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Lottery

data StartParams = StartParams
    { spAdmin          :: !PubKeyHash
    , spBenAddress     :: !PubKeyHash
    , spDeadline       :: !POSIXTime
    , spTicket         :: !Integer
    , spJackpot        :: !Integer
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)
    
PlutusTx.unstableMakeIsData ''StartParams

data LottoRedeemer = 
       Init 
     | Start StartParams BuiltinByteString
     | Buy BuiltinByteString PubKeyHash
     | Close BuiltinByteString 
     | Redeem PubKeyHash 
     | CollectFees
     | CalcPayout
     | Payout PubKeyHash

    deriving Haskell.Show
      
PlutusTx.unstableMakeIsData ''LottoRedeemer

    
data LottoDatum = LottoDatum
    { admin         :: PubKeyHash
    , deadline      :: POSIXTime
    , cost          :: Integer
    , winNum        :: BuiltinByteString
    , winners       :: [(PubKeyHash, BuiltinByteString)]
    , mph           :: MintingPolicyHash
    , jackpot       :: Integer
    , seqNum        :: Integer
    , treasury      :: Integer
    , adminFees     :: Integer
    , beneficiaries :: AssocMap.Map PubKeyHash Integer
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.unstableMakeIsData ''LottoDatum

--{-# INLINABLE lottoNumbers #-}
--lottoNumbers :: BuiltinByteString
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
    checkMintedAmount = case flattenValue (txInfoMint info) of
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
lottoValue :: MintingPolicyHash -> BuiltinByteString -> Value.Value
lottoValue mph' tn' = Value.singleton (Value.mpsSymbol mph') lottoToken 1
    where 
        lottoToken::TokenName
        lottoToken = Value.tokenName(fromBuiltin(tn'))
    
    
{-# INLINABLE ownsLottoToken #-}
ownsLottoToken :: MintingPolicyHash -> BuiltinByteString -> TxConstraints Void Void
ownsLottoToken mph'' tn'' = Constraints.mustSpendAtLeast (lottoValue mph'' tn'')


{-# INLINABLE calcPayouts #-}
calcPayouts :: Integer -> [(PubKeyHash, BuiltinByteString)] -> AssocMap.Map PubKeyHash Integer
calcPayouts j' w' = finalPayout
    where         
        pkhOnly :: [(PubKeyHash, BuiltinByteString)] -> [PubKeyHash]
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
lottoStateMachine lot = SM.mkStateMachine (lToken lot) (transition lot) PlutusTx.Prelude.isNothing

{-# INLINABLE mkLottoValidator #-}
mkLottoValidator :: Lottery -> Maybe LottoDatum -> LottoRedeemer -> ScriptContext -> Bool
mkLottoValidator = SM.mkValidator . lottoStateMachine

type LT = StateMachine (Maybe LottoDatum) LottoRedeemer

lottoTypedValidator :: Lottery -> Scripts.TypedValidator LT
lottoTypedValidator lot = Scripts.mkTypedValidator @LT
    ($$(PlutusTx.compile [|| mkLottoValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode lot)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe LottoDatum) @LottoRedeemer

lottoValidator :: Lottery -> Validator
lottoValidator = Scripts.validatorScript . lottoTypedValidator

lottoAddress :: Lottery -> Address
lottoAddress = scriptAddress . lottoValidator

lottoClient :: Lottery -> StateMachineClient (Maybe LottoDatum) LottoRedeemer
lottoClient lot = SM.mkStateMachineClient $ SM.StateMachineInstance (lottoStateMachine lot) (lottoTypedValidator lot)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ Data.Text.pack . Haskell.show

    
data LottoSMError =
    StateMachineContractError SM.SMContractError -- ^ State machine operation failed
    | LottoContractError ContractError -- ^ Endpoint, coin selection, etc. failed
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''LottoSMError

instance AsContractError LottoSMError where
    _ContractError = _LottoContractError . _ContractError

instance SM.AsSMContractError LottoSMError where
    _SMContractError = _StateMachineContractError . SM._SMContractError
    

strToBS :: Haskell.String -> BS.ByteString
strToBS = C8.pack    

initLotto :: StartParams -> Bool -> Contract (Last Lottery) s Text ()
initLotto sp useTT = do

    tt  <- if useTT then Just <$> mapErrorSM SM.getThreadToken else return Nothing
    
    let lottery = Lottery
            { lToken          = tt
            }
            
        v = lovelaceValueOf (spJackpot sp)
        mph' = Scripts.forwardingMintingPolicyHash (lottoTypedValidator lottery)
        ticketNum = toBuiltin(strToBS("0")) -- intialize winning ticket to 0    
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
        
    void $ mapErrorSM $ SM.runInitialise client (Just lDatum) v
    tell $ Last $ Just lottery
    logInfo $ "lotto has been intialized " ++ Haskell.show lottery


startLotto :: Lottery -> StartParams -> Contract w s LottoSMError ()
startLotto lot sp' = do

    let lClient = lottoClient lot
    
    l <- mapError StateMachineContractError $ SM.getOnChainState lClient
    
    case l of
        Nothing       -> logInfo $ "No lotto state machine found"  ++ Haskell.show lot
        Just (ocs, _) -> case tyTxOutData (ocsTxOut ocs) of

            Just(LottoDatum _ _ _ _ _ _ _ s' _ _ _) -> do
                logInfo @Haskell.String "setting lotto sequence to start of winning ticket number "
                let tn' = toBuiltin(strToBS((Haskell.show s') ++ "-")) -- intialize winning ticket with lotto seq
                
                void $ mapError StateMachineContractError $ SM.runStep (lottoClient lot) $ Start sp' tn'
                logInfo $ "datum: " ++ Haskell.show (tyTxOutData (ocsTxOut ocs))
                logInfo $ "lotto has started " ++ Haskell.show lot
                
            _ -> logInfo @Haskell.String "no lotto datum found"
                
    
buyTicket :: Lottery -> Integer -> Contract w s LottoSMError ()
buyTicket lot num = do
       
    pkh' <- Contract.ownPubKeyHash
    let lClient = lottoClient lot
    l <- mapError StateMachineContractError $ SM.getOnChainState lClient
        
    case l of
        Nothing       -> logInfo $ "No lotto state machine found"  ++ Haskell.show lot
        Just (ocs, _) -> case tyTxOutData (ocsTxOut ocs) of

            Just(LottoDatum _ _ _ n' _ _ _ _ _ _ _) -> do
                logInfo @Haskell.String "appending lotto sequence to lotto ticket: "
     
                -- TODO num > 0
                let ticketNum  = strToBS(Haskell.show num)
                    n'' = fromBuiltin(n')
                    ticketNum' = toBuiltin(BS.append n'' ticketNum)
    
                void $ mapError StateMachineContractError $ SM.runStep (lottoClient lot) $ Buy ticketNum' pkh'
                logInfo $ "datum: " ++ Haskell.show (tyTxOutData (ocsTxOut ocs))
                utxos <- utxosAt (pubKeyHashAddress pkh')
                logInfo $ "utxos: " ++ Haskell.show utxos
                
                case Map.keys utxos of
                    []       -> Contract.logError @Haskell.String "no utxo found"
                    oref : _ -> do
                        let ticketNum'' = Value.tokenName(strToBS(Haskell.show num)) 
                            val       = Value.singleton (curSymbol oref ticketNum'') ticketNum'' 1
                            mint'   = Constraints.mintingPolicy (policy oref ticketNum'') 
                        logInfo $ "value: " ++ Haskell.show val
                        logInfo $ "lookups: " ++ Haskell.show mint'
                
            
            _ -> logInfo @Haskell.String "no lotto datum found"
            

closeLotto :: Lottery -> Integer -> Contract w s LottoSMError ()
closeLotto lot num = do
        
    let lClient = lottoClient lot
    
    l <- mapError StateMachineContractError $ SM.getOnChainState lClient
    
    case l of
        Nothing       -> logInfo $ "No lotto state machine found"  ++ Haskell.show lot
        Just (ocs, _) -> case tyTxOutData (ocsTxOut ocs) of

            Just(LottoDatum _ _ _ n' _ _ _ _ _ _ _) -> do
                logInfo @Haskell.String "appending lotto sequence to lotto ticket: "
     
                -- TODO num > 0
                let ticketNum  = strToBS(Haskell.show num)
                    n'' = fromBuiltin(n')
                    ticketNum' = toBuiltin(BS.append n'' ticketNum)
    
                void $ mapError StateMachineContractError $ SM.runStep (lottoClient lot) $ Close ticketNum'
                logInfo $ "datum: " ++ Haskell.show (tyTxOutData (ocsTxOut ocs))
            _ -> logInfo @Haskell.String "no lotto datum found"
    
    pkh' <- Contract.ownPubKeyHash
    utxos <- utxosAt (pubKeyHashAddress pkh')
    logInfo $ "utxos: " ++ Haskell.show utxos

redeemLotto :: Lottery -> Contract w s LottoSMError ()
redeemLotto lot = do 
    
     pkh' <- Contract.ownPubKeyHash
     void $ mapError StateMachineContractError $ SM.runStep (lottoClient lot) $ Redeem pkh'     
     utxos <- utxosAt (pubKeyHashAddress pkh')
     logInfo $ "utxos: " ++ Haskell.show utxos
     

collectFees :: Lottery -> Contract w s LottoSMError ()
collectFees lot = do
    
    void $ mapError StateMachineContractError $ SM.runStep (lottoClient lot) $ CollectFees
    pkh' <- Contract.ownPubKeyHash
    utxos <- utxosAt (pubKeyHashAddress pkh')
    logInfo $ "utxos: " ++ Haskell.show utxos

    
calcPayoutLotto :: Lottery -> Contract w s LottoSMError ()
calcPayoutLotto lot = do
    
    void $ mapError StateMachineContractError $ SM.runStep (lottoClient lot) $ CalcPayout   
    
     
payoutLotto :: Lottery -> Contract w s LottoSMError ()
payoutLotto lot = do
    
    pkh' <- Contract.ownPubKeyHash 
    void $ mapError StateMachineContractError $ SM.runStep (lottoClient lot) $ Payout pkh'    
    utxos <- utxosAt (pubKeyHashAddress pkh')
    logInfo $ "utxos: " ++ Haskell.show utxos
    

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
