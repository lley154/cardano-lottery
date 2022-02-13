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
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Lottery
    (   Lottery (..)
    ,   LottoRedeemer (..)
    ,   LottoDatum (..)
    ,   LottoSchema
    ,   StartParams (..)
    ,   initEndpoint
    ,   useEndpoint
    ) where

import Control.Lens (review)
import Control.Monad (forever)
--import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
--import Data.ByteString as BS (ByteString)
--import Data.ByteString.Char8 as C8 (pack)
--import Data.Set qualified as Set
import Data.Map (keys, toList, singleton)
import Data.Monoid (Last (..))
import Data.Text qualified as T (pack, Text)
--import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Address, CurrencySymbol, Datum(..), mkMintingPolicyScript, MintingPolicyHash, scriptCurrencySymbol, TokenName, PaymentPubKeyHash (unPaymentPubKeyHash), POSIXTime, pubKeyHashAddress, Redeemer(..), ScriptContext (ScriptContext, scriptContextTxInfo),
               scriptHashAddress, TxOutRef, TxInfo, txInInfoOutRef, txInfoInputs, txInfoMint, txOutDatum, Value, Validator, ValidatorHash(..))
import Ledger.Ada qualified as Ada (fromValue, lovelaceValueOf, getLovelace)
import Ledger.Constraints qualified as Constraints (adjustUnbalancedTx, mintingPolicy, mustBeSignedBy, mustSpendScriptOutput, mustMintValue, mustMintValueWithRedeemer,  mustPayToTheScript, mustSpendPubKeyOutput, otherScript, typedValidatorLookups, unspentOutputs)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Tx (ChainIndexTxOut(..))
import Ledger.Value qualified as Value (flattenValue, mpsSymbol, singleton, tokenName, Value, valueOf)
import Ledger.Contexts as Contexts (ownCurrencySymbol)
import Playground.Contract as Playground (ToSchema)
import Plutus.Contract (AsContractError (_ConstraintResolutionError), awaitTxConfirmed, awaitPromise,  Contract, Endpoint, handleError, mapError,  endpoint, logInfo, logError, select,
                        tell, throwError, type (.\/))
import Plutus.Contract.Request (mkTxContract, ownPaymentPubKeyHash, submitTxConfirmed, utxosAt)
import Plutus.Contract.StateMachine as SM (getThreadToken, SMContractError, ttOutRef)
import Plutus.Contract.StateMachine.ThreadToken qualified as TT (curPolicy, threadTokenValue, ThreadToken(..), ttCurrencySymbol)
import Plutus.Contract.StateMachine.MintingPolarity as MP (MintingPolarity(Mint))
import PlutusTx qualified
import PlutusTx.Prelude (any, Bool (True), const, foldMap, fromBuiltin, fst, snd, Integer, Maybe (Just), Maybe (Nothing), maybe, mempty, ($), (&&), (-), (.), (>=), (==), (++), (>>=), traceIfFalse, unless)
import Prelude (Semigroup (..))
import Prelude qualified as Haskell 
import qualified PlutusTx ()


newtype Lottery = Lottery
    { ttLotery          :: TT.ThreadToken
    } deriving stock    (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON, Playground.ToSchema)
      deriving newtype  (Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Lottery

data LottoRedeemer = 
       Open 
     | Buy TokenName

    deriving Haskell.Show
      
PlutusTx.unstableMakeIsData ''LottoRedeemer
PlutusTx.makeLift ''LottoRedeemer

data LottoDatum = LottoDatum
    {   adminPkh    :: PaymentPubKeyHash
    ,   seqNum      :: Integer
    ,   mph         :: MintingPolicyHash
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.unstableMakeIsData ''LottoDatum
--PlutusTx.makeIsDataIndexed ''LottoDatum
PlutusTx.makeLift ''LottoDatum

data LotteryType
instance Scripts.ValidatorTypes LotteryType where
    type instance RedeemerType LotteryType = LottoRedeemer
    type instance DatumType LotteryType = LottoDatum


{-# INLINABLE mkLottoValidator #-}
mkLottoValidator :: LottoDatum -> LottoRedeemer -> ScriptContext -> Bool
mkLottoValidator _ _ _ = True



typedLottoValidator :: Scripts.TypedValidator LotteryType
typedLottoValidator = Scripts.mkTypedValidator @LotteryType
    $$(PlutusTx.compile [|| mkLottoValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @LottoDatum @LottoRedeemer


lottoValidator :: Ledger.Validator
lottoValidator = Scripts.validatorScript typedLottoValidator

lottoHash :: Ledger.ValidatorHash
lottoHash = Scripts.validatorHash typedLottoValidator

lottoAddress :: Ledger.Address
lottoAddress = scriptHashAddress lottoHash


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
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(cs, tn', amt)] -> cs  == Contexts.ownCurrencySymbol ctx && tn' == tn && amt == 1 
        _                -> Haskell.False

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

{-# INLINABLE lottoTicketMphValue #-}
lottoTicketMphValue :: MintingPolicyHash -> TokenName -> Value.Value
lottoTicketMphValue mph' tn' = Value.singleton (Value.mpsSymbol mph') tn' 1

{-# INLINABLE lottoTicketValue #-}
lottoTicketValue :: CurrencySymbol -> TokenName -> Value.Value
lottoTicketValue cs' tn' = Value.singleton cs' tn' 1


threadTokenValueInner :: Maybe TT.ThreadToken -> ValidatorHash -> Value
threadTokenValueInner = maybe (const mempty) (TT.threadTokenValue . TT.ttCurrencySymbol)

ttTokenName :: ValidatorHash -> TokenName
ttTokenName (ValidatorHash bsHash) = Value.tokenName $ fromBuiltin bsHash


data StartParams = StartParams
    { 
        spSeq       :: Integer
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)
    
PlutusTx.unstableMakeIsData ''StartParams
--PlutusTx.makeIsDataIndexed ''StartParams

--strToBS :: Haskell.String -> BS.ByteString
--strToBS = C8.pack

--lottoToken::Haskell.String -> TokenName
--lottoToken tn = Value.tokenName $ strToBS tn

mapErrorSM :: Contract w s SM.SMContractError a -> Contract w s T.Text a
mapErrorSM = mapError $ T.pack . Haskell.show

initLotto :: Contract (Last Lottery) s T.Text ()
initLotto = do

    tt <- mapErrorSM SM.getThreadToken
    logInfo $ "initLotto: tt= " ++ Haskell.show tt

    ownPkh <- ownPaymentPubKeyHash
    utxo <- utxosAt (Ledger.pubKeyHashAddress ownPkh Nothing)

    let mph' = Scripts.forwardingMintingPolicyHash typedLottoValidator
        lDatum = LottoDatum 
            {   adminPkh    = ownPkh
            ,   seqNum      = 0 
            ,   mph         = mph'
            }
        lottery = Lottery
            {   ttLotery    = tt
            }
        red = Redeemer (PlutusTx.toBuiltinData (lottoHash, Mint))
        ttPolicy = TT.curPolicy $ ttOutRef tt
        constraints = Constraints.mustPayToTheScript lDatum ((Ada.lovelaceValueOf 5000000) <> threadTokenValueInner (Just tt) lottoHash)
            <> Constraints.mustMintValueWithRedeemer red (threadTokenValueInner (Just tt) lottoHash)
            <> Constraints.mustSpendPubKeyOutput (ttOutRef tt)
            <> Constraints.mustBeSignedBy ownPkh
        lookups = Constraints.typedValidatorLookups typedLottoValidator
            <> Constraints.mintingPolicy ttPolicy 
            <> Constraints.unspentOutputs utxo

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx

    logInfo $ "initLotto: tx submitted successfully= " ++ Haskell.show adjustedUtx
    logInfo $ "initLotto: Lottery= " ++ Haskell.show lottery
    logInfo $ "initLotto: LotteryDatum= " ++ Haskell.show lDatum
    logInfo $ "init: lotto validator hash= " ++ Haskell.show lottoHash

    tell $ Last $ Just lottery

findLottery :: CurrencySymbol
            -> TokenName
            -> Contract w s T.Text (TxOutRef, ChainIndexTxOut, LottoDatum)
findLottery cs tn = do
    utxos <- utxosAt $ scriptHashAddress lottoHash
    let xs = [ (oref, o)
             | (oref, o) <- toList utxos
             , Value.valueOf (_ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Haskell.Left _          -> throwError "initLotto: datum missing"
            Haskell.Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "initLotto: datum has wrong type"
                Just d@LottoDatum{} -> Haskell.return (oref, o, d)
        _           -> throwError "initLotto: lotto utxo not found"



buyTicket :: Lottery -> TokenName -> Contract w s T.Text ()
buyTicket lot tn = do
 
    let tt' = ttLotery lot
        cs = TT.ttCurrencySymbol tt'
        ttTn = ttTokenName lottoHash
       
    (oref, o, d@LottoDatum{mph}) <- findLottery cs ttTn
    logInfo $ "buyTicket: found lotto utxo with datum= " ++ Haskell.show d
    logInfo $ "buyTicket: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "buyTicket: lotto validator hash= " ++ Haskell.show lottoHash

    let lotPolicy = policy oref tn
        red = Redeemer $ PlutusTx.toBuiltinData $ Buy tn
        constraints = Constraints.mustPayToTheScript d ((Ada.lovelaceValueOf 7000000) <> threadTokenValueInner (Just tt') lottoHash)
            <> Constraints.mustMintValue (lottoTicketMphValue mph tn)
            <> Constraints.mustSpendScriptOutput oref red
        lookups = Constraints.typedValidatorLookups typedLottoValidator
            <> Constraints.otherScript lottoValidator 
            <> Constraints.mintingPolicy lotPolicy
            <> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "buy: tx submitted= " ++ Haskell.show adjustedUtx
   

type LottoInitSchema =
        Endpoint "init" ()

type LottoSchema =
        Endpoint "buy"  (Lottery, TokenName)


initEndpoint :: Contract (Last Lottery) LottoInitSchema T.Text ()
initEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"init" $ \() -> initLotto


useEndpoint :: Contract () LottoSchema T.Text ()
useEndpoint = forever $ handleError logError $ awaitPromise buy
  where
    buy        = endpoint @"buy"       $ \(lot, tn) -> buyTicket lot tn



