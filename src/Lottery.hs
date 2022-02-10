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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Lottery
    ( LottoRedeemer (..)
    , LottoDatum (..)
    , LottoSchema
    , StartParams (..)
    , endpoints
    ) where

import Control.Lens (review)
import Control.Monad (void, forever)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.Set qualified as Set
import Data.Map (elemAt)
import Data.Text qualified as T (pack, Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Ada, CurrencySymbol, getCardanoTxId, getCardanoTxInputs, getCardanoTxOutRefs, getCardanoTxUnspentOutputsTx, mkMintingPolicyScript, MintingPolicyHash, scriptCurrencySymbol, TokenName, txInRef, PaymentPubKeyHash (unPaymentPubKeyHash), POSIXTime, pubKeyHashAddress, Redeemer(..), ScriptContext (ScriptContext, scriptContextTxInfo),
               TxOutRef, TxInfo, txInInfoOutRef, txInfoInputs, txInfoMint, Value, ValidatorHash, valuePaidTo)
import Ledger.Ada qualified as Ada (fromValue, lovelaceValueOf, getLovelace, toValue)
import Ledger.Constraints qualified as Constraints (adjustUnbalancedTx, mintingPolicy, mustBeSignedBy, mustSpendScriptOutput, mustMintValue, mustMintValueWithRedeemer, mustPayToPubKey, mustPayToTheScript, mkTx, mustSpendPubKeyOutput, typedValidatorLookups, unspentOutputs)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value (flattenValue, mpsSymbol, singleton, tokenName, Value)
import Ledger.Contexts as Contexts (ownCurrencySymbol)
import Playground.Contract as Playground (ToSchema)
import Plutus.Contract (AsContractError (_ConstraintResolutionError), awaitTxConfirmed, awaitPromise, balanceTx, Contract, Endpoint, handleError, mapError, Promise, endpoint, logInfo, logError, select, selectList,
                        submitTxConstraints, type (.\/))
import Plutus.Contract.Request (mkTxConstraints, mkTxContract, ownPaymentPubKeyHash, submitUnbalancedTx, submitTxConfirmed, utxosAt, submitTxConstraintsWith)
import Plutus.Contract.StateMachine as SM (getThreadToken, SMContractError, ttOutRef)
import Plutus.Contract.StateMachine.ThreadToken qualified as TT (curPolicy, threadTokenValue, ThreadToken(..), ttCurrencySymbol)
import Plutus.Contract.StateMachine.MintingPolarity as MP (MintingPolarity(Mint))
import Plutus.Contract.Wallet (getUnspentOutput)
import PlutusTx qualified
--import PlutusTx.Prelude (any, Bool (True), Integer, Semigroup ((<>)), ($), (&&), (-), (.), (>=), (==), (++), (>>=), traceIfFalse)
import PlutusTx.Prelude (any, Bool (True), const, foldMap, fst, snd, Integer, Maybe (Just), Maybe (Nothing), maybe, mempty, ($), (&&), (-), (.), (>=), (==), (++), (>>=), traceIfFalse, unless)
import Prelude (Semigroup (..))
import Prelude qualified as Haskell 
import Schema (ToSchema)
--import Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)
import Wallet.Types (AsContractError (_ConstraintResolutionError, _OtherError))
import qualified PlutusTx


data LottoRedeemer = 
       Open 
     | Buy TokenName

    deriving Haskell.Show
      
PlutusTx.unstableMakeIsData ''LottoRedeemer

data LottoDatum = LottoDatum
    {
        sequence :: Integer
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.unstableMakeIsData ''LottoDatum
--PlutusTx.makeIsDataIndexed ''LottoDatum
PlutusTx.makeLift ''LottoDatum


{-# INLINABLE mkLottoValidator #-}
mkLottoValidator :: LottoDatum -> LottoRedeemer -> ScriptContext -> Bool
mkLottoValidator _ _ _ = True


data Lottery
instance Scripts.ValidatorTypes Lottery where
    type instance RedeemerType Lottery = LottoRedeemer
    type instance DatumType Lottery = LottoDatum

lottoValidator :: Scripts.TypedValidator Lottery
lottoValidator = Scripts.mkTypedValidator @Lottery
    $$(PlutusTx.compile [|| mkLottoValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @LottoDatum @LottoRedeemer

{-
lottoValidator :: Lottery -> Validator
lottoValidator = Scripts.validatorScript . lottoTypedValidator

lottoAddress :: Lottery -> Address
lottoAddress = scriptAddress . lottoValidator

-}


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

{-# INLINABLE threadTokenValueInner #-}
threadTokenValueInner :: Maybe TT.ThreadToken -> ValidatorHash -> Value
threadTokenValueInner = maybe (const mempty) (TT.threadTokenValue . TT.ttCurrencySymbol)


data StartParams = StartParams
    { 
        spSeq       :: Integer
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)
    
PlutusTx.unstableMakeIsData ''StartParams
--PlutusTx.makeIsDataIndexed ''StartParams

strToBS :: Haskell.String -> BS.ByteString
strToBS = C8.pack

lottoToken::Haskell.String -> TokenName
lottoToken tn = Value.tokenName $ strToBS tn

mapErrorSM :: Contract w s SM.SMContractError a -> Contract w s T.Text a
mapErrorSM = mapError $ T.pack . Haskell.show

initLotto :: LottoDatum -> Contract w s T.Text ()
initLotto s@LottoDatum{sequence} = do
    logInfo $ "Initializing Lotto Datum: " <> Haskell.show s

    tt <- mapErrorSM SM.getThreadToken
    logInfo $ "initLotto: tt= " ++ Haskell.show tt

    ownPK <- ownPaymentPubKeyHash
    utxo <- utxosAt (Ledger.pubKeyHashAddress ownPK Nothing)

    let mph         = Scripts.forwardingMintingPolicyHash lottoValidator
        red = Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash lottoValidator, Mint))
        ttPolicy = TT.curPolicy $ ttOutRef tt
        --lookups     = Constraints.mintingPolicy curVali
        constraints = Constraints.mustPayToTheScript s ((Ada.lovelaceValueOf sequence) <> threadTokenValueInner (Just tt) (Scripts.validatorHash lottoValidator))
            <> Constraints.mustMintValueWithRedeemer red (threadTokenValueInner (Just tt) (Scripts.validatorHash lottoValidator))
            <> Constraints.mustSpendPubKeyOutput (ttOutRef tt)
            <> Constraints.mustBeSignedBy ownPK
        lookups = Constraints.typedValidatorLookups lottoValidator
            <> Constraints.mintingPolicy ttPolicy
            <> Constraints.unspentOutputs utxo

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    --unless (utx == adjustedUtx) $
    --  logInfo $ "Plutus.Contract.StateMachine.runInitialise: "
    --                <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
    submitTxConfirmed adjustedUtx
    logInfo $ "initLotto: tx submitted successfully " ++ Haskell.show adjustedUtx
    logInfo $ "initLotto: mph " ++ Haskell.show mph

    {-


   tt <- mapErrorSM SM.getThreadToken
    logInfo $ "initLotto: tt= " ++ Haskell.show tt

    ownPK <- ownPaymentPubKeyHash
    utxo <- utxosAt (Ledger.pubKeyHashAddress ownPK Nothing)

    let constraints = Constraints.mustPayToTheScript s (Ada.lovelaceValueOf sequence)
            <> foldMap ttConstraints tt
        red = Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash lottoValidator, Mint))
        ttConstraints TT.ThreadToken{SM.ttOutRef} =
            Constraints.mustMintValueWithRedeemer red (threadTokenValueInner tt)
            <> Constraints.mustSpendPubKeyOutput ttOutRef
        lookups = Constraints.typedValidatorLookups lottoValidator
            <> foldMap (Constraints.mintingPolicy . TT.curPolicy . ttOutRef) tt
            <> Constraints.unspentOutputs utxo

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    unless (utx == adjustedUtx) $
      logInfo $ "Plutus.Contract.StateMachine.runInitialise: "
                    <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
    submitTxConfirmed adjustedUtx



    let tx = Constraints.mustPayToTheScript s $ Ada.lovelaceValueOf sequence
    utx <- submitTxConstraints lottoValidator tx

    logInfo $ "initLotto: txOutRefs= " ++ Haskell.show (getCardanoTxUnspentOutputsTx utx)

    let txOutRef = fst $ elemAt 1 (getCardanoTxUnspentOutputsTx utx)
    logInfo $ "initLotto: txOutRef only= " ++ Haskell.show txOutRef

    utxos <- utxosAt (Scripts.validatorAddress lottoValidator)
    logInfo $ "initLotto: utxos= " ++ Haskell.show txOutRef

    let tn          = lottoToken "abc"
        mph         = Scripts.forwardingMintingPolicyHash lottoValidator
        --curSym      = curSymbol txOutRef tn
        curVali     = policy txOutRef tn
        lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendScriptOutput txOutRef _
                        <> Constraints.mustMintValue (lottoTicketMphValue mph tn)
    txs <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    --_ <- awaitTxConfirmed (getCardanoTxId txs)
    logInfo $ "initLotto: token created " ++ Haskell.show txs

    --case Set.lookupMin (getCardanoTxOutRefs utx) of
    --    Just inp -> LogInfo "Got uTOutRef: " ++ inp
    --    Nothing  -> throwing _OtherError "Balanced transaction has no inputs"
    -}
{-
-- | Get an unspent output belonging to the validator script.
getUnspentScriptOutput :: AsContractError e => Contract w s e TxOutRef
getUnspentScriptOutput = do
    pkh <- ownPaymentPubKeyHash
    utxo <- utxosAt (pubKeyHashAddress pkh Nothing)

    let ld = LottoDatum
                {
                    sequence = 5000000
                }
        constraints = Constraints.mustPayToTheScript ld (Ada.lovelaceValueOf 1)
        lookups = Constraints.unspentOutputs utxo
    
    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    --let adjustedUtx = Constraints.adjustUnbalancedTx utx
    --unless (utx == adjustedUtx) $
    --  logInfo $ "getUnspentScriptOutput: "
    --                <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
    --submitTxConfirmed adjustedUtx

    --utx <- Haskell.either (throwing _ConstraintResolutionError) Haskell.pure (Constraints.mkTx @Scripts.Any Haskell.mempty constraints)
    tx <- balanceTx (Constraints.adjustUnbalancedTx utx)
    case Set.lookupMin (getCardanoTxInputs tx) of
        Just inp -> Haskell.pure $ txInRef inp
        Nothing  -> throwing _OtherError "Balanced transaction has no inputs"

-}

buyTicket :: TokenName -> Contract w s T.Text ()
buyTicket tn = do
    pkh <- ownPaymentPubKeyHash
    txOutRef <- getUnspentOutput

    logInfo $ "BuyTicket: txOutRefs= " ++ Haskell.show txOutRef

    {-
    utxos <- utxosAt (Scripts.validatorAddress lottoValidator)

    let mph         = Scripts.forwardingMintingPolicyHash lottoValidator
        --curSym      = curSymbol txOutRef tn
        curVali     = policy txOutRef tn
        lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (lottoTicketMphValue mph tn)
    tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    _ <- awaitTxConfirmed (getCardanoTxId tx)
    -}
    {-

    pkh <- ownPaymentPubKeyHash
    txOutRef <- getUnspentOutput
    utxos <- utxosAt (pubKeyHashAddress pkh Haskell.Nothing)

    let curSym      = curSymbol txOutRef tn
        curVali     = policy txOutRef tn
        lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (lottoTicketValue curSym tn)
    tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    _ <- awaitTxConfirmed (getCardanoTxId tx)
    -}

    -- logInfo $ "buyTicket tx has been completed: " ++ Haskell.show mintTx


    {-

    let mph         = Scripts.forwardingMintingPolicyHash lottoValidator
        curVali     = Constraints.mintingPolicy $ policy txOutRef tn
        lookups     = -- Constraints.mintingPolicy curVali
                      -- Constraints.unspentOutputs utxos
                         Constraints.mustPayToPubKey pkh (lottoTicketValue mph tn)
        mintTx      = -- Constraints.mustSpendPubKeyOutput txOutRef
                        Constraints.mustPayToPubKey pkh (lottoTicketValue mph tn)
                        <> Constraints.mustMintValue (lottoTicketValue mph tn)
    --tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    --_ <- awaitTxConfirmed (getCardanoTxId tx)
    --pure theCurrency

    --txid <- mkTxConstraints (Constraints.typedValidatorLookups lottoValidator) mintTx
    txid <- mkTxConstraints (Constraints.typedValidatorLookups mph) mintTx
        >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx
    logInfo $ "buyTicket tx has been completed: " ++ Haskell.show mintTx

-}
{-
    pkh <- ownPaymentPubKeyHash
    
    let mph = Scripts.forwardingMintingPolicyHash lottoValidator
        tx = Constraints.mustMintValue (lottoTicketValue mph tn)
            -- <> Constraints.mustPayToPubKey pkh (lottoTicketValue mph tn)                                                                    
            -- <> Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 5000000)
        

    logInfo $ "buyTicket mph: " ++ Haskell.show mph
    ledgerTx <- mkTxConstraints (Constraints.typedValidatorLookups lottoValidator) tx
        >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx
    
    --tx' <- submitUnbalancedTx 
    --tx' <- submitTxConstraints lottoValidator tx
    --_ <- awaitTxConfirmed (getCardanoTxId tx')

    --txid <- mkTxConstraints (lottoValidator) tx
    --    >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx
    logInfo $ "buyTicket tx has been completed: " ++ Haskell.show tx
-}

mkLottoDatum :: StartParams -> LottoDatum
mkLottoDatum StartParams{spSeq} =
    LottoDatum
        { sequence = spSeq
        }


type LottoSchema =
        Endpoint "init" StartParams
    .\/ Endpoint "buy"  TokenName


endpoints :: Contract () LottoSchema T.Text ()
endpoints = forever $ handleError logError $ awaitPromise $ init `select` buy
  where
    init       = endpoint @"init"      (initLotto . mkLottoDatum)
    buy        = endpoint @"buy"       $ \(tn) -> buyTicket tn



