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
import Control.Monad (void, forever)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.Set qualified as Set
import Data.Map (elemAt, keys, toList, singleton)
import Data.Monoid (Last (..))
import Data.Text qualified as T (pack, Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Ada, Address, CurrencySymbol, Datum(..), getCardanoTxId, getCardanoTxInputs, getCardanoTxOutRefs, getCardanoTxUnspentOutputsTx, mkMintingPolicyScript, MintingPolicyHash, scriptCurrencySymbol, TokenName, txInRef, PaymentPubKeyHash (unPaymentPubKeyHash), POSIXTime, pubKeyHashAddress, Redeemer(..), ScriptContext (ScriptContext, scriptContextTxInfo),
               scriptHashAddress, toTxOut, TxOutRef, TxInfo, txInInfoOutRef, txInfoInputs, txInfoMint, txOutDatum, Value, Validator, ValidatorHash(..), valuePaidTo)
import Ledger.Ada qualified as Ada (fromValue, lovelaceValueOf, getLovelace, toValue)
import Ledger.Constraints qualified as Constraints (adjustUnbalancedTx, mintingPolicy, mustBeSignedBy, mustSpendScriptOutput, mustMintValue, mustMintValueWithRedeemer, mustPayToPubKey, mustPayToTheScript, mkTx, mustSpendPubKeyOutput, otherScript, typedValidatorLookups, unspentOutputs)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Tx (ChainIndexTxOut(..))
import Ledger.Value qualified as Value (flattenValue, mpsSymbol, singleton, tokenName, Value, valueOf)
import Ledger.Contexts as Contexts (ownCurrencySymbol)
import Playground.Contract as Playground (ToSchema)
import Plutus.Contract (AsContractError (_ConstraintResolutionError), awaitTxConfirmed, awaitPromise, balanceTx, Contract, Endpoint, handleError, mapError, Promise, endpoint, logInfo, logError, select, selectList,
                        tell, submitTxConstraints, throwError, type (.\/))
import Plutus.Contract.Request (mkTxConstraints, mkTxContract, ownPaymentPubKeyHash, submitUnbalancedTx, submitTxConfirmed, utxosAt, submitTxConstraintsWith, txOutFromRef)
import Plutus.Contract.StateMachine as SM (getThreadToken, SMContractError, ttOutRef)
import Plutus.Contract.StateMachine.ThreadToken qualified as TT (curPolicy, threadTokenValue, ThreadToken(..), ttCurrencySymbol)
import Plutus.Contract.StateMachine.MintingPolarity as MP (MintingPolarity(Mint))
import Plutus.Contract.Wallet (getUnspentOutput)
import PlutusTx qualified
--import PlutusTx.Prelude (any, Bool (True), Integer, Semigroup ((<>)), ($), (&&), (-), (.), (>=), (==), (++), (>>=), traceIfFalse)
import PlutusTx.Prelude (any, Bool (True), const, foldMap, fromBuiltin, fst, snd, Integer, Maybe (Just), Maybe (Nothing), maybe, mempty, ($), (&&), (-), (.), (>=), (==), (++), (>>=), traceIfFalse, unless)
import Prelude (Semigroup (..))
import Prelude qualified as Haskell 
import Schema (ToSchema)
--import Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)
import Wallet.Types (AsContractError (_ConstraintResolutionError, _OtherError))
import qualified PlutusTx


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

strToBS :: Haskell.String -> BS.ByteString
strToBS = C8.pack

lottoToken::Haskell.String -> TokenName
lottoToken tn = Value.tokenName $ strToBS tn

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
        --ttPolicy = policy (ttOutRef tt) (ttTokenName lottoHash)
        constraints = Constraints.mustPayToTheScript lDatum ((Ada.lovelaceValueOf 5000000) <> threadTokenValueInner (Just tt) lottoHash)
            <> Constraints.mustMintValueWithRedeemer red (threadTokenValueInner (Just tt) lottoHash)
           -- <> Constraints.mustMintValue (threadTokenValueInner (Just tt) lottoHash)
            <> Constraints.mustSpendPubKeyOutput (ttOutRef tt)
            <> Constraints.mustBeSignedBy ownPkh
        lookups = Constraints.typedValidatorLookups typedLottoValidator
            -- <> Constraints.mintingPolicy (Scripts.forwardingMintingPolicy typedLottoValidator)
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
            Haskell.Left _          -> throwError "datum missing"
            Haskell.Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@LottoDatum{..} -> Haskell.return (oref, o, d)
        _           -> throwError "lotto utxo not found"



buyTicket :: Lottery -> TokenName -> Contract w s T.Text ()
buyTicket lot tn = do
    ownPkh <- ownPaymentPubKeyHash
    --txOutRef <- getUnspentOutput
    utxos <- utxosAt (Ledger.pubKeyHashAddress ownPkh Nothing)

    let tt' = ttLotery lot
        cs = TT.ttCurrencySymbol tt'
        ttTn = ttTokenName lottoHash
       
    (soref, so, d@LottoDatum{..}) <- findLottery cs ttTn
    logInfo $ "found lotto utxo with datum= " ++ Haskell.show d
    logInfo $ "found lotto utxo oref= " ++ Haskell.show soref
    logInfo $ "buy: lotto validator hash= " ++ Haskell.show lottoHash

    let lotPolicy = policy soref tn

    case keys utxos of
        []       -> throwError "no utxo found"
        oref : _ -> do
            let red = Redeemer (PlutusTx.toBuiltinData (lottoHash, Mint))
                -- lotPolicy = policy oref tn
                constraints = Constraints.mustPayToTheScript d ((Ada.lovelaceValueOf 7000000) <> threadTokenValueInner (Just tt') lottoHash)
                    -- <> Constraints.mustMintValueWithRedeemer red (lottoTicketMphValue mph tn)
                    <> Constraints.mustMintValue (lottoTicketMphValue mph tn)
                    -- <> Constraints.mustPayToPubKey ownPkh (lottoTicketMphValue mph tn) 
                    <> Constraints.mustSpendScriptOutput soref red
                    -- <> Constraints.mustSpendScriptOutput oref red
                    -- <> Constraints.mustSpendPubKeyOutput oref
                    -- <> Constraints.mustBeSignedBy ownPkh
                lookups = Constraints.typedValidatorLookups typedLottoValidator
                    <> Constraints.otherScript lottoValidator 
                    <> Constraints.mintingPolicy lotPolicy
                    <> Constraints.unspentOutputs (singleton soref so)
                    -- <> Constraints.unspentOutputs utxos

            utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
            let adjustedUtx = Constraints.adjustUnbalancedTx utx
            submitTxConfirmed adjustedUtx
            logInfo $ "buy: tx submitted= " ++ Haskell.show adjustedUtx
            

    {-

        let curPolicy = policy oref tn
        red = Redeemer (PlutusTx.toBuiltinData lottoHash)
        constraints = Constraints.mustPayToTheScript d (Ada.lovelaceValueOf 2000000)
             -- <> Constraints.mustPayToPubKey ownPkh (lottoTicketMphValue mph tn)
             -- <> Constraints.mustMintValueWithRedeemer red (lottoTicketMphValue mph tn)
             -- <> Constraints.mustMintValue (lottoTicketMphValue mph tn) 
             <> Constraints.mustSpendScriptOutput oref red
            -- <> Constraints.mustBeSignedBy ownPkh
        lookups = Constraints.typedValidatorLookups typedLottoValidator
              -- <> Constraints.mintingPolicy curPolicy
              <> Constraints.unspentOutputs (singleton oref o)


    
    ownPkh <- ownPaymentPubKeyHash
    utxo <- utxosAt (Ledger.pubKeyHashAddress ownPkh Nothing)
    utxos <- utxosAt (Scripts.validatorAddress lottoValidator)

    case keys utxo of
        []       -> logError @Haskell.String "no utxo found at pkh address"
        oref : _ -> do
            logInfo $ "utxos at pkh address: " ++ Haskell.show oref
    
    case keys utxos of
        []       -> logError @Haskell.String "no utxo found at script address"
        oref : _ -> do
            logInfo $ "utxo at script address: " ++ Haskell.show oref
            chainIndexTxOut <- txOutFromRef oref
            case chainIndexTxOut of  
                Just cito -> do 
                    logInfo $ "txOut at script address: " ++ Haskell.show cito
                    --ld <- cito{_ciTxOutDatum}
                    case _ciTxOutDatum cito of
                        Haskell.Left dh' -> logError $ "got lotto datum hash at script address: " ++ Haskell.show dh'
                        Haskell.Right d' -> do
                                            logInfo $ "lotto datum at script address: " ++ Haskell.show d'
                                            let mph' = mph d'
                                                constraints = Constraints.mustPayToTheScript d' (Ada.lovelaceValueOf 2000000)
                                                    <> Constraints.mustMintValue (lottoTicketMphValue mph' tn)
                                                    <> Constraints.mustBeSignedBy ownPkh
                                                lookups = Constraints.unspentOutputs utxo       
    
                                            utx <- Haskell.either (throwing _ConstraintResolutionError)
                                                            Haskell.pure
                                                            (Constraints.mkTx lookups constraints)
                                            let adjustedUtx = Constraints.adjustUnbalancedTx utx
                                            submitTxConfirmed adjustedUtx
                                            logInfo $ "buyTicket tx has been completed: " ++ Haskell.show adjustedUtx
                                            
                Nothing -> logError @Haskell.String "no chain index found at utxo"

    --let mph'             = mph lot
      --  lDatum = LottoDatum 
      --      {   adminPkh    = adminPkh lot
      --     ,   seqNum      = 0 
      --      ,   mph         = mph lot
      --     }


-}

{-

    pkh <- ownPaymentPubKeyHash
    txOutRef <- getUnspentOutput

    logInfo $ "BuyTicket: txOutRefs= " ++ Haskell.show txOutRef

    utxos <- utxosAt (Scripts.validatorAddress lottoValidator)

    let mph             = mph lot
        --curSym      = curSymbol txOutRef tn
        curPolicy       = policy txOutRef tn
        constraints     = Constraints.mustSpendPubKeyOutput txOutRef
                            <> Constraints.mustMintValue (lottoTicketMphValue mph tn)
        lookups         = Constraints.mintingPolicy curVali
                            <> Constraints.unspentOutputs utxos

    tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    _ <- awaitTxConfirmed (getCardanoTxId tx)

-}
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

{-
mkLottoDatum :: StartParams -> LottoDatum
mkLottoDatum StartParams{spSeq} =
    LottoDatum
        { sequence = spSeq
        }
-}

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



