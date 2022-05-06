{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-} 
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}

module OnChain
    (   buyHash
    ,   buyValidator
    ,   calcPayouts
    ,   LottoDatum(..)
    ,   lottoHash
    ,   lottoValidator
    ,   minAda
    ,   mkLottoScript
    ,   ticketCurSymbol
    ,   ticketMintPolicy
    ,   ticketMphValue
    ,   typedBuyValidator
    ,   typedLottoValidator
    ,   threadTokenPolicy
    ,   threadTokenValue
    ,   threadTokenCurSymbol
    ,   untypedBuyHash
    ,   untypedBuyValidator
    ,   untypedLottoValidator
    ,   untypedLottoHash
    ) where


import Data.Aeson                                               (FromJSON, ToJSON)
import GHC.Generics                                             (Generic)
import Ledger.Ada qualified as Ada                              (lovelaceValueOf)
import Ledger.Address qualified as Address                      (Address, PaymentPubKeyHash(..))
import Ledger.Contexts qualified as Contexts                    (scriptCurrencySymbol, TxInInfo(..), TxInInfo(txInInfoOutRef), TxInInfo(txInInfoResolved), TxOut, 
                                                                scriptCurrencySymbol, TxInInfo(..), TxInInfo(txInInfoOutRef), TxInInfo(txInInfoResolved), TxOut)
import Ledger.Crypto qualified as Crypto                        (PubKeyHash)
import Ledger.Scripts qualified as Scripts                      (Datum(..), DatumHash, MintingPolicy, MintingPolicyHash, mkMintingPolicyScript, 
                                                                 mkValidatorScript, Script, Validator, ValidatorHash(..), validatorHash)
import Ledger.Time qualified as Time                            (POSIXTimeRange)
import Ledger.Typed.Scripts qualified as TScripts               (TypedValidator, validatorScript, validatorHash)
import Ledger.Typed.Scripts.Validators qualified as Validators  (unsafeMkTypedValidator)
import Ledger.Typed.TypeUtils qualified as TypeUtils            (Any)
import Ledger.Tx qualified as Tx                                (TxOut(..), TxOut(txOutAddress), TxOut(txOutValue ), TxOutRef(..))
import Ledger.TxId as TxId                                      (TxId(..))   
import Ledger.Value qualified as Value                          (CurrencySymbol, flattenValue, mpsSymbol, singleton, TokenName(..), TokenName(unTokenName), Value)
import Plutus.V1.Ledger.Api as Ledger                           (unsafeFromBuiltinData, unValidatorScript)
import PlutusTx qualified                                       (applyCode, compile, fromBuiltinData, liftCode, makeLift, makeIsDataIndexed)
import PlutusTx.AssocMap qualified as AssocMap                  (delete, keys, insert, lookup, Map, member, singleton)
import PlutusTx.Prelude                                         (abs, any, Bool(..), BuiltinByteString, BuiltinData, check, divide, find, fmap, filter, fst, head,
                                                                 Integer, length, Maybe (..), modulo, ($), (&&), (==), (<=), (>=), (>), (||), (!!), (+), (-), 
                                                                 (++), (*), (<$>), (<>), otherwise, sha2_256, snd, take, traceIfFalse, traceError)              
import Prelude qualified as Haskell                             (Eq, Show)
import Types
import Utils

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

-- Lotto Datum fields
-- a = lotto (a)dministration:
--      - lottoId = unique lotto identifier
--      - adminPkh = lotto admin public key hash
--      - sponsorPkh = sponsor publich key hash
--      - lottoValAddr = lotto validator script address
--      - lottoTokenValue = lotto token value 
--      - buyValAddr = buy validator script address
--      - buyTokenValue = buy token value
--      - ticketMph   = buy ticket minting policy hash for minting lotto tickets
--      - percentFees = precentage of fees from the ticket costs that goes to the lotto admin
--      - ticketCost = ticket cost
--      - difficulty = difficult of the lotto (ie. the number of digits in the winning number)
--      - deadline = buy lotto tickets *before the deadline and close (and draw) the lotto *after* the deadline
-- winner = the winner + sponsor
-- jackpot = the size of the jackpot
-- seqNum = the lotto sequence number (it increments with every new lotto cycle)
-- treasury = the lotto treasury
-- fees = the lotto admin fees
-- beneficiaries = the beneficiaries which include the winner and the sponsor
-- lottoState = state is (0 = init, 1 = open, 2 = closed, 3 = redeem, 4 = payout, 5 = end)
-- winNums = an array of winning integer digits representing the base10 digits of the (h)ash from the close output tx

data LottoDatum = LottoDatum
    { lottoAdmin        :: !LottoAdmin                                      -- 0
    , winner            :: ![(Address.PaymentPubKeyHash, [Integer])]        -- 1
    , jackpot           :: !Integer                                         -- 2
    , seqNum            :: !Integer                                         -- 3
    , treasury          :: !Integer                                         -- 4
    , fees              :: !Integer                                         -- 5
    , beneficiaries     :: AssocMap.Map Address.PaymentPubKeyHash Integer   -- 6
    , lottoState        :: !Integer                                         -- 7
    , winNums           :: ![Integer]                                       -- 8
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''LottoDatum [('LottoDatum, 0)]
PlutusTx.makeLift ''LottoDatum

-------------------------------------------------------------------------------
-- The following wrappers have only been created for performance optimizations
-------------------------------------------------------------------------------

-- Create a wrapper for TxInfo so we only parse what we need
data ATxInfo = ATxInfo
    { txInfoInputs      :: [Contexts.TxInInfo]  -- ^ Transaction inputs
    , txInfoOutputs     :: [Tx.TxOut]           -- ^ Transaction outputs
    , txInfoFee         :: BuiltinData          -- ^ The fee paid by this transaction.
    , txInfoMint        :: Value.Value          -- ^ The 'Value' minted by this transaction.
    , txInfoDCert       :: BuiltinData          -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: BuiltinData          -- ^ Withdrawals
    , txInfoValidRange  :: Time.POSIXTimeRange  -- ^ The valid range for the transaction.
    , txInfoSignatories :: [Crypto.PubKeyHash]  -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(Scripts.DatumHash, Scripts.Datum)]
    , txInfoId          :: BuiltinData
    } deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeIsDataIndexed ''ATxInfo [('ATxInfo,0)]

-- Create wraper for ScriptPurpose so we only parse what we need, eg Spending and Minting
data AScriptPurpose
    = Minting Value.CurrencySymbol
    | Spending Tx.TxOutRef
    | Rewarding BuiltinData
    | Certifying BuiltinData
    deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeIsDataIndexed
  ''AScriptPurpose
  [ ('Minting, 0),
    ('Spending, 1),
    ('Rewarding, 2),
    ('Certifying, 3)
  ]

-- Create a wrapper for ScriptContext so we only parse what we need
data AScriptContext = AScriptContext
  { aScriptContextTxInfo :: ATxInfo
  , aScriptContextPurpose :: AScriptPurpose
  }
  deriving stock (Generic, Haskell.Eq, Haskell.Show)

PlutusTx.makeIsDataIndexed ''AScriptContext [('AScriptContext,0)]


{-# INLINABLE findOwnInput' #-}
-- | Find the input currently being validated.
findOwnInput' :: AScriptContext -> Maybe Contexts.TxInInfo
findOwnInput' AScriptContext{aScriptContextTxInfo=ATxInfo{txInfoInputs}, aScriptContextPurpose=Spending txOutRef} =
    find (\Contexts.TxInInfo{Contexts.txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
findOwnInput' _ = Nothing


{-# INLINABLE getContinuingOutputs' #-}
getContinuingOutputs' :: AScriptContext -> [Tx.TxOut]
getContinuingOutputs' actx 
    | Just Contexts.TxInInfo{Contexts.txInInfoResolved=Tx.TxOut{Tx.txOutAddress}} <- 
        findOwnInput' actx = filter (f txOutAddress) (txInfoOutputs $ aScriptContextTxInfo actx)
  where
    f addr Tx.TxOut{Tx.txOutAddress=otherAddress} = addr == otherAddress
getContinuingOutputs' _ = traceError "Lf'" -- Can't get any continuing outputs


{-# INLINABLE txSignedBy' #-}
-- | Check if a transaction was signed by the given public key.
txSignedBy' :: ATxInfo -> Crypto.PubKeyHash -> Bool
txSignedBy' ATxInfo{txInfoSignatories} k = case find (k ==) txInfoSignatories of
    Just _  -> True
    Nothing -> False


{-# INLINABLE findDat' #-}
-- | Find the data corresponding to a data hash, if there is one
findDat' :: Scripts.DatumHash -> ATxInfo -> Maybe Scripts.Datum
findDat' dsh ATxInfo{txInfoData} = snd <$> find f txInfoData
  where
    f (dsh', _) = dsh' == dsh


{-# INLINABLE spendsOutput' #-}
-- | Check if the pending transaction spends a specific transaction output
--   (identified by the hash of a transaction and an index into that
--   transactions' outputs)
spendsOutput' :: ATxInfo -> TxId -> Integer -> Bool
spendsOutput' p h i =
    let spendsOutRef inp =
            let outRef = Contexts.txInInfoOutRef inp
            in h == Tx.txOutRefId outRef
                && i == Tx.txOutRefIdx outRef

    in any spendsOutRef (txInfoInputs p)


{-# INLINABLE ownCurrencySymbol' #-}
-- | The 'CurrencySymbol' of the current validator script.
ownCurrencySymbol' :: AScriptContext -> Value.CurrencySymbol
ownCurrencySymbol' AScriptContext{aScriptContextPurpose=Minting cs} = cs
ownCurrencySymbol' _                                                = traceError "Lh" -- Can't get currency symbol of the current validator script

-------------------------------------------------------------------------------
-- End of wrappers created for performance optimizations
-------------------------------------------------------------------------------

-- | minAda is only required to support work around due to unable to include
--   minting in same redeem lotto validator transaction.   After vasil
--   hard fork, we should be able to include minting in the same transaction
--   and will validate the burn of the ticket and not need minAda
{-# INLINABLE minAda #-}
minAda :: Value.Value
minAda = Ada.lovelaceValueOf 2000000

    
-- | Check to see if the buy token belongs to a value
{-# INLINABLE checkBuyValue #-}
checkBuyValue :: Value.Value -> Value.Value -> Bool
checkBuyValue buyTokenValue addrValue  = 
    let (buyCs, buyTn, buyAmt) = (Value.flattenValue buyTokenValue)!!0
        valuesAtAddr = Value.flattenValue addrValue

        inspectValues :: [(Value.CurrencySymbol, Value.TokenName, Integer)] -> Bool
        inspectValues [] = False
        inspectValues ((cs, tn', amt):xs)
            | (cs == buyCs) && (tn' == buyTn) && (amt == buyAmt) = True
            | otherwise = inspectValues xs
    in inspectValues valuesAtAddr

-- | Find the buy token in the list of outputs, returning both the address and the value
--   of that utxo
{-# INLINABLE findBuyOutput #-}
findBuyOutput :: Value.Value -> [Contexts.TxOut] -> Maybe (Address.Address, Value.Value)
findBuyOutput _ [] = Nothing
findBuyOutput txVal (x:xs) 
    | checkBuyValue txVal (Tx.txOutValue x) = Just (Tx.txOutAddress x, Tx.txOutValue x)
    | otherwise = findBuyOutput txVal xs


-- | Find the buy token in the list of inputs, returning both the address and the value
--   of that utxo
{-# INLINABLE findBuyAddrInputs #-}
findBuyAddrInputs :: Value.Value -> [Contexts.TxInInfo] -> Maybe (Address.Address, Value.Value)
findBuyAddrInputs _ [] = Nothing
findBuyAddrInputs txVal (x:xs) = case findBuyOutput txVal [Contexts.txInInfoResolved x] of
                                    (Just (scriptAddr, val)) -> Just (scriptAddr, val)  
                                    Nothing                  ->  findBuyAddrInputs txVal xs


-- | Check to see if the buy token is in the list of outputs locked at an address            
{-# INLINABLE validBuyOutputs #-}
validBuyOutputs :: Address.Address -> Value.Value -> [Contexts.TxOut] -> Bool
validBuyOutputs _ _ [] = False
validBuyOutputs scriptAddr txVal (x:xs)
    | (Tx.txOutAddress x == scriptAddr) && checkBuyValue txVal (Tx.txOutValue x) = True
    | otherwise = validBuyOutputs scriptAddr txVal xs


-- | Check to see if the buy token is in the list of inputs locked at an address
{-# INLINABLE validBuyInputs #-}
validBuyInputs :: Address.Address -> Value.Value -> [Contexts.TxInInfo] -> Bool
validBuyInputs _ _ [] = False
validBuyInputs scriptAddr txVal (x:xs)
    | validBuyOutputs scriptAddr txVal [Contexts.txInInfoResolved x] = True
    | otherwise = validBuyInputs scriptAddr txVal xs


-- | Check that that the value is included in the outputs
{-# INLINABLE validWinOut #-}
validWinOut :: Value.Value -> [Contexts.TxOut] -> Bool
validWinOut _ [] = False
validWinOut txVal (x:xs)
    | (Tx.txOutValue x == txVal) = True
    | otherwise = validWinOut txVal xs
       

-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Address.Address -> Value.Value -> [Contexts.TxOut] -> Bool
validOutputs _ _ [] = False
validOutputs scriptAddr txVal (x:xs)
    | (Tx.txOutAddress x == scriptAddr) && (Tx.txOutValue x == txVal) = True
    | otherwise = validOutputs scriptAddr txVal xs
                             

-- | Check that the value is locked at an address for the provided inputs
{-# INLINABLE validInputs #-}
validInputs :: Address.Address -> Value.Value -> [Contexts.TxInInfo] -> Bool
validInputs _ _ [] = False
validInputs scriptAddr txVal (x:xs)
    | validOutputs scriptAddr txVal [Contexts.txInInfoResolved x] = True
    | otherwise = validInputs scriptAddr txVal xs
                                   

-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the Open redeemer
{-# INLINABLE verifyOpenDat #-}
verifyOpenDat :: LottoDatum -> LottoDatum -> Bool
verifyOpenDat new old =                 
    sponsorPkh (lottoAdmin old) == sponsorPkh (lottoAdmin new) --can't change sponsor until governance is implemented
    && (lottoTokenValue (lottoAdmin old) == lottoTokenValue (lottoAdmin new))
    && (buyTokenValue (lottoAdmin old) == buyTokenValue (lottoAdmin new))
    && (ticketMph   (lottoAdmin old) == ticketMph   (lottoAdmin new))
    && (difficulty (lottoAdmin old) == difficulty (lottoAdmin new))
    && (length (winner new) == 1)
    && checkJackpot 
    && (modulo (seqNum old + 1) 255 == seqNum new) 
    && checkTreasury 
    && (fees old == fees new)
    && (lottoState new == 1)
  where
    checkJackpot :: Bool
    checkJackpot = 
        if jackpot old > treasury old then jackpot old <= jackpot new   
        else jackpot old + divide (treasury old) 2 <= jackpot new

    checkTreasury :: Bool
    checkTreasury = 
        if jackpot old > treasury old then treasury old == treasury new
        else divide (treasury old) 2 == treasury new


-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the StartBuy redeemer
{-# INLINABLE verifyStartBuyDat #-}
verifyStartBuyDat :: LottoDatum -> LottoDatum -> Bool
verifyStartBuyDat new old =
    (lottoAdmin old == lottoAdmin new) 
    && (winner old == winner new) 
    && (jackpot old == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees new == fees new)
    && (lottoState old == lottoState new)


-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the StopBuy redeemer
{-# INLINABLE verifyStopBuyDat #-}
verifyStopBuyDat :: LottoDatum -> LottoDatum -> Value.Value -> Address.Address ->  [Contexts.TxInInfo] -> Bool
verifyStopBuyDat new old buyTokVal' addr txIns =              
    lottoAdmin old == lottoAdmin new 
    && (winner old == winner new) 
    && (seqNum old == seqNum new) 
    && (lottoState old == lottoState new)
    && checkDistribution
    && checkTotalBuyValue
  where     
    -- The total $$$ value of tickets bought during this lotto cycle
    totalBuyValue = (jackpot new - jackpot old) + (treasury new - treasury old) + (fees new - fees old)

    -- The percentage of ticket costs that goes to treasury and lotto admin fees
    perctFee :: Integer
    perctFee = percentFees (lottoAdmin old)

    checkDistribution :: Bool
    checkDistribution =    
        (jackpot new == jackpot old + divide (totalBuyValue * abs(divide (100 - perctFee) 2)) 100)
        && (treasury new == treasury old + divide (totalBuyValue * abs(divide (100 - perctFee) 2)) 100)
        && (fees new == fees old + divide (totalBuyValue * perctFee) 100)

    -- Get the current total value of the buy contract
    getTotalBuyValue = Ada.lovelaceValueOf (abs(totalBuyValue)) <> buyTokVal'

    checkTotalBuyValue :: Bool
    checkTotalBuyValue = validInputs addr getTotalBuyValue txIns
        

-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the Close redeemer                            
{-# INLINABLE verifyCloseDat #-}
verifyCloseDat :: LottoDatum -> LottoDatum -> Bool
verifyCloseDat new old =
    lottoAdmin old == lottoAdmin new
    && (winner old == winner new)
    && (jackpot old == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees old == fees new)  
    && (lottoState new == 2)
              

-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the Draw redeemer
{-# INLINABLE verifyDrawDat #-}
verifyDrawDat :: LottoDatum -> LottoDatum -> [Contexts.TxInInfo] -> BuiltinByteString -> Bool
verifyDrawDat new old txInList txId'' =
    lottoAdmin old == lottoAdmin new 
    && validTxIn txInList -- verify that the tx is part of our inputs
    && (winner old == winner new)
    && (jackpot old == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees old == fees new)   
    && (lottoState new == 3) 
    && (getHexInt txId'' == winNums new) -- verify the winning numbers derived by the close tx hash
  where
    lotValAddr' = lottoValAddr (lottoAdmin old)
    lotTokVal' = lottoTokenValue (lottoAdmin old)
    buyTokVal' = buyTokenValue (lottoAdmin old)

    getTxIdBS :: Contexts.TxInInfo -> BuiltinByteString
    getTxIdBS tx' = getTxId(Tx.txOutRefId (Contexts.txInInfoOutRef tx'))

    -- The tx input value needs to include the datum value + lotto token value + buy token value
    getTxValue :: Value.Value
    getTxValue = Ada.lovelaceValueOf amount <> lotTokVal' <> buyTokVal'
      where amount = jackpot old + treasury old + fees old

    -- Confirm that the txId maticketCosthes the input utxo and that the input utxo
    -- was locked at the script address
    validTxIn :: [Contexts.TxInInfo] -> Bool
    validTxIn []     = False
    validTxIn (x:xs)
        | (getTxIdBS x == txId'') && (validOutputs lotValAddr' getTxValue [Contexts.txInInfoResolved x]) = True
        | otherwise = validTxIn xs


-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the Redeem redeemer     
{-# INLINABLE verifyRedeemDat #-}  
verifyRedeemDat :: LottoDatum -> LottoDatum -> Value.TokenName -> Integer -> Bool
verifyRedeemDat new old tn difficulty =  
    lottoAdmin old == lottoAdmin new    
    && (head (winner old) == head (winner new)) -- make sure the sponsor is still in the list
    && (jackpot old == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees old == fees new) 
    && (lottoState old == lottoState new) 
    && (winNums old == winNums new)
    && maxWinNum
    && validWinNum -- verify that the token maticketCosthes the winning numbers
  where
    (seq, num) = getIntsFromToken (Value.unTokenName tn) difficulty

    validWinNum :: Bool
    validWinNum =    seq == seqNum old
                    && num == take difficulty (winNums old)

    maxWinNum :: Bool
    maxWinNum = length (winner new) <= 2 -- the winner + the beneficiary
            

-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the Calc redeemer
{-# INLINABLE verifyCalcDat #-}
verifyCalcDat :: LottoDatum -> LottoDatum -> Integer -> Bool
verifyCalcDat new old potSplit' =
    lottoAdmin old == lottoAdmin new   
    && (length (winner new) > 1)  -- has to be at least one winner
    && (jackpot old == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees old == fees new) 
    && (calcPayouts (jackpot old) (winner old) potSplit' == beneficiaries new)
    && (lottoState new == 4)
    && (winNums old == winNums new)


-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the Payout redeemer
{-# INLINABLE verifyPayoutDat #-}
verifyPayoutDat :: LottoDatum -> LottoDatum -> Address.PaymentPubKeyHash -> Bool
verifyPayoutDat new old pkh =
    AssocMap.member pkh (beneficiaries old) -- check to see if the pkh is in the beneficiary list
    && (lottoAdmin old == lottoAdmin new)
    && ((jackpot old - potPay) == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees old == fees new) 
    && (AssocMap.delete pkh (beneficiaries old) == beneficiaries new) 
    && (lottoState old == lottoState new)
    && (winNums old == winNums new) 
  where
    potPay = case AssocMap.lookup pkh (beneficiaries old) of
                Just amount -> amount
                Nothing     -> 0


-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the End redeemer
{-# INLINABLE verifyEndDat #-}
verifyEndDat :: LottoDatum -> LottoDatum -> Bool
verifyEndDat new old =
    lottoAdmin old == lottoAdmin new
    && (jackpot old == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees old == fees new)  
    && (lottoState new == 5)


-- | Validate the lotto datum changes between the input datum vs the output datum
--   for the Collect redeemer
{-# INLINABLE verifyCollectDat #-}
verifyCollectDat :: LottoDatum -> LottoDatum -> Bool
verifyCollectDat new old =
    lottoAdmin old == lottoAdmin new 
    && (winner old == winner new) 
    && (jackpot old == jackpot new)  
    && (seqNum old == seqNum new) 
    && (treasury old == treasury new) 
    && (fees new == 0)
    && (beneficiaries old == beneficiaries new) 
    && (lottoState old == lottoState new)
    && (winNums old == winNums new)
            

-- | Calculate the payout to the winner and the sponsor based on the 
--   the amount of the jackpot and the % that goes to the sponsor vs
--   the winner
{-# INLINABLE calcPayouts #-}
calcPayouts :: Integer -> [(Address.PaymentPubKeyHash, [Integer])] -> Integer -> AssocMap.Map Address.PaymentPubKeyHash Integer
calcPayouts j' w' potSplit' = finalPayout
  where     
    potSplit = divide (j' * potSplit') 100

    -- update the winner with payout
    totalPayout :: AssocMap.Map Address.PaymentPubKeyHash Integer
    totalPayout = AssocMap.singleton (fst(w'!!1)) (j' - potSplit)

        -- update sponsor beneficiary with potSplit of the jackpot
    finalPayout :: AssocMap.Map Address.PaymentPubKeyHash Integer
    finalPayout = AssocMap.insert (fst(w'!!0)) potSplit totalPayout
    
       
-- | Veriy the payout that is sent to the winner and the sponsor
--   We need to look at the list of pkh that were provided with the
--   transaction and need to pull out the one that matches the 
--   beneficiary list.  Only that pkh is the one we should validate
--   the datum with.
{-# INLINABLE verifyPayout #-}
verifyPayout :: LottoDatum -> LottoDatum -> [Crypto.PubKeyHash] -> Bool
verifyPayout new old pkh' = verifyPayoutDat new old getPaymentPubKey
  where
    getKeys :: [Address.PaymentPubKeyHash]
    getKeys = AssocMap.keys (beneficiaries old)

    getPubKeyHash :: [Crypto.PubKeyHash]
    getPubKeyHash = fmap Address.unPaymentPubKeyHash getKeys

    findKey :: [Crypto.PubKeyHash] -> [Crypto.PubKeyHash]
    findKey [] = []
    findKey (x:xs) = filter (==x) pkh' ++ findKey xs 

    getPaymentPubKey::Address.PaymentPubKeyHash
    getPaymentPubKey = Address.PaymentPubKeyHash ((findKey getPubKeyHash)!!0)


-- | The main lotto validator which controls the lotto state machine
--   and validates the datum transistions and each state accordingly
{-# INLINABLE mkLottoValidator #-}
mkLottoValidator :: LottoValidatorParams -> LottoDatum -> LottoRedeemer -> AScriptContext -> Bool
mkLottoValidator params dat red ctx =    
    case red of
        Open        ->  validOpen 
        StartBuy    ->  validStartBuy
        StopBuy     ->  validStopBuy  
        Close       ->  validClose 
        Draw txId   ->  validDraw txId 
        Redeem tn   ->  validRedeem tn 
        Calc        ->  validCalc 
        Payout      ->  validPayout 
        End         ->  validEnd  
        Collect     ->  validCollect  
     
  where
    info :: ATxInfo
    info = aScriptContextTxInfo ctx

    adminPkh' = adminPkh (lottoAdmin dat)
    lotTokVal' = lottoTokenValue (lottoAdmin dat)
    lotValAddr' = lottoValAddr (lottoAdmin dat)
    buyTokVal' = buyTokenValue (lottoAdmin dat)

    sigByLotAdmin :: Bool
    sigByLotAdmin =  txSignedBy' info $ Address.unPaymentPubKeyHash adminPkh'

    outputDat :: LottoDatum
    (_, outputDat) = case getContinuingOutputs' ctx of
        [o] -> case Tx.txOutDatumHash o of
            Nothing   -> traceError "LV11"              -- wrong output type
            Just h -> case findDat' h info of
                Nothing        -> traceError "LV12"     -- datum not found
                Just (Scripts.Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ld' -> (o, ld')
                    Nothing  -> traceError "LV13"       -- error decoding data
        _   -> traceError "LV14"                        -- expected exactly one continuing output

    jackpotOld = jackpot dat + treasury dat + fees dat
    jackpotNew = jackpot outputDat + treasury outputDat + fees outputDat

    getOldDatVal :: Value.Value
    getOldDatVal = Ada.lovelaceValueOf jackpotOld <> lotTokVal' <> buyTokVal' 

    getNewDatVal :: Value.Value
    getNewDatVal = Ada.lovelaceValueOf jackpotNew <> lotTokVal' <> buyTokVal' 

    -- During the buy state, the buy token must *not* be locked to the lotto validator address
    -- The buy token *must* be locked at the buy validator address to signal to the buy minting policy
    -- that it is ok to mint lotto tickets 
    getOldBuyDatVal :: Value.Value
    getOldBuyDatVal = Ada.lovelaceValueOf jackpotOld <> lotTokVal' 

    getNewBuyDatVal :: Value.Value
    getNewBuyDatVal = Ada.lovelaceValueOf jackpotNew <> lotTokVal'

    validOpen :: Bool
    validOpen = 
        ( lottoState dat == 0 || lottoState dat == 5) -- check for valid lotto state
        && sigByLotAdmin
        && (percentFees (lottoAdmin outputDat) >= 0 && percentFees (lottoAdmin outputDat) <= 100) -- ensure valid percent fees
        && (lvpPotSplit params >= 0 && lvpPotSplit params <= 100) -- ensure valid pot split percent
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info))     
        && (verifyOpenDat outputDat dat)
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))

    validStartBuy :: Bool
    validStartBuy = 
        ( lottoState dat == 1) -- check for valid lotto state
        && sigByLotAdmin
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info))     
        && (verifyStartBuyDat outputDat dat)
        && (validOutputs lotValAddr' getNewBuyDatVal (txInfoOutputs info))

                -- commenting out deadline check for now for beta testing
                -- && (contains (to $ deadline (lottoAdmin dat)) $ txInfoValidRange info)
    
    validStopBuy :: Bool
    validStopBuy = 
        ( lottoState dat == 1) -- check for valid lotto state
        && sigByLotAdmin
        && (validInputs lotValAddr' getOldBuyDatVal (txInfoInputs info))     
        && (verifyStopBuyDat outputDat dat buyTokVal' lotValAddr' (txInfoInputs info))
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))
        -- commenting out deadline check for now for beta testing
        -- && (contains (from $ deadline (lottoAdmin dat)) $ txInfoValidRange info)

    validClose :: Bool
    validClose = 
        (lottoState dat == 1)  -- check for valid lotto state
        && sigByLotAdmin
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info)) 
        && (verifyCloseDat outputDat dat)
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))

    validDraw :: BuiltinByteString -> Bool
    validDraw txId' = 
        (lottoState dat == 2)  -- check for valid lotto state
        && sigByLotAdmin
        && (verifyDrawDat outputDat dat (txInfoInputs info) txId')
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))

    validRedeem :: Value.TokenName -> Bool
    validRedeem tn' = 
        (lottoState dat == 3) -- check for valid lotto state
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info)) 
        && (verifyRedeemDat outputDat dat tn' (lvpDifficulty params))
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))
        --only needed if not burning the ticket
        && (validWinOut (minAda <> (ticketMphValue (ticketMph (lottoAdmin dat)) tn' 1))  (txInfoOutputs info))

        -- Commenting out for now due to going over max tx size limits
        -- once vasil hard fork arrives, we should be able to re-enable.
        -- this is nice to have when there is only one winner, but is required
        -- for multiple winner scenarios                
        -- && (txInfoMint info == ticketMphValue (ticketMph (lottoAdmin dat)) tn' (-1))

    validCalc :: Bool
    validCalc = 
        (lottoState dat == 3) -- check for valid lotto state
        && sigByLotAdmin
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info)) 
        && (verifyCalcDat outputDat dat (lvpPotSplit params))
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))
    
    validPayout :: Bool
    validPayout = 
        (lottoState dat == 4) -- check for valid lotto state
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info))
        && (verifyPayout outputDat dat (txInfoSignatories info))
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))
                                
    validEnd :: Bool
    validEnd =     
        sigByLotAdmin
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info)) 
        && (verifyEndDat outputDat dat)
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))

    validCollect :: Bool
    validCollect = 
        sigByLotAdmin
        && (validInputs lotValAddr' getOldDatVal (txInfoInputs info)) 
        && (verifyCollectDat outputDat dat)
        && (validOutputs lotValAddr' getNewDatVal (txInfoOutputs info))            


-- | Creating a wrapper around lotto validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapLottoValidator #-}
wrapLottoValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapLottoValidator params datum redeemer ctx =
   check $ mkLottoValidator (unsafeFromBuiltinData params) (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx)


untypedLottoValidator :: BuiltinData -> Scripts.Validator
untypedLottoValidator params = Scripts.mkValidatorScript $
    $$(PlutusTx.compile [||wrapLottoValidator||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params


-- | We need a typedValidator for offchain mkTxConstraints, so 
--   created it using the untyped validator
typedLottoValidator :: BuiltinData -> TScripts.TypedValidator TypeUtils.Any
typedLottoValidator params = Validators.unsafeMkTypedValidator $ untypedLottoValidator params


mkLottoScript :: BuiltinData -> Scripts.Script
mkLottoScript params = unValidatorScript $ untypedLottoValidator params


lottoValidator :: BuiltinData -> Scripts.Validator
lottoValidator params = TScripts.validatorScript $ typedLottoValidator params


lottoHash :: BuiltinData -> Scripts.ValidatorHash
lottoHash params = TScripts.validatorHash $ typedLottoValidator params


untypedLottoHash :: BuiltinData -> Scripts.ValidatorHash
untypedLottoHash params = Scripts.validatorHash $ untypedLottoValidator params



-- | The buy validator is used only for buying of tickets and transferring
--   the buy token and moving collected funds back to the lotto validator script
--   when done with the buy state
{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: BuyValidatorParams -> BuyDat -> BuyRedeemer -> AScriptContext -> Bool
mkBuyValidator params dat red ctx = 
    case red of
        BuyTicket      ->  traceIfFalse "BV1" checkBuyTicketOutput          -- total value of the output not found
                        && traceIfFalse "BV2" checkBuyDatum                 -- buy datum invalid 
                        -- && checkDeadline

        TransferToken   -> traceIfFalse "BV3" sigByLotAdmin                 -- invalid admin signature
                        && traceIfFalse "BV4" checkTransferTokenOutput      -- total value of the output not found 

      where
        info :: ATxInfo
        info = aScriptContextTxInfo ctx

        -- only valid for BuyTicket transactions
        outputDat :: BuyDat
        (_, outputDat) = case getContinuingOutputs' ctx of
            [o] -> case Tx.txOutDatumHash o of
                Nothing   -> traceError "BV5"               -- wrong output type
                Just h -> case findDat' h info of
                    Nothing        -> traceError "BV6"      -- datum not found
                    Just (Scripts.Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just ld' -> (o, ld')
                        Nothing  -> traceError "BV7"       -- error decoding data
            _   -> traceError "BV8"                        -- expected exactly one continuing output

        sigByLotAdmin :: Bool
        sigByLotAdmin =  txSignedBy' info $ Address.unPaymentPubKeyHash (bvpAdminPkh params)
   
        buyTokVal' = bvpBuyTokenValue params
    
        -- Find the buy script address and value by looking for the buy token in the script context inputs
        -- this also validates that the buy token exists as part of the inputs
        buyValAddrIn = findBuyAddrInputs buyTokVal' (txInfoInputs info)
        ticketCost = bvpTicketCost params * 100
       
        -- Get the current total value of the buy contract
        getTotalBuyValue :: Value.Value
        getTotalBuyValue = Ada.lovelaceValueOf (abs(bdTotalBuyValue outputDat)) <> buyTokVal'

        checkBuyTicketOutput :: Bool
        checkBuyTicketOutput = 
            case buyValAddrIn of 
                (Just (scriptAddr, _)) -> validOutputs scriptAddr getTotalBuyValue (txInfoOutputs info)
                Nothing -> False
 
        checkBuyDatum :: Bool
        checkBuyDatum =    (bdTicketTotal outputDat == (abs(bdTicketTotal dat) + 1) )
                        && (bdTotalBuyValue outputDat == (abs(bdTotalBuyValue dat) + ticketCost))
  
        getTotalValue :: Value.Value
        getTotalValue = Ada.lovelaceValueOf (bdTotalBuyValue dat)  <> buyTokVal'

        checkTransferTokenOutput :: Bool
        checkTransferTokenOutput = validOutputs (bvpLottoValAddr params) getTotalValue (txInfoOutputs info)

        -- Commenting out deadline check for now for testing
        --checkDeadline :: Bool
        --checkDeadline = (contains (to $ deadline (bvpDeadline dat)) $ txInfoValidRange info)


-- | Creating a wrapper around buy validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapBuyValidator #-}
wrapBuyValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapBuyValidator params dat red ctx =
   check $ mkBuyValidator (unsafeFromBuiltinData params) (unsafeFromBuiltinData dat) (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx)


untypedBuyValidator :: BuiltinData -> Scripts.Validator
untypedBuyValidator params = Scripts.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapBuyValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
    

-- | We need a typedValidator for offchain mkTxConstraints, so 
-- created it using the untyped validator
typedBuyValidator :: BuiltinData -> TScripts.TypedValidator TypeUtils.Any
typedBuyValidator params =
  Validators.unsafeMkTypedValidator $ untypedBuyValidator params


mkBuyScript :: BuiltinData -> Scripts.Script
mkBuyScript params = unValidatorScript $ untypedBuyValidator params


buyValidator :: BuiltinData -> Scripts.Validator
buyValidator params = TScripts.validatorScript $ typedBuyValidator params


buyHash :: BuiltinData -> Scripts.ValidatorHash
buyHash params = TScripts.validatorHash $ typedBuyValidator params


untypedBuyHash :: BuiltinData -> Scripts.ValidatorHash
untypedBuyHash params = Scripts.validatorHash $ untypedBuyValidator params


-- | Mint a unique NFT representing a lotto thread token and a buy token
mkThreadTokenPolicy :: ThreadTokenRedeemer -> AScriptContext -> Bool
mkThreadTokenPolicy (ThreadTokenRedeemer (Tx.TxOutRef refHash refIdx)) ctx = 
    traceIfFalse "TP1" txOutputSpent        --  UTxO not consumed
    && traceIfFalse "TP2" checkMintedAmount    -- wrong amount minted    
  where
    info :: ATxInfo
    info = aScriptContextTxInfo ctx

    -- True if the pending transaction spends the output
    -- identified by @(refHash, refIdx)@
    txOutputSpent = spendsOutput' info refHash refIdx
    ownSymbol = ownCurrencySymbol' ctx
    minted = txInfoMint info
    threadToken = sha2_256 $ getTxId refHash <> intToBBS refIdx
    buyToken = sha2_256 threadToken

    checkMintedAmount :: Bool
    checkMintedAmount = minted == threadTokenValue ownSymbol (Value.TokenName threadToken) 
                                            <> threadTokenValue ownSymbol (Value.TokenName buyToken)


{-# INLINABLE wrapThreadTokenPolicy #-}
wrapThreadTokenPolicy :: BuiltinData -> BuiltinData -> ()
wrapThreadTokenPolicy redeemer ctx =
   check $ mkThreadTokenPolicy (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx)


threadTokenPolicy :: Scripts.MintingPolicy
threadTokenPolicy = Scripts.mkMintingPolicyScript $
     $$(PlutusTx.compile [|| wrapThreadTokenPolicy ||])


{-# INLINABLE threadTokenCurSymbol #-}
threadTokenCurSymbol :: Value.CurrencySymbol
threadTokenCurSymbol = Contexts.scriptCurrencySymbol threadTokenPolicy


{-# INLINABLE threadTokenValue #-}
threadTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
threadTokenValue cs' tn' = Value.singleton cs' tn' 1


-- | Mint or burn an NFT representing a lottery ticket
--   The buy token value and the buy script address are locked as a parameter
{-# INLINABLE mkTicketPolicy #-}
mkTicketPolicy :: BuiltinData -> BuiltinData -> AScriptContext -> Bool
mkTicketPolicy rawParams rawRedeemer ctx = 
    case polarity' of
        True  ->   traceIfFalse "BP1" checkMintedAmount     -- wrong amount minted
                && traceIfFalse "BP2" checkBuyTokenInput    -- buy token input not found
                && traceIfFalse "BP3" checkBuyTokenOutput   -- buy token output not found
        False ->   traceIfFalse "BP4" checkBurnedAmount     -- wrong amount burned
      where
        info :: ATxInfo
        info = aScriptContextTxInfo ctx

        red = unsafeFromBuiltinData rawRedeemer
        params = unsafeFromBuiltinData rawParams
        polarity' = polarity red

        checkMintedAmount :: Bool
        checkMintedAmount = 
            case Value.flattenValue (txInfoMint info) of
                [(_, tn', amt)] -> tn' == bTicketNum red && amt == 1
                _               -> False

        checkBurnedAmount :: Bool
        checkBurnedAmount = 
            case Value.flattenValue (txInfoMint info) of
                [(_, tn', amt)] -> tn' == bTicketNum red && amt == (-1)
                _               -> False

        checkBuyTokenInput :: Bool
        checkBuyTokenInput = validBuyInputs (tmpBuyValAddr params) (tmpBuyTokenValue params) (txInfoInputs info)

        checkBuyTokenOutput :: Bool
        checkBuyTokenOutput = validBuyOutputs (tmpBuyValAddr params) (tmpBuyTokenValue params) (txInfoOutputs info)


{-# INLINABLE wrapTicketPolicy #-}
wrapTicketPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapTicketPolicy params redeemer ctx =
   check $ mkTicketPolicy (unsafeFromBuiltinData params) (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx)


ticketMintPolicy :: BuiltinData-> Scripts.MintingPolicy
ticketMintPolicy params = Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapTicketPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params


ticketCurSymbol :: BuiltinData -> Value.CurrencySymbol
ticketCurSymbol params = Contexts.scriptCurrencySymbol $ ticketMintPolicy params


{-# INLINABLE ticketMphValue #-}
ticketMphValue :: Scripts.MintingPolicyHash -> Value.TokenName -> Integer -> Value.Value
ticketMphValue mph tn qty = Value.singleton (Value.mpsSymbol mph) tn qty



