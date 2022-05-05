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

module OffChain
    (   initEndpoint
    ,   Lottery (..)
    ,   LottoInitSchema
    ,   LottoUseSchema
    ,   StartParams (..)
    ,   useEndpoint
    ) where

import Control.Lens                                             (review)
import Control.Monad                                            (forever)
import Data.Aeson                                               (FromJSON, ToJSON)
import Data.ByteString qualified as BS                          (append)
import Data.Map                                                 (toList, singleton)
import Data.Monoid                                              (Last (..))
import Data.Text qualified as T                                 (Text)
import GHC.Generics                                             (Generic)
import Ledger.Ada qualified as Ada                              (lovelaceValueOf, adaSymbol, adaToken)
import Ledger.Address qualified as Address                      (PaymentPubKeyHash(..), pubKeyHashAddress, scriptAddress, scriptHashAddress)
import Ledger.Constraints qualified as Constraints              (adjustUnbalancedTx, mintingPolicy, mustBeSignedBy, mustSpendScriptOutput, mustMintValueWithRedeemer,  
                                                                 mustPayToOtherScript, mustPayToTheScript, mustSpendPubKeyOutput, mustPayToPubKey, otherScript,  
                                                                 typedValidatorLookups, unspentOutputs)
import Ledger.Scripts qualified as Scripts                      (Datum(..), mintingPolicyHash, Redeemer(..))
import Ledger.Time qualified as Time                            (POSIXTime)
import Ledger.Tx qualified as Tx                                (ChainIndexTxOut (_ciTxOutValue,_ciTxOutDatum), TxOutRef(..))
import Ledger.TxId as TxId                                      (TxId(..))   
import Ledger.Value qualified as Value                          (CurrencySymbol, flattenValue, split, singleton, tokenName, TokenName(..), valueOf)
import Playground.Contract as Playground                        (ToSchema)
import Plutus.Contract                                          (AsContractError (_ConstraintResolutionError), awaitPromise,  Contract, Endpoint, handleError,
                                                                 mapError,  endpoint, logInfo, logError, select, tell, throwError, type (.\/))
import Plutus.Contract.Request                                  (mkTxContract, ownPaymentPubKeyHash, submitTxConfirmed, utxosAt)
import Plutus.Contract.Wallet as Wallet                         (getUnspentOutput)
import PlutusTx qualified                                       (fromBuiltinData, toBuiltinData)
import PlutusTx.AssocMap qualified as AssocMap                  (delete, empty, lookup, singleton)
import PlutusTx.Prelude                                         (abs, appendByteString, Bool(..), BuiltinByteString, divide, encodeUtf8, fromBuiltin, 
                                                                 Integer, Maybe (..), ($), (==),  (>), (!!), (+), (-), (++), (*), (<>), sha2_256, take)              
import Prelude qualified as Haskell                             (Either(Left, Right), mod, return, Semigroup ((<>)), Show, show)
import Types
import Utils
import OnChain


------------------------------------------------------------------------
-- Off Chain Code
------------------------------------------------------------------------

-- | The Lotter data type is used by the contract to pass key data
--   to other contracts.   This information is used to locate the lottery
--   onchain.
data Lottery = Lottery
    {   lotId                           :: !BuiltinByteString
    ,   lotDifficulty                   :: !Integer
    ,   lotTokenName                    :: !Value.TokenName
    ,   lotPotSplit                     :: !Integer
    } deriving stock    (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON, Playground.ToSchema)


-- | Lotto start initialization and open parameters
data StartParams = StartParams
    { spDifficulty                      :: Integer
    , spAdmin                           :: !Address.PaymentPubKeyHash
    , spSpon                            :: !Address.PaymentPubKeyHash
    , spSponWeight                      :: !Integer
    , spDeadline                        :: !Time.POSIXTime
    , spTicket                          :: !Integer
    , spJackpot                         :: !Integer
    , spFees                            :: !Integer
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)
    

-- | Init contract
initLotto :: StartParams -> Contract (Last Lottery) s T.Text ()
initLotto sp = do

    txOutRef <- Wallet.getUnspentOutput
    let txBS = TxId.getTxId(Tx.txOutRefId txOutRef) <> intToBBS(Tx.txOutRefIdx txOutRef) 
        lottoTokenName  = Value.TokenName $ sha2_256 txBS
        buyTokenName  = Value.TokenName $ sha2_256 $ sha2_256 txBS
        (_, lotTokVal') = Value.split(threadTokenValue threadTokenCurSymbol lottoTokenName)
        (_, buyTokVal') = Value.split(threadTokenValue threadTokenCurSymbol buyTokenName)

    logInfo $ "initLotto: lotto token name= " ++ Haskell.show lottoTokenName
    logInfo $ "initLotto: buy token name= " ++ Haskell.show buyTokenName
    
    ownPkh <- ownPaymentPubKeyHash
    utxo <- utxosAt (Address.pubKeyHashAddress ownPkh Nothing)

    let lottoId' = encodeUtf8(encodeHex txBS) -- use the txBS as the unique lotto id        
        adminPkh' = spAdmin sp
        difficulty' = spDifficulty sp
        potSplit' = spSponWeight sp  
        lvParams = LottoValidatorParams
            {   lvpLottoId              = lottoId'
            ,   lvpDifficulty           = difficulty'
            ,   lvpPotSplit             = potSplit' 
            }
        lotValAddr' = Address.scriptAddress $ lottoValidator $ PlutusTx.toBuiltinData lvParams
        bvParams = BuyValidatorParams
            {   bvpAdminPkh             = adminPkh'
            ,   bvpBuyTokenValue        = buyTokVal'
            ,   bvpLottoTokenValue      = lotTokVal'
            ,   bvpLottoValAddr         = lotValAddr'
            ,   bvpTicketCost           = abs(spTicket sp)
            }
        tmpParams = TicketMintParams
            {   tmpBuyTokenValue        = buyTokVal'
            ,   tmpBuyValAddr           = buyValAddr'
            }
        buyValAddr' = Address.scriptAddress $ buyValidator $ PlutusTx.toBuiltinData bvParams
        mph = Scripts.mintingPolicyHash $ ticketMintPolicy $ PlutusTx.toBuiltinData tmpParams
        lAdmin = LottoAdmin
            {   adminPkh                = adminPkh' 
            ,   sponsorPkh              = spSpon sp
            ,   lottoValAddr            = lotValAddr'
            ,   lottoTokenValue         = lotTokVal'
            ,   buyValAddr              = buyValAddr'
            ,   buyTokenValue           = buyTokVal'
            ,   ticketMph               = mph
            ,   percentFees             = spFees sp
            ,   ticketCost              = abs(spTicket sp)
            ,   difficulty              = difficulty'
            }
        lDat = LottoDat 
            {  a                        = lAdmin
            ,  w                        = [(spSpon sp, [])] -- the sponsor pkh is always the 1st element in list
            ,  j                        = abs(spJackpot sp)
            ,  s                        = 0 
            ,  t                        = 0
            ,  f                        = 0
            ,  b                        = AssocMap.singleton (spSpon sp) 0
            ,  l                        = 0
            ,  h                        = []
            }
        lottery = Lottery
            {   lotId                   = lottoId'
            ,   lotDifficulty           = difficulty'
            ,   lotTokenName            = lottoTokenName
            ,   lotPotSplit             = potSplit'
            }
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ ThreadTokenRedeemer txOutRef
        dat = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript dat (Ada.lovelaceValueOf (spJackpot sp) <> lotTokVal' <> buyTokVal')
            Haskell.<> Constraints.mustMintValueWithRedeemer red (lotTokVal' <> buyTokVal')
            Haskell.<> Constraints.mustSpendPubKeyOutput txOutRef
            Haskell.<> Constraints.mustBeSignedBy ownPkh
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.mintingPolicy threadTokenPolicy 
            Haskell.<> Constraints.unspentOutputs utxo

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx

    logInfo $ "initLotto: tx submitted successfully= " ++ Haskell.show adjustedUtx
    logInfo $ "initLotto: Lottery= " ++ Haskell.show lottery
    logInfo $ "initLotto: LotteryDat= " ++ Haskell.show lDat
    logInfo $ "initLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    tell $ Last $ Just lottery


-- | Find the lotto validator onchain using the lotto params and the threadtoken
findLottery :: LottoValidatorParams -> Value.CurrencySymbol -> Value.TokenName -> Contract w s T.Text (Tx.TxOutRef, Tx.ChainIndexTxOut, LottoDat)
findLottery params cs tn = do
    
    utxos <- utxosAt $ Address.scriptHashAddress $ lottoHash $ PlutusTx.toBuiltinData params
    let xs = [ (oref, o)
             | (oref, o) <- toList utxos
             , Value.valueOf (Tx._ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case Tx._ciTxOutDatum o of
            Haskell.Left _          -> throwError "findLottery: datum missing"
            Haskell.Right (Scripts.Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "findLottery: datum has wrong type"
                Just d@LottoDat{} -> Haskell.return (oref, o, d)
        _           -> throwError "findLottery: lotto utxo not found"


-- | Open contract
openLotto :: Lottery -> StartParams -> Contract w s T.Text ()
openLotto lot sp = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "openLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "openLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "openLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    -- the lotto admin pkh may change during open, if so then need to 
    -- recalc the buy validator because the adminPkh is a parameter to 
    -- the buy validator
    let adminPkh' = spAdmin sp
        sponsorPkh'   = sponsorPkh (a ld)
        buyTokVal' = buyTokenValue (a ld)
        lotValAddr' = lottoValAddr (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyValAddr' = Address.scriptAddress $ buyValidator $ PlutusTx.toBuiltinData bvParams
        bvParams = BuyValidatorParams
            {   bvpAdminPkh             = adminPkh'
            ,   bvpBuyTokenValue        = buyTokVal'
            ,   bvpLottoTokenValue      = lotTokVal'
            ,   bvpLottoValAddr         = lotValAddr'
            ,   bvpTicketCost           = abs(spTicket sp)
            } 
        lAdmin = LottoAdmin
            {   adminPkh                = adminPkh' 
            ,   sponsorPkh              = sponsorPkh (a ld) -- can't change benificiary until governance is implemented
            ,   lottoValAddr            = lottoValAddr (a ld)
            ,   lottoTokenValue         = lottoTokenValue (a ld)
            ,   buyValAddr              = buyValAddr'
            ,   buyTokenValue           = buyTokenValue (a ld)
            ,   ticketMph               = ticketMph   (a ld)
            ,   percentFees             = spFees sp
            ,   ticketCost              = abs(spTicket sp)
            ,   difficulty              = difficulty (a ld)
            }
        fromTreasury = if j ld > t ld then 0
                        else divide (t ld) 2
        newTreasury = if j ld > t ld then t ld
                        else divide (t ld) 2
        lDat = LottoDat 
              {  a                      = lAdmin
              ,  w                      = [(sponsorPkh', [])] -- the sponsor pkh is always the 1st element in list
              ,  j                      = j ld + abs(spJackpot sp) + fromTreasury
              ,  s                      = Haskell.mod (s ld + 1) 255
              ,  t                      = newTreasury
              ,  f                      = f ld
              ,  b                      = AssocMap.singleton sponsorPkh' 0  -- initialize the benificary map
              ,  l                      = 1
              ,  h                      = []
              }
        totalValue = j lDat + t lDat + f lDat
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Open
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustBeSignedBy adminPkh'
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "openLotto: tx submitted= " ++ Haskell.show adjustedUtx


-- | StartBuy contract
startBuy :: Lottery -> Contract w s T.Text ()
startBuy lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "startBuy: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "startBuy: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "startBuy: lotto validator hash= " ++ (Haskell.show $ lottoHash $ PlutusTx.toBuiltinData lvParams)
  
    let lDat = LottoDat 
              {  a                      = a ld
              ,  w                      = w ld
              ,  j                      = j ld
              ,  s                      = s ld
              ,  t                      = t ld
              ,  f                      = f ld
              ,  b                      = b ld
              ,  l                      = l ld
              ,  h                      = h ld
              }
        totalValue = j lDat + t lDat + f lDat
        bDat = BuyDat 
              {  bdTicketTotal          = 0
              ,  bdTotalBuyValue        = 2000000 -- min Ada to lock buy token at the script address
              }
        adminPkh' = adminPkh (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        lotValAddr' = lottoValAddr (a ld)
        buyTokVal' = buyTokenValue (a ld)
        ticketCost' = ticketCost (a ld)
        bvParams = BuyValidatorParams
            {   bvpAdminPkh             = adminPkh'
            ,   bvpBuyTokenValue        = buyTokVal'
            ,   bvpLottoTokenValue      = lotTokVal'
            ,   bvpLottoValAddr         = lotValAddr'
            ,   bvpTicketCost           = ticketCost'
            }
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ StartBuy
        datBuy = Scripts.Datum $ PlutusTx.toBuiltinData bDat
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal')
            Haskell.<> Constraints.mustPayToOtherScript (buyHash $ PlutusTx.toBuiltinData bvParams) datBuy ((Ada.lovelaceValueOf $ bdTotalBuyValue bDat) Haskell.<> buyTokVal') 
            Haskell.<> Constraints.mustSpendScriptOutput oref red
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "startBuy: tx submitted= " ++ Haskell.show adjustedUtx

-- | Find the buy validator onchain using the buy params and buy token
findBuyValidator :: BuyValidatorParams -> Value.CurrencySymbol -> Value.TokenName -> Contract w s T.Text (Tx.TxOutRef, Tx.ChainIndexTxOut, BuyDat)
findBuyValidator params cs tn = do
    
    utxos <- utxosAt $ Address.scriptHashAddress $ buyHash $ PlutusTx.toBuiltinData params
    let xs = [ (oref, o)
             | (oref, o) <- toList utxos
             , Value.valueOf (Tx._ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case Tx._ciTxOutDatum o of
            Haskell.Left _          -> throwError "findBuy: datum missing"
            Haskell.Right (Scripts.Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "findBuy: datum has wrong type"
                Just d@BuyDat{} -> Haskell.return (oref, o, d)
        _           -> throwError "findBuy: lotto utxo not found"


-- | Buy contract
buyLotto :: Lottery -> Integer -> Contract w s T.Text ()
buyLotto lot ticketNum = do
 
    let tt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot 
             }
       
    (oref, _, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol tt
    logInfo $ "buyTicket: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "buyTicket: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "buyTicket: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)
    logInfo $ "buyTicket: ticket number= " ++ Haskell.show ticketNum
    
    let adminPkh' = adminPkh (a ld)
        lotValAddr' = lottoValAddr (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyValAddr' = buyValAddr (a ld)
        buyTokVal' = buyTokenValue (a ld)
        ticketCost' = ticketCost (a ld)
        bvParams = BuyValidatorParams
            {   bvpAdminPkh             = adminPkh'
            ,   bvpBuyTokenValue        = buyTokVal'
            ,   bvpLottoTokenValue      = lotTokVal'
            ,   bvpLottoValAddr         = lotValAddr'
            ,   bvpTicketCost           = ticketCost'
            }
        tmpParams = TicketMintParams
            {   
                tmpBuyTokenValue        = buyTokVal'
            ,   tmpBuyValAddr           = buyValAddr'
            }
        (buyCs, buyTn, _) = (Value.flattenValue buyTokVal')!!0

    (buyOref, buyOutput, bd@BuyDat{}) <- findBuyValidator bvParams buyCs buyTn
    logInfo $ "buyTicket: found buy utxo buyOref= " ++ Haskell.show buyOref
    logInfo $ "buyTicket: buy validator buyHash= " ++ Haskell.show (buyHash $ PlutusTx.toBuiltinData bvParams)

    let bDat = BuyDat 
            {  bdTicketTotal            = bdTicketTotal bd + 1
            ,  bdTotalBuyValue          = bdTotalBuyValue bd + (ticketCost' * 100)
            }
        ticketNumBS  = strToBS(Haskell.show ticketNum)
        s' = fromBuiltin(intToBBS(s ld))
        tn = Value.tokenName(BS.append s' ticketNumBS)
        buyMintRed = MintTicketRedeemer
            {
                polarity                = True
            ,   bTicketNum              = tn
            }
        totalValue = bdTotalBuyValue bDat
        redBuy = Scripts.Redeemer $ PlutusTx.toBuiltinData BuyTicket
        redMint = Scripts.Redeemer $ PlutusTx.toBuiltinData buyMintRed
        datBuy = PlutusTx.toBuiltinData bDat
        valMint = Value.singleton (ticketCurSymbol $ PlutusTx.toBuiltinData tmpParams) tn 1
        constraints = Constraints.mustPayToTheScript datBuy (Ada.lovelaceValueOf totalValue 
                                                            Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustMintValueWithRedeemer redMint valMint
            Haskell.<> Constraints.mustSpendScriptOutput buyOref redBuy
        lookups = Constraints.typedValidatorLookups (typedBuyValidator $ PlutusTx.toBuiltinData bvParams)
            Haskell.<> Constraints.otherScript (buyValidator $ PlutusTx.toBuiltinData bvParams)
            Haskell.<> Constraints.mintingPolicy (ticketMintPolicy $ PlutusTx.toBuiltinData tmpParams)
            Haskell.<> Constraints.unspentOutputs (singleton buyOref buyOutput)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "buyTicket: tx submitted= " ++ Haskell.show adjustedUtx


-- | TransferToken contract
transferToken :: Lottery -> Contract w s T.Text ()
transferToken lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot 
             }
       
    (oref, _, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "transferToken: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "transferToken: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "transferToken: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    let adminPkh' = adminPkh (a ld)
        lotValAddr' = lottoValAddr (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)
        ticketCost' = ticketCost (a ld)
        bvParams = BuyValidatorParams
            {   bvpAdminPkh             = adminPkh'
            ,   bvpBuyTokenValue        = buyTokVal'
            ,   bvpLottoTokenValue      = lotTokVal'
            ,   bvpLottoValAddr         = lotValAddr'
            ,   bvpTicketCost           = ticketCost'
            }
        (buyCs, buyTn, _) = (Value.flattenValue buyTokVal')!!0

    (buyOref, buyOutput, bd@BuyDat{}) <- findBuyValidator bvParams buyCs buyTn
    logInfo $ "buyTicket: found buy utxo buyOref= " ++ Haskell.show buyOref
    logInfo $ "buyTicket: buy validator buyHash= " ++ Haskell.show (buyHash $ PlutusTx.toBuiltinData bvParams)
    logInfo $ "buyTicket: buy datum= " ++ Haskell.show bd

    let totalBuyValue = bdTotalBuyValue bd
        redBuy = Scripts.Redeemer $ PlutusTx.toBuiltinData TransferToken
        datLotto = PlutusTx.toBuiltinData ld
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalBuyValue
                                                              Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustSpendScriptOutput buyOref redBuy
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (buyValidator $ PlutusTx.toBuiltinData bvParams)
            Haskell.<> Constraints.unspentOutputs (singleton buyOref buyOutput)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "transferToken: tx submitted= " ++ Haskell.show adjustedUtx


-- | Find the Lotto validator contract that has the buy token locked at that script address
--   We need to find that utxo to so we can spend it, as well as the Ada value too.   This utxo
--   represents the amount of Ada that was collected during the minting of tickets during the buy
--   phase.
findLottoBuy :: LottoValidatorParams -> Value.CurrencySymbol -> Value.TokenName -> Contract w s T.Text (Tx.TxOutRef, Tx.ChainIndexTxOut, LottoDat)
findLottoBuy params cs tn = do
    
    utxos <- utxosAt $ Address.scriptHashAddress $ lottoHash $ PlutusTx.toBuiltinData params
    let xs = [ (oref, o)
             | (oref, o) <- toList utxos
             , Value.valueOf (Tx._ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case Tx._ciTxOutDatum o of
            Haskell.Left _          -> throwError "findLottoBuy: datum missing"
            Haskell.Right (Scripts.Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "findLottoBuy: datum has wrong type"
                Just d@LottoDat{} -> Haskell.return (oref, o, d)
        _           -> throwError "findLottoBuy: lotto utxo not found"


-- | StopBuy contract
stopBuy :: Lottery -> Contract w s T.Text ()
stopBuy lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId           = lotId lot
             ,   lvpDifficulty        = lotDifficulty lot
             ,   lvpPotSplit          = lotPotSplit lot 
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "stopBuy: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "stopBuy: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "stopBuy: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    let perctFee = percentFees (a ld)   
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)
        (buyCs, buyTn, _) = (Value.flattenValue buyTokVal')!!0

    (buyOref, buyOutput, _) <- findLottoBuy lvParams buyCs buyTn
    logInfo $ "stopBuy: found buy datum utxo buyOref= " ++ Haskell.show buyOref

    let totalBuyValue = Value.valueOf (Tx._ciTxOutValue buyOutput) Ada.adaSymbol Ada.adaToken -- get buy total value from chain index query
        lDat = LottoDat 
              {  a                      = a ld
              ,  w                      = w ld
              ,  j                      = j ld + divide (totalBuyValue * abs(divide (100 - perctFee) 2)) 100
              ,  s                      = s ld
              ,  t                      = t ld + divide (totalBuyValue * abs(divide (100 - perctFee) 2)) 100
              ,  f                      = f ld + divide (totalBuyValue * perctFee) 100
              ,  b                      = b ld
              ,  l                      = l ld
              ,  h                      = h ld
              }
        totalValue = j lDat + t lDat + f lDat
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData StopBuy
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
            Haskell.<> Constraints.mustSpendScriptOutput buyOref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)
            Haskell.<> Constraints.unspentOutputs (singleton buyOref buyOutput)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "stopBuy: tx submitted= " ++ Haskell.show adjustedUtx


-- | Close contract
closeLotto :: Lottery -> Contract w s T.Text ()
closeLotto lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId           = lotId lot
             ,   lvpDifficulty        = lotDifficulty lot
             ,   lvpPotSplit          = lotPotSplit lot 
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "closeLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "closeLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "closeLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    let lDat = LottoDat 
              {  a                      = a ld
              ,  w                      = w ld 
              ,  j                      = j ld
              ,  s                      = s ld 
              ,  t                      = t ld
              ,  f                      = f ld
              ,  b                      = b ld
              ,  l                      = 2
              ,  h                      = h ld
              }
        adminPkh' = adminPkh (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)             
        totalValue = j ld + t ld + f ld
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Close
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustBeSignedBy adminPkh'
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "closeLotto: tx submitted= " ++ Haskell.show adjustedUtx


-- | Draw contract
drawLotto :: Lottery -> Contract w s T.Text ()
drawLotto lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot 
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "drawLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "clostTxCheckLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "clostTxCheckLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    -- validate and store the close Tx used for the winning number generation
    let txIdRef = Tx.txOutRefId oref
        txId    = getTxId $ Tx.txOutRefId oref
        txIdInt = getHexInt txId
        lDat = LottoDat 
              {  a                      = a ld
              ,  w                      = w ld 
              ,  j                      = j ld
              ,  s                      = s ld 
              ,  t                      = t ld
              ,  f                      = f ld
              ,  b                      = b ld
              ,  l                      = 3
              ,  h                      = txIdInt
              }            
        adminPkh' = adminPkh (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)
        totalValue = j ld + t ld + f ld
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Draw txId
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustBeSignedBy adminPkh'
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    logInfo $ "drawLotto: txIdRef= " ++ Haskell.show txIdRef
    logInfo $ "drawLotto: txId= " ++ Haskell.show txId
    logInfo $ "drawLotto: txIdInt= " ++ Haskell.show txIdInt
    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "drawLotto: tx submitted= " ++ Haskell.show adjustedUtx


-- | Redeem contract
redeemLotto :: Lottery -> Contract w s T.Text ()
redeemLotto lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot 
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "redeemLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "redeemLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "redeemLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    ownPkh <- ownPaymentPubKeyHash

    let diff = lotDifficulty lot
        winNums = take diff (h ld)
        winNum = intsToBS winNums
        seq = intToBBS(s ld)
        tn = Value.TokenName (appendByteString seq winNum)
        lDat = LottoDat 
              {  a                      = a ld 
              ,  w                      = w ld ++ [(ownPkh, winNums)]
              ,  j                      = j ld
              ,  s                      = s ld 
              ,  t                      = t ld
              ,  f                      = f ld
              ,  b                      = b ld
              ,  l                      = l ld 
              ,  h                      = h ld
              }
        lotTokVal' = lottoTokenValue (a ld)
        --buyValAddr' = buyValAddr (a ld)
        buyTokVal' = buyTokenValue (a ld)
        --buyMintRed = MintTicketRedeemer
        --    {
        --        polarity = False  -- burn the lotto token if the player has one
        --    ,   bTicketNum  = tn
        --    }
        --tmpParams = TicketMintParams
        --    {   
        --        tmpBuyTokenValue        = buyTokVal'
        --    ,   tmpBuyValAddr           = buyValAddr'
        --    }
        totalValue = j ld + t ld + f ld
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Redeem tn
        --redMint = Scripts.Redeemer $ PlutusTx.toBuiltinData buyMintRed
        datLotto = PlutusTx.toBuiltinData lDat

        -- commenting out burnning of ticket for now due to going over max tx size limits
        -- once vasil hard fork arrives, we should be able to re-enable.
        -- this is nice to have when there is only one winner, but is required
        -- for multiple winner scenarios  
        --valMint = Value.singleton (ticketCurSymbol $ PlutusTx.toBuiltinData tmpParams) tn (-1)
        ticketValue = minAda <> ticketMphValue (ticketMph (a ld)) tn 1
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            --Haskell.<> Constraints.mustMintValueWithRedeemer redMint valMint
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
            Haskell.<> Constraints.mustPayToPubKey ownPkh ticketValue --only needed if not burning the ticket 
            Haskell.<> Constraints.mustBeSignedBy ownPkh  
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            --Haskell.<> Constraints.mintingPolicy (ticketMintPolicy $ PlutusTx.toBuiltinData tmpParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "redeemLotto: tx submitted= " ++ Haskell.show adjustedUtx


-- | CalcPayout contract
calcPayoutLotto :: Lottery -> Contract w s T.Text ()
calcPayoutLotto lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "calcPayoutLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "calcPayoutLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "calcPayoutLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    let lDat = LottoDat 
              {  a                      = a ld 
              ,  w                      = w ld
              ,  j                      = j ld
              ,  s                      = s ld 
              ,  t                      = t ld
              ,  f                      = f ld
              ,  b                      = calcPayouts (j ld) (w ld) (lotPotSplit lot)
              ,  l                      = 4
              ,  h                      = h ld
              }
        adminPkh' = adminPkh (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)
        totalValue = j ld + t ld + f ld
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Calc
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustBeSignedBy adminPkh'
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "calcPayoutLotto: tx submitted= " ++ Haskell.show adjustedUtx


-- | Payout contract
payoutLotto :: Lottery -> Contract w s T.Text ()
payoutLotto lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId           = lotId lot
             ,   lvpDifficulty        = lotDifficulty lot
             ,   lvpPotSplit          = lotPotSplit lot 
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    ownPkh <- ownPaymentPubKeyHash
    logInfo $ "payoutLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "payoutLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "payoutLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)
    logInfo $ "payoutLotto: ownPkh= " ++ Haskell.show ownPkh

    let payout = getInteger(AssocMap.lookup ownPkh (b ld))
        lDat = LottoDat 
              {  a                      = a ld 
              ,  w                      = w ld 
              ,  j                      = j ld - payout
              ,  s                      = s ld 
              ,  t                      = t ld
              ,  f                      = f ld
              ,  b                      = AssocMap.delete ownPkh (b ld)
              ,  l                      = l ld
              ,  h                      = h ld
              }
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)
        totalValue = j lDat + t ld + f ld
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Payout
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustBeSignedBy ownPkh
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "payoutLotto: tx submitted= " ++ Haskell.show adjustedUtx


-- | End contract
endLotto :: Lottery -> Contract w s T.Text ()
endLotto lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "endLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "endLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "endLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    let lDat = LottoDat 
              {  a                      = a ld
              ,  w                      = [] 
              ,  j                      = j ld
              ,  s                      = s ld 
              ,  t                      = t ld
              ,  f                      = f ld
              ,  b                      = AssocMap.empty
              ,  l                      = 5
              ,  h                      = []
              }
        adminPkh' = adminPkh (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)
        totalValue = j ld + t ld + f ld
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ End
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustBeSignedBy adminPkh'
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "endLotto: tx submitted= " ++ Haskell.show adjustedUtx


-- | Collect contract
collectLotto :: Lottery -> Contract w s T.Text ()
collectLotto lot = do
 
    let lt = lotTokenName lot
        lvParams = LottoValidatorParams
             {   lvpLottoId             = lotId lot
             ,   lvpDifficulty          = lotDifficulty lot
             ,   lvpPotSplit            = lotPotSplit lot 
             }
       
    (oref, o, ld@LottoDat{}) <- findLottery lvParams threadTokenCurSymbol lt
    logInfo $ "collectLotto: found lotto utxo with datum= " ++ Haskell.show ld
    logInfo $ "collectLotto: found lotto utxo oref= " ++ Haskell.show oref
    logInfo $ "collectLotto: lotto validator hash= " ++ Haskell.show (lottoHash $ PlutusTx.toBuiltinData lvParams)

    let lDat = LottoDat 
              {  a                      = a ld 
              ,  w                      = w ld
              ,  j                      = j ld
              ,  s                      = s ld 
              ,  t                      = t ld
              ,  f                      = 0
              ,  b                      = b ld
              ,  l                      = l ld
              ,  h                      = h ld
              }
        adminPkh' = adminPkh (a ld)
        lotTokVal' = lottoTokenValue (a ld)
        buyTokVal' = buyTokenValue (a ld)
        totalValue = j ld + t ld
        redLotto = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Collect
        datLotto = PlutusTx.toBuiltinData lDat
        constraints = Constraints.mustPayToTheScript datLotto (Ada.lovelaceValueOf totalValue 
                                                              Haskell.<> lotTokVal' Haskell.<> buyTokVal')
            Haskell.<> Constraints.mustBeSignedBy adminPkh'
            Haskell.<> Constraints.mustSpendScriptOutput oref redLotto
        lookups = Constraints.typedValidatorLookups (typedLottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.otherScript (lottoValidator $ PlutusTx.toBuiltinData lvParams)
            Haskell.<> Constraints.unspentOutputs (singleton oref o)

    utx <- mapError (review _ConstraintResolutionError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx
    logInfo $ "collectLotto: tx submitted= " ++ Haskell.show adjustedUtx


type LottoInitSchema =
        Endpoint "init" StartParams

type LottoUseSchema =
        Endpoint "open" (Lottery, StartParams)
    .\/ Endpoint "startbuy" Lottery
    .\/ Endpoint "buy"  (Lottery, Integer)
    .\/ Endpoint "transfertoken" Lottery
    .\/ Endpoint "stopbuy" Lottery
    .\/ Endpoint "close" Lottery
    .\/ Endpoint "draw" Lottery
    .\/ Endpoint "redeem" Lottery
    .\/ Endpoint "calc" Lottery
    .\/ Endpoint "payout" Lottery
    .\/ Endpoint "end" Lottery
    .\/ Endpoint "collect" Lottery
    

initEndpoint :: Contract (Last Lottery) LottoInitSchema T.Text ()
initEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"init" $ \sp -> initLotto sp



useEndpoint :: Contract () LottoUseSchema T.Text ()
useEndpoint = forever $ handleError logError $ awaitPromise $ open `select` 
                                                            startbuy `select` 
                                                            buy `select` 
                                                            transfertoken `select` 
                                                            stopbuy `select` 
                                                            close `select` 
                                                            draw `select` 
                                                            redeem `select` 
                                                            calc `select` 
                                                            payout `select` 
                                                            end `select` 
                                                            collect
  where
    open            = endpoint @"open"          $ \(lot, sp) -> openLotto lot sp
    startbuy        = endpoint @"startbuy"      $ \lot       -> startBuy lot
    buy             = endpoint @"buy"           $ \(lot, tn) -> buyLotto lot tn
    transfertoken   = endpoint @"transfertoken" $ \lot       -> transferToken lot
    stopbuy         = endpoint @"stopbuy"       $ \lot       -> stopBuy lot
    close           = endpoint @"close"         $ \lot       -> closeLotto lot
    draw            = endpoint @"draw"          $ \lot       -> drawLotto lot
    redeem          = endpoint @"redeem"        $ \lot       -> redeemLotto lot
    calc            = endpoint @"calc"          $ \lot       -> calcPayoutLotto lot
    payout          = endpoint @"payout"        $ \lot       -> payoutLotto lot
    end             = endpoint @"end"           $ \lot       -> endLotto lot
    collect         = endpoint @"collect"       $ \lot       -> collectLotto lot

