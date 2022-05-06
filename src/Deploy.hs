{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Deploy
    ( main
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley                (PlutusScript (..))
import           Codec.Serialise                    (serialise)
import           Data.Aeson                         (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger                           
import           Ledger.Scripts as Scripts
import qualified Ledger.Value as Value 
import           PlutusTx                           (Data (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Prelude as PlutusPrelude 
import           Types
import           Utils
import           OnChain


-------------------------------------------------------------------------------------
-- START - Lotto Admin Configuration Settings
-------------------------------------------------------------------------------------
-- These are test values and need to be replaced with real values for
-- the appropriate enviornment (eg devnet, testnet or mainnet)

lottoAdminPubKeyHashBS :: B.ByteString
lottoAdminPubKeyHashBS = "4ccdbf08ac0bd876fc9cf1ac03ed86ceef4052cde7a4989c59e87819"

sponsorPubKeyHashBS :: B.ByteString
sponsorPubKeyHashBS = "84314878b6fdc6a65b3c98983a2bab8e2714e5d90f4e8aa5ae9f0268"

txIdBS :: B.ByteString
txIdBS = "9bf48a110b5f5620f90bf484dfadba3ccabe4ae2bc760d9f01a19dc447c27b8f"

txIdIdxInt :: Integer
txIdIdxInt = 0

difficultyInt :: Integer
difficultyInt = 1

jackpotSplitInt :: Integer
jackpotSplitInt = 50  -- sponsor split needs to be between 0 and 100

ticketCostInt :: Integer
ticketCostInt = 20000  -- the actual ticket cost is 20,000 * 100 = 2,000,000 or 2 ADA

percentFeesInt :: Integer
percentFeesInt = 2

-------------------------------------------------------------------------------------
-- END - Lotto Admin Configuration Settings
-------------------------------------------------------------------------------------

-- Dummy place holder value
buyTokenBS :: PlutusPrelude.BuiltinByteString
buyTokenBS = "15"

-------------------------------------------------------------------------------------
-- START - Derived values from source code
-------------------------------------------------------------------------------------

lottoTokenName :: Value.TokenName
lottoTokenName  = Value.TokenName $ PlutusPrelude.sha2_256 $ decodeHex txIdBS <> intToBBS txIdIdxInt

buyTokenName :: Value.TokenName
buyTokenName  = Value.TokenName $ PlutusPrelude.sha2_256 $ PlutusPrelude.sha2_256 $ decodeHex txIdBS <> intToBBS txIdIdxInt

lotTokVal' :: Value.Value
(_, lotTokVal') = Value.split(threadTokenValue threadTokenCurSymbol lottoTokenName)

buyTokVal' :: Value.Value
(_, buyTokVal') = Value.split(threadTokenValue threadTokenCurSymbol buyTokenName)

lottoAdminPaymentPkh :: Ledger.PaymentPubKeyHash
lottoAdminPaymentPkh = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex lottoAdminPubKeyHashBS)

sponsorPaymentPkh :: Ledger.PaymentPubKeyHash
sponsorPaymentPkh = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex sponsorPubKeyHashBS)

lottoId' :: PlutusPrelude.BuiltinByteString
lottoId' = decodeHex txIdBS -- use the txBS as the unique lotto id

lvParams :: LottoValidatorParams
lvParams = LottoValidatorParams
    { lvpLottoId = lottoId'
    , lvpDifficulty = difficultyInt
    , lvpPotSplit = jackpotSplitInt
    }

lotValAddr' :: Ledger.Address
lotValAddr' = Ledger.scriptAddress$ untypedLottoValidator $ PlutusTx.toBuiltinData lvParams

bvParams :: BuyValidatorParams
bvParams = BuyValidatorParams
    { bvpAdminPkh = lottoAdminPaymentPkh 
    , bvpBuyTokenValue = buyTokVal'
    , bvpLottoTokenValue = lotTokVal'
    , bvpLottoValAddr = lotValAddr'
    , bvpTicketCost = ticketCostInt
    }

buyValAddr' :: Ledger.Address
buyValAddr' = Ledger.scriptAddress $ untypedBuyValidator $ PlutusTx.toBuiltinData bvParams


tmpParams :: TicketMintParams
tmpParams = TicketMintParams
    { tmpBuyTokenValue = buyTokVal'
    , tmpBuyValAddr = buyValAddr'
    }

mph :: MintingPolicyHash
mph = Scripts.mintingPolicyHash $ ticketMintPolicy $ PlutusTx.toBuiltinData tmpParams

-------------------------------------------------------------------------------------
-- END - Derived values from source code
-------------------------------------------------------------------------------------

main::IO ()
main = do

    -- generate plutus scripts and hashes
    _ <- writeLottoValidator
    writeLottoValidatorHash
    _ <- writeBuyValidator
    writeBuyValidatorHash
    _ <- writeThreadTokenPolicy
    writeThreadTokenPolicyHash
    _ <- writeTicketPolicy
    writeTicketPolicyHash
    
    -- generated inital datums and values
    writeDatumInit
    writeDatumStartBuy
    writeLottoTokenName
    writeBuyTokenName 
    writeLottoTokenValue
    writeBuyTokenValue
    writeLottoValAddr
    writeBuyValAddr
    writeJackpotSplit    
  
    -- generate redeemers
    writeRedeemerInit
    writeRedeemerOpen
    writeRedeemerStartBuy
    writeRedeemerBuyTicket
    writeRedeemerTicketMint
    writeRedeemerStopBuy
    writeRedeemerTransferToken
    writeRedeemerClose
    writeRedeemerDraw
    writeRedeemerLottoRedeem
    writeRedeemerTicketBurn
    writeRedeemerCalc
    writeRedeemerPayout
    writeRedeemerEnd
    writeRedeemerCollect

    return ()
 
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x', dataToScriptData y) | (x', y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData


writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript 


writeRedeemerInit :: IO ()
writeRedeemerInit = 

    let txOutRef' = Ledger.TxOutRef
            {
                Ledger.txOutRefId = Ledger.TxId
                {
                    Ledger.getTxId = decodeHex txIdBS
                } 
            ,   Ledger.txOutRefIdx = txIdIdxInt
            }
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ ThreadTokenRedeemer txOutRef'
    in
        writeJSON "deploy/redeemer-thread-token-mint.json" red


writeRedeemerOpen :: IO ()
writeRedeemerOpen = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Open
    in
        writeJSON "deploy/redeemer-lotto-open.json" red


writeRedeemerStartBuy :: IO ()
writeRedeemerStartBuy = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ StartBuy
    in
        writeJSON "deploy/redeemer-lotto-startbuy.json" red


writeRedeemerBuyTicket :: IO ()
writeRedeemerBuyTicket = 
    let red  = Scripts.Redeemer $ PlutusTx.toBuiltinData BuyTicket
    in
        writeJSON "deploy/redeemer-buy-ticket.json" red
  
        
writeRedeemerTicketMint :: IO ()
writeRedeemerTicketMint = 
    let tn' = Value.TokenName buyTokenBS
        bMintRed = MintTicketRedeemer
            {
                polarity = True
            ,   bTicketNum  = tn'
            }
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ bMintRed
    in
        writeJSON "deploy/redeemer-ticket-mint.json" red


writeRedeemerStopBuy :: IO ()
writeRedeemerStopBuy = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ StopBuy
    in
        writeJSON "deploy/redeemer-lotto-stopbuy.json" red


writeRedeemerTransferToken :: IO ()
writeRedeemerTransferToken = 
    let red  = Scripts.Redeemer $ PlutusTx.toBuiltinData TransferToken
    in
        writeJSON "deploy/redeemer-buy-transfertoken.json" red
  
     
writeRedeemerClose :: IO ()
writeRedeemerClose = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Close
    in
        writeJSON "deploy/redeemer-lotto-close.json" red


writeRedeemerDraw :: IO ()
writeRedeemerDraw = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Draw $ decodeHex txIdBS
    in
        writeJSON "deploy/redeemer-lotto-draw.json" red


writeRedeemerLottoRedeem :: IO ()
writeRedeemerLottoRedeem = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Redeem $ Value.TokenName buyTokenBS
    in
        writeJSON "deploy/redeemer-lotto-redeem.json" red


writeRedeemerTicketBurn :: IO ()
writeRedeemerTicketBurn = 
    let buyMintRed = MintTicketRedeemer
            {
                polarity = False  -- burn the lotto token if the player has one
            ,   bTicketNum  = Value.TokenName buyTokenBS
            }
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData buyMintRed
    in
        writeJSON "deploy/redeemer-ticket-burn.json" red


writeRedeemerCalc :: IO ()
writeRedeemerCalc = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Calc
    in
        writeJSON "deploy/redeemer-lotto-calc.json" red


writeRedeemerPayout :: IO ()
writeRedeemerPayout = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Payout
    in
        writeJSON "deploy/redeemer-lotto-payout.json" red


writeRedeemerEnd :: IO ()
writeRedeemerEnd = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ End
    in
        writeJSON "deploy/redeemer-lotto-end.json" red


writeRedeemerCollect :: IO ()
writeRedeemerCollect = 
    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ Collect
    in
        writeJSON "deploy/redeemer-lotto-collect.json" red


writeDatumInit :: IO ()
writeDatumInit = 
    let lAdmin = LottoAdmin
            { adminPkh = lottoAdminPaymentPkh  
            , sponsorPkh = sponsorPaymentPkh
            , lottoValAddr = lotValAddr'
            , lottoTokenValue = lotTokVal'
            , buyValAddr = buyValAddr'
            , buyTokenValue = buyTokVal'
            , ticketMph = mph
            , percentFees = percentFeesInt
            , ticketCost = ticketCostInt
            , difficulty = difficultyInt
            --, deadline = spDeadline sp
            }

        lDatum = LottoDatum 
            { lottoAdmin  = lAdmin
            , winner = [(sponsorPaymentPkh,[])] -- the sponsor pkh is always the 1st element in list
            , jackpot = 10000000
            , seqNum = 0 
            , treasury = 0
            , fees = 0
            , beneficiaries = AssocMap.singleton sponsorPaymentPkh 0
            , lottoState = 0
            , winNums = []
            }

        dat = PlutusTx.toBuiltinData lDatum

    in 
        writeJSON "deploy/lotto-datum-init.datum" dat


writeDatumStartBuy :: IO ()
writeDatumStartBuy = 

    let bDat = BuyDat 
              { bdTicketTotal = 0
              , bdTotalBuyValue = 2000000  -- min ada required to lock token at script address
              }

        dat = PlutusTx.toBuiltinData bDat

    in 
        writeJSON "deploy/buy-datum-startbuy.datum" dat


writeLottoTokenName :: IO ()
writeLottoTokenName = writeJSON "deploy/lotto-token-name.json" lottoTokenName

writeBuyTokenName :: IO ()
writeBuyTokenName = writeJSON "deploy/buy-token-name.json" buyTokenName

writeLottoTokenValue :: IO ()
writeLottoTokenValue = writeJSON "deploy/lotto-token-value.json" lotTokVal'

writeBuyTokenValue :: IO ()
writeBuyTokenValue = writeJSON "deploy/buy-token-value.json" buyTokVal'

writeLottoValAddr :: IO ()
writeLottoValAddr = writeJSON "deploy/lotto-val-addr.json" lotValAddr'

writeBuyValAddr :: IO ()
writeBuyValAddr = writeJSON "deploy/buy-val-addr.json" buyValAddr'

writeJackpotSplit :: IO ()
writeJackpotSplit  = writeJSON "deploy/jackpot-split.json" jackpotSplitInt

writeLottoValidator :: IO (Either (FileError ()) ())
writeLottoValidator = writeValidator "deploy/lotto-validator.plutus" $ untypedLottoValidator $ PlutusTx.toBuiltinData lvParams

writeLottoValidatorHash :: IO ()
writeLottoValidatorHash = writeJSON "deploy/lotto-validator.hash" $ PlutusTx.toBuiltinData $ untypedLottoHash $ PlutusTx.toBuiltinData lvParams

writeBuyValidator :: IO (Either (FileError ()) ())
writeBuyValidator = writeValidator "deploy/buy-validator.plutus" $ untypedBuyValidator $ PlutusTx.toBuiltinData bvParams

writeBuyValidatorHash :: IO ()
writeBuyValidatorHash = writeJSON "deploy/buy-validator.hash" $ PlutusTx.toBuiltinData $ untypedBuyHash $ PlutusTx.toBuiltinData bvParams

writeThreadTokenPolicyHash :: IO ()
writeThreadTokenPolicyHash = writeJSON "deploy/lotto-thread-token-policy.hash" $ PlutusTx.toBuiltinData $ Scripts.mintingPolicyHash threadTokenPolicy

writeTicketPolicyHash :: IO ()
writeTicketPolicyHash = writeJSON "deploy/ticket-policy.hash" $ PlutusTx.toBuiltinData $ Scripts.mintingPolicyHash $ ticketMintPolicy $ PlutusTx.toBuiltinData tmpParams

-- pull out the validator from a minting policy
mintValidator :: Ledger.MintingPolicy -> Ledger.Validator
mintValidator pol = Ledger.Validator $ unMintingPolicyScript pol

writeThreadTokenPolicy :: IO (Either (FileError ()) ())
writeThreadTokenPolicy = writeValidator "deploy/lotto-thread-token-policy.plutus" $ mintValidator $ threadTokenPolicy

writeTicketPolicy :: IO (Either (FileError ()) ())
writeTicketPolicy = writeValidator "deploy/ticket-policy.plutus" $ mintValidator $ ticketMintPolicy $ PlutusTx.toBuiltinData tmpParams


-- | Decode from hex base 16 to a base 10 bytestring is needed because
--   that is how it is stored in the ledger onchain
decodeHex :: B.ByteString -> PlutusPrelude.BuiltinByteString
decodeHex hexBS =    
         case getTx of
            Right decHex -> do
                --putStrLn $ "Tx name: " ++ show t
                PlutusPrelude.toBuiltin(decHex)  
            Left _ -> do
                --putStrLn $ "No Token name: " ++ show e
                PlutusPrelude.emptyByteString 
                
        where        
            getTx :: Either String B.ByteString = B16.decode hexBS


  
        

