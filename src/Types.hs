{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-} 

module Types where

import Data.Aeson                                               (FromJSON, ToJSON)  
import GHC.Generics                                             (Generic)
import Ledger.Address qualified as Address                      (Address, PaymentPubKeyHash(..))
import Ledger.Scripts qualified as Scripts                      (MintingPolicyHash)
import Ledger.Tx qualified as Tx                                (TxOutRef(..))
import Ledger.Value qualified as Value                          (TokenName(..), Value)
import PlutusTx qualified                                       (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude                                         (Bool(..), BuiltinByteString, Eq, Integer, (==), (&&))
import Prelude qualified as Haskell                             (Show)

-- | The lotto redemeer used for state transition and lotto actions
data LottoRedeemer = 
       Open
     | StartBuy 
     | StopBuy
     | Close
     | Draw BuiltinByteString 
     | Redeem Value.TokenName
     | Calc
     | Payout
     | End
     | Collect
     
    deriving Haskell.Show

PlutusTx.makeIsDataIndexed
  ''LottoRedeemer
  [ ('Open, 0),
    ('StartBuy, 1),
    ('StopBuy, 2),
    ('Close, 3),
    ('Draw, 4),
    ('Redeem, 5),
    ('Calc, 6),
    ('Payout, 7),
    ('End, 8),
    ('Collect, 9)
  ]
PlutusTx.makeLift ''LottoRedeemer

-- | The lotto admin data type is used to store key lotto administration
--   information in the lotto datum. 
data LottoAdmin = LottoAdmin
    {   adminPkh                  :: !Address.PaymentPubKeyHash   -- 0  
    ,   sponsorPkh                :: !Address.PaymentPubKeyHash   -- 1 
    ,   lottoValAddr              :: !Address.Address             -- 2 
    ,   lottoTokenValue           :: !Value.Value                 -- 3
    ,   buyValAddr                :: !Address.Address             -- 4 
    ,   buyTokenValue             :: !Value.Value                 -- 5 
    ,   ticketMph                 :: !Scripts.MintingPolicyHash   -- 6
    ,   percentFees               :: !Integer                     -- 7
    ,   ticketCost                :: !Integer                     -- 8  
    ,   difficulty                :: !Integer                     -- 9
    --,   deadline                :: !POSIXTime             
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq LottoAdmin where
    {-# INLINABLE (==) #-}
    a == b =    (adminPkh         a == adminPkh           b)
             && (sponsorPkh       a == sponsorPkh         b)
             && (lottoValAddr     a == lottoValAddr       b)
             && (lottoTokenValue  a == lottoTokenValue    b)
             && (buyValAddr       a == buyValAddr         b)
             && (buyTokenValue    a == buyTokenValue      b)
             && (ticketMph        a == ticketMph          b)
             && (percentFees      a == percentFees        b)
             && (ticketCost       a == ticketCost         b)
             && (difficulty       a == difficulty         b)
       
PlutusTx.makeIsDataIndexed ''LottoAdmin [('LottoAdmin,0)] 
PlutusTx.makeLift ''LottoAdmin

-- | The thread token redeemer passes a utxo from the lotto admin's wallet 
--   to the thread token miting policy which is used to create the lotto 
--   and buy thread tokens
data ThreadTokenRedeemer = ThreadTokenRedeemer
    {   ttTxOutRef :: !Tx.TxOutRef  
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''ThreadTokenRedeemer [('ThreadTokenRedeemer,0)] 

-- | The mint ticket reeemder passes the ticket to be minted or burned
data MintTicketRedeemer = MintTicketRedeemer
    { polarity                  :: !Bool  -- True = Mint, False = Burn
    , bTicketNum                :: !Value.TokenName
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintTicketRedeemer [('MintTicketRedeemer,0)] 
PlutusTx.makeLift ''MintTicketRedeemer

-- | The buy redeemer BuyTicket is called by the player to buy a ticket whereas
--   the TransferToken is called by the lotto admin to move the buy token and ada
--   locked at the buy script address back to the lotto script address
data BuyRedeemer = 
       BuyTicket
     | TransferToken 
  
    deriving Haskell.Show
      
PlutusTx.makeIsDataIndexed
  ''BuyRedeemer
  [ ('BuyTicket, 0),
    ('TransferToken, 1)
  ]
PlutusTx.makeLift ''BuyRedeemer

-- | The lotto validator parmas are attached to the lotto script and cannot
--   be changed once the lotto script has been created.  This is by design so
--   the lotto difficulty and jackpot split cannot be changed after the lotter has been started.
--   The lottoId is include to help ensure each lottery is unique and is 
--   recommended to use the lotto admin utxo that is provided to the threadtoken when the 
--   the lotto is initialized.
data LottoValidatorParams = LottoValidatorParams
    { lvpLottoId                :: !BuiltinByteString
    , lvpDifficulty             :: !Integer
    , lvpPotSplit               :: !Integer -- the % of the jackpot goes to the sponsor
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''LottoValidatorParams [('LottoValidatorParams,0)] 
PlutusTx.makeLift ''LottoValidatorParams

-- | The buy validator params are also locked to a particuar buy script instance by design
--   This ensures that if parameters such as admin pkh change within the lotto contract
--   that a new buy validator is needed.  The same is true for the other parameters within
--   the buy validator params.
data BuyValidatorParams = BuyValidatorParams
    { bvpAdminPkh               :: !Address.PaymentPubKeyHash
    , bvpBuyTokenValue          :: !Value.Value
    , bvpLottoTokenValue        :: !Value.Value
    , bvpLottoValAddr           :: !Address.Address
    , bvpTicketCost             :: !Integer
    --, bvpDeadline             :: !Time.POSIXTime 
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''BuyValidatorParams [('BuyValidatorParams,0)] 
PlutusTx.makeLift ''BuyValidatorParams

-- | The ticket mint params lock that buy token value and buy script address
--   because the ticket mintig policy needs to validate that the buytoken exists
--   in a buy transaction and that it is being sent back to the buy script address.
--   This is the only way the ticket minting policy knows that it is ok to mint a
--   a buy ticket.
data TicketMintParams = TicketMintParams
    { tmpBuyTokenValue          :: !Value.Value
    , tmpBuyValAddr             :: !Address.Address
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''TicketMintParams [('TicketMintParams,0)] 
PlutusTx.makeLift ''TicketMintParams

-- | The Buy datum keeps count of the number of ticket purchased during
--   a lottery cycle and the amount of ada locked at the buy script address
data BuyDat = BuyDat
    { bdTicketTotal             :: !Integer
    , bdTotalBuyValue           :: !Integer
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''BuyDat [('BuyDat, 0)]
PlutusTx.makeLift ''BuyDat

