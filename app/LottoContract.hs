{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module LottoContract(
    StarterContracts(..)
    ) where


import           Data.Aeson                             (Result (..), FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                        , defaultOptions, Options(..), fromJSON)
import           Data.Default                           (def)
import qualified Data.Monoid                            as Monoid                                                        
import qualified Data.OpenApi                           as OpenApi
import           Data.Text                              (Text)
import           GHC.Generics                           (Generic)
import           Data.Text.Prettyprint.Doc              (Pretty (..), viaShow)
import qualified Ledger.TimeSlot                        as TimeSlot
import           Lottery                                as Lottery
import           Plutus.PAB.Effects.Contract.Builtin    (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin    as Builtin
import           Plutus.Contract                        (Contract, ownPubKeyHash)
import qualified Plutus.V1.Ledger.Slot                  as Slot 
import           Prelude                                hiding (init)
import qualified Plutus.PAB.Simulator                   as Simulator
import           Wallet.Emulator.Types                  (Wallet(..), getWallet, knownWallet, walletPubKeyHash)


data StarterContracts = 
                        InitLottoContract
                      | UseLottoContract Lottery.Lottery
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty StarterContracts where
    pretty = viaShow

   
instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [InitLottoContract]
    getSchema =  \case
        UseLottoContract _   -> Builtin.endpointsToSchemas @Lottery.LottoUseSchema   
        InitLottoContract    -> Builtin.endpointsToSchemas @Lottery.LottoInitSchema
   
    getContract = \case
        UseLottoContract lt  -> Builtin.SomeBuiltin $ Lottery.useEndpoints lt
        InitLottoContract    -> Builtin.SomeBuiltin Lottery.initEndpoint


