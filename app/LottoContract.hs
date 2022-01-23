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


import           Data.Aeson                             (FromJSON (..), ToJSON (..))                                                      
import qualified Data.OpenApi                           as OpenApi
import           GHC.Generics                           (Generic)
import           Data.Text.Prettyprint.Doc              (Pretty (..), viaShow)
import           Lottery
import qualified Plutus.PAB.Effects.Contract.Builtin    as Builtin
import           Prelude                                hiding (init)

{-
data StarterContracts = 
                        InitLottoContract
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty StarterContracts where
    pretty = viaShow

   
instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [InitLottoContract]
    getSchema =  \case  
        InitLottoContract    -> Builtin.endpointsToSchemas @Lottery.LottoInitSchema
   
    getContract = \case
        InitLottoContract    -> Builtin.SomeBuiltin Lottery.initEndpoint

-}


data StarterContracts = 
                        InitLottoContract
                      | UseLottoContract Lottery
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty StarterContracts where
    pretty = viaShow

   
instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [InitLottoContract]
    getSchema =  \case
        UseLottoContract _   -> Builtin.endpointsToSchemas @LottoUseSchema   
        InitLottoContract    -> Builtin.endpointsToSchemas @LottoInitSchema
   
    getContract = \case
        UseLottoContract lt  -> Builtin.SomeBuiltin $ useEndpoints lt
        InitLottoContract    -> Builtin.SomeBuiltin initEndpoint

