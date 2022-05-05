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
import           Prettyprinter                          (Pretty (..), viaShow)
import           OffChain
import qualified Plutus.PAB.Effects.Contract.Builtin    as Builtin
import           Prelude                                hiding (init)


data StarterContracts = InitLottoContract
                      | UseLottoContract
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty StarterContracts where
    pretty = viaShow
 

instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [ InitLottoContract, UseLottoContract ]
    getSchema =  \case
        InitLottoContract    -> Builtin.endpointsToSchemas @LottoInitSchema
        UseLottoContract     -> Builtin.endpointsToSchemas @LottoUseSchema   
   
    getContract = \case
        InitLottoContract    -> Builtin.SomeBuiltin initEndpoint
        UseLottoContract     -> Builtin.SomeBuiltin useEndpoint
     
