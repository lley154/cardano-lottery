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

module BenchContract(
    StarterContracts(..)
    ) where


import           Data.Aeson                             (FromJSON (..), ToJSON (..))                                                      
import qualified Data.OpenApi                           as OpenApi
import           GHC.Generics                           (Generic)
import           Data.Text.Prettyprint.Doc              (Pretty (..), viaShow)
import           Benchmark
import qualified Plutus.PAB.Effects.Contract.Builtin    as Builtin
import           Prelude                                hiding (init)


data StarterContracts = BenchContract
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty StarterContracts where
    pretty = viaShow
 

instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [ BenchContract ]
    getSchema =  \case
        BenchContract    -> Builtin.endpointsToSchemas @GiftSchema
   
    getContract = \case
        BenchContract    -> Builtin.SomeBuiltin endpoints

     

