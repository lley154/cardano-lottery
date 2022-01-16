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

import Control.Monad.Freer
import Data.Aeson                           (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON, defaultOptions, Options(..))
import Data.Default                         (Default (def))
import qualified Data.OpenApi               as OpenApi
import GHC.Generics                         (Generic)
import Prettyprinter
import Lottery                              as Lottery
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Prelude


data StarterContracts = InitLottoContract
                      | UseLottoContract Lottery.Lottery
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass (FromJSON, ToJSON)
                      deriving anyclass OpenApi.ToSchema
 
 {-   
instance ToJSON StarterContracts where
  toJSON = genericToJSON defaultOptions {
             tagSingleConstructors = True }
instance FromJSON StarterContracts where
  parseJSON = genericParseJSON defaultOptions {
             tagSingleConstructors = True }
-}

instance Pretty StarterContracts where
    pretty = viaShow
    
instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [InitLottoContract]
    getSchema =  \case
        InitLottoContract    -> Builtin.endpointsToSchemas @Lottery.LottoInitSchema
        UseLottoContract _   -> Builtin.endpointsToSchemas @Lottery.LottoUseSchema   
       
    getContract = \case
        InitLottoContract    -> SomeBuiltin Lottery.initEndpoint
        UseLottoContract lt  -> SomeBuiltin $ Lottery.useEndpoints lt


        
