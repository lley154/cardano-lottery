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
    , handlers
    ) where

import Control.Monad.Freer
import Data.Aeson (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON, defaultOptions, Options(..))
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Prettyprinter
--import Data.OpenApi.Schema qualified as OpenApi
--import Data.Row
--import Ledger (TxId)
import Lottery as Lottery
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Prelude


data StarterContracts = InitLotto'
                      | UseLotto' Lottery.Lottery
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass (FromJSON, ToJSON)
--    deriving anyclass OpenApi.ToSchema
 
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
    getDefinitions = [InitLotto']
    getSchema =  \case
        InitLotto'    -> Builtin.endpointsToSchemas @Lottery.LottoInitSchema
        UseLotto' _ -> Builtin.endpointsToSchemas @Lottery.LottoUseSchema   
       
    getContract = \case
        InitLotto'    -> SomeBuiltin Lottery.initEndpoint
        UseLotto' sp  -> SomeBuiltin Lottery.useEndpoints sp

        
        
