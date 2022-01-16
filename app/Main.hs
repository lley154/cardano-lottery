{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where


import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..))
import           Data.Default                        (def)
import qualified Data.OpenApi                        as OpenApi
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           LottoContract                       (StarterContracts)
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.Trace.Emulator.Extract       (writeScriptsTo, ScriptsConfig (..), Command (..))
import           Ledger.Index                        (ValidatorMode(..))



{-
main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)
-}

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    -- Example of spinning up a game instance on startup
    -- void $ Simulator.activateContract (Wallet 1) GameContract
    -- You can add simulator actions here:
    -- Simulator.observableState
    -- etc.
    -- That way, the simulation gets to a predefined state and you don't have to
    -- use the HTTP API for setup.

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers = Simulator.mkSimulatorHandlers def def $ interpret (contractHandler Builtin.handleBuiltin)