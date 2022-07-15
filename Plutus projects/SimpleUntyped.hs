module SimpleUntyped where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

-- Create mkValidator function that takes datum, redeemer and context values of type BuiltinData: untyped arguements.And returns a value of type unit. 
-- This is becuase the result of this function doesn,t really matter. What matters is if the function executes sucessfully or with an exception.
-- Sucessful execution means validation was a sucess, while an error means validation failed.
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = () --We don't care about the value of the 3 arguments, we will always return a unit value.

-- At Compile time we want to convert the haskell mkValidator function to a plutus Validator.(Has plutus script embeded to it)

-- Template Haskell is a GHC extension to Haskell that adds compile-time meta-programming facilities. Allows us to write mkValidator as a Plutus program.
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||]) -- $$ is a splicer: 

-- we get the validator script hash using validatorHash function from the package Scripts
valhash :: Legder.validatorHash
valHash = Scripts.validatorHash validator

-- we get the validator script address directly using scriptAddress function
scrAddress :: Ledger.address
scrAddress = scriptAddress validator