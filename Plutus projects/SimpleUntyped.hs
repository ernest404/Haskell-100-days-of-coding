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

-- we get the validator script hash using validatorHash function from the package Scripts. We are going to use it to build a transaction.
valhash :: Legder.validatorHash
valHash = Scripts.validatorHash validator

-- we get the validator script address directly using scriptAddress function
scrAddress :: Ledger.address
scrAddress = scriptAddress validator

type GiftSchema = 
        Endpoint "give" Integer
    .\/ Endpoint "grab" () -- .\/ is used to join the two endpoints.

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    -- mustPayToOtherScript: locks the value v in lovelace with the given script hash vh alongside a datum d of Type Builtins. This operation is saved as tx to be executed in the next line.
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount 
    -- Build a transaction that satisfies the constraints above, then submit it to the network.
    ledgerTx <- submitTx tx
    -- get the transaction id of the above transaction. 
    -- Wait until a transaction is confirmed (added to the ledger) this returns value of type Contract wse(). If the transaction is never added to the ledger then awaitTxConfirmed never returns.
    -- value discards or ignores the result of evaluation, such as the return value of a Contract monad action
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    -- Logs a message at the Info level of the wallet.
    -- the type indicator introduced by @ relates to the type of the input taken by logInfo. logInfo :: ToJSON a => a -> Contract w s e ()
    -- https://stackoverflow.com/questions/30326249/what-does-mean-in-haskell
    logInfo @String "Sucessfully locked %d lovelace" amount

grab :: forall w s e. AsContractError e -> Contract w s e ()
grab = do
    -- Gets the unspent transaction outputs at the script address. utxosAt :: forall w s e. AsContractError e => Address -> Contract w s e (Map TxOutRef ChainIndexTxOut) 
    -- Use <- to get the result (Map TxOutRef ChainIndexTxOut) in the contract monad. ChainIndexTxOut: List of outputs of a transaction. TxOutRef: A reference to a transaction output. 
    utxos <- utxosAt scrAddress
    -- Map.toList utxos: takes all the (TxOutRef ChainIndexTxOut) pairs and puts them in a list. fst returns the first value from the pair.
    -- This grabs the first utxo at the script address.
    let orefs = fst <$> Map.toList utxos

 --"The "constraints" specify what properties the transaction should have.
-- The "lookups" allow the algorithm to actually construct a transaction with those properties.
-- So if a property is "consume that script output", then in order to construct the transaction, the algorithm needs to know the script belonging to that address.
        lookups = Constraints.unspentOutputs utxos <> --A script lookups value that uses the map of unspent outputs to resolve input constraints.
                  Constraints.OtherScript validator --A script lookups value with a validator script.
        tx :: TxConstraints Void Void --construct transaction using the constraints above
        tx = mc

