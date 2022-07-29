{-# LANGUAGE DataKinds           #-} -- lets you promote data types to kinds and data constructors to types. The intuition is that, just like types can be considered sets of values, kinds can be considered sets of types.
{-# LANGUAGE FlexibleContexts    #-} -- Remove the type-variable restriction on class contexts.
{-# LANGUAGE NoImplicitPrelude   #-} -- used to disable the prelude from being implicitly imported by GHC.https://typeclasses.com/ghc/no-implicit-prelude.This is because we want to make use of plutus preluded which is better suited for our implementation.
{-# LANGUAGE ScopedTypeVariables #-} -- Enables you to write an explicit type signature for any sub-term of a function. https://serokell.io/blog/universal-and-existential-quantification
{-# LANGUAGE TemplateHaskell     #-} --Introduces metaprogramming capabilities of Haskell. It is mostly useful for generating boilerplate code and automating some aspects of the compilation: https://serokell.io/blog/introduction-to-template-haskell
{-# LANGUAGE TypeApplications    #-} --the extension allows you to give explicit type arguments to a polymorphic function such as read
{-# LANGUAGE TypeFamilies        #-} -- used to support ad-hoc overloading of data types.It is useful for generic programming, for creating highly parameterised library interfaces, and for creating interfaces with enhanced static information, much like dependent types
{-# LANGUAGE TypeOperators       #-} --makes it possible to use an operator as the name of a type. https://typeclasses.com/ghc/type-operators


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
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "Wrong Redeemer" --Validation fails with error

data Typed --records that datum and redeemer are using high level types
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed= ()
    type instance RedeemerType Typed = Integer
-- At Compile time we want to convert the haskell mkValidator function to a plutus Validator.(Has plutus script embeded to it)

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed -- used to force a type decision to polymorphic functions.(Forcing mkTypedValidator to be of type typed.)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) -- offers translation between high and low level types for compilation to Plutus script and back.
  where
    wrap = Scripts.wrapValidator @() @Integer --translates to hign level types.

validator :: Validator
validator = Scripts.validatorScript typedValidator

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
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx --https://typeclasses.com/featured/dollar#:~:text=The%20dollar%20sign%2C%20%24%2C%20is%20a%20controversial%20little,not%20via%20its%20type%20but%20via%20its%20precedence
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
    -- This grabs the first utxo at the script address. not sure.
    let orefs = fst <$> Map.toList utxos

 --"The "constraints" specify what properties the transaction should have.
-- The "lookups" allow the algorithm to actually construct a transaction with those properties.
-- So if a property is "consume that script output", then in order to construct the transaction, the algorithm needs to know the script belonging to that address.
        lookups = Constraints.unspentOutputs utxos <> --A script lookups value that uses the map of unspent outputs to resolve input constraints.
                  Constraints.OtherScript validator --A script lookups value with a validator script.
        tx :: TxConstraints Void Void --construct transaction using the constraints above
-- The transaction must spend the given unspent transaction script output.
-- mustSpendScriptOutput: this constraint adds utxo(s) and redeemer as an input to the transaction. Information about this utxo must be provided in the ScriptLookups with unspentOutputs. The validator must be either provided by unspentOutputs or through otherScript. The datum must be either provided by unspentOutputs or through otherData.
-- mustSpendScriptOutput :: forall i o. TxOutRef -> Redeemer -> TxConstraints i o : i - inputs and o - outputs
-- The output of this function is a singleton containing TxConstraint of each utxo at the script.
--  Since there could be several singleton TxConstraints produced we use mconcat to flatten the list from list of singleton lists of TxConstraints. 
-- txConstraints :: [TxConstraint] 
-- txOwnInputs :: [ScriptInputConstraint i] 
-- txOwnOutputs :: [ScriptOutputConstraint o]
-- mconcat is a monoid method that takes flattens the list of constraints 
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs] 
    -- Build a transaction that satisfies the constraints, then submit it to the network. Using the given constraints.    
    -- submitTxConstraintsWith :: ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> Contract w s e CardanoTx
    -- <- gets the result from constract wsea monad: CardanoTx
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    -- get Traction id.
    -- Wait until a transaction is confirmed (added to the ledger). If the transaction is never added to the ledger then awaitTxConfirmed never returns. 
    -- awaitTxConfirmed :: forall w s e. AsContractError e => TxId -> Contract w s e () 
    -- value discards or ignores the result of evaluation, such as the return value of an contract action (contract wse()) from awaitTxConfirmed 
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
-- awaitPromise: A wrapper indicating that this contract starts with a waiting action. For use with select. awaitPromise :: Promise w s e a -> Contract w s e a 
-- select returns the contract that makes progress first, discarding the other one.
-- >>: Sequentially compose two actions, discarding any value produced by the first, like sequencing operators (such as the semicolon) in imperative languages.
-- The function calls itself again exposing the endpints 
endpoints = awaitPromise (give' `select` grab') >> endpoints
    where
        -- Expose an endpoint, return the data that was entered.
        -- endpoint :: forall l a w s e b. (HasEndpoint l a s, AsContractError e, FromJSON a) => (a -> Contract w s e b) -> Promise w s e b 
        give' = endpoint @"give" give
        grab' = endpoint @"grab" $ const grab

-- 
mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []



        
