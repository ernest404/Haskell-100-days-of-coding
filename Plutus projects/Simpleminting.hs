{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-} --enables us to use usual string syntax for ByteString and Text which both provide certain time and space advantages over [Char]. This means we can enter the Currency Symbol and Token name as literal strings.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-} --oxford brackets work with functions defined inside them, since mkPolicy is already defined we can use the INLINABLE program to trick it.
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True --This policy ignores the context and redeemer value always returns True. This will allow arbitrary minting and burning of tokens for value and token name that belongs to the currency symbol associated with this policy.

policy :: Scripts.MintingPolicy
--We use Template Haskell to compile mkPolicy function to plutus core. So make sure it is inlinable 
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

-- Now that we have a policy, we can get a currency symbol from the policy this is used to c.We can call curSymbol in cabal repl to have a look at the currency symbol.
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- We define the other 2 parameters needed to burn or mint a native token in data MintParams since we already have CurrencySymbol. The two fields will be mpTokenName and mpAmount
data MintParams = MintParams
    { mpTokenName :: !TokenName -- ! makes the compiler to fully evaluate the arguments to a function instead of parsing thunks around. "thunk" is a piece of code that does some delayed work". Rather than execute some logic now.
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
-- if the mpAmount is positive, we should create tokens, and if it is negative, we should burn tokens.

-- Define the schema which carries the available actions we can take in the wallet
type FreeSchema = Endpoint "mint" MintParams

-- Build the mint action transaction.
--Remember Contract monad takes four type parameters. The first is the writer monad which allows us to use a tell function. By leaving this parametric with a small w, we indicate that we will not be making use of this parameter - we won’t tell any state.
--The next parameter is the schema: we pass FreeSchema which gives us access to the regular block chain actions, as well as the mint endpoint.
--The third parameter is the type of error message, and as we have seen, Text works fine.
--Finally, the last parameter is the return type, and our contract will just have the Unit return type.
mint :: MintParams -> Contract w FreeSchema Text ()
-- First we define the value that we want to forge. For this we are using the singleton function whose arguments are currencysymbol, tokenname and an Integer.
-- Currecysymbol is from hashing the policy, tokenname and Integer amount are passed along with the action mint. 
-- 
mint mp = do -- we use do notation because this function produces a monadic type and we use it to glue together monadic values in sequence.
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy policy --Script lookups: specify pieces of data that the script interacts with. Minting policies, Unspent outputs that the script may want to spend etc: https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints-OffChain.html#t:ScriptLookups 
        tx      = Constraints.mustMintValue val --specifies to the tx to mint the token val specified 
    ledgerTx <- submitTxConstraintsWith @Void lookups tx --Build a transaction that satisfies the constraints, then submit it to the network. Using the given constraints.
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx --Wait until a transaction of a certain TxId is confirmed (added to the ledger). If the transaction is never added to the ledger then awaitTxConfirmed never returns
    Contract.logInfo @String $ printf "forged %s" (show val) --Log a message at the Info level

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint --endpoint: Exposes an endpoint, return the data that was entered.

-- structures the contract actions in a way that is suitable for consumption by the Playground's website
mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
