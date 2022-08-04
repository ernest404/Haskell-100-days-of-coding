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

-- Now that we have a policy, we can get a currency symbol from the policy.We can call curSymbol in cabal repl to have a look at the currency symbol.
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- We define the other 2 parameters needed to burn or mint a native token in data MintParams since we already have CurrencySymbol. The two fields will be mpTokenName and mpAmount
data MintParams = MintParams
    { mpTokenName :: !TokenName -- ! makes the compiler to fully evaluate the arguments to a function instead of parsing thunks around. "thunk" is a piece of code that does some delayed work". Rather than execute some logic now.
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
-- if the mpAmount is positive, we should create tokens, and if it is negative, we should burn tokens.

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy policy
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

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
