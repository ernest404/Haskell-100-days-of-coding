{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DerivingStrategies    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Test where

import qualified PlutusTx
import PlutusTx.Prelude
import           GHC.Generics                         ( Generic )
import           Data.Aeson                           ( FromJSON
                                                      , ToJSON )
import Plutus.V2.Ledger.Api
import           Prelude                              ( Show
                                                      , show
                                                      , repeat
                                                      , read
                                                      , String)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Ledger.Ada                           as Ada
import Plutus.V2.Ledger.Contexts

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
data TestingDatum = TestingDatum
    { value1     :: !Integer
    , value2     :: !Integer
    , value3     :: !Integer
    , value4     :: !Integer
    , value5     :: !Integer
    , value6     :: !Integer
    , value7     :: !Integer
    , value8     :: !Integer
    , value9     :: !Integer
    , value10    :: !Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''TestingDatum

{-# INLINABLE mkValidator #-}
mkValidator ::  TestingDatum -> () -> ScriptContext -> Bool
mkValidator _ () ctx  = True

data Testing
instance PSU.V2.ValidatorTypes Testing where
  type DatumType             Testing = TestingDatum
  type instance RedeemerType Testing = ()

typedValidator :: PSU.V2.TypedValidator Testing
typedValidator = PSU.V2.mkTypedValidator @Testing
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = PSU.V2.mkUntypedValidator @TestingDatum @()

data TestingParams = TestingParams
    { tp1  :: !Integer
    , tp2  :: !Integer
    , tp3  :: !Integer
    , tp4  :: !Integer
    , tp5  :: !Integer
    , tp6  :: !Integer
    , tp7  :: !Integer
    , tp8  :: !Integer
    , tp9  :: !Integer
    , tp10 :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

makeLargeStrings :: Integer -> Integer
makeLargeStrings n =  toInt (concat["1111111111" | n <- [0..n-1]])

toInt :: String -> Integer
toInt = read

type GiftSchema = Endpoint "give" TestingParams

give :: AsContractError e => TestingParams -> Contract w s e ()
give params = do
    let dat = TestingDatum
                { value1  = tp1  params
                , value2  = tp2  params
                , value3  = tp3  params
                , value4  = tp4  params
                , value5  = tp5  params
                , value6  = tp6  params
                , value7  = tp7  params
                , value8  = tp8  params
                , value9  = tp9  params
                , value10 = tp10 params
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ 1000000
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "Sucessfully locked 1000000 lovelaces"

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    callEndpoint @"give" h1
    void $ waitUntilSlot 20

test :: IO ()
test = runEmulatorTraceIO myTrace