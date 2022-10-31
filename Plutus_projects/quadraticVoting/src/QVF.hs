-- EXTENSIONS
-- {{{

{-# LANGUAGE DataKinds           #-} -- lets you promote data types to kinds and data constructors to types. The intuition is that, just like types can be considered sets of values, kinds can be considered sets of types.
{-# LANGUAGE FlexibleContexts    #-} -- Remove the type-variable restriction on class contexts.
{-# LANGUAGE NoImplicitPrelude   #-} -- used to disable the prelude from being implicitly imported by GHC.https://typeclasses.com/ghc/no-implicit-prelude.This is because we want to make use of plutus preluded which is better suited for our implementation.
{-# LANGUAGE ScopedTypeVariables #-} -- Enables you to write an explicit type signature for any sub-term of a function. https://serokell.io/blog/universal-and-existential-quantification
{-# LANGUAGE TemplateHaskell     #-} -- Introduces metaprogramming capabilities of Haskell. It is mostly useful for generating boilerplate code and automating some aspects of the compilation: https://serokell.io/blog/introduction-to-template-haskell
{-# LANGUAGE TypeApplications    #-} -- the extension allows you to give explicit type arguments to a polymorphic function such as read
{-# LANGUAGE TypeFamilies        #-} -- used to support ad-hoc overloading of data types.It is useful for generic programming, for creating highly parameterised library interfaces, and for creating interfaces with enhanced static information, much like dependent types
{-# LANGUAGE TypeOperators       #-} -- makes it possible to use an operator as the name of a type. https://typeclasses.com/ghc/type-operators


{-# LANGUAGE DeriveGeneric         #-} --lests us use derriving clause to make a type an instance of a generic typeclass.
{-# LANGUAGE DeriveAnyClass        #-} -- Allow use of any typeclass in deriving clauses
{-# LANGUAGE OverloadedStrings     #-} --Enable overloaded string literals (e.g. string literals desugared via the IsString class).This means usual string syntax can be used for ByteString, Text, and other variations of string like types.
{-# LANGUAGE TupleSections         #-} --extension enables partially applied tuple constructors.
{-# LANGUAGE RecordWildCards       #-} --Allow the use of wildcards in record construction and pattern matching
{-# LANGUAGE NumericUnderscores    #-} --Allow the use of underscores in numeric literals. eg date 2017_12_31
{-# LANGUAGE MultiParamTypeClasses #-} -- Enable use of type classes which can take multiple arguments,
{-# LANGUAGE LambdaCase            #-} --Allow the use of lambda-case syntax.


{-# OPTIONS_GHC -fno-warn-unused-imports #-} --supresses warnings regarding imports which are not being used.The OPTIONS_GHC pragma is used to specify additional options that are given to the compiler when compiling this source file
-- }}}


-- MODULE
-- {{{
module QVF where
-- }}}


-- IMPORTS
-- {{{
import           Ledger.Ada                           ( lovelaceValueOf ) -- Provides functions for working with Ada in Template Haskell.
-- If you do a qualified import, you have to prefix the name with the module it's imported from this allows using functions with the same name imported from several modules, e.g. map from the Prelude and map from Data.Map.
import qualified Ledger.Typed.Scripts                 as Scripts -- Provides the validator script type and associated functions.
-- Validator is a wrapper around Scripts which are used as validators in transaction outputs.
-- MintingPolicy is a wrapper around Scripts which are used as validators for minting constraints.
import qualified Plutonomy --plutonomy is an optimizer for untyped plutus core.
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2 -- Provides functions and types for working with typed and untyped Plutus Core scripts.
import           Plutus.V1.Ledger.Address             ( scriptHashAddress ) -- Provides functions for working with addresses.
import qualified Plutus.V1.Ledger.Interval            as Interval -- Provides functions for working with time intervals.
import           Plutus.V1.Ledger.Value             ( flattenValue --Flattens a value into a list of individual currency values.
                                                      , AssetClass(..) )-- An asset class is a pair of a currency symbol and a token name. It is used to identify a specific token.
-- Provides functions for working with values.
import Plutus.V2.Ledger.Api -- The interface to Plutus V2 for the ledger.https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html  
import Plutus.V2.Ledger.Contexts -- Provides types and functions to work with Plutus V2 contexts.https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Contexts.html
import qualified PlutusTx -- Used to compile Haskell code to Plutus Core.https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html
import qualified PlutusTx.AssocMap                    as Map --Provides a map represented as an "association list" of key-value pairs.[(k, v)] is equal to Map k v
import           PlutusTx.AssocMap                    ( Map )
import qualified PlutusTx.Builtins                    as Builtins --Provides primitive names and functions for working with Plutus Core builtins.https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html 
import PlutusTx.Prelude                               ( Bool(..)
                                                      , Integer
                                                      , Maybe(..)
                                                      , BuiltinByteString
                                                      , Eq((==))
                                                      , Semigroup((<>))
                                                      , Ord(max, (>=), (<), min, (>), (<=))
                                                      , AdditiveGroup((-))
                                                      , AdditiveSemigroup((+))
                                                      , MultiplicativeSemigroup((*))
                                                      , ($)
                                                      , (.)
                                                      , (&&)
                                                      , any
                                                      , find
                                                      , foldr
                                                      , filter
                                                      , isJust
                                                      , negate
                                                      , divide
                                                      , traceError
                                                      , traceIfFalse ) --The PlutusTx Prelude is a replacement for the Haskell Prelude that works better with Plutus Tx. You should use it if you're writing code that will be compiled with the Plutus Tx compiler.https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html
import           PlutusTx.Sqrt                        ( Sqrt(..) --Integer square-root representation, discarding imaginary integers.
                                                      , isqrt )
import           Prelude                              ( Show
                                                      , show ) --Haskell Prelude providing the Show typeclass and the show function.
import qualified Prelude                              as P --Haskell Prelude qualified import.P.Integer

import           Data.Datum --Provides types and functions defined in the module Data.Datum in the Data folder.
import           Data.DonationInfo --Provides types and functions defined in the module Data.DonationInfo in the Data folder.
import           Data.Redeemer --Provides types and functions defined in the module Data.Redeemer in the Data folder.
import           Data.RegistrationInfo --Provides types and functions defined in the module Data.RegistrationInfo in the Data folder.
import qualified Minter.Governance                    as Gov --Provides types and functions defined in the module Minter.Governance in the Minter folder.
import           Minter.Governance                    ( qvfTokenName
                                                      , deadlineTokenName ) --Provides types and functions defined in the module Minter.Governance in the Minter folder.
import qualified Minter.Registration --Provides types and functions defined in the module Minter.Registration in the Minter folder.
import           Utils --Provides types and functions defined in the module Utils. This are utility functions used in the QVF module.
-- }}}


-- QVF PARAMETERS
-- {{{
data QVFParams = QVFParams
  { qvfKeyHolder      :: PubKeyHash --The public key hash of the QVF key holder.
  , qvfSymbol         :: CurrencySymbol --The currency symbol of the QVF governance token.
  , qvfProjectSymbol  :: CurrencySymbol --The currency symbol of the QVF project token.
  , qvfDonationSymbol :: CurrencySymbol --The currency symbol of the QVF donation token.
  }

-- Make QVFParams an instance of the Show typeclass.
-- To be printed as a dictionary like map.
instance Show QVFParams where
  show QVFParams{..} = --this will implicitly write for every parameter of QVFParams if you write {..} when we do record pattern matching.
         "QVFParams"
    P.++ "\n  { qvfKeyHolder      = " P.++ show qvfKeyHolder
    P.++ "\n  , qvfSymbol         = " P.++ show qvfSymbol
    P.++ "\n  , qvfProjectSymbol  = " P.++ show qvfProjectSymbol
    P.++ "\n  , qvfDonationSymbol = " P.++ show qvfDonationSymbol
    P.++ "\n  }"


PlutusTx.makeLift ''QVFParams --a Template Haskell statement that generates an instance of the PlutusTx.Lift.Class.Lift class for QVFParams. This class is used by the Plutus compiler at compile-time to construct Plutus core programs and types.
-- }}}


-- QVF VALIDATOR 
-- {{{
{-# INLINABLE mkQVFValidator #-} --Inlinable pragma is used to trick the PlutusTx oxford brackets into thinking mkQVFValidator function is defined in the oxford brackets.
-- mkQVFValidator function is used to make the QVF script.It that QVFParams(parameterizer), datum(QVFDatum), redeemer(QVFAction) and context values and returns a Bool value.
mkQVFValidator :: QVFParams
               -> QVFDatum
               -> QVFAction
               -> ScriptContext
               -> Bool
mkQVFValidator QVFParams{..} datum action ctx =
  -- {{{
  let
    info   = scriptContextTxInfo ctx --gets the values of txinfo field from the script context(txinfo, txpurpose).

    inputs = txInfoInputs info --gets the Transaction inputs (TxInInfo field) of current transaction.

    -- | Get the UTxO currently being validated.
    currUTxO :: TxOut
    currUTxO =
      -- {{{
        -- Calling findOwnInput finds the input to script currently being validated..
      case findOwnInput ctx of
        Nothing -> traceError "Couldn't find UTxO." --if no TxInInfo field is returned throw an error message.
        Just i  -> txInInfoResolved i --If TxInInfo field is found, run txInInfoResolved function to get the UTxO from the TxInInfo field..
      -- }}}

    -- | Get Script's address.
    ownAddr :: Address
    ownAddr =
      -- {{{
      txOutAddress currUTxO --Get the addess of the UTxO currently being validated.This utxo is held at the script address.
      -- }}}

    -- | Checks if a given UTxO is in fact from this contract.
    utxoSitsAtScript :: TxOut -> Bool
    utxoSitsAtScript =
      -- {{{
      (== ownAddr) . txOutAddress --Checks if the address of a given UTxo is the script's address.Returns true or false.
      -- }}}

    -- | Checks for key holder's signature in a transaction. Induced laziness.
    signedByKeyHolder :: Bool
    signedByKeyHolder =
      -- {{{
      traceIfFalse "Unauthorized." $ txSignedBy info qvfKeyHolder --txSignedBy :: TxInfo -> PubKeyHash -> Bool. Checks if the transaction is signed by the public key hash provided.
      -- }}}

    -- | Checks if the UTxO currently being validated carries a single asset X.
    currUTxOHasX :: CurrencySymbol -> TokenName -> Bool
    currUTxOHasX sym tn =
      -- {{{
      utxoHasOnlyX sym tn currUTxO --where is the logic for this function? utility.hs
      -- }}}

    -- | Tries to find a singular asset with a given symbol inside the given
    --   UTxO, and returns its token name.
    getTokenNameOfUTxO :: CurrencySymbol -> TxOut -> Maybe TokenName
    getTokenNameOfUTxO sym utxo =
      -- {{{
      case flattenValue (txOutValue utxo) of -- utxo is of type TxOut which is a record with txOutAddress :: Address,txOutValue :: Value, txOutDatumHash :: Maybe DatumHash.  Get the value of a transaction output.The flattenValue function returns a list of triples of currency symbol, token name and integer value.
        [(sym', tn', amt'), _] -> --if there is some value check if the currsymbol is one passed to function and the amount is 1
          -- {{{
          if sym' == sym && amt' == 1 then
            Just tn' --if the a bove is the case return the token name
          else
            Nothing --else nothing
          -- }}}
        _                      -> -- if there are no values types in return nothing
          -- {{{
          Nothing
          -- }}}
      -- }}}

    -- | Tries to find a singular asset with a given symbol inside the UTxO currently being validated, and returns its token name.
    --   Raises exception upon failure. Makes use of getTokenNameOfUTxO function and the currUTxO
    getCurrTokenName :: CurrencySymbol -> TokenName
    getCurrTokenName sym =
      -- {{{
      case getTokenNameOfUTxO sym currUTxO of --uses the above helper function to get the token name of the UTxO currently being validated.
        Just tn -> tn
        Nothing -> traceError "Current UTxO is unauthentic."
      -- }}}

    -- | Looks inside the reference inputs and extracts the inline datum attached to one of which carries an asset X.
    getDatumFromRefX :: CurrencySymbol -> TokenName -> QVFDatum
    getDatumFromRefX sym tn =
      -- {{{
      case find (utxoHasX sym (Just tn) . txInInfoResolved) (txInfoReferenceInputs info) of --Finds the first element from a list of referenced inputs (txInfoReferenceInputs info returns a list of all refinputs) that satisfies the predicate if it has the specified asset (utxoHasX sym (Just tn) . txInInfoResolved), if any.
        Just txIn ->
          -- {{{
          getInlineDatum (txInInfoResolved txIn) --txInInfoResolved txIn gets the utxo of the given txin.
          -- }}}
        Nothing   ->     --   Raises exception if the reference input, or the datum is not found.
          -- {{{
          traceError "Missing reference input."
          -- }}}
      -- }}}

    -- | Looks for the presence of a UTxO from the script in the input list
    --   with 1 X asset, and a datum that complies with the given predicate.
    xInputWithSpecificDatumExists :: CurrencySymbol
                                  -> TokenName
                                  -> (QVFDatum -> Bool)
                                  -> Bool
    xInputWithSpecificDatumExists sym tn datumPred =
      -- {{{
      let
        predicate TxInInfo{txInInfoResolved = txOut} = --Define a function predicate that checks if TxInInfo{txInInfoResolved = txOut} is a TxIn) has the specified asset, if it's utxo sits at a script and the datum complies with the given predicate. 
          -- {{{
             utxoHasX sym (Just tn) txOut
          && utxoSitsAtScript txOut
          && datumPred (getInlineDatum txOut)
          -- }}}
      in
      isJust $ find predicate inputs --The isJust function returns True iff its argument is of the form Just _
      -- The find function takes a predicate and a list and returns just the first element in the list matching the predicate, or Nothing if there is no such element.
      -- }}}

    -- | Collection of validations for consuming a set number of donation UTxOs, along with the project's UTxO. Outputs are expected to be
    --   project's UTxO with an updated datum (given), and also a single Donations` UTxO.
    --
    --   Raises exception on @False@.
    
    foldDonationsPhaseOne :: Integer -> QVFDatum -> Bool
    foldDonationsPhaseOne requiredDonationCount psUpdatedDatum =
      -- {{{
      let
        tn                    = getCurrTokenName qvfProjectSymbol
        (ds, total, finalMap) = foldDonationInputs qvfDonationSymbol tn inputs
        outputs               = getContinuingOutputs ctx
        mOD                   = find (utxoHasX qvfDonationSymbol $ Just tn) outputs
        mOP                   = find (utxoHasX qvfProjectSymbol $ Just tn) outputs
      in
      case (mOD, mOP) of
        (Just od, Just op) ->
          -- {{{
             traceIfFalse
               "Incorrect number of donations included for the first phase of folding."
               (ds == requiredDonationCount)
          && traceIfFalse
               "Donations output must carry the folded donations."
               (utxosDatumMatchesWith (Donations finalMap) od)
          && traceIfFalse
               "Project output must be properly updated."
               (utxosDatumMatchesWith psUpdatedDatum op)
          && traceIfFalse
               "Donations output must carry all the donation Lovelaces."
               (utxoHasLovelaces total od)
          && traceIfFalse
               "Project output must preserve its Lovelaces."
               (utxoHasLovelaces halfOfTheRegistrationFee op)
          && canFoldOrDistribute
          && signedByKeyHolder
          -- }}}
        _                  ->
          -- {{{
          traceError "Missing proper outputs for the first phase of folding donations."
          -- }}}
      -- }}}

    -- | Traverses transaction inputs and outputs to validate the proper
    --   correspondence between input `PrizeWeight` datums and their
    --   depleted couterparts.
    --
    --   Raises exception upon failure.
    traversePrizeWeights :: Integer -> Integer -> Integer -> Integer -> Bool
    traversePrizeWeights totPs psSoFar lovelacesSoFar wSoFar =
      -- {{{
      let
        remaining = totPs - psSoFar
        inputFoldFn TxInInfo{txInInfoResolved = o} acc@(pCount, sumL, sumW, wMap) =
          -- {{{
          if utxoHasX qvfProjectSymbol Nothing o then
            -- {{{
            case getInlineDatum o of
              PrizeWeight w False ->
                -- {{{
                case getTokenNameOfUTxO qvfProjectSymbol o of
                  Just tn ->
                    ( pCount + 1
                    , sumL + lovelaceFromValue (txOutValue o)
                    , sumW + w
                    , Map.insert tn w wMap
                    )
                  Nothing ->
                    traceError "Project asset not found."
                -- }}}
              _                   ->
                -- {{{
                traceError "Unexpected UTxO encountered (expected a `PrizeWeight`."
                -- }}}
            -- }}}
          else
            -- {{{
            acc
            -- }}}
          -- }}}
        (inputPs, sumOfTheirLovelaces, sumOfTheirWs, mapFromTNsToWs) =
          foldr inputFoldFn (0, 0, 0, Map.empty) inputs
        outputFoldFn o acc =
          -- {{{
          case getTokenNameOfUTxO qvfProjectSymbol o of
            Just tn ->
              -- {{{
              case Map.lookup tn mapFromTNsToWs of
                Just validW ->
                  -- {{{
                  let
                    isValid =
                         utxosDatumMatchesWith (PrizeWeight validW True) o
                      && utxoHasLovelaces halfOfTheRegistrationFee o
                  in
                  if isValid then
                    acc + 1
                  else
                    traceError
                      "Invalid datum attached to depleted prize weight UTxO."
                  -- }}}
                Nothing         ->
                  -- {{{
                  traceError
                    "Invalid prize weight UTxO is being produced."
                  -- }}}
              -- }}}
            Nothing ->
              -- {{{
              let
                lovelaces    = lovelacesSoFar + sumOfTheirLovelaces
                w            = wSoFar + sumOfTheirWs
                datumIsValid =
                  -- {{{
                  if inputPs < remaining then
                    -- {{{
                    utxosDatumMatchesWith
                      ( DonationAccumulationProgress
                          totPs
                          (psSoFar + inputPs)
                          lovelaces
                          w
                      )
                      o
                    -- }}}
                  else if inputPs == remaining then
                    -- {{{
                    utxosDatumMatchesWith
                      ( DonationAccumulationConcluded
                          totPs
                          lovelaces
                          w
                          False
                      )
                      o
                    -- }}}
                  else
                    -- {{{
                    traceError
                      "Excessive number of prize weight inputs are provided."
                    -- }}}
                  -- }}}
                isValid      =
                  -- {{{
                     utxoHasX qvfSymbol (Just qvfTokenName) o
                  && traceIfFalse
                       "Main UTxO should carry all the donations."
                       (utxoHasLovelaces lovelaces o)
                  && traceIfFalse
                       "Invalid datum attached to the produced main datum."
                       datumIsValid
                  -- }}}
              in
              if isValid then
                acc
              else
                -- Failure in case of missing main authentication asset.
                traceError "Invalid UTxO getting produced at the script."
              -- }}}
          -- }}}
        outputPs =
          -- {{{
          foldr outputFoldFn 0 $ getContinuingOutputs ctx
          -- }}}
      in
      traceIfFalse
        "Improper correspondence between input and output prize weights."
        (inputPs == outputPs)
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   is still in progress.
    --
    --   Raises exception on @False@.
    canRegisterOrDonate :: Bool
    canRegisterOrDonate =
      -- {{{
      case getDatumFromRefX qvfSymbol deadlineTokenName of
        DeadlineDatum dl ->
          traceIfFalse "This funding round is over." $ deadlineNotReached dl
        _                ->
          traceError "Invalid deadline datum."
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   has ended.
    --
    --   Raises exception on @False@.
    canFoldOrDistribute :: Bool
    canFoldOrDistribute =
      -- {{{
      case getDatumFromRefX qvfSymbol deadlineTokenName of
        DeadlineDatum dl ->
          traceIfFalse "This funding round is still in progress." $ deadlineReached dl
        _                ->
          traceError "Invalid deadline datum."
      -- }}}

    -- | Checks whether project registrations and donations are still allowed.
    deadlineNotReached :: POSIXTime -> Bool
    deadlineNotReached dl =
      -- {{{
      Interval.to dl `Interval.contains` txInfoValidRange info
      -- }}}

    -- | Checks whether the distribution can be triggered.
    deadlineReached :: POSIXTime -> Bool
    deadlineReached dl =
      -- {{{
      Interval.from dl `Interval.contains` txInfoValidRange info
      -- }}}

    -- | Validates the minting value.
    mintIsPresent :: CurrencySymbol -> TokenName -> Integer -> Bool
    mintIsPresent sym tn amt =
      -- {{{
      any
        (\(sym', tn', amt') -> sym' == sym && tn' == tn && amt' == amt)
        (flattenValue $ txInfoMint info)
      -- }}}

    projectMintIsPresent :: Bool -> Bool
    projectMintIsPresent mint =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(sym', _, amt')] ->
             traceIfFalse
               "Invalid asset is getting minted/burnt."
               (sym' == qvfProjectSymbol)
          && traceIfFalse
               "There should be exactly 2 project assets minted/burnt."
               (if mint then amt' == 2 else amt' == negate 2)
        _                 ->
          traceError "Only one project asset must be minted/burnt."
               -- (mintIsPresent qvfProjectSymbol tn $ if mint then 2 else negate 2)
      -- }}}

    -- | Expects a single continuing output, and validates given predicates.
    validateSingleOutput :: Maybe Integer
                         -> Maybe QVFDatum
                         -> Maybe (CurrencySymbol, TokenName)
                         -> Bool
    validateSingleOutput mExpectedLovelaces mExpectedDatum mExpectedAsset =
      -- {{{
      case getContinuingOutputs ctx of
        [o] ->
          -- {{{
             traceIfFalse
               "Invalid Lovelace count at output."
               ( case mExpectedLovelaces of
                   Just outputLovelaces ->
                     utxoHasLovelaces outputLovelaces o
                   Nothing              ->
                     True
               )
          && traceIfFalse
               "Invalid output datum."
               ( case mExpectedDatum of
                   Just updatedDatum ->
                     utxosDatumMatchesWith updatedDatum o
                   Nothing           ->
                     True
               )
          && traceIfFalse
               "Unauthentic output UTxO."
               ( case mExpectedAsset of
                   Just (sym, tn) ->
                     utxoHasX sym (Just tn) o
                   Nothing        ->
                     True
               )
          -- }}}
        _   ->
          -- {{{
          traceError
            "There should be exactly 1 UTxO going back to the script."
          -- }}}
      -- }}}
  in
  case (datum, action) of
    (DeadlineDatum _                              , UpdateDeadline newDl   ) ->
      -- {{{
         signedByKeyHolder
      && traceIfFalse
           "New deadline has already passed."
           (deadlineReached newDl)
      && traceIfFalse
           "Missing authentication asset."
           (currUTxOHasX qvfSymbol deadlineTokenName)
      && validateSingleOutput
           Nothing
           (Just $ DeadlineDatum newDl)
           (Just (qvfSymbol, deadlineTokenName))
      -- }}}

    (RegisteredProjectsCount _                    , RegisterProject        ) ->
      -- Project Registration
      -- {{{
      projectMintIsPresent True && canRegisterOrDonate
      -- }}}

    (ReceivedDonationsCount _                     , DonateToProject projId ) ->
      -- Project Donation
      -- {{{
         traceIfFalse
           "There should be exactly 1 donation asset minted."
           (mintIsPresent qvfDonationSymbol (TokenName projId) 1)
      && canRegisterOrDonate
      -- }}}

    (Donation _                                   , FoldDonations          ) ->
      -- Folding Donations
      -- To avoid excessive transaction fees, this endpoint delegates its
      -- logic to the @P@ UTxO by checking that it is in fact being spent.
      -- {{{ 
      let
        -- | Finds the token name of the current donation UTxO being spent.
        --   Uses this value to check the presence of relevant project UTxO.
        --
        --   Raises exception upon failure.
        tn = getCurrTokenName qvfDonationSymbol
      in
        traceIfFalse "The project UTxO must also be getting consumed."
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          ReceivedDonationsCount _          -> True
          DonationFoldingProgress tot soFar -> tot > soFar
          _                                 -> False
      -- }}} 

    (ReceivedDonationsCount tot                   , FoldDonations          ) ->
      -- Folding Donations
      -- {{{
      if tot <= maxDonationInputsForPhaseTwo then
        -- No need for a two phase folding.
        let
          tn = getCurrTokenName qvfProjectSymbol
        in
        -- foldDonationsPhaseTwo tot
           traceIfFalse
             "All donation assets must be burnt."
             (mintIsPresent qvfDonationSymbol tn (negate tot))
        && canFoldOrDistribute
        && signedByKeyHolder
      else
        -- Folding should happen in two phases.
        foldDonationsPhaseOne
          maxDonationInputsForPhaseOne -- The number of donation assets expected in inputs.
          ( DonationFoldingProgress
              tot                                  -- Total number of donations.
              (tot - maxDonationInputsForPhaseOne) -- Donations folded so far.
          )
      -- }}}

    (DonationFoldingProgress tot soFar            , FoldDonations          ) ->
      -- Folding Donations
      -- {{{
      let
        remaining = tot - soFar
      in
      if remaining == 0 then
        let
          tn = getCurrTokenName qvfProjectSymbol
        in
        -- foldDonationsPhaseTwo tot
           traceIfFalse
             "All donation assets must be burnt."
             (mintIsPresent qvfDonationSymbol tn (negate tot))
        && canFoldOrDistribute
        && signedByKeyHolder
      else
        let
          expected  = min remaining maxDonationInputsForPhaseOne
        in
        foldDonationsPhaseOne
          expected
          (DonationFoldingProgress tot (soFar + expected))
      -- }}}

    (Donations _                                  , FoldDonations          ) ->
      -- Second Phase of Folding Donations
      -- Similar to `Donation`, this endpoint also delegates its logic. This
      -- time, specifically to `DonationFoldingProgress`.
      -- {{{ 
      let
        tn = getCurrTokenName qvfDonationSymbol
      in
        traceIfFalse "The concluded folding project UTxO must also be getting consumed."
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          DonationFoldingProgress tot soFar -> tot == soFar
          _                                 -> False
      -- }}} 

    (PrizeWeight _ False                          , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      -- (Delegation of logic to the main UTxO.)
      -- {{{ 
        traceIfFalse "The main UTxO must also be getting consumed."
      $ xInputWithSpecificDatumExists qvfSymbol qvfTokenName
      $ \case
          RegisteredProjectsCount _                  -> True
          DonationAccumulationProgress tot soFar _ _ -> tot > soFar
          _                                          -> False
      -- }}} 

    (RegisteredProjectsCount tot                  , AccumulateDonations    ) ->
      -- First Accumulation of Donations
      -- {{{ 
      traversePrizeWeights tot 0 0 0
      -- }}} 

    (DonationAccumulationProgress tot ps ds ws    , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      -- {{{ 
      traversePrizeWeights tot ps ds ws
      -- }}} 

    (DonationAccumulationConcluded ps ds den False, PayKeyHolderFee        ) ->
      -- Key Holder Fee Collection
      -- {{{
      let
        (khFee, updatedDatum) = findDatumAfterPayingKeyHoldersFee ps ds den
        keyHolderImbursed     =
          lovelaceFromValue (valuePaidTo info qvfKeyHolder) == khFee
      in
         traceIfFalse
           "Unauthentic governance UTxO provided."
           (currUTxOHasX qvfSymbol qvfTokenName)
      && ( case getContinuingOutputs ctx of
             [o] ->
               -- {{{
               let
                 inVal         = txOutValue currUTxO
                 desiredOutVal = inVal <> lovelaceValueOf (negate khFee)
               in
                  traceIfFalse
                    "Invalid Lovelace count at the produced governance UTxO."
                    (utxoHasValue desiredOutVal o)
               && traceIfFalse
                    "Governance datum not updated properly."
                    (utxosDatumMatchesWith updatedDatum o)
               -- }}}
             _   ->
               -- {{{
               traceError "Governance UTxO not produced."
               -- }}}
         )
      && traceIfFalse
           "Key holder fees must be paid accurately."
           keyHolderImbursed
      -- }}}

    (DonationAccumulationConcluded ps ds den True , DistributePrizes       ) ->
      -- Prize Distribution
      -- {{{
      let
        scriptOutputs    = getContinuingOutputs ctx

        -- | Folds all the reference project info UTxOs in a @Map@ from their
        --   token names to their details.
        recepientsInfoMap =
          -- {{{
          foldr
            ( \TxInInfo{txInInfoResolved = txOut} acc ->
                case getTokenNameOfUTxO qvfProjectSymbol txOut of
                  Just tn ->
                    -- {{{
                    case getInlineDatum txOut of
                      ProjectInfo dets ->
                        Map.insert tn dets acc
                      _                ->
                        traceError "Bad project info reference provided."
                    -- }}}
                  Nothing ->
                    -- {{{
                    acc
                    -- }}}
            )
            Map.empty
            (txInfoReferenceInputs info)
          -- }}}

        -- | Folding function to go over all the input UTxOs. For each project
        --   `PrizeWeight` input it finds (such that its info is also present
        --   in the reference inputs), checks to see if the project owner is
        --   paid his/her rightful portion.
        foldFn TxInInfo{txInInfoResolved = txOut} acc@(accP, accPaid) =
          -- {{{
          case getTokenNameOfUTxO qvfProjectSymbol txOut of
            Just tn ->
              -- {{{
              case Map.lookup tn recepientsInfoMap of
                Just ProjectDetails{..} ->
                  -- {{{
                  case getInlineDatum txOut of
                    PrizeWeight w True ->
                      -- {{{
                      let
                        portion          = findProjectsWonLovelaces ds den w
                        paidAmount       =
                          -- {{{
                          lovelaceFromValue (valuePaidTo info pdPubKeyHash)
                          -- }}}
                        prizeIsPaid      = paidAmount == portion
                        mEscrowOutput    =
                          -- {{{
                          find
                            (utxoHasX qvfProjectSymbol $ Just tn)
                            scriptOutputs
                          -- }}}
                        escrowIsProduced =
                          -- {{{
                          case mEscrowOutput of
                            Just o  ->
                              -- {{{
                                 traceIfFalse
                                   "Escrow must carry the excess reward."
                                   ( utxoHasLovelaces
                                       ( max halfOfTheRegistrationFee $
                                             halfOfTheRegistrationFee
                                           + portion
                                           - pdRequested
                                       )
                                       o
                                   )
                              && traceIfFalse
                                   "Escrow's inline datum is invalid."
                                   (utxosDatumMatchesWith (Escrow Map.empty) o)
                              -- }}}
                            Nothing ->
                              -- {{{
                              traceError "Escrow UTxO was not produced."
                              -- }}}
                          -- }}}
                      in
                      if prizeIsPaid && escrowIsProduced then
                        (accP + 1, accPaid + portion)
                      else
                        traceError "Prize not paid."
                      -- }}}
                    _                  ->
                      -- {{{
                      traceError "Bad datum attached to a project UTxO."
                      -- }}}
                  -- }}}
                Nothing                 ->
                  -- {{{
                  traceError
                    "Couldn't find the corresponding input UTxO of a provided reference project UTxO."
                  -- }}}
              -- }}}
            Nothing ->
              -- {{{
              acc
              -- }}}
          -- }}}

        (projectsAccountedFor, prizesPaid) = foldr foldFn (0, 0) inputs
      in
      if projectsAccountedFor > ps then -- TODO: Is this redundant?
        traceError "The impossible happened."
      else
        case find (utxoHasX qvfSymbol (Just qvfTokenName)) scriptOutputs of
          Just o  ->
            -- {{{
            let
              currLovelaces      = lovelaceFromValue $ txOutValue currUTxO
              remainingLovelaces = currLovelaces - prizesPaid
            in
               traceIfFalse
                 "Current UTxO is unauthentic."
                 (currUTxOHasX qvfSymbol qvfTokenName)
            && traceIfFalse
                 "Impossible 2.0 happened." -- TODO: Is this necessary?
                 (remainingLovelaces >= governanceLovelaces)
            && traceIfFalse
                 "Governance UTxO is not getting updated properly."
                 ( utxosDatumMatchesWith
                     ( DonationAccumulationConcluded
                         (ps - projectsAccountedFor)
                         ds
                         den
                         True
                     )
                     o
                 )
            && traceIfFalse
                 "Prize Lovelaces are not properly withdrawn."
                 (utxoHasLovelaces remainingLovelaces o)
            -- }}}
          Nothing ->
            -- {{{
            traceError "Missing output governance UTxO."
            -- }}}
      -- }}}

    (PrizeWeight _ True                           , DistributePrizes       ) ->
      -- Prize Distribution
      -- (Delegation of logic to the project's UTxO.)
      -- {{{ 
      let
        tn = getCurrTokenName qvfProjectSymbol
      in
        traceIfFalse "The project UTxO must also be getting consumed."
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          DonationAccumulationConcluded _ _ _ True -> True
          _                                        -> False
      -- }}} 

    --      v-----------v is there a better term?
    (Escrow beneficiaries                         , UnlockEscrowFor ben amt) ->
      -- Giving Withdrawal Rights to a Bounty Winner
      -- {{{
      let
        currLovelaces = lovelaceFromValue $ txOutValue currUTxO
        available     = currLovelaces - halfOfTheRegistrationFee
        tn            = getCurrTokenName qvfProjectSymbol
        projectOwner  =
          -- {{{
          case getDatumFromRefX qvfProjectSymbol tn of
            ProjectInfo ProjectDetails{..} ->
              pdPubKeyHash
            _                              ->
              traceError "Bad reference project datum provided."
          -- }}}
      in
         traceIfFalse
           "Unauthentic escrow UTxO."
           (currUTxOHasX qvfProjectSymbol tn)
      && traceIfFalse
           "Insufficient funds."
           (amt <= available)
      && traceIfFalse
           "Missing project owner's signature."
           (txSignedBy info projectOwner)
      && validateSingleOutput
           (Just currLovelaces)
           (   Just
             $ Escrow
             $ Map.unionWith (+) (Map.singleton ben amt) beneficiaries
           )
           (Just (qvfSymbol, qvfTokenName))
      -- }}}

    (Escrow beneficiaries                         , WithdrawBounty winner  ) ->
      -- Bounty Collection from Escrow Account
      -- {{{
      let
        currLovelaces = lovelaceFromValue $ txOutValue currUTxO
        tn            = getCurrTokenName qvfProjectSymbol
      in
      case Map.lookup winner beneficiaries of
        Just bounty ->
          -- {{{
          let
            remainingBounty = currLovelaces - bounty
          in
          if remainingBounty > halfOfTheRegistrationFee then
            -- {{{
               traceIfFalse
                 "Unauthentic escrow UTxO."
                 (currUTxOHasX qvfProjectSymbol tn)
            && traceIfFalse
                 "The bounty winner must be imbursed."
                 (lovelaceFromValue (valuePaidTo info winner) == bounty)
            && validateSingleOutput
                 (Just remainingBounty)
                 (Just $ Escrow $ Map.delete winner beneficiaries)
                 (Just (qvfSymbol, qvfTokenName))
            -- }}}
          else 
            -- {{{
            projectMintIsPresent False
            -- }}}
          -- }}}
        Nothing     ->
          -- {{{
          traceError "Not eligible for bounty withdrawal."
          -- }}}
      -- }}}

    (Escrow beneficiaries                         , ConcludeProject        ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
         traceIfFalse
           "Can not conclude with withstanding beneficiaries."
           (Map.null beneficiaries) --checks if the escrow is empty
      && projectMintIsPresent False --checks if project token is present.
        -- if both any is false traceError is called.
      -- }}}

    (ProjectInfo _                                , ConcludeProject        ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
      projectMintIsPresent False
      -- }}}

    (_                                            , Dev                    ) ->
      -- For development. TODO: REMOVE.
      True
    (_                                            , _                      ) ->
      traceError "Invalid transaction."
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
data QVF
instance Scripts.ValidatorTypes QVF where
  type DatumType    QVF = QVFDatum
  type RedeemerType QVF = QVFAction


typedQVFValidator :: QVFParams -> PSU.V2.TypedValidator QVF
typedQVFValidator =
  let
    wrap = PSU.V2.mkUntypedValidator @QVFDatum @QVFAction
  in
  PSU.V2.mkTypedValidatorParam @QVF
    $$(PlutusTx.compile [|| mkQVFValidator ||])
    $$(PlutusTx.compile [|| wrap ||])


qvfValidator :: QVFParams -> Validator
qvfValidator =
    Plutonomy.optimizeUPLC
  . PSU.V2.validatorScript
  . typedQVFValidator


qvfValidatorHash :: QVFParams -> ValidatorHash
qvfValidatorHash = PSU.V2.validatorHash . typedQVFValidator


qvfAddress :: QVFParams -> Address
qvfAddress = PSU.V2.validatorAddress . typedQVFValidator
-- }}}
-- }}}


-- UTILS
-- {{{
{-# INLINABLE findDatumAfterPayingKeyHoldersFee #-}
-- | Returns the calculated fee, and the updated datum.
findDatumAfterPayingKeyHoldersFee :: Integer
                                  -> Integer
                                  -> Integer
                                  -> (Integer, QVFDatum)
findDatumAfterPayingKeyHoldersFee ps totalLovelaces denominator =
  -- {{{
  let
    forKH = max minKeyHolderFee $ 5 * totalLovelaces `divide` 100
  in
  ( forKH
  , DonationAccumulationConcluded ps (totalLovelaces - forKH) denominator True
  )
  -- }}}


{-# INLINABLE findProjectsWonLovelaces #-}
findProjectsWonLovelaces :: Integer -> Integer -> Integer -> Integer
findProjectsWonLovelaces pool sumW w =
  -- {{{
  (w * pool) `divide` sumW
  -- }}}
-- }}}


-- TxInfo	= TxInfo
--   { txInfoInputs          :: [TxInInfo]
--   , txInfoReferenceInputs :: [TxInInfo]
--   , txInfoOutputs         :: [TxOut]
--   , txInfoFee             :: Value
--   , txInfoMint            :: Value
--   , txInfoDCert           :: [DCert]
--   , txInfoWdrl            :: Map StakingCredential Integer
--   , txInfoValidRange      :: POSIXTimeRange
--   , txInfoSignatories     :: [PubKeyHash]
--   , txInfoRedeemers       :: Map ScriptPurpose Redeemer 
--   , txInfoData            :: Map DatumHash Datum 
--   , txInfoId              :: TxId
--   }

-- TxInInfo = TxInInfo
--   { txInInfoOutRef    :: TxOutRef
--   , txInInfoResolved  :: TxOut
--   }

-- TxOut = TxOut
--   { txOutAddress         :: Address
--   , txOutValue           :: Value
--   , txOutDatum           :: OutputDatum
--   , txOutReferenceScript :: Maybe ScriptHash
--   }

-- OutputDatum
--   = NoOutputDatum	 
--   | OutputDatumHash DatumHash	 
--   | OutputDatum     Datum

