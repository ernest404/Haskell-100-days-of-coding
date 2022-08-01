-- Alternative to Plutus Playground
-- allows simulation of smart contracts
-- Defines initial conditions of the wallets and actions
-- Just like the simulation tab of the playground.
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras --This module provides effects and handlers for structured logging and tracing.
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace --Runs the trace with runEmulatorTrace, with default configuration that prints a selection of well formatted events to stdout.

myTrace :: EmulatorTrace () --emulator trace is a monad, we can use do notation, return, >>, >>=. It enables us to
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints -- activates a endpoint contract for wallets 1 and 2. The results is a handles. Monadic action whose effect is to start the contract.
    h2 <- activateContractWallet (knownWallet 2) endpoints -- enables us to select a simulator wallet
    callEndpoint @"give" h1 $ GiveParams -- call endpoint contract give together with it's parameter on wallet 1.
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2 --Beneficiary's address
        , gpDeadline    = slotToBeginPOSIXTime def 20 --Get the starting POSIXTime of a Slot given a SlotConfig. SlotConfig used is the defualt one.
        , gpAmount      = 10000000 --amount in lovlace
        }
    void $ waitUntilSlot 20 -- wait action for 20 solts
    callEndpoint @"grab" h2 () -- call the grab endpoint contract action on wallet 2 without any parameters.
    s <- waitNSlots 2 --wait another 2 slots 
    Extras.logInfo $ "reached " ++ show s -- then log
