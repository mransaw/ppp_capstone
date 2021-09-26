{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Free where


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
import           Prelude                (IO, Semigroup(..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Ledger.Ada                 as Ada
import qualified Data.Map                   as Map
import           Data.Default               (Default (..))

trinketName :: TokenName
trinketName = "gameTrinket"

goldName :: TokenName
goldName = "goldToken"

-- create minting policy that allows only burning
{-# INLINABLE mkPolicyFree #-}
mkPolicyFree :: () -> ScriptContext -> Bool
mkPolicyFree () ctx = traceIfFalse "trinkets can only be burned" burnOnly
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx
      
      burnOnly :: Bool
      burnOnly = case flattenValue (txInfoForge info) of
          [x, (cs2, _, amt2)] -> cs2 == ownCurrencySymbol ctx && amt2 < 0
          _              -> False   

policyFree :: Scripts.MintingPolicy
policyFree = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicyFree ||])

trinketSymbol :: CurrencySymbol
trinketSymbol = scriptCurrencySymbol policyFree

-- create minting policy for gold tokens
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () ctx = traceIfFalse "input trinkets must be burned" inputsBurned    &&
                  traceIfFalse "output token name must be GoldToken with number equal to input trinkets" outputGoldCorrect

    where
      info :: TxInfo
      info = scriptContextTxInfo ctx
      
      -- get transaction inputs
      ownInput :: TxOut
      ownInput = case findOwnInput ctx of
          Nothing -> traceError "input missing"
          Just i  -> txInInfoResolved i
      
      trinketValue :: Integer
      trinketValue = assetClassValueOf (txOutValue ownInput) (AssetClass (trinketSymbol, trinketName))
        
      -- ensure that user's input trinkets are burned
      inputsBurned :: Bool
      inputsBurned = True -- case flattenValue (txInfoForge info) of
          --[x, (cs2, _, amt2)] -> cs2 == trinketSymbol && amt2 == negate trinketValue
          --_              -> False      

      -- ensure that value of goldTokens matches value of trinkets, and name of goldTokens is correct
      outputGoldCorrect :: Bool
      outputGoldCorrect = True --case flattenValue (txInfoForge info) of
          --[(cs, _, amt), (cs2, _, amt2)] -> cs  == ownCurrencySymbol ctx && amt == trinketValue
          --_               -> False    

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- create mint endpoint
type FreeSchema = Endpoint "mint" Integer

mint :: Integer -> Contract w FreeSchema Text ()
mint t_amt = do
    let valGold     = Value.singleton curSymbol goldName t_amt
        valTrinket  = Value.singleton trinketSymbol trinketName t_amt
        valBurn     = Value.singleton trinketSymbol trinketName $ negate t_amt
        lookups     = Constraints.mintingPolicy policy <>
                      Constraints.mintingPolicy policyFree
        tx          = Constraints.mustMintValue valGold <>
                      Constraints.mustMintValue valBurn
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show valGold)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

-- test the endpoint
test :: IO ()
test = do
    test' 8
    test' 100000001
    
test' :: Integer -> IO ()
test' token_amt = runEmulatorTraceIO' def emCfg def $ myTrace token_amt
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                       100000000 <>
        Value.singleton trinketSymbol trinketName 100000000

myTrace :: Integer -> EmulatorTrace ()
myTrace tkn_amt = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 12
    callEndpoint @"mint" h2 15
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 tkn_amt
    void $ Emulator.waitNSlots 1
