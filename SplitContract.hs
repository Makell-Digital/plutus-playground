import Control.Monad (void)
import Data.Text qualified as T
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (Ada, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (ScriptContext, scriptContextTxInfo), valuePaidTo)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts

import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell
import Schema (ToSchema)
import Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)

data SplitData =
    SplitData 
        { recipient1 :: PaymentPubKeyHash
        , recipient2 :: PaymentPubKeyHash
        , amount     :: Ada
        }
    deriving stock (Haskell.Show, Generic)

PlutusTx.unstableMakeIsData ''SplitData
PlutusTx.makeLift ''SplitData

validateScript :: SplitData -> () -> ScriptContext -> Bool
validateScript SplitData{recipient1, recipient2, amount} _ ScriptContext{scriptContextTxInfo} = 
    let half = Ada.divide amount 2 in
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash recipient1)) >= half &&
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash recipient2)) >= (amount - half)

data SplitValidator
instance Scripts.ValidatorTypes SplitValidator where
    type instance RedeemerType SplitValidator = ()
    type instance DatumType SplitValidator = SplitData

splitValidator :: Scripts.TypedValidator SplitValidator
splitValidator = Scripts.mkTypedValidator @SplitValidator
    $$(PlutusTx.compile [|| validateScript ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SplitData @()

data LockArgs =
    LockArgs
        { recipient1Wallet :: Wallet
        , recipient2Wallet :: Wallet
        , totalAda         :: Ada
        }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type SplitSchema =
    Endpoint "lock" LockArgs
    .\/ Endpoint "unlock" LockArgs

lock :: Promise () SplitSchema T.Text ()
lock = endpoint @"lock" (lockFunds . mkSplitData)

unlock :: Promise () SplitSchema T.Text ()
unlock = endpoint @"unlock" (unlockFunds . mkSplitData)

mkSplitData :: LockArgs -> SplitData
mkSplitData LockArgs{recipient1Wallet, recipient2Wallet, totalAda} =
    SplitData
        { recipient1 = mockWalletPaymentPubKeyHash recipient1Wallet
        , recipient2 = mockWalletPaymentPubKeyHash recipient2Wallet
        , amount = totalAda
        }

lockFunds :: SplitData -> Contract () SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> Haskell.show amount
    let constraints = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints splitValidator constraints

unlockFunds :: SplitData -> Contract () SplitSchema T.Text ()
unlockFunds SplitData{recipient1, recipient2, amount} = do
    let contractAddress = Scripts.validatorAddress splitValidator
    utxos <- utxosAt contractAddress
    let half = Ada.divide amount 2
        tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient1 (Ada.toValue half)
            <> Constraints.mustPayToPubKey recipient2 (Ada.toValue $ amount - half)
    void $ submitTxConstraintsSpending splitValidator utxos tx

endpoints :: Contract () SplitSchema T.Text ()
endpoints = selectList [lock, unlock]

mkSchemaDefinitions ''SplitSchema
$(mkKnownCurrencies [])
