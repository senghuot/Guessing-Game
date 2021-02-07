import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import           Language.Plutus.Contract
import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude hiding (pure, (<$>))
import           Ledger                    (Address, Validator, ValidatorCtx, Value, scriptAddress, pubKeyHash)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract
import qualified Prelude
import           Wallet.Emulator.Wallet    (Wallet, walletPubKey)

-- grabbing the random number from the last character of the wallet 1 address
-- Note: this value can't be access on Plutos-Core so we have to lift it
-- to blockchain as part of the validator function
randomNum :: Integer
randomNum = toInteger $ fromEnum $ C.last $ C.pack $ show $ walletPubKey $ Wallet 1

-- this magic number allows us to win the game
magicNum :: Integer
magicNum = 123

-- Validator function has the format of (Datum -> Redeemer -> ValidatorCtx -> Bool) where it is computed
-- on the chain. If we look at the ScriptInstance, this is where we define the type of the
-- Datum and Redeemer.
validateGuess :: Integer -> Integer -> ValidatorCtx -> Bool
validateGuess actual guess _ = guess == actual || guess == magicNum

-- This is a hash of the validator. We can extract the functin from the the game instance.
gameAddress :: Address
gameAddress = Ledger.scriptAddress $ Scripts.validatorScript gameInstance

-- Parameters for the "lock" endpoint
data LockParams = LockParams
    { amount :: Value}
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, IotsType, ToSchema, ToArgument)

--  Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessNumber :: Integer}
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, IotsType, ToSchema, ToArgument)

-- The "lock" contract endpoint. using the GameSchema, we can extract the input from "lock" endpoint.
-- To submit the transaction to the network, we need (1) Script Instance (2) transaction
-- constraints, so we put restrictions on fund to be spent later by the validator
-- function, in this example the constraints will be the randomNum.
lock :: AsContractError e => Contract GameSchema e ()
lock = do
    LockParams amount <- endpoint @"lock" @LockParams
    let tx            = Constraints.mustPayToTheScript randomNum amount
    void $ submitTxConstraints gameInstance tx

-- The "guess" contract endpoint. using the GameSchema, we can extract the input from "guess" endpoint.
-- To spend the money that locked in the script, we need to pass the constraints which defines
-- by the validator. submitTxConstraintsSpending will take the gameInstance. 
guess :: AsContractError e => Contract GameSchema e ()
guess = do
    GuessParams theGuess <- endpoint @"guess" @GuessParams
    if theGuess > randomNum
        then logInfo $ show "It's lower."
        else if theGuess < randomNum
            then logInfo $ show "It's higher."
            else logInfo $ show "Congrats."

    unspentOutputs <- utxoAt gameAddress
    let tx = collectFromScript unspentOutputs theGuess
    void $ submitTxConstraintsSpending gameInstance unspentOutputs tx

-- the script is taking two parameter DatumType which is a wrapper around the data we used
-- in the script output reedemerType is the wrapper around data we take as an input
data GameDataType
instance Scripts.ScriptType GameDataType where
    type instance DatumType GameDataType = Integer
    type instance RedeemerType GameDataType = Integer

-- this is our game instance where we take the validator script and make it part of the on-chain
-- later on we will use it as part of input of submit the transaction to the network.
gameInstance :: Scripts.ScriptInstance GameDataType
gameInstance = Scripts.validator @GameDataType
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| Scripts.wrapValidator @Integer @Integer ||])

-- here we're taking input from the interface, this game schema consist of two endpoints.
-- we're using the .\/ operator to combine "lock" and "guess".
type GameSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockParams
        .\/ Endpoint "guess" GuessParams

-- we gonna use select to create two branches, the app exposes both endpoints but we want to proceed
-- with the whichever branch receives the input first.
endpoints :: AsContractError e => Contract GameSchema e ()
endpoints = lock `select` guess

-- lastly, we gonna bind the everything to the playground via GameSchema
mkSchemaDefinitions ''GameSchema

$(mkKnownCurrencies [])