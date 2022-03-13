{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Utils
    ( unsafeReadTxOutRef
    , writeMintingPolicy
    , unsafeTokenNameToHex
    ) where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (PlutusScript (..))
import           Codec.Serialise             (serialise)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromJust)
import           Data.String                 (IsString (..))
import           Plutus.V1.Ledger.Value      (TokenName (..)) 
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import qualified Ledger                      as Plutus


unsafeReadTxOutRef :: String -> Plutus.TxOutRef
unsafeReadTxOutRef s =
  let
    (x, _ : y) = span (/= '#') s
  in
    Plutus.TxOutRef
        { Plutus.txOutRefId  = fromString x
        , Plutus.txOutRefIdx = read y
        }

writeMintingPolicy :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy

unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BS8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
  where
    getByteString (BuiltinByteString bs) = bs
