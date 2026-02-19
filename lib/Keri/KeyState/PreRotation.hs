module Keri.KeyState.PreRotation
    ( commitKey
    , verifyCommitment
    ) where

{- |
Module      : Keri.KeyState.PreRotation
Description : Pre-rotation key commitment
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Implements simplified pre-rotation: each next-key
commitment is the CESR-encoded hash of the public
key bytes. During rotation, the revealed public key
is hashed and compared against the commitment.
-}

import Data.Text (Text)
import Keri.Cesr.DerivationCode
    ( DerivationCode (Blake2bDigest)
    )
import Keri.Cesr.Decode qualified as Cesr
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Blake3 qualified as Blake3

{- | Create a pre-rotation commitment for a public
key. The commitment is the CESR-encoded hash of the
raw public key bytes extracted from the CESR-encoded
key.
-}
commitKey :: Text -> Either String Text
commitKey cesrKey = do
    prim <- Cesr.decode cesrKey
    let keyHash = Blake3.hash (raw prim)
    pure $
        Cesr.encode
            Primitive
                { code = Blake2bDigest
                , raw = keyHash
                }

{- | Verify that a CESR-encoded public key matches a
pre-rotation commitment digest.
-}
verifyCommitment
    :: Text
    -- ^ CESR-encoded public key
    -> Text
    -- ^ CESR-encoded commitment digest
    -> Either String Bool
verifyCommitment cesrKey commitment = do
    computed <- commitKey cesrKey
    pure (computed == commitment)
