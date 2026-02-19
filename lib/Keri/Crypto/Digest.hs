module Keri.Crypto.Digest
    ( computeSaid
    , saidPlaceholder
    ) where

-- \|
-- Module      : Keri.Crypto.Digest
-- Description : SAID (Self-Addressing Identifier) computation
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Computes Self-Addressing Identifiers by hashing
-- serialized event data and CESR-encoding the digest.
-- The placeholder is 44 @#@ characters (matching a
-- 1-char-code 32-byte CESR primitive).

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Keri.Cesr.DerivationCode
    ( DerivationCode (Blake2bDigest)
    , totalLength
    )
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Blake3 qualified as Blake3

{- | Placeholder for the @d@ field during SAID
computation: 44 @#@ characters.
-}
saidPlaceholder :: Text
saidPlaceholder =
    T.replicate
        (totalLength Blake2bDigest)
        "#"

{- | Compute the SAID of serialized event bytes.
Hash the bytes and CESR-encode as a digest primitive.
-}
computeSaid :: ByteString -> Text
computeSaid bs =
    Cesr.encode
        Primitive
            { code = Blake2bDigest
            , raw = Blake3.hash bs
            }
