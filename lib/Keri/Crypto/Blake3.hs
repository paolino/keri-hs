module Keri.Crypto.Blake3
    ( hash
    , hashSize
    ) where

{- |
Module      : Keri.Crypto.Blake3
Description : Blake2b-256 hashing (Blake3 architecture)
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Provides a 256-bit hash function. Uses Blake2b-256 from
@crypton@ as the backend. In a production KERI
implementation this would use actual Blake3; the API and
CESR code (\"E\") remain identical.
-}

import Crypto.Hash qualified as Hash
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)

-- | Hash size in bytes (32 bytes = 256 bits).
hashSize :: Int
hashSize = 32

-- | Compute a 256-bit hash of the input.
hash :: ByteString -> ByteString
hash =
    BA.convert
        . Hash.hashWith Hash.Blake2b_256
