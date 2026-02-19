module Keri.Cesr.DerivationCode
    ( DerivationCode (..)
    , codeText
    , rawSize
    , codeLength
    , totalLength
    , identifyCode
    ) where

{- |
Module      : Keri.Cesr.DerivationCode
Description : CESR derivation code table
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Defines the CESR code table for cryptographic primitives
used in KERI. Each code identifies the type and size of
the attached cryptographic material.
-}

import Data.Text (Text)
import Data.Text qualified as T

-- | CESR derivation codes for cryptographic primitives.
data DerivationCode
    = -- | Ed25519 public key (code @D@, 32 bytes)
      Ed25519PubKey
    | -- | Blake2b-256 digest (code @E@, 32 bytes)
      Blake2bDigest
    | -- | Ed25519 signature (code @0B@, 64 bytes)
      Ed25519Sig
    deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Text representation of a derivation code.
codeText :: DerivationCode -> Text
codeText = \case
    Ed25519PubKey -> "D"
    Blake2bDigest -> "E"
    Ed25519Sig -> "0B"

-- | Size of the raw cryptographic material in bytes.
rawSize :: DerivationCode -> Int
rawSize = \case
    Ed25519PubKey -> 32
    Blake2bDigest -> 32
    Ed25519Sig -> 64

-- | Length of the code prefix in characters.
codeLength :: DerivationCode -> Int
codeLength = \case
    Ed25519PubKey -> 1
    Blake2bDigest -> 1
    Ed25519Sig -> 2

{- | Total length of the CESR-encoded primitive in
characters. Equals @(codeLength + rawSize) * 4 / 3@.
-}
totalLength :: DerivationCode -> Int
totalLength c =
    let padded = codeLength c + rawSize c
    in (padded * 4) `div` 3

{- | Identify the derivation code from a CESR-encoded
text prefix.
-}
identifyCode
    :: Text -> Either String DerivationCode
identifyCode txt
    | T.take 2 txt == "0B" = Right Ed25519Sig
    | T.take 1 txt == "D" = Right Ed25519PubKey
    | T.take 1 txt == "E" = Right Blake2bDigest
    | otherwise =
        Left $
            "Unknown CESR code: "
                <> T.unpack (T.take 2 txt)
